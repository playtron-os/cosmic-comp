//! Following the active workspace, and its colour.
//!
//! The workspace registry (`one.playtron.Workspaces1`) is the single source of
//! truth for which workspace is on screen; the compositor is a consumer. It
//! learns about switches from `ActiveChanged` and does not decide them —
//! residency, provisioning and slice management all live in the registry.
//!
//! It also learns each workspace's **accent**, the colour the user gave it,
//! which tints the airlock wash. That is why `ListChanged` matters as much as
//! `ActiveChanged`: recolouring the workspace you are already standing in
//! changes nothing about which one is active, and without that subscription the
//! new colour would never arrive.
//!
//! Nothing here is on the switch path's critical section. The registry answers
//! `Switch` as soon as the active id is written and the signal is out, so the
//! shell has its 300ms to become interactive (W-9) whatever the services are
//! doing behind it.
//!
//! With no registry running the active workspace stays `None`, every client is
//! visible and nothing is refused capture — so this changes no behaviour until
//! workspaces are switched on.

use calloop::LoopHandle;
use futures_executor::ThreadPool;
use tracing::{debug, warn};

use crate::state::State;

const DEST: &str = "one.playtron.Workspaces1";
const PATH: &str = "/one/playtron/Workspaces1";

/// The workspace on screen, as the registry describes it.
#[derive(Debug, Clone, PartialEq)]
pub struct ActiveWorkspace {
    pub id: String,
    /// The user's colour for this workspace.
    ///
    /// `None` when the registry gave none or gave something unparseable — the
    /// wash then falls back to a theme colour rather than painting whatever a
    /// malformed string happened to decode to.
    pub accent: Option<[f32; 3]>,
}

pub fn init(handle: &LoopHandle<'static, State>, executor: &ThreadPool) {
    let (tx, rx) = calloop::channel::channel::<Option<ActiveWorkspace>>();

    if let Err(err) = handle.insert_source(rx, |event, _, state| {
        if let calloop::channel::Event::Msg(active) = event {
            debug!(
                workspace = active.as_ref().map_or("<none>", |a| a.id.as_str()),
                "active workspace changed"
            );
            state.set_active_workspace(active);
        }
    }) {
        warn!(?err, "Failed to register workspace channel");
        return;
    }

    executor.spawn_ok(async move {
        if let Err(err) = watch(tx).await {
            // Not fatal, and not even unusual: no registry is running until
            // workspaces ship. Everything stays visible.
            debug!(%err, "not following a workspace registry");
        }
    });
}

async fn watch(tx: calloop::channel::Sender<Option<ActiveWorkspace>>) -> zbus::Result<()> {
    let conn = zbus::Connection::session().await?;

    // Subscribe BEFORE the first read. The compositor and the registry race at
    // boot, and a switch landing between the read and the subscription would
    // otherwise be lost — leaving the screen following a workspace it is no
    // longer on.
    let rule = |member: &'static str| -> zbus::Result<zbus::MatchRule<'static>> {
        Ok(zbus::MatchRule::builder()
            .msg_type(zbus::message::Type::Signal)
            .sender(DEST)?
            .interface(DEST)?
            .member(member)?
            .build())
    };
    let mut active_changed =
        zbus::MessageStream::for_match_rule(rule("ActiveChanged")?, &conn, None).await?;
    // Recolouring the workspace you are standing in does not change which one
    // is active, so a new accent only ever arrives through this one.
    let mut list_changed =
        zbus::MessageStream::for_match_rule(rule("ListChanged")?, &conn, None).await?;

    // A failed first read is normal when the registry has not claimed its name
    // yet: report "no workspace" and keep listening rather than giving up, or a
    // compositor that wins the boot race would never follow the registry at all.
    let _ = tx.send(read(&conn).await);

    use futures_util::stream::StreamExt;
    loop {
        // Both signals mean the same thing here — "re-read" — because
        // ActiveChanged carries only an id and the accent lives in the list.
        let alive = {
            let next_active = active_changed.next();
            let next_list = list_changed.next();
            futures_util::pin_mut!(next_active);
            futures_util::pin_mut!(next_list);
            match futures_util::future::select(next_active, next_list).await {
                futures_util::future::Either::Left((msg, _))
                | futures_util::future::Either::Right((msg, _)) => msg.is_some(),
            }
        };
        if !alive {
            break;
        }
        if tx.send(read(&conn).await).is_err() {
            break;
        }
    }
    Ok(())
}

/// The active workspace and its colour, or `None` if there is not one.
async fn read(conn: &zbus::Connection) -> Option<ActiveWorkspace> {
    let id = conn
        .call_method(Some(DEST), PATH, Some(DEST), "Active", &())
        .await
        .ok()?
        .body()
        .deserialize::<String>()
        .ok()?;
    // The registry reports "nothing active" as an empty string; taking it at
    // face value would tag every client with an id nothing can match.
    if id.is_empty() {
        return None;
    }

    let accent = conn
        .call_method(Some(DEST), PATH, Some(DEST), "List", &())
        .await
        .ok()
        .and_then(|reply| {
            reply
                .body()
                .deserialize::<Vec<(String, String, String, String, bool)>>()
                .ok()
        })
        .and_then(|rows| {
            rows.into_iter()
                .find(|(row_id, ..)| *row_id == id)
                .and_then(|(_, _, accent, ..)| parse_accent(&accent))
        });

    Some(ActiveWorkspace { id, accent })
}

/// `#rrggbb` to an RGB triple.
///
/// Returns `None` rather than a guess for anything malformed: the caller falls
/// back to a theme colour, which is a better answer than washing the screen in
/// whatever a bad string happened to decode to.
fn parse_accent(accent: &str) -> Option<[f32; 3]> {
    let hex = accent.strip_prefix('#')?;
    if hex.len() != 6 || !hex.bytes().all(|b| b.is_ascii_hexdigit()) {
        return None;
    }
    let channel = |i: usize| {
        u8::from_str_radix(&hex[i..i + 2], 16)
            .ok()
            .map(|v| f32::from(v) / 255.0)
    };
    Some([channel(0)?, channel(2)?, channel(4)?])
}

/// Ask the registry to step `delta` workspaces along — the vertical axis.
///
/// Fire and forget: the switch arrives back through `ActiveChanged` like any
/// other, so a keypress and a click on the panel take exactly the same path and
/// cannot disagree about what happened. Nothing here waits on the round trip,
/// because the shell has 300ms to become interactive (W-9).
pub fn cycle(delta: i32) {
    std::thread::spawn(move || {
        let result = futures_executor::block_on(async {
            let conn = zbus::Connection::session().await?;
            conn.call_method(Some(DEST), PATH, Some(DEST), "Cycle", &(delta,))
                .await
        });
        match result {
            Ok(_) => {}
            // No registry is the normal case until workspaces ship; the
            // keypress simply does nothing.
            Err(err) => debug!(%err, delta, "workspace cycle went nowhere"),
        }
    });
}

#[cfg(test)]
mod tests {
    use super::parse_accent;

    #[test]
    fn a_registry_accent_becomes_an_rgb_triple() {
        let c = parse_accent("#7aa2f7").unwrap();
        assert!((c[0] - 0.478).abs() < 0.01);
        assert!((c[1] - 0.635).abs() < 0.01);
        assert!((c[2] - 0.969).abs() < 0.01);
    }

    #[test]
    fn black_and_white_are_the_endpoints() {
        assert_eq!(parse_accent("#000000").unwrap(), [0.0, 0.0, 0.0]);
        assert_eq!(parse_accent("#ffffff").unwrap(), [1.0, 1.0, 1.0]);
    }

    #[test]
    fn anything_malformed_is_refused_rather_than_guessed() {
        // The caller falls back to a theme colour; washing the screen in
        // whatever a bad string decoded to would be worse than not washing.
        for bad in [
            "",
            "#",
            "7aa2f7",
            "#7aa2f",
            "#7aa2f7f",
            "#zzzzzz",
            "rebeccapurple",
        ] {
            assert!(parse_accent(bad).is_none(), "{bad:?} should not parse");
        }
    }
}
