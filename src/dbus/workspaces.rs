//! Following the active workspace, and its colour.
//!
//! The registry (`one.playtron.Workspaces1`) owns which workspace is on screen;
//! the compositor is a consumer and does not decide switches. `ListChanged`
//! matters as much as `ActiveChanged` because recolouring the workspace you are
//! standing in changes no active id.
//!
//! Two things gate it: `COSMIC_WORKSPACES` decides whether the session runs
//! workspaces at all, and [`Registry`] says whether one is actually answering.

use calloop::LoopHandle;
use futures_executor::ThreadPool;
use tracing::{debug, warn};

use crate::state::State;

const DEST: &str = "one.playtron.Workspaces1";
const PATH: &str = "/one/playtron/Workspaces1";

/// Whether this session runs workspaces at all.
///
/// Off unless `COSMIC_WORKSPACES` says otherwise, so a build with no registry
/// grows no vertical axis, no switch gesture and no bus connection.
pub fn enabled() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED.get_or_init(|| crate::utils::env::bool_var("COSMIC_WORKSPACES").unwrap_or(false))
}

/// The workspace on screen, as the registry describes it.
#[derive(Debug, Clone, PartialEq)]
pub struct ActiveWorkspace {
    pub id: String,
    /// Retained independently of transition rendering for workspace styling.
    pub accent: Option<[f32; 3]>,
}

/// What the compositor knows about the registry.
///
/// With none the switch gesture is not ours to intercept; one that says nothing
/// is active yet still owns those keys.
#[derive(Debug, Clone, PartialEq)]
pub enum Registry {
    Absent,
    Present {
        active: Option<ActiveWorkspace>,
        /// Every workspace whose services are up, the active one included.
        /// Each needs a realm, or its desktops cannot be shown until visited.
        running: Vec<String>,
        /// Every workspace the registry lists, whatever its tier. A realm for
        /// an id that is NOT in here belongs to a workspace that has been
        /// deleted, and goes with it — `running` cannot say that, since a
        /// workspace going cold leaves it too.
        known: Vec<String>,
    },
}

pub fn init(handle: &LoopHandle<'static, State>, executor: &ThreadPool) {
    if !enabled() {
        debug!("COSMIC_WORKSPACES is not set; workspaces stay off");
        return;
    }

    let (tx, rx) = calloop::channel::channel::<Registry>();

    if let Err(err) = handle.insert_source(rx, |event, _, state| {
        if let calloop::channel::Event::Msg(registry) = event {
            debug!(?registry, "workspace registry changed");
            state.set_workspace_registry(registry);
        }
    }) {
        warn!(?err, "Failed to register workspace channel");
        return;
    }

    executor.spawn_ok(async move {
        if let Err(err) = watch(tx).await {
            debug!(%err, "stopped following the workspace registry");
        }
    });
}

async fn watch(tx: calloop::channel::Sender<Registry>) -> zbus::Result<()> {
    let conn = zbus::Connection::session().await?;

    let from_registry = |member: &'static str| -> zbus::Result<zbus::MatchRule<'static>> {
        Ok(zbus::MatchRule::builder()
            .msg_type(zbus::message::Type::Signal)
            .sender(DEST)?
            .interface(DEST)?
            .member(member)?
            .build())
    };

    // Subscribe before the first read. Watching the name is what stops the
    // start order from mattering — the registry and the compositor come up
    // together — and picks the registry up again when it restarts.
    let mut events = futures_util::stream::select_all([
        zbus::MessageStream::for_match_rule(from_registry("ActiveChanged")?, &conn, None).await?,
        zbus::MessageStream::for_match_rule(from_registry("ListChanged")?, &conn, None).await?,
        zbus::MessageStream::for_match_rule(
            zbus::MatchRule::builder()
                .msg_type(zbus::message::Type::Signal)
                .sender("org.freedesktop.DBus")?
                .interface("org.freedesktop.DBus")?
                .member("NameOwnerChanged")?
                // Only this name; unfiltered, every service coming or going
                // would wake us.
                .add_arg(DEST)?
                .build(),
            &conn,
            None,
        )
        .await?,
    ]);

    let _ = tx.send(read(&conn).await);

    use futures_util::stream::StreamExt;
    // ActiveChanged carries only an id; ListChanged carries accent updates.
    while events.next().await.is_some() {
        if tx.send(read(&conn).await).is_err() {
            break;
        }
    }

    // The streams end only with the connection. Say so rather than keep hiding
    // clients for a registry we can no longer hear.
    let _ = tx.send(Registry::Absent);
    Ok(())
}

/// The active workspace and its colour, which workspaces are running, or
/// whether there is a registry at all.
async fn read(conn: &zbus::Connection) -> Registry {
    // Nobody owns the name, or whoever does will not answer — the same thing
    // from here.
    let Ok(reply) = conn
        .call_method(Some(DEST), PATH, Some(DEST), "Active", &())
        .await
    else {
        return Registry::Absent;
    };
    let Ok(id) = reply.body().deserialize::<String>() else {
        return Registry::Absent;
    };

    // id, name, accent, tier, pinned.
    let rows = conn
        .call_method(Some(DEST), PATH, Some(DEST), "List", &())
        .await
        .ok()
        .and_then(|reply| {
            reply
                .body()
                .deserialize::<Vec<(String, String, String, String, bool)>>()
                .ok()
        })
        .unwrap_or_default();
    let running = rows
        .iter()
        .filter(|(_, _, _, tier, _)| tier != "cold")
        .map(|(row_id, ..)| row_id.clone())
        .collect();
    let known: Vec<String> = rows.iter().map(|(row_id, ..)| row_id.clone()).collect();

    // "Nothing active" comes back as an empty string; taken at face value it
    // would tag every client with an id nothing can match.
    if id.is_empty() {
        return Registry::Present {
            active: None,
            running,
            known,
        };
    }

    let accent = rows
        .iter()
        .find(|(row_id, ..)| *row_id == id)
        .and_then(|(_, _, accent, ..)| parse_accent(accent));

    Registry::Present {
        active: Some(ActiveWorkspace { id, accent }),
        running,
        known,
    }
}

/// Parse a registry `#rrggbb`, leaving malformed values for styling fallback.
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

/// Ask the registry to step `delta` workspaces along.
///
/// Fire and forget: the switch comes back through `ActiveChanged`, so a
/// keypress and a click on the panel take the same path. The gate is upstream —
/// this is only reachable while a registry is answering.
pub fn cycle(delta: i32) {
    std::thread::spawn(move || {
        let result = futures_executor::block_on(async {
            let conn = zbus::Connection::session().await?;
            conn.call_method(Some(DEST), PATH, Some(DEST), "Cycle", &(delta,))
                .await
        });
        if let Err(err) = result {
            debug!(%err, delta, "workspace cycle went nowhere");
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
