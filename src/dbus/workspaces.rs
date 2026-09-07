//! Following the active workspace.
//!
//! The workspace registry (`one.playtron.Workspaces1`) is the single source of
//! truth for which workspace is on screen; the compositor is a consumer. It
//! learns about switches from `ActiveChanged` and does not decide them —
//! residency, provisioning and slice management all live in the registry.
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

pub fn init(handle: &LoopHandle<'static, State>, executor: &ThreadPool) {
    let (tx, rx) = calloop::channel::channel::<Option<String>>();

    if let Err(err) = handle.insert_source(rx, |event, _, state| {
        if let calloop::channel::Event::Msg(active) = event {
            debug!(
                workspace = active.as_deref().unwrap_or("<none>"),
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

async fn watch(tx: calloop::channel::Sender<Option<String>>) -> zbus::Result<()> {
    let conn = zbus::Connection::session().await?;

    // Ask once, so a compositor started after the registry is already correct
    // rather than waiting for the next switch.
    let active = conn
        .call_method(Some(DEST), PATH, Some(DEST), "Active", &())
        .await
        .ok()
        .and_then(|reply| reply.body().deserialize::<String>().ok());
    let _ = tx.send(active);

    let rule = zbus::MatchRule::builder()
        .msg_type(zbus::message::Type::Signal)
        .sender(DEST)?
        .interface(DEST)?
        .member("ActiveChanged")?
        .build();
    let mut stream = zbus::MessageStream::for_match_rule(rule, &conn, None).await?;

    use futures_util::stream::StreamExt;
    while let Some(msg) = stream.next().await {
        let Ok(msg) = msg else { continue };
        let id = msg.body().deserialize::<String>().ok();
        if tx.send(id).is_err() {
            break;
        }
    }
    Ok(())
}
