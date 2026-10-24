//! Following the workspace registry's grants for the clients it boxes.
//!
//! Read again whenever a policy, a declared row or the workspace list changes,
//! and when the registry comes or goes; never on a timer.

use calloop::LoopHandle;
use futures_executor::ThreadPool;
use tracing::{debug, warn};

use crate::display_grants::Table;
use crate::state::State;

const DEST: &str = "one.playtron.Workspaces1";
const PATH: &str = "/one/playtron/Workspaces1";

pub fn init(handle: &LoopHandle<'static, State>, executor: &ThreadPool) {
    if !super::workspaces::enabled() {
        return;
    }
    let (tx, rx) = calloop::channel::channel::<Table>();
    if let Err(err) = handle.insert_source(rx, |event, _, state| {
        if let calloop::channel::Event::Msg(table) = event {
            debug!(?table, "display grants changed");
            let dh = state.common.display_handle.clone();
            state.common.display_grants.replace(table, &dh);
        }
    }) {
        warn!(?err, "Failed to register the display grants channel");
        return;
    }
    executor.spawn_ok(async move {
        if let Err(err) = watch(tx).await {
            debug!(%err, "stopped following display grants");
        }
    });
}

async fn watch(tx: calloop::channel::Sender<Table>) -> zbus::Result<()> {
    let conn = zbus::Connection::session().await?;
    let from_registry = |member: &'static str| -> zbus::Result<zbus::MatchRule<'static>> {
        Ok(zbus::MatchRule::builder()
            .msg_type(zbus::message::Type::Signal)
            .sender(DEST)?
            .interface(DEST)?
            .member(member)?
            .build())
    };
    // Subscribed before the first read, so a change in between is not lost.
    let mut events = futures_util::stream::select_all([
        zbus::MessageStream::for_match_rule(from_registry("AgentPolicyChanged")?, &conn, None)
            .await?,
        zbus::MessageStream::for_match_rule(
            from_registry("AgentPermissionRowsChanged")?,
            &conn,
            None,
        )
        .await?,
        zbus::MessageStream::for_match_rule(from_registry("ListChanged")?, &conn, None).await?,
        zbus::MessageStream::for_match_rule(
            zbus::MatchRule::builder()
                .msg_type(zbus::message::Type::Signal)
                .sender("org.freedesktop.DBus")?
                .interface("org.freedesktop.DBus")?
                .member("NameOwnerChanged")?
                .add_arg(DEST)?
                .build(),
            &conn,
            None,
        )
        .await?,
    ]);

    let _ = tx.send(read(&conn).await);
    use futures_util::stream::StreamExt;
    while events.next().await.is_some() {
        if tx.send(read(&conn).await).is_err() {
            break;
        }
    }
    // With the connection gone nothing can say a grant still holds.
    let _ = tx.send(Table::default());
    Ok(())
}

/// The registry's grants; none when it is away or predates them.
async fn read(conn: &zbus::Connection) -> Table {
    conn.call_method(Some(DEST), PATH, Some(DEST), "DisplayGrants", &())
        .await
        .ok()
        .and_then(|reply| {
            reply
                .body()
                .deserialize::<Vec<(String, String, String, Vec<String>)>>()
                .ok()
        })
        .map(Table::from_rows)
        .unwrap_or_default()
}
