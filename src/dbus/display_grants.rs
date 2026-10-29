//! Following the workspace registry's grants for the clients it boxes, and
//! asking it about the requests they hold.
//!
//! Grants are read again whenever a policy, a declared row or the workspace
//! list changes, and when the registry comes or goes; never on a timer.

use std::os::fd::{AsFd, BorrowedFd};
use std::sync::atomic::{AtomicBool, Ordering};

use calloop::LoopHandle;
use futures_executor::ThreadPool;
use futures_util::StreamExt;
use kora_workspaces_client::display::{self, Verdict};
use tracing::{debug, warn};

use crate::display_grants::{Answer, Ask, Question, Table};
use crate::state::State;

const DEST: &str = "one.playtron.Workspaces1";

pub fn init(handle: &LoopHandle<'static, State>, executor: &ThreadPool) {
    if !super::workspaces::enabled() {
        return;
    }
    let (tx, rx) = calloop::channel::channel::<Table>();
    if let Err(err) = handle.insert_source(rx, |event, _, state| {
        if let calloop::channel::Event::Msg(table) = event {
            debug!(?table, "display grants changed");
            let dh = state.common.display_handle.clone();
            let old = state.common.display_grants.replace(table, &dh);
            crate::display_grants::table_changed(state, &old);
        }
    }) {
        warn!(?err, "Failed to register the display grants channel");
        return;
    }
    let (answers, answered) = calloop::channel::channel::<Answer>();
    if let Err(err) = handle.insert_source(answered, |event, _, state| {
        if let calloop::channel::Event::Msg(answer) = event {
            crate::display_grants::answered(state, answer);
        }
    }) {
        warn!(?err, "Failed to register the display answers channel");
        return;
    }
    let (questions, mut asked) = futures_channel::mpsc::unbounded::<Question>();
    handle.insert_idle(move |state| state.common.display_asks.set_questions(questions));
    executor.spawn_ok(async move {
        let followed = async {
            let conn = zbus::Connection::session().await?;
            watch(&conn, |table| tx.send(table).is_ok()).await
        };
        if let Err(err) = followed.await {
            debug!(%err, "stopped following display grants");
        }
    });
    let pool = executor.clone();
    executor.spawn_ok(async move {
        let conn = match zbus::Connection::session().await {
            Ok(conn) => conn,
            Err(err) => {
                warn!(%err, "cannot ask the registry about held requests");
                // Each question is answered as unanswerable, so its requests fail.
                while let Some(question) = asked.next().await {
                    let _ = answers.send((question.client, question.ask, None));
                }
                return;
            }
        };
        while let Some(question) = asked.next().await {
            let (conn, answers) = (conn.clone(), answers.clone());
            pool.spawn_ok(async move {
                let verdict = judge(
                    &conn,
                    question.pidfd.as_fd(),
                    &question.workspace,
                    question.kind,
                    &question.id,
                    question.ask,
                )
                .await;
                let _ = answers.send((question.client, question.ask, verdict));
            });
        }
    });
}

/// The registry's verdict, waiting past its hold on the same question; `None`
/// when it is away or predates the question, which refuses the request.
async fn judge(
    conn: &zbus::Connection,
    pidfd: BorrowedFd<'_>,
    workspace: &str,
    kind: &str,
    id: &str,
    (class, change): Ask,
) -> Option<Verdict> {
    // Said once per absence: every held request fails the same way until it returns.
    static AWAY: AtomicBool = AtomicBool::new(false);
    loop {
        let verdict = display::judge(conn, pidfd, workspace, kind, id, class.name(), change).await;
        match verdict {
            Ok(Verdict::Pending) => continue,
            Ok(verdict) => {
                AWAY.store(false, Ordering::Relaxed);
                return Some(verdict);
            }
            Err(err) if AWAY.swap(true, Ordering::Relaxed) => {
                debug!(%err, "the registry could not judge a held request");
                return None;
            }
            Err(err) => {
                warn!(%err, "the registry could not judge a held request; refusing them until it can");
                return None;
            }
        }
    }
}

/// Hand `emit` the grants at the start and after every change, until it
/// returns false or the bus goes.
async fn watch(conn: &zbus::Connection, mut emit: impl FnMut(Table) -> bool) -> zbus::Result<()> {
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
        zbus::MessageStream::for_match_rule(from_registry("AgentPolicyChanged")?, conn, None)
            .await?,
        zbus::MessageStream::for_match_rule(
            from_registry("AgentPermissionRowsChanged")?,
            conn,
            None,
        )
        .await?,
        zbus::MessageStream::for_match_rule(from_registry("ListChanged")?, conn, None).await?,
        zbus::MessageStream::for_match_rule(
            zbus::MatchRule::builder()
                .msg_type(zbus::message::Type::Signal)
                .sender("org.freedesktop.DBus")?
                .interface("org.freedesktop.DBus")?
                .member("NameOwnerChanged")?
                .add_arg(DEST)?
                .build(),
            conn,
            None,
        )
        .await?,
    ]);

    if !emit(read(conn).await) {
        return Ok(());
    }
    while events.next().await.is_some() {
        if !emit(read(conn).await) {
            return Ok(());
        }
    }
    // With the connection gone nothing can say a grant still holds.
    emit(Table::default());
    Ok(())
}

/// The registry's grants; none when it is away or predates them, so every
/// boxed class asks and, with nobody to answer, is refused.
async fn read(conn: &zbus::Connection) -> Table {
    display::grants(conn)
        .await
        .map(Table::from_grants)
        .unwrap_or_default()
}

#[cfg(test)]
#[path = "display_grants_tests.rs"]
mod tests;
