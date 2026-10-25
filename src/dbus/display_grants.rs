//! Following the workspace registry's grants for the clients it boxes, and
//! asking it about the requests they hold.
//!
//! Grants are read again whenever a policy, a declared row or the workspace
//! list changes, and when the registry comes or goes; never on a timer.

use std::os::fd::AsFd;

use calloop::LoopHandle;
use futures_executor::ThreadPool;
use futures_util::StreamExt;
use kora_workspaces_client::display::{self, Verdict};
use tracing::{debug, warn};

use crate::display_grants::{Answer, Question, Table};
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
        if let Err(err) = watch(tx).await {
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
                let verdict = judge(&conn, &question).await;
                let _ = answers.send((question.client, question.ask, verdict));
            });
        }
    });
}

/// The registry's verdict, waiting past its hold on the same question.
async fn judge(conn: &zbus::Connection, question: &Question) -> Option<Verdict> {
    let (class, change) = question.ask;
    loop {
        let verdict = display::judge(
            conn,
            question.pidfd.as_fd(),
            &question.workspace,
            question.kind,
            &question.id,
            class.name(),
            change,
        )
        .await;
        match verdict {
            Ok(Verdict::Pending) => continue,
            Ok(verdict) => return Some(verdict),
            Err(err) => {
                warn!(%err, "the registry could not judge a held request");
                return None;
            }
        }
    }
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
    while events.next().await.is_some() {
        if tx.send(read(&conn).await).is_err() {
            break;
        }
    }
    // With the connection gone nothing can say a grant still holds.
    let _ = tx.send(Table::default());
    Ok(())
}

/// The registry's grants; none when it is away, so every boxed class asks
/// and, with nobody to answer, is refused.
async fn read(conn: &zbus::Connection) -> Table {
    display::grants(conn)
        .await
        .map(Table::from_grants)
        .unwrap_or_default()
}
