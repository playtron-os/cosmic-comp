// SPDX-License-Identifier: GPL-3.0-only

//! Following the machine's run registry, `one.playtron.Runs1`.
//!
//! `Changed` carries only a generation, so every change is a fresh `List`.
//! Watching the name as well means the start order does not matter and a
//! restarted registry is picked up again; with no registry there are no runs.

use std::collections::HashMap;

use calloop::LoopHandle;
use futures_executor::ThreadPool;
use futures_util::StreamExt;
use tracing::{debug, warn};
use zbus::zvariant::OwnedValue;

use crate::{
    shell::element::window::runs::{Run, RunState},
    state::State,
};

const DEST: &str = "one.playtron.Runs1";
const PATH: &str = "/one/playtron/Runs1";

type Fields = HashMap<String, OwnedValue>;

pub fn init(handle: &LoopHandle<'static, State>, executor: &ThreadPool) {
    let (tx, rx) = calloop::channel::channel::<Vec<Run>>();
    if let Err(err) = handle.insert_source(rx, |event, _, state| {
        if let calloop::channel::Event::Msg(runs) = event {
            crate::shell::element::window::runs::apply(state, runs);
        }
    }) {
        warn!(?err, "Failed to register the runs channel");
        return;
    }
    executor.spawn_ok(async move {
        let followed = async {
            let conn = zbus::Connection::session().await?;
            watch(&conn, |runs| tx.send(runs).is_ok()).await
        };
        if let Err(err) = followed.await {
            debug!(%err, "stopped following the run registry");
        }
    });
}

/// Hand `emit` every new snapshot, until it returns false or the bus goes.
async fn watch(
    conn: &zbus::Connection,
    mut emit: impl FnMut(Vec<Run>) -> bool,
) -> zbus::Result<()> {
    let mut events = futures_util::stream::select_all([
        zbus::MessageStream::for_match_rule(
            zbus::MatchRule::builder()
                .msg_type(zbus::message::Type::Signal)
                .sender(DEST)?
                .interface(DEST)?
                .member("Changed")?
                .build(),
            conn,
            None,
        )
        .await?,
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

    let mut last = None;
    loop {
        let snapshot = read(conn).await;
        let key = snapshot
            .as_ref()
            .map(|(owner, generation, _)| (owner.clone(), *generation));
        // A generation only grows under one owner; anything else is a restart.
        let stale = matches!((&last, &key), (Some((owner, seen)), Some((now, generation)))
            if owner == now && generation <= seen);
        if !stale && (key.is_some() || last.is_some()) {
            let runs = snapshot.map(|(_, _, runs)| runs).unwrap_or_default();
            if !emit(runs) {
                return Ok(());
            }
            last = key;
        }
        if events.next().await.is_none() {
            break;
        }
    }
    emit(Vec::new());
    Ok(())
}

async fn read(conn: &zbus::Connection) -> Option<(String, u64, Vec<Run>)> {
    let reply = conn
        .call_method(Some(DEST), PATH, Some(DEST), "List", &())
        .await
        .ok()?;
    let owner = reply.header().sender()?.to_string();
    let (generation, rows) = reply.body().deserialize::<(u64, Vec<Fields>)>().ok()?;
    Some((owner, generation, rows.iter().filter_map(parse).collect()))
}

fn text(fields: &Fields, name: &str) -> Option<String> {
    fields
        .get(name)
        .and_then(|value| <&str>::try_from(value).ok())
        .filter(|value| !value.is_empty())
        .map(str::to_owned)
}

fn millis(fields: &Fields, name: &str) -> Option<u64> {
    fields.get(name).and_then(|value| u64::try_from(value).ok())
}

/// One registry row, or `None` for a state the Halo does not show. A `kind`
/// is the design's test that the run is inference at all.
pub(crate) fn parse(fields: &Fields) -> Option<Run> {
    text(fields, "kind")?;
    let state = match text(fields, "state")?.as_str() {
        "running" => RunState::Running,
        "queued" => RunState::Queued,
        "done" => RunState::Done,
        _ => return None,
    };
    Some(Run {
        id: text(fields, "id")?,
        workspace: fields
            .get("workspace")
            .and_then(|value| <&str>::try_from(value).ok())?
            .to_owned(),
        window: text(fields, "window"),
        state,
        verb: text(fields, "verb"),
        title: text(fields, "title"),
        progress: fields
            .get("progress")
            .and_then(|value| f64::try_from(value).ok())
            .filter(|progress| progress.is_finite()),
        created: millis(fields, "created").unwrap_or_default(),
        ended: millis(fields, "ended"),
    })
}

#[cfg(test)]
#[path = "runs_tests.rs"]
mod tests;
