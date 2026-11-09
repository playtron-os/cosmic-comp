//! The recorder behind the halo's Record: `one.playtron.CaptureEngine1`, served by
//! kora-capture-engine and started on demand by the bus, through its client crate.

use std::collections::HashMap;

use anyhow::{Context, Result};
use calloop::{InsertError, stream::StreamSource};
use futures_util::{StreamExt, stream::select};
use kora_capture_client::engine::{BUS_NAME, CaptureEngine1Proxy, Codec, RecordOptions, Source};
use tracing::debug;
use zbus::fdo::DBusProxy;
use zbus::names::BusName;
use zbus::proxy::CacheProperties;

use super::DBusState;
use crate::state::State;

/// The recorder's answer, back on the compositor thread.
pub type Reply = Result<String, String>;

impl DBusState {
    /// Record the toplevel `identifier` into `path`. `done` gets the recording id.
    pub fn start_recording(
        &self,
        identifier: String,
        path: String,
        done: impl FnOnce(&mut State, Reply) + 'static,
    ) {
        let state = self.clone();
        let evlh = self.0.evlh.clone();
        self.spawn(async move {
            let reply = start(&state, &identifier, &path)
                .await
                .map_err(|err| err.to_string());
            evlh.insert_idle(move |state| done(state, reply));
        });
    }

    /// Finish the recording `id`. `done` gets the file's path.
    pub fn stop_recording(&self, id: String, done: impl FnOnce(&mut State, Reply) + 'static) {
        let state = self.clone();
        let evlh = self.0.evlh.clone();
        self.spawn(async move {
            let reply = stop(&state, &id).await.map_err(|err| err.to_string());
            evlh.insert_idle(move |state| done(state, reply));
        });
    }
}

async fn start(state: &DBusState, identifier: &str, path: &str) -> zbus::Result<String> {
    let conn = state.session_conn().await?;
    let proxy = CaptureEngine1Proxy::new(conn).await?;
    // A recording shows the pointer, as Kora Capture's do by default.
    let options = RecordOptions {
        codec: Some(Codec::Auto),
        cursor: true,
        ..RecordOptions::default()
    };
    proxy
        .start_recording(&Source::toplevel(identifier), path, &options)
        .await
}

async fn stop(state: &DBusState, id: &str) -> zbus::Result<String> {
    let conn = state.session_conn().await?;
    let proxy = CaptureEngine1Proxy::new(conn).await?;
    proxy.stop_recording(id).await
}

enum Heard {
    /// The engine started or stopped being there, or started a recording.
    Refresh,
    Stopped {
        id: String,
        path: String,
        error: String,
    },
}

/// Mirror the engine's recordings onto game mode: what it lists right now,
/// re-read each time it says anything or comes and goes, so what game mode
/// shows is the engine's own answer. Never starts it: the list is only read
/// while it has an owner, and it serves every recording it has.
pub(super) async fn watch_recordings(state: DBusState) -> Result<()> {
    let conn = state.session_conn().await?.clone();
    let dbus = DBusProxy::new(&conn).await.context("bus proxy")?;
    let proxy = CaptureEngine1Proxy::builder(&conn)
        .cache_properties(CacheProperties::No)
        .build()
        .await
        .context("recorder proxy")?;
    // Subscribed first, so a change in between is not missed.
    let owners = dbus
        .receive_name_owner_changed_with_args(&[(0, BUS_NAME)])
        .await
        .context("subscribe to the recorder's name")?
        .map(|_| Heard::Refresh);
    let started = proxy
        .receive_recording_started()
        .await
        .context("subscribe to RecordingStarted")?
        .map(|_| Heard::Refresh);
    let stopped = proxy
        .receive_recording_stopped()
        .await
        .context("subscribe to RecordingStopped")?
        .filter_map(|signal| {
            std::future::ready(signal.args().ok().map(|args| Heard::Stopped {
                id: args.id.clone(),
                path: args.path.clone(),
                error: args.error.clone(),
            }))
        });
    let mut events = Box::pin(select(owners, select(started, stopped)));

    let mut known: HashMap<String, String> = HashMap::new();
    mirror(&state, &dbus, &proxy, Heard::Refresh, &mut known).await;
    while let Some(heard) = events.next().await {
        mirror(&state, &dbus, &proxy, heard, &mut known).await;
    }
    Ok(())
}

async fn mirror(
    state: &DBusState,
    dbus: &DBusProxy<'_>,
    proxy: &CaptureEngine1Proxy<'_>,
    heard: Heard,
    known: &mut HashMap<String, String>,
) {
    let mut ended = Vec::new();
    if let Heard::Stopped { id, path, error } = heard {
        known.remove(&id);
        ended.push((String::new(), path, error));
    }
    let name = BusName::from_static_str(BUS_NAME).expect("the recorder's name is valid");
    // None when the engine is there but did not answer: nothing changes then.
    let listed = if dbus.name_has_owner(name).await.unwrap_or(false) {
        proxy.list_recordings().await.ok()
    } else {
        Some(Vec::new())
    };
    let snapshot = listed.map(|listed| {
        // What the engine dropped without a RecordingStopped went with its process.
        known.retain(|id, path| {
            let live = listed.iter().any(|recording| recording.id == *id);
            if !live {
                ended.push((id.clone(), path.clone(), "the recorder stopped".to_owned()));
            }
            live
        });
        for recording in &listed {
            known.insert(recording.id.clone(), recording.path.clone());
        }
        let since = listed.iter().map(|r| r.started).min().unwrap_or(0);
        (since, listed.into_iter().map(|r| r.id).collect::<Vec<_>>())
    });
    state.0.evlh.insert_idle(move |state| {
        if let Some((since, ids)) = snapshot {
            state.common.game_mode_bridge.set_recordings(since, ids);
        }
        for (id, path, error) in ended {
            if !id.is_empty() {
                crate::utils::recording::stopped(state, &id, &path, &error);
            }
            state.common.game_mode_bridge.notify_recording_ended(
                path,
                if error.is_empty() {
                    String::new()
                } else {
                    crate::utils::recording::failure_code(&error).to_owned()
                },
            );
        }
    });
}

/// Follow `RecordingStopped` for as long as the session bus lives: every
/// recording ends through it, whether the halo stopped it, the window closed,
/// or the recorder failed. The match survives the recorder starting and
/// stopping; nothing here needs it to be running.
pub(super) async fn watch_stopped(state: DBusState) -> Result<()> {
    let conn = state.session_conn().await?.clone();
    let proxy = CaptureEngine1Proxy::new(&conn)
        .await
        .context("recorder proxy")?;
    let stream = proxy
        .receive_recording_stopped()
        .await
        .context("subscribe to RecordingStopped")?;
    let source = StreamSource::new(stream).unwrap();
    state
        .0
        .evlh
        .insert_source(source, |signal, _, state| {
            let Some(signal) = signal else {
                return;
            };
            let Ok(args) = signal.args() else {
                return;
            };
            debug!(id = %args.id, path = %args.path, error = %args.error, "recording stopped");
            crate::utils::recording::stopped(state, &args.id, &args.path, &args.error);
        })
        .map_err(|InsertError { error, .. }| error)
        .context("add RecordingStopped to the event loop")?;
    Ok(())
}

/// Follow another app's stills and recordings of a window (Kora Capture's), so
/// that window's halo plays them as if its own buttons had.
pub(super) async fn watch_windows(state: DBusState) -> Result<()> {
    let conn = state.session_conn().await?.clone();
    let proxy = CaptureEngine1Proxy::new(&conn)
        .await
        .context("recorder proxy")?;
    let captured = proxy
        .receive_window_captured()
        .await
        .context("subscribe to WindowCaptured")?;
    let started = proxy
        .receive_window_recording_started()
        .await
        .context("subscribe to WindowRecordingStarted")?;
    let evlh = &state.0.evlh;
    evlh.insert_source(StreamSource::new(captured).unwrap(), |signal, _, state| {
        let Some(Ok(args)) = signal.as_ref().map(|s| s.args()) else {
            return;
        };
        debug!(window = %args.identifier, "window captured");
        crate::utils::screenshot::captured_elsewhere(state, &args.identifier);
    })
    .map_err(|InsertError { error, .. }| error)
    .context("add WindowCaptured to the event loop")?;
    evlh.insert_source(StreamSource::new(started).unwrap(), |signal, _, state| {
        let Some(Ok(args)) = signal.as_ref().map(|s| s.args()) else {
            return;
        };
        debug!(id = %args.id, window = %args.identifier, "window recording started");
        crate::utils::recording::started_elsewhere(state, &args.id, &args.identifier);
    })
    .map_err(|InsertError { error, .. }| error)
    .context("add WindowRecordingStarted to the event loop")?;
    Ok(())
}
