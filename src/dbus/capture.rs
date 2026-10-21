//! The recorder behind the halo's Record: `one.playtron.CaptureEngine1`, served by
//! kora-capture-engine and started on demand by the bus, through its client crate.

use anyhow::{Context, Result};
use calloop::{InsertError, stream::StreamSource};
use kora_capture_client::engine::{CaptureEngine1Proxy, Codec, RecordOptions, Source};
use tracing::debug;

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
    let options = RecordOptions {
        codec: Some(Codec::Auto),
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

/// Follow `RecordingStopped` for as long as the session bus lives: every
/// recording ends through it, whether the halo stopped it, the window closed,
/// or the recorder failed. The match survives the recorder starting and
/// stopping; nothing here needs it to be running.
pub(super) async fn watch_stopped(state: DBusState) -> Result<()> {
    let conn = state.session_conn().await?.clone();
    let proxy = CaptureEngine1Proxy::new(&conn).await.context("recorder proxy")?;
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
