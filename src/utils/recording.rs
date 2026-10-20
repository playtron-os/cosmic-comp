//! The halo's Record: a window's recording through agentos-capture.
//!
//! The compositor never owns the recorder process. It asks
//! `one.playtron.Capture1` to start and stop, keeps the recording id on the
//! window so the halo can show it, and hears every ending through
//! `RecordingStopped` — the halo's own stop and a window that closed alike.

use std::path::{Path, PathBuf};
use std::time::Instant;

use tracing::warn;

use crate::{
    dbus::notifications::{Notification, Tone},
    fl,
    shell::element::{CosmicSurface, surface::Recording},
    state::State,
    utils::captures::{self, CaptureKind},
    wayland::protocols::toplevel_info::foreign_toplevel_identifier,
};

/// A freedesktop icon name for the toasts.
const NOTIFICATION_ICON: &str = "media-record";
const NOTIFICATION_TIMEOUT_MS: i32 = 5000;

/// Start recording `surface`, or stop the recording it has.
pub fn toggle(state: &mut State, surface: &CosmicSurface) {
    match surface.recording() {
        // A request is on the bus; the answer decides.
        Recording::Starting => {}
        Recording::Idle => start(state, surface),
        Recording::Active { id, since, .. } => {
            let surface = surface.clone();
            let stopping = id.clone();
            state
                .common
                .dbus_state
                .stop_recording(id, move |state, reply| {
                    // The recorder's RecordingStopped may have landed first
                    // and already cleared and announced it; one toast, not two.
                    if !carries(&surface, &stopping) {
                        return;
                    }
                    match reply {
                        Ok(path) => finished(state, &surface, &path, since),
                        Err(error) => {
                            warn!(%error, "Failed to stop the recording");
                            surface.set_recording(Recording::Idle);
                            refresh_halo(state, &surface);
                            notify(state, fl!("recording-failed"), error);
                        }
                    }
                });
        }
    }
}

fn start(state: &mut State, surface: &CosmicSurface) {
    let Some(identifier) = foreign_toplevel_identifier(surface) else {
        notify(
            state,
            fl!("recording-failed"),
            fl!("recording-window-not-capturable"),
        );
        return;
    };
    let Some(destination) =
        captures::Destination::for_surface(state, surface, CaptureKind::Recording)
    else {
        notify(state, fl!("recording-failed"), "HOME is not set".into());
        return;
    };
    // It waits here while its preview is up, as a screenshot does.
    if let Err(err) = std::fs::create_dir_all(&destination.pending) {
        notify(state, fl!("recording-failed"), err.to_string());
        return;
    }
    let path = destination.pending.join(format!(
        "{}.mp4",
        captures::capture_stem(CaptureKind::Recording, &jiff::Zoned::now())
    ));
    write_poster(state, surface, &path);

    surface.set_recording(Recording::Starting);
    refresh_halo(state, surface);
    let surface = surface.clone();
    let path_string = path.to_string_lossy().into_owned();
    state.common.dbus_state.start_recording(
        identifier,
        path_string,
        move |state, reply| match reply {
            Ok(id) => {
                // Only a request still waiting takes the answer; a window
                // closed meanwhile is Idle and its recording ends on its own.
                if surface.recording() == Recording::Starting {
                    let since = std::time::Instant::now();
                    surface.set_recording(Recording::Active { id, path, since });
                    let notification = notification(fl!("recording-started"), String::new());
                    state.common.dbus_state.system_toast_or(
                        fl!("recording-started"),
                        Tone::Destructive,
                        notification,
                    );
                }
                refresh_halo(state, &surface);
            }
            Err(error) => {
                warn!(%error, "Failed to start a recording");
                surface.set_recording(Recording::Idle);
                refresh_halo(state, &surface);
                notify(state, fl!("recording-failed"), error);
            }
        },
    );
}

/// `RecordingStopped` from the recorder: the halo's stop, a window that
/// closed, or a recording that broke.
pub fn stopped(state: &mut State, id: &str, path: &str, error: &str) {
    let Some(surface) = surface_recording(state, id) else {
        return;
    };
    let Recording::Active { since, .. } = surface.recording() else {
        return;
    };
    if error.is_empty() {
        finished(state, &surface, path, since);
    } else {
        warn!(%id, %error, "Recording ended with an error");
        surface.set_recording(Recording::Idle);
        refresh_halo(state, &surface);
        notify(state, fl!("recording-failed"), error.to_owned());
    }
}

/// The window as the recording starts, for the preview to show. Nothing is
/// lost without it: the preview draws a stand-in.
fn write_poster(state: &mut State, surface: &CosmicSurface, recording: &Path) {
    let poster = poster_of(recording);
    let file = match std::fs::File::create(&poster) {
        Ok(file) => file,
        Err(err) => return warn!(?err, "Failed to create the recording's poster"),
    };
    crate::utils::screenshot::write_window(state, surface, 1.0, file, move |written| {
        if let Err(err) = written {
            warn!(%err, "Failed to write the recording's poster");
            let _ = std::fs::remove_file(&poster);
        }
    });
}

fn poster_of(recording: &Path) -> PathBuf {
    recording.with_extension("poster.png")
}

/// The recording is on disk: hand it to the preview, or file it and say so.
fn finished(state: &mut State, surface: &CosmicSurface, path: &str, since: Instant) {
    surface.set_recording(Recording::Idle);
    refresh_halo(state, surface);
    let path = PathBuf::from(path);
    let poster = Some(poster_of(&path)).filter(|p| p.is_file());
    let Some(destination) =
        captures::Destination::for_surface(state, surface, CaptureKind::Recording)
    else {
        return announce(state, &path);
    };
    let app =
        crate::utils::screenshot::app_name(&surface.app_id()).unwrap_or_else(|| surface.title());
    let length = u64::try_from(since.elapsed().as_millis()).unwrap_or(u64::MAX);
    let handoff = destination.handoff(&path, &app, poster.as_deref(), Some(length));
    state
        .common
        .dbus_state
        .show_capture(handoff, move |state, handoff| {
            if let Some(poster) = poster {
                let _ = std::fs::remove_file(poster);
            }
            match captures::file_into(&handoff.path, &handoff.save_to) {
                Ok(filed) => announce(state, &filed),
                Err(err) => {
                    warn!(?err, "Failed to file the recording");
                    announce(state, &handoff.path);
                }
            }
        });
}

fn announce(state: &State, path: &Path) {
    let notification = notification(fl!("recording-stopped"), path.display().to_string());
    state.common.dbus_state.system_toast_or(
        fl!("recording-stopped-toast"),
        Tone::Neutral,
        notification,
    );
}

/// Whether `surface` is recording as `id` right now.
fn carries(surface: &CosmicSurface, id: &str) -> bool {
    matches!(surface.recording(), Recording::Active { id: ref active, .. } if active == id)
}

/// The window whose recording carries `id`, mapped or fullscreen.
fn surface_recording(state: &State, id: &str) -> Option<CosmicSurface> {
    let shell = state.common.shell.read();
    shell
        .mapped()
        .flat_map(|mapped| mapped.windows().map(|(window, _)| window))
        .find(|surface| carries(surface, id))
        .or_else(|| {
            shell
                .workspaces()
                .spaces()
                .flat_map(|workspace| workspace.get_fullscreen_surfaces())
                .map(|fullscreen| fullscreen.surface.clone())
                .find(|surface| carries(surface, id))
        })
}

/// Redraw the halo so its Record glyph shows the new state.
fn refresh_halo(state: &State, surface: &CosmicSurface) {
    crate::shell::element::window::CosmicWindow::refresh_halo(&state.common.shell.read(), surface);
}

fn notify(state: &State, summary: String, body: String) {
    state.common.dbus_state.notify(notification(summary, body));
}

fn notification(summary: String, body: String) -> Notification {
    Notification {
        app_name: fl!("recording-app-name"),
        app_icon: NOTIFICATION_ICON.to_owned(),
        summary,
        body,
        expire_timeout: NOTIFICATION_TIMEOUT_MS,
        transient: true,
    }
}
