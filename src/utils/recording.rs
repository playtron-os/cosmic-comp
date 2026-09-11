//! The halo's Record: a window's recording through agentos-capture.
//!
//! The compositor never owns the recorder process. It asks
//! `one.playtron.Capture1` to start and stop, keeps the recording id on the
//! window so the halo can show it, and hears every ending through
//! `RecordingStopped` — the halo's own stop and a window that closed alike.

use tracing::warn;

use crate::{
    dbus::notifications::Notification,
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
        Recording::Active { id, .. } => {
            let surface = surface.clone();
            state
                .common
                .dbus_state
                .stop_recording(id, move |state, reply| match reply {
                    Ok(path) => finished(state, &surface, &path),
                    Err(error) => {
                        warn!(%error, "Failed to stop the recording");
                        surface.set_recording(Recording::Idle);
                        refresh_halo(state, &surface);
                        notify(state, fl!("recording-failed"), error);
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
    let Some(directory) = captures::directory_for(state, surface, CaptureKind::Recording) else {
        notify(state, fl!("recording-failed"), "HOME is not set".into());
        return;
    };
    let mut title = surface.title();
    if title.trim().is_empty() {
        title = surface.app_id();
    }
    if title.trim().is_empty() {
        title = fl!("recording-app-name");
    }
    let path = directory.join(format!(
        "{}.mp4",
        captures::file_stem(&title, &jiff::Zoned::now())
    ));

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
                    surface.set_recording(Recording::Active { id, path });
                    notify(state, fl!("recording-started"), String::new());
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
    if error.is_empty() {
        finished(state, &surface, path);
    } else {
        warn!(%id, %error, "Recording ended with an error");
        surface.set_recording(Recording::Idle);
        refresh_halo(state, &surface);
        notify(state, fl!("recording-failed"), error.to_owned());
    }
}

fn finished(state: &mut State, surface: &CosmicSurface, path: &str) {
    surface.set_recording(Recording::Idle);
    refresh_halo(state, surface);
    notify(state, fl!("recording-saved-to"), path.to_owned());
}

/// The window whose recording carries `id`, mapped or fullscreen.
fn surface_recording(state: &State, id: &str) -> Option<CosmicSurface> {
    let shell = state.common.shell.read();
    let carries = |surface: &CosmicSurface| matches!(surface.recording(), Recording::Active { id: ref active, .. } if active == id);
    shell
        .mapped()
        .flat_map(|mapped| mapped.windows().map(|(window, _)| window))
        .find(carries)
        .or_else(|| {
            shell
                .workspaces()
                .spaces()
                .flat_map(|workspace| workspace.get_fullscreen_surfaces())
                .map(|fullscreen| fullscreen.surface.clone())
                .find(carries)
        })
}

/// Redraw the halo so its Record glyph shows the new state.
fn refresh_halo(state: &State, surface: &CosmicSurface) {
    crate::shell::element::window::CosmicWindow::refresh_halo(&state.common.shell.read(), surface);
}

fn notify(state: &State, summary: String, body: String) {
    state.common.dbus_state.notify(Notification {
        app_name: fl!("recording-app-name"),
        app_icon: NOTIFICATION_ICON.to_owned(),
        summary,
        body,
        expire_timeout: NOTIFICATION_TIMEOUT_MS,
        transient: true,
    });
}
