//! The halo's Record: a window's recording through kora-capture-engine.
//!
//! The compositor never owns the recorder process. It asks
//! `one.playtron.CaptureEngine1` to start and stop, keeps the recording id on the
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
    wayland::protocols::toplevel_info::{foreign_toplevel_identifier, mapped_toplevel_identifier},
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
        // The app that started it hears RecordingStopped and files the result.
        Recording::Watched { id } => {
            let surface = surface.clone();
            state
                .common
                .dbus_state
                .stop_recording(id, move |state, reply| {
                    if let Err(error) = reply {
                        warn!(%error, "Failed to stop another app's recording");
                        notify(state, fl!("recording-failed"), error);
                    }
                    surface.set_recording(Recording::Idle);
                    refresh_halo(state, &surface);
                });
        }
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

/// Who asked, for how a failure or a start is told.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Caller {
    Halo,
    Game,
}

fn start(state: &mut State, surface: &CosmicSurface) {
    begin(state, surface, Caller::Halo, |state, result| {
        if let Err(error) = result {
            notify(state, fl!("recording-failed"), error);
        }
    });
}

/// Record the game on screen for game mode's launcher, through the halo's path.
/// `done` gets a [`failure_code`] or nothing.
pub fn start_game(
    state: &mut State,
    surface: &CosmicSurface,
    done: impl FnOnce(&mut State, Result<(), String>) + 'static,
) {
    if surface.recording() != Recording::Idle {
        done(state, Ok(()));
        return;
    }
    begin(state, surface, Caller::Game, move |state, result| {
        done(
            state,
            result.map_err(|error| failure_code(&error).to_owned()),
        );
    });
}

/// Finish every recording the engine lists; `done` gets a [`failure_code`] or
/// nothing. Each file is filed by `RecordingStopped`, as any other ending is.
pub fn stop_all(state: &mut State, done: impl FnOnce(Result<(), String>) + 'static) {
    let ids = state.common.game_mode_bridge.recording_ids();
    if ids.is_empty() {
        done(Ok(()));
        return;
    }
    let left = std::rc::Rc::new(std::cell::RefCell::new((ids.len(), Ok(()), Some(done))));
    for id in ids {
        let left = left.clone();
        state.common.dbus_state.stop_recording(id, move |_, reply| {
            let mut left = left.borrow_mut();
            left.0 -= 1;
            if let Err(error) = reply {
                warn!(%error, "Failed to stop a recording");
                left.1 = Err(failure_code(&error).to_owned());
            }
            if left.0 == 0 {
                let result = std::mem::replace(&mut left.1, Ok(()));
                if let Some(done) = left.2.take() {
                    done(result);
                }
            }
        });
    }
}

/// A recording's error as a code a launcher can act on: `no-space`,
/// `no-permission`, `engine-down` or `failed`.
#[must_use]
pub fn failure_code(error: &str) -> &'static str {
    let has = |needle: &str| error.contains(needle);
    if has("No space left") || has("os error 28") || has("ENOSPC") {
        "no-space"
    } else if has("Permission denied") || has("os error 13") || has("AccessDenied") {
        "no-permission"
    } else if has("ServiceUnknown")
        || has("NameHasNoOwner")
        || has("was not provided")
        || has("NoReply")
        || has("the recorder stopped")
    {
        "engine-down"
    } else {
        "failed"
    }
}

/// Room a game's recording needs on its disk before it starts.
const GAME_RECORDING_MIN_FREE: u64 = 512 * 1024 * 1024;

fn short_of_space(dir: &Path) -> bool {
    rustix::fs::statvfs(dir)
        .is_ok_and(|fs| fs.f_bavail.saturating_mul(fs.f_frsize) < GAME_RECORDING_MIN_FREE)
}

fn begin(
    state: &mut State,
    surface: &CosmicSurface,
    caller: Caller,
    report: impl FnOnce(&mut State, Result<(), String>) + 'static,
) {
    let Some(identifier) = foreign_toplevel_identifier(surface) else {
        report(state, Err(fl!("recording-window-not-capturable")));
        return;
    };
    let Some(destination) =
        captures::Destination::for_surface(state, surface, CaptureKind::Recording)
    else {
        report(state, Err("HOME is not set".into()));
        return;
    };
    // It waits here while its preview is up, as a screenshot does.
    if let Err(err) = std::fs::create_dir_all(&destination.pending) {
        report(state, Err(err.to_string()));
        return;
    }
    if caller == Caller::Game && short_of_space(&destination.pending) {
        report(state, Err("No space left for a recording".into()));
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
                    // The game's own indicator says so over a game; a toast cannot.
                    if caller == Caller::Halo {
                        let notification = notification(fl!("recording-started"), String::new());
                        state.common.dbus_state.system_toast_or(
                            fl!("recording-started"),
                            Tone::Destructive,
                            notification,
                        );
                    }
                }
                refresh_halo(state, &surface);
                report(state, Ok(()));
            }
            Err(error) => {
                warn!(%error, "Failed to start a recording");
                surface.set_recording(Recording::Idle);
                refresh_halo(state, &surface);
                report(state, Err(error));
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
    let since = match surface.recording() {
        Recording::Active { since, .. } => since,
        // Its own app announces how it ended; the halo only lets go.
        Recording::Watched { .. } => {
            surface.set_recording(Recording::Idle);
            refresh_halo(state, &surface);
            return;
        }
        Recording::Idle | Recording::Starting => return,
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

/// `WindowRecordingStarted` from the recorder: another app records the window
/// `identifier`, so its halo shows Record on, as if it had been pressed.
pub fn started_elsewhere(state: &mut State, id: &str, identifier: &str) {
    let Some(surface) = surface_with_identifier(state, identifier) else {
        return;
    };
    // The halo's own request hears the same signal; its answer decides.
    if surface.recording() != Recording::Idle {
        return;
    }
    surface.set_recording(Recording::Watched { id: id.to_owned() });
    refresh_halo(state, &surface);
}

/// Whether `surface` is recording as `id` right now.
fn carries(surface: &CosmicSurface, id: &str) -> bool {
    match surface.recording() {
        Recording::Active { id: active, .. } | Recording::Watched { id: active } => active == id,
        Recording::Idle | Recording::Starting => false,
    }
}

/// The window the recorder knows as `identifier`, mapped or fullscreen.
pub fn surface_with_identifier(state: &State, identifier: &str) -> Option<CosmicSurface> {
    let named = |surface: &CosmicSurface| {
        foreign_toplevel_identifier(surface)
            .or_else(|| mapped_toplevel_identifier(surface))
            .is_some_and(|known| known == identifier)
    };
    let shell = state.common.shell.read();
    shell
        .mapped()
        .flat_map(|mapped| mapped.windows().map(|(window, _)| window))
        .find(named)
        .or_else(|| {
            shell
                .workspaces()
                .spaces()
                .flat_map(|workspace| workspace.get_fullscreen_surfaces())
                .map(|fullscreen| fullscreen.surface.clone())
                .find(named)
        })
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

#[cfg(test)]
mod tests {
    use super::failure_code;

    #[test]
    fn failures_become_codes_a_launcher_can_act_on() {
        for (error, code) in [
            ("No space left on device (os error 28)", "no-space"),
            ("No space left for a recording", "no-space"),
            ("Permission denied (os error 13)", "no-permission"),
            (
                "org.freedesktop.DBus.Error.AccessDenied: no",
                "no-permission",
            ),
            (
                "org.freedesktop.DBus.Error.ServiceUnknown: gone",
                "engine-down",
            ),
            ("the recorder stopped", "engine-down"),
            ("the compositor stopped the session", "failed"),
        ] {
            assert_eq!(failure_code(error), code, "{error}");
        }
    }
}
