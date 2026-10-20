// SPDX-License-Identifier: GPL-3.0-only

//! A drag held still over a window behind others raises it, so a file can reach
//! a covered window; the drop focuses the window that took it. Passing over
//! windows changes nothing, and focus never moves mid-drag. A capture's drag
//! also tells the preview which app under the pointer takes it.

use std::any::Any;
use std::cell::RefCell;
use std::os::fd::OwnedFd;
use std::sync::{
    Arc,
    atomic::{AtomicBool, Ordering},
};
use std::time::Duration;

use calloop::{
    RegistrationToken,
    ping::{Ping, make_ping},
    timer::{TimeoutAction, Timer},
};
use futures_channel::mpsc::UnboundedSender;
use smithay::{
    input::{
        Seat,
        dnd::{DndAction, Source, SourceMetadata},
    },
    output::Output,
    utils::{IsAlive, Point, SERIAL_COUNTER},
};

use crate::{
    shell::{Shell, focus::target::KeyboardFocusTarget},
    state::State,
    utils::prelude::*,
};

/// How long the pointer rests before the window under it comes forward.
pub const HOLD: Duration = Duration::from_millis(650);
/// Movement within this many pixels still counts as resting.
const SLOP: f64 = 6.0;

/// The type the capture preview offers; only its drags name the app under them.
pub const CAPTURE_MIME: &str = "application/x-kora-capture";

#[derive(Default)]
struct Hover {
    dragging: bool,
    pending: Option<Pending>,
    naming: Option<Naming>,
}

/// A capture drag's ghost label and the channel that carries it to the preview.
struct Naming {
    accepted: Arc<AtomicBool>,
    token: RegistrationToken,
    app_id: Option<String>,
    label: DropLabel,
    tell: UnboundedSender<String>,
}

/// What the ghost reads: the app under the pointer, once that app accepts the drag.
#[derive(Debug, Default)]
struct DropLabel {
    app: Option<String>,
    accepted: bool,
    shown: Option<String>,
}

impl DropLabel {
    /// The name to show when it changed, `""` for none.
    fn update(&mut self) -> Option<String> {
        let now = self.app.clone().filter(|_| self.accepted);
        if now == self.shown {
            return None;
        }
        self.shown.clone_from(&now);
        Some(now.unwrap_or_default())
    }
}

/// The drag's source, which also hears whether the target under the pointer accepts it.
pub struct Watched<S> {
    inner: S,
    accepted: Arc<AtomicBool>,
    ping: Option<Ping>,
}

impl<S: IsAlive> IsAlive for Watched<S> {
    fn alive(&self) -> bool {
        self.inner.alive()
    }
}

impl<S: Source> Source for Watched<S> {
    fn is_client_local(&self, target: &dyn Any) -> bool {
        self.inner.is_client_local(target)
    }

    fn metadata(&self) -> Option<SourceMetadata> {
        self.inner.metadata()
    }

    fn target(&self, mime_type: Option<&str>) {
        self.inner.target(mime_type);
        self.accepted.store(mime_type.is_some(), Ordering::Relaxed);
        if let Some(ping) = &self.ping {
            ping.ping();
        }
    }

    fn choose_action(&self, action: DndAction) {
        self.inner.choose_action(action);
    }

    fn send(&self, mime_type: &str, fd: OwnedFd) {
        self.inner.send(mime_type, fd);
    }

    fn drop_performed(&self) {
        self.inner.drop_performed();
    }

    fn cancel(&self) {
        self.inner.cancel();
    }

    fn finished(&self) {
        self.inner.finished();
    }
}

struct Pending {
    target: KeyboardFocusTarget,
    at: Point<f64, Global>,
    token: RegistrationToken,
}

fn with<R>(seat: &Seat<State>, f: impl FnOnce(&mut Hover) -> R) -> R {
    seat.user_data()
        .insert_if_missing(RefCell::<Hover>::default);
    let cell = seat.user_data().get::<RefCell<Hover>>().unwrap();
    f(&mut cell.borrow_mut())
}

/// A drag began on `seat`. A capture's drag is watched so the preview can name its target.
pub fn started<S: Source>(state: &mut State, seat: &Seat<State>, source: S) -> Watched<S> {
    let accepted = Arc::new(AtomicBool::new(false));
    let capture = source
        .metadata()
        .is_some_and(|meta| meta.mime_types.iter().any(|mime| mime == CAPTURE_MIME));
    let mut ping = None;
    let naming = capture
        .then(|| watch(state, seat, &accepted, &mut ping))
        .flatten();
    let old = with(seat, |hover| {
        hover.dragging = true;
        std::mem::replace(&mut hover.naming, naming)
    });
    if let Some(old) = old {
        state.common.event_loop_handle.remove(old.token);
    }
    Watched {
        inner: source,
        accepted,
        ping,
    }
}

fn watch(
    state: &mut State,
    seat: &Seat<State>,
    accepted: &Arc<AtomicBool>,
    ping: &mut Option<Ping>,
) -> Option<Naming> {
    let (pinger, answers) = make_ping().ok()?;
    let held = seat.clone();
    let token = state
        .common
        .event_loop_handle
        .insert_source(answers, move |_, _, _| refresh(&held))
        .ok()?;
    *ping = Some(pinger);
    Some(Naming {
        accepted: accepted.clone(),
        token,
        app_id: None,
        label: DropLabel::default(),
        tell: state.common.dbus_state.capture_drag_targets(),
    })
}

/// Show the app under the pointer on the ghost once it accepts, and only on change.
fn refresh(seat: &Seat<State>) {
    with(seat, |hover| {
        let Some(naming) = hover.naming.as_mut() else {
            return;
        };
        naming.label.accepted = naming.accepted.load(Ordering::Relaxed);
        if let Some(name) = naming.label.update() {
            let _ = naming.tell.unbounded_send(name);
        }
    });
}

/// The pointer is over `app_id`'s window, or no app's. Read once the grab has moved focus.
fn over_app(state: &mut State, seat: &Seat<State>, app_id: Option<String>) {
    let changed = with(seat, |hover| {
        hover
            .naming
            .as_mut()
            .is_some_and(|naming| naming.app_id != app_id)
    });
    if !changed {
        return;
    }
    let name = app_id
        .as_deref()
        .and_then(crate::utils::screenshot::app_name);
    with(seat, |hover| {
        if let Some(naming) = hover.naming.as_mut() {
            naming.app_id = app_id;
            naming.label.app = name;
        }
    });
    let held = seat.clone();
    state
        .common
        .event_loop_handle
        .insert_idle(move |_| refresh(&held));
}

/// Whether a drag is in flight on `seat`.
pub fn dragging(seat: &Seat<State>) -> bool {
    with(seat, |hover| hover.dragging)
}

/// The drag ended, dropped or not; a pending raise goes with it.
pub fn ended(state: &mut State, seat: &Seat<State>) {
    let (pending, naming) = with(seat, |hover| {
        hover.dragging = false;
        (hover.pending.take(), hover.naming.take())
    });
    if let Some(pending) = pending {
        state.common.event_loop_handle.remove(pending.token);
    }
    if let Some(naming) = naming {
        state.common.event_loop_handle.remove(naming.token);
    }
}

/// The pointer moved under a grab. While dragging, a rest over a window
/// that is not in front starts the clock; moving on restarts it.
pub fn moved(state: &mut State, seat: &Seat<State>, position: Point<f64, Global>, output: &Output) {
    if !with(seat, |hover| hover.dragging) {
        return;
    }
    let under = {
        let shell = state.common.shell.read();
        State::element_under(position, output, &shell, seat)
    };
    let app_id = match &under {
        Some(KeyboardFocusTarget::Element(mapped)) => Some(mapped.active_window().app_id()),
        _ => None,
    };
    over_app(state, seat, app_id.filter(|id| !id.is_empty()));
    let focused = seat.get_keyboard().and_then(|k| k.current_focus());
    let target = under
        .filter(|t| matches!(t, KeyboardFocusTarget::Element(_)) && Some(t) != focused.as_ref());
    let resting = with(seat, |hover| {
        hover.pending.as_ref().is_some_and(|p| {
            Some(&p.target) == target.as_ref()
                && (p.at.x - position.x).hypot(p.at.y - position.y) <= SLOP
        })
    });
    if resting {
        return;
    }
    if let Some(old) = with(seat, |hover| hover.pending.take()) {
        state.common.event_loop_handle.remove(old.token);
    }
    let Some(target) = target else {
        return;
    };
    let held = seat.clone();
    let raised = target.clone();
    let token = state.common.event_loop_handle.insert_source(
        Timer::from_duration(HOLD),
        move |_, _, state| {
            let still = with(&held, |hover| {
                let pending = hover.pending.take();
                hover.dragging && pending.is_some_and(|p| p.target == raised)
            });
            if let (true, KeyboardFocusTarget::Element(mapped)) = (still, &raised) {
                // Focus waits for the drop: a focus change mid-drag ends the drag.
                state.common.shell.write().raise_without_focus(mapped);
                for output in state
                    .common
                    .shell
                    .read()
                    .outputs()
                    .cloned()
                    .collect::<Vec<_>>()
                {
                    state.backend.schedule_render(&output);
                }
            }
            TimeoutAction::Drop
        },
    );
    let Ok(token) = token else {
        return;
    };
    with(seat, |hover| {
        hover.pending = Some(Pending {
            target,
            at: position,
            token,
        })
    });
}

/// A drop landed at `position`: the window under it takes focus.
pub fn dropped(state: &mut State, seat: &Seat<State>, position: Point<f64, Global>) {
    ended(state, seat);
    let output = seat.active_output();
    let under = {
        let shell = state.common.shell.read();
        State::element_under(position, &output, &shell, seat)
    };
    if let Some(target @ KeyboardFocusTarget::Element(_)) = under {
        Shell::set_focus(
            state,
            Some(&target),
            seat,
            Some(SERIAL_COUNTER.next_serial()),
            false,
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn label(app: Option<&str>, accepted: bool, shown: Option<&str>) -> DropLabel {
        DropLabel {
            app: app.map(str::to_owned),
            accepted,
            shown: shown.map(str::to_owned),
        }
    }

    #[test]
    fn names_the_app_once_it_accepts() {
        let mut over = label(Some("Explorer"), false, None);
        assert_eq!(over.update(), None);
        over.accepted = true;
        assert_eq!(over.update().as_deref(), Some("Explorer"));
        assert_eq!(over.update(), None, "only a change is sent");
    }

    #[test]
    fn a_refusing_window_or_the_shell_names_nothing() {
        let mut over = label(Some("Explorer"), true, Some("Explorer"));
        over.accepted = false;
        assert_eq!(over.update().as_deref(), Some(""));
        let mut shell = label(None, true, Some("Slate"));
        assert_eq!(shell.update().as_deref(), Some(""));
    }

    #[test]
    fn moving_between_apps_renames() {
        let mut over = label(Some("Slate"), true, Some("Explorer"));
        assert_eq!(over.update().as_deref(), Some("Slate"));
    }

    struct Fake(Arc<AtomicBool>);

    impl IsAlive for Fake {
        fn alive(&self) -> bool {
            true
        }
    }

    impl Source for Fake {
        fn metadata(&self) -> Option<SourceMetadata> {
            None
        }
        fn target(&self, mime_type: Option<&str>) {
            self.0.store(mime_type.is_some(), Ordering::Relaxed);
        }
        fn choose_action(&self, _: DndAction) {}
        fn send(&self, _: &str, _: OwnedFd) {}
        fn drop_performed(&self) {}
        fn cancel(&self) {}
        fn finished(&self) {}
    }

    #[test]
    fn the_source_still_hears_what_the_target_accepts() {
        let heard = Arc::new(AtomicBool::new(false));
        let (ping, _answers) = make_ping().unwrap();
        let watched = Watched {
            inner: Fake(heard.clone()),
            accepted: Arc::new(AtomicBool::new(false)),
            ping: Some(ping),
        };
        watched.target(Some(CAPTURE_MIME));
        assert!(heard.load(Ordering::Relaxed));
        assert!(watched.accepted.load(Ordering::Relaxed));
        watched.target(None);
        assert!(!heard.load(Ordering::Relaxed));
        assert!(!watched.accepted.load(Ordering::Relaxed));
    }
}
