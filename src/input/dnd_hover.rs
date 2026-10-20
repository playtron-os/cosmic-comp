// SPDX-License-Identifier: GPL-3.0-only

//! A drag held still over a window behind others raises it, so a file can reach
//! a covered window; the drop focuses the window that took it. Passing over
//! windows changes nothing, and focus never moves mid-drag.

use std::cell::RefCell;
use std::time::Duration;

use calloop::{
    RegistrationToken,
    timer::{TimeoutAction, Timer},
};
use smithay::{
    input::Seat,
    output::Output,
    utils::{Point, SERIAL_COUNTER},
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

#[derive(Default)]
struct Hover {
    dragging: bool,
    pending: Option<Pending>,
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

/// A drag began on `seat`.
pub fn started(seat: &Seat<State>) {
    with(seat, |hover| hover.dragging = true);
}

/// Whether a drag is in flight on `seat`.
pub fn dragging(seat: &Seat<State>) -> bool {
    with(seat, |hover| hover.dragging)
}

/// The drag ended, dropped or not; a pending raise goes with it.
pub fn ended(state: &mut State, seat: &Seat<State>) {
    if let Some(pending) = with(seat, |hover| {
        hover.dragging = false;
        hover.pending.take()
    }) {
        state.common.event_loop_handle.remove(pending.token);
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
