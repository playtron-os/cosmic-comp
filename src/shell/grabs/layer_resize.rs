// SPDX-License-Identifier: GPL-3.0-only

//! Interactive resize of an edge-anchored layer-shell side panel (the
//! `agentos-chat-panel`).
//!
//! Unlike a toplevel resize, the compositor is fully authoritative here: it has the
//! global pointer position every frame, so it computes the panel's new width directly
//! and drives the layer surface's size + exclusive zone via the cached-state override
//! that runs before every `arrange()` (see [`Shell::override_active_layer_resize`]).
//! The client just renders the `configure`s it receives — there is no client-side
//! measurement and therefore no feedback loop.

use crate::utils::prelude::*;
use crate::{backend::render::cursor::CursorState, shell::focus::target::PointerFocusTarget};
use smithay::desktop::layer_map_for_output;
use smithay::{
    input::{
        Seat,
        pointer::{
            AxisFrame, ButtonEvent, CursorIcon, GestureHoldBeginEvent, GestureHoldEndEvent,
            GesturePinchBeginEvent, GesturePinchEndEvent, GesturePinchUpdateEvent,
            GestureSwipeBeginEvent, GestureSwipeEndEvent, GestureSwipeUpdateEvent,
            GrabStartData as PointerGrabStartData, MotionEvent, PointerGrab, PointerInnerHandle,
            RelativeMotionEvent,
        },
    },
    output::Output,
    utils::{Logical, Point},
};

/// Fraction of the output width at/above which releasing a drag snaps the panel to full
/// width. Looser than the maximize-detect threshold used by the double-click toggle
/// (`MAXIMIZE_FRACTION` in `shell`), because a drag is a coarse gesture: releasing
/// "near enough" to the edge should commit to fully maximized.
const SNAP_TO_FULL_FRACTION: f32 = 0.96;

pub struct LayerResizeGrab {
    start_data: PointerGrabStartData<State>,
    seat: Seat<State>,
    output: Output,
}

impl LayerResizeGrab {
    pub fn new(
        start_data: PointerGrabStartData<State>,
        seat: &Seat<State>,
        output: Output,
    ) -> Self {
        let cursor_state = seat.user_data().get::<CursorState>().unwrap();
        cursor_state.lock().unwrap().set_shape(CursorIcon::EwResize);
        LayerResizeGrab {
            start_data,
            seat: seat.clone(),
            output,
        }
    }

    /// Recompute the panel width from the global pointer position and re-arrange the
    /// output so the client gets a fresh `configure`. Returns `false` if the active
    /// resize was cleared out from under us (the grab should then end).
    fn apply(&self, data: &mut State, location: Point<f64, Global>) -> bool {
        let output = self.output.clone();
        let mut shell = data.common.shell.write();
        let active = if let Some(resize) = shell.active_layer_resize.as_mut() {
            let geo = output.geometry();
            // The panel's anchored (outer) edge is pinned to the output edge; the
            // dragged (inner) edge follows the pointer, so the width is just the
            // pointer's distance from the fixed outer edge.
            let raw = if resize.anchor_right {
                (geo.loc.x + geo.size.w) as f64 - location.x
            } else {
                location.x - geo.loc.x as f64
            };
            resize.width = (raw.round() as i32).clamp(resize.min, resize.max);
            tracing::debug!(
                "RESIZE_DBG apply anchor_right={} out_loc_x={} out_w={} pointer_x={:.1} raw={:.1} -> width={} (min={} max={})",
                resize.anchor_right,
                geo.loc.x,
                geo.size.w,
                location.x,
                raw,
                resize.width,
                resize.min,
                resize.max,
            );
            true
        } else {
            false
        };
        if !active {
            return false;
        }
        shell.override_active_layer_resize(&output);
        if layer_map_for_output(&output).arrange() {
            shell.workspaces.recalculate();
        }
        drop(shell);
        data.backend.schedule_render(&output);
        true
    }
}

impl PointerGrab<State> for LayerResizeGrab {
    fn motion(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        _focus: Option<(PointerFocusTarget, Point<f64, Logical>)>,
        event: &MotionEvent,
    ) {
        // While the grab is active, no client has pointer focus.
        handle.motion(data, None, event);
        if !self.apply(data, event.location.as_global()) {
            handle.unset_grab(self, data, event.serial, event.time, true);
        }
    }

    fn relative_motion(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        _focus: Option<(PointerFocusTarget, Point<f64, Logical>)>,
        event: &RelativeMotionEvent,
    ) {
        handle.relative_motion(data, None, event);
    }

    fn button(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &ButtonEvent,
    ) {
        handle.button(data, event);
        if handle.current_pressed().is_empty() {
            // Finalize (snap-to-full) runs in `unset` — which is the single end path
            // shared with the suppressed-release case (`ptr.unset_grab` in the input
            // handler), so it can't be skipped.
            handle.unset_grab(self, data, event.serial, event.time, true);
        }
    }

    fn axis(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        details: AxisFrame,
    ) {
        handle.axis(data, details)
    }

    fn frame(&mut self, data: &mut State, handle: &mut PointerInnerHandle<'_, State>) {
        handle.frame(data)
    }

    fn gesture_swipe_begin(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GestureSwipeBeginEvent,
    ) {
        handle.gesture_swipe_begin(data, event)
    }

    fn gesture_swipe_update(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GestureSwipeUpdateEvent,
    ) {
        handle.gesture_swipe_update(data, event)
    }

    fn gesture_swipe_end(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GestureSwipeEndEvent,
    ) {
        handle.gesture_swipe_end(data, event)
    }

    fn gesture_pinch_begin(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GesturePinchBeginEvent,
    ) {
        handle.gesture_pinch_begin(data, event)
    }

    fn gesture_pinch_update(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GesturePinchUpdateEvent,
    ) {
        handle.gesture_pinch_update(data, event)
    }

    fn gesture_pinch_end(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GesturePinchEndEvent,
    ) {
        handle.gesture_pinch_end(data, event)
    }

    fn gesture_hold_begin(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GestureHoldBeginEvent,
    ) {
        handle.gesture_hold_begin(data, event)
    }

    fn gesture_hold_end(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GestureHoldEndEvent,
    ) {
        handle.gesture_hold_end(data, event)
    }

    fn start_data(&self) -> &PointerGrabStartData<State> {
        &self.start_data
    }

    fn unset(&mut self, data: &mut State) {
        let output = self.output.clone();
        let mut shell = data.common.shell.write();
        // Snap to full screen if released within 4% of the output edge, apply the
        // final width once, then clear the active resize so normal layout resumes.
        if let Some(resize) = shell.active_layer_resize.as_mut()
            && resize.width >= (resize.max as f32 * SNAP_TO_FULL_FRACTION) as i32
        {
            resize.width = resize.max;
        }
        if let Some(resize) = shell.active_layer_resize.as_ref() {
            tracing::debug!(
                "RESIZE_DBG grab END (unset) final_width={} max={} anchor_right={} snap_threshold={}",
                resize.width,
                resize.max,
                resize.anchor_right,
                (resize.max as f32 * SNAP_TO_FULL_FRACTION) as i32,
            );
        }
        // Hold the final width so the size override + edge-pin offset keep applying until
        // the client's buffer catches up (no blink to the trailing width on a fast
        // release); cleared in `Shell::clear_layer_resize_settle_if_caught_up`.
        shell.layer_resize_settle = shell.active_layer_resize.clone();
        shell.override_active_layer_resize(&output);
        if layer_map_for_output(&output).arrange() {
            shell.workspaces.recalculate();
        }
        shell.active_layer_resize = None;
        drop(shell);
        data.backend.schedule_render(&output);
    }
}

impl Drop for LayerResizeGrab {
    fn drop(&mut self) {
        let cursor_state = self.seat.user_data().get::<CursorState>().unwrap();
        cursor_state.lock().unwrap().unset_shape();
    }
}
