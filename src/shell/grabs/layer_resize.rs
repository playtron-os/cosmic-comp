// SPDX-License-Identifier: GPL-3.0-only

//! Interactive edge drag for an opt-in side panel (via the `layer_edge_resize`
//! protocol; e.g. `agentos-chat-panel`).
//!
//! Unlike a live resize, this grab does NOT resize the panel while dragging: the
//! panel stays put and only a "ghost" indicator follows the pointer
//! ([`Shell::edge_drag_ghost`], drawn by the render path). On release the panel
//! springs to the ghost width via [`Shell::set_layer_resize_width`] (the shared
//! `--ease-spring` resize animation). The compositor is authoritative — it has the
//! global pointer position every frame and computes the width directly, so there is
//! no client-side measurement and no feedback loop.

use crate::utils::prelude::*;
use crate::{
    backend::render::cursor::CursorState,
    shell::{LayerResize, focus::target::PointerFocusTarget},
};
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
use wayland_backend::server::ObjectId;

/// Fraction of the output width at/above which releasing a drag snaps the panel to
/// full width. Looser than the maximize-detect threshold used by the double-click
/// toggle (`MAXIMIZE_FRACTION` in `shell`), because a drag is a coarse gesture:
/// releasing "near enough" to the edge should commit to fully maximized.
const SNAP_TO_FULL_FRACTION: f32 = 0.96;

pub struct EdgeResizeGrab {
    start_data: PointerGrabStartData<State>,
    seat: Seat<State>,
    output: Output,
    surface_id: ObjectId,
    anchor_right: bool,
    /// The panel's width when the drag started — the spring's `from` on release.
    start_width: i32,
    min: i32,
    max: i32,
}

impl EdgeResizeGrab {
    /// Start an edge-drag from a [`LayerResize`] target (produced by
    /// [`Shell::layer_resize_target`]). The caller must have already seeded
    /// [`Shell::edge_drag_ghost`] for this surface.
    pub fn new(
        start_data: PointerGrabStartData<State>,
        seat: &Seat<State>,
        target: LayerResize,
    ) -> Self {
        let cursor_state = seat.user_data().get::<CursorState>().unwrap();
        cursor_state.lock().unwrap().set_shape(CursorIcon::EwResize);
        EdgeResizeGrab {
            start_data,
            seat: seat.clone(),
            output: target.output,
            surface_id: target.surface_id,
            anchor_right: target.anchor_right,
            start_width: target.width,
            min: target.min,
            max: target.max,
        }
    }

    /// Recompute the ghost width from the global pointer position and request a
    /// redraw so the indicator follows the cursor. The panel is NOT resized.
    /// Returns `false` if the ghost was cleared out from under us (end the grab).
    fn apply(&self, data: &mut State, location: Point<f64, Global>) -> bool {
        let geo = self.output.geometry();
        // The anchored (outer) edge is pinned to the output edge; the dragged edge
        // follows the pointer, so the width is the pointer's distance from it.
        let raw = if self.anchor_right {
            (geo.loc.x + geo.size.w) as f64 - location.x
        } else {
            location.x - geo.loc.x as f64
        };
        let width = (raw.round() as i32).clamp(self.min, self.max);
        let mut shell = data.common.shell.write();
        let Some(ghost) = shell.edge_drag_ghost.as_mut() else {
            return false;
        };
        ghost.width = width;
        drop(shell);
        data.backend.schedule_render(&self.output);
        true
    }
}

impl PointerGrab<State> for EdgeResizeGrab {
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
            // Finalize (snap + spring) runs in `unset` — the single end path shared
            // with the suppressed-release case (`ptr.unset_grab` in the input
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
        let Some(ghost) = shell.edge_drag_ghost.take() else {
            return;
        };
        let mut target = ghost.width;
        // Snap to full screen if released within 4% of the output edge.
        if target >= (self.max as f32 * SNAP_TO_FULL_FRACTION) as i32 {
            target = self.max;
        }
        // Spring the panel from its pre-drag width to the released target. This
        // seeds the `LayerResizeAnim`, which the render/animation loop drives.
        shell.set_layer_resize_width(
            &self.surface_id,
            &output,
            self.anchor_right,
            self.start_width,
            target,
        );
        drop(shell);
        data.backend.schedule_render(&output);
        // Re-establish the hover sash + cursor immediately. A no-op release (target
        // == start width) starts no animation, so the dispatch-loop re-eval won't
        // run; this also seeds the correct hover for the animated case. (The grab
        // cleared edge_hover at start, and its Drop unset the cursor.)
        data.update_edge_resize_hover(&self.seat);
    }
}

impl Drop for EdgeResizeGrab {
    fn drop(&mut self) {
        let cursor_state = self.seat.user_data().get::<CursorState>().unwrap();
        cursor_state.lock().unwrap().unset_shape();
    }
}
