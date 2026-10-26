// SPDX-License-Identifier: GPL-3.0-only

//! Surfaces their clients left out of captures: drawn on screen, skipped while
//! a capture is drawn.
//!
//! Elements are collected deep in the shell, so whether the draw is a capture
//! travels with the thread that collects them rather than through every call.

use std::cell::Cell;

use smithay::{
    reexports::wayland_server::protocol::wl_surface::WlSurface,
    wayland::compositor::{SurfaceData, with_states},
};

use crate::wayland::protocols::kora_capture_exclusion::is_excluded;

thread_local! {
    static CAPTURING: Cell<bool> = const { Cell::new(false) };
    static DREW_EXCLUDED: Cell<bool> = const { Cell::new(false) };
}

/// Elements collected on this thread while it lives are for a capture.
#[must_use]
pub struct CapturePass {
    outer: bool,
}

impl CapturePass {
    pub fn begin() -> Self {
        CapturePass {
            outer: CAPTURING.replace(true),
        }
    }
}

impl Drop for CapturePass {
    fn drop(&mut self) {
        CAPTURING.set(self.outer);
    }
}

/// Whether the surface holding `states` is left out of what is collected now.
/// A screen draw keeps it and notes that it did, for [`take_drew_excluded`].
pub fn leaves_out(states: &SurfaceData) -> bool {
    if !is_excluded(states) {
        return false;
    }
    if CAPTURING.get() {
        return true;
    }
    DREW_EXCLUDED.set(true);
    false
}

pub fn leaves_out_surface(surface: &WlSurface) -> bool {
    with_states(surface, leaves_out)
}

/// Whether a screen draw on this thread kept an excluded surface since the last
/// call: a capture of that screen then has to be drawn on its own.
pub fn take_drew_excluded() -> bool {
    DREW_EXCLUDED.replace(false)
}
