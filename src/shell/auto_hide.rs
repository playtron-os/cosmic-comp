// SPDX-License-Identifier: GPL-3.0-only

//! Compositor-driven auto-hide for layer surfaces.
//!
//! When a layer surface registers for auto-hide via the render-offset protocol,
//! the compositor manages its visibility animation internally. This provides
//! smooth 60fps animation synced with the compositor's render loop, eliminating
//! the protocol round-trip latency of client-driven animation.
//!
//! The state machine:
//! - **Visible**: Surface is fully shown (offset = 0).
//! - **HidePending**: Waiting before starting hide animation.
//! - **SlidingOut**: Animate surface off the specified edge.
//! - **Hidden**: Surface is fully off-screen.
//! - **HoverPending**: Cursor hit the edge zone; waiting before showing.
//! - **SlidingIn**: Animate surface back on-screen.

use keyframe::{ease, functions::EaseInOutCubic};
use smithay::reexports::wayland_server::Resource;
use smithay::reexports::wayland_server::Weak;
use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;
use std::time::{Duration, Instant};

// ---------------------------------------------------------------------------
// Constants – matching the tuned values from the dock's previous client-side
// animation, now driven at compositor frame rate.
// ---------------------------------------------------------------------------

/// Duration of the hide animation (slide off screen).
pub const HIDE_DURATION: Duration = Duration::from_millis(200);
/// Duration of the show animation (slide on screen).
pub const SHOW_DURATION: Duration = Duration::from_millis(300);
/// Delay before starting the hide animation after cursor leaves.
pub const HIDE_DELAY: Duration = Duration::from_millis(500);
/// Delay before starting the show animation after cursor enters edge zone.
pub const SHOW_DELAY: Duration = Duration::from_millis(400);
/// Extra pixels beyond the surface height for the hide offset (shadow/blur).
pub const SHADOW_BUFFER: i32 = 60;

// ---------------------------------------------------------------------------
// Edge enum – matches the protocol's auto_hide_edge enum
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AutoHideEdge {
    Bottom,
}

impl AutoHideEdge {
    pub fn from_protocol(value: u32) -> Option<Self> {
        match value {
            0 => Some(AutoHideEdge::Bottom),
            _ => None,
        }
    }
}

// ---------------------------------------------------------------------------
// Visibility state machine
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub enum AutoHideVisibility {
    /// Fully visible (factor = 0.0).
    Visible,
    /// Waiting before starting hide animation.
    HidePending { start: Instant },
    /// Actively hiding (sliding off-screen).
    SlidingOut { start: Instant, from_factor: f32 },
    /// Fully hidden (factor = 1.0).
    Hidden,
    /// Cursor entered edge zone; waiting before showing.
    HoverPending { start: Instant },
    /// Actively showing (sliding on-screen).
    SlidingIn { start: Instant, from_factor: f32 },
}

impl AutoHideVisibility {
    /// Returns the current visibility factor: 0.0 = fully visible, 1.0 = fully hidden.
    pub fn factor(&self) -> f32 {
        match self {
            Self::Visible | Self::HidePending { .. } => 0.0,
            Self::SlidingOut {
                start, from_factor, ..
            } => {
                let t = progress_clamped(*start, HIDE_DURATION);
                let eased = ease(EaseInOutCubic, 0.0_f32, 1.0_f32, t);
                from_factor + (1.0 - from_factor) * eased
            }
            Self::Hidden | Self::HoverPending { .. } => 1.0,
            Self::SlidingIn {
                start, from_factor, ..
            } => {
                let t = progress_clamped(*start, SHOW_DURATION);
                let eased = ease_out_back(t);
                from_factor * (1.0 - eased)
            }
        }
    }

    /// True while any animation or pending delay is active.
    pub fn is_animating(&self) -> bool {
        !matches!(self, Self::Visible | Self::Hidden)
    }

    /// Advance the state machine. Returns `true` if a state transition occurred
    /// (callers should send `visibility_changed` events for terminal transitions).
    pub fn update(&mut self) -> Option<bool> {
        match self {
            Self::HidePending { start } => {
                if start.elapsed() >= HIDE_DELAY {
                    *self = Self::SlidingOut {
                        start: Instant::now(),
                        from_factor: 0.0,
                    };
                }
                None
            }
            Self::SlidingOut { start, .. } => {
                if start.elapsed() >= HIDE_DURATION {
                    *self = Self::Hidden;
                    Some(false) // now hidden → visibility_changed(0)
                } else {
                    None
                }
            }
            Self::HoverPending { start } => {
                if start.elapsed() >= SHOW_DELAY {
                    *self = Self::SlidingIn {
                        start: Instant::now(),
                        from_factor: 1.0,
                    };
                }
                None
            }
            Self::SlidingIn { start, .. } => {
                if start.elapsed() >= SHOW_DURATION {
                    *self = Self::Visible;
                    Some(true) // now visible → visibility_changed(1)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    // -- State transition helpers --

    /// Begin hiding (with optional delay). Call when maximize detected or
    /// cursor leaves the surface while maximized windows exist.
    pub fn start_hide(&mut self, with_delay: bool) {
        match self {
            Self::Visible => {
                if with_delay {
                    *self = Self::HidePending {
                        start: Instant::now(),
                    };
                } else {
                    *self = Self::SlidingOut {
                        start: Instant::now(),
                        from_factor: 0.0,
                    };
                }
            }
            Self::SlidingIn { .. } => {
                // Reverse from current position.
                let current = self.factor();
                *self = Self::SlidingOut {
                    start: Instant::now(),
                    from_factor: current,
                };
            }
            Self::HoverPending { .. } => {
                // Cancel pending show; stay hidden.
                *self = Self::Hidden;
            }
            _ => {} // Already hiding or hidden
        }
    }

    /// Begin showing. Call when cursor enters edge zone while hidden, or
    /// when all maximized/fullscreen windows are gone.
    pub fn start_show(&mut self, with_delay: bool) {
        match self {
            Self::Hidden => {
                if with_delay {
                    *self = Self::HoverPending {
                        start: Instant::now(),
                    };
                } else {
                    *self = Self::SlidingIn {
                        start: Instant::now(),
                        from_factor: 1.0,
                    };
                }
            }
            Self::SlidingOut { .. } => {
                // Reverse from current position.
                let current = self.factor();
                *self = Self::SlidingIn {
                    start: Instant::now(),
                    from_factor: current,
                };
            }
            Self::HidePending { .. } => {
                // Cancel pending hide; stay visible.
                *self = Self::Visible;
            }
            _ => {} // Already showing or visible
        }
    }

    /// Force immediate show (no delay). Used when un-maximize occurs.
    pub fn force_show(&mut self) {
        match self {
            Self::Hidden | Self::HoverPending { .. } => {
                *self = Self::SlidingIn {
                    start: Instant::now(),
                    from_factor: 1.0,
                };
            }
            Self::SlidingOut { .. } => {
                let current = self.factor();
                *self = Self::SlidingIn {
                    start: Instant::now(),
                    from_factor: current,
                };
            }
            Self::HidePending { .. } => {
                *self = Self::Visible;
            }
            _ => {} // Already visible or showing
        }
    }
}

// ---------------------------------------------------------------------------
// Per-surface auto-hide tracking
// ---------------------------------------------------------------------------

#[derive(Debug)]
pub struct AutoHideSurface {
    /// Weak reference to the underlying wl_surface.
    pub surface: Weak<WlSurface>,
    /// Protocol ID of the wl_surface (for fast comparison).
    pub surface_id: u32,
    /// Which edge the surface hides toward.
    pub edge: AutoHideEdge,
    /// Current animation state.
    pub visibility: AutoHideVisibility,
    /// Whether the cursor is currently over this surface or its edge zone.
    pub cursor_over: bool,
}

impl AutoHideSurface {
    pub fn new(surface: &WlSurface, edge: AutoHideEdge) -> Self {
        Self {
            surface: surface.downgrade(),
            surface_id: surface.id().protocol_id(),
            edge,
            visibility: AutoHideVisibility::Visible,
            cursor_over: false,
        }
    }

    /// Compute the render offset for the current animation factor.
    /// `surface_height` is the layer surface's height from the layer map.
    /// Factor may go slightly negative during the show animation overshoot
    /// (bounce effect), producing a small offset in the opposite direction.
    pub fn render_offset(&self, surface_height: i32) -> (i32, i32) {
        let factor = self.visibility.factor();
        if factor == 0.0 {
            return (0, 0);
        }
        let max_offset = surface_height + SHADOW_BUFFER;
        match self.edge {
            AutoHideEdge::Bottom => (0, (max_offset as f32 * factor).round() as i32),
        }
    }
}

// ---------------------------------------------------------------------------
// Easing functions
// ---------------------------------------------------------------------------

/// Ease-out-back: overshoot past the target then settle back for a
/// springy bounce feel.  c1 = 1.2 gives a subtle ~7 % overshoot.
fn ease_out_back(t: f32) -> f32 {
    let c1: f32 = 1.2;
    let c3 = c1 + 1.0;
    1.0 + c3 * (t - 1.0).powi(3) + c1 * (t - 1.0).powi(2)
}

/// Clamped progress ratio for an animation started at `start` with `duration`.
fn progress_clamped(start: Instant, duration: Duration) -> f32 {
    (start.elapsed().as_secs_f32() / duration.as_secs_f32()).min(1.0)
}
