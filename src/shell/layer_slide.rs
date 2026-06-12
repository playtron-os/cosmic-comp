// SPDX-License-Identifier: GPL-3.0-only

//! Slide animation for layer surfaces toggled via the visibility protocol.
//!
//! When a layer surface anchored to a single edge (Left or Right) is shown or
//! hidden via `layer_surface_visibility`, the compositor slides it in/out from
//! that edge instead of just fading opacity. This provides a smooth, polished
//! animation for side panels like the chat panel.
//!
//! The state machine:
//! - **Visible**: Surface is fully shown (offset = 0).
//! - **SlidingOut**: Animate surface off the edge.
//! - **Hidden**: Surface is fully off-screen.
//! - **SlidingIn**: Animate surface back on-screen.

use std::time::{Duration, Instant};
use wayland_backend::server::ObjectId;

/// Duration of the slide-out (hide) animation.
pub const SLIDE_OUT_DURATION: Duration = Duration::from_millis(420);
/// Duration of the slide-in (show) animation.
pub const SLIDE_IN_DURATION: Duration = Duration::from_millis(420);

/// Which edge the surface slides toward.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SlideEdge {
    Left,
    Right,
}

/// Visibility state machine for the slide animation.
///
/// The `duration` of an active transition is scaled by the distance it has to
/// cover, so reversing a half-finished slide takes half the time instead of
/// crawling through the full duration for the remaining distance.
#[derive(Debug, Clone)]
pub enum SlideVisibility {
    /// Fully visible (factor = 0.0, no offset).
    Visible,
    /// Actively sliding off the edge.
    SlidingOut {
        start: Instant,
        from_factor: f32,
        duration: Duration,
    },
    /// Fully hidden (factor = 1.0, fully off-screen).
    Hidden,
    /// Actively sliding back on-screen.
    SlidingIn {
        start: Instant,
        from_factor: f32,
        duration: Duration,
    },
}

impl SlideVisibility {
    /// Returns the current slide factor: 0.0 = fully visible, 1.0 = fully hidden.
    pub fn factor(&self) -> f32 {
        match self {
            Self::Visible => 0.0,
            Self::SlidingOut {
                start,
                from_factor,
                duration,
            } => {
                let t = progress_clamped(*start, *duration);
                let eased = cubic_bezier(t);
                from_factor + (1.0 - from_factor) * eased
            }
            Self::Hidden => 1.0,
            Self::SlidingIn {
                start,
                from_factor,
                duration,
            } => {
                let t = progress_clamped(*start, *duration);
                let eased = cubic_bezier(t);
                from_factor * (1.0 - eased)
            }
        }
    }

    /// True while any animation is active.
    pub fn is_animating(&self) -> bool {
        matches!(self, Self::SlidingOut { .. } | Self::SlidingIn { .. })
    }

    /// Fraction of the slide still to run: ~1.0 right after a transition
    /// starts, 0.0 once settled. Eased, and consistent across reversals —
    /// used to drive content crossfades locked to the motion.
    pub fn remaining_fraction(&self) -> f32 {
        match self {
            Self::Visible | Self::Hidden => 0.0,
            // factor: current → 0
            Self::SlidingIn { .. } => self.factor().clamp(0.0, 1.0),
            // factor: current → 1
            Self::SlidingOut { .. } => (1.0 - self.factor()).clamp(0.0, 1.0),
        }
    }

    /// Advance the state machine. Returns `Some(true)` when fully visible,
    /// `Some(false)` when fully hidden (terminal transitions).
    pub fn update(&mut self) -> Option<bool> {
        match self {
            Self::SlidingOut {
                start, duration, ..
            } => {
                if start.elapsed() >= *duration {
                    *self = Self::Hidden;
                    Some(false)
                } else {
                    None
                }
            }
            Self::SlidingIn {
                start, duration, ..
            } => {
                if start.elapsed() >= *duration {
                    *self = Self::Visible;
                    Some(true)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Begin sliding out (hiding).
    pub fn start_hide(&mut self) {
        match self {
            Self::Visible => {
                *self = Self::SlidingOut {
                    start: Instant::now(),
                    from_factor: 0.0,
                    duration: SLIDE_OUT_DURATION,
                };
            }
            Self::SlidingIn { .. } => {
                // Reverse from current position; remaining distance is 1 - current.
                let current = self.factor();
                *self = Self::SlidingOut {
                    start: Instant::now(),
                    from_factor: current,
                    duration: SLIDE_OUT_DURATION.mul_f32((1.0 - current).clamp(0.0, 1.0)),
                };
            }
            _ => {} // Already hiding or hidden
        }
    }

    /// Begin sliding in (showing).
    pub fn start_show(&mut self) {
        match self {
            Self::Hidden => {
                *self = Self::SlidingIn {
                    start: Instant::now(),
                    from_factor: 1.0,
                    duration: SLIDE_IN_DURATION,
                };
            }
            Self::SlidingOut { .. } => {
                // Reverse from current position; remaining distance is the current factor.
                let current = self.factor();
                *self = Self::SlidingIn {
                    start: Instant::now(),
                    from_factor: current,
                    duration: SLIDE_IN_DURATION.mul_f32(current.clamp(0.0, 1.0)),
                };
            }
            _ => {} // Already showing or visible
        }
    }
}

/// Per-surface slide animation tracking.
#[derive(Debug)]
pub struct LayerSlide {
    /// The surface ObjectId this slide is for.
    pub surface_id: ObjectId,
    /// Which edge the surface slides toward.
    pub edge: SlideEdge,
    /// Current animation state.
    pub visibility: SlideVisibility,
    /// Width of the surface (for computing offset distance).
    pub surface_width: i32,
    /// The surface's exclusive zone value (for animating workspace layout).
    pub exclusive_zone: i32,
    /// The exclusive zone value last written into cached state and arranged.
    /// Lets the animation tick skip re-arranging/relayouting when the integer
    /// zone hasn't moved a whole pixel since the last application.
    pub last_applied_ez: Option<i32>,
}

impl LayerSlide {
    pub fn new(
        surface_id: ObjectId,
        edge: SlideEdge,
        surface_width: i32,
        exclusive_zone: i32,
    ) -> Self {
        Self {
            surface_id,
            edge,
            visibility: SlideVisibility::Visible,
            surface_width,
            exclusive_zone,
            last_applied_ez: None,
        }
    }

    /// Create a slide starting from fully hidden (for slide-in animations).
    pub fn new_hidden(
        surface_id: ObjectId,
        edge: SlideEdge,
        surface_width: i32,
        exclusive_zone: i32,
    ) -> Self {
        Self {
            surface_id,
            edge,
            visibility: SlideVisibility::Hidden,
            surface_width,
            exclusive_zone,
            last_applied_ez: None,
        }
    }

    /// Compute the render offset for the current animation factor.
    /// Returns (x_offset, y_offset) to shift the surface off its edge.
    pub fn render_offset(&self) -> (i32, i32) {
        let factor = self.visibility.factor();
        if factor == 0.0 {
            return (0, 0);
        }
        let max_offset = self.surface_width;
        let offset = (max_offset as f32 * factor).round() as i32;
        match self.edge {
            SlideEdge::Left => (-offset, 0),
            SlideEdge::Right => (offset, 0),
        }
    }

    /// Compute the current effective exclusive zone (animated).
    /// When fully visible (factor=0), returns full exclusive_zone.
    /// When fully hidden (factor=1), returns 0.
    /// During animation, interpolates between these values.
    ///
    /// Computed as the exact complement of `render_offset`'s rounding so that
    /// (when `exclusive_zone == surface_width`, the common case) the panel's
    /// sliding edge and the workspace zone edge always sum to the full width —
    /// rounding them independently produced transient 1px seams/overlaps.
    pub fn effective_exclusive_zone(&self) -> i32 {
        let factor = self.visibility.factor();
        self.exclusive_zone - ((self.exclusive_zone as f32) * factor).round() as i32
    }
}

/// cubic-bezier(0.16, 1, 0.3, 1) — fast start, gentle deceleration.
/// Uses De Casteljau subdivision to solve t→x then sample y.
fn cubic_bezier(t: f32) -> f32 {
    // Control points: P0=(0,0), P1=(0.16,1), P2=(0.3,1), P3=(1,1)
    const X1: f64 = 0.16;
    const Y1: f64 = 1.0;
    const X2: f64 = 0.3;
    const Y2: f64 = 1.0;

    let t = t as f64;
    // Newton-Raphson to find parameter u where bezier_x(u) = t
    let mut u = t; // initial guess
    for _ in 0..8 {
        let x = bezier_component(u, X1, X2) - t;
        let dx = bezier_component_derivative(u, X1, X2);
        if dx.abs() < 1e-12 {
            break;
        }
        u -= x / dx;
        u = u.clamp(0.0, 1.0);
    }
    bezier_component(u, Y1, Y2) as f32
}

/// Evaluate one component of a cubic bezier at parameter u.
/// B(u) = 3(1-u)^2*u*p1 + 3(1-u)*u^2*p2 + u^3
fn bezier_component(u: f64, p1: f64, p2: f64) -> f64 {
    let u2 = u * u;
    let u3 = u2 * u;
    let inv = 1.0 - u;
    let inv2 = inv * inv;
    3.0 * inv2 * u * p1 + 3.0 * inv * u2 * p2 + u3
}

/// Derivative of bezier_component with respect to u.
fn bezier_component_derivative(u: f64, p1: f64, p2: f64) -> f64 {
    let inv = 1.0 - u;
    3.0 * inv * inv * p1 + 6.0 * inv * u * (p2 - p1) + 3.0 * u * u * (1.0 - p2)
}

/// Clamped progress ratio for an animation started at `start` with `duration`.
/// A zero duration yields 1.0 (animation instantly complete).
fn progress_clamped(start: Instant, duration: Duration) -> f32 {
    (start.elapsed().as_secs_f32() / duration.as_secs_f32()).min(1.0)
}
