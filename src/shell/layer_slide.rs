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
#[derive(Debug, Clone)]
pub enum SlideVisibility {
    /// Fully visible (factor = 0.0, no offset).
    Visible,
    /// Actively sliding off the edge.
    SlidingOut { start: Instant, from_factor: f32 },
    /// Fully hidden (factor = 1.0, fully off-screen).
    Hidden,
    /// Actively sliding back on-screen.
    SlidingIn { start: Instant, from_factor: f32 },
}

impl SlideVisibility {
    /// Returns the current slide factor: 0.0 = fully visible, 1.0 = fully hidden.
    pub fn factor(&self) -> f32 {
        match self {
            Self::Visible => 0.0,
            Self::SlidingOut { start, from_factor } => {
                let t = progress_clamped(*start, SLIDE_OUT_DURATION);
                let eased = cubic_bezier(t);
                from_factor + (1.0 - from_factor) * eased
            }
            Self::Hidden => 1.0,
            Self::SlidingIn { start, from_factor } => {
                let t = progress_clamped(*start, SLIDE_IN_DURATION);
                let eased = cubic_bezier(t);
                from_factor * (1.0 - eased)
            }
        }
    }

    /// True while any animation is active.
    pub fn is_animating(&self) -> bool {
        matches!(self, Self::SlidingOut { .. } | Self::SlidingIn { .. })
    }

    /// Advance the state machine. Returns `Some(true)` when fully visible,
    /// `Some(false)` when fully hidden (terminal transitions).
    pub fn update(&mut self) -> Option<bool> {
        match self {
            Self::SlidingOut { start, .. } => {
                if start.elapsed() >= SLIDE_OUT_DURATION {
                    *self = Self::Hidden;
                    Some(false)
                } else {
                    None
                }
            }
            Self::SlidingIn { start, .. } => {
                if start.elapsed() >= SLIDE_IN_DURATION {
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
                };
            }
            Self::SlidingIn { .. } => {
                // Reverse from current position.
                let current = self.factor();
                *self = Self::SlidingOut {
                    start: Instant::now(),
                    from_factor: current,
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
                };
            }
            Self::SlidingOut { .. } => {
                // Reverse from current position.
                let current = self.factor();
                *self = Self::SlidingIn {
                    start: Instant::now(),
                    from_factor: current,
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
    pub fn effective_exclusive_zone(&self) -> i32 {
        let factor = self.visibility.factor();
        ((self.exclusive_zone as f32) * (1.0 - factor)).round() as i32
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
fn progress_clamped(start: Instant, duration: Duration) -> f32 {
    (start.elapsed().as_secs_f32() / duration.as_secs_f32()).min(1.0)
}

// ---------------------------------------------------------------------------
// Per-output effective non-exclusive zone cache
// ---------------------------------------------------------------------------

use parking_lot::RwLock;
use smithay::utils::{Logical, Rectangle};
use std::collections::HashMap;
use std::sync::LazyLock;

static EFFECTIVE_NON_EXCLUSIVE_ZONES: LazyLock<RwLock<HashMap<String, Rectangle<i32, Logical>>>> =
    LazyLock::new(|| RwLock::new(HashMap::new()));

/// Store the effective non-exclusive zone for an output.
/// Called by Shell during animation updates.
pub fn set_effective_non_exclusive_zone(output_name: &str, zone: Rectangle<i32, Logical>) {
    EFFECTIVE_NON_EXCLUSIVE_ZONES
        .write()
        .insert(output_name.to_string(), zone);
}

/// Get the effective non-exclusive zone for an output.
/// Returns the animated zone if available, otherwise falls back to the
/// layer_map's static non_exclusive_zone.
pub fn get_effective_non_exclusive_zone(
    output: &smithay::output::Output,
) -> Rectangle<i32, Logical> {
    let name = output.name();
    if let Some(zone) = EFFECTIVE_NON_EXCLUSIVE_ZONES.read().get(&name) {
        *zone
    } else {
        smithay::desktop::layer_map_for_output(output).non_exclusive_zone()
    }
}
