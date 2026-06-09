// SPDX-License-Identifier: GPL-3.0-only

//! Compositor-side open animation for popover layer-shell surfaces.
//!
//! agentos-panel's popover surfaces (namespace `agentos-panel-popover`) are
//! `Layer::Overlay`, anchored bottom, auto-sized. They should animate IN when
//! first shown rather than appearing instantly.
//!
//! The animation matches the design prototype exactly:
//! - duration: 160ms
//! - easing: easeInOut == cubic-bezier(0.42, 0, 0.58, 1)
//! - translateY: +6px (below the resting anchored position) → 0 (slides UP)
//! - scale: 0.97 → 1.0
//! - opacity: 0 → 1
//! - transform-origin: CENTER of the surface
//!
//! ALL THREE channels (alpha, translateY, scale) are driven from a single
//! eased factor `t ∈ [0,1]` so they stay perfectly in sync.

use std::time::{Duration, Instant};
use wayland_backend::server::ObjectId;

/// Total duration of the open animation (design `160ms`).
pub const OPEN_DURATION: Duration = Duration::from_millis(160);
/// Distance the surface rises during the animation (design `translateY: 6px → 0`).
pub const OPEN_RISE_PX: f32 = 6.0;
/// Starting scale of the surface (design `scale: 0.97 → 1.0`).
pub const START_SCALE: f32 = 0.97;

/// Per-surface open-animation tracking.
#[derive(Debug, Clone)]
pub struct LayerOpen {
    /// The surface ObjectId this open animation is for.
    pub surface_id: ObjectId,
    /// When the animation started (first buffer commit).
    pub start: Instant,
}

impl LayerOpen {
    pub fn new(surface_id: ObjectId) -> Self {
        Self {
            surface_id,
            start: Instant::now(),
        }
    }

    /// The single eased factor `t ∈ [0,1]` that drives all three channels.
    /// `0.0` at animation start, `1.0` at rest. easeInOut over `OPEN_DURATION`.
    pub fn factor(&self) -> f32 {
        let progress =
            (self.start.elapsed().as_secs_f32() / OPEN_DURATION.as_secs_f32()).clamp(0.0, 1.0);
        ease_in_out(progress)
    }

    /// Opacity for the surface: `0.0 → 1.0`, equal to the eased factor.
    pub fn alpha(&self) -> f32 {
        self.factor()
    }

    /// Translation offset `(x, y)` in logical pixels.
    /// Starts at `(0, +OPEN_RISE_PX)` (below the resting position) and settles to
    /// `(0, 0)` — i.e. it slides UP.
    pub fn translate_offset(&self) -> (i32, i32) {
        let t = self.factor();
        (0, ((1.0 - t) * OPEN_RISE_PX).round() as i32)
    }

    /// Scale for the surface: `START_SCALE → 1.0`, scaled around its CENTER.
    pub fn scale(&self) -> f32 {
        let t = self.factor();
        START_SCALE + t * (1.0 - START_SCALE)
    }

    /// True while the animation is still running.
    pub fn is_animating(&self) -> bool {
        self.start.elapsed() < OPEN_DURATION
    }
}

/// Total duration of the close animation (mirrors the open: design `160ms`).
pub const CLOSE_DURATION: Duration = Duration::from_millis(160);

/// Per-surface close-animation tracking: the EXACT REVERSE of [`LayerOpen`].
///
/// Plays when an `agentos-panel-popover` surface is hidden via the
/// `layer_surface_visibility` protocol (the panel sends `HideWindow`, then
/// destroys the surface once this completes). The surface stays alive and
/// rendered (from its last committed buffer) for the duration so it can
/// animate OUT — the reverse of the entrance:
/// - translateY: 0 → +6px (slides DOWN, below the resting position)
/// - scale: 1.0 → 0.97 (scales DOWN about CENTER)
/// - opacity: 1 → 0 (fades OUT)
///
/// All three channels are driven from the SAME eased factor so they stay
/// in sync, identical easing to the open.
#[derive(Debug, Clone)]
pub struct LayerClose {
    /// The surface ObjectId this close animation is for.
    pub surface_id: ObjectId,
    /// When the animation started (the `set_surface_hidden(true)` request).
    pub start: Instant,
}

impl LayerClose {
    pub fn new(surface_id: ObjectId) -> Self {
        Self {
            surface_id,
            start: Instant::now(),
        }
    }

    /// Create a close whose clock is back-dated by `back_ms`, so it begins at a
    /// non-zero progress. Used to hand off from an in-flight OPEN seamlessly:
    /// because the easing is point-symmetric about (0.5, 0.5), starting the
    /// close at linear progress `1 - p` (i.e. `back_ms = (1 - p) * CLOSE_DURATION`)
    /// makes its first frame match the open's current alpha/scale/offset exactly
    /// — no jump when a popover is dismissed mid-entrance. A surface that was
    /// never actually shown (`back_ms == CLOSE_DURATION`) starts already hidden.
    pub fn new_backdated(surface_id: ObjectId, back_ms: u64) -> Self {
        let now = Instant::now();
        let start = now
            .checked_sub(Duration::from_millis(back_ms))
            .unwrap_or(now);
        Self { surface_id, start }
    }

    /// The single eased factor `t ∈ [0,1]` driving all three channels.
    /// `0.0` at the start of the close, `1.0` when fully hidden.
    pub fn factor(&self) -> f32 {
        let progress =
            (self.start.elapsed().as_secs_f32() / CLOSE_DURATION.as_secs_f32()).clamp(0.0, 1.0);
        ease_in_out(progress)
    }

    /// Opacity for the surface: `1.0 → 0.0` (the reverse of the open).
    pub fn alpha(&self) -> f32 {
        1.0 - self.factor()
    }

    /// Translation offset `(x, y)` in logical pixels.
    /// Starts at `(0, 0)` (resting) and settles to `(0, +OPEN_RISE_PX)` — i.e.
    /// it slides DOWN, the reverse of the open's slide-up.
    pub fn translate_offset(&self) -> (i32, i32) {
        let t = self.factor();
        (0, (t * OPEN_RISE_PX).round() as i32)
    }

    /// Scale for the surface: `1.0 → START_SCALE`, about its CENTER.
    pub fn scale(&self) -> f32 {
        let t = self.factor();
        1.0 - t * (1.0 - START_SCALE)
    }

    /// True while the animation is still running.
    pub fn is_animating(&self) -> bool {
        self.start.elapsed() < CLOSE_DURATION
    }
}

/// easeInOut == cubic-bezier(0.42, 0, 0.58, 1) (the CSS `ease-in-out` keyword).
/// Solves the parametric cubic bezier for `y` given `x = t` via Newton-Raphson.
fn ease_in_out(t: f32) -> f32 {
    // Control points: P0=(0,0), P1=(0.42,0), P2=(0.58,1), P3=(1,1)
    const X1: f64 = 0.42;
    const Y1: f64 = 0.0;
    const X2: f64 = 0.58;
    const Y2: f64 = 1.0;

    let t = t as f64;
    // Newton-Raphson to find parameter u where bezier_x(u) = t.
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

/// Evaluate one component of a cubic bezier at parameter `u`.
/// B(u) = 3(1-u)^2*u*p1 + 3(1-u)*u^2*p2 + u^3
fn bezier_component(u: f64, p1: f64, p2: f64) -> f64 {
    let u2 = u * u;
    let u3 = u2 * u;
    let inv = 1.0 - u;
    let inv2 = inv * inv;
    3.0 * inv2 * u * p1 + 3.0 * inv * u2 * p2 + u3
}

/// Derivative of `bezier_component` with respect to `u`.
fn bezier_component_derivative(u: f64, p1: f64, p2: f64) -> f64 {
    let inv = 1.0 - u;
    3.0 * inv * inv * p1 + 6.0 * inv * u * (p2 - p1) + 3.0 * u * u * (1.0 - p2)
}
