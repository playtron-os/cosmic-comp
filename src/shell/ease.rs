// SPDX-License-Identifier: GPL-3.0-only

//! Shared spring easing for layer-surface animations.
//!
//! This is the design system's `--ease-spring` / `--duration-slow` pair, used
//! for both the panel show/hide slide ([`super::layer_slide`]) and the panel
//! width resize ([`super::layer_resize_anim`]) so every panel motion shares one
//! curve and timing:
//! - duration: `420ms` (design `--duration-slow`)
//! - easing:   `cubic-bezier(0.16, 1, 0.3, 1)` (design `--ease-spring`)

use std::time::Duration;

/// Duration of a panel motion (design `--duration-slow`).
pub const SPRING_DURATION: Duration = Duration::from_millis(420);

/// `cubic-bezier(0.16, 1, 0.3, 1)` — fast start, gentle deceleration (design
/// `--ease-spring`). Maps linear progress `t ∈ [0,1]` to its eased value by
/// solving `t → u` with Newton-Raphson, then sampling the y component.
pub fn ease_spring(t: f32) -> f32 {
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

/// Derivative of [`bezier_component`] with respect to u.
fn bezier_component_derivative(u: f64, p1: f64, p2: f64) -> f64 {
    let inv = 1.0 - u;
    3.0 * inv * inv * p1 + 6.0 * inv * u * (p2 - p1) + 3.0 * u * u * (1.0 - p2)
}
