// SPDX-License-Identifier: GPL-3.0-only

//! Compositor-side open/close animation for layer-shell surfaces — the DEFAULT
//! show/hide transition for every surface that isn't edge-sliding (see
//! [`super::layer_slide`] and [`super::Shell::set_surface_hidden`]).
//!
//! It first shipped for agentos-panel's popover surfaces and now applies to all
//! fade+rise surfaces (panels, popovers, modals, notifications, the launcher…),
//! which animate IN when shown rather than appearing instantly.
//!
//! The animation matches the design prototype, with values resolved from the
//! theme's motion tokens (captured into [`motion::Motion`] at creation):
//! - duration: `motion.layer_open`
//! - easing: `motion.ease_in_out` (design `--ease-in-out`)
//! - translateY: +6px (below the resting anchored position) → 0 (slides UP)
//! - scale: 0.97 → 1.0
//! - opacity: 0 → 1
//! - transform-origin: CENTER of the surface
//!
//! ALL THREE channels (alpha, translateY, scale) are driven from a single
//! eased factor `t ∈ [0,1]` so they stay perfectly in sync.

use crate::backend::render::animations::motion;
use icetron_p::animation::easing;
use std::time::{Duration, Instant};
use wayland_backend::server::ObjectId;

/// Distance the surface rises during the animation (design `translateY: 6px → 0`).
pub const OPEN_RISE_PX: f32 = 6.0;
/// Starting scale of the surface (design `scale: 0.97 → 1.0`).
pub const START_SCALE: f32 = 0.97;

/// Which show/hide motion a surface plays.
///
/// Both styles drive the same three channels through the same render path; they
/// differ only in how far, how long, and on what curve.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Style {
    /// The default: a subtle rise and fade, for popovers, panels and modals.
    #[default]
    FadeRise,
    /// The chat input's arrival, from the design's `fluidReveal` /
    /// `fluidDismiss` keyframes.
    ///
    /// Travels much further than [`Style::FadeRise`] and overshoots on the way
    /// in, so it reads as something landing rather than materialising. It lives
    /// here, in the compositor, rather than in the client because the surface's
    /// BACKDROP BLUR animates with the surface — a client animating its own
    /// pixels leaves the blur behind as a rectangle that cannot follow it.
    FluidReveal,
}

/// `fluidReveal`: distance the surface starts below its resting place.
pub const FLUID_ENTER_RISE_PX: f32 = 24.0;
/// `fluidReveal`: scale it starts at.
pub const FLUID_ENTER_SCALE: f32 = 0.92;
/// `fluidDismiss`: distance the surface sinks to. Less than it rose — leaving
/// is deliberately not the arrival reversed.
pub const FLUID_EXIT_DROP_PX: f32 = 16.0;
/// `fluidDismiss`: scale it ends at.
pub const FLUID_EXIT_SCALE: f32 = 0.96;
/// `fluidReveal`: 400ms in.
pub const FLUID_ENTER: Duration = Duration::from_millis(400);
/// `fluidDismiss`: 300ms out, shorter than the arrival.
pub const FLUID_EXIT: Duration = Duration::from_millis(300);
/// `fluidReveal`: the point the opacity ramp changes slope, as a fraction of
/// the duration. The content is most of the way readable by here, while the
/// overshoot is still settling.
const FLUID_FADE_KNEE: f32 = 0.4;
/// `fluidReveal`: opacity at the knee.
const FLUID_FADE_KNEE_ALPHA: f32 = 0.8;

/// Per-surface open-animation tracking.
#[derive(Debug, Clone)]
pub struct LayerOpen {
    /// The surface ObjectId this open animation is for.
    pub surface_id: ObjectId,
    /// When the animation started (first buffer commit).
    pub start: Instant,
    /// Motion tokens captured from the theme when the animation began.
    motion: motion::Motion,
    /// Which motion this surface asked for.
    style: Style,
}

impl LayerOpen {
    pub fn new(surface_id: ObjectId, motion: motion::Motion) -> Self {
        Self::styled(surface_id, motion, Style::default())
    }

    /// As [`LayerOpen::new`], with an explicit [`Style`].
    pub fn styled(surface_id: ObjectId, motion: motion::Motion, style: Style) -> Self {
        Self {
            surface_id,
            start: Instant::now(),
            motion,
            style,
        }
    }

    /// How long this style's entrance runs for.
    fn duration(&self) -> Duration {
        match self.style {
            Style::FadeRise => self.motion.layer_open,
            Style::FluidReveal => FLUID_ENTER,
        }
    }

    /// Create an open whose clock is back-dated by `back_ms`, so it begins at a
    /// non-zero progress. Used to hand off from an in-flight CLOSE seamlessly:
    /// starting the open at linear progress `1 - p` (i.e.
    /// `back_ms = (1 - p) * layer_open`) makes its first frame match the
    /// close's current alpha/scale/offset exactly — because the easing is
    /// point-symmetric about (0.5, 0.5) — so a surface re-shown mid-dismissal
    /// rises the rest of the way instead of snapping to fully hidden first.
    pub fn new_backdated(surface_id: ObjectId, back_ms: u64, motion: motion::Motion) -> Self {
        Self::styled_backdated(surface_id, back_ms, motion, Style::default())
    }

    /// As [`LayerOpen::new_backdated`], with an explicit [`Style`].
    pub fn styled_backdated(
        surface_id: ObjectId,
        back_ms: u64,
        motion: motion::Motion,
        style: Style,
    ) -> Self {
        let now = Instant::now();
        let start = now
            .checked_sub(Duration::from_millis(back_ms))
            .unwrap_or(now);
        Self {
            surface_id,
            start,
            motion,
            style,
        }
    }

    /// Linear progress through the animation, `0.0` at start to `1.0` at rest.
    fn progress(&self) -> f32 {
        (self.start.elapsed().as_secs_f32() / self.duration().as_secs_f32()).clamp(0.0, 1.0)
    }

    /// The single eased factor `t ∈ [0,1]` that drives translate and scale.
    /// `0.0` at animation start, `1.0` at rest.
    ///
    /// `FluidReveal` uses icetron's `EASE_OUT_BACK` — the same curve the design
    /// names — which deliberately returns values ABOVE 1.0 partway through. That
    /// overshoot is the effect; anything consuming this must not clamp it.
    pub fn factor(&self) -> f32 {
        match self.style {
            Style::FadeRise => self.motion.ease_in_out(self.progress()),
            Style::FluidReveal => easing::EASE_OUT_BACK.y_at_x(self.progress()),
        }
    }

    /// Opacity for the surface: `0.0 → 1.0`.
    ///
    /// `FluidReveal` runs opacity on its own two-slope ramp rather than the
    /// eased factor, so the content is readable well before the overshoot has
    /// settled — and so the overshoot above 1.0 never reaches alpha.
    pub fn alpha(&self) -> f32 {
        match self.style {
            Style::FadeRise => self.factor(),
            Style::FluidReveal => {
                let p = self.progress();
                if p < FLUID_FADE_KNEE {
                    p / FLUID_FADE_KNEE * FLUID_FADE_KNEE_ALPHA
                } else {
                    FLUID_FADE_KNEE_ALPHA
                        + (p - FLUID_FADE_KNEE) / (1.0 - FLUID_FADE_KNEE)
                            * (1.0 - FLUID_FADE_KNEE_ALPHA)
                }
            }
        }
    }

    /// Translation offset `(x, y)` in logical pixels.
    /// Starts at `(0, +OPEN_RISE_PX)` (below the resting position) and settles to
    /// `(0, 0)` — i.e. it slides UP.
    pub fn translate_offset(&self) -> (i32, i32) {
        let t = self.factor();
        let rise = match self.style {
            Style::FadeRise => OPEN_RISE_PX,
            Style::FluidReveal => FLUID_ENTER_RISE_PX,
        };
        (0, ((1.0 - t) * rise).round() as i32)
    }

    /// Scale for the surface, rising to `1.0` about its CENTER.
    pub fn scale(&self) -> f32 {
        let t = self.factor();
        let from = match self.style {
            Style::FadeRise => START_SCALE,
            Style::FluidReveal => FLUID_ENTER_SCALE,
        };
        from + t * (1.0 - from)
    }

    /// True while the animation is still running.
    pub fn is_animating(&self) -> bool {
        self.start.elapsed() < self.duration()
    }
}

/// Per-surface close-animation tracking: the EXACT REVERSE of [`LayerOpen`].
///
/// How far to back-date a close animation, given what the surface was doing.
///
/// The close always runs; where it STARTS is the whole question, and there are
/// three answers:
///
/// * mid-open at linear progress `p` — start where the open had got to, so the
///   two meet instead of the close snapping to fully-open and popping. The
///   easing is point-symmetric about (0.5, 0.5), hence `1 - p`.
/// * nothing on screen yet — start where the close would have ENDED, i.e.
///   already hidden. In practice this is a surface that was re-shown and is
///   still waiting for the first commit of its content, so there is nothing
///   drawn to animate away.
/// * open and resting — start from the top, a full close.
///
/// A surface that has NEVER been on screen does not reach here at all:
/// `set_surface_hidden` hides it outright, because a back-dated close still
/// leaves it nominally visible until the next cleanup pass and a frame can slip
/// through that gap.
#[must_use]
pub fn close_backdate(
    open_progress: Option<f32>,
    on_screen_now: bool,
    layer_open: Duration,
) -> Duration {
    match open_progress {
        Some(progress) => layer_open.mul_f32(1.0 - progress.clamp(0.0, 1.0)),
        None if !on_screen_now => layer_open,
        None => Duration::ZERO,
    }
}

/// Plays when a fade+rise surface is hidden via the `layer_surface_visibility`
/// protocol (the client sends `HideWindow`, then typically destroys the surface
/// once this completes). The surface stays alive and rendered (from its last
/// committed buffer) for the duration so it can animate OUT — the reverse of the
/// entrance:
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
    /// Motion tokens captured from the theme when the animation began.
    motion: motion::Motion,
    /// Which motion this surface asked for.
    style: Style,
}

impl LayerClose {
    pub fn new(surface_id: ObjectId, motion: motion::Motion) -> Self {
        Self::styled(surface_id, motion, Style::default())
    }

    /// As [`LayerClose::new`], with an explicit [`Style`].
    pub fn styled(surface_id: ObjectId, motion: motion::Motion, style: Style) -> Self {
        Self {
            surface_id,
            start: Instant::now(),
            motion,
            style,
        }
    }

    /// How long this style's exit runs for.
    fn duration(&self) -> Duration {
        match self.style {
            Style::FadeRise => self.motion.layer_open,
            Style::FluidReveal => FLUID_EXIT,
        }
    }

    /// Create a close whose clock is back-dated by `back_ms`, so it begins at a
    /// non-zero progress. Used to hand off from an in-flight OPEN seamlessly:
    /// because the easing is point-symmetric about (0.5, 0.5), starting the
    /// close at linear progress `1 - p` (i.e. `back_ms = (1 - p) * layer_open`)
    /// makes its first frame match the open's current alpha/scale/offset exactly
    /// — no jump when a popover is dismissed mid-entrance. A surface that was
    /// never actually shown (`back_ms == layer_open`) starts already hidden.
    pub fn new_backdated(surface_id: ObjectId, back_ms: u64, motion: motion::Motion) -> Self {
        Self::styled_backdated(surface_id, back_ms, motion, Style::default())
    }

    /// As [`LayerClose::new_backdated`], with an explicit [`Style`].
    pub fn styled_backdated(
        surface_id: ObjectId,
        back_ms: u64,
        motion: motion::Motion,
        style: Style,
    ) -> Self {
        let now = Instant::now();
        let start = now
            .checked_sub(Duration::from_millis(back_ms))
            .unwrap_or(now);
        Self {
            surface_id,
            start,
            motion,
            style,
        }
    }

    /// Linear progress through the animation, `0.0` at start to `1.0` when hidden.
    fn progress(&self) -> f32 {
        (self.start.elapsed().as_secs_f32() / self.duration().as_secs_f32()).clamp(0.0, 1.0)
    }

    /// The single eased factor `t ∈ [0,1]` driving all three channels.
    /// `0.0` at the start of the close, `1.0` when fully hidden.
    ///
    /// `FluidReveal` leaves on `EASE_IN` — accelerating away, so it never looks
    /// like it settled. Not the entrance reversed: that would decelerate into
    /// the exit and read as hesitation.
    pub fn factor(&self) -> f32 {
        match self.style {
            Style::FadeRise => self.motion.ease_in_out(self.progress()),
            Style::FluidReveal => easing::EASE_IN.y_at_x(self.progress()),
        }
    }

    /// Opacity for the surface: `1.0 → 0.0`.
    pub fn alpha(&self) -> f32 {
        1.0 - self.factor()
    }

    /// Translation offset `(x, y)` in logical pixels.
    /// Starts at `(0, 0)` (resting) and settles to `(0, +OPEN_RISE_PX)` — i.e.
    /// it slides DOWN, the reverse of the open's slide-up.
    pub fn translate_offset(&self) -> (i32, i32) {
        let t = self.factor();
        let drop = match self.style {
            Style::FadeRise => OPEN_RISE_PX,
            Style::FluidReveal => FLUID_EXIT_DROP_PX,
        };
        (0, (t * drop).round() as i32)
    }

    /// Scale for the surface, falling away about its CENTER.
    pub fn scale(&self) -> f32 {
        let t = self.factor();
        let to = match self.style {
            Style::FadeRise => START_SCALE,
            Style::FluidReveal => FLUID_EXIT_SCALE,
        };
        1.0 - t * (1.0 - to)
    }

    /// True while the animation is still running.
    pub fn is_animating(&self) -> bool {
        self.start.elapsed() < self.duration()
    }
}

// The eased factor is `Motion::ease_in_out` (the theme's `--ease-in-out`),
// shared with every other curve consumer via the captured `motion::Motion`.

#[cfg(test)]
mod backdate_tests {
    use super::*;

    const OPEN: Duration = Duration::from_millis(400);

    /// Compared in whole milliseconds because that is the unit the caller
    /// back-dates in — `mul_f32` lands a few nanoseconds off an exact `Duration`
    /// and the difference cannot reach a frame.
    fn ms(open_progress: Option<f32>, on_screen_now: bool) -> u128 {
        close_backdate(open_progress, on_screen_now, OPEN).as_millis()
    }

    /// A surface with nothing drawn yet — re-shown and still awaiting its first
    /// commit — closes from where the close would have ended, so it does not
    /// appear in order to fade out.
    #[test]
    fn a_surface_with_nothing_drawn_closes_from_the_end() {
        assert_eq!(ms(None, false), OPEN.as_millis());
    }

    /// One that has been on screen and is resting gets the full close.
    #[test]
    fn a_resting_surface_plays_the_whole_close() {
        assert_eq!(ms(None, true), 0);
    }

    /// Interrupting an open picks the close up where the open had got to, so the
    /// two meet rather than the close snapping back to fully-open.
    #[test]
    fn interrupting_an_open_meets_it_where_it_is() {
        assert_eq!(ms(Some(0.25), true), 300);
        assert_eq!(ms(Some(0.75), true), 100);
        // An open that has only just started is nearly a no-op to reverse.
        assert_eq!(ms(Some(0.0), true), OPEN.as_millis());
        // And one that has landed closes in full.
        assert_eq!(ms(Some(1.0), true), 0);
    }

    /// Progress is a ratio of elapsed time to duration, so it can overshoot
    /// between the animation landing and being cleaned up.
    #[test]
    fn progress_past_the_end_is_clamped() {
        assert_eq!(ms(Some(1.8), true), 0);
    }
}
