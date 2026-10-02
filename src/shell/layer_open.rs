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
//!
//! A surface can instead name what it is (`layer_surface_visibility` v5: popover, panel,
//! launcher, notification, ...). The theme's motion for that role then moves it
//! ([`Style::Kit`]): a spring or tween from the role's enter pose to rest, and from rest
//! toward its exit pose. A role the theme gives no motion plays FadeRise as above.

use crate::backend::render::animations::motion;
use icetron_p::animation::{easing, spring::Spring};
use icetron_themes::{LayerMotion, MotionCurve, MotionPose};
use std::time::{Duration, Instant};
use wayland_backend::server::ObjectId;

/// Distance the surface rises during the animation (design `translateY: 6px → 0`).
pub const OPEN_RISE_PX: f32 = 6.0;
/// Starting scale of the surface (design `scale: 0.97 → 1.0`).
pub const START_SCALE: f32 = 0.97;

/// Shared preset behind the visibility protocol's `fade` transition and
/// compositor-owned popups, which have no wl_surface to send that request on.
#[derive(Debug, Clone, Copy)]
pub(crate) struct FadeRise {
    pub duration: Duration,
    pub curve: [f32; 4],
}

impl FadeRise {
    pub fn new(motion: motion::Motion) -> Self {
        Self {
            duration: motion.layer_open,
            curve: motion.ease_in_out_cp,
        }
    }

    pub fn factor(self, progress: f32) -> f32 {
        motion::cubic_bezier_cp(progress, self.curve)
    }

    pub fn offset(factor: f32) -> f32 {
        (1.0 - factor) * OPEN_RISE_PX
    }

    pub fn scale(factor: f32) -> f32 {
        START_SCALE + factor * (1.0 - START_SCALE)
    }
}

/// What a surface is, as it names itself through `layer_surface_visibility` v5. The theme
/// gives each role its own motion; a role it gives none plays [`Style::FadeRise`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Role {
    Popover,
    Panel,
    ControlPanel,
    Launcher,
    Spotlight,
    Notification,
    ContextMenu,
    Modal,
}

impl Role {
    /// The theme's motion for this role, `None` for FadeRise.
    pub fn motion(self, surfaces: &motion::SurfaceMotions) -> Option<LayerMotion> {
        match self {
            Role::Popover => surfaces.popover,
            Role::Panel => surfaces.panel,
            Role::ControlPanel => surfaces.control_panel,
            Role::Launcher => surfaces.launcher,
            Role::Spotlight => surfaces.spotlight,
            Role::Notification => surfaces.notification,
            Role::ContextMenu => surfaces.context_menu,
            Role::Modal => surfaces.modal,
        }
    }
}

/// Where a surface is drawn while it moves: opacity, offset in logical px, and scale about its
/// centre.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Pose {
    pub alpha: f32,
    pub x: f32,
    pub y: f32,
    pub scale: f32,
}

impl Pose {
    pub const REST: Self = Self {
        alpha: 1.0,
        x: 0.0,
        y: 0.0,
        scale: 1.0,
    };

    /// A theme's hidden pose, where the surface is transparent.
    pub fn hidden(pose: MotionPose) -> Self {
        Self {
            alpha: 0.0,
            x: pose.x,
            y: pose.y,
            scale: pose.scale,
        }
    }

    /// `progress` of the way to `to`. A spring passes 1 on the way; only opacity is held.
    pub fn toward(self, to: Self, progress: f32) -> Self {
        let lerp = |a: f32, b: f32| a + (b - a) * progress;
        Self {
            alpha: lerp(self.alpha, to.alpha).clamp(0.0, 1.0),
            x: lerp(self.x, to.x),
            y: lerp(self.y, to.y),
            scale: lerp(self.scale, to.scale),
        }
    }
}

/// A role's motion from the theme, its length worked out once.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Kit {
    pub motion: LayerMotion,
    pub duration: Duration,
}

impl Kit {
    pub fn new(motion: LayerMotion) -> Self {
        let duration = match motion.curve {
            MotionCurve::Spring(spring) => {
                // Settled on a whole millisecond.
                motion::ms(Spring::from_token(spring).settle_time() * 1000.0)
            }
            MotionCurve::Tween { ms, .. } => motion::ms(ms),
        };
        Self { motion, duration }
    }

    /// How far toward its target a move on this curve has gone after `elapsed`: 0 at the
    /// start, 1 there; a spring passes 1 on the way.
    pub fn progress(&self, elapsed: Duration) -> f32 {
        match self.motion.curve {
            MotionCurve::Spring(spring) => {
                Spring::from_token(spring).progress(elapsed.as_secs_f32()).0
            }
            MotionCurve::Tween { ease, .. } => {
                let t = if self.duration.is_zero() {
                    1.0
                } else {
                    (elapsed.as_secs_f32() / self.duration.as_secs_f32()).min(1.0)
                };
                motion::cubic_bezier_cp(t, ease)
            }
        }
    }

    pub fn enter(&self) -> Pose {
        Pose::hidden(self.motion.enter)
    }

    pub fn exit(&self) -> Pose {
        Pose::hidden(self.motion.exit)
    }
}

/// Which show/hide motion a surface plays.
///
/// Every style drives the same channels through the same render path; they differ only in how
/// far, how long, and on what curve.
#[derive(Debug, Clone, Copy, PartialEq, Default)]
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
    /// A role's motion from the theme: its curve between its enter and exit poses.
    Kit(Kit),
}

impl Style {
    /// What a surface plays for the role it named: the theme's motion for it, or FadeRise.
    pub fn for_role(role: Role, motion: &motion::Motion) -> Self {
        role.motion(&motion.surfaces)
            .map_or(Style::FadeRise, |m| Style::Kit(Kit::new(m)))
    }
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

/// `COSMIC_HOLD_LAYER_OPEN_MS`: hold every entrance that many ms in, so the headless harness can
/// shoot it at fixed times. Unset, entrances run on the clock.
pub(crate) fn held_open() -> Option<Duration> {
    static HOLD: std::sync::OnceLock<Option<Duration>> = std::sync::OnceLock::new();
    *HOLD.get_or_init(|| {
        std::env::var("COSMIC_HOLD_LAYER_OPEN_MS")
            .ok()?
            .parse()
            .ok()
            .map(Duration::from_millis)
    })
}

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
    /// Where a [`Style::Kit`] entrance starts, when it takes over from a close in flight.
    from: Option<Pose>,
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
            from: None,
        }
    }

    /// As [`LayerOpen::styled`], starting from `from` rather than the style's own start: how a [`Style::Kit`]
    /// motion takes over from one in flight, as motion retargets from the current value.
    pub fn styled_from(
        surface_id: ObjectId,
        motion: motion::Motion,
        style: Style,
        from: Pose,
    ) -> Self {
        Self {
            from: Some(from),
            ..Self::styled(surface_id, motion, style)
        }
    }

    /// How long this style's entrance runs for.
    fn duration(&self) -> Duration {
        match self.style {
            Style::FadeRise => FadeRise::new(self.motion).duration,
            Style::FluidReveal => FLUID_ENTER,
            Style::Kit(kit) => kit.duration,
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
            from: None,
        }
    }

    /// Time since the entrance began, or the harness's held time.
    fn elapsed(&self) -> Duration {
        held_open().unwrap_or_else(|| self.start.elapsed())
    }

    /// Linear progress through the animation, `0.0` at start to `1.0` at rest.
    fn progress(&self) -> f32 {
        (self.elapsed().as_secs_f32() / self.duration().as_secs_f32()).clamp(0.0, 1.0)
    }

    /// The single eased factor `t ∈ [0,1]` that drives translate and scale.
    /// `0.0` at animation start, `1.0` at rest.
    ///
    /// `FluidReveal` uses icetron's `EASE_OUT_BACK` — the same curve the design
    /// names — which deliberately returns values ABOVE 1.0 partway through. That
    /// overshoot is the effect; anything consuming this must not clamp it.
    pub fn factor(&self) -> f32 {
        match self.style {
            Style::FadeRise => FadeRise::new(self.motion).factor(self.progress()),
            Style::FluidReveal => easing::EASE_OUT_BACK.y_at_x(self.progress()),
            Style::Kit(kit) => kit.progress(self.elapsed()),
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
            Style::Kit(_) => self.pose().alpha,
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
        if let Style::Kit(_) = self.style {
            let pose = self.pose();
            return (pose.x.round() as i32, pose.y.round() as i32);
        }
        let t = self.factor();
        let offset = match self.style {
            Style::FadeRise | Style::Kit(_) => FadeRise::offset(t),
            Style::FluidReveal => (1.0 - t) * FLUID_ENTER_RISE_PX,
        };
        (0, offset.round() as i32)
    }

    /// Scale for the surface, rising to `1.0` about its CENTER.
    pub fn scale(&self) -> f32 {
        let t = self.factor();
        match self.style {
            Style::FadeRise => FadeRise::scale(t),
            Style::FluidReveal => FLUID_ENTER_SCALE + t * (1.0 - FLUID_ENTER_SCALE),
            Style::Kit(_) => self.pose().scale,
        }
    }

    /// Where a [`Style::Kit`] entrance has the surface now. Other styles read their own channels.
    pub fn pose(&self) -> Pose {
        match self.style {
            Style::Kit(kit) => self
                .from
                .unwrap_or_else(|| kit.enter())
                .toward(Pose::REST, kit.progress(self.elapsed())),
            _ => {
                let (x, y) = self.translate_offset();
                Pose {
                    alpha: self.alpha(),
                    x: x as f32,
                    y: y as f32,
                    scale: self.scale(),
                }
            }
        }
    }

    /// True while the animation is still running.
    pub fn is_animating(&self) -> bool {
        self.elapsed() < self.duration()
    }
}

/// Per-surface close-animation tracking: the EXACT REVERSE of [`LayerOpen`].
///
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
    /// Where a [`Style::Kit`] exit starts, when it takes over from an entrance in flight.
    from: Option<Pose>,
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
            from: None,
        }
    }

    /// As [`LayerClose::styled`], starting from `from` rather than at rest: how a [`Style::Kit`]
    /// motion takes over from one in flight, as motion retargets from the current value.
    pub fn styled_from(
        surface_id: ObjectId,
        motion: motion::Motion,
        style: Style,
        from: Pose,
    ) -> Self {
        Self {
            from: Some(from),
            ..Self::styled(surface_id, motion, style)
        }
    }

    /// How long this style's exit runs for.
    fn duration(&self) -> Duration {
        match self.style {
            Style::FadeRise => self.motion.layer_open,
            Style::FluidReveal => FLUID_EXIT,
            Style::Kit(kit) => kit.duration,
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
            from: None,
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
            Style::Kit(kit) => kit.progress(self.start.elapsed()),
        }
    }

    /// Opacity for the surface: `1.0 → 0.0`.
    pub fn alpha(&self) -> f32 {
        match self.style {
            Style::Kit(_) => self.pose().alpha,
            _ => 1.0 - self.factor(),
        }
    }

    /// Translation offset `(x, y)` in logical pixels.
    /// Starts at `(0, 0)` (resting) and settles to `(0, +OPEN_RISE_PX)` — i.e.
    /// it slides DOWN, the reverse of the open's slide-up.
    pub fn translate_offset(&self) -> (i32, i32) {
        if let Style::Kit(_) = self.style {
            let pose = self.pose();
            return (pose.x.round() as i32, pose.y.round() as i32);
        }
        let t = self.factor();
        let drop = match self.style {
            Style::FadeRise | Style::Kit(_) => OPEN_RISE_PX,
            Style::FluidReveal => FLUID_EXIT_DROP_PX,
        };
        (0, (t * drop).round() as i32)
    }

    /// Scale for the surface, falling away about its CENTER.
    pub fn scale(&self) -> f32 {
        if let Style::Kit(_) = self.style {
            return self.pose().scale;
        }
        let t = self.factor();
        let to = match self.style {
            Style::FadeRise | Style::Kit(_) => START_SCALE,
            Style::FluidReveal => FLUID_EXIT_SCALE,
        };
        1.0 - t * (1.0 - to)
    }

    /// Where a [`Style::Kit`] exit has the surface now. Other styles read their own channels.
    pub fn pose(&self) -> Pose {
        match self.style {
            Style::Kit(kit) => self
                .from
                .unwrap_or(Pose::REST)
                .toward(kit.exit(), kit.progress(self.start.elapsed())),
            _ => {
                let (x, y) = self.translate_offset();
                Pose {
                    alpha: self.alpha(),
                    x: x as f32,
                    y: y as f32,
                    scale: self.scale(),
                }
            }
        }
    }

    /// True while the animation is still running.
    pub fn is_animating(&self) -> bool {
        self.start.elapsed() < self.duration()
    }
}

// The eased factor is `Motion::ease_in_out` (the theme's `--ease-in-out`),
// shared with every other curve consumer via the captured `motion::Motion`.

#[cfg(test)]
mod tests {
    use super::*;
    use crate::comp_theme::CompTheme;

    const TIMES: [u64; 5] = [0, 40, 88, 120, 200];

    fn pose(x: f32, y: f32, scale: f32) -> MotionPose {
        MotionPose { x, y, scale }
    }

    fn role(curve: MotionCurve, enter: MotionPose, exit: MotionPose) -> LayerMotion {
        LayerMotion { curve, enter, exit }
    }

    /// Playtron's motions, which are the kit's (`icetron-theme-playtron`).
    fn kit() -> motion::SurfaceMotions {
        let snappy = MotionCurve::Spring([400.0, 32.0, 1.0]);
        motion::SurfaceMotions {
            popover: Some(role(snappy, pose(0.0, 6.0, 0.97), pose(0.0, 4.0, 0.97))),
            panel: Some(role(snappy, pose(0.0, 8.0, 0.98), pose(0.0, 6.0, 0.98))),
            control_panel: Some(role(
                MotionCurve::Tween {
                    ms: 160.0,
                    ease: [0.42, 0.0, 0.58, 1.0],
                },
                pose(0.0, 6.0, 0.97),
                pose(0.0, 4.0, 0.97),
            )),
            launcher: Some(role(snappy, pose(0.0, -10.0, 0.97), pose(0.0, -8.0, 0.97))),
            spotlight: Some(role(snappy, pose(0.0, 0.0, 0.97), pose(0.0, 0.0, 0.97))),
            notification: Some(role(
                MotionCurve::Spring([350.0, 25.0, 0.8]),
                pose(24.0, 0.0, 0.97),
                pose(16.0, 0.0, 0.97),
            )),
            context_menu: Some(role(
                MotionCurve::Tween {
                    ms: 120.0,
                    ease: [0.16, 1.0, 0.3, 1.0],
                },
                pose(0.0, -2.0, 0.97),
                pose(0.0, -2.0, 0.98),
            )),
            modal: Some(role(
                MotionCurve::Spring([350.0, 30.0, 1.0]),
                pose(0.0, -8.0, 0.96),
                pose(0.0, -4.0, 0.97),
            )),
        }
    }

    /// How far each curve has gone at [`TIMES`], read off motion itself (`motion-dom`'s spring
    /// and `motion-utils`' cubicBezier, design `main`), and when motion calls a spring done.
    fn motion_truth(role: Role) -> ([f32; 5], Option<u64>) {
        let snappy = ([0.0, 0.207637, 0.595652, 0.787073, 0.993347], Some(473));
        match role {
            Role::Popover | Role::Panel | Role::Launcher | Role::Spotlight => snappy,
            Role::Notification => ([0.0, 0.227870, 0.646824, 0.843486, 1.023697], Some(433)),
            Role::Modal => ([0.0, 0.186741, 0.553639, 0.746477, 0.978180], Some(504)),
            Role::ControlPanel => ([0.0, 0.128941, 0.585682, 0.871059, 1.0], None),
            Role::ContextMenu => ([0.0, 0.902844, 0.997116, 1.0, 1.0], None),
        }
    }

    const ROLES: [Role; 8] = [
        Role::Popover,
        Role::Panel,
        Role::ControlPanel,
        Role::Launcher,
        Role::Spotlight,
        Role::Notification,
        Role::ContextMenu,
        Role::Modal,
    ];

    fn kit_motion() -> motion::Motion {
        let mut motion = CompTheme::default().motion;
        motion.surfaces = kit();
        motion
    }

    #[test]
    fn each_role_moves_on_motion_s_curve() {
        let motion = kit_motion();
        for role in ROLES {
            let Style::Kit(kit) = Style::for_role(role, &motion) else {
                panic!("{role:?} has a motion");
            };
            let (truth, done) = motion_truth(role);
            for (ms, want) in TIMES.into_iter().zip(truth) {
                let got = kit.progress(Duration::from_millis(ms));
                // motion solves a cubic bezier to about 1/4096 of its x; springs agree closer.
                assert!(
                    (got - want).abs() < 5e-4,
                    "{role:?} at {ms}ms: {got} vs {want}"
                );
            }
            if let Some(done) = done {
                assert_eq!(
                    kit.duration,
                    Duration::from_millis(done),
                    "{role:?} settles"
                );
            }
        }
    }

    #[test]
    fn an_entrance_runs_from_the_enter_pose_to_rest() {
        let motion = kit_motion();
        for role in ROLES {
            let m = role.motion(&motion.surfaces).unwrap();
            let (truth, _) = motion_truth(role);
            for (ms, p) in TIMES.into_iter().zip(truth) {
                let open = LayerOpen::styled_backdated(
                    ObjectId::null(),
                    ms,
                    motion,
                    Style::for_role(role, &motion),
                );
                let got = open.pose();
                let want_y = m.enter.y * (1.0 - p);
                let want_scale = m.enter.scale + (1.0 - m.enter.scale) * p;
                // The open's clock has run on a little since it was back-dated.
                assert!(
                    (got.y - want_y).abs() < 0.05,
                    "{role:?} at {ms}ms: y {}",
                    got.y
                );
                assert!(
                    (got.x - m.enter.x * (1.0 - p)).abs() < 0.05,
                    "{role:?} at {ms}ms: x"
                );
                assert!(
                    (got.scale - want_scale).abs() < 1e-3,
                    "{role:?} at {ms}ms: scale"
                );
                assert!(
                    (got.alpha - p.min(1.0)).abs() < 1e-2,
                    "{role:?} at {ms}ms: alpha"
                );
                assert_eq!(
                    open.translate_offset(),
                    (got.x.round() as i32, got.y.round() as i32)
                );
            }
        }
    }

    #[test]
    fn an_exit_leaves_toward_the_exit_pose() {
        let motion = kit_motion();
        let style = Style::for_role(Role::Popover, &motion);
        let close = LayerClose::styled_backdated(ObjectId::null(), 0, motion, style);
        assert!((close.pose().alpha - 1.0).abs() < 1e-2);
        let gone = LayerClose::styled_backdated(ObjectId::null(), 2000, motion, style);
        assert!(!gone.is_animating());
        let at_rest = gone.pose();
        assert!(at_rest.alpha < 1e-3);
        assert!(
            (at_rest.y - 4.0).abs() < 0.05,
            "popovers leave 4px low, not 6"
        );
        assert!((at_rest.scale - 0.97).abs() < 1e-3);
    }

    /// Hiding mid-entrance starts the exit where the entrance had got to, as motion retargets.
    #[test]
    fn a_reversal_starts_from_the_current_pose() {
        let motion = kit_motion();
        let style = Style::for_role(Role::Launcher, &motion);
        let open = LayerOpen::styled_backdated(ObjectId::null(), 40, motion, style);
        let reached = open.pose();
        let close = LayerClose::styled_from(ObjectId::null(), motion, style, reached);
        let start = close.pose();
        assert!((start.y - reached.y).abs() < 0.05 && (start.alpha - reached.alpha).abs() < 1e-2);
        let reopen = LayerOpen::styled_from(ObjectId::null(), motion, style, start);
        assert!((reopen.pose().scale - start.scale).abs() < 1e-3);
    }

    /// With no motion for a role, a surface naming it plays FadeRise, frame for frame.
    #[test]
    fn a_role_the_theme_leaves_alone_is_fade_rise() {
        let motion = CompTheme::default().motion;
        for role in ROLES {
            assert_eq!(role.motion(&motion.surfaces), None);
            assert_eq!(Style::for_role(role, &motion), Style::FadeRise);
        }
        let preset = FadeRise::new(motion);
        for ms in TIMES {
            let open = LayerOpen::styled_backdated(ObjectId::null(), ms, motion, Style::FadeRise);
            let p = (ms as f32 / preset.duration.as_millis() as f32).min(1.0);
            let t = preset.factor(p);
            assert!((open.alpha() - t).abs() < 2e-2, "{ms}ms");
            assert_eq!(
                open.translate_offset().1,
                FadeRise::offset(open.factor()).round() as i32
            );
        }
    }
}
