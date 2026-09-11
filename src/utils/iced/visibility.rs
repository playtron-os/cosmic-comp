//! Compositor-side visibility: the UI texture and its backdrop share a frame.

use iced_core::{
    Animation, Point as IcedPoint, Rectangle as IcedRectangle, Vector,
    time::{Duration, Instant},
};

use crate::backend::render::animations::motion::cubic_bezier_cp;
use smithay::utils::{Buffer, Logical, Physical, Point, Rectangle, Scale, Size};

/// A program renders its view fully visible and lets the compositor animate it.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Visibility {
    pub visible: bool,
    pub duration: Duration,
    pub opacity_curve: [f32; 4],
    pub translation_curve: [f32; 4],
    pub hidden_offset: Vector,
    pub hidden_scale: f32,
    /// Layer-shell popups animate their first buffer; ordinary window chrome
    /// instead starts in its requested visibility state.
    pub animate_initial: bool,
}

impl Visibility {
    /// Tooltip xdg_popups use the surface preset's opacity, without the
    /// layer-shell card's translation/scale (see Stage::LayerPopup rendering).
    pub fn fade(motion: crate::backend::render::animations::motion::Motion) -> Self {
        Self {
            hidden_offset: Vector::ZERO,
            hidden_scale: 1.0,
            ..Self::fade_rise(motion)
        }
    }

    pub fn fade_rise(motion: crate::backend::render::animations::motion::Motion) -> Self {
        let preset = crate::shell::layer_open::FadeRise::new(motion);
        Self {
            visible: true,
            duration: preset.duration,
            opacity_curve: preset.curve,
            translation_curve: preset.curve,
            hidden_offset: Vector::new(0.0, crate::shell::layer_open::FadeRise::offset(0.0)),
            hidden_scale: crate::shell::layer_open::FadeRise::scale(0.0),
            animate_initial: true,
        }
    }

    fn target(self) -> VisibilityFrame {
        if self.visible {
            VisibilityFrame::VISIBLE
        } else {
            VisibilityFrame {
                opacity: 0.0,
                offset: self.hidden_offset,
                scale: self.hidden_scale,
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(super) struct VisibilityFrame {
    pub opacity: f32,
    pub offset: Vector,
    pub scale: f32,
}

impl VisibilityFrame {
    pub const VISIBLE: Self = Self {
        opacity: 1.0,
        offset: Vector::ZERO,
        scale: 1.0,
    };

    pub fn around(mut self, pivot: IcedPoint) -> Self {
        self.offset += Vector::new(pivot.x, pivot.y) * (1.0 - self.scale);
        self
    }

    pub fn unproject(self, position: IcedPoint) -> IcedPoint {
        let local = position - self.offset;
        IcedPoint::new(local.x / self.scale, local.y / self.scale)
    }

    pub fn bounds(self, bounds: IcedRectangle) -> IcedRectangle {
        IcedRectangle {
            x: bounds.x * self.scale + self.offset.x,
            y: bounds.y * self.scale + self.offset.y,
            width: bounds.width * self.scale,
            height: bounds.height * self.scale,
        }
    }

    pub fn alpha(self, parent: f32) -> f32 {
        parent * self.opacity
    }

    pub fn location(self, origin: Point<i32, Physical>, scale: Scale<f64>) -> Point<f64, Physical> {
        origin.to_f64()
            + Point::from((
                self.offset.x as f64 * scale.x,
                self.offset.y as f64 * scale.y,
            ))
    }

    /// Smithay rounds destination geometry to pixels. Carry the remainder in
    /// the texture source so a 3px slide still moves between pixel boundaries.
    /// Iced's MemoryRenderBuffers have scale 1 and transparent shadow padding.
    pub fn texture_source(
        location: Point<f64, Physical>,
        size: Size<i32, Buffer>,
    ) -> Rectangle<f64, Logical> {
        let rounded = location.to_i32_round::<i32>().to_f64();
        Rectangle::new(
            (rounded.x - location.x, rounded.y - location.y).into(),
            (size.w as f64, size.h as f64).into(),
        )
    }
}

#[derive(Clone, Debug)]
pub(super) struct VisibilityAnimation {
    settings: Visibility,
    from: VisibilityFrame,
    clock: Animation<bool>,
    pending: bool,
}

impl VisibilityAnimation {
    pub fn new(settings: Visibility) -> Self {
        let pending = settings.animate_initial && settings.visible && !settings.duration.is_zero();
        Self {
            settings,
            from: if pending {
                Visibility {
                    visible: false,
                    ..settings
                }
                .target()
            } else {
                settings.target()
            },
            clock: Animation::new(!pending),
            pending,
        }
    }

    /// Start only when a compositor draw is ready, not during the menu's
    /// measuring/layout passes, which would consume the opening offscreen.
    pub fn start_on_draw(&mut self, now: Instant) -> bool {
        if !self.pending {
            return false;
        }
        self.pending = false;
        self.clock = Animation::new(false)
            .duration(self.settings.duration)
            .easing(iced_core::animation::Easing::Linear)
            .go(true, now);
        true
    }

    pub fn update(&mut self, settings: Visibility, now: Instant) {
        if settings == self.settings {
            return;
        }
        if self.pending {
            // Theme/settings changes during measurement still precede the
            // first buffer; they must not start a clock offscreen either.
            *self = Self::new(settings);
            return;
        }
        // Surface popups use the same point-symmetric fade-rise curve as
        // LayerClose. Reverse the entrance's timeline, not a fresh full-length
        // interpolation from a partly visible frame. This matches the
        // protocol's backdated close when dismissed halfway through opening.
        let was_hidden = Visibility {
            visible: false,
            ..self.settings
        }
        .target();
        let reverse_open = self.settings.animate_initial
            && self.settings.visible
            && !settings.visible
            && self.settings
                == Visibility {
                    visible: true,
                    ..settings
                }
            && self.from == was_hidden
            && [settings.opacity_curve, settings.translation_curve]
                .iter()
                .all(|c| {
                    (c[0] + c[2] - 1.0).abs() < 0.00001 && (c[1] + c[3] - 1.0).abs() < 0.00001
                });
        if reverse_open {
            let opened = self.clock.interpolate(0.0, 1.0, now).clamp(0.0, 1.0);
            let start = now
                .checked_sub(settings.duration.mul_f32(1.0 - opened))
                .unwrap_or(now);
            self.from = VisibilityFrame::VISIBLE;
            self.settings = settings;
            self.clock = Animation::new(false)
                .duration(settings.duration)
                .easing(iced_core::animation::Easing::Linear)
                .go(true, start);
            return;
        }
        // Retarget from both currently displayed values, not from the endpoints
        // or from eased absolute visibility (which jumps on asymmetric curves).
        self.from = self.frame(now);
        self.settings = settings;
        self.pending = false;
        self.clock = Animation::new(false)
            .duration(settings.duration)
            .easing(iced_core::animation::Easing::Linear)
            .go(true, now);
    }

    pub fn frame(&self, now: Instant) -> VisibilityFrame {
        if self.pending {
            return self.from;
        }
        let progress = self.clock.interpolate(0.0, 1.0, now);
        let target = self.settings.target();
        let opacity = cubic_bezier_cp(progress, self.settings.opacity_curve);
        let translation = cubic_bezier_cp(progress, self.settings.translation_curve);
        VisibilityFrame {
            opacity: (self.from.opacity + (target.opacity - self.from.opacity) * opacity)
                .clamp(0.0, 1.0),
            offset: self.from.offset + (target.offset - self.from.offset) * translation,
            scale: self.from.scale + (target.scale - self.from.scale) * translation,
        }
    }

    pub fn is_animating(&self, now: Instant) -> bool {
        self.pending || self.clock.is_animating(now)
    }

    pub fn is_fully_hidden(&self, now: Instant) -> bool {
        !self.settings.visible && !self.is_animating(now) && self.frame(now).opacity <= 0.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{comp_theme::CompTheme, shell::layer_open::FadeRise};

    #[test]
    fn popup_open_uses_the_protocol_preset_and_waits_for_its_first_draw() {
        let motion = CompTheme::default().motion;
        let settings = Visibility::fade_rise(motion);
        let preset = FadeRise::new(motion);
        let mut animation = VisibilityAnimation::new(settings);
        let now = Instant::now();
        animation.update(settings, now + Duration::from_secs(1));
        let first = now + Duration::from_secs(2);
        assert_eq!(animation.frame(first).opacity, 0.0);
        assert!(animation.start_on_draw(first));
        for (elapsed, fraction) in [
            (Duration::ZERO, 0.0),
            (settings.duration / 4, 0.25),
            (settings.duration / 2, 0.5),
            (settings.duration, 1.0),
        ] {
            let frame = animation.frame(first + elapsed);
            let factor = preset.factor(fraction);
            assert!((frame.opacity - factor).abs() < 0.00001);
            assert!((frame.scale - FadeRise::scale(factor)).abs() < 0.00001);
            assert!((frame.offset.y - FadeRise::offset(factor)).abs() < 0.00001);
            assert!(
                !animation.start_on_draw(first + elapsed),
                "subsequent draws must not restart it"
            );
        }
        assert_eq!(
            animation.frame(first + settings.duration),
            VisibilityFrame::VISIBLE
        );
        assert!(!animation.is_animating(first + settings.duration));
    }

    #[test]
    fn scaled_popup_bounds_and_input_share_the_centered_transform() {
        let motion = CompTheme::default().motion;
        let settings = Visibility::fade_rise(motion);
        let mut animation = VisibilityAnimation::new(settings);
        let now = Instant::now();
        animation.start_on_draw(now);
        let surface_center = IcedPoint::new(210.0, 145.0);
        let body = IcedRectangle {
            x: 24.0,
            y: 16.0,
            width: 360.0,
            height: 200.0,
        };
        for elapsed in [Duration::ZERO, settings.duration / 2, settings.duration] {
            let raw = animation.frame(now + elapsed);
            let frame = raw.around(surface_center);
            let painted = frame.bounds(body);
            let center = frame.bounds(IcedRectangle::new(surface_center, iced_core::Size::ZERO));
            assert!((center.x - surface_center.x).abs() < 0.0001);
            assert!((center.y - surface_center.y - raw.offset.y).abs() < 0.0001);
            let local = frame.unproject(painted.center());
            assert!((local.x - body.center_x()).abs() < 0.0001);
            assert!((local.y - body.center_y()).abs() < 0.0001);
        }
        let half = now + settings.duration / 2;
        let before = animation.frame(half);
        animation.update(
            Visibility {
                visible: false,
                ..settings
            },
            half,
        );
        let after = animation.frame(half);
        assert!((after.opacity - before.opacity).abs() < 0.00001);
        assert!((after.offset.y - before.offset.y).abs() < 0.00001);
        assert!((after.scale - before.scale).abs() < 0.00001);
    }

    #[test]
    fn zero_duration_popup_is_immediately_visible() {
        let mut motion = CompTheme::default().motion;
        motion.layer_open = Duration::ZERO;
        let mut animation = VisibilityAnimation::new(Visibility::fade_rise(motion));
        let now = Instant::now();
        assert_eq!(animation.frame(now), VisibilityFrame::VISIBLE);
        assert!(!animation.start_on_draw(now));
        assert!(!animation.is_animating(now));
    }

    #[test]
    fn changing_pending_popup_motion_does_not_start_it_offscreen() {
        let mut settings = Visibility::fade_rise(CompTheme::default().motion);
        let mut animation = VisibilityAnimation::new(settings);
        let now = Instant::now();
        settings.duration *= 2;
        animation.update(settings, now);
        assert_eq!(animation.frame(now + settings.duration * 2).opacity, 0.0);
        assert!(animation.start_on_draw(now + settings.duration * 2));
        assert_eq!(animation.frame(now + settings.duration * 2).opacity, 0.0);
    }

    #[test]
    fn popup_close_retraces_open_and_finishes_without_another_draw() {
        let settings = Visibility::fade_rise(CompTheme::default().motion);
        let now = Instant::now();
        for fraction in [0.25, 0.5, 1.0] {
            let mut animation = VisibilityAnimation::new(settings);
            animation.start_on_draw(now);
            let shown_for = settings.duration.mul_f32(fraction);
            let dismissed = now + shown_for;
            let before = animation.frame(dismissed);
            animation.update(
                Visibility {
                    visible: false,
                    ..settings
                },
                dismissed,
            );
            let after = animation.frame(dismissed);
            assert!((after.opacity - before.opacity).abs() < 0.00001);
            assert!((after.offset.y - before.offset.y).abs() < 0.00001);
            assert!((after.scale - before.scale).abs() < 0.00001);
            let halfway = animation.frame(dismissed + shown_for / 2);
            assert!(halfway.opacity < after.opacity && halfway.scale < after.scale);
            assert!(halfway.offset.y > after.offset.y);
            assert!(!animation.is_fully_hidden(dismissed + shown_for / 2));
            assert!(animation.is_fully_hidden(dismissed + shown_for + Duration::from_millis(1)));
        }
        let mut unseen = VisibilityAnimation::new(settings);
        unseen.update(
            Visibility {
                visible: false,
                ..settings
            },
            now,
        );
        assert!(
            unseen.is_fully_hidden(now),
            "an unshown menu must not flash on dismissal"
        );
        let mut disabled = VisibilityAnimation::new(Visibility {
            duration: Duration::ZERO,
            ..settings
        });
        disabled.update(
            Visibility {
                visible: false,
                duration: Duration::ZERO,
                ..settings
            },
            now,
        );
        assert!(disabled.is_fully_hidden(now));
    }
}
