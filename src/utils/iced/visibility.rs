//! Compositor-side visibility: the UI texture and its backdrop share a frame.

use iced_core::{
    Animation, Vector,
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
}

impl Visibility {
    fn target(self) -> VisibilityFrame {
        if self.visible {
            VisibilityFrame::VISIBLE
        } else {
            VisibilityFrame {
                opacity: 0.0,
                offset: self.hidden_offset,
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(super) struct VisibilityFrame {
    pub opacity: f32,
    pub offset: Vector,
}

impl VisibilityFrame {
    pub const VISIBLE: Self = Self {
        opacity: 1.0,
        offset: Vector::ZERO,
    };

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
}

impl VisibilityAnimation {
    pub fn new(settings: Visibility) -> Self {
        Self {
            settings,
            from: settings.target(),
            clock: Animation::new(true),
        }
    }

    pub fn update(&mut self, settings: Visibility, now: Instant) {
        if settings == self.settings {
            return;
        }
        // Retarget from both currently displayed values, not from the endpoints
        // or from eased absolute visibility (which jumps on asymmetric curves).
        self.from = self.frame(now);
        self.settings = settings;
        self.clock = Animation::new(false)
            .duration(settings.duration)
            .easing(iced_core::animation::Easing::Linear)
            .go(true, now);
    }

    pub fn frame(&self, now: Instant) -> VisibilityFrame {
        let progress = self.clock.interpolate(0.0, 1.0, now);
        let target = self.settings.target();
        let opacity = cubic_bezier_cp(progress, self.settings.opacity_curve);
        let translation = cubic_bezier_cp(progress, self.settings.translation_curve);
        VisibilityFrame {
            opacity: (self.from.opacity + (target.opacity - self.from.opacity) * opacity)
                .clamp(0.0, 1.0),
            offset: self.from.offset + (target.offset - self.from.offset) * translation,
        }
    }

    pub fn is_animating(&self, now: Instant) -> bool {
        self.clock.is_animating(now)
    }
}
