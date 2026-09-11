//! A dot that breathes: the halo's mark for a command that is on, such as a
//! window being recorded. Its rhythm is the prototype's `kora-halo-pulse`.
//!
//! The widget drives itself: every redraw tick it stores the time and asks for
//! the next frame, so it only costs frames while it is on screen.

use std::time::Duration;

use iced_core::{
    Background, Border, Color, Element, Event, Layout, Length, Rectangle, Renderer as _, Shadow,
    Shell, Size, Vector, layout, mouse,
    renderer::{Quad, Style},
    time::Instant,
    widget::{Tree, Widget, tree},
    window,
};

use crate::backend::render::animations::motion::cubic_bezier_cp;

/// One breath, from the prototype's `kora-halo-pulse 1.6s`.
pub const PERIOD: Duration = Duration::from_millis(1600);
/// The dimmest the dot gets, halfway through a breath.
pub const FLOOR: f32 = 0.35;
/// Frames the breath is drawn at; smooth for a 1.6s cycle without a full
/// refresh-rate repaint of the halo.
const FRAME: Duration = Duration::from_millis(33);
/// The glow is the prototype's 50% shadow of the dot's own colour.
const GLOW_ALPHA: f32 = 0.5;

/// Opacity `elapsed` into the cycle: 1 down to `floor` and back, each half on
/// `easing`, the way CSS applies a timing function per keyframe segment.
pub fn breath(elapsed: Duration, period: Duration, floor: f32, easing: [f32; 4]) -> f32 {
    let period = period.as_secs_f32().max(f32::EPSILON);
    let phase = (elapsed.as_secs_f32() % period) / period;
    let (from, to, t) = if phase < 0.5 {
        (1.0, floor, phase * 2.0)
    } else {
        (floor, 1.0, (phase - 0.5) * 2.0)
    };
    from + (to - from) * cubic_bezier_cp(t, easing)
}

pub struct PulsingDot {
    diameter: f32,
    color: Color,
    glow: f32,
    easing: [f32; 4],
}

#[derive(Default)]
struct State {
    epoch: Option<Instant>,
    now: Option<Instant>,
}

impl PulsingDot {
    /// A dot `diameter` wide in `color`, glowing `glow` px around it.
    pub fn new(diameter: f32, color: Color, glow: f32, easing: [f32; 4]) -> Self {
        Self {
            diameter,
            color,
            glow,
            easing,
        }
    }

    fn opacity(&self, state: &State) -> f32 {
        match (state.epoch, state.now) {
            (Some(epoch), Some(now)) => breath(now - epoch, PERIOD, FLOOR, self.easing),
            _ => 1.0,
        }
    }
}

impl<Message> Widget<Message, iced_core::Theme, iced_tiny_skia::Renderer> for PulsingDot {
    fn size(&self) -> Size<Length> {
        Size::new(Length::Fixed(self.diameter), Length::Fixed(self.diameter))
    }

    fn layout(
        &mut self,
        _tree: &mut Tree,
        _renderer: &iced_tiny_skia::Renderer,
        _limits: &layout::Limits,
    ) -> layout::Node {
        layout::Node::new(Size::new(self.diameter, self.diameter))
    }

    fn tag(&self) -> tree::Tag {
        tree::Tag::of::<State>()
    }

    fn state(&self) -> tree::State {
        tree::State::new(State::default())
    }

    fn update(
        &mut self,
        tree: &mut Tree,
        event: &Event,
        _layout: Layout<'_>,
        _cursor: mouse::Cursor,
        _renderer: &iced_tiny_skia::Renderer,
        shell: &mut Shell<'_, Message>,
        _viewport: &Rectangle,
    ) {
        if let Event::Window(window::Event::RedrawRequested(now)) = event {
            let state = tree.state.downcast_mut::<State>();
            state.epoch.get_or_insert(*now);
            state.now = Some(*now);
            shell.request_redraw_at(*now + FRAME);
        }
    }

    fn draw(
        &self,
        tree: &Tree,
        renderer: &mut iced_tiny_skia::Renderer,
        _theme: &iced_core::Theme,
        _style: &Style,
        layout: Layout<'_>,
        _cursor: mouse::Cursor,
        _viewport: &Rectangle,
    ) {
        let opacity = self.opacity(tree.state.downcast_ref::<State>());
        let bounds = layout.bounds();
        let color = Color {
            a: self.color.a * opacity,
            ..self.color
        };
        renderer.fill_quad(
            Quad {
                bounds,
                border: Border::default().rounded(bounds.width / 2.0),
                shadow: Shadow {
                    color: Color {
                        a: color.a * GLOW_ALPHA,
                        ..color
                    },
                    offset: Vector::ZERO,
                    blur_radius: self.glow,
                    ..Default::default()
                },
                ..Default::default()
            },
            Background::Color(color),
        );
    }
}

impl<'a, Message: 'a> From<PulsingDot>
    for Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer>
{
    fn from(dot: PulsingDot) -> Self {
        Element::new(dot)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const EASE: [f32; 4] = [0.22, 1.0, 0.36, 1.0];

    #[test]
    fn a_breath_dims_to_the_floor_and_recovers() {
        let at = |ms: u64| breath(Duration::from_millis(ms), PERIOD, FLOOR, EASE);
        assert!((at(0) - 1.0).abs() < 1e-5);
        assert!((at(800) - FLOOR).abs() < 1e-3);
        assert!((at(1600) - 1.0).abs() < 1e-3);
        // The second cycle repeats the first.
        assert!((at(2400) - FLOOR).abs() < 1e-3);
    }

    #[test]
    fn the_first_half_only_dims_and_the_second_only_brightens() {
        let at = |ms: u64| breath(Duration::from_millis(ms), PERIOD, FLOOR, EASE);
        for ms in (0..800).step_by(50) {
            assert!(at(ms + 50) <= at(ms) + 1e-4, "dims at {ms}");
        }
        for ms in (800..1600).step_by(50) {
            assert!(at(ms + 50) >= at(ms) - 1e-4, "brightens at {ms}");
        }
    }

    #[test]
    fn a_zero_period_does_not_divide_by_zero() {
        assert!(breath(Duration::from_millis(5), Duration::ZERO, FLOOR, EASE).is_finite());
    }
}
