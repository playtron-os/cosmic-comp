use super::*;
use crate::shell::element::header_bar::{halo_visibility, header_bar, ssd_header_render_height};
use icetron_themes::{WindowHeaderStyle, dynamic::DEFAULT_THEME_PAIR};

struct Header {
    visible: bool,
}

impl Program for Header {
    type Message = ();

    fn visibility(&self, theme: &CompTheme) -> Option<Visibility> {
        (theme.window_header_style() == WindowHeaderStyle::Halo)
            .then(|| halo_visibility(theme, self.visible))
    }

    fn view<'a>(&'a self, theme: &'a CompTheme) -> CompElement<'a, ()> {
        header_bar()
            .theme(theme)
            .title("Explorer")
            .focused(true)
            .on_minimize(())
            .on_maximize(())
            .on_close(())
            .on_right_click(())
            .on_new_window(())
            .on_screenshot(())
            .on_fullscreen((), false)
            .into_element()
    }
}

fn theme() -> CompTheme {
    let mut theme = DEFAULT_THEME_PAIR.load(true);
    theme.window_header_style = WindowHeaderStyle::Halo;
    // Isolate status propagation from interpolation in the input regression.
    theme.duration_fast = 0.0;
    CompTheme::new(Arc::new(theme), true)
}

#[test]
fn fullscreen_halo_reveal_animates_inside_the_screen_and_hides_after_leave() {
    use crate::shell::element::header_bar::{fullscreen_header_offset, halo_is_visible};
    struct FullscreenHeader {
        hovered: bool,
        menu_open: bool,
    }
    impl Program for FullscreenHeader {
        type Message = ();
        fn backdrop_blur(
            &self,
            theme: &CompTheme,
            size: Size<i32, Logical>,
            layers: &[Layer],
            _: [u8; 4],
        ) -> Option<(iced_core::Rectangle, [u8; 4])> {
            crate::shell::element::window::halo_backdrop_blur(
                layers,
                theme.halo_style().pill_height(),
                size.w as f32,
            )
        }
        fn visibility(&self, theme: &CompTheme) -> Option<Visibility> {
            Some(halo_visibility(
                theme,
                halo_is_visible(true, self.hovered, true, self.menu_open),
            ))
        }
        fn view<'a>(&'a self, theme: &'a CompTheme) -> CompElement<'a, ()> {
            header_bar()
                .theme(theme)
                .title("Fullscreen app")
                .focused(true)
                .on_close(())
                .on_fullscreen((), true)
                .on_right_click(())
                .into_element()
        }
    }
    let event_loop = calloop::EventLoop::<crate::state::State>::try_new().unwrap();
    let theme = theme();
    let element = IcedElement::new(
        FullscreenHeader {
            hovered: false,
            menu_open: false,
        },
        (900, ssd_header_render_height(&theme) as i32),
        event_loop.handle(),
        theme.clone(),
    );
    let body = element.backdrop_bounds().unwrap();
    let origin_y = fullscreen_header_offset(&theme) as f32;
    assert!(
        (origin_y + body.y - halo_visibility(&theme, false).hidden_offset.y - 10.0).abs() < 1.0,
        "the pill must sit inside the fullscreen output"
    );
    assert!(
        body.x > 0.0 && body.x + body.width < 900.0,
        "the pill is centered on the output, not the client buffer"
    );
    let mut internal = element.0.lock().unwrap();
    assert_eq!(
        internal.visibility_frame.opacity, 0.0,
        "focus must not show fullscreen chrome"
    );
    let now = IcedInstant::now();
    let duration = halo_visibility(&theme, true).duration;
    internal.program.hovered = true;
    internal.sync_visibility(now);
    assert_eq!(internal.visibility_frame.opacity, 0.0);
    internal.sync_visibility(now + duration / 2);
    assert!(internal.visibility_frame.opacity > 0.0 && internal.visibility_frame.opacity < 1.0);
    internal.sync_visibility(now + duration);
    assert_eq!(internal.visibility_frame, VisibilityFrame::VISIBLE);
    internal.program.hovered = false;
    internal.program.menu_open = true;
    internal.sync_visibility(now + duration * 2);
    assert_eq!(internal.visibility_frame, VisibilityFrame::VISIBLE);
    internal.program.menu_open = false;
    internal.sync_visibility(now + duration * 3);
    internal.sync_visibility(now + duration * 4);
    assert_eq!(internal.visibility_frame.opacity, 0.0);
}

#[test]
fn header_hover_paints_on_entry_and_clears_on_exit() {
    let event_loop = calloop::EventLoop::<crate::state::State>::try_new().unwrap();
    let theme = theme();
    let element = IcedElement::new(
        Header { visible: true },
        (640, ssd_header_render_height(&theme) as i32),
        event_loop.handle(),
        theme.clone(),
    );
    let mut internal = element.0.lock().unwrap();
    let height = theme.halo_style().pill_height();
    let pill = internal
        .renderer
        .layers()
        .iter()
        .flat_map(|layer| &layer.quads)
        .find(|(quad, _)| (quad.bounds.height - height).abs() < 1.0 && quad.bounds.width > 100.0)
        .unwrap()
        .0
        .bounds;
    let close = IcedPoint::new(
        pill.x + pill.width
            - theme.halo_style().padding_horizontal
            - theme.halo_style().control_size / 2.0,
        pill.y + pill.height / 2.0,
    );
    let has_close_fill = |internal: &mut IcedElementInternal<Header>| {
        internal
            .renderer
            .layers()
            .iter()
            .flat_map(|layer| &layer.quads)
            .any(|(_, fill)| *fill == iced_core::Background::Color(theme.feedback_error_primary()))
    };
    assert!(!has_close_fill(&mut internal));
    internal.cursor_pos = Some((close.x as f64, close.y as f64).into());
    internal
        .event_queue
        .push(Event::Mouse(MouseEvent::CursorMoved { position: close }));
    internal.update(UpdateSource::Input);
    assert!(
        has_close_fill(&mut internal),
        "entering close must paint its hovered background on this frame"
    );
    internal
        .event_queue
        .push(Event::Mouse(MouseEvent::ButtonPressed(
            iced_core::mouse::Button::Left,
        )));
    internal
        .event_queue
        .push(Event::Mouse(MouseEvent::ButtonReleased(
            iced_core::mouse::Button::Left,
        )));
    internal.update(UpdateSource::Input);
    assert!(
        has_close_fill(&mut internal),
        "the message-driven rebuild after a click must retain hover paint"
    );
    internal.cursor_pos = None;
    internal
        .event_queue
        .push(Event::Mouse(MouseEvent::CursorLeft));
    internal.update(UpdateSource::Input);
    assert!(
        !has_close_fill(&mut internal),
        "leaving close must not flash the previous hover fill"
    );
}

fn output(name: &str) -> Output {
    Output::new(
        name.into(),
        smithay::output::PhysicalProperties {
            size: (0, 0).into(),
            subpixel: smithay::output::Subpixel::Unknown,
            make: "Test".into(),
            model: "Test".into(),
            serial_number: String::new(),
        },
    )
}

#[test]
fn halo_tooltip_delays_slides_fades_and_dismisses_without_input_locks() {
    use icetron_p::utils::platform::test_clock;
    use std::time::Duration;
    let _clock = test_clock::Frozen::start();
    let at = icetron_p::utils::platform::now();
    let event_loop = calloop::EventLoop::<crate::state::State>::try_new().unwrap();
    let mut tokens = icetron_themes::dynamic::DynamicTheme::from_theme(&*theme());
    tokens.duration_fast = 120.0;
    let theme = CompTheme::new(Arc::new(tokens), true);
    let element = IcedElement::new(
        Header { visible: true },
        (640, ssd_header_render_height(&theme) as i32),
        event_loop.handle(),
        theme.clone(),
    );
    let mut internal = element.0.lock().unwrap();
    let pill = internal
        .renderer
        .layers()
        .iter()
        .flat_map(|layer| &layer.quads)
        .find(|(quad, _)| {
            (quad.bounds.height - theme.halo_style().pill_height()).abs() < 1.0
                && quad.bounds.width > 100.0
        })
        .unwrap()
        .0
        .bounds;
    let close = IcedPoint::new(
        pill.x + pill.width
            - theme.halo_style().padding_horizontal
            - theme.halo_style().control_size / 2.0,
        pill.center_y(),
    );
    internal.cursor_pos = Some((close.x as f64, close.y as f64).into());
    let step = |internal: &mut IcedElementInternal<Header>, ms| {
        let now = at + Duration::from_millis(ms);
        test_clock::set(now);
        internal
            .event_queue
            .push(Event::Window(WindowEvent::RedrawRequested(now)));
        internal.update(UpdateSource::AnimRedraw);
    };
    step(&mut internal, 0);
    step(&mut internal, 399);
    assert!(internal.tooltip.report.is_none());
    step(&mut internal, 400);
    let first = internal
        .tooltip
        .report
        .clone()
        .expect("tooltip must open at the deadline");
    assert_eq!(first.label, "Close");
    assert_eq!(first.opacity, 0.0);
    assert!(
        first.bounds.y > pill.y,
        "operation must report laid-out, not zero, coordinates"
    );
    step(&mut internal, 460);
    let middle = internal.tooltip.report.clone().unwrap();
    assert!(
        (middle.opacity - 0.685).abs() < 0.01,
        "CSS ease-out is not a linear fade"
    );
    step(&mut internal, 520);
    let last = internal.tooltip.report.clone().unwrap();
    assert_eq!(last.opacity, 1.0);
    assert!((last.bounds.y - first.bounds.y - 3.0).abs() < 0.01);
    assert!(last.bounds.y > middle.bounds.y);
    // Native Iced button transitions still read wall time. Stop those independently
    // before checking that the tooltip itself has no remaining frame requests.
    let mut tokens = icetron_themes::dynamic::DynamicTheme::from_theme(&*internal.theme);
    tokens.duration_fast = 0.0;
    internal.theme = CompTheme::new(Arc::new(tokens), true);
    step(&mut internal, 536);
    assert!(
        !internal.needs_redraw,
        "settled tooltips must stop requesting frames"
    );
    internal
        .event_queue
        .push(Event::Mouse(MouseEvent::ButtonPressed(MouseButton::Left)));
    step(&mut internal, 540);
    assert!(internal.tooltip.report.is_none());
    step(&mut internal, 1000);
    assert!(
        internal.tooltip.report.is_none(),
        "click suppression lasts until leave"
    );
    internal.cursor_pos = None;
    step(&mut internal, 1010);
    let mut tokens = icetron_themes::dynamic::DynamicTheme::from_theme(&*internal.theme);
    tokens.duration_fast = 120.0;
    internal.theme = CompTheme::new(Arc::new(tokens), true);
    internal.cursor_pos = Some((close.x as f64, close.y as f64).into());
    step(&mut internal, 1020);
    step(&mut internal, 1419);
    assert!(
        internal.tooltip.report.is_none(),
        "re-entry must restart the delay"
    );
    step(&mut internal, 1420);
    assert_eq!(internal.tooltip.report.as_ref().unwrap().opacity, 0.0);
}

#[test]
fn header_transition_requests_frames_with_a_stationary_pointer_then_stops() {
    let event_loop = calloop::EventLoop::<crate::state::State>::try_new().unwrap();
    let mut theme = theme();
    let mut tokens = icetron_themes::dynamic::DynamicTheme::from_theme(&*theme);
    tokens.duration_fast = 60_000.0; // Keep the transition in flight without sleeping.
    theme = CompTheme::new(Arc::new(tokens), true);
    let element = IcedElement::new(
        Header { visible: true },
        (640, ssd_header_render_height(&theme) as i32),
        event_loop.handle(),
        theme.clone(),
    );
    let active_output = output("header");
    let other_output = output("idle");
    let mut internal = element.0.lock().unwrap();
    internal.outputs.insert(active_output.clone());
    let height = theme.halo_style().pill_height();
    let pill = internal
        .renderer
        .layers()
        .iter()
        .flat_map(|layer| &layer.quads)
        .find(|(quad, _)| (quad.bounds.height - height).abs() < 1.0 && quad.bounds.width > 100.0)
        .unwrap()
        .0
        .bounds;
    let close = IcedPoint::new(
        pill.x + pill.width
            - theme.halo_style().padding_horizontal
            - theme.halo_style().control_size / 2.0,
        pill.y + pill.height / 2.0,
    );
    internal.cursor_pos = Some((close.x as f64, close.y as f64).into());
    internal
        .event_queue
        .push(Event::Mouse(MouseEvent::CursorMoved { position: close }));
    internal.update(UpdateSource::Input);
    assert!(take_redraw_request(&active_output));
    assert!(!take_redraw_request(&other_output));
    for _ in 0..3 {
        internal
            .event_queue
            .push(Event::Window(WindowEvent::RedrawRequested(
                IcedInstant::now(),
            )));
        internal.update(UpdateSource::AnimRedraw);
        assert!(internal.needs_redraw);
        assert!(
            take_redraw_request(&active_output),
            "animation must request its next output frame without input"
        );
        assert!(
            !take_redraw_request(&active_output),
            "request must be consumed once"
        );
    }
    let mut tokens = icetron_themes::dynamic::DynamicTheme::from_theme(&*theme);
    tokens.duration_fast = 0.0;
    internal.theme = CompTheme::new(Arc::new(tokens), true);
    // This test concerns button transitions; leaving also cancels the new tooltip delay.
    internal.cursor_pos = None;
    internal.update(UpdateSource::Forced);
    assert!(!internal.needs_redraw);
    assert!(
        !take_redraw_request(&active_output),
        "settled widgets must let the output idle"
    );
}

#[test]
fn header_and_backdrop_share_one_fade_including_reversal() {
    let event_loop = calloop::EventLoop::<crate::state::State>::try_new().unwrap();
    let theme = theme();
    let settings = halo_visibility(&theme, true);
    let duration = settings.duration;
    assert_eq!(duration, std::time::Duration::from_millis(200));
    let element = IcedElement::new(
        Header { visible: false },
        (640, ssd_header_render_height(&theme) as i32),
        event_loop.handle(),
        theme,
    );
    let mut internal = element.0.lock().unwrap();
    let now = IcedInstant::now();
    assert_eq!(internal.visibility_frame.alpha(1.0), 0.0);
    assert_eq!(internal.visibility_frame.offset.y, 3.0);
    internal.program.visible = true;
    internal.sync_visibility(now);
    assert_eq!(
        internal.visibility_frame.alpha(1.0),
        0.0,
        "first reveal frame must not show a full-strength backdrop"
    );
    internal.sync_visibility(now + duration / 4);
    // Quarter-time also detects accidentally easing the clock before applying
    // the CSS curves (both clocks happen to agree at half-time).
    assert!((internal.visibility_frame.opacity - 0.23658736).abs() < 0.001);
    assert!((internal.visibility_frame.offset.y - 0.5231331).abs() < 0.001);
    let half = now + duration / 2;
    internal.sync_visibility(half);
    let frame = internal.visibility_frame;
    // CSS cubic-bezier(0.4, 0, 0.2, 1) at t=0.5, not a linear fade.
    assert!((frame.opacity - 0.7755613).abs() < 0.001);
    // The non-overshooting slide has almost settled halfway through the fade.
    assert!(frame.offset.y > 0.0 && frame.offset.y < 0.1);
    assert!(
        (frame.alpha(0.6) - 0.4653368).abs() < 0.001,
        "window opacity must multiply the shared fade"
    );
    internal.program.visible = false;
    internal.sync_visibility(half);
    assert_eq!(
        internal.visibility_frame, frame,
        "leaving must not remove the backdrop abruptly"
    );
    internal.sync_visibility(half + duration * 2);
    assert_eq!(internal.visibility_frame.alpha(1.0), 0.0);
    assert_eq!(internal.visibility_frame.offset.y, 3.0);
    assert!(
        !internal
            .visibility
            .as_ref()
            .unwrap()
            .is_animating(half + duration * 2)
    );
}

#[test]
fn halo_slide_keeps_fractional_placement_after_smithay_rounds_geometry() {
    let frame = VisibilityFrame {
        opacity: 0.5,
        offset: iced_core::Vector::new(0.0, 0.375),
    };
    for scale in [1.0, 1.25, 1.5, 2.0] {
        let origin = Point::<i32, Physical>::from((100, 200));
        let location = frame.location(origin, scale.into());
        assert_eq!(location.x, 100.0);
        assert_eq!(location.y, 200.0 + 0.375 * scale);
        assert_ne!(
            location.y.fract(),
            0.0,
            "don't quantize a 3px slide into three jumps"
        );
        let source = VisibilityFrame::texture_source(location, (640, 180).into());
        let destination = location.to_i32_round::<i32>();
        assert_eq!(destination.y as f64 - source.loc.y, location.y);
    }
}

#[test]
fn halo_motion_uses_theme_duration_and_slide_curve_and_stops_at_rest() {
    let mut tokens = DEFAULT_THEME_PAIR.load(true);
    tokens.animation_transition_duration_fade_default = 400.0;
    tokens.ease_out_expo = [0.0, 0.0, 1.0, 1.0];
    let theme = CompTheme::new(Arc::new(tokens), true);
    let now = IcedInstant::now();
    let mut animation = VisibilityAnimation::new(halo_visibility(&theme, false));
    animation.update(halo_visibility(&theme, true), now);
    let half = now + std::time::Duration::from_millis(200);
    assert!((animation.frame(half).offset.y - 1.5).abs() < 0.001);
    // Updating an unchanged visibility must not restart its timeline.
    animation.update(halo_visibility(&theme, true), half);
    let end = now + std::time::Duration::from_millis(400);
    assert_eq!(animation.frame(end), VisibilityFrame::VISIBLE);
    assert!(!animation.is_animating(end));
    let mut settings = halo_visibility(&theme, false);
    settings.duration = std::time::Duration::ZERO;
    animation.update(settings, end);
    assert_eq!(animation.frame(end).opacity, 0.0);
    assert_eq!(animation.frame(end).offset.y, 3.0);
    assert!(!animation.is_animating(end));
}

#[test]
fn halo_exit_uses_the_css_curve_in_the_forward_direction() {
    let theme = theme();
    let now = IcedInstant::now();
    let mut animation = VisibilityAnimation::new(halo_visibility(&theme, true));
    let settings = halo_visibility(&theme, false);
    animation.update(settings, now);
    let half = animation.frame(now + settings.duration / 2);
    assert!((half.opacity - 0.2244387).abs() < 0.001);
    assert!(half.offset.y > 2.9 && half.offset.y < 3.0);
    // An early re-entry must preserve both values, despite their different curves.
    animation.update(halo_visibility(&theme, true), now + settings.duration / 2);
    assert_eq!(animation.frame(now + settings.duration / 2), half);
    for step in 1..=20 {
        let frame = animation.frame(now + settings.duration / 2 + settings.duration * step / 20);
        assert!(frame.opacity >= half.opacity && frame.opacity <= 1.0);
        assert!(frame.offset.y >= 0.0 && frame.offset.y <= half.offset.y);
    }
}

#[test]
fn conventional_header_does_not_slide_or_fade() {
    let event_loop = calloop::EventLoop::<crate::state::State>::try_new().unwrap();
    let mut tokens = DEFAULT_THEME_PAIR.load(true);
    tokens.window_header_style = WindowHeaderStyle::Bar;
    let theme = CompTheme::new(Arc::new(tokens), true);
    let element = IcedElement::new(
        Header { visible: false },
        (640, ssd_header_render_height(&theme) as i32),
        event_loop.handle(),
        theme,
    );
    let internal = element.0.lock().unwrap();
    assert!(internal.visibility.is_none());
    assert_eq!(internal.visibility_frame, VisibilityFrame::VISIBLE);
}

#[test]
fn halo_buttons_follow_the_slide_under_a_stationary_pointer() {
    let event_loop = calloop::EventLoop::<crate::state::State>::try_new().unwrap();
    let theme = theme();
    let element = IcedElement::new(
        Header { visible: false },
        (640, ssd_header_render_height(&theme) as i32),
        event_loop.handle(),
        theme.clone(),
    );
    let mut internal = element.0.lock().unwrap();
    let pill = internal
        .renderer
        .layers()
        .iter()
        .flat_map(|layer| &layer.quads)
        .find(|(quad, _)| {
            (quad.bounds.height - theme.halo_style().pill_height()).abs() < 1.0
                && quad.bounds.width > 100.0
        })
        .unwrap()
        .0
        .bounds;
    let close = IcedPoint::new(
        pill.x + pill.width
            - theme.halo_style().padding_horizontal
            - theme.halo_style().control_size / 2.0,
        pill.y + pill.height / 2.0,
    );
    // This stationary point is inside the button while lowered, but outside its
    // final bounds. The hover must update as the button moves, without input.
    let point = close + iced_core::Vector::new(0.0, theme.halo_style().control_size / 2.0 + 1.0);
    internal.cursor_pos = Some((point.x as f64, point.y as f64).into());
    internal.program.visible = true;
    let now = IcedInstant::now();
    internal.sync_visibility(now);
    let has_close_fill = |internal: &mut IcedElementInternal<Header>| {
        internal
            .renderer
            .layers()
            .iter()
            .flat_map(|layer| &layer.quads)
            .any(|(_, fill)| *fill == iced_core::Background::Color(theme.feedback_error_primary()))
    };
    internal
        .event_queue
        .push(Event::Window(WindowEvent::RedrawRequested(
            now + std::time::Duration::from_millis(1),
        )));
    internal.update(UpdateSource::AnimRedraw);
    assert!(has_close_fill(&mut internal));
    internal
        .event_queue
        .push(Event::Window(WindowEvent::RedrawRequested(
            now + halo_visibility(&theme, true).duration,
        )));
    internal.update(UpdateSource::AnimRedraw);
    assert!(!has_close_fill(&mut internal));
    assert_eq!(internal.local_position(close), close);
}
