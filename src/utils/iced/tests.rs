use super::*;
use crate::shell::element::header_bar::{header_bar, ssd_header_render_height};
use icetron_themes::{WindowHeaderStyle, dynamic::DEFAULT_THEME_PAIR};

struct Header {
    visible: bool,
}

impl Program for Header {
    type Message = ();

    fn visibility(&self, _: &CompTheme) -> Option<bool> {
        Some(self.visible)
    }

    fn view<'a>(&'a self, theme: &'a CompTheme) -> CompElement<'a, ()> {
        header_bar()
            .theme(theme)
            .title("Explorer")
            .focused(true)
            .on_minimize(())
            .on_maximize(())
            .on_close(())
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
    let duration = theme.motion.animation;
    let element = IcedElement::new(
        Header { visible: false },
        (640, ssd_header_render_height(&theme) as i32),
        event_loop.handle(),
        theme,
    );
    let mut internal = element.0.lock().unwrap();
    let now = IcedInstant::now();
    assert_eq!(internal.frame_alpha(1.0, now), 0.0);
    internal.program.visible = true;
    internal.sync_visibility(now);
    assert_eq!(
        internal.frame_alpha(1.0, now),
        0.0,
        "first reveal frame must not show a full-strength backdrop"
    );
    let half = now + duration / 2;
    assert!((internal.frame_alpha(1.0, half) - 0.5).abs() < 0.001);
    assert!(
        (internal.frame_alpha(0.6, half) - 0.3).abs() < 0.001,
        "window opacity must multiply the shared fade"
    );
    internal.program.visible = false;
    internal.sync_visibility(half);
    assert!(
        (internal.frame_alpha(1.0, half) - 0.5).abs() < 0.001,
        "leaving must not remove the backdrop abruptly"
    );
    assert_eq!(internal.frame_alpha(1.0, half + duration * 2), 0.0);
    assert!(
        !internal
            .visibility
            .as_ref()
            .unwrap()
            .is_animating(half + duration * 2)
    );
}
