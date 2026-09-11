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
fn focus_outline_uses_one_redraw_clock_and_live_halo_geometry() {
    struct FocusHeader {
        focused: bool,
        title: String,
    }
    impl Program for FocusHeader {
        type Message = ();
        fn view<'a>(&'a self, theme: &'a CompTheme) -> CompElement<'a, ()> {
            header_bar()
                .theme(theme)
                .compositor_outline(true)
                .title(&self.title)
                .focused(self.focused)
                .on_close(())
                .into_element()
        }
        fn focus_outline(&self, _: &CompTheme) -> Option<FocusOutline> {
            Some(FocusOutline {
                focused: self.focused,
                animate: true,
                duration: std::time::Duration::from_millis(420),
                curve: [0.0, 0.0, 1.0, 1.0],
            })
        }
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
    }
    let event_loop = calloop::EventLoop::<crate::state::State>::try_new().unwrap();
    let theme = theme();
    let element = IcedElement::new(
        FocusHeader {
            focused: false,
            title: "Explorer".into(),
        },
        (800, ssd_header_render_height(&theme) as i32),
        event_loop.handle(),
        theme.clone(),
    );
    let mut internal = element.0.lock().unwrap();
    let now = IcedInstant::now();
    let redraw = |internal: &mut IcedElementInternal<FocusHeader>, ms| {
        internal.needs_redraw = false;
        internal
            .event_queue
            .push(Event::Window(WindowEvent::RedrawRequested(
                now + std::time::Duration::from_millis(ms),
            )));
        internal.update(UpdateSource::AnimRedraw);
        internal
            .focus_outline_frame([0; 4], now + std::time::Duration::from_millis(ms))
            .unwrap()
    };
    internal.program.focused = true;
    // Focus is handled before the renderer gets its first frame. Layout, input
    // handlers or GPU scheduling can take time without showing any outline.
    internal
        .event_queue
        .push(Event::Window(WindowEvent::RedrawRequested(now)));
    internal.update(UpdateSource::Input);
    let start = redraw(&mut internal, 200);
    assert_eq!(start.progress, 0.0);
    assert!(internal.focus.is_animating());
    let middle = redraw(&mut internal, 410);
    assert!((middle.progress - 0.5).abs() < 0.01);
    assert!(internal.focus.is_animating());
    // Text/size changes relayout the actual pill but never restart focus gain.
    internal.program.title = "A much longer conversation title for the focused app".into();
    internal.size.w = 1000;
    let resized = redraw(&mut internal, 410);
    assert_eq!(middle.progress, resized.progress);
    assert_ne!(middle.bounds, resized.bounds);
    assert!((resized.bounds.center_x() - 500.0).abs() < 1.0);
    let pill = internal
        .renderer
        .layers()
        .iter()
        .flat_map(|layer| &layer.quads)
        .find(|(quad, _)| quad.bounds == resized.bounds)
        .unwrap();
    assert_eq!(
        pill.0.border.color,
        Color::TRANSPARENT,
        "no second static hairline"
    );
    assert_eq!(redraw(&mut internal, 620).progress, 1.0);
    assert!(!internal.focus.is_animating());
    assert_eq!(redraw(&mut internal, 700).progress, 1.0);
    internal.program.focused = false;
    assert_eq!(redraw(&mut internal, 701).progress, 0.0);
    assert!(!internal.focus.is_animating());
    // The shared theme keeps its neutral border for legacy/standalone headers.
    assert_ne!(theme.window_border_color(), Color::TRANSPARENT);
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
fn popup_exit_is_render_only_and_requests_frames_until_hidden() {
    struct Popup {
        visible: bool,
        views: Arc<std::sync::atomic::AtomicUsize>,
    }
    impl Program for Popup {
        type Message = ();
        fn view<'a>(&'a self, _: &'a CompTheme) -> CompElement<'a, ()> {
            self.views.fetch_add(1, Ordering::SeqCst);
            iced_widget::Space::new().into()
        }
        fn visibility(&self, theme: &CompTheme) -> Option<Visibility> {
            Some(Visibility {
                visible: self.visible,
                ..Visibility::fade_rise(theme.motion)
            })
        }
    }
    let event_loop = calloop::EventLoop::<crate::state::State>::try_new().unwrap();
    let theme = theme();
    let duration = theme.motion.layer_open;
    let views = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    let element = IcedElement::new(
        Popup {
            visible: true,
            views: views.clone(),
        },
        (100, 40),
        event_loop.handle(),
        theme,
    );
    let active = output("exit");
    let now = IcedInstant::now();
    let mut internal = element.0.lock().unwrap();
    internal.outputs.insert(active.clone());
    internal.start_visibility_frame(now);
    internal.sync_visibility(now + duration);
    assert_eq!(internal.visibility_frame, VisibilityFrame::VISIBLE);
    internal.program.visible = false;
    internal.cursor_pos = Some((50.0, 20.0).into());
    internal
        .event_queue
        .push(Event::Mouse(MouseEvent::CursorMoved {
            position: IcedPoint::new(50.0, 20.0),
        }));
    internal.animate_exit(now + duration);
    assert!(internal.render_only);
    assert!(internal.event_queue.is_empty() && internal.cursor_pos.is_none());
    assert!(take_redraw_request(&active));
    let before = views.load(Ordering::SeqCst);
    for elapsed in [
        std::time::Duration::ZERO,
        duration / 2,
        duration + std::time::Duration::from_millis(1),
    ] {
        internal.advance_exit(now + duration + elapsed);
        assert_eq!(
            views.load(Ordering::SeqCst),
            before,
            "keep the last paint, not a new hover state"
        );
        assert!(!internal.needs_redraw);
        assert_eq!(take_redraw_request(&active), elapsed < duration);
    }
    assert_eq!(internal.visibility_frame.opacity, 0.0);
    drop(internal);
    assert!(element.is_fully_hidden_at(now + duration * 3));
}

#[test]
fn popup_surface_animation_keeps_body_hit_testing_and_pointer_coordinates_in_sync() {
    struct Popup;
    impl Program for Popup {
        type Message = ();
        fn view<'a>(&'a self, _: &'a CompTheme) -> CompElement<'a, ()> {
            iced_widget::button(iced_widget::Space::new().width(100.0).height(40.0))
                .padding(0)
                .on_press(())
                .into()
        }
        fn visibility(&self, theme: &CompTheme) -> Option<Visibility> {
            Some(Visibility::fade_rise(theme.motion))
        }
        fn backdrop_blur(
            &self,
            _: &CompTheme,
            size: Size<i32, Logical>,
            _: &[Layer],
            _: [u8; 4],
        ) -> Option<(iced_core::Rectangle, [u8; 4])> {
            Some((
                iced_core::Rectangle::with_size(IcedSize::new(size.w as f32, size.h as f32)),
                [12; 4],
            ))
        }
    }
    let event_loop = calloop::EventLoop::<crate::state::State>::try_new().unwrap();
    let theme = theme();
    let element = IcedElement::new(Popup, (100, 40), event_loop.handle(), theme.clone());
    let active_output = output("popup");
    let now = IcedInstant::now();
    let duration = theme.motion.layer_open;
    {
        let mut internal = element.0.lock().unwrap();
        internal.outputs.insert(active_output.clone());
        assert_eq!(internal.visibility_frame.opacity, 0.0);
        // Measuring/resizing does not advance an unshown surface.
        internal.sync_visibility(now + duration * 3);
        assert_eq!(internal.visibility_frame.opacity, 0.0);
        internal.start_visibility_frame(now);
    }
    for elapsed in [std::time::Duration::ZERO, duration / 2, duration] {
        let (frame, body) = {
            let mut internal = element.0.lock().unwrap();
            internal.needs_redraw = false;
            internal
                .event_queue
                .push(Event::Window(WindowEvent::RedrawRequested(now + elapsed)));
            internal.update(UpdateSource::AnimRedraw);
            assert_eq!(take_redraw_request(&active_output), elapsed < duration);
            let frame = internal.visibility_frame;
            let body = frame.bounds(iced_core::Rectangle::with_size(IcedSize::new(100.0, 40.0)));
            let cursor = internal.local_position(body.center());
            assert!((cursor.x - 50.0).abs() < 0.0001 && (cursor.y - 20.0).abs() < 0.0001);
            (frame, body)
        };
        assert_eq!(element.backdrop_bounds().unwrap(), body);
        let hit = element.backdrop_input_bounds().unwrap();
        assert!((hit.loc.x - body.x as f64).abs() < 0.0001);
        assert!((hit.size.w - 100.0 * frame.scale as f64).abs() < 0.0001);
        for output_scale in [1.0, 1.5, 2.0] {
            let origin = Point::<i32, Physical>::from((150, 240));
            let pixel_origin = frame.location(origin, output_scale.into());
            assert!((pixel_origin.x - 150.0 - body.x as f64 * output_scale).abs() < 0.0001);
            assert!((pixel_origin.y - 240.0 - body.y as f64 * output_scale).abs() < 0.0001);
        }
    }
    assert_eq!(
        element.0.lock().unwrap().visibility_frame,
        VisibilityFrame::VISIBLE
    );
}

#[test]
#[ignore = "requires surfaceless EGL (Mesa llvmpipe or a GPU driver)"]
fn gles_popup_texture_and_blur_share_the_surface_animation() -> anyhow::Result<()> {
    use smithay::backend::{
        egl::{EGLContext, EGLDisplay, native::EGLSurfacelessDisplay},
        renderer::{
            Bind, Offscreen, damage::OutputDamageTracker, element::Element as _,
            gles::GlesRenderbuffer, glow::GlowRenderer,
        },
    };
    use std::borrow::BorrowMut;
    struct Popup {
        visible: bool,
    }
    impl Program for Popup {
        type Message = ();
        fn visibility(&self, theme: &CompTheme) -> Option<Visibility> {
            Some(Visibility {
                visible: self.visible,
                ..Visibility::fade_rise(theme.motion)
            })
        }
        fn view<'a>(&'a self, _: &'a CompTheme) -> CompElement<'a, ()> {
            iced_widget::container(iced_widget::Space::new())
                .width(120.0)
                .height(64.0)
                .style(|_| iced_widget::container::Style {
                    background: Some(Color::from_rgba(0.2, 0.3, 0.4, 0.75).into()),
                    border: iced_core::Border::default().rounded(12),
                    ..Default::default()
                })
                .into()
        }
        fn backdrop_blur(
            &self,
            _: &CompTheme,
            size: Size<i32, Logical>,
            _: &[Layer],
            _: [u8; 4],
        ) -> Option<(iced_core::Rectangle, [u8; 4])> {
            Some((
                iced_core::Rectangle::with_size(IcedSize::new(size.w as f32, size.h as f32)),
                [12; 4],
            ))
        }
    }
    // SAFETY: a fresh surfaceless context, used only on this test thread.
    let display = unsafe { EGLDisplay::new(EGLSurfacelessDisplay)? };
    let context = EGLContext::new(&display)?;
    let mut renderer = unsafe { GlowRenderer::new(context)? };
    crate::backend::render::init_shaders(renderer.borrow_mut())?;
    let event_loop = calloop::EventLoop::<crate::state::State>::try_new().unwrap();
    for scale in [1.0, 1.5, 2.0] {
        let theme = theme();
        let duration = theme.motion.layer_open;
        let element = IcedElement::new(
            Popup { visible: true },
            (120, 64),
            event_loop.handle(),
            theme,
        );
        let output = output("popup-gpu");
        output.change_current_state(
            None,
            None,
            Some(smithay::output::Scale::Fractional(scale)),
            None,
        );
        SpaceElement::output_enter(&element, &output, element.bbox());
        // Blur captures include a sampling gutter beyond the visible body.
        // Compare their transform against an unanimated capture, not against
        // the UI rectangle (which deliberately has no such gutter).
        let mut reference_state = BlurState::default();
        let reference = BlurElement::from_state(
            &mut renderer,
            &mut reference_state,
            Rectangle::new(
                Point::<f64, Physical>::from((150.0, 240.0)).to_logical(scale),
                (120.0, 64.0).into(),
            ),
            scale,
            [12; 4],
            configured_blur_strength(true),
            1.0,
        )?
        .expect("reference backdrop")
        .geometry(scale.into());
        let now = IcedInstant::now();
        let mut previous_ui_id = None;
        for (opening, elapsed) in [
            (true, std::time::Duration::ZERO),
            (true, duration / 2),
            (true, duration),
            (false, std::time::Duration::ZERO),
            (false, duration / 2),
            (false, duration + std::time::Duration::from_millis(1)),
        ] {
            let frame = {
                let mut internal = element.0.lock().unwrap();
                if opening {
                    internal.start_visibility_frame(now);
                    internal.sync_visibility(now + elapsed);
                } else {
                    if elapsed.is_zero() {
                        internal.program.visible = false;
                        internal.animate_exit(now + duration);
                    }
                    internal.render_only = true;
                    internal.advance_exit(now + duration + elapsed);
                    // Keep the deterministic sample during GPU submission;
                    // advance_exit's real-time scheduling is tested separately.
                    internal.render_only = false;
                }
                // Freeze the deterministic sample; this is a GPU transform,
                // not a reason to render the widget tree into another buffer.
                internal.needs_redraw = false;
                assert_eq!(internal.buffers.len(), 1);
                internal.visibility_frame
            };
            let mut elements = Vec::new();
            element.push_render_elements(
                &mut renderer,
                (150, 240).into(),
                scale.into(),
                1.0,
                [12; 4],
                &mut |e| elements.push(e),
                None,
            );
            if frame.opacity == 0.0 {
                assert!(
                    elements.is_empty(),
                    "no bare blur rectangle before opening or after closing"
                );
                continue;
            }
            let ui = elements
                .iter()
                .find(|e| matches!(e, IcedRenderElement::UI(_) | IcedRenderElement::ScaledUI(_)))
                .unwrap();
            let blur = elements
                .iter()
                .find(|e| {
                    matches!(
                        e,
                        IcedRenderElement::Blur(_) | IcedRenderElement::ScaledBlur(_)
                    )
                })
                .expect("animated backdrop");
            let capture = blur.geometry(scale.into());
            let animated_origin = frame
                .location((150, 240).into(), scale.into())
                .to_i32_round::<i32>();
            let mut expected = reference;
            expected.loc -= Point::from((150, 240));
            expected = expected.to_f64().upscale(frame.scale as f64).to_i32_round();
            expected.loc += animated_origin;
            for (actual, expected) in [
                (capture.loc.x, expected.loc.x),
                (capture.loc.y, expected.loc.y),
                (capture.size.w, expected.size.w),
                (capture.size.h, expected.size.h),
            ] {
                assert!(
                    actual.abs_diff(expected) <= 1,
                    "blur capture did not follow the surface: {capture:?}, expected {expected}"
                );
            }
            assert!((ui.alpha() - frame.opacity).abs() < 0.0001);
            assert!((blur.alpha() - frame.opacity).abs() < 0.0001);
            assert_eq!(
                matches!(ui, IcedRenderElement::ScaledUI(_)),
                frame.scale < 1.0
            );
            if let Some(id) = &previous_ui_id {
                assert_eq!(ui.id(), id, "opening must reuse its raster buffer");
            }
            previous_ui_id = Some(ui.id().clone());
            let mut target = <GlowRenderer as Offscreen<GlesRenderbuffer>>::create_buffer(
                &mut renderer,
                Fourcc::Abgr8888,
                (512, 512).into(),
            )?;
            let mut fb = renderer.bind(&mut target)?;
            let mut damage = OutputDamageTracker::new((512, 512), scale, Transform::Normal);
            // Exercise framebuffer capture through the scale wrapper as well
            // as drawing. A stationary/missing backdrop must not be hidden by
            // a geometry-only assertion.
            damage.render_output(&mut renderer, &mut fb, 0, &elements, [0.2, 0.4, 0.7, 1.0])?;
        }
    }
    Ok(())
}

#[test]
fn focus_only_frames_do_not_rebuild_iced_and_stop_requesting_frames_when_settled() {
    struct OutlineOnly {
        focused: bool,
        laid_out: bool,
        views: Arc<std::sync::atomic::AtomicUsize>,
    }
    impl Program for OutlineOnly {
        type Message = ();
        fn view<'a>(&'a self, _: &'a CompTheme) -> CompElement<'a, ()> {
            self.views.fetch_add(1, Ordering::Relaxed);
            iced_widget::Space::new().into()
        }
        fn focus_outline(&self, theme: &CompTheme) -> Option<FocusOutline> {
            Some(crate::shell::element::header_bar::halo_focus_outline(
                theme,
                self.focused,
                false,
            ))
        }
        fn backdrop_blur(
            &self,
            _: &CompTheme,
            _: Size<i32, Logical>,
            _: &[Layer],
            _: [u8; 4],
        ) -> Option<(iced_core::Rectangle, [u8; 4])> {
            self.laid_out.then_some((
                iced_core::Rectangle::new(IcedPoint::new(20.0, 0.0), IcedSize::new(80.0, 40.0)),
                [20; 4],
            ))
        }
    }
    let event_loop = calloop::EventLoop::<crate::state::State>::try_new().unwrap();
    let views = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    let element = IcedElement::new(
        OutlineOnly {
            focused: false,
            laid_out: false,
            views: views.clone(),
        },
        (120, 40),
        event_loop.handle(),
        theme(),
    );
    let active = output("focus");
    let idle = output("idle");
    let mut internal = element.0.lock().unwrap();
    internal.outputs.insert(active.clone());
    let now = IcedInstant::now();
    internal.program.focused = true;
    internal
        .event_queue
        .push(Event::Window(WindowEvent::RedrawRequested(now)));
    internal.update(UpdateSource::Input);
    assert!(
        !internal.needs_redraw,
        "focus is shader-only, not a widget animation"
    );
    assert!(take_redraw_request(&active));
    assert!(!take_redraw_request(&idle));
    assert!(internal.focus_outline_frame([0; 4], now).is_none());
    internal.program.laid_out = true;
    let first = now + std::time::Duration::from_millis(200);
    let view_count = views.load(Ordering::Relaxed);
    for ms in [0, 16, 32, 210, 420, 500] {
        let frame = internal
            .focus_outline_frame([0; 4], first + std::time::Duration::from_millis(ms))
            .unwrap();
        if ms == 0 {
            assert_eq!(frame.progress, 0.0);
        } else if ms == 16 {
            assert!(frame.progress > 0.0 && frame.progress < 0.01);
        } else if ms >= 420 {
            assert_eq!(frame.progress, 1.0);
        }
        assert_eq!(
            views.load(Ordering::Relaxed),
            view_count,
            "no new widget tree for a focus-only frame"
        );
        assert!(!internal.needs_redraw);
        assert_eq!(take_redraw_request(&active), ms < 420);
        assert!(!take_redraw_request(&idle));
    }
}

#[test]
fn halo_tooltip_delay_and_suppression_delegate_fades_to_the_compositor() {
    use icetron_p::utils::platform::test_clock;
    use std::time::Duration;
    let _clock = test_clock::Frozen::start();
    let at = icetron_p::utils::platform::now();
    let event_loop = calloop::EventLoop::<crate::state::State>::try_new().unwrap();
    let mut tokens = icetron_themes::dynamic::DynamicTheme::from_theme(&*theme());
    tokens.duration_fast = 120.0;
    tokens.duration_normal = 120.0;
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
        internal.tooltip.advance(now);
    };
    step(&mut internal, 0);
    step(&mut internal, 399);
    assert!(internal.tooltip.report.is_none());
    step(&mut internal, 400);
    assert_eq!(
        internal.tooltip.report.as_ref().unwrap().opacity,
        1.0,
        "widget supplies the request, not a second fade"
    );
    let first = internal.tooltip.snapshots()[0].clone();
    assert_eq!(first.label, crate::fl!("window-menu-close"));
    assert_eq!(first.opacity, 0.0);
    assert!(
        first.bounds.y > pill.y,
        "operation must report laid-out, not zero, coordinates"
    );
    step(&mut internal, 460);
    let middle = internal.tooltip.snapshots()[0].clone();
    assert!(
        (middle.opacity - 0.5).abs() < 0.01,
        "use the surface fade preset"
    );
    step(&mut internal, 520);
    let last = internal.tooltip.snapshots()[0].clone();
    assert_eq!(last.opacity, 1.0);
    assert_eq!(last.bounds, first.bounds);
    assert_eq!(last.bounds, middle.bounds);
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
    assert_eq!(
        internal.tooltip.snapshots()[0].label,
        "Close",
        "retain the outgoing chip after click suppression"
    );
    step(&mut internal, 600);
    let fading = internal.tooltip.snapshots()[0].opacity;
    assert!(fading > 0.0 && fading < 1.0);
    step(&mut internal, 1000);
    assert!(internal.tooltip.snapshots().is_empty());
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
    assert_eq!(internal.tooltip.snapshots()[0].opacity, 0.0);
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
        scale: 1.0,
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
