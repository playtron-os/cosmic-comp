//! Raster regressions for the actual Tiny-Skia SSD widget tree.

use super::*;
use iced_core::{Color, Font, Pixels, Rectangle, Size, mouse, renderer::Style};
use iced_graphics::{Viewport, damage};
use iced_runtime::{UserInterface, user_interface};
use iced_tiny_skia::{Layer, Renderer};
use icetron_themes::dynamic::DEFAULT_THEME_PAIR;
use std::sync::Arc;

#[test]
fn halo_header_double_click_toggles_without_stealing_button_clicks_or_drags() {
    use iced_core::{
        Event, Point, Vector,
        widget::{Id, Operation},
    };

    #[derive(Debug, Clone, PartialEq)]
    enum Message {
        Toggle,
        Drag,
        Close,
        Menu,
    }
    #[derive(Clone, Copy)]
    enum Target {
        Title,
        Maximize,
        Close,
    }
    #[derive(Clone, Copy)]
    enum Gesture {
        Single,
        Double,
        Drag,
        DoubleThenDrag,
        Right,
    }
    #[derive(Default)]
    struct Title(Option<Rectangle>);
    impl Operation for Title {
        fn traverse(&mut self, f: &mut dyn FnMut(&mut dyn Operation)) {
            f(self);
        }
        fn text(&mut self, _: Option<&Id>, bounds: Rectangle, text: &str) {
            if text == "Gesture target" {
                self.0 = Some(bounds);
            }
        }
    }

    let theme = theme();
    for maximized in [false, true] {
        for (target, gesture, expected) in [
            (Target::Title, Gesture::Single, vec![]),
            (Target::Title, Gesture::Double, vec![Message::Toggle]),
            (
                Target::Title,
                Gesture::DoubleThenDrag,
                vec![Message::Toggle],
            ),
            (Target::Title, Gesture::Drag, vec![Message::Drag]),
            (Target::Title, Gesture::Right, vec![Message::Menu]),
            (
                Target::Maximize,
                Gesture::Double,
                vec![Message::Toggle, Message::Toggle],
            ),
            (
                Target::Close,
                Gesture::Double,
                vec![Message::Close, Message::Close],
            ),
        ] {
            let mut renderer = Renderer::new(Font::DEFAULT, Pixels(16.0));
            let header = header_bar()
                .theme(&theme)
                .title("Gesture target")
                .focused(true)
                .maximized(maximized)
                .on_drag(Message::Drag)
                .on_maximize(Message::Toggle)
                .on_close(Message::Close)
                .on_right_click(Message::Menu)
                .into_element();
            let size = Size::new(900.0, ssd_header_render_height(&theme) as f32);
            let mut ui = UserInterface::build(
                header,
                size,
                user_interface::Cache::default(),
                &mut renderer,
            );
            ui.draw(
                &mut renderer,
                &theme.to_iced_theme(),
                &Style::default(),
                mouse::Cursor::Unavailable,
            );
            let mut title = Title::default();
            ui.operate(&renderer, &mut title);
            let metrics = theme.halo_style();
            let (pill, _) = crate::shell::element::window::halo_backdrop_blur(
                renderer.layers(),
                metrics.pill_height(),
                size.width,
            )
            .unwrap();
            let point = match target {
                Target::Title => title.0.expect("laid-out title").center(),
                Target::Close => Point::new(
                    pill.x + pill.width - metrics.padding_horizontal - metrics.control_size * 0.5,
                    pill.center_y(),
                ),
                Target::Maximize => Point::new(
                    pill.x + pill.width
                        - metrics.padding_horizontal
                        - metrics.control_size * 1.5
                        - metrics.gap,
                    pill.center_y(),
                ),
            };
            let press = Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left));
            let release = Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left));
            let moved = Event::Mouse(mouse::Event::CursorMoved {
                position: point + Vector::new(10.0, 0.0),
            });
            let events = match gesture {
                Gesture::Single => vec![press, release],
                Gesture::Double => vec![press.clone(), release.clone(), press, release],
                Gesture::DoubleThenDrag => {
                    vec![press.clone(), release.clone(), press, moved, release]
                }
                Gesture::Drag => vec![press, moved, release],
                Gesture::Right => vec![
                    Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Right)),
                    Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Right)),
                ],
            };
            let mut messages = Vec::new();
            let mut cursor = mouse::Cursor::Available(point);
            for event in
                std::iter::once(Event::Mouse(mouse::Event::CursorMoved { position: point }))
                    .chain(events)
            {
                if let Event::Mouse(mouse::Event::CursorMoved { position }) = event {
                    cursor = mouse::Cursor::Available(position);
                }
                ui.update(&[event], cursor, &mut renderer, &mut messages);
            }
            assert_eq!(messages, expected, "maximized={maximized}");
        }
    }
}

#[test]
fn maximize_control_shows_restore_in_fullscreen_and_emits_its_action() {
    use iced_core::{
        Event, Point,
        widget::{Id, Operation},
    };
    use icetron_p::{
        prelude::TooltipReport,
        utils::platform::{now, test_clock},
    };
    #[derive(Debug, Clone, PartialEq)]
    enum Message {
        Maximize,
        Fullscreen,
    }
    #[derive(Default)]
    struct Tooltip(Option<String>);
    impl Operation for Tooltip {
        fn traverse(&mut self, operate: &mut dyn FnMut(&mut dyn Operation)) {
            operate(self);
        }
        fn custom(&mut self, _: Option<&Id>, _: Rectangle, state: &mut dyn std::any::Any) {
            if let Some(report) = state.downcast_ref::<TooltipReport>() {
                self.0 = Some(report.label.clone());
            }
        }
    }
    let _clock = test_clock::Frozen::start();
    let mut tokens = icetron_themes::dynamic::DynamicTheme::from_theme(&*theme());
    tokens.duration_fast = 0.0;
    let theme = CompTheme::new(Arc::new(tokens), true);
    let maximize = crate::fl!("window-menu-maximize");
    let restore = crate::fl!("window-menu-restore");
    for (maximized, fullscreen, expected) in [
        (false, false, maximize.as_str()),
        (true, false, restore.as_str()),
        (false, true, restore.as_str()),
        (true, true, restore.as_str()),
    ] {
        let mut renderer = Renderer::new(Font::DEFAULT, Pixels(16.0));
        let header = header_bar()
            .theme(&theme)
            .title("Document")
            .focused(true)
            .maximized(maximized)
            .on_maximize(Message::Maximize)
            .on_fullscreen(Message::Fullscreen, fullscreen)
            .into_element();
        let size = Size::new(900.0, ssd_header_render_height(&theme) as f32);
        let mut ui = UserInterface::build(
            header,
            size,
            user_interface::Cache::default(),
            &mut renderer,
        );
        ui.draw(
            &mut renderer,
            &theme.to_iced_theme(),
            &Style::default(),
            mouse::Cursor::Unavailable,
        );
        let metrics = theme.halo_style();
        let (pill, _) = crate::shell::element::window::halo_backdrop_blur(
            renderer.layers(),
            metrics.pill_height(),
            size.width,
        )
        .unwrap();
        // Maximize is immediately left of the fullscreen button in this two-action header.
        let point = Point::new(
            pill.x + pill.width
                - metrics.padding_horizontal
                - 1.5 * metrics.control_size
                - metrics.gap,
            pill.center_y(),
        );
        let cursor = mouse::Cursor::Available(point);
        let mut messages = Vec::new();
        ui.update(
            &[
                Event::Mouse(mouse::Event::CursorMoved { position: point }),
                Event::Window(iced_core::window::Event::RedrawRequested(now())),
            ],
            cursor,
            &mut renderer,
            &mut messages,
        );
        test_clock::advance(std::time::Duration::from_millis(400));
        ui.update(
            &[Event::Window(iced_core::window::Event::RedrawRequested(
                now(),
            ))],
            cursor,
            &mut renderer,
            &mut messages,
        );
        ui.draw(
            &mut renderer,
            &theme.to_iced_theme(),
            &Style::default(),
            cursor,
        );
        let mut tooltip = Tooltip::default();
        ui.operate(&renderer, &mut tooltip);
        assert_eq!(
            tooltip.0.as_deref(),
            Some(expected),
            "maximized={maximized}, fullscreen={fullscreen}"
        );
        ui.update(
            &[
                Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)),
                Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left)),
            ],
            cursor,
            &mut renderer,
            &mut messages,
        );
        assert_eq!(messages, vec![Message::Maximize]);
    }
}

fn theme() -> CompTheme {
    let mut theme = DEFAULT_THEME_PAIR.load(true);
    theme.window_header_style = WindowHeaderStyle::Halo;
    theme.radii_max = 9999.0;
    theme.glass_glance = Color::from_rgba(0.0, 0.0, 0.0, 0.76);
    theme.shadow_popover = [(8.0, 16.0, 0.102), (16.0, 32.0, 0.149), (32.0, 64.0, 0.2)]
        .map(|(y, blur_radius, a)| iced_core::Shadow {
            color: Color::from_rgba(0.0, 0.0, 0.0, a),
            offset: iced_core::Vector::new(0.0, y),
            blur_radius,
            ..Default::default()
        })
        .to_vec();
    CompTheme::new(Arc::new(theme), true)
}

fn layout(
    theme: &CompTheme,
    width: f32,
    title: &str,
    scale: f32,
) -> (Renderer, Viewport, user_interface::Cache) {
    layout_with(theme, width, title, scale, |header| header)
}

/// [`layout`] with the header adjusted before it is built.
fn layout_with(
    theme: &CompTheme,
    width: f32,
    title: &str,
    scale: f32,
    configure: impl for<'a> FnOnce(HeaderBar<'a, ()>) -> HeaderBar<'a, ()>,
) -> (Renderer, Viewport, user_interface::Cache) {
    static FONTS: std::sync::Once = std::sync::Once::new();
    FONTS.call_once(|| {
        let mut fonts = iced_graphics::text::font_system().write().unwrap();
        for data in icetron_themes::fonts::ALL {
            fonts.load_font(std::borrow::Cow::Borrowed(*data));
        }
    });
    let mut renderer = Renderer::new(Font::DEFAULT, Pixels(16.0));
    let element = header_bar()
        .theme(theme)
        .title(title)
        .app_name("Files")
        .focused(true)
        .on_close(())
        .on_minimize(())
        .on_maximize(())
        .on_right_click(())
        .on_screenshot(())
        .on_fullscreen((), false)
        .on_new_window(());
    let element = configure(element).into_element();
    let size = Size::new(width, ssd_header_render_height(theme) as f32);
    let mut ui = UserInterface::build(
        element,
        size,
        user_interface::Cache::default(),
        &mut renderer,
    );
    let mut messages = Vec::new();
    ui.update(
        &[iced_core::Event::Window(
            iced_core::window::Event::RedrawRequested(iced_core::time::Instant::now()),
        )],
        mouse::Cursor::Unavailable,
        &mut renderer,
        &mut messages,
    );
    ui.draw(
        &mut renderer,
        &iced_core::Theme::Dark,
        &Style::default(),
        mouse::Cursor::Unavailable,
    );
    let viewport = Viewport::with_physical_size(
        Size::new(
            (size.width * scale).round() as u32,
            (size.height * scale).round() as u32,
        ),
        scale,
    );
    // Renderer text items hold weak paragraph references into the widget tree.
    (renderer, viewport, ui.into_cache())
}

fn draw(
    renderer: &mut Renderer,
    viewport: &Viewport,
    pixels: &mut tiny_skia::Pixmap,
    damage: &[Rectangle],
) {
    let mut mask = tiny_skia::Mask::new(pixels.width(), pixels.height()).unwrap();
    renderer.draw(
        &mut pixels.as_mut(),
        &mut mask,
        viewport,
        damage,
        Color::TRANSPARENT,
    );
}

#[test]
fn halo_partial_repaint_matches_fresh_frame() {
    let theme = theme();
    let (mut old, viewport, _old_cache) = layout(&theme, 1024.0, "A longer Explorer title", 1.5);
    let mut pixels =
        tiny_skia::Pixmap::new(viewport.physical_width(), viewport.physical_height()).unwrap();
    let full = [Rectangle::with_size(viewport.logical_size())];
    draw(&mut old, &viewport, &mut pixels, &full);
    let (mut current, _, _current_cache) = layout(&theme, 1024.0, "Files", 1.5);
    let damage = damage::group(
        damage::diff(
            old.layers(),
            current.layers(),
            |layer| vec![layer.bounds],
            Layer::damage,
        ),
        full[0],
    );
    draw(&mut current, &viewport, &mut pixels, &damage);
    let mut fresh = tiny_skia::Pixmap::new(pixels.width(), pixels.height()).unwrap();
    draw(&mut current, &viewport, &mut fresh, &full);
    if let Some(dir) = std::env::var_os("HALO_SNAPSHOT_DIR") {
        let dir = std::path::PathBuf::from(dir);
        std::fs::create_dir_all(&dir).unwrap();
        pixels.save_png(dir.join("halo-partial.png")).unwrap();
        fresh.save_png(dir.join("halo-fresh.png")).unwrap();
    }
    let different = pixels
        .data()
        .iter()
        .zip(fresh.data())
        .filter(|(a, b)| a != b)
        .count();
    assert_eq!(
        different, 0,
        "partial repaint must not leave shadow remnants"
    );
}

#[test]
fn halo_controls_stay_neutral_when_the_workspace_accent_changes() {
    let mut theme = theme();
    let accent = Color::from_rgb(0.18, 0.62, 0.91);
    theme.workspace_accent = Some(accent);
    let (mut renderer, viewport, _cache) = layout(&theme, 1024.0, "Explorer", 1.0);
    let glyphs: Vec<_> = renderer
        .layers()
        .iter()
        .flat_map(|layer| &layer.images)
        .filter_map(|image| match image {
            iced_graphics::Image::Vector { svg, .. } => svg.color,
            _ => None,
        })
        .collect();
    assert_eq!(
        glyphs.len(),
        8,
        "screenshot, record, menu, new, minimize, maximize, fullscreen, close"
    );
    for (index, color) in glyphs.into_iter().enumerate() {
        assert_eq!(
            color,
            if index == 1 {
                theme.text_quaternary()
            } else {
                theme.text_tertiary()
            },
            "ordinary icons, including the chevron, use neutral text tokens; record is disabled"
        );
    }
    if let Some(dir) = std::env::var_os("HALO_SNAPSHOT_DIR") {
        let mut pixels =
            tiny_skia::Pixmap::new(viewport.physical_width(), viewport.physical_height()).unwrap();
        draw(
            &mut renderer,
            &viewport,
            &mut pixels,
            &[Rectangle::with_size(viewport.logical_size())],
        );
        // Tiny-Skia's iced renderer emits BGRA for the compositor's ARGB buffer;
        // PNG expects RGBA. This conversion is only for the saved preview.
        for pixel in pixels.data_mut().chunks_exact_mut(4) {
            pixel.swap(0, 2);
        }
        pixels
            .save_png(std::path::PathBuf::from(dir).join("halo-controls.png"))
            .unwrap();
    }
}

#[test]
fn halo_record_glyph_turns_destructive_while_recording() {
    let theme = theme();
    let (mut renderer, _viewport, _cache) =
        layout_with(&theme, 1024.0, "Explorer", 1.0, |header| {
            header.on_record(()).recording(true)
        });
    let glyphs: Vec<_> = renderer
        .layers()
        .iter()
        .flat_map(|layer| &layer.images)
        .filter_map(|image| match image {
            iced_graphics::Image::Vector { svg, .. } => svg.color,
            _ => None,
        })
        .collect();
    assert_eq!(glyphs.len(), 8, "the dot is a quad, not a ninth glyph");
    for (index, color) in glyphs.into_iter().enumerate() {
        assert_eq!(
            color,
            if index == 1 {
                theme.feedback_error_primary()
            } else {
                theme.text_tertiary()
            },
            "only the active Record glyph wears the destructive colour"
        );
    }
}

#[test]
fn halo_app_name_is_a_secondary_label_only_for_a_distinct_title() {
    let theme = theme();
    let secondary_labels = |renderer: &mut Renderer| {
        renderer
            .layers()
            .iter()
            .flat_map(|layer| &layer.text)
            .flat_map(|item| item.as_slice())
            .filter(|text| match text {
                iced_graphics::text::Text::Paragraph { color, .. }
                | iced_graphics::text::Text::Cached { color, .. }
                | iced_graphics::text::Text::Editor { color, .. } => {
                    *color == theme.text_quaternary()
                }
                _ => false,
            })
            .count()
    };
    let (mut same, _, _same_cache) = layout(&theme, 1024.0, "Files", 1.0);
    let (mut different, _, _different_cache) = layout(&theme, 1024.0, "Documents", 1.0);
    assert_eq!(secondary_labels(&mut same), 0);
    assert_eq!(secondary_labels(&mut different), 1);
}

#[test]
fn halo_shadow_fades_before_the_buffer_edge() {
    let theme = theme();
    for scale in [1.0, 1.25, 1.5, 2.0] {
        let (mut renderer, viewport, _cache) = layout(&theme, 1024.0, "Explorer", scale);
        let mut pixels =
            tiny_skia::Pixmap::new(viewport.physical_width(), viewport.physical_height()).unwrap();
        draw(
            &mut renderer,
            &viewport,
            &mut pixels,
            &[Rectangle::with_size(viewport.logical_size())],
        );
        let stride = pixels.width() as usize * 4;
        for row in [0, pixels.height() as usize - 1] {
            let max_alpha = pixels.data()[row * stride..(row + 1) * stride]
                .chunks_exact(4)
                .map(|pixel| pixel[3])
                .max()
                .unwrap();
            assert_eq!(
                max_alpha, 0,
                "shadow is clipped at buffer row {row}, creating a hard strip"
            );
        }
    }
}

#[test]
fn halo_shadow_padding_preserves_pill_and_blur_position() {
    let theme = theme();
    let (mut renderer, _, _cache) = layout(&theme, 1024.0, "Explorer", 1.5);
    let (pill, _) = super::super::window::halo_backdrop_blur(
        renderer.layers(),
        theme.halo_style().pill_height(),
        1024.0,
    )
    .expect("actual header draw must contain the blur pill");
    assert_eq!(pill.y - ssd_header_render_overhang(&theme) as f32, -15.0);
    assert_eq!(
        pill.y + pill.height - ssd_header_render_overhang(&theme) as f32,
        16.0
    );
    assert_eq!(ssd_header_input_height(&theme), 34);
    assert_eq!(ssd_header_height(&theme), 0);
}

#[test]
fn attached_halo_is_flush_with_square_bottom_corners() {
    let theme = theme();
    let mut floating_size = None;
    for joined in [false, true] {
        let (mut renderer, _, _cache) = layout_with(&theme, 900.0, "Explorer", 1.5, |header| {
            header.joined_to_window(joined)
        });
        let (pill, radii) = super::super::window::halo_backdrop_blur(
            renderer.layers(),
            theme.halo_style().pill_height(),
            900.0,
        )
        .expect("the actual background/shadow shape supplies the blur corners");
        let size = (pill.width, pill.height);
        if let Some(expected) = floating_size {
            assert_eq!(size, expected, "joining Halo must not resize or replace it");
        } else {
            floating_size = Some(size);
        }
        let reserved = ssd_header_height_for(&theme, joined);
        let offset = halo_header_offset(&theme, joined);
        let top = pill.y - ssd_header_render_overhang(&theme) as f32 - offset as f32;
        let bottom = top + pill.height;
        assert_eq!(radii, if joined { [0, 16, 0, 16] } else { [16; 4] });
        let (quad, _) = renderer
            .layers()
            .iter()
            .flat_map(|layer| &layer.quads)
            .find(|(quad, _)| quad.bounds == pill && quad.border.width > 0.0)
            .expect("the Halo's own border quad");
        let width = theme.halo_style().border_width;
        assert_eq!(
            quad.border.sides,
            joined.then_some([width, width, 0.0, width])
        );
        assert_eq!(bottom, if joined { reserved as f32 } else { 16.0 });
        if joined {
            assert_eq!(
                top, 0.0,
                "maximized/tiled Halo must start inside the allocated frame"
            );
            assert_eq!(
                reserved as f32, pill.height,
                "reserve the visible header, not its shadow buffer"
            );
        } else {
            assert_eq!(
                reserved, 0,
                "protocol opt-in keeps the original overlay geometry"
            );
        }
    }
}
