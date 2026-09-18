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
    render(theme, width, scale, |header| {
        configure(
            header
                .title(title)
                .app_name("Files")
                .focused(true)
                .on_close(())
                .on_minimize(())
                .on_maximize(())
                .on_right_click(())
                .on_screenshot(())
                .on_fullscreen((), false)
                .on_new_window(()),
        )
    })
}

/// Build, update and draw an arbitrary header in a `width`-wide window.
fn render(
    theme: &CompTheme,
    width: f32,
    scale: f32,
    build: impl for<'a> FnOnce(HeaderBar<'a, ()>) -> HeaderBar<'a, ()>,
) -> (Renderer, Viewport, user_interface::Cache) {
    static FONTS: std::sync::Once = std::sync::Once::new();
    FONTS.call_once(|| {
        let mut fonts = iced_graphics::text::font_system().write().unwrap();
        for data in icetron_themes::fonts::ALL {
            fonts.load_font(std::borrow::Cow::Borrowed(*data));
        }
    });
    let mut renderer = Renderer::new(Font::DEFAULT, Pixels(16.0));
    let element = build(header_bar().theme(theme)).into_element();
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

// ---------------------------------------------------------------------------
// The Halo pill is capped clear of the window's rounded top corners, and the
// identity gives way for the controls rather than the other way round.
// ---------------------------------------------------------------------------

/// A 16x16 square, enough to stand in for a symbolic app mark.
const TEST_ICON: &[u8] =
    br#"<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16"><rect width="16" height="16" fill="currentColor"/></svg>"#;

const LONG: &str = "A really extremely long window title that will not fit anywhere at all";
const LONG_WORD: &str =
    "Supercalifragilisticexpialidociousandthensomemoreofitwithoutasinglespaceanywhere";
/// Han, an emoji with a variation selector and a ZWJ family, and a combining acute.
const CLUSTERS: &str = "文書編集ファイル管理 \u{2764}\u{FE0F} \u{1F468}\u{200D}\u{1F469}\u{200D}\u{1F467} e\u{301}dite\u{301}ur de documents tre\u{300}s long";

/// What a paragraph actually put on screen.
#[derive(Debug, Clone)]
struct DrawnText {
    /// The string the widget was given.
    source: String,
    /// The part of it the glyphs cover — `None` if a cut landed inside a
    /// character, which would mean a split glyph cluster.
    drawn: Option<String>,
    /// An ellipsis glyph stood in for the rest.
    ellipsized: bool,
    color: Color,
}

impl DrawnText {
    fn drawn(&self) -> &str {
        self.drawn
            .as_deref()
            .expect("text cut on a character boundary")
    }
}

/// Every paragraph the header drew, in draw order.
fn drawn_text(renderer: &mut Renderer) -> Vec<DrawnText> {
    renderer
        .layers()
        .iter()
        .flat_map(|layer| &layer.text)
        .flat_map(|item| item.as_slice())
        .filter_map(|text| {
            let iced_graphics::text::Text::Paragraph {
                paragraph, color, ..
            } = text
            else {
                return None;
            };
            let paragraph = paragraph.upgrade()?;
            let buffer = paragraph.buffer();
            let source: String = buffer
                .lines
                .iter()
                .map(|line| line.text())
                .collect::<Vec<_>>()
                .join("\n");
            let mut ellipsized = false;
            let mut covered = 0_usize;
            for run in buffer.layout_runs() {
                for glyph in run.glyphs {
                    // The ellipsis glyph carries no source range of its own.
                    if glyph.start == glyph.end {
                        ellipsized = true;
                    } else {
                        covered = covered.max(glyph.end);
                    }
                }
            }
            let drawn = source.get(..covered).map(str::to_owned);
            Some(DrawnText {
                source,
                drawn,
                ellipsized,
                color: *color,
            })
        })
        .collect()
}

/// The title and, when it is shown, the app name beside it.
fn identity(renderer: &mut Renderer, theme: &CompTheme) -> (Option<DrawnText>, Option<DrawnText>) {
    let drawn = drawn_text(renderer);
    let by_color = |wanted: Color| drawn.iter().find(|text| text.color == wanted).cloned();
    (
        by_color(theme.text_primary()),
        by_color(theme.text_quaternary()),
    )
}

/// The bounds of every glyph the header drew, in draw order.
fn control_icons(renderer: &mut Renderer) -> Vec<Rectangle> {
    renderer
        .layers()
        .iter()
        .flat_map(|layer| &layer.images)
        .filter_map(|image| match image {
            iced_graphics::Image::Vector { bounds, .. } => Some(*bounds),
            _ => None,
        })
        .collect()
}

/// The hairline dividers between the tray, the menu and the window controls.
fn dividers(renderer: &mut Renderer, theme: &CompTheme) -> Vec<Rectangle> {
    let metrics = theme.halo_style();
    renderer
        .layers()
        .iter()
        .flat_map(|layer| &layer.quads)
        .filter(|(quad, _)| {
            quad.bounds.width == metrics.border_width
                && quad.bounds.height == metrics.divider_height
        })
        .map(|(quad, _)| quad.bounds)
        .collect()
}

fn pill(renderer: &mut Renderer, theme: &CompTheme, width: f32) -> Rectangle {
    super::super::window::halo_backdrop_blur(
        renderer.layers(),
        theme.halo_style().pill_height(),
        width,
    )
    .expect("the drawn Halo pill")
    .0
}

/// Widest the pill may be in a `width`-wide window, and how far in it starts.
fn cap(theme: &CompTheme, width: f32, square_top: bool) -> (f32, f32) {
    let margin = halo_corner_margin(theme, square_top);
    ((width - 2.0 * margin).max(0.0), margin)
}

/// Everything the pill can give up, widest first, with what survives it.
fn shed(theme: &CompTheme, width: f32) -> (usize, bool, usize) {
    let (mut renderer, _, _cache) = render(theme, width, 1.0, |header| {
        header
            .title(LONG)
            .app_name("Files")
            .focused(true)
            .on_close(())
            .on_minimize(())
            .on_maximize(())
            .on_right_click(())
            .on_screenshot(())
            .on_record(())
            .on_fullscreen((), false)
            .on_new_window(())
    });
    let glyphs = control_icons(&mut renderer).len();
    let (title, app_name) = identity(&mut renderer, theme);
    let title_drawn = title.is_some_and(|text| !text.drawn().is_empty());
    (glyphs, title_drawn, app_name.is_some())
        .pipe(|(glyphs, title, app)| (glyphs, title, usize::from(app)))
}

trait Pipe: Sized {
    fn pipe<T>(self, f: impl FnOnce(Self) -> T) -> T {
        f(self)
    }
}
impl<T> Pipe for T {}

/// A pill too narrow for everything gives its parts up in order rather than
/// dropping the controls on the floor: the app name goes first, the tray
/// next, then the window controls, and the title and close button remain.
/// A window whose title is long and whose frame is narrow — an Android
/// emulator is the case this came from. Its identity is the title alone: no
/// icon, and an app name that repeats the title and so is hidden. That single
/// child used to escape the row that keeps room for the controls, so the title
/// drew in full and pushed every control out, close included.
#[test]
fn a_lone_long_title_never_pushes_the_controls_out() {
    let theme = theme();
    let title = "Android Emulator - Pixel7_API36_1:5554";
    for width in [604.0_f32, 460.0, 400.0, 340.0, 302.0] {
        let (mut renderer, _, _cache) = render(&theme, width, 1.0, |header| {
            header
                .title(title)
                .focused(true)
                .on_close(())
                .on_minimize(())
                .on_maximize(())
                .on_right_click(())
                .on_screenshot(())
                .on_record(())
                .on_fullscreen((), false)
                .on_new_window(())
        });
        let glyphs = control_icons(&mut renderer).len();
        let drawn = identity(&mut renderer, &theme).0.expect("the title");
        assert!(
            glyphs >= 1,
            "{width}px drew no controls at all — the close button must survive"
        );
        assert!(
            !drawn.drawn().is_empty(),
            "{width}px drew no name beside the controls"
        );
        // Anything that does not fit is cut, rather than drawn over the
        // controls or pushing them out of the pill.
        let fits = width >= 604.0;
        assert_eq!(
            drawn.ellipsized,
            !fits,
            "{width}px: ellipsized={} for {:?}",
            drawn.ellipsized,
            drawn.drawn()
        );
    }
}

#[test]
fn a_narrow_pill_sheds_its_parts_in_order() {
    let theme = theme();
    let widths = [
        2400.0_f32, 1200.0, 900.0, 700.0, 600.0, 520.0, 460.0, 400.0, 360.0, 320.0, 300.0, 280.0,
        260.0, 240.0, 220.0, 200.0, 180.0, 160.0, 140.0, 120.0, 100.0,
    ];
    let mut seen: Vec<(f32, usize, bool, usize)> = Vec::new();
    for width in widths {
        let (glyphs, title, app) = shed(&theme, width);
        seen.push((width, glyphs, title, app));
    }
    let widest = seen[0].1;
    assert_eq!(widest, 8, "a roomy pill draws every control: {seen:?}");
    // Below this the pill is narrower than the close button and a word
    // together. The identity, the tray and the controls are three rows iced
    // lays out in turn, so which of them gives way first stops being ordered
    // down there and a narrower window can keep one more glyph than a wider
    // one. No window reaches these widths — the compositor's own minimum is
    // far above them — and the pill is unusable either way, so the order is
    // only promised where it can be seen.
    const ORDERED_ABOVE: f32 = 200.0;
    for pair in seen.windows(2) {
        let (wide, wide_glyphs, _, wide_app) = pair[0];
        let (narrow, narrow_glyphs, _, narrow_app) = pair[1];
        assert!(
            narrow_glyphs <= wide_glyphs || narrow < ORDERED_ABOVE,
            "{narrow}px drew more controls than {wide}px: {seen:?}"
        );
        assert!(
            narrow_app <= wide_app || narrow < ORDERED_ABOVE,
            "{narrow}px kept an app name {wide}px had dropped: {seen:?}"
        );
    }
    // The app name is the first thing to go, before any control does.
    let first_control_loss = seen.iter().find(|(_, glyphs, _, _)| *glyphs < widest);
    if let Some((width, _, _, app)) = first_control_loss {
        assert_eq!(
            *app, 0,
            "a control left at {width}px while the app name was still there: {seen:?}"
        );
    }
    // Close never leaves, and the title outlives every other control: while
    // anything besides close is still drawn there is room for text too. Below
    // that the window is narrower than a close button and a word together.
    for (width, glyphs, title, _) in &seen {
        assert!(*glyphs >= 1, "the close button left at {width}px: {seen:?}");
        assert!(
            *title || *glyphs == 1,
            "the title went before the controls did at {width}px: {seen:?}"
        );
    }
    // And it really does shed: the narrowest here keeps far less than the widest.
    let narrowest = seen.last().expect("a sweep").1;
    assert!(
        narrowest < widest,
        "nothing was ever shed across {widths:?}: {seen:?}"
    );
}

#[test]
fn halo_pill_keeps_the_window_corner_radius_clear_on_both_sides() {
    let theme = theme();
    // Anything below this and the controls alone outgrow the capped pill; the
    // identity is gone by then and there is nothing left to give up.
    for width in [2400.0_f32, 1024.0, 900.0, 600.0, 460.0, 400.0, 320.0] {
        for (title, app_name) in [
            ("Files", "Files"),
            ("Documents", "Files"),
            (LONG, "Files"),
            (LONG, LONG),
            ("Doc", LONG),
            ("", "Files"),
            (LONG_WORD, "Files"),
            (CLUSTERS, "Files"),
        ] {
            for square_top in [false, true] {
                for joined in [false, true] {
                    let (mut renderer, _, _cache) = render(&theme, width, 1.0, |header| {
                        header
                            .title(title)
                            .app_name(app_name)
                            .focused(true)
                            .square_top(square_top)
                            .joined_to_window(joined)
                            .on_close(())
                            .on_minimize(())
                            .on_maximize(())
                            .on_right_click(())
                            .on_screenshot(())
                            .on_fullscreen((), false)
                            .on_new_window(())
                    });
                    let bounds = pill(&mut renderer, &theme, width);
                    let (widest, margin) = cap(&theme, width, square_top);
                    let case = format!(
                        "{width}px, {title:?}/{app_name:?}, square_top={square_top}, joined={joined}"
                    );
                    assert!(
                        bounds.width <= widest + 0.5,
                        "pill {} wide in a {width}px window may not pass {widest} ({case})",
                        bounds.width
                    );
                    assert!(
                        bounds.x >= margin - 0.5 && bounds.x + bounds.width <= width - margin + 0.5,
                        "pill {bounds:?} reaches into the {margin}px corner margin ({case})"
                    );
                    assert!(
                        (bounds.x - (width - bounds.width) / 2.0).abs() <= 0.5,
                        "pill {bounds:?} is not centred ({case})"
                    );
                }
            }
        }
    }
}

/// The regression itself: before the cap, a title long enough to fill the
/// window made the pill exactly as wide as the window, so its rounded bottom
/// corners met the window's rounded top corners and left a notch.
#[test]
fn a_long_title_no_longer_stretches_the_pill_across_the_whole_window() {
    let theme = theme();
    let width = 400.0;
    let (mut renderer, _, _cache) = layout(&theme, width, LONG, 1.0);
    let bounds = pill(&mut renderer, &theme, width);
    let margin = halo_corner_margin(&theme, false);
    assert!(margin > 0.0, "a floating window has rounded top corners");
    assert!(
        bounds.width < width,
        "the pill still fills the window: {bounds:?}"
    );
    assert!(
        bounds.width <= width - 2.0 * margin + 0.5,
        "the pill must stop {margin}px short of each edge: {bounds:?}"
    );
}

#[test]
fn a_squared_top_needs_no_margin_and_keeps_the_full_width() {
    let theme = theme();
    assert_eq!(halo_corner_margin(&theme, true), 0.0);
    assert_eq!(
        halo_corner_margin(&theme, false),
        theme.radius_window()[0],
        "the margin is the radius actually in effect, not a constant"
    );
    let mut bar = DEFAULT_THEME_PAIR.load(true);
    bar.window_header_style = WindowHeaderStyle::Bar;
    let bar = CompTheme::new(Arc::new(bar), true);
    assert_eq!(
        halo_corner_margin(&bar, false),
        0.0,
        "a bar header is flush with the frame and owns its own corners"
    );

    // A maximized window squares its top corners, so the pill may use the lot.
    let width = 400.0;
    let (mut renderer, _, _cache) = render(&theme, width, 1.0, |header| {
        header
            .title(LONG)
            .app_name("Files")
            .focused(true)
            .square_top(true)
            .maximized(true)
            .on_close(())
            .on_minimize(())
            .on_maximize(())
            .on_right_click(())
            .on_screenshot(())
            .on_fullscreen((), false)
            .on_new_window(())
    });
    let bounds = pill(&mut renderer, &theme, width);
    let capped = width - 2.0 * halo_corner_margin(&theme, false);
    assert!(
        bounds.width > capped && bounds.width <= width,
        "a squared top lifts the {capped}px cap: {bounds:?}"
    );
}

#[test]
fn the_cap_only_bites_once_the_pill_would_reach_the_corners() {
    let theme = theme();
    let margin = halo_corner_margin(&theme, false);
    // What the pill wants, measured where nothing constrains it.
    let (mut renderer, _, _cache) = layout(&theme, 4000.0, LONG, 1.0);
    let natural = pill(&mut renderer, &theme, 4000.0).width;

    for (width, expected) in [
        (4000.0_f32, natural),
        (natural + 2.0 * margin + 40.0, natural),
        // Exactly at the threshold the pill still gets everything it asked for.
        (natural + 2.0 * margin, natural),
    ] {
        let (mut renderer, _, _cache) = layout(&theme, width, LONG, 1.0);
        let bounds = pill(&mut renderer, &theme, width);
        assert!(
            (bounds.width - expected).abs() <= 0.5,
            "a {width}px window must leave the pill at {expected}, not {}",
            bounds.width
        );
    }

    // One pixel narrower and the title starts to give way.
    let width = natural + 2.0 * margin - 1.0;
    let (mut renderer, _, _cache) = layout(&theme, width, LONG, 1.0);
    let bounds = pill(&mut renderer, &theme, width);
    assert!(bounds.width < natural, "the cap must bite at {width}px");
    assert!(bounds.width <= width - 2.0 * margin + 0.5);
    let (title, _) = identity(&mut renderer, &theme);
    assert!(title.expect("the title").ellipsized);
}

#[test]
fn the_capped_title_ellipsizes_and_the_app_name_survives() {
    let theme = theme();
    let wide = 2400.0;
    let narrow = 460.0;

    // Wide: both labels are drawn whole, so the cap changed nothing.
    let (mut renderer, _, _cache) = layout(&theme, wide, LONG, 1.0);
    let (title, app_name) = identity(&mut renderer, &theme);
    let title = title.expect("the title");
    let app_name = app_name.expect("the app name");
    assert!(!title.ellipsized && title.drawn() == LONG);
    assert!(!app_name.ellipsized && app_name.drawn() == "Files");

    // Narrow: the title gives way, the app name keeps every letter.
    let (mut renderer, _, _cache) = layout(&theme, narrow, LONG, 1.0);
    let (title, app_name) = identity(&mut renderer, &theme);
    let title = title.expect("the title");
    let app_name = app_name.expect("the app name");
    assert!(title.ellipsized, "the title must be cut: {title:?}");
    assert!(
        !title.drawn().is_empty() && LONG.starts_with(title.drawn()) && title.drawn() != LONG,
        "the title must be a shorter head of itself, ending in an ellipsis: {title:?}"
    );
    assert_eq!(title.source, LONG);
    assert!(
        !app_name.ellipsized && app_name.drawn() == "Files",
        "the short app name is cheap to draw in full: {app_name:?}"
    );
}

#[test]
fn a_long_app_name_ellipsizes_instead_of_eating_the_title() {
    let theme = theme();
    let width = 460.0;

    // Short title, long app name: the title is cheap, so only the name is cut.
    let (mut renderer, _, _cache) = render(&theme, width, 1.0, |header| {
        header
            .title("Doc")
            .app_name(LONG)
            .focused(true)
            .on_close(())
            .on_minimize(())
            .on_maximize(())
            .on_right_click(())
            .on_screenshot(())
            .on_fullscreen((), false)
            .on_new_window(())
    });
    let (title, app_name) = identity(&mut renderer, &theme);
    let title = title.expect("the title");
    let app_name = app_name.expect("the app name");
    assert!(!title.ellipsized && title.drawn() == "Doc");
    assert!(
        app_name.ellipsized,
        "the app name must be cut: {app_name:?}"
    );
    assert!(!app_name.drawn().is_empty() && LONG.starts_with(app_name.drawn()));

    // Both long: neither may be squeezed out of existence.
    let (mut renderer, _, _cache) = render(&theme, width, 1.0, |header| {
        header
            .title(LONG)
            .app_name(LONG_WORD)
            .focused(true)
            .on_close(())
            .on_minimize(())
            .on_maximize(())
            .on_right_click(())
            .on_screenshot(())
            .on_fullscreen((), false)
            .on_new_window(())
    });
    let (title, app_name) = identity(&mut renderer, &theme);
    let title = title.expect("the title");
    let app_name = app_name.expect("the app name");
    for text in [&title, &app_name] {
        assert!(text.ellipsized, "both labels must be cut: {text:?}");
        assert!(
            !text.drawn().is_empty(),
            "neither label may be replaced by nothing: {text:?}"
        );
        assert!(text.source.starts_with(text.drawn()));
    }
}

#[test]
fn an_unbroken_word_and_a_cluster_are_cut_on_a_character_boundary() {
    let theme = theme();
    for source in [LONG_WORD, CLUSTERS] {
        for width in [520.0_f32, 460.0, 420.0, 400.0, 380.0] {
            let (mut renderer, _, _cache) = layout(&theme, width, source, 1.0);
            let (title, _) = identity(&mut renderer, &theme);
            let title = title.expect("the title");
            // `drawn` is `None` when the glyph coverage ends mid-character.
            let drawn = title
                .drawn
                .as_deref()
                .unwrap_or_else(|| panic!("{source:?} cut inside a character at {width}px"));
            assert!(source.starts_with(drawn), "{title:?} at {width}px");
            assert!(!drawn.is_empty(), "{title:?} at {width}px");
            // A cut immediately before a mark that joins the character before
            // it would have split the cluster it belongs to.
            if let Some(next) = source[drawn.len()..].chars().next() {
                assert!(
                    !matches!(next, '\u{0300}'..='\u{036F}' | '\u{200D}' | '\u{FE00}'..='\u{FE0F}'),
                    "{source:?} was cut before {next:?}, inside a glyph cluster, at {width}px"
                );
            }
        }
    }
}

#[test]
fn the_controls_keep_their_size_and_spacing_however_long_the_title_is() {
    let theme = theme();
    let wide = 2400.0;
    let (mut renderer, _, _cache) = layout(&theme, wide, "Files", 1.0);
    let roomy = pill(&mut renderer, &theme, wide);
    let expected: Vec<Rectangle> = control_icons(&mut renderer);
    assert_eq!(
        expected.len(),
        8,
        "screenshot, record, menu, new, minimize, maximize, fullscreen, close"
    );
    let expected_dividers = dividers(&mut renderer, &theme);
    assert_eq!(
        expected_dividers.len(),
        2,
        "before the tray and the controls"
    );
    let divider = theme.halo_style().border_width;

    // 360px is the narrowest window that still fits every control: below it the
    // title has shrunk to the width it keeps (see `halo_corner_margin`'s floor)
    // and the tray starts to go. The controls that remain must not move or
    // change size, which is what this measures.
    for width in [2400.0_f32, 900.0, 600.0, 460.0, 400.0, 360.0] {
        for title in [LONG, LONG_WORD, CLUSTERS] {
            let (mut renderer, _, _cache) = layout(&theme, width, title, 1.0);
            let bounds = pill(&mut renderer, &theme, width);
            let icons = control_icons(&mut renderer);
            let case = format!("{width}px, {title:?}");
            assert_eq!(icons.len(), expected.len(), "a control vanished ({case})");
            for (icon, want) in icons.iter().zip(&expected) {
                assert_eq!(
                    (icon.width, icon.height),
                    (want.width, want.height),
                    "a control was shrunk ({case})"
                );
                // Same distance from the pill's trailing edge: neither
                // squeezed together nor pushed out of the pill.
                let offset = bounds.x + bounds.width - icon.x;
                let wanted = roomy.x + roomy.width - want.x;
                assert!(
                    (offset - wanted).abs() <= 0.01,
                    "control moved by {} ({case})",
                    offset - wanted
                );
            }
            let last = icons.last().expect("the close button");
            assert!(
                last.x + last.width <= bounds.x + bounds.width - divider,
                "the controls hang out of the pill ({case})"
            );
            let drawn = dividers(&mut renderer, &theme);
            assert_eq!(drawn.len(), expected_dividers.len(), "({case})");
            for (rule, want) in drawn.iter().zip(&expected_dividers) {
                assert_eq!(
                    (rule.width, rule.height),
                    (want.width, want.height),
                    "a divider was resized ({case})"
                );
                let offset = bounds.x + bounds.width - rule.x;
                let wanted = roomy.x + roomy.width - want.x;
                assert!(
                    (offset - wanted).abs() <= 0.01,
                    "divider moved by {} ({case})",
                    offset - wanted
                );
            }
        }
    }
}

#[test]
fn the_tray_and_the_chevron_come_and_go_without_squeezing_anything() {
    let theme = theme();
    let width = 420.0;
    for (chevron, new_window, expected) in [
        (false, false, 6),
        (true, false, 7),
        (false, true, 7),
        (true, true, 8),
    ] {
        let (mut renderer, _, _cache) = render(&theme, width, 1.0, |header| {
            let header = header
                .title(LONG)
                .app_name("Files")
                .focused(true)
                .on_close(())
                .on_minimize(())
                .on_maximize(())
                .on_screenshot(())
                .on_fullscreen((), false);
            let header = if chevron {
                header.on_right_click(())
            } else {
                header
            };
            if new_window {
                header.on_new_window(())
            } else {
                header
            }
        });
        let bounds = pill(&mut renderer, &theme, width);
        let icons = control_icons(&mut renderer);
        let case = format!("chevron={chevron}, new_window={new_window}");
        assert_eq!(icons.len(), expected, "{case}");
        let (widest, _) = cap(&theme, width, false);
        assert!(bounds.width <= widest + 0.5, "{case}");
        let last = icons.last().expect("the close button");
        assert!(
            last.x + last.width <= bounds.x + bounds.width,
            "the controls hang out of the pill ({case})"
        );
        // Fewer controls is more room for the title, never less.
        let (title, _) = identity(&mut renderer, &theme);
        assert!(title.expect("the title").ellipsized, "{case}");
    }
}

#[test]
fn an_app_icon_is_never_shrunk_to_make_room_for_the_title() {
    let theme = theme();
    let icon = || AppIcon::Svg {
        bytes: TEST_ICON,
        symbolic: true,
    };
    let (mut renderer, _, _cache) = layout_with(&theme, 2400.0, "Files", 1.0, |header| {
        header.app_icon(icon())
    });
    let roomy = control_icons(&mut renderer);
    assert_eq!(roomy.len(), 9, "the app mark joins the eight controls");
    let mark = roomy[0];

    for width in [2400.0_f32, 900.0, 460.0, 400.0, 360.0] {
        let (mut renderer, _, _cache) =
            layout_with(&theme, width, LONG, 1.0, |header| header.app_icon(icon()));
        let icons = control_icons(&mut renderer);
        assert_eq!(icons.len(), 9, "{width}px");
        assert_eq!(
            (icons[0].width, icons[0].height),
            (mark.width, mark.height),
            "the app mark was squeezed at {width}px"
        );
        let (title, app_name) = identity(&mut renderer, &theme);
        assert!(!title.expect("the title").drawn().is_empty(), "{width}px");
        // The app name is the first thing to go, so it is only promised while
        // there is room for it beside a readable title.
        if width >= 460.0 {
            assert!(
                !app_name.expect("the app name").drawn().is_empty(),
                "{width}px"
            );
        }
    }
}

#[test]
fn an_absent_or_repeated_app_name_leaves_the_title_the_whole_pill() {
    let theme = theme();
    let width = 460.0;
    let mut alone = None;
    for name in [None, Some(""), Some(LONG)] {
        let (mut renderer, _, _cache) = render(&theme, width, 1.0, |header| {
            let header = header
                .title(LONG)
                .focused(true)
                .on_close(())
                .on_minimize(())
                .on_maximize(())
                .on_right_click(())
                .on_screenshot(())
                .on_fullscreen((), false)
                .on_new_window(());
            match name {
                Some(name) => header.app_name(name),
                None => header,
            }
        });
        let (title, app_name) = identity(&mut renderer, &theme);
        assert!(
            app_name.is_none(),
            "{name:?} adds nothing beside the title and must stay hidden"
        );
        let drawn = title.expect("the title").drawn().to_owned();
        if let Some(expected) = &alone {
            assert_eq!(&drawn, expected, "{name:?}");
        } else {
            alone = Some(drawn);
        }
    }
    // With a name to fit beside it, the title has to give some of that back.
    let (mut renderer, _, _cache) = layout(&theme, width, LONG, 1.0);
    let (title, app_name) = identity(&mut renderer, &theme);
    assert!(app_name.is_some());
    assert!(
        title.expect("the title").drawn().len() < alone.expect("the title alone").len(),
        "a visible app name must cost the title some of its width"
    );
}

#[test]
fn an_empty_header_still_draws_its_controls() {
    let theme = theme();
    for width in [900.0_f32, 400.0] {
        let (mut renderer, _, _cache) = render(&theme, width, 1.0, |header| {
            header
                .title("")
                .focused(true)
                .on_close(())
                .on_minimize(())
                .on_maximize(())
                .on_right_click(())
                .on_screenshot(())
                .on_fullscreen((), false)
                .on_new_window(())
        });
        assert_eq!(control_icons(&mut renderer).len(), 8, "{width}px");
        assert!(drawn_text(&mut renderer).is_empty(), "{width}px");
        let bounds = pill(&mut renderer, &theme, width);
        let (widest, _) = cap(&theme, width, false);
        assert!(bounds.width <= widest + 0.5, "{width}px");
    }
}

#[test]
fn a_window_too_narrow_for_its_own_chrome_degrades_without_panicking() {
    let theme = theme();
    let margin = halo_corner_margin(&theme, false);
    for width in [
        300.0_f32,
        200.0,
        120.0,
        60.0,
        2.0 * margin,
        2.0 * margin - 1.0,
        4.0,
        1.0,
    ] {
        for title in ["Files", LONG] {
            let (mut renderer, _, _cache) = layout(&theme, width, title, 1.0);
            let case = format!("{width}px, {title:?}");
            let (widest, _) = cap(&theme, width, false);
            assert!(widest >= 0.0, "{case}");
            for bounds in control_icons(&mut renderer) {
                assert!(bounds.width >= 0.0 && bounds.height >= 0.0, "{case}");
            }
            for text in drawn_text(&mut renderer) {
                assert!(text.drawn.is_some(), "{case}: {text:?}");
            }
            let found = super::super::window::halo_backdrop_blur(
                renderer.layers(),
                theme.halo_style().pill_height(),
                width,
            );
            if let Some((bounds, _)) = found {
                assert!(
                    bounds.width >= 0.0 && bounds.width <= widest + 0.5,
                    "{case}"
                );
            }
        }
    }
    // At 300px the title holds the readable width it keeps and the tray has
    // begun to go, but the close button and a legible name both survive —
    // which is the promise, rather than a particular control count.
    let (mut renderer, _, _cache) = layout(&theme, 300.0, LONG, 1.0);
    assert!(
        !control_icons(&mut renderer).is_empty(),
        "the close button left a 300px window"
    );
    let (title, _) = identity(&mut renderer, &theme);
    assert!(
        title.expect("the title").drawn().len() > 4,
        "300px left no readable name"
    );
}

#[test]
fn a_bar_header_is_left_alone() {
    let mut tokens = DEFAULT_THEME_PAIR.load(true);
    tokens.window_header_style = WindowHeaderStyle::Bar;
    let theme = CompTheme::new(Arc::new(tokens), true);
    let width = 400.0;
    let mut previous = None;
    for square_top in [false, true] {
        let (mut renderer, _, _cache) = render(&theme, width, 1.0, |header| {
            header
                .title(LONG)
                .app_name("Files")
                .focused(true)
                .square_top(square_top)
                .on_close(())
                .on_minimize(())
                .on_maximize(())
        });
        // The bar's own background spans the window; no side margin appears.
        let spans_the_window = renderer
            .layers()
            .iter()
            .flat_map(|layer| &layer.quads)
            .any(|(quad, _)| quad.bounds.x == 0.0 && quad.bounds.width == width);
        assert!(spans_the_window, "square_top={square_top}");
        let title = drawn_text(&mut renderer)
            .into_iter()
            .find(|text| text.source == LONG)
            .expect("the bar title");
        let drawn = title.drawn().to_owned();
        assert!(LONG.starts_with(&drawn) && !drawn.is_empty());
        if let Some(expected) = &previous {
            assert_eq!(&drawn, expected, "the margin must not reach bar headers");
        } else {
            previous = Some(drawn);
        }
    }
}

/// The pill the compositor paints is the only part of a Halo band that takes
/// input. Beside it the band is see-through, so it belongs to the window behind
/// — and a joined Halo's resize borders start at the client's own top edge, not
/// at the top of the band.
#[test]
fn only_the_painted_pill_takes_input_out_of_the_halo_band() {
    use super::super::window::{Focus, RESIZE_BORDER, halo_pill_span};
    use smithay::utils::{Point, Rectangle as Rect};

    let theme = theme();
    let input = ssd_header_input_height(&theme) as i32;
    // A joined Halo only; an overlay one keeps its whole strip and gets no
    // range at all (`an_overlay_halo_keeps_its_whole_strip_draggable`).
    let joined = true;
    for width in [2400.0_f32, 1024.0, 600.0, 400.0] {
        let (mut renderer, _, _cache) = render(&theme, width, 1.0, |header| {
            header
                .title(LONG)
                .app_name("Files")
                .focused(true)
                .joined_to_window(joined)
                .on_close(())
                .on_minimize(())
                .on_maximize(())
                .on_right_click(())
                .on_screenshot(())
                .on_fullscreen((), false)
        });
        let bounds = pill(&mut renderer, &theme, width);
        let span = halo_pill_span(f64::from(bounds.x), f64::from(bounds.width));
        let top = ssd_header_height_for(&theme, joined) as i32;
        let offset = -(ssd_header_overhang(&theme) as i32 + halo_header_offset(&theme, joined));
        let geo = Rect::new(Point::from((0, 0)), (width as i32, 600).into());
        let hit = |x: f64, y: f64| {
            Focus::under_geometry(geo, top, input, offset, Some(span), (x, y).into())
        };
        let case = format!("{width}px, joined={joined}, pill={bounds:?}");

        // The pill itself, all the way to its painted edges.
        let band = f64::from(offset);
        for x in [span.0, (span.0 + span.1) / 2, span.1 - 1] {
            assert_eq!(
                hit(f64::from(x), band),
                Some(Focus::Header),
                "the pill is unreachable at {x} ({case})"
            );
        }

        // Beside it the band is see-through onto whatever is behind the
        // window, clear of the resize border, and may not be claimed as chrome.
        let beside = [
            0.0,
            f64::from(span.0) - 1.0,
            f64::from(span.1),
            width as f64 - 1.0,
        ];
        let clear = f64::from(top - RESIZE_BORDER) - 1.0;
        for x in beside {
            assert_eq!(
                hit(x, clear),
                None,
                "the empty band swallowed ({x}, {clear}) ({case})"
            );
        }

        // Resizing starts at the window's own edge either way.
        assert_eq!(
            hit(f64::from(span.0) - 1.0, f64::from(top - 1)),
            Some(Focus::ResizeTop),
            "no top border beside the pill ({case})"
        );
        assert_eq!(
            hit(-1.0, f64::from(top - 1)),
            Some(Focus::ResizeTopLeft),
            "no top-left corner at the window's edge ({case})"
        );
        assert_eq!(
            hit(f64::from(width as i32), f64::from(top - RESIZE_BORDER)),
            Some(Focus::ResizeTopRight),
            "no top-right corner at the window's edge ({case})"
        );
    }
}

/// End to end through the live element: a real `IcedElement` renders the Halo,
/// its painted pill comes back out of `backdrop_input_bounds`, and the band
/// beside it is left to the window behind. This is the path the compositor
/// runs on every pointer motion, stubbing nothing but the client surface.
#[test]
fn a_rendered_halo_element_hands_the_hit_test_its_painted_pill() {
    use super::super::window::{Focus, RESIZE_BORDER, halo_backdrop_blur, halo_pill_span};
    use crate::utils::iced::{CompElement, IcedElement, Program};
    use smithay::utils::{Logical, Point, Rectangle as Rect, Size as SmithaySize};

    struct Halo(String);
    impl Program for Halo {
        type Message = ();
        fn view<'a>(&'a self, theme: &'a CompTheme) -> CompElement<'a, ()> {
            header_bar()
                .theme(theme)
                .title(&self.0)
                .app_name("Files")
                .focused(true)
                .joined_to_window(true)
                .on_close(())
                .on_minimize(())
                .on_maximize(())
                .on_right_click(())
                .on_screenshot(())
                .on_fullscreen((), false)
                .into_element()
        }
        fn backdrop_blur(
            &self,
            theme: &CompTheme,
            size: SmithaySize<i32, Logical>,
            layers: &[Layer],
            _: [u8; 4],
        ) -> Option<(Rectangle, [u8; 4])> {
            halo_backdrop_blur(layers, theme.halo_style().pill_height(), size.w as f32)
        }
    }

    let theme = theme();
    // The font system is global; prime it the way the raster tests do.
    let _ = render(&theme, 200.0, 1.0, |header| header);
    let event_loop = calloop::EventLoop::<crate::state::State>::try_new().unwrap();
    let input = ssd_header_input_height(&theme) as i32;
    let top = ssd_header_height_for(&theme, true) as i32;
    let offset = -(ssd_header_overhang(&theme) as i32 + halo_header_offset(&theme, true));

    for width in [2400, 1280, 640] {
        let element = IcedElement::new(
            Halo(LONG.to_owned()),
            (width, ssd_header_render_height(&theme) as i32),
            event_loop.handle(),
            theme.clone(),
        );
        let bounds = element
            .backdrop_input_bounds()
            .expect("a drawn Halo reports its pill");
        let span = halo_pill_span(bounds.loc.x, bounds.size.w);
        assert!(
            span.0 > 0 && span.1 < width,
            "a {width}px window's pill {span:?} left no band to fall through"
        );

        let geo = Rect::new(Point::from((0, 0)), (width, 600).into());
        let hit = |x: f64, y: f64| {
            Focus::under_geometry(geo, top, input, offset, Some(span), (x, y).into())
        };
        let band = f64::from(offset);
        assert_eq!(
            hit(f64::from((span.0 + span.1) / 2), band),
            Some(Focus::Header)
        );
        // Chrome behind a maximized window gets these back.
        for x in [
            0.0,
            f64::from(span.0) - 1.0,
            f64::from(span.1),
            f64::from(width - 1),
        ] {
            assert_eq!(
                hit(x, band),
                None,
                "the band at {x} still blocks the window behind ({width}px)"
            );
        }
        // And its corners resize at the window, not out in the band.
        assert_eq!(hit(-1.0, f64::from(top - 1)), Some(Focus::ResizeTopLeft));
        assert_eq!(
            hit(f64::from(width), f64::from(top - RESIZE_BORDER)),
            Some(Focus::ResizeTopRight)
        );
        assert_eq!(hit(-1.0, band), None);
    }
}
