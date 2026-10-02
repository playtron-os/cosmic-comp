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
fn fill_keeps_its_glyph_and_label_in_every_state_and_emits_its_action() {
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
    let fill = crate::fl!("halo-fill");
    for (maximized, fullscreen) in [(false, false), (true, false), (false, true), (true, true)] {
        let expected = fill.as_str();
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
        // Fill is immediately left of the fullscreen button in this two-action header.
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
    theme.halo_style = super::design_halo_style();
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
                .tray(capture_tray(false))
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

/// Every glyph's tint, in draw order.
fn glyph_tints(renderer: &mut Renderer) -> Vec<Color> {
    renderer
        .layers()
        .iter()
        .flat_map(|layer| &layer.images)
        .filter_map(|image| match image {
            iced_graphics::Image::Vector { svg, .. } => svg.color,
            _ => None,
        })
        .collect()
}

#[test]
fn halo_controls_stay_neutral_when_the_workspace_accent_changes() {
    let mut theme = theme();
    let accent = Color::from_rgb(0.18, 0.62, 0.91);
    theme.workspace_accent = Some(accent);
    let (mut renderer, viewport, _cache) = layout(&theme, 1024.0, "Explorer", 1.0);
    let glyphs = glyph_tints(&mut renderer);
    assert_eq!(
        glyphs.len(),
        9,
        "app mark, screenshot, record, menu, new, park, fill, fullscreen, close"
    );
    for (index, color) in glyphs.into_iter().enumerate() {
        assert_eq!(
            color,
            if index == 0 {
                // The app mark belongs to the workspace, not the window's chrome.
                theme.halo_accent()
            } else {
                theme.text_secondary()
            },
            "every icon but the app mark rests in `--color-text-secondary`"
        );
    }
    if let Some(dir) = std::env::var_os("HALO_SNAPSHOT_DIR") {
        save(&mut renderer, &viewport, &dir, "halo-controls.png");
    }
}

/// Draw the whole header and save it as a PNG preview.
fn save(
    renderer: &mut Renderer,
    viewport: &Viewport,
    dir: &std::ffi::OsStr,
    name: &str,
) -> tiny_skia::Pixmap {
    let mut pixels =
        tiny_skia::Pixmap::new(viewport.physical_width(), viewport.physical_height()).unwrap();
    draw(
        renderer,
        viewport,
        &mut pixels,
        &[Rectangle::with_size(viewport.logical_size())],
    );
    let mut png = pixels.clone();
    // Tiny-Skia's iced renderer emits BGRA for the compositor's ARGB buffer;
    // PNG expects RGBA. This conversion is only for the saved preview.
    for pixel in png.data_mut().chunks_exact_mut(4) {
        pixel.swap(0, 2);
    }
    let dir = std::path::PathBuf::from(dir);
    std::fs::create_dir_all(&dir).unwrap();
    png.save_png(dir.join(name)).unwrap();
    pixels
}

#[test]
fn halo_record_glyph_turns_destructive_while_recording() {
    let theme = theme();
    let (mut renderer, _viewport, _cache) =
        layout_with(&theme, 1024.0, "Explorer", 1.0, |header| {
            header.tray(capture_tray(true))
        });
    let glyphs = glyph_tints(&mut renderer);
    assert_eq!(glyphs.len(), 9, "the dot is a quad, not a tenth glyph");
    for (index, color) in glyphs.into_iter().enumerate() {
        assert_eq!(
            color,
            match index {
                0 => theme.halo_accent(),
                2 => theme.feedback_error_primary(),
                _ => theme.text_secondary(),
            },
            "only the active Record glyph wears the destructive colour"
        );
    }
}

/// `.kora-halo__identity`: the app's name leads; the window's title follows as
/// the selection only when it says something the name does not.
#[test]
fn the_app_name_leads_and_the_window_title_follows_as_the_selection() {
    let theme = theme();
    let (mut same, _, _same_cache) = layout(&theme, 1024.0, "Files", 1.0);
    let (name, selection) = identity(&mut same, &theme);
    assert_eq!(name.expect("the name").source, "Files");
    assert!(
        selection.is_none(),
        "a title equal to the name is redundant"
    );

    let (mut different, _, _different_cache) = layout(&theme, 1024.0, "Documents", 1.0);
    let (name, selection) = identity(&mut different, &theme);
    assert_eq!(name.expect("the name").source, "Files");
    assert_eq!(selection.expect("the selection").source, "Documents");

    // Without an app name the window's title is all there is.
    let (mut nameless, _, _cache) = render(&theme, 1024.0, 1.0, |header| {
        header.title("Documents").focused(true).on_close(())
    });
    let (name, selection) = identity(&mut nameless, &theme);
    assert_eq!(name.expect("the title").source, "Documents");
    assert!(selection.is_none());
}

/// The label role for the name, the caption role for the selection.
#[test]
fn the_identity_uses_the_label_and_caption_roles() {
    let theme = theme();
    let (mut renderer, _, _cache) = layout(&theme, 1024.0, "Documents", 1.0);
    let sizes: Vec<(String, f32)> = renderer
        .layers()
        .iter()
        .flat_map(|layer| &layer.text)
        .flat_map(|item| item.as_slice())
        .filter_map(|text| match text {
            iced_graphics::text::Text::Paragraph { paragraph, .. } => {
                let paragraph = paragraph.upgrade()?;
                let buffer = paragraph.buffer();
                let source = buffer.lines.first()?.text().to_owned();
                Some((source, buffer.metrics().font_size))
            }
            _ => None,
        })
        .collect();
    let size = |wanted: &str| {
        sizes
            .iter()
            .find(|(source, _)| source == wanted)
            .map(|(_, size)| *size)
    };
    use icetron_themes::TextRole;
    assert_eq!(
        size("Files"),
        Some(theme.text_styles().role(TextRole::Label).font_size)
    );
    assert_eq!(
        size("Documents"),
        Some(theme.text_styles().role(TextRole::Caption).font_size)
    );
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

/// The drawn pill sits where every placement path leaves room for it: its
/// bottom 4px above the window, its top 36px above it, at any scale.
#[test]
fn the_drawn_pill_floats_4px_above_the_window() {
    let theme = theme();
    for scale in [1.0, 1.5, 2.0] {
        let (mut renderer, _, _cache) = layout(&theme, 1024.0, "Explorer", scale);
        let (pill, radii) = super::super::window::halo_backdrop_blur(
            renderer.layers(),
            theme.halo_style().pill_height(),
            1024.0,
        )
        .expect("actual header draw must contain the blur pill");
        let above = ssd_header_render_overhang(&theme) as f32;
        assert_eq!(pill.y - above, -36.0, "{scale}x");
        assert_eq!(pill.y + pill.height - above, -4.0, "{scale}x");
        assert_eq!(pill.height, 32.0, "{scale}x");
        assert_eq!(radii, [16; 4], "round at both ends ({scale}x)");
        let (quad, _) = renderer
            .layers()
            .iter()
            .flat_map(|layer| &layer.quads)
            .find(|(quad, _)| quad.bounds == pill && quad.border.width > 0.0)
            .expect("the Halo's own border quad");
        assert_eq!(quad.border.sides, None, "a border on all four sides");
    }
    assert_eq!(
        ssd_header_height(&theme),
        0,
        "nothing is reserved in the window"
    );
}

// ---------------------------------------------------------------------------
// Identity and overflow tiers.
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

/// The name and, when it is shown, the selection beside it.
fn identity(renderer: &mut Renderer, theme: &CompTheme) -> (Option<DrawnText>, Option<DrawnText>) {
    let drawn = drawn_text(renderer);
    let by_color = |wanted: Color| drawn.iter().find(|text| text.color == wanted).cloned();
    (
        by_color(theme.text_primary()),
        by_color(theme.text_secondary()),
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

/// The hairline dividers between the identity, the app side and the window side.
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

/// A header carrying every control there is.
fn everything<'a>(header: HeaderBar<'a, ()>) -> HeaderBar<'a, ()> {
    header
        .title(LONG)
        .app_name("Files")
        .focused(true)
        .on_close(())
        .on_minimize(())
        .on_maximize(())
        .on_right_click(())
        .on_commands(())
        .tray(capture_tray(false))
        .on_fullscreen((), false)
        .on_new_window(())
}

/// `halo.css` `[data-tier]`: tier 2 folds the inactive pins away, tier 3 the
/// ⌄, `+` and window verbs behind ⋯, tier 4 the dividers as well.
#[test]
fn the_overflow_tiers_shed_what_the_design_sheds() {
    let theme = theme();
    for (width, glyphs, divider_count, case) in [
        (
            1200.0_f32,
            9,
            2,
            "tier 1: mark, 2 pins, ⌄, +, park, fill, fullscreen, close",
        ),
        (680.0, 9, 2, "tier 1 at its threshold"),
        (
            600.0,
            7,
            1,
            "tier 2: mark, ⌄, +, park, fill, fullscreen, close",
        ),
        (480.0, 7, 1, "tier 2 at its threshold"),
        (400.0, 3, 1, "tier 3: mark, ⋯, close"),
        (280.0, 3, 1, "tier 3 at its threshold"),
        (240.0, 3, 0, "tier 4: mark, ⋯, close, no dividers"),
    ] {
        let (mut renderer, viewport, _cache) = render(&theme, width, 1.0, |header| {
            everything(header).window_width(width)
        });
        assert_eq!(
            control_icons(&mut renderer).len(),
            glyphs,
            "{width}px {case}"
        );
        assert_eq!(
            dividers(&mut renderer, &theme).len(),
            divider_count,
            "{width}px {case}"
        );
        let bounds = pill(&mut renderer, &theme, width);
        assert!(
            bounds.x >= 0.0 && bounds.x + bounds.width <= width + 0.5,
            "{width}px: the pill {bounds:?} leaves the window"
        );
        if let Some(dir) = std::env::var_os("HALO_SNAPSHOT_DIR") {
            save(
                &mut renderer,
                &viewport,
                &dir,
                &format!("halo-tier-{width}.png"),
            );
        }
    }
    // An active capture stays out at every tier.
    for width in [600.0_f32, 400.0, 240.0] {
        let (mut renderer, _, _cache) = render(&theme, width, 1.0, |header| {
            everything(header)
                .tray(capture_tray(true))
                .window_width(width)
        });
        let tints = glyph_tints(&mut renderer);
        assert_eq!(
            tints
                .iter()
                .filter(|tint| **tint == theme.feedback_error_primary())
                .count(),
            1,
            "{width}px keeps the recording pin"
        );
    }
    // A secondary panel takes tier 4 at any width.
    let (mut renderer, _, _cache) = render(&theme, 1200.0, 1.0, |header| {
        everything(header).window_width(1200.0).panel(true)
    });
    assert_eq!(control_icons(&mut renderer).len(), 3);
    assert!(dividers(&mut renderer, &theme).is_empty());
}

/// ⋯ is the route to every shed verb: it opens the window's commands.
#[test]
fn more_opens_the_commands_once_the_verbs_have_shed() {
    use iced_core::{Event, Point, mouse::Cursor};
    #[derive(Debug, Clone, PartialEq)]
    enum Message {
        Commands,
        Close,
    }
    let theme = theme();
    let mut renderer = Renderer::new(Font::DEFAULT, Pixels(16.0));
    let header = header_bar()
        .theme(&theme)
        .title("Files")
        .focused(true)
        .window_width(400.0)
        .on_commands(Message::Commands)
        .on_close(Message::Close)
        .into_element();
    let size = Size::new(400.0, ssd_header_render_height(&theme) as f32);
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
    let icons = control_icons(&mut renderer);
    assert_eq!(icons.len(), 3, "mark, ⋯, close");
    let more = icons[1];
    let point = Point::new(more.center_x(), more.center_y());
    let mut messages = Vec::new();
    for event in [
        Event::Mouse(mouse::Event::CursorMoved { position: point }),
        Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)),
        Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left)),
    ] {
        ui.update(
            &[event],
            Cursor::Available(point),
            &mut renderer,
            &mut messages,
        );
    }
    assert_eq!(messages, vec![Message::Commands]);
}

/// A window whose title is long and whose app name is unknown — an Android
/// emulator is the case this came from — still keeps its controls and a
/// readable name at every tier.
#[test]
fn a_lone_long_title_never_pushes_the_controls_out() {
    let theme = theme();
    let title = "Android Emulator - Pixel7_API36_1:5554";
    for width in [1200.0_f32, 604.0, 460.0, 400.0, 340.0, 302.0, 260.0] {
        let (mut renderer, _, _cache) = render(&theme, width, 1.0, |header| {
            header
                .title(title)
                .focused(true)
                .window_width(width)
                .on_close(())
                .on_minimize(())
                .on_maximize(())
                .on_right_click(())
                .on_commands(())
                .tray(capture_tray(false))
                .on_fullscreen((), false)
                .on_new_window(())
        });
        let bounds = pill(&mut renderer, &theme, width);
        let icons = control_icons(&mut renderer);
        let close = icons.last().expect("the close button");
        assert!(
            close.x + close.width <= bounds.x + bounds.width,
            "{width}px: close hangs out of the pill"
        );
        let drawn = identity(&mut renderer, &theme).0.expect("the title");
        assert!(drawn.drawn().len() > 4, "{width}px left no readable name");
        assert!(drawn.ellipsized, "{width}px: 144px caps the name");
    }
}

/// `.kora-halo__title` caps the name at 144px, `.kora-halo__sub` the selection
/// at 128px, and `.kora-halo__identity` the two together at 192px.
#[test]
fn the_identity_keeps_the_design_caps() {
    let theme = theme();
    let width = 2400.0;
    let bare = {
        let (mut renderer, _, _cache) = render(&theme, width, 1.0, |header| {
            everything(header).title("").app_name("")
        });
        pill(&mut renderer, &theme, width).width
    };
    for (name, title) in [
        ("Files", LONG),
        (LONG, "Doc"),
        (LONG, LONG),
        (LONG_WORD, CLUSTERS),
    ] {
        let (mut renderer, _, _cache) = render(&theme, width, 1.0, |header| {
            everything(header).title(title).app_name(name)
        });
        let extra = pill(&mut renderer, &theme, width).width - bare;
        let gap = theme.halo_style().gap;
        assert!(
            extra <= 192.0 + gap + 1.0,
            "{name:?}/{title:?}: the identity took {extra}px"
        );
        let (name_drawn, selection) = identity(&mut renderer, &theme);
        let name_drawn = name_drawn.expect("the name");
        assert_eq!(name_drawn.ellipsized, name.len() > 8, "{name:?}");
        if let Some(selection) = selection {
            assert!(
                selection.ellipsized || selection.source.len() <= 3,
                "{title:?}"
            );
            assert!(!selection.drawn().is_empty());
        }
    }
}

#[test]
fn an_unbroken_word_and_a_cluster_are_cut_on_a_character_boundary() {
    let theme = theme();
    for source in [LONG_WORD, CLUSTERS] {
        for (width, as_name) in [
            (1200.0_f32, false),
            (700.0, false),
            (400.0, true),
            (240.0, true),
        ] {
            let (mut renderer, _, _cache) = render(&theme, width, 1.0, |header| {
                let header = everything(header).window_width(width);
                if as_name {
                    header.title("Doc").app_name(source)
                } else {
                    header.title(source).app_name("Files")
                }
            });
            let (name, selection) = identity(&mut renderer, &theme);
            let text = if as_name { name } else { selection }.expect("the label");
            // `drawn` is `None` when the glyph coverage ends mid-character.
            let drawn = text
                .drawn
                .as_deref()
                .unwrap_or_else(|| panic!("{source:?} cut inside a character at {width}px"));
            assert!(source.starts_with(drawn), "{text:?} at {width}px");
            assert!(!drawn.is_empty(), "{text:?} at {width}px");
            if let Some(next) = source[drawn.len()..].chars().next() {
                assert!(
                    !matches!(next, '\u{0300}'..='\u{036F}' | '\u{200D}' | '\u{FE00}'..='\u{FE0F}'),
                    "{source:?} was cut before {next:?}, inside a glyph cluster, at {width}px"
                );
            }
        }
    }
}

/// Within a tier the controls never move or shrink, however long the title.
#[test]
fn the_controls_keep_their_size_and_spacing_however_long_the_title_is() {
    let theme = theme();
    let (mut renderer, _, _cache) = render(&theme, 2400.0, 1.0, |header| {
        everything(header).title("Files").window_width(2400.0)
    });
    let roomy = pill(&mut renderer, &theme, 2400.0);
    let expected: Vec<Rectangle> = control_icons(&mut renderer).split_off(1);
    assert_eq!(expected.len(), 8);
    let expected_dividers = dividers(&mut renderer, &theme);
    assert_eq!(expected_dividers.len(), 2);
    for width in [2400.0_f32, 900.0, 700.0] {
        for title in [LONG, LONG_WORD, CLUSTERS] {
            let (mut renderer, _, _cache) = render(&theme, width, 1.0, |header| {
                everything(header).title(title).window_width(width)
            });
            let bounds = pill(&mut renderer, &theme, width);
            let icons = control_icons(&mut renderer).split_off(1);
            let case = format!("{width}px, {title:?}");
            assert_eq!(icons.len(), expected.len(), "a control vanished ({case})");
            for (icon, want) in icons.iter().zip(&expected) {
                assert_eq!(
                    (icon.width, icon.height),
                    (want.width, want.height),
                    "a control was shrunk ({case})"
                );
                let offset = bounds.x + bounds.width - icon.x;
                let wanted = roomy.x + roomy.width - want.x;
                assert!((offset - wanted).abs() <= 0.01, "control moved ({case})");
            }
            let drawn = dividers(&mut renderer, &theme);
            assert_eq!(drawn.len(), expected_dividers.len(), "({case})");
        }
    }
}

#[test]
fn the_tray_and_the_chevron_come_and_go_without_squeezing_anything() {
    let theme = theme();
    let width = 700.0;
    for (chevron, new_window, expected) in [
        (false, false, 7),
        (true, false, 8),
        (false, true, 8),
        (true, true, 9),
    ] {
        let (mut renderer, _, _cache) = render(&theme, width, 1.0, |header| {
            let header = header
                .title(LONG)
                .app_name("Files")
                .focused(true)
                .window_width(width)
                .on_close(())
                .on_minimize(())
                .on_maximize(())
                .tray(capture_tray(false))
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
        let last = icons.last().expect("the close button");
        assert!(
            last.x + last.width <= bounds.x + bounds.width,
            "the controls hang out of the pill ({case})"
        );
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
    let mark = control_icons(&mut renderer)[0];
    assert_eq!((mark.width, mark.height), (14.0, 14.0), "a 14px mark");
    for width in [2400.0_f32, 900.0, 460.0, 400.0, 260.0] {
        let (mut renderer, _, _cache) = layout_with(&theme, width, LONG, 1.0, |header| {
            header.app_icon(icon()).window_width(width)
        });
        let icons = control_icons(&mut renderer);
        assert_eq!(
            (icons[0].width, icons[0].height),
            (mark.width, mark.height),
            "the app mark was squeezed at {width}px"
        );
        let (name, _) = identity(&mut renderer, &theme);
        assert!(!name.expect("the name").drawn().is_empty(), "{width}px");
    }
}

#[test]
fn an_absent_or_repeated_app_name_shows_the_title_alone() {
    let theme = theme();
    for name in [None, Some(""), Some(LONG)] {
        let (mut renderer, _, _cache) = render(&theme, 1200.0, 1.0, |header| {
            let header = header
                .title(LONG)
                .focused(true)
                .window_width(1200.0)
                .on_close(())
                .tray(capture_tray(false));
            match name {
                Some(name) => header.app_name(name),
                None => header,
            }
        });
        let (title, selection) = identity(&mut renderer, &theme);
        assert!(
            selection.is_none(),
            "{name:?} adds nothing beside the title"
        );
        assert_eq!(title.expect("the title").source, LONG, "{name:?}");
    }
}

#[test]
fn an_empty_header_still_draws_its_controls() {
    let theme = theme();
    for (width, glyphs) in [(900.0_f32, 9), (400.0, 3)] {
        let (mut renderer, _, _cache) = render(&theme, width, 1.0, |header| {
            everything(header)
                .title("")
                .app_name("")
                .window_width(width)
        });
        // The mark is drawn even with no title: it is the route to the
        // window's commands, so it cannot depend on there being text.
        assert_eq!(control_icons(&mut renderer).len(), glyphs, "{width}px");
        assert!(drawn_text(&mut renderer).is_empty(), "{width}px");
    }
}

#[test]
fn a_window_too_narrow_for_its_own_chrome_degrades_without_panicking() {
    let theme = theme();
    for width in [300.0_f32, 200.0, 120.0, 60.0, 28.0, 4.0, 1.0] {
        for title in ["Files", LONG] {
            let (mut renderer, _, _cache) = render(&theme, width, 1.0, |header| {
                everything(header).title(title).window_width(width)
            });
            let case = format!("{width}px, {title:?}");
            for bounds in control_icons(&mut renderer) {
                assert!(bounds.width >= 0.0 && bounds.height >= 0.0, "{case}");
            }
            for text in drawn_text(&mut renderer) {
                assert!(text.drawn.is_some(), "{case}: {text:?}");
            }
        }
    }
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
            assert_eq!(&drawn, expected, "the corners must not reach bar headers");
        } else {
            previous = Some(drawn);
        }
    }
}

/// End to end through the live element: a real `IcedElement` renders the Halo,
/// its painted pill comes back out of `backdrop_input_bounds`, and the band
/// beside it is left to the window behind. This is the path the compositor
/// runs on every pointer motion, stubbing nothing but the client surface.
#[test]
fn a_rendered_halo_element_hands_the_hit_test_its_painted_pill() {
    use super::super::window::{Focus, HeaderBand, halo_backdrop_blur, halo_pill_span};
    use crate::utils::iced::{CompElement, IcedElement, Program};
    use smithay::utils::{Logical, Point, Rectangle as Rect, Size as SmithaySize};

    struct Halo(String);
    impl Program for Halo {
        type Message = ();
        fn view<'a>(&'a self, theme: &'a CompTheme) -> CompElement<'a, ()> {
            everything(header_bar().theme(theme))
                .title(&self.0)
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
        // The pill the element reports is the pill the hit test assumes.
        let above = ssd_header_render_overhang(&theme) as f64;
        assert_eq!(bounds.loc.y - above, -36.0);
        assert_eq!(bounds.loc.y + bounds.size.h - above, -4.0);

        let geo = Rect::new(Point::from((0, 0)), (width, 600).into());
        let band = HeaderBand::Halo(HaloBand::new(&theme, Some(span), false));
        let hit = |x: f64, y: f64| Focus::under_geometry(geo, band, (x, y).into());
        assert_eq!(
            hit(f64::from((span.0 + span.1) / 2), -20.0),
            Some(Focus::Header)
        );
        for x in [
            0.0,
            f64::from(span.0) - 1.0,
            f64::from(span.1),
            f64::from(width - 1),
        ] {
            assert_eq!(
                hit(x, -20.0),
                None,
                "the band at {x} still blocks the window behind ({width}px)"
            );
        }
        assert_eq!(hit(-1.0, -2.0), Some(Focus::ResizeTopLeft));
        assert_eq!(hit(10.0, -2.0), Some(Focus::Header), "the bridge");
    }
}

/// The app glyph is the route to a window's commands, so it must answer a click
/// whether or not the window's own mark ever resolved.
#[test]
fn the_app_glyph_answers_a_click_with_or_without_a_resolved_mark() {
    use iced_core::{Event, Point, mouse::Cursor};

    let theme = theme();
    for icon in [
        None,
        Some(AppIcon::Svg {
            bytes: TEST_ICON,
            symbolic: true,
        }),
    ] {
        let mut renderer = Renderer::new(Font::DEFAULT, Pixels(16.0));
        let mut header = header_bar()
            .theme(&theme)
            .title("Commands target")
            .focused(true)
            .on_close(())
            .on_commands(());
        if let Some(icon) = icon.clone() {
            header = header.app_icon(icon);
        }
        let size = Size::new(900.0, ssd_header_render_height(&theme) as f32);
        let mut ui = UserInterface::build(
            header.into_element(),
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
        // The mark is the pill's leading glyph, whatever else it carries.
        let mark = control_icons(&mut renderer)
            .into_iter()
            .next()
            .expect("the app mark is always drawn");
        let point = Point::new(mark.center_x(), mark.center_y());
        let mut messages = Vec::new();
        for event in [
            Event::Mouse(mouse::Event::CursorMoved { position: point }),
            Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)),
            Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left)),
        ] {
            ui.update(
                &[event],
                Cursor::Available(point),
                &mut renderer,
                &mut messages,
            );
        }
        assert_eq!(messages, vec![()], "resolved mark: {}", icon.is_some());
    }
}

/// The tray is whatever is pinned, in pin order — not a fixed pair.
#[test]
fn the_tray_draws_the_pins_it_is_given_in_order() {
    use crate::shell::element::header_bar::TrayEntry;

    let theme = theme();
    let entry = |icon| TrayEntry {
        icon,
        message: (),
        label: String::new(),
        on: false,
    };
    for pins in [
        vec![],
        vec![entry(icons::MINUS)],
        vec![
            entry(icons::MINUS),
            entry(icons::PLUS),
            entry(icons::CAMERA),
        ],
    ] {
        let count = pins.len();
        let (mut renderer, _, _cache) = render(&theme, 1200.0, 1.0, |header| {
            header
                .title("Files")
                .focused(true)
                .on_close(())
                .tray(pins.clone())
        });
        // The app mark, the pins, and close.
        assert_eq!(
            control_icons(&mut renderer).len(),
            count + 2,
            "{count} pins"
        );
        // The divider only exists to separate the identity from the pins.
        assert_eq!(
            dividers(&mut renderer, &theme).len(),
            usize::from(count > 0) + 1,
            "{count} pins"
        );
    }
}

/// The outline shader draws the pill's edge, so the texture must not: icetron
/// mixes its own edge from the border and the accent, and the hairline that
/// left behind streaked straight past the corner at 2x.
#[test]
fn a_compositor_outlined_pill_has_no_edge_of_its_own() {
    let theme = theme();
    let (mut renderer, viewport, _cache) = layout_with(&theme, 423.0, "Explorer", 2.0, |header| {
        header.compositor_outline(true)
    });
    let (pill, _) = super::super::window::halo_backdrop_blur(
        renderer.layers(),
        theme.halo_style().pill_height(),
        423.0,
    )
    .expect("the drawn Halo pill");
    let mut pixels =
        tiny_skia::Pixmap::new(viewport.physical_width(), viewport.physical_height()).unwrap();
    draw(
        &mut renderer,
        &viewport,
        &mut pixels,
        &[Rectangle::with_size(viewport.logical_size())],
    );
    let top = (pill.y * 2.0).round() as u32;
    let left = ((pill.x + 20.0) * 2.0) as u32;
    let right = ((pill.x + pill.width - 20.0) * 2.0) as u32;
    for row in top.saturating_sub(1)..=top + 2 {
        for x in left..right {
            let px = pixels.pixel(x, row).unwrap();
            assert_eq!(
                (px.red(), px.green(), px.blue()),
                (0, 0, 0),
                "the texture draws an edge at ({x}, {row})"
            );
        }
    }
}
