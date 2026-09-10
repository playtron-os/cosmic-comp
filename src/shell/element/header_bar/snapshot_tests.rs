//! Raster regressions for the actual Tiny-Skia SSD widget tree.

use super::*;
use iced_core::{Color, Font, Pixels, Rectangle, Size, mouse, renderer::Style};
use iced_graphics::{Viewport, damage};
use iced_runtime::{UserInterface, user_interface};
use iced_tiny_skia::{Layer, Renderer};
use icetron_themes::dynamic::DEFAULT_THEME_PAIR;
use std::sync::Arc;

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
        .on_new_window(())
        .into_element();
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
