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
        .focused(true)
        .on_close(())
        .on_minimize(())
        .on_maximize(())
        .on_right_click(())
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
    let (mut current, _, _current_cache) = layout(&theme, 1024.0, "Explorer", 1.5);
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
