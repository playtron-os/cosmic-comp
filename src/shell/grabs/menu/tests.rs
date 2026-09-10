use super::*;
use iced_core::{Color, Font, Pixels, Size as IcedSize, renderer::Style};
use iced_graphics::Viewport;
use iced_runtime::{UserInterface, user_interface};
use icetron_themes::dynamic::DEFAULT_THEME_PAIR;

#[test]
fn halo_dropdown_is_transparent_outside_its_blurred_body_and_anchored_at_click() {
    let event_loop = calloop::EventLoop::<State>::try_new().unwrap();
    let theme = CompTheme::new(Arc::new(DEFAULT_THEME_PAIR.load(true)), true);
    let mut menu = ContextMenu::new(vec![
        Item::new("New Window", |_| {}),
        Item::Separator,
        Item::new("Screenshot", |_| {}),
        Item::new("Record — coming soon", |_| {}).disabled(true),
        Item::Separator,
        Item::new("Fullscreen", |_| {}),
        Item::new("Close window", |_| {}),
    ]);
    menu.halo = true;
    let iced = IcedElement::new(menu, Size::default(), event_loop.handle(), theme.clone());
    let size = iced.minimum_size();
    iced.resize(size);
    let padding = halo_menu_padding(&theme);
    let (blur, _) = iced
        .with_program(|p| p.backdrop_blur(&theme, size, &[], [0; 4]))
        .unwrap();
    assert_eq!(blur.width, theme.halo_style().menu_width);
    assert_eq!((blur.x, blur.y), (padding.left, padding.top));
    assert_eq!(blur.height, size.h as f32 - padding.top - padding.bottom);
    let mut pixels = tiny_skia::Pixmap::new(size.w as u32, size.h as u32).unwrap();
    iced.with_program(|menu| {
        assert_eq!(menu.background_color(&theme), Color::TRANSPARENT);
        let mut renderer = iced_tiny_skia::Renderer::new(Font::DEFAULT, Pixels(16.0));
        let mut ui = UserInterface::build(
            menu.view(&theme),
            IcedSize::new(size.w as f32, size.h as f32),
            user_interface::Cache::default(),
            &mut renderer,
        );
        let cursor = iced_core::mouse::Cursor::Unavailable;
        ui.update(
            &[iced_core::Event::Window(
                iced_core::window::Event::RedrawRequested(iced_core::time::Instant::now()),
            )],
            cursor,
            &mut renderer,
            &mut Vec::new(),
        );
        ui.draw(
            &mut renderer,
            &theme.to_iced_theme(),
            &Style::default(),
            cursor,
        );
        let viewport =
            Viewport::with_physical_size(IcedSize::new(size.w as u32, size.h as u32), 1.0);
        let mut mask = tiny_skia::Mask::new(size.w as u32, size.h as u32).unwrap();
        renderer.draw(
            &mut pixels.as_mut(),
            &mut mask,
            &viewport,
            &[IcedRectangle::with_size(viewport.logical_size())],
            Color::TRANSPARENT,
        );
    });
    assert_eq!(pixels.pixel(0, 0).unwrap().alpha(), 0);
    assert_eq!(
        pixels
            .pixel(size.w as u32 - 1, size.h as u32 - 1)
            .unwrap()
            .alpha(),
        0
    );
    if let Some(dir) = std::env::var_os("HALO_SNAPSHOT_DIR") {
        for pixel in pixels.data_mut().chunks_exact_mut(4) {
            pixel.swap(0, 2);
        }
        pixels
            .save_png(std::path::PathBuf::from(dir).join("halo-menu.png"))
            .unwrap();
    }
    let element = Element {
        iced,
        position: (100 - padding.left as i32, 200 - padding.top as i32).into(),
        pointer_entered: false,
        touch_entered: None,
    };
    let body = element.input_bbox();
    assert_eq!(
        body.loc,
        (100, 200).into(),
        "the menu body, not its shadow, starts at the saved click"
    );
    assert_eq!(body.size.w, theme.halo_style().menu_width as i32);
    assert!(
        !body.contains(Point::from((99, 200))),
        "shadow padding is an outside click"
    );
}
