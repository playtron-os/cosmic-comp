use super::*;
use iced_core::{Color, Font, Pixels, Size as IcedSize, renderer::Style};
use iced_graphics::Viewport;
use iced_runtime::{UserInterface, user_interface};
use icetron_themes::dynamic::DEFAULT_THEME_PAIR;

fn exit_test_grab(
    seat: &Seat<State>,
    handle: LoopHandle<'static, State>,
    halo: bool,
    animated: bool,
) -> MenuGrab {
    let mut theme = CompTheme::default();
    theme.motion.layer_open = std::time::Duration::ZERO;
    let mut menu = ContextMenu::new(vec![Item::new("New window", |_| {})]);
    menu.halo = halo;
    // Start settled, then enable a long exit: lifecycle tests need no GPU,
    // sleeping, or dependence on how quickly the machine runs the test.
    let iced = IcedElement::new(menu, (400, 200), handle, theme.clone());
    if animated {
        theme.motion.layer_open = std::time::Duration::from_secs(60);
        iced.set_theme(theme);
    }
    let elements = Arc::new(Mutex::new(vec![Element {
        iced,
        position: (100, 200).into(),
        pointer_entered: true,
        touch_entered: Some(TouchSlot::from(Some(0))),
    }]));
    let scale = Arc::new(Mutex::new(1.0));
    seat.user_data()
        .insert_if_missing_threadsafe(SeatMenuGrabState::default);
    *seat
        .user_data()
        .get::<SeatMenuGrabState>()
        .unwrap()
        .lock()
        .unwrap() = Some(MenuGrabState {
        elements: elements.clone(),
        screen_space_relative: None,
        scale: scale.clone(),
    });
    MenuGrab {
        elements,
        scale,
        screen_space_relative: None,
        seat: seat.clone(),
        on_close: None,
        start_data: GrabStartData::Pointer(PointerGrabStartData {
            focus: None,
            button: 0x110,
            location: (100.0, 200.0).into(),
        }),
    }
}

#[test]
fn dismissed_menu_releases_input_and_callback_before_releasing_its_image() {
    use std::sync::atomic::AtomicUsize;
    let event_loop = calloop::EventLoop::<State>::try_new().unwrap();
    let mut seats = smithay::input::SeatState::<State>::new();
    for (halo, animated) in [(true, true), (true, false), (false, true)] {
        let seat = seats.new_seat("menu-exit");
        let called = Arc::new(AtomicUsize::new(0));
        let counter = called.clone();
        let grab = exit_test_grab(&seat, event_loop.handle(), halo, animated).on_close(move || {
            counter.fetch_add(1, Ordering::SeqCst);
        });
        let image = Arc::downgrade(&grab.elements);
        drop(grab);
        assert_eq!(called.load(Ordering::SeqCst), 1);
        assert!(
            seat.user_data()
                .get::<SeatMenuGrabState>()
                .unwrap()
                .lock()
                .unwrap()
                .is_none()
        );
        if halo && animated {
            assert!(
                image.upgrade().is_some(),
                "retain the painted menu during exit"
            );
            let mut closing = seat
                .user_data()
                .get::<SeatClosingMenus>()
                .unwrap()
                .lock()
                .unwrap();
            assert_eq!(closing.0.len(), 1);
            {
                let elements = closing.0[0].elements.lock().unwrap();
                assert!(!elements[0].pointer_entered);
                assert!(elements[0].touch_entered.is_none());
                assert!(
                    elements[0]
                        .iced
                        .with_program(|menu| menu.closing.load(Ordering::SeqCst))
                );
                assert!(!closing.0[0].is_in_screen_space());
            }
            // Cleanup must work even if that output never drew another frame.
            closing.cleanup(iced_core::time::Instant::now() + std::time::Duration::from_secs(61));
            assert!(closing.0.is_empty());
        }
        assert!(
            image.upgrade().is_none(),
            "release buffers after exit (immediately for legacy/zero motion)"
        );
        assert_eq!(
            called.load(Ordering::SeqCst),
            1,
            "render cleanup must not call on_close again"
        );
    }
}

#[test]
fn dropping_a_replaced_grab_cannot_remove_the_new_popup() {
    let event_loop = calloop::EventLoop::<State>::try_new().unwrap();
    let mut seats = smithay::input::SeatState::<State>::new();
    let seat = seats.new_seat("replace-menu");
    let old = exit_test_grab(&seat, event_loop.handle(), true, true);
    let new = exit_test_grab(&seat, event_loop.handle(), true, true);
    drop(old);
    {
        let active = seat
            .user_data()
            .get::<SeatMenuGrabState>()
            .unwrap()
            .lock()
            .unwrap();
        assert!(Arc::ptr_eq(
            &active.as_ref().unwrap().elements,
            &new.elements
        ));
        assert!(
            new.elements.lock().unwrap()[0]
                .iced
                .with_program(|menu| !menu.closing.load(Ordering::SeqCst))
        );
    }
    assert_eq!(
        seat.user_data()
            .get::<SeatClosingMenus>()
            .unwrap()
            .lock()
            .unwrap()
            .0
            .len(),
        1
    );
    drop(new);
    assert!(
        seat.user_data()
            .get::<SeatMenuGrabState>()
            .unwrap()
            .lock()
            .unwrap()
            .is_none()
    );
    let mut closing = seat
        .user_data()
        .get::<SeatClosingMenus>()
        .unwrap()
        .lock()
        .unwrap();
    assert_eq!(closing.0.len(), 2);
    closing.cleanup(iced_core::time::Instant::now() + std::time::Duration::from_secs(61));
    assert!(closing.0.is_empty());
}

#[test]
fn halo_dropdown_opts_into_surface_motion_but_legacy_menus_do_not() {
    let theme = CompTheme::default();
    let mut menu = ContextMenu::new(vec![Item::new("New window", |_| {})]);
    assert!(menu.visibility(&theme).is_none());
    menu.halo = true;
    assert_eq!(
        menu.visibility(&theme),
        Some(crate::utils::iced::Visibility::fade_rise(theme.motion))
    );
}

#[test]
fn halo_menu_hover_updates_seat_cursor_for_enabled_rows_only() {
    use iced_core::{
        Event, Point as IcedPoint, mouse,
        widget::{Id, Operation},
    };
    use smithay::input::{SeatState, pointer::CursorIcon};

    #[derive(Default)]
    struct Labels(std::collections::HashMap<String, IcedRectangle>);
    impl Operation for Labels {
        fn traverse(&mut self, operate: &mut dyn FnMut(&mut dyn Operation)) {
            operate(self);
        }
        fn text(&mut self, _: Option<&Id>, bounds: IcedRectangle, text: &str) {
            self.0.insert(text.to_owned(), bounds);
        }
    }

    // Deliberately no pointer device: publishing the cursor must not query or
    // re-lock the pointer handle whose grab is currently dispatching the event.
    let mut seats = SeatState::<State>::new();
    let seat = seats.new_seat("halo-menu-test");
    seat.user_data()
        .insert_if_missing_threadsafe(|| Mutex::new(CursorImageStatus::default_named()));
    seat.user_data()
        .insert_if_missing_threadsafe(CursorState::default);
    let event_loop = calloop::EventLoop::<State>::try_new().unwrap();
    let theme = CompTheme::new(Arc::new(DEFAULT_THEME_PAIR.load(true)), true);
    let mut menu = ContextMenu::new(vec![
        Item::new("Enabled", |_| {}),
        Item::Separator,
        Item::new("Disabled", |_| {}).disabled(true),
    ]);
    menu.halo = true;
    let element = IcedElement::new(menu, Size::default(), event_loop.handle(), theme.clone());
    let size = element.minimum_size();
    element.with_program(|menu| {
        let mut renderer = iced_tiny_skia::Renderer::new(Font::DEFAULT, Pixels(16.0));
        let mut ui = UserInterface::build(menu.view(&theme), IcedSize::new(size.w as f32, size.h as f32),
            user_interface::Cache::default(), &mut renderer);
        let mut labels = Labels::default();
        ui.operate(&renderer, &mut labels);
        let enabled = labels.0["Enabled"];
        let disabled = labels.0["Disabled"];
        let separator = IcedPoint::new(enabled.center_x(), (enabled.y + enabled.height + disabled.y) / 2.0);
        for (point, expected) in [
            (enabled.center(), CursorIcon::Pointer),
            (disabled.center(), CursorIcon::Default),
            (enabled.center(), CursorIcon::Pointer),
            (separator, CursorIcon::Default),
            (enabled.center(), CursorIcon::Pointer),
            (IcedPoint::new(-1.0, -1.0), CursorIcon::Default),
        ] {
            let (state, _) = ui.update(&[
                Event::Mouse(mouse::Event::CursorMoved { position: point }),
                Event::Window(iced_core::window::Event::RedrawRequested(iced_core::time::Instant::now())),
            ], mouse::Cursor::Available(point), &mut renderer, &mut Vec::new());
            let user_interface::State::Updated { mouse_interaction, .. } = state else {
                panic!("menu hover unexpectedly invalidated the UI");
            };
            set_menu_cursor(&seat, Some(mouse_interaction));
            assert!(matches!(seat.cursor_image_status(), CursorImageStatus::Named(icon) if icon == expected),
                "wrong cursor over {point:?}: {:?}", seat.cursor_image_status());
        }
    });
    set_menu_cursor(&seat, Some(Interaction::Pointer));
    set_menu_cursor(&seat, None);
    assert!(
        matches!(
            seat.cursor_image_status(),
            CursorImageStatus::Named(CursorIcon::Default)
        ),
        "releasing the menu grab must restore the default cursor"
    );
}

#[test]
fn halo_dropdown_is_transparent_outside_its_blurred_body_and_anchored_at_click() {
    let event_loop = calloop::EventLoop::<State>::try_new().unwrap();
    let mut theme = CompTheme::new(Arc::new(DEFAULT_THEME_PAIR.load(true)), true);
    theme.motion.layer_open = std::time::Duration::ZERO; // This test measures the settled body.
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
        (100.0, 200.0).into(),
        "the menu body, not its shadow, starts at the saved click"
    );
    assert_eq!(body.size.w, theme.halo_style().menu_width as f64);
    assert!(
        !body.contains(Point::from((99.0, 200.0))),
        "shadow padding is an outside click"
    );
}

/// A pin is a statement about the header, not a choice of verb, so the palette
/// stays up and the grab is not dismissed by the press that made it.
#[test]
fn pinning_from_the_palette_updates_the_tray_without_closing_it() {
    use crate::shell::element::window::commands;
    use icetron_p::prelude::{HaloCommand, HaloCommandGroup};

    let app_id = "palette-pin-test";
    let commands_list = vec![
        HaloCommand::new("shot", "Screenshot window", HaloCommandGroup::Capture),
        HaloCommand::new("minimize", "Minimize", HaloCommandGroup::Window),
    ];
    let mut menu = ContextMenu::new(vec![
        Item::new("Screenshot window", |_| {}),
        Item::new("Minimize", |_| {}),
    ]);
    menu.halo = true;
    menu.palette = Some(Palette::new(app_id, "Example", commands_list));

    let before = commands::pins(app_id);
    assert!(!before.contains(&"minimize".to_owned()));

    let loop_handle = crate::utils::iced::ProgramLoop::test_handle();
    let _ = menu.update(Message::TogglePin("minimize".into()), &loop_handle, None);
    assert!(
        menu.keep_open.load(Ordering::SeqCst),
        "the press that pinned must not dismiss the grab"
    );
    assert!(
        !menu.selected.load(Ordering::SeqCst),
        "pinning selects nothing"
    );
    assert!(commands::pins(app_id).contains(&"minimize".to_owned()));
}

/// Typing goes into the palette's query and never dismisses it; Enter runs
/// the first row the query leaves, and hands the query to chat when it leaves
/// none. The Ask row does the same without a query.
#[test]
fn the_palette_query_filters_rows_and_enter_runs_the_first_hit_or_asks_chat() {
    use icetron_p::prelude::{HaloCommand, HaloCommandGroup};

    let palette = || {
        let mut menu = ContextMenu::new(vec![
            Item::new("Screenshot window", |_| {}),
            Item::new("Minimize", |_| {}),
        ]);
        menu.halo = true;
        menu.palette = Some(Palette::new(
            "palette-query-test",
            "Example",
            vec![
                HaloCommand::new("shot", "Screenshot window", HaloCommandGroup::Capture),
                HaloCommand::new("minimize", "Minimize", HaloCommandGroup::Window),
            ],
        ));
        menu
    };
    let loop_handle = crate::utils::iced::ProgramLoop::test_handle();

    let mut menu = palette();
    let _ = menu.update(Message::Query("mini".into()), &loop_handle, None);
    assert_eq!(
        menu.palette
            .as_ref()
            .unwrap()
            .query
            .lock()
            .unwrap()
            .as_str(),
        "mini"
    );
    assert!(
        !menu.selected.load(Ordering::SeqCst),
        "typing selects nothing"
    );
    let _ = menu.update(Message::Submit, &loop_handle, None);
    assert!(
        menu.selected.load(Ordering::SeqCst),
        "Enter ran the one row left"
    );

    let mut menu = palette();
    let _ = menu.update(Message::Query("nothing like it".into()), &loop_handle, None);
    let _ = menu.update(Message::Submit, &loop_handle, None);
    assert!(
        menu.selected.load(Ordering::SeqCst),
        "with no row left, Enter asks chat and the palette is done"
    );

    let mut menu = palette();
    let _ = menu.update(Message::AskChat, &loop_handle, None);
    assert!(
        menu.selected.load(Ordering::SeqCst),
        "the Ask row closes the palette"
    );
}

/// Unpinning says nothing, since the header shows the result, while a full
/// tray still explains its refusal.
#[test]
fn only_a_refused_pin_leaves_a_notice() {
    use crate::shell::element::window::commands;
    use icetron_p::prelude::{HaloCommand, HaloCommandGroup, TRAY_CAP};

    let app_id = "palette-notice-test";
    let ids: Vec<String> = (0..=TRAY_CAP).map(|i| format!("cmd{i}")).collect();
    let commands_list = ids
        .iter()
        .map(|id| HaloCommand::new(id.clone(), id.clone(), HaloCommandGroup::App))
        .collect();
    let mut menu = ContextMenu::new(ids.iter().map(|id| Item::new(id.clone(), |_| {})).collect());
    menu.halo = true;
    menu.palette = Some(Palette::new(app_id, "Example", commands_list));
    let loop_handle = crate::utils::iced::ProgramLoop::test_handle();
    let notice = |menu: &ContextMenu| {
        menu.palette
            .as_ref()
            .unwrap()
            .notice
            .lock()
            .unwrap()
            .clone()
    };

    // Start from an empty tray, whatever the app's defaults are.
    for id in commands::pins(app_id) {
        commands::toggle_pin(app_id, &id);
    }
    for id in ids.iter().take(TRAY_CAP) {
        let _ = menu.update(Message::TogglePin(id.clone()), &loop_handle, None);
        assert_eq!(notice(&menu), None, "a pin that fits says nothing");
    }
    let _ = menu.update(
        Message::TogglePin(ids[TRAY_CAP].clone()),
        &loop_handle,
        None,
    );
    assert!(notice(&menu).is_some(), "the refused pin explains itself");
    let _ = menu.update(Message::TogglePin(ids[0].clone()), &loop_handle, None);
    assert_eq!(notice(&menu), None, "unpinning says nothing");
}

/// The palette's backdrop is its card: the card's width, its `radii_xl`
/// corners rather than the dropdown's, and inset by the popover shadow it
/// draws around itself.
#[test]
fn the_palette_blur_matches_its_card() {
    use icetron_p::prelude::{HaloCommand, HaloCommandGroup, PALETTE_WIDTH};

    let event_loop = calloop::EventLoop::<State>::try_new().unwrap();
    let mut tokens = DEFAULT_THEME_PAIR.load(true);
    // A theme whose dropdown is squarer than its cards, as the shipped one is.
    tokens.dropdown_radius = Some(tokens.radii_xl / 2.0);
    let mut theme = CompTheme::new(Arc::new(tokens), true);
    theme.motion.layer_open = std::time::Duration::ZERO;
    let mut menu = ContextMenu::new(vec![
        Item::new("Screenshot window", |_| {}),
        Item::new("Minimize", |_| {}),
    ]);
    menu.halo = true;
    menu.palette = Some(Palette::new(
        "palette-blur-test",
        "Example",
        vec![
            HaloCommand::new("shot", "Screenshot window", HaloCommandGroup::Capture),
            HaloCommand::new("minimize", "Minimize", HaloCommandGroup::Window),
        ],
    ));
    let iced = IcedElement::new(menu, Size::default(), event_loop.handle(), theme.clone());
    let size = iced.minimum_size();
    iced.resize(size);
    let padding = palette_padding(&theme);
    let (blur, radii) = iced
        .with_program(|p| p.backdrop_blur(&theme, size, &[], [0; 4]))
        .unwrap();
    assert_eq!(blur.width, PALETTE_WIDTH);
    assert_eq!((blur.x, blur.y), (padding.left, padding.top));
    assert_eq!(blur.height, size.h as f32 - padding.top - padding.bottom);
    let card = theme.radii_xl().round() as u8;
    assert_eq!(radii, [card; 4]);
    assert_ne!(radii, [theme.dropdown_radius().round() as u8; 4]);
}

/// Typing shrinks the list, and the surface — its blur and its click-through
/// region — must shrink with it rather than keep the tallest list it ever
/// showed, then grow back when the query is cleared.
#[test]
fn the_palette_surface_refits_as_the_query_changes_its_rows() {
    use icetron_p::prelude::{HaloCommand, HaloCommandGroup};

    let event_loop = calloop::EventLoop::<State>::try_new().unwrap();
    let mut theme = CompTheme::new(Arc::new(DEFAULT_THEME_PAIR.load(true)), true);
    theme.motion.layer_open = std::time::Duration::ZERO;
    let mut menu = ContextMenu::new(vec![
        Item::new("Screenshot window", |_| {}),
        Item::new("Minimize", |_| {}),
        Item::new("Maximize", |_| {}),
    ]);
    menu.halo = true;
    menu.palette = Some(Palette::new(
        "palette-refit-test",
        "Example",
        vec![
            HaloCommand::new("shot", "Screenshot window", HaloCommandGroup::Capture),
            HaloCommand::new("minimize", "Minimize", HaloCommandGroup::Window),
            HaloCommand::new("maximize", "Maximize", HaloCommandGroup::Window),
        ],
    ));
    let iced = IcedElement::new(menu, Size::default(), event_loop.handle(), theme.clone());
    let full = iced.minimum_size();
    iced.resize(full);
    let element = Element {
        iced,
        position: (0, 0).into(),
        pointer_entered: false,
        touch_entered: None,
    };
    let padding = palette_padding(&theme);

    element.iced.queue_message(Message::Query("mini".into()));
    element.refit();
    let shrunk = element.iced.current_size();
    assert!(shrunk.h < full.h, "one row left must take less than three");
    let (blur, _) = element
        .iced
        .with_program(|p| p.backdrop_blur(&theme, shrunk, &[], [0; 4]))
        .unwrap();
    assert_eq!(blur.height, shrunk.h as f32 - padding.top - padding.bottom);

    element.iced.queue_message(Message::Query(String::new()));
    element.refit();
    assert_eq!(element.iced.current_size(), full);
}
