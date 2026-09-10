use super::*;

pub(super) fn fullscreen_hit(
    point: Point<f64, Logical>,
    width: f64,
    scale: f64,
    pill: Option<iced_core::Rectangle>,
    revealed: bool,
) -> bool {
    if point.x < 0.0 || point.x >= width || point.y < 0.0 {
        return false;
    }
    // Only the outermost physical pixel can reveal a hidden header. Once it is
    // revealed, bridge the 10px inset over the pill's width so controls are reachable.
    point.y < 1.0 / scale
        || (revealed
            && pill.is_some_and(|pill| {
                point.x >= f64::from(pill.x)
                    && point.x < f64::from(pill.x + pill.width)
                    && point.y < f64::from(pill.y + pill.height)
            }))
}
use crate::{
    fl,
    shell::{
        Shell,
        element::CosmicMappedInternal,
        grabs::{GrabStartData, Item, MenuGrab},
    },
};

/// Seat queries must run after PointerTarget/TouchTarget dispatch releases its
/// input lock. Constructing the deferred query must not inspect either device.
pub(super) fn menu_input_query(
    seat: Seat<State>,
    serial: Serial,
) -> impl FnOnce() -> Option<(GrabStartData, Point<i32, Global>)> + Send {
    move || {
        let start = crate::shell::check_grab_preconditions(&seat, Some(serial), None)?;
        let position = start.current_location(&seat).to_i32_round().as_global();
        Some((start, position))
    }
}

pub(super) fn perform_action(
    state: &mut State,
    surface: &CosmicSurface,
    seat: Option<&Seat<State>>,
    message: Message,
    new_window: Option<&NewWindowAction>,
) {
    if !surface.alive() {
        return;
    }
    match message {
        Message::Screenshot => crate::utils::screenshot::screenshot_window(state, surface),
        Message::NewWindow => {
            if let Some(action) = new_window {
                action.launch();
            }
        }
        Message::Close => surface.close(),
        Message::Minimize => state.common.shell.write().minimize_request(surface),
        Message::Maximize => {
            let mut shell = state.common.shell.write();
            if let Some(mapped) = shell.element_for_surface(surface).cloned() {
                let seat = seat
                    .cloned()
                    .unwrap_or_else(|| shell.seats.last_active().clone());
                shell.maximize_toggle(&mapped, &seat, &state.common.event_loop_handle);
            }
        }
        Message::Fullscreen => {
            let mut shell = state.common.shell.write();
            let seat = seat
                .cloned()
                .unwrap_or_else(|| shell.seats.last_active().clone());
            let target = if surface.is_fullscreen(false) {
                shell.unfullscreen_request(surface, &state.common.event_loop_handle)
            } else {
                shell.fullscreen_request(
                    surface,
                    seat.active_output(),
                    &state.common.event_loop_handle,
                )
            };
            drop(shell);
            if let Some(target) = target {
                Shell::set_focus(state, Some(&target), &seat, None, false);
            }
        }
        Message::Menu | Message::DragStart => {}
    }
}

fn menu_items(
    surface: &CosmicSurface,
    seat: &Seat<State>,
    action: Option<NewWindowAction>,
) -> Vec<Item> {
    let item = |title: String, message: Message| {
        let surface = surface.clone();
        let seat = seat.clone();
        let action = action.clone();
        Item::new(title, move |handle| {
            let surface = surface.clone();
            let seat = seat.clone();
            let action = action.clone();
            handle.insert_idle(move |state| {
                perform_action(state, &surface, Some(&seat), message, action.as_ref())
            });
        })
    };
    let mut items = Vec::new();
    if action.is_some() {
        items.push(item("New Window".into(), Message::NewWindow));
        items.push(Item::Separator);
    }
    items.extend([
        item(fl!("window-menu-screenshot"), Message::Screenshot),
        Item::new("Record — coming soon", |_| {}).disabled(true),
        Item::Separator,
        item(fl!("window-menu-minimize"), Message::Minimize),
        item(
            if surface.is_maximized(false) {
                "Restore".into()
            } else {
                fl!("window-menu-maximize")
            },
            Message::Maximize,
        ),
        item(
            if surface.is_fullscreen(false) {
                "Leave fullscreen".into()
            } else {
                fl!("window-menu-fullscreen")
            },
            Message::Fullscreen,
        ),
        Item::Separator,
        item(fl!("window-menu-close"), Message::Close),
    ]);
    items
}

pub(super) fn open_menu(
    state: &mut State,
    surface: &CosmicSurface,
    seat: &Seat<State>,
    serial: Serial,
    start: GrabStartData,
    position: Point<i32, Global>,
    action: Option<NewWindowAction>,
) {
    let shell = state.common.shell.read();
    let ui = shell
        .element_for_surface(surface)
        .and_then(|mapped| {
            if let CosmicMappedInternal::Window(window) = &mapped.element {
                Some(window.0.clone())
            } else {
                None
            }
        })
        .or_else(|| {
            shell
                .workspaces()
                .spaces()
                .flat_map(|w| &w.fullscreen_surfaces)
                .find(|fullscreen| &fullscreen.surface == surface)
                .map(|fullscreen| fullscreen.halo.0.clone())
        });
    let Some(ui) = ui else {
        return;
    };
    let open = ui.with_program(|p| p.menu_open.clone());
    let theme = shell.theme().clone();
    drop(shell);
    let items = menu_items(surface, seat, action);
    open.store(true, Ordering::SeqCst);
    ui.force_update();
    let grab = MenuGrab::new_halo(
        start,
        seat,
        items.into_iter(),
        position,
        state.common.event_loop_handle.clone(),
        theme,
    )
    .on_close(move || {
        open.store(false, Ordering::SeqCst);
        ui.force_update();
    });
    if grab.is_touch_grab() {
        if let Some(touch) = seat.get_touch() {
            touch.set_grab(state, grab, serial);
        }
    } else if let Some(pointer) = seat.get_pointer() {
        pointer.set_grab(state, grab, serial, smithay::input::pointer::Focus::Keep);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use smithay::input::SeatState;

    #[test]
    fn fullscreen_reveals_only_at_top_edge_then_keeps_the_pill_interactive() {
        let pill = Some(iced_core::Rectangle::new(
            iced_core::Point::new(400.0, 10.0),
            iced_core::Size::new(400.0, 31.0),
        ));
        for scale in [1.0, 1.25, 2.0] {
            let hit = |x, y, revealed| fullscreen_hit((x, y).into(), 1200.0, scale, pill, revealed);
            for x in [0.0, 600.0, 1199.9] {
                assert!(hit(x, 0.0, false));
                assert!(hit(x, 0.9 / scale, false));
                assert!(!hit(x, 1.0 / scale, false));
                assert!(!hit(x, 25.9, false));
            }
            assert!(!hit(600.0, 5.0, false));
            assert!(
                hit(600.0, 5.0, true),
                "revealed controls must remain reachable across the inset"
            );
            assert!(!hit(600.0, 38.0, false));
            assert!(hit(600.0, 38.0, true));
            assert!(
                !hit(200.0, 5.0, true),
                "the bridge must not span unrelated content"
            );
            assert!(!hit(200.0, 38.0, true));
            assert!(!hit(600.0, 42.0, true));
            assert!(!hit(-1.0, 0.0, true));
            assert!(!hit(1200.0, 0.0, true));
            assert!(!hit(600.0, -1.0, true));
        }
    }

    #[test]
    fn fullscreen_focus_is_not_a_reveal_trigger_but_an_open_menu_is() {
        use crate::shell::element::header_bar::halo_is_visible;
        assert!(!halo_is_visible(true, false, true, false));
        assert!(halo_is_visible(true, true, true, false));
        assert!(halo_is_visible(true, false, true, true));
        assert!(!halo_is_visible(true, false, false, false));
        assert!(halo_is_visible(false, false, true, false));
        assert!(halo_is_visible(false, true, false, false));
    }

    #[test]
    fn constructing_menu_request_does_not_query_locked_input_devices() {
        let mut seats = SeatState::<State>::new();
        let seat = seats.new_seat("menu-test");
        // No input devices: eager check_grab_preconditions would panic here.
        // During a real click it instead re-locks Smithay's held pointer mutex.
        let query = menu_input_query(seat, Serial::from(1));
        drop(query);
    }

    #[test]
    fn deferred_menu_query_captures_input_after_dispatch() {
        let mut seats = SeatState::<State>::new();
        let mut seat = seats.new_seat("menu-test");
        seat.add_pointer();
        seat.add_touch();
        let (_, position) = menu_input_query(seat, Serial::from(1))().unwrap();
        assert_eq!(position, Point::from((0, 0)));
    }
}
