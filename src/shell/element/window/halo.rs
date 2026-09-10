use super::*;
use crate::{
    fl,
    shell::{
        Shell,
        element::CosmicMappedInternal,
        grabs::{GrabStartData, Item, MenuGrab},
    },
};

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
    let Some(mapped) = shell.element_for_surface(surface) else {
        return;
    };
    let CosmicMappedInternal::Window(window) = &mapped.element else {
        return;
    };
    let ui = window.0.clone();
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
