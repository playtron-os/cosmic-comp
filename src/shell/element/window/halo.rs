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
        grabs::{GrabStartData, Item, MenuGrab, Palette, PaletteKeyboardGrab},
    },
    utils::desktop_action::DesktopApp,
};

use super::commands;
use crate::shell::element::header_bar;
use icetron_p::prelude::{PALETTE_TOP_OFFSET, PALETTE_WIDTH};

/// The prototype keeps the palette this far inside the output's edges.
const PALETTE_EDGE_MARGIN: i32 = 10;

/// Where the palette goes: centred on `window` and hanging under its pill, so
/// it reads as the window's rather than the click's. A window low enough that
/// the card would leave the output puts it above the pill instead, and a card
/// that fits neither way slides up over the window rather than off the screen.
///
/// `PALETTE_TOP_OFFSET` is the design's distance from a floating window's top
/// edge, whose pill ends 16px in; taken as clearance under `pill_bottom`, a
/// joined or fullscreen pill gets the same gap.
pub(super) fn palette_origin(
    theme: &crate::comp_theme::CompTheme,
    window: Rectangle<i32, Global>,
    pill_bottom: i32,
    output: Rectangle<i32, Global>,
    card: Size<i32, Logical>,
) -> Point<i32, Global> {
    let gap = PALETTE_TOP_OFFSET as i32 - header_bar::halo_pill_bottom(theme, false);
    let width = PALETTE_WIDTH as i32;
    let x = (window.loc.x + window.size.w / 2 - width / 2)
        .max(output.loc.x + PALETTE_EDGE_MARGIN)
        .min(output.loc.x + output.size.w - width - PALETTE_EDGE_MARGIN);
    let top = output.loc.y + PALETTE_EDGE_MARGIN;
    let lowest = output.loc.y + output.size.h - PALETTE_EDGE_MARGIN - card.h;
    let below = window.loc.y + pill_bottom + gap;
    let pill_top = pill_bottom - theme.halo_style().pill_height().ceil() as i32;
    let above = window.loc.y + pill_top - gap - card.h;
    let y = if below > lowest && above >= top {
        above
    } else {
        below.min(lowest).max(top)
    };
    Point::from((x, y))
}

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
        Message::Record => crate::utils::recording::toggle(state, surface),
        Message::NewWindow => {
            if let Some(action) = new_window {
                action.launch();
            }
        }
        Message::Close => surface.close(),
        Message::Minimize => state.common.shell.write().minimize_request(surface),
        Message::Maximize => {
            let mut shell = state.common.shell.write();
            let seat = seat
                .cloned()
                .unwrap_or_else(|| shell.seats.last_active().clone());
            // Fullscreen surfaces are no longer in the normal mapped-window list.
            // Restore that state first, including its saved output/workspace and size.
            let restored = shell.unfullscreen_request(surface, &state.common.event_loop_handle);
            if restored.is_none()
                && let Some(mapped) = shell.element_for_surface(surface).cloned()
            {
                shell.maximize_toggle(&mapped, &seat, &state.common.event_loop_handle);
            }
            drop(shell);
            if let Some(target) = restored {
                Shell::set_focus(state, Some(&target), &seat, None, false);
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
        // Routed through `perform_command`, which has the desktop entry to
        // resolve the id against; there is nothing to do with it here.
        Message::Action(_) | Message::Menu | Message::Commands | Message::DragStart => {}
    }
}

/// Run a command by id: a window verb, or one of the actions the window's
/// desktop entry declares.
pub(super) fn perform_command(
    state: &mut State,
    surface: &CosmicSurface,
    seat: Option<&Seat<State>>,
    id: &str,
    app: Option<&DesktopApp>,
) {
    if let Some(message) = commands::message_for(id) {
        perform_action(
            state,
            surface,
            seat,
            message,
            app.and_then(|app| app.new_window.as_ref()),
        );
    } else if let Some(action) = commands::desktop_action(app, id) {
        action.launch();
    }
}

/// The window's desktop-entry actions, as menu rows.
///
/// The freedesktop entry is where an application already declares what it can
/// do from outside itself, so the window menu offers those rather than a
/// parallel set only apps built for this compositor could answer.
fn app_action_items(
    surface: &CosmicSurface,
    seat: &Seat<State>,
    app: Option<&DesktopApp>,
) -> Vec<Item> {
    let Some(app) = app else {
        return Vec::new();
    };
    app.actions
        .iter()
        .map(|action| {
            let surface = surface.clone();
            let seat = seat.clone();
            let app = app.clone();
            let id = format!("{}{}", commands::ACTION_PREFIX, action.id);
            Item::new(action.name.clone(), move |handle| {
                let surface = surface.clone();
                let seat = seat.clone();
                let app = app.clone();
                let id = id.clone();
                handle.insert_idle(move |state| {
                    perform_command(state, &surface, Some(&seat), &id, Some(&app));
                });
            })
        })
        .collect()
}

fn menu_items(
    surface: &CosmicSurface,
    seat: &Seat<State>,
    action: Option<NewWindowAction>,
    app_actions: Vec<Item>,
    close_all: Option<Item>,
) -> Vec<Item> {
    let item = |title: String, message: Message| {
        let surface = surface.clone();
        let seat = seat.clone();
        let action = action.clone();
        Item::new(title, move |handle| {
            let surface = surface.clone();
            let seat = seat.clone();
            let action = action.clone();
            let message = message.clone();
            handle.insert_idle(move |state| {
                perform_action(state, &surface, Some(&seat), message, action.as_ref())
            });
        })
    };
    let mut items = Vec::new();
    if action.is_some() {
        items.push(item(fl!("window-menu-new-window"), Message::NewWindow));
        items.push(Item::Separator);
    }
    if !app_actions.is_empty() {
        items.extend(app_actions);
        items.push(Item::Separator);
    }
    items.extend([
        item(fl!("window-menu-screenshot"), Message::Screenshot),
        item(
            if surface.is_recording() {
                fl!("window-menu-stop-recording")
            } else {
                fl!("window-menu-record")
            },
            Message::Record,
        ),
        Item::Separator,
        item(fl!("window-menu-minimize"), Message::Minimize),
        item(
            if surface.is_maximized(false) || surface.is_fullscreen(false) {
                fl!("window-menu-restore")
            } else {
                fl!("window-menu-maximize")
            },
            Message::Maximize,
        ),
        item(
            if surface.is_fullscreen(false) {
                fl!("window-menu-leave-fullscreen")
            } else {
                fl!("window-menu-fullscreen")
            },
            Message::Fullscreen,
        ),
        Item::Separator,
        item(fl!("window-menu-close"), Message::Close),
    ]);
    items.extend(close_all);
    items
}

fn select_app_windows<T: Clone + Eq + std::hash::Hash>(
    app_id: &str,
    origin_realm: &str,
    current_realm: &str,
    candidates: impl Iterator<Item = (T, String)>,
) -> Vec<T> {
    // Unknown app IDs must not group unrelated applications. A stale menu must
    // not close an identically named app in a different workspace context.
    if app_id.is_empty() || origin_realm != current_realm {
        return Vec::new();
    }
    let mut seen = std::collections::HashSet::new();
    candidates
        .filter_map(|(window, candidate_app)| {
            (candidate_app == app_id && seen.insert(window.clone())).then_some(window)
        })
        .collect()
}

fn app_windows(shell: &Shell, app_id: &str, origin_realm: &str) -> Vec<CosmicSurface> {
    let mapped = shell
        .mapped()
        .flat_map(|mapped| mapped.windows().map(|(window, _)| window));
    // mapped() includes normal minimized windows, but not minimized fullscreen
    // surfaces. Deduplicate below, also covering windows in transition.
    let minimized = shell.workspaces().iter().flat_map(|(_, set)| {
        set.minimized_windows
            .iter()
            .chain(
                set.workspaces
                    .iter()
                    .flat_map(|desktop| &desktop.minimized_windows),
            )
            .flat_map(|minimized| minimized.windows())
    });
    let fullscreen = shell
        .workspaces()
        .spaces()
        .flat_map(|desktop| desktop.get_fullscreen_surfaces())
        .map(|fullscreen| fullscreen.surface.clone());
    let candidates = mapped
        .chain(minimized)
        .chain(fullscreen)
        .filter(|window| {
            window.alive() && !window.is_override_redirect() && !is_surface_embedded(window)
        })
        .map(|window| {
            let app_id = window.app_id();
            (window, app_id)
        });
    select_app_windows(app_id, origin_realm, shell.active_realm(), candidates)
}

fn close_all_item(shell: &Shell, origin: &CosmicSurface) -> Option<Item> {
    let app_id = origin.app_id();
    let realm = shell.active_realm().to_owned();
    let windows = app_windows(shell, &app_id, &realm);
    if !has_multiple_app_windows(&windows, origin) {
        return None;
    }
    Some(Item::new(fl!("window-menu-close-all"), move |handle| {
        let app_id = app_id.clone();
        let realm = realm.clone();
        handle.insert_idle(move |state| {
            let windows = {
                let shell = state.common.shell.read();
                app_windows(&shell, &app_id, &realm)
            };
            // Snapshot before closing, outside the shell lock. Normal close
            // requests allow applications to ask about unsaved work.
            for window in windows {
                window.close();
            }
        });
    }))
}

fn has_multiple_app_windows<T: PartialEq>(windows: &[T], origin: &T) -> bool {
    windows.len() > 1 && windows.contains(origin)
}

/// The compact app menu — the `⌄`, or a right-click.
pub(super) fn open_menu(
    state: &mut State,
    surface: &CosmicSurface,
    seat: &Seat<State>,
    serial: Serial,
    start: GrabStartData,
    position: Point<i32, Global>,
    app: Option<DesktopApp>,
) {
    open_surface(state, surface, seat, serial, start, position, app, false);
}

/// The command palette — every verb this window answers, each pinnable to the
/// header. Opened from the app glyph.
pub(super) fn open_commands(
    state: &mut State,
    surface: &CosmicSurface,
    seat: &Seat<State>,
    serial: Serial,
    start: GrabStartData,
    position: Point<i32, Global>,
    app: Option<DesktopApp>,
) {
    open_surface(state, surface, seat, serial, start, position, app, true);
}

#[allow(clippy::too_many_arguments)]
fn open_surface(
    state: &mut State,
    surface: &CosmicSurface,
    seat: &Seat<State>,
    serial: Serial,
    start: GrabStartData,
    position: Point<i32, Global>,
    app: Option<DesktopApp>,
    palette: bool,
) {
    let shell = state.common.shell.read();
    let theme = shell.theme().clone();
    // The palette anchors to the window rather than the click: its rect, where
    // its pill ends below the rect's top, and the output it is on.
    let (ui, anchor) = if let Some((ui, mapped)) =
        shell
            .element_for_surface(surface)
            .and_then(|mapped| match &mapped.element {
                CosmicMappedInternal::Window(window) => Some((window.0.clone(), mapped)),
                _ => None,
            }) {
        let anchor = shell.element_geometry(mapped).map(|geometry| {
            let joined = ui.with_program(|p| p.joined_halo());
            let center = geometry.loc + Point::from((geometry.size.w / 2, geometry.size.h / 2));
            let output = shell
                .outputs()
                .find(|output| output.geometry().contains(center))
                .cloned()
                .unwrap_or_else(|| seat.active_output());
            (
                geometry,
                header_bar::halo_pill_bottom(&theme, joined),
                output.geometry(),
            )
        });
        (ui, anchor)
    } else {
        let Some((fullscreen, output)) = shell
            .workspaces()
            .spaces()
            .flat_map(|w| w.fullscreen_surfaces.iter().map(move |f| (f, w.output())))
            .find(|(fullscreen, _)| &fullscreen.surface == surface)
        else {
            return;
        };
        let output = output.geometry();
        (
            fullscreen.halo.0.clone(),
            Some((output, header_bar::fullscreen_pill_bottom(&theme), output)),
        )
    };
    let open = ui.with_program(|p| {
        if palette {
            p.commands_open.clone()
        } else {
            p.menu_open.clone()
        }
    });
    let close_all = close_all_item(&shell, surface);
    let resizable = {
        let min = surface.min_size_without_ssd();
        !(min.is_some() && min == surface.max_size_without_ssd())
    };
    drop(shell);
    let action = app.as_ref().and_then(|app| app.new_window.clone());
    let place: Box<dyn FnOnce(Size<i32, Logical>) -> Point<i32, Global>> = match anchor {
        Some((window, pill_bottom, output)) => {
            let theme = theme.clone();
            Box::new(move |card| palette_origin(&theme, window, pill_bottom, output, card))
        }
        None => Box::new(move |_| position),
    };
    let grab = if palette {
        let facts = commands::WindowFacts {
            recording: surface.is_recording(),
            maximized: surface.is_maximized(false),
            fullscreen: surface.is_fullscreen(false),
            resizable,
            close_all: close_all.is_some(),
            app: app.as_ref(),
        };
        let list = commands::commands(&facts);
        // Index-aligned with the command list: a row press is the same
        // callback the menu row would have run.
        let items = list.iter().map(|command| {
            let surface = surface.clone();
            let seat = seat.clone();
            let app = app.clone();
            let id = command.id.clone();
            Item::new(command.label.clone(), move |handle| {
                let surface = surface.clone();
                let seat = seat.clone();
                let app = app.clone();
                let id = id.clone();
                handle.insert_idle(move |state| {
                    perform_command(state, &surface, Some(&seat), &id, app.as_ref());
                });
            })
        });
        let scope = app
            .as_ref()
            .and_then(|app| app.name.clone())
            .unwrap_or_else(|| surface.app_id());
        let palette = Palette::new(surface.app_id(), scope, list.clone());
        open.store(true, Ordering::SeqCst);
        ui.force_update();
        MenuGrab::new_palette(
            start,
            seat,
            items.collect::<Vec<_>>().into_iter(),
            place,
            state.common.event_loop_handle.clone(),
            theme,
            palette,
        )
    } else {
        let app_actions = app_action_items(surface, seat, app.as_ref());
        let items = menu_items(surface, seat, action, app_actions, close_all);
        open.store(true, Ordering::SeqCst);
        ui.force_update();
        MenuGrab::new_halo(
            start,
            seat,
            items.into_iter(),
            position,
            state.common.event_loop_handle.clone(),
            theme,
        )
    };
    let grab = grab.on_close(move || {
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
    // The palette has a search field: typing goes to it, and Escape closes
    // the palette, for as long as it is up.
    if palette && let Some(keyboard) = seat.get_keyboard() {
        keyboard.set_grab(state, PaletteKeyboardGrab::new(seat.clone()), serial);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use smithay::input::SeatState;

    /// The palette is the window's: centred on it, hanging under its pill, and
    /// kept off the output's edges rather than following the click.
    #[test]
    fn the_palette_hangs_centred_under_the_pill_inside_the_output() {
        let mut theme = icetron_themes::dynamic::DEFAULT_THEME_PAIR.load(false);
        theme.window_header_style = icetron_themes::WindowHeaderStyle::Halo;
        let theme = crate::comp_theme::CompTheme::new(std::sync::Arc::new(theme), false);
        let output = Rectangle::<i32, Global>::new((0, 0).into(), (1920, 1080).into());
        let window = Rectangle::new((500, 300).into(), (800, 600).into());
        let card = Size::from((430, 300));
        assert_eq!(
            palette_origin(&theme, window, 16, output, card),
            Point::from((500 + 400 - 215, 300 + 22))
        );
        // A joined pill ends 15px lower, and the palette keeps its clearance.
        assert_eq!(palette_origin(&theme, window, 31, output, card).y, 300 + 37);
        // Against an edge it stops at the margin instead of leaving the output.
        let left = Rectangle::new((-100, -50).into(), (300, 600).into());
        assert_eq!(
            palette_origin(&theme, left, 16, output, card),
            Point::from((10, 10))
        );
        let right = Rectangle::new((1800, 300).into(), (300, 600).into());
        assert_eq!(
            palette_origin(&theme, right, 16, output, card).x,
            1920 - 430 - 10
        );
    }

    /// Too low for the card to fit below the pill, it hangs above the pill with
    /// the same clearance; a card that fits nowhere slides up to the bottom
    /// margin, and a huge one stops at the top margin rather than leaving the
    /// output.
    #[test]
    fn a_palette_that_would_leave_the_output_goes_above_the_pill_or_slides_up() {
        let mut theme = icetron_themes::dynamic::DEFAULT_THEME_PAIR.load(false);
        theme.window_header_style = icetron_themes::WindowHeaderStyle::Halo;
        let theme = crate::comp_theme::CompTheme::new(std::sync::Arc::new(theme), false);
        let output = Rectangle::<i32, Global>::new((0, 0).into(), (1920, 1080).into());
        let low = Rectangle::new((500, 900).into(), (800, 600).into());
        let card = Size::from((430, 300));
        // The floating pill spans -15..16 from the window's top: above means the
        // card's bottom sits 6px over the pill's top.
        assert_eq!(
            palette_origin(&theme, low, 16, output, card).y,
            900 - 15 - 6 - 300
        );
        let window = Rectangle::new((500, 300).into(), (800, 600).into());
        let tall = Size::from((430, 800));
        assert_eq!(
            palette_origin(&theme, window, 16, output, tall).y,
            1080 - 10 - 800,
            "neither side fits, so it slides up to the bottom margin"
        );
        let huge = Size::from((430, 1200));
        assert_eq!(palette_origin(&theme, low, 16, output, huge).y, 10);
    }

    #[test]
    fn close_all_counts_distinct_app_windows_and_requires_the_origin() {
        let candidates = [
            (1, "editor"),
            (2, "editor"),
            (1, "editor"),
            (3, "terminal"),
            (4, "Editor"),
            (2, "editor"),
        ];
        let windows = select_app_windows(
            "editor",
            "work",
            "work",
            candidates.into_iter().map(|(id, app)| (id, app.to_owned())),
        );
        assert_eq!(windows, [1, 2]);
        assert!(has_multiple_app_windows(&windows, &1));
        assert!(!has_multiple_app_windows(&windows, &3));
        assert!(!has_multiple_app_windows(&[1], &1));
        assert!(!has_multiple_app_windows::<u32>(&[], &1));
    }

    #[test]
    fn close_all_never_groups_unknown_apps_or_crosses_workspace_contexts() {
        let unknown = [(1, String::new()), (2, String::new())];
        assert!(select_app_windows("", "work", "work", unknown.into_iter()).is_empty());
        let other_workspace = [(1, "editor".to_owned()), (2, "editor".to_owned())];
        assert!(
            select_app_windows("editor", "work", "personal", other_workspace.into_iter())
                .is_empty()
        );
    }

    #[test]
    fn close_all_targets_follow_the_current_window_list() {
        let before = [(1, "editor".to_owned()), (2, "editor".to_owned())];
        let after = [
            (2, "editor".to_owned()),
            (3, "editor".to_owned()),
            (4, "terminal".to_owned()),
        ];
        assert_eq!(
            select_app_windows("editor", "work", "work", before.into_iter()),
            [1, 2]
        );
        assert_eq!(
            select_app_windows("editor", "work", "work", after.into_iter()),
            [2, 3]
        );
    }

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
