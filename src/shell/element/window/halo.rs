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
        grabs::{GrabStartData, Item, MenuGrab, MoveDialog, MoveRow, Palette, PaletteKeyboardGrab},
    },
    utils::desktop_action::DesktopApp,
};

use super::commands;
use crate::shell::element::header_bar;
use crate::wayland::protocols::app_commands::{self, Selection};
use cosmic_settings_config::shortcuts;
use icetron_p::prelude::{PALETTE_TOP_OFFSET, PALETTE_WIDTH};

/// The prototype keeps the palette this far inside the output's edges.
const PALETTE_EDGE_MARGIN: i32 = 10;

/// Where the palette goes: centred on `window` and hanging under its pill, so
/// it reads as the window's rather than the click's. A window low enough that
/// the card would leave the output puts it above the pill instead, and a card
/// that fits neither way slides up over the window rather than off the screen.
///
/// The card sits 22px below the window's top; a fullscreen pill gets the same gap.
pub(super) fn palette_origin(
    theme: &crate::comp_theme::CompTheme,
    window: Rectangle<i32, Global>,
    pill_bottom: i32,
    output: Rectangle<i32, Global>,
    card: Size<i32, Logical>,
) -> Point<i32, Global> {
    let gap = PALETTE_TOP_OFFSET as i32 - header_bar::halo_pill_bottom(theme);
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
            if !invoke_own(surface, "neww", None)
                && let Some(action) = new_window
            {
                action.launch();
            }
        }
        Message::Close => surface.close(),
        Message::Minimize => {
            let mut shell = state.common.shell.write();
            shell.minimize_request(surface);
            shell.settle_fullscreen_desktops(&mut state.common.workspace_state.update());
        }
        Message::Maximize => {
            let mut shell = state.common.shell.write();
            let seat = seat
                .cloned()
                .unwrap_or_else(|| shell.seats.last_active().clone());
            // Fullscreen surfaces are no longer in the normal mapped-window list.
            // Restore that state first, including its saved output/workspace and size.
            let restored = shell.unfullscreen_request(surface, &state.common.event_loop_handle);
            shell.settle_fullscreen_desktops(&mut state.common.workspace_state.update());
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
            let mut workspace_state = state.common.workspace_state.update();
            let target = if surface.is_fullscreen(false) {
                let target = shell.unfullscreen_request(surface, &state.common.event_loop_handle);
                shell.settle_fullscreen_desktops(&mut workspace_state);
                target
            } else {
                shell.fullscreen_on_own_desktop(
                    surface,
                    seat.active_output(),
                    &state.common.event_loop_handle,
                    &mut workspace_state,
                )
            };
            drop(workspace_state);
            drop(shell);
            if let Some(target) = target {
                Shell::set_focus(state, Some(&target), &seat, None, false);
            }
        }
        // Routed through `perform_command`, which has the desktop entry to
        // resolve the id against; there is nothing to do with it here.
        Message::Action(_)
        | Message::Menu
        | Message::Commands
        | Message::DragStart
        | Message::RunChip => {}
    }
}

/// Hand standard verb or app command `id` to the window, if it answers it.
/// `generation` is the catalog the selection was made from; `None` takes the
/// current one, for controls redrawn whenever it changes.
fn invoke_own(surface: &CosmicSurface, id: &str, generation: Option<u32>) -> bool {
    let Some(wl) = surface.wl_surface() else {
        return false;
    };
    let Some(catalog) = app_commands::committed(&wl) else {
        return false;
    };
    let Some(command) = commands::app_command(&catalog, id) else {
        return false;
    };
    app_commands::invoke(
        &wl,
        &Selection::Command(command.to_owned()),
        generation.unwrap_or(catalog.generation),
        smithay::utils::SERIAL_COUNTER.next_serial(),
    );
    true
}

/// Run a command by id: a window verb, the window's own, or one of the actions
/// its desktop entry declares.
pub(super) fn perform_command(
    state: &mut State,
    surface: &CosmicSurface,
    seat: Option<&Seat<State>>,
    id: &str,
    app: Option<&DesktopApp>,
) {
    run_command(state, surface, seat, id, app, None);
}

/// [`perform_command`], picked from a menu or palette built from catalog `generation`.
fn run_command(
    state: &mut State,
    surface: &CosmicSurface,
    seat: Option<&Seat<State>>,
    id: &str,
    app: Option<&DesktopApp>,
    generation: Option<u32>,
) {
    // The tray hands every verb that is not the compositor's over as a desktop
    // action; the window's own come back out here.
    let id = match id.strip_prefix(commands::ACTION_PREFIX) {
        Some(rest) if commands::desktop_action(app, id).is_none() => rest,
        _ => id,
    };
    let can_open_another = app.is_some_and(|app| app.new_window.is_some())
        || surface
            .wl_surface()
            .and_then(|wl| app_commands::committed(&wl))
            .is_some_and(|catalog| catalog.handles.contains_key("neww"));
    if id == "neww" && !can_open_another {
        let name = app
            .and_then(|app| app.name.clone())
            .unwrap_or_else(|| surface.app_id());
        toast(state, fl!("halo-one-window", app = name.as_str()));
        return;
    }
    if id == commands::MOVE_TO_DESKTOP {
        if let Some(seat) = seat {
            open_move_dialog(state, surface, seat, app.cloned());
        }
        return;
    }
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
    } else {
        invoke_own(surface, id, generation);
    }
}

/// The Move to Desktop chooser, centred on the window's output as the design's Modal is.
/// Opened from an idle, after whatever grab picked it has let go.
pub(super) fn open_move_dialog(
    state: &mut State,
    surface: &CosmicSurface,
    seat: &Seat<State>,
    app: Option<DesktopApp>,
) {
    let surface = surface.clone();
    let seat = seat.clone();
    state.common.event_loop_handle.insert_idle(move |state| {
        let shell = state.common.shell.read();
        let Some(choices) = shell.move_choices(&surface) else {
            return;
        };
        let Some(output) = surface
            .wl_surface()
            .and_then(|wl| shell.workspace_for_surface(&wl))
            .map(|(_, output)| output.geometry())
        else {
            return;
        };
        let source = shell.move_source(&surface).unwrap_or_default();
        let theme = shell.theme().clone();
        drop(shell);
        let serial = smithay::utils::SERIAL_COUNTER.next_serial();
        let Some(start) = crate::shell::check_grab_preconditions(&seat, Some(serial), None) else {
            return;
        };
        let name = app
            .and_then(|app| app.name)
            .unwrap_or_else(|| surface.app_id());
        let title = surface.title();
        let identity = if title.is_empty() || title == name {
            name
        } else {
            format!("{name} · {title}")
        };
        let rows = move_rows(
            choices
                .iter()
                .map(|choice| (choice.label.as_str(), choice.windows, choice.current)),
        );
        let destinations = choices
            .iter()
            .map(|choice| Some(choice.handle))
            .chain([None]);
        let items: Vec<Item> = rows
            .iter()
            .zip(destinations)
            .map(|(row, to)| {
                let (surface, seat) = (surface.clone(), seat.clone());
                Item::new(row.label.clone(), move |handle| {
                    let (surface, seat) = (surface.clone(), seat.clone());
                    handle.insert_idle(move |state| move_and_follow(state, &surface, &seat, to));
                })
                .disabled(row.current)
            })
            .collect();
        let dialog = MoveDialog {
            identity,
            source,
            rows,
            focus: 0,
        };
        let place = move |card: Size<i32, Logical>| {
            Point::from((
                output.loc.x + (output.size.w - card.w) / 2,
                output.loc.y + (output.size.h - card.h) / 2,
            ))
        };
        let grab = MenuGrab::new_dialog(
            start,
            &seat,
            items.into_iter(),
            place,
            state.common.event_loop_handle.clone(),
            theme,
            dialog,
        );
        if grab.is_touch_grab() {
            if let Some(touch) = seat.get_touch() {
                touch.set_grab(state, grab, serial);
            }
        } else if let Some(pointer) = seat.get_pointer() {
            pointer.set_grab(state, grab, serial, smithay::input::pointer::Focus::Keep);
        }
        if let Some(keyboard) = seat.get_keyboard() {
            keyboard.set_grab(state, PaletteKeyboardGrab::new(seat.clone()), serial);
        }
    });
}

/// The chooser's rows from each desktop's (name, windows, current), then "New desktop".
fn move_rows<'a>(desktops: impl IntoIterator<Item = (&'a str, usize, bool)>) -> Vec<MoveRow> {
    desktops
        .into_iter()
        .map(|(label, windows, current)| MoveRow {
            label: label.to_owned(),
            detail: Some(if current {
                fl!("move-desktop-current")
            } else {
                crate::dbus::notifications::plain(fl!("move-desktop-windows", count = windows))
            }),
            current,
        })
        .chain([MoveRow {
            label: fl!("move-desktop-new"),
            detail: None,
            current: false,
        }])
        .collect()
}

/// Move the window to `to`, or to a new desktop, follow it, and say where it went.
fn move_and_follow(
    state: &mut State,
    surface: &CosmicSurface,
    seat: &Seat<State>,
    to: Option<crate::wayland::protocols::workspace::WorkspaceHandle>,
) {
    let moved = state.common.shell.write().move_to_desktop(
        surface,
        seat,
        to,
        &mut state.common.workspace_state.update(),
        &state.common.event_loop_handle,
    );
    if let Some((target, desktop)) = moved {
        Shell::set_focus(state, Some(&target), seat, None, true);
        toast(state, fl!("move-desktop-moved", desktop = desktop.as_str()));
    }
}

fn toast(state: &State, message: String) {
    state
        .common
        .dbus_state
        .system_toast(message, crate::dbus::notifications::Tone::Neutral);
}

/// A menu row that runs command `id` on the window, as of catalog `generation`.
fn command_item(
    surface: &CosmicSurface,
    seat: &Seat<State>,
    app: Option<&DesktopApp>,
    title: String,
    id: String,
    generation: Option<u32>,
) -> Item {
    let surface = surface.clone();
    let seat = seat.clone();
    let app = app.cloned();
    Item::new(title, move |handle| {
        let surface = surface.clone();
        let seat = seat.clone();
        let app = app.clone();
        let id = id.clone();
        handle.insert_idle(move |state| {
            run_command(state, &surface, Some(&seat), &id, app.as_ref(), generation);
        });
    })
}

/// The command palette — every verb this window answers, each pinnable to the
/// header. Opened from the app glyph.
#[allow(clippy::too_many_arguments)]
pub(super) fn open_commands(
    state: &mut State,
    surface: &CosmicSurface,
    seat: &Seat<State>,
    serial: Serial,
    start: GrabStartData,
    position: Point<i32, Global>,
    app: Option<DesktopApp>,
    on_escape: Option<crate::shell::grabs::EscapeHook>,
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
            let center = geometry.loc + Point::from((geometry.size.w / 2, geometry.size.h / 2));
            let output = shell
                .outputs()
                .find(|output| output.geometry().contains(center))
                .cloned()
                .unwrap_or_else(|| seat.active_output());
            (
                geometry,
                header_bar::halo_pill_bottom(&theme),
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
    let open = ui.with_program(|p| p.commands_open.clone());
    let on_desktop = shell.move_choices(surface).is_some();
    let resizable = {
        let min = surface.min_size_without_ssd();
        !(min.is_some() && min == surface.max_size_without_ssd())
    };
    drop(shell);
    let catalog = surface
        .wl_surface()
        .and_then(|surface| app_commands::committed(&surface));
    let generation = catalog.as_ref().map(|catalog| catalog.generation);
    let place: Box<dyn FnOnce(Size<i32, Logical>) -> Point<i32, Global>> = match anchor {
        Some((window, pill_bottom, output)) => {
            let theme = theme.clone();
            Box::new(move |card| palette_origin(&theme, window, pill_bottom, output, card))
        }
        None => Box::new(move |_| position),
    };
    let facts = commands::WindowFacts {
        recording: surface.is_recording(),
        maximized: surface.is_maximized(false),
        fullscreen: surface.is_fullscreen(false),
        resizable,
        on_desktop,
        app: app.as_ref(),
        catalog: catalog.as_ref(),
    };
    let mut list = commands::commands(&facts);
    for command in &mut list {
        let action = match command.id.as_str() {
            "minimize" => shortcuts::Action::Minimize,
            "maximize" => shortcuts::Action::Maximize,
            "fullscreen" => shortcuts::Action::Fullscreen,
            commands::MOVE_TO_DESKTOP => shortcuts::Action::MoveToDesktop,
            "close" => shortcuts::Action::Close,
            _ => continue,
        };
        command.shortcut = state
            .common
            .config
            .shortcut_for_action(&action)
            .map(|keys| commands::key_text(&keys));
    }
    // Index-aligned with the command list: a row press runs that command.
    let items = list.iter().map(|command| {
        command_item(
            surface,
            seat,
            app.as_ref(),
            command.label.clone(),
            command.id.clone(),
            generation,
        )
    });
    let scope = app
        .as_ref()
        .and_then(|app| app.name.clone())
        .unwrap_or_else(|| surface.app_id());
    let palette = Palette::new(surface.app_id(), scope, list.clone());
    open.store(true, Ordering::SeqCst);
    crate::frametrace::mark(format_args!("palette opened ({})", surface.app_id()));
    ui.force_update();
    let grab = MenuGrab::new_palette(
        start,
        seat,
        items.collect::<Vec<_>>().into_iter(),
        place,
        state.common.event_loop_handle.clone(),
        theme,
        palette,
    )
    .on_close(move || {
        crate::frametrace::mark("palette closed");
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
    if let Some(keyboard) = seat.get_keyboard() {
        keyboard.set_grab(
            state,
            PaletteKeyboardGrab::new(seat.clone()).on_escape(on_escape),
            serial,
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_chooser_lists_desktops_with_their_counts_then_a_new_one() {
        use crate::dbus::notifications::plain;
        let rows = move_rows([
            ("Main", 2, true),
            ("Desktop 2", 1, false),
            ("Desktop 3", 3, false),
        ]);
        let shown: Vec<_> = rows
            .iter()
            .map(|row| {
                (
                    row.label.as_str(),
                    row.detail.clone().map(plain),
                    row.current,
                )
            })
            .collect();
        assert_eq!(
            shown,
            [
                ("Main", Some("Current desktop".to_owned()), true),
                ("Desktop 2", Some("1 window".to_owned()), false),
                ("Desktop 3", Some("3 windows".to_owned()), false),
                ("New desktop", None, false),
            ]
        );
    }

    use smithay::input::SeatState;

    /// The palette is the window's: centred on it, hanging under its pill, and
    /// kept off the output's edges rather than following the click.
    #[test]
    fn the_palette_hangs_centred_under_the_pill_inside_the_output() {
        let mut theme = icetron_themes::dynamic::DEFAULT_THEME_PAIR.load(false);
        theme.window_header_style = icetron_themes::WindowHeaderStyle::Halo;
        theme.halo_style = header_bar::design_halo_style();
        let theme = crate::comp_theme::CompTheme::new(std::sync::Arc::new(theme), false);
        let output = Rectangle::<i32, Global>::new((0, 0).into(), (1920, 1080).into());
        let window = Rectangle::new((500, 300).into(), (800, 600).into());
        let card = Size::from((430, 300));
        assert_eq!(
            palette_origin(&theme, window, -4, output, card),
            Point::from((500 + 400 - 215, 300 + 22))
        );
        assert_eq!(palette_origin(&theme, output, 42, output, card).y, 42 + 26);
        // Against an edge it stops at the margin instead of leaving the output.
        let left = Rectangle::new((-100, -50).into(), (300, 600).into());
        assert_eq!(
            palette_origin(&theme, left, -4, output, card),
            Point::from((10, 10))
        );
        let right = Rectangle::new((1800, 300).into(), (300, 600).into());
        assert_eq!(
            palette_origin(&theme, right, -4, output, card).x,
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
        theme.halo_style = header_bar::design_halo_style();
        let theme = crate::comp_theme::CompTheme::new(std::sync::Arc::new(theme), false);
        let output = Rectangle::<i32, Global>::new((0, 0).into(), (1920, 1080).into());
        let low = Rectangle::new((500, 900).into(), (800, 600).into());
        let card = Size::from((430, 300));
        assert_eq!(
            palette_origin(&theme, low, -4, output, card).y,
            900 - 36 - 26 - 300
        );
        let window = Rectangle::new((500, 300).into(), (800, 600).into());
        let tall = Size::from((430, 800));
        assert_eq!(
            palette_origin(&theme, window, -4, output, tall).y,
            1080 - 10 - 800,
            "neither side fits, so it slides up to the bottom margin"
        );
        let huge = Size::from((430, 1200));
        assert_eq!(palette_origin(&theme, low, -4, output, huge).y, 10);
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
