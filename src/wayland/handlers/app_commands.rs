// SPDX-License-Identifier: GPL-3.0-only

use smithay::{
    input::Seat,
    reexports::wayland_server::protocol::{wl_seat::WlSeat, wl_surface::WlSurface},
    utils::Serial,
    wayland::seat::WaylandFocus,
};

use crate::{
    shell::{CosmicSurface, element::window::CosmicWindow, focus::target::KeyboardFocusTarget},
    state::State,
};

fn is(window: &CosmicSurface, surface: &WlSurface) -> bool {
    window.wl_surface().as_deref() == Some(surface)
}

impl State {
    /// A window committed new commands: redraw its Halo, whose tray and `+` follow them.
    pub fn app_commands_changed(&mut self, surface: &WlSurface) {
        let shell = self.common.shell.read();
        let window = shell
            .element_for_surface(surface)
            .and_then(|mapped| {
                mapped
                    .windows()
                    .map(|(window, _)| window)
                    .find(|window| is(window, surface))
            })
            .or_else(|| {
                shell
                    .workspaces()
                    .spaces()
                    .flat_map(|workspace| workspace.get_fullscreen_surfaces())
                    .find(|fullscreen| is(&fullscreen.surface, surface))
                    .map(|fullscreen| fullscreen.surface.clone())
            });
        if let Some(window) = window {
            CosmicWindow::refresh_app_halos(&shell, &window.app_id());
        }
    }

    /// The window's own palette shortcut, honoured only for the focused window
    /// and an input event it received since it got the keyboard.
    pub fn app_commands_palette(&mut self, surface: &WlSurface, seat: &WlSeat, serial: Serial) {
        let Some(seat) = Seat::<State>::from_resource(seat) else {
            return;
        };
        let Some(keyboard) = seat.get_keyboard() else {
            return;
        };
        if !keyboard
            .last_enter()
            .is_some_and(|enter| serial.is_no_older_than(&enter))
        {
            return;
        }
        let shell = self.common.shell.read();
        let handle = &self.common.event_loop_handle;
        match keyboard.current_focus() {
            Some(KeyboardFocusTarget::Element(mapped))
                if is(&mapped.active_window(), surface) && !mapped.is_minimized() =>
            {
                mapped.show_commands(&seat, handle);
            }
            Some(KeyboardFocusTarget::Fullscreen(window)) if is(&window, surface) => {
                if let Some(fullscreen) = shell
                    .workspaces()
                    .spaces()
                    .flat_map(|workspace| workspace.get_fullscreen_surfaces())
                    .find(|fullscreen| fullscreen.surface == window)
                {
                    fullscreen.halo.show_commands(&seat, handle);
                }
            }
            _ => {}
        }
    }
}
