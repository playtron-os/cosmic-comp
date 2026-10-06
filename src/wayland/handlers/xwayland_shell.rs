// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use smithay::{
    backend::renderer::utils::with_renderer_surface_state,
    reexports::wayland_server::protocol::wl_surface::WlSurface,
    wayland::xwayland_shell::{XWaylandShellHandler, XWaylandShellState},
    xwayland::{X11Surface, xwm::XwmId},
};

impl XWaylandShellHandler for State {
    fn xwayland_shell_state(&mut self) -> &mut XWaylandShellState {
        &mut self.common.xwayland_shell_state
    }

    fn surface_associated(&mut self, _xwm: XwmId, surface: WlSurface, window: X11Surface) {
        // Association can arrive after the first buffer, or inside its commit hook.
        // Recheck after dispatch so either ordering sees the committed buffer.
        self.common.event_loop_handle.insert_idle(move |state| {
            if window.wl_surface().as_ref() != Some(&surface)
                || !with_renderer_surface_state(&surface, |s| s.buffer().is_some()).unwrap_or(false)
            {
                return;
            }
            let pending = state
                .common
                .shell
                .read()
                .pending_windows
                .iter()
                .find(|pending| {
                    pending.frame_notified && pending.surface.x11_surface() == Some(&window)
                })
                .map(|pending| pending.surface.clone());
            if let Some(pending) = pending {
                state.map_x11_window_now(&pending);
                if let Some(output) = state
                    .common
                    .shell
                    .read()
                    .visible_output_for_surface(&surface)
                {
                    state.backend.schedule_render(output);
                }
            }
        });
    }
}
