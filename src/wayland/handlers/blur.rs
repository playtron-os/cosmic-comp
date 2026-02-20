// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use crate::wayland::protocols::blur::{BlurHandler, BlurState, delegate_blur};
use smithay::reexports::wayland_server::Resource;
use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;

impl BlurHandler for State {
    fn blur_state(&mut self) -> &mut BlurState {
        &mut self.common.blur_state
    }

    fn blur_set(&mut self, surface: &WlSurface) {
        let shell = self.common.shell.read();
        let output = shell.visible_output_for_surface(surface).cloned();
        std::mem::drop(shell);

        let surface_id = surface.id();
        self.common.shell.write().restart_layer_fade_in(surface_id);

        if let Some(output) = output {
            self.backend.schedule_render(&output);
        }
    }

    fn blur_unset(&mut self, surface: &WlSurface) {
        if let Some(output) = self.common.shell.read().visible_output_for_surface(surface) {
            self.backend.schedule_render(output);
        }
    }
}

delegate_blur!(State);
