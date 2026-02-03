// SPDX-License-Identifier: GPL-3.0-only

use crate::delegate_layer_surface_visibility;
use crate::state::State;
use crate::wayland::protocols::layer_surface_visibility::{
    LayerSurfaceVisibilityHandler, LayerSurfaceVisibilityState,
};

impl LayerSurfaceVisibilityHandler for State {
    fn layer_surface_visibility_state(&self) -> &LayerSurfaceVisibilityState {
        &self.common.layer_surface_visibility_state
    }

    fn set_surface_hidden(&mut self, surface_id: u32, hidden: bool) {
        let mut shell = self.common.shell.write();
        shell.set_surface_hidden(surface_id, hidden);
    }

    fn is_surface_hidden(&self, surface_id: u32) -> bool {
        let shell = self.common.shell.read();
        shell.is_surface_hidden(surface_id)
    }
}

delegate_layer_surface_visibility!(State);
