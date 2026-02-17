// SPDX-License-Identifier: GPL-3.0-only

use crate::shell::auto_hide::{AutoHideEdge, AutoHideMode};
use crate::state::State;
use crate::wayland::protocols::layer_auto_hide::{
    LayerAutoHideHandler, LayerAutoHideState, delegate_layer_auto_hide,
};
use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;

impl LayerAutoHideHandler for State {
    fn layer_auto_hide_state(&mut self) -> &mut LayerAutoHideState {
        &mut self.common.layer_auto_hide_state
    }

    fn auto_hide_registered(&mut self, surface: &WlSurface, edge: AutoHideEdge, mode: AutoHideMode) {
        let mut shell = self.common.shell.write();
        shell.register_auto_hide(surface, edge, mode);
    }

    fn auto_hide_unregistered(&mut self, surface: &WlSurface) {
        let mut shell = self.common.shell.write();
        shell.unregister_auto_hide(surface);
    }
}

delegate_layer_auto_hide!(State);
