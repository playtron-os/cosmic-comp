// SPDX-License-Identifier: GPL-3.0-only

use crate::delegate_home_visibility;
use crate::state::State;
use crate::wayland::protocols::home_visibility::{
    HomeVisibilityHandler, HomeVisibilityState, VisibilityMode,
};

impl HomeVisibilityHandler for State {
    fn home_visibility_state(&self) -> &HomeVisibilityState {
        &self.common.home_visibility_state
    }

    fn set_surface_visibility_mode(&mut self, surface_id: u32, mode: VisibilityMode) {
        let mut shell = self.common.shell.write();
        shell.set_surface_visibility_mode(surface_id, mode);
    }

    fn remove_surface_visibility(&mut self, surface_id: u32) {
        let mut shell = self.common.shell.write();
        shell.remove_surface_visibility(surface_id);
    }
}

delegate_home_visibility!(State);
