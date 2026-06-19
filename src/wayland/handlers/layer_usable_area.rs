// SPDX-License-Identifier: GPL-3.0-only

use smithay::output::Output;

use crate::{
    state::State,
    wayland::protocols::layer_usable_area::{
        UsableAreaHandler, UsableAreaState, delegate_layer_usable_area,
    },
};

impl UsableAreaHandler for State {
    fn usable_area_state(&mut self) -> &mut UsableAreaState {
        &mut self.common.usable_area_state
    }

    fn outputs(&self) -> impl Iterator<Item = Output> {
        let shell = self.common.shell.read();
        shell.outputs().cloned().collect::<Vec<_>>().into_iter()
    }
}

delegate_layer_usable_area!(State);
