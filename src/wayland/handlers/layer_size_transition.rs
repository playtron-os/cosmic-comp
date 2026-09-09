// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    state::State,
    wayland::protocols::layer_size_transition::{
        SizeTransitionHandler, SizeTransitionState, delegate_layer_size_transition,
    },
};

impl SizeTransitionHandler for State {
    fn size_transition_state(&mut self) -> &mut SizeTransitionState {
        &mut self.common.size_transition_state
    }
}

delegate_layer_size_transition!(State);
