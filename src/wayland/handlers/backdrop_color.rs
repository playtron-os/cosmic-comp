// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use crate::wayland::protocols::backdrop_color::{
    BackdropColorHandler, BackdropColorState, delegate_backdrop_color,
};

impl BackdropColorHandler for State {
    fn backdrop_color_state(&mut self) -> &mut BackdropColorState {
        &mut self.common.backdrop_color_state
    }
}

delegate_backdrop_color!(State);
