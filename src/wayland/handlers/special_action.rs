// SPDX-License-Identifier: GPL-3.0-only

use crate::delegate_special_action;
use crate::state::State;
use crate::wayland::protocols::special_action::{SpecialActionHandler, SpecialActionState};

impl SpecialActionHandler for State {
    fn special_action_state(&mut self) -> &mut SpecialActionState {
        &mut self.common.special_action_state
    }
}

delegate_special_action!(State);
