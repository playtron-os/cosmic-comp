// SPDX-License-Identifier: GPL-3.0-only

use crate::delegate_tooltip;
use crate::state::State;
use crate::wayland::protocols::tooltip::{TooltipHandler, TooltipManagerState};

impl TooltipHandler for State {
    fn tooltip_state(&mut self) -> &mut TooltipManagerState {
        &mut self.common.tooltip_state
    }
}

delegate_tooltip!(State);
