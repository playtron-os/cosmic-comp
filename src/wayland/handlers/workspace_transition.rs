// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    delegate_workspace_transition,
    state::State,
    wayland::protocols::workspace_transition::{
        WorkspaceTransitionHandler, WorkspaceTransitionState,
    },
};

impl WorkspaceTransitionHandler for State {
    fn workspace_transition_state(&mut self) -> &mut WorkspaceTransitionState {
        &mut self.common.workspace_transition_state
    }
}

delegate_workspace_transition!(State);
