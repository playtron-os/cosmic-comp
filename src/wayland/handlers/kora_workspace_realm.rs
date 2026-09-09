// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    delegate_kora_workspace_realm,
    state::State,
    wayland::protocols::{
        kora_workspace_realm::{RealmHandler, RealmState},
        workspace::WorkspaceGroupHandle,
    },
};

impl RealmHandler for State {
    fn realm_state(&self) -> &RealmState {
        &self.common.realm_state
    }

    fn realm_state_mut(&mut self) -> &mut RealmState {
        &mut self.common.realm_state
    }

    fn realm_groups(&self) -> Vec<(String, WorkspaceGroupHandle)> {
        self.common
            .shell
            .read()
            .realm_groups()
            .map(|(id, group)| (id.to_owned(), group))
            .collect()
    }
}

delegate_kora_workspace_realm!(State);
