// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    delegate_kora_workspace_realm,
    state::{ClientState, State},
    wayland::protocols::{
        kora_workspace_realm::{RealmHandler, RealmState},
        workspace::WorkspaceGroupHandle,
    },
};
use smithay::reexports::wayland_server::{Client, Resource, protocol::wl_surface::WlSurface};

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

    fn client_workspace(&self, client: &Client) -> Option<String> {
        client
            .get_data::<ClientState>()
            .and_then(|state| state.workspace.clone())
    }

    fn assign_layer_realm(&mut self, surface: &WlSurface, id: String) {
        let outputs = {
            let mut shell = self.common.shell.write();
            shell.assign_layer_realm(surface.id(), id);
            shell.outputs().cloned().collect::<Vec<_>>()
        };
        // Whether it shows may have just changed.
        for output in outputs {
            self.backend.schedule_render(&output);
        }
    }
}

delegate_kora_workspace_realm!(State);
