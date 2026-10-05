// SPDX-License-Identifier: GPL-3.0-only

use smithay::{
    reexports::{
        wayland_protocols::{
            ext::foreign_toplevel_list::v1::server::ext_foreign_toplevel_handle_v1::ExtForeignToplevelHandleV1,
            xdg::shell::server::xdg_toplevel::XdgToplevel,
        },
        wayland_server::{Client, Resource},
    },
    utils::IsAlive,
    wayland::foreign_toplevel_list::ForeignToplevelHandle,
};

use crate::{
    state::State,
    wayland::protocols::{
        kora_toplevel_identity::{
            IdentityHandler, IdentityState, ToplevelIdentity, delegate_kora_toplevel_identity,
        },
        toplevel_info::{mapped_toplevel_identifier, window_from_ext_handle},
    },
};

impl IdentityHandler for State {
    fn identity_state(&mut self) -> &mut IdentityState {
        &mut self.common.toplevel_identity_state
    }

    fn client_workspace(&self, client: &Client) -> Option<String> {
        crate::workspace_tag::of_client(client)
    }

    fn owned_identity(&self, toplevel: &XdgToplevel) -> Option<ToplevelIdentity> {
        let window = self
            .common
            .toplevel_info_state
            .mapped_toplevels()
            .find(|window| {
                window.alive()
                    && window
                        .0
                        .toplevel()
                        .is_some_and(|surface| surface.xdg_toplevel() == toplevel)
            })?;
        Some(ToplevelIdentity {
            identifier: mapped_toplevel_identifier(window)?,
            workspace: self
                .common
                .shell
                .read()
                .client_workspace(window)
                .unwrap_or_default(),
        })
    }

    fn foreign_identity(&self, toplevel: &ExtForeignToplevelHandleV1) -> Option<ToplevelIdentity> {
        let client = toplevel.client()?;
        let handle = ForeignToplevelHandle::from_resource(toplevel)?;
        if handle.is_closed() || !handle.resources_for_client(&client).contains(toplevel) {
            return None;
        }
        let window = window_from_ext_handle(self, toplevel)?;
        if !window.alive() {
            return None;
        }
        let workspace = self.common.shell.read().client_workspace(window);
        Some(ToplevelIdentity {
            identifier: handle.identifier(),
            workspace: workspace.unwrap_or_default(),
        })
    }
}

delegate_kora_toplevel_identity!(State);
