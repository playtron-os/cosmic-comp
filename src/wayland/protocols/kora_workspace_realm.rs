// SPDX-License-Identifier: GPL-3.0-only

//! Tells a client which Kora workspace (realm) each desktop group belongs to,
//! which `ext_workspace_v1` alone does not say.

pub use generated::kora_workspace_realm_manager_v1;

// `unused_imports`: scanner boilerplate for protocols referencing wl_ objects.
#[allow(
    non_snake_case,
    non_upper_case_globals,
    non_camel_case_types,
    unused_imports
)]
mod generated {
    use smithay::reexports::wayland_protocols::ext::workspace::v1::server::*;
    use smithay::reexports::wayland_server::{self, protocol::*};

    pub mod __interfaces {
        use smithay::reexports::wayland_protocols::ext::workspace::v1::server::__interfaces::*;
        use smithay::reexports::wayland_server::protocol::__interfaces::*;
        use wayland_backend;
        wayland_scanner::generate_interfaces!("resources/protocols/kora-workspace-realm.xml");
    }
    use self::__interfaces::*;

    wayland_scanner::generate_server_code!("resources/protocols/kora-workspace-realm.xml");
}

use std::collections::HashSet;

use smithay::reexports::wayland_server::{
    Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource,
    backend::{GlobalId, ObjectId},
    protocol::wl_surface::WlSurface,
};

use super::workspace::{WorkspaceGroupHandle, WorkspaceHandler};
use kora_workspace_realm_manager_v1::KoraWorkspaceRealmManagerV1;

#[derive(Debug)]
pub struct RealmState {
    global: GlobalId,
    instances: Vec<KoraWorkspaceRealmManagerV1>,
    /// Groups each manager has already been told about.
    sent: HashSet<(ObjectId, WorkspaceGroupHandle)>,
}

impl RealmState {
    pub fn new<D>(dh: &DisplayHandle) -> RealmState
    where
        D: GlobalDispatch<KoraWorkspaceRealmManagerV1, ()>
            + Dispatch<KoraWorkspaceRealmManagerV1, ()>
            + 'static,
    {
        let global = dh.create_global::<D, KoraWorkspaceRealmManagerV1, _>(2, ());
        RealmState {
            global,
            instances: Vec::new(),
            sent: HashSet::new(),
        }
    }

    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }

    /// Announce every group a bound client can see but has not heard about.
    /// Runs after the workspace protocol's own refresh, so the group object it
    /// names already exists on the client.
    pub fn refresh<D>(state: &mut D)
    where
        D: RealmHandler + WorkspaceHandler + 'static,
    {
        let groups = state.realm_groups();
        let live: HashSet<WorkspaceGroupHandle> = groups.iter().map(|(_, g)| *g).collect();
        let mut sends = Vec::new();
        {
            let realm = state.realm_state();
            let workspaces = state.workspace_state();
            for mngr in &realm.instances {
                let Some(client) = mngr.client() else {
                    continue;
                };
                for (id, group) in &groups {
                    if realm.sent.contains(&(mngr.id(), *group)) {
                        continue;
                    }
                    let announced = workspaces
                        .group_ext_instances(group)
                        .iter()
                        .find(|obj| obj.client().is_some_and(|c| c.id() == client.id()));
                    if let Some(obj) = announced {
                        sends.push((mngr.clone(), obj.clone(), id.clone(), *group));
                    }
                }
            }
        }
        let realm = state.realm_state_mut();
        realm.sent.retain(|(_, group)| live.contains(group));
        for (mngr, obj, id, group) in sends {
            mngr.realm(&obj, id);
            realm.sent.insert((mngr.id(), group));
        }
    }

    fn remove(&mut self, resource: &KoraWorkspaceRealmManagerV1) {
        self.instances.retain(|i| i != resource);
        self.sent.retain(|(id, _)| *id != resource.id());
    }
}

pub trait RealmHandler {
    fn realm_state(&self) -> &RealmState;
    fn realm_state_mut(&mut self) -> &mut RealmState;
    /// Every desktop group with the id of the workspace that owns it.
    fn realm_groups(&self) -> Vec<(String, WorkspaceGroupHandle)>;
    /// The workspace `client` was launched into; `None` is machine-plane.
    fn client_workspace(&self, client: &Client) -> Option<String>;
    /// A machine-plane client put `surface`'s layer surface in workspace `id`.
    fn assign_layer_realm(&mut self, surface: &WlSurface, id: String);
}

impl<D> GlobalDispatch<KoraWorkspaceRealmManagerV1, (), D> for RealmState
where
    D: GlobalDispatch<KoraWorkspaceRealmManagerV1, ()>
        + Dispatch<KoraWorkspaceRealmManagerV1, ()>
        + RealmHandler
        + 'static,
{
    fn bind(
        state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<KoraWorkspaceRealmManagerV1>,
        _global_data: &(),
        data_init: &mut DataInit<'_, D>,
    ) {
        // The next refresh announces every group this client can see.
        let resource = data_init.init(resource, ());
        state.realm_state_mut().instances.push(resource);
    }
}

impl<D> Dispatch<KoraWorkspaceRealmManagerV1, (), D> for RealmState
where
    D: GlobalDispatch<KoraWorkspaceRealmManagerV1, ()>
        + Dispatch<KoraWorkspaceRealmManagerV1, ()>
        + RealmHandler
        + 'static,
{
    fn request(
        state: &mut D,
        client: &Client,
        resource: &KoraWorkspaceRealmManagerV1,
        request: kora_workspace_realm_manager_v1::Request,
        _data: &(),
        _dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            kora_workspace_realm_manager_v1::Request::Destroy => {
                state.realm_state_mut().remove(resource);
            }
            kora_workspace_realm_manager_v1::Request::Assign { surface, id } => {
                // A client inside a workspace has its layer surfaces there
                // already; letting it name another would be a way out.
                if let Some(own) = state.client_workspace(client) {
                    resource.post_error(
                        kora_workspace_realm_manager_v1::Error::NotPermitted,
                        format!("a client in workspace {own} cannot assign surfaces to workspaces"),
                    );
                    return;
                }
                state.assign_layer_realm(&surface, id);
            }
        }
    }

    fn destroyed(
        state: &mut D,
        _client: smithay::reexports::wayland_server::backend::ClientId,
        resource: &KoraWorkspaceRealmManagerV1,
        _data: &(),
    ) {
        state.realm_state_mut().remove(resource);
    }
}

#[macro_export]
macro_rules! delegate_kora_workspace_realm {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::kora_workspace_realm::kora_workspace_realm_manager_v1::KoraWorkspaceRealmManagerV1: ()
        ] => $crate::wayland::protocols::kora_workspace_realm::RealmState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::kora_workspace_realm::kora_workspace_realm_manager_v1::KoraWorkspaceRealmManagerV1: ()
        ] => $crate::wayland::protocols::kora_workspace_realm::RealmState);
    };
}
