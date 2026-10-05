// SPDX-License-Identifier: GPL-3.0-only

use cosmic_protocols::kora_toplevel_identity::v1::server::{
    kora_toplevel_identity_handle_v1::{self, KoraToplevelIdentityHandleV1},
    kora_toplevel_identity_v1::{self, KoraToplevelIdentityV1},
};
use smithay::reexports::{
    wayland_protocols::{
        ext::foreign_toplevel_list::v1::server::ext_foreign_toplevel_handle_v1::ExtForeignToplevelHandleV1,
        xdg::shell::server::xdg_toplevel::XdgToplevel,
    },
    wayland_server::{
        Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource, Weak,
        backend::{ClientId, GlobalId},
    },
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ToplevelIdentity {
    pub identifier: String,
    pub workspace: String,
}

#[derive(Clone, Debug)]
enum Target {
    Owned(Weak<XdgToplevel>),
    Foreign(Weak<ExtForeignToplevelHandleV1>),
}

#[derive(Debug)]
struct Subscription {
    handle: KoraToplevelIdentityHandleV1,
    target: Target,
    sent: Option<ToplevelIdentity>,
}

#[derive(Debug)]
pub struct IdentityState {
    _global: GlobalId,
    subscriptions: Vec<Subscription>,
}

pub trait IdentityHandler {
    fn identity_state(&mut self) -> &mut IdentityState;
    fn client_workspace(&self, client: &Client) -> Option<String>;
    fn owned_identity(&self, toplevel: &XdgToplevel) -> Option<ToplevelIdentity>;
    fn foreign_identity(&self, toplevel: &ExtForeignToplevelHandleV1) -> Option<ToplevelIdentity>;
}

impl IdentityState {
    pub fn new<D>(dh: &DisplayHandle) -> Self
    where
        D: GlobalDispatch<KoraToplevelIdentityV1, ()> + 'static,
    {
        Self {
            _global: dh.create_global::<D, KoraToplevelIdentityV1, _>(1, ()),
            subscriptions: Vec::new(),
        }
    }

    pub fn refresh<D: IdentityHandler>(state: &mut D) {
        let subscriptions = std::mem::take(&mut state.identity_state().subscriptions);
        let mut live = Vec::new();
        for mut subscription in subscriptions {
            let handle = &subscription.handle;
            if !handle.is_alive() {
                continue;
            }
            let Some(client) = handle.client() else {
                continue;
            };
            let (identity, waiting) = match &subscription.target {
                Target::Owned(target) => match target.upgrade() {
                    Ok(target) if target.id().same_client_as(&handle.id()) => {
                        (state.owned_identity(&target), subscription.sent.is_none())
                    }
                    _ => (None, false),
                },
                Target::Foreign(target) => match target.upgrade() {
                    Ok(target) if target.id().same_client_as(&handle.id()) => {
                        let caller = state.client_workspace(&client);
                        let identity = state.foreign_identity(&target).filter(|identity| {
                            let owner = (!identity.workspace.is_empty())
                                .then_some(identity.workspace.as_str());
                            crate::workspace_tag::visible_in(owner, caller.as_deref())
                        });
                        (identity, false)
                    }
                    _ => (None, false),
                },
            };
            let Some(identity) = identity else {
                if waiting {
                    live.push(subscription);
                } else {
                    handle.closed();
                }
                continue;
            };
            if let Some(sent) = &subscription.sent {
                if sent != &identity {
                    handle.closed();
                    continue;
                }
            } else {
                handle.identifier(identity.identifier.clone());
                handle.workspace(identity.workspace.clone());
                handle.done();
                subscription.sent = Some(identity);
            }
            live.push(subscription);
        }
        state.identity_state().subscriptions = live;
    }
}

impl<D> GlobalDispatch<KoraToplevelIdentityV1, (), D> for IdentityState
where
    D: GlobalDispatch<KoraToplevelIdentityV1, ()> + Dispatch<KoraToplevelIdentityV1, ()> + 'static,
{
    fn bind(
        _state: &mut D,
        _dh: &DisplayHandle,
        _client: &Client,
        resource: New<KoraToplevelIdentityV1>,
        _data: &(),
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }
}

impl<D> Dispatch<KoraToplevelIdentityV1, (), D> for IdentityState
where
    D: Dispatch<KoraToplevelIdentityV1, ()>
        + Dispatch<KoraToplevelIdentityHandleV1, ()>
        + IdentityHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        _resource: &KoraToplevelIdentityV1,
        request: kora_toplevel_identity_v1::Request,
        _data: &(),
        _dh: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        let (id, target) = match request {
            kora_toplevel_identity_v1::Request::GetIdentity { id, toplevel } => {
                (id, Target::Owned(toplevel.downgrade()))
            }
            kora_toplevel_identity_v1::Request::GetForeignIdentity { id, toplevel } => {
                (id, Target::Foreign(toplevel.downgrade()))
            }
            _ => return,
        };
        state.identity_state().subscriptions.push(Subscription {
            handle: data_init.init(id, ()),
            target,
            sent: None,
        });
        Self::refresh(state);
    }
}

impl<D> Dispatch<KoraToplevelIdentityHandleV1, (), D> for IdentityState
where
    D: Dispatch<KoraToplevelIdentityHandleV1, ()> + IdentityHandler,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        _resource: &KoraToplevelIdentityHandleV1,
        _request: kora_toplevel_identity_handle_v1::Request,
        _data: &(),
        _dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
    }

    fn destroyed(
        state: &mut D,
        _client: ClientId,
        resource: &KoraToplevelIdentityHandleV1,
        _data: &(),
    ) {
        state
            .identity_state()
            .subscriptions
            .retain(|s| s.handle != *resource);
    }
}

macro_rules! delegate_kora_toplevel_identity {
    ($ty:ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($ty: [
            cosmic_protocols::kora_toplevel_identity::v1::server::kora_toplevel_identity_v1::KoraToplevelIdentityV1: ()
        ] => $crate::wayland::protocols::kora_toplevel_identity::IdentityState);
        smithay::reexports::wayland_server::delegate_dispatch!($ty: [
            cosmic_protocols::kora_toplevel_identity::v1::server::kora_toplevel_identity_v1::KoraToplevelIdentityV1: ()
        ] => $crate::wayland::protocols::kora_toplevel_identity::IdentityState);
        smithay::reexports::wayland_server::delegate_dispatch!($ty: [
            cosmic_protocols::kora_toplevel_identity::v1::server::kora_toplevel_identity_handle_v1::KoraToplevelIdentityHandleV1: ()
        ] => $crate::wayland::protocols::kora_toplevel_identity::IdentityState);
    };
}
pub(crate) use delegate_kora_toplevel_identity;

#[cfg(test)]
#[path = "kora_toplevel_identity_tests.rs"]
mod tests;
