// SPDX-License-Identifier: GPL-3.0-only

//! Tells shell components when a workspace switch is animating, and for how
//! long, so they can fade their contents across it rather than swapping
//! mid-animation.

// Re-export generated types
pub use generated::workspace_transition_manager_v1;

// `unused_imports`: scanner boilerplate for protocols referencing wl_ objects.
#[allow(
    non_snake_case,
    non_upper_case_globals,
    non_camel_case_types,
    unused_imports
)]
mod generated {
    use smithay::reexports::wayland_server::{self, protocol::*};

    pub mod __interfaces {
        use smithay::reexports::wayland_server::protocol::__interfaces::*;
        use wayland_backend;
        wayland_scanner::generate_interfaces!("resources/protocols/workspace-transition.xml");
    }
    use self::__interfaces::*;

    wayland_scanner::generate_server_code!("resources/protocols/workspace-transition.xml");
}

use smithay::reexports::wayland_server::{
    Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, backend::GlobalId,
};

/// Bound managers, and the switch currently animating.
#[derive(Debug)]
pub struct WorkspaceTransitionState {
    global: GlobalId,
    instances: Vec<workspace_transition_manager_v1::WorkspaceTransitionManagerV1>,
    /// The workspace a `started` was sent for and no `finished` yet, so an
    /// interrupted switch still pairs up.
    animating_to: Option<String>,
}

impl WorkspaceTransitionState {
    pub fn new<D>(dh: &DisplayHandle) -> WorkspaceTransitionState
    where
        D: GlobalDispatch<workspace_transition_manager_v1::WorkspaceTransitionManagerV1, ()>
            + Dispatch<workspace_transition_manager_v1::WorkspaceTransitionManagerV1, ()>
            + 'static,
    {
        let global = dh
            .create_global::<D, workspace_transition_manager_v1::WorkspaceTransitionManagerV1, _>(
                1,
                (),
            );
        WorkspaceTransitionState {
            global,
            instances: Vec::new(),
            animating_to: None,
        }
    }

    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }

    /// A switch began animating. Finishes one still in flight first, so a
    /// client fading on `started` is never left mid-fade.
    pub fn started(&mut self, from: Option<&str>, to: &str, duration_ms: u32) {
        if self.animating_to.is_some() {
            self.finished();
        }
        for instance in &self.instances {
            instance.started(from.map(ToString::to_string), to.to_string(), duration_ms);
        }
        self.animating_to = Some(to.to_string());
    }

    /// The switch in flight finished animating. A no-op when none is.
    pub fn finished(&mut self) {
        let Some(to) = self.animating_to.take() else {
            return;
        };
        for instance in &self.instances {
            instance.finished(to.clone());
        }
    }

    /// Forget a manager whose client has gone.
    fn remove(&mut self, resource: &workspace_transition_manager_v1::WorkspaceTransitionManagerV1) {
        self.instances.retain(|i| i != resource);
    }
}

/// Reaches the state from the compositor's own.
pub trait WorkspaceTransitionHandler {
    fn workspace_transition_state(&mut self) -> &mut WorkspaceTransitionState;
}

impl<D> GlobalDispatch<workspace_transition_manager_v1::WorkspaceTransitionManagerV1, (), D>
    for WorkspaceTransitionState
where
    D: GlobalDispatch<workspace_transition_manager_v1::WorkspaceTransitionManagerV1, ()>
        + Dispatch<workspace_transition_manager_v1::WorkspaceTransitionManagerV1, ()>
        + WorkspaceTransitionHandler
        + 'static,
{
    fn bind(
        state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<workspace_transition_manager_v1::WorkspaceTransitionManagerV1>,
        _global_data: &(),
        data_init: &mut DataInit<'_, D>,
    ) {
        // Nothing is replayed: a client binding mid-switch has nothing to fade.
        let resource = data_init.init(resource, ());
        state.workspace_transition_state().instances.push(resource);
    }
}

impl<D> Dispatch<workspace_transition_manager_v1::WorkspaceTransitionManagerV1, (), D>
    for WorkspaceTransitionState
where
    D: GlobalDispatch<workspace_transition_manager_v1::WorkspaceTransitionManagerV1, ()>
        + Dispatch<workspace_transition_manager_v1::WorkspaceTransitionManagerV1, ()>
        + WorkspaceTransitionHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        resource: &workspace_transition_manager_v1::WorkspaceTransitionManagerV1,
        request: workspace_transition_manager_v1::Request,
        _data: &(),
        _dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            workspace_transition_manager_v1::Request::Destroy => {
                state.workspace_transition_state().remove(resource);
            }
        }
    }

    fn destroyed(
        state: &mut D,
        _client: smithay::reexports::wayland_server::backend::ClientId,
        resource: &workspace_transition_manager_v1::WorkspaceTransitionManagerV1,
        _data: &(),
    ) {
        // A client that goes away without destroying leaves a dead resource.
        state.workspace_transition_state().remove(resource);
    }
}

#[macro_export]
macro_rules! delegate_workspace_transition {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::workspace_transition::workspace_transition_manager_v1::WorkspaceTransitionManagerV1: ()
        ] => $crate::wayland::protocols::workspace_transition::WorkspaceTransitionState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::workspace_transition::workspace_transition_manager_v1::WorkspaceTransitionManagerV1: ()
        ] => $crate::wayland::protocols::workspace_transition::WorkspaceTransitionState);
    };
}
