// SPDX-License-Identifier: GPL-3.0-only

//! Implementation of the COSMIC layer surface dismiss protocol (zcosmic_layer_surface_dismiss_v1)
//!
//! This protocol allows layer-shell clients to receive notifications when
//! user interaction occurs outside their surfaces. This enables "close on
//! click outside" behavior for popup menus without changing keyboard focus.

pub use generated::{zcosmic_layer_surface_dismiss_manager_v1, zcosmic_layer_surface_dismiss_v1};

#[allow(non_snake_case, non_upper_case_globals, non_camel_case_types)]
mod generated {
    use smithay::reexports::wayland_server::{self, protocol::*};

    pub mod __interfaces {
        use smithay::reexports::wayland_server::protocol::__interfaces::*;
        use wayland_backend;
        wayland_scanner::generate_interfaces!("resources/protocols/layer_surface_dismiss.xml");
    }
    use self::__interfaces::*;

    wayland_scanner::generate_server_code!("resources/protocols/layer_surface_dismiss.xml");
}

use smithay::reexports::wayland_server::{
    Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource, Weak,
    backend::GlobalId, protocol::wl_surface::WlSurface,
};
use std::collections::HashSet;
use std::sync::Mutex;
use tracing::{debug, info, warn};

/// User data for the dismiss controller
pub struct LayerSurfaceDismissControllerData {
    /// The wl_surface this controls (should be a layer-shell surface)
    pub surface: Weak<WlSurface>,
    /// Whether dismiss notifications are currently armed
    pub armed: Mutex<bool>,
    /// Surfaces in the dismiss group (clicks on these won't trigger dismiss)
    pub group_surfaces: Mutex<HashSet<u32>>,
}

/// State for the layer surface dismiss manager protocol
pub struct LayerSurfaceDismissState {
    global: GlobalId,
}

impl std::fmt::Debug for LayerSurfaceDismissState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LayerSurfaceDismissState").finish()
    }
}

impl LayerSurfaceDismissState {
    /// Create a new layer surface dismiss manager global
    pub fn new<D>(dh: &DisplayHandle) -> LayerSurfaceDismissState
    where
        D: GlobalDispatch<
                zcosmic_layer_surface_dismiss_manager_v1::ZcosmicLayerSurfaceDismissManagerV1,
                (),
            > + Dispatch<
                zcosmic_layer_surface_dismiss_manager_v1::ZcosmicLayerSurfaceDismissManagerV1,
                (),
            > + Dispatch<
                zcosmic_layer_surface_dismiss_v1::ZcosmicLayerSurfaceDismissV1,
                LayerSurfaceDismissControllerData,
            > + LayerSurfaceDismissHandler
            + 'static,
    {
        let global = dh.create_global::<
            D,
            zcosmic_layer_surface_dismiss_manager_v1::ZcosmicLayerSurfaceDismissManagerV1,
            _,
        >(1, ());
        LayerSurfaceDismissState { global }
    }

    /// Get the global ID
    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }
}

/// Handler trait for layer surface dismiss protocol
pub trait LayerSurfaceDismissHandler {
    /// Get the layer surface dismiss state
    fn layer_surface_dismiss_state(&self) -> &LayerSurfaceDismissState;

    /// Register a dismiss controller for tracking
    fn register_dismiss_controller(
        &mut self,
        controller: zcosmic_layer_surface_dismiss_v1::ZcosmicLayerSurfaceDismissV1,
    );

    /// Unregister a dismiss controller
    fn unregister_dismiss_controller(&mut self, surface_id: u32);

    /// Get all armed dismiss controllers
    fn get_armed_dismiss_controllers(
        &self,
    ) -> Vec<zcosmic_layer_surface_dismiss_v1::ZcosmicLayerSurfaceDismissV1>;
}

impl<D>
    GlobalDispatch<
        zcosmic_layer_surface_dismiss_manager_v1::ZcosmicLayerSurfaceDismissManagerV1,
        (),
        D,
    > for LayerSurfaceDismissState
where
    D: GlobalDispatch<
            zcosmic_layer_surface_dismiss_manager_v1::ZcosmicLayerSurfaceDismissManagerV1,
            (),
        > + Dispatch<zcosmic_layer_surface_dismiss_manager_v1::ZcosmicLayerSurfaceDismissManagerV1, ()>
        + Dispatch<
            zcosmic_layer_surface_dismiss_v1::ZcosmicLayerSurfaceDismissV1,
            LayerSurfaceDismissControllerData,
        > + LayerSurfaceDismissHandler
        + 'static,
{
    fn bind(
        _state: &mut D,
        _dh: &DisplayHandle,
        _client: &Client,
        resource: New<
            zcosmic_layer_surface_dismiss_manager_v1::ZcosmicLayerSurfaceDismissManagerV1,
        >,
        _global_data: &(),
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }
}

impl<D>
    Dispatch<zcosmic_layer_surface_dismiss_manager_v1::ZcosmicLayerSurfaceDismissManagerV1, (), D>
    for LayerSurfaceDismissState
where
    D: GlobalDispatch<
            zcosmic_layer_surface_dismiss_manager_v1::ZcosmicLayerSurfaceDismissManagerV1,
            (),
        > + Dispatch<zcosmic_layer_surface_dismiss_manager_v1::ZcosmicLayerSurfaceDismissManagerV1, ()>
        + Dispatch<
            zcosmic_layer_surface_dismiss_v1::ZcosmicLayerSurfaceDismissV1,
            LayerSurfaceDismissControllerData,
        > + LayerSurfaceDismissHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        _resource: &zcosmic_layer_surface_dismiss_manager_v1::ZcosmicLayerSurfaceDismissManagerV1,
        request: zcosmic_layer_surface_dismiss_manager_v1::Request,
        _data: &(),
        _dh: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_layer_surface_dismiss_manager_v1::Request::Destroy => {
                // Nothing to do, global persists
            }
            zcosmic_layer_surface_dismiss_manager_v1::Request::GetDismissController {
                id,
                surface,
            } => {
                let surface_id = surface.id().protocol_id();
                debug!(surface_id, "Creating dismiss controller for layer surface");

                // Include the surface itself in its own group
                let mut group_surfaces = HashSet::new();
                group_surfaces.insert(surface_id);

                let controller_data = LayerSurfaceDismissControllerData {
                    surface: surface.downgrade(),
                    armed: Mutex::new(false),
                    group_surfaces: Mutex::new(group_surfaces),
                };

                let controller = data_init.init(id, controller_data);

                // Register the controller for tracking
                state.register_dismiss_controller(controller);
            }
        }
    }
}

impl<D>
    Dispatch<
        zcosmic_layer_surface_dismiss_v1::ZcosmicLayerSurfaceDismissV1,
        LayerSurfaceDismissControllerData,
        D,
    > for LayerSurfaceDismissState
where
    D: Dispatch<
            zcosmic_layer_surface_dismiss_v1::ZcosmicLayerSurfaceDismissV1,
            LayerSurfaceDismissControllerData,
        > + LayerSurfaceDismissHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        _resource: &zcosmic_layer_surface_dismiss_v1::ZcosmicLayerSurfaceDismissV1,
        request: zcosmic_layer_surface_dismiss_v1::Request,
        data: &LayerSurfaceDismissControllerData,
        _dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_layer_surface_dismiss_v1::Request::Destroy => {
                // Unregister the controller
                if let Ok(surface) = data.surface.upgrade() {
                    let surface_id = surface.id().protocol_id();
                    state.unregister_dismiss_controller(surface_id);
                }
            }
            zcosmic_layer_surface_dismiss_v1::Request::Arm => {
                if let Ok(surface) = data.surface.upgrade() {
                    let surface_id = surface.id().protocol_id();
                    info!(surface_id, "Dismiss controller armed");
                    *data.armed.lock().unwrap() = true;
                } else {
                    warn!("Arm called on dead surface");
                }
            }
            zcosmic_layer_surface_dismiss_v1::Request::Disarm => {
                if let Ok(surface) = data.surface.upgrade() {
                    let surface_id = surface.id().protocol_id();
                    debug!(surface_id, "Dismiss controller disarmed");
                    *data.armed.lock().unwrap() = false;
                } else {
                    warn!("Disarm called on dead surface");
                }
            }
            zcosmic_layer_surface_dismiss_v1::Request::AddToGroup { surface } => {
                let group_surface_id = surface.id().protocol_id();
                debug!(group_surface_id, "Adding surface to dismiss group");
                data.group_surfaces.lock().unwrap().insert(group_surface_id);
            }
            zcosmic_layer_surface_dismiss_v1::Request::RemoveFromGroup { surface } => {
                let group_surface_id = surface.id().protocol_id();
                debug!(group_surface_id, "Removing surface from dismiss group");
                data.group_surfaces
                    .lock()
                    .unwrap()
                    .remove(&group_surface_id);
            }
        }
    }
}

/// Check if a click on the given surface should trigger dismiss for any armed controllers
/// Returns the controllers that should be dismissed
pub fn check_dismiss_on_click(
    clicked_surface_id: u32,
    controllers: &[zcosmic_layer_surface_dismiss_v1::ZcosmicLayerSurfaceDismissV1],
) -> Vec<zcosmic_layer_surface_dismiss_v1::ZcosmicLayerSurfaceDismissV1> {
    let mut to_dismiss = Vec::new();

    for controller in controllers {
        let data = controller.data::<LayerSurfaceDismissControllerData>();
        if let Some(data) = data {
            let armed = *data.armed.lock().unwrap();
            if armed {
                let group = data.group_surfaces.lock().unwrap();
                let controlled_surface_id = data
                    .surface
                    .upgrade()
                    .map(|s| s.id().protocol_id())
                    .unwrap_or(0);
                debug!(
                    clicked_surface_id,
                    controlled_surface_id,
                    group_members = ?group.iter().collect::<Vec<_>>(),
                    "Checking dismiss - clicked vs group"
                );
                // If the clicked surface is NOT in the group, trigger dismiss
                if !group.contains(&clicked_surface_id) {
                    to_dismiss.push(controller.clone());
                }
            }
        }
    }

    to_dismiss
}

/// Send dismiss_requested and disarm the controllers
pub fn fire_dismiss(
    controllers: Vec<zcosmic_layer_surface_dismiss_v1::ZcosmicLayerSurfaceDismissV1>,
) {
    for controller in controllers {
        if let Some(data) = controller.data::<LayerSurfaceDismissControllerData>() {
            // Disarm
            *data.armed.lock().unwrap() = false;
            // Send event
            controller.dismiss_requested();
            info!("Sent dismiss_requested event");
        }
    }
}

/// Macro to delegate layer surface dismiss dispatch to the state
#[macro_export]
macro_rules! delegate_layer_surface_dismiss {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::layer_surface_dismiss::zcosmic_layer_surface_dismiss_manager_v1::ZcosmicLayerSurfaceDismissManagerV1: ()
        ] => $crate::wayland::protocols::layer_surface_dismiss::LayerSurfaceDismissState);

        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::layer_surface_dismiss::zcosmic_layer_surface_dismiss_manager_v1::ZcosmicLayerSurfaceDismissManagerV1: ()
        ] => $crate::wayland::protocols::layer_surface_dismiss::LayerSurfaceDismissState);

        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::layer_surface_dismiss::zcosmic_layer_surface_dismiss_v1::ZcosmicLayerSurfaceDismissV1: $crate::wayland::protocols::layer_surface_dismiss::LayerSurfaceDismissControllerData
        ] => $crate::wayland::protocols::layer_surface_dismiss::LayerSurfaceDismissState);
    };
}
