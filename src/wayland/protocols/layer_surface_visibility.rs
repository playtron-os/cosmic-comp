// SPDX-License-Identifier: GPL-3.0-only

//! Implementation of the COSMIC layer surface visibility protocol (zcosmic_layer_surface_visibility_v1)
//!
//! This protocol allows layer-shell clients to hide and show their surfaces
//! without destroying them. Hidden surfaces are not rendered and do not
//! receive input events, but maintain their configuration for instant show.

pub use generated::{
    zcosmic_layer_surface_visibility_manager_v1, zcosmic_layer_surface_visibility_v1,
};

#[allow(non_snake_case, non_upper_case_globals, non_camel_case_types)]
mod generated {
    use smithay::reexports::wayland_server::{self, protocol::*};

    pub mod __interfaces {
        use smithay::reexports::wayland_server::protocol::__interfaces::*;
        use wayland_backend;
        wayland_scanner::generate_interfaces!("resources/protocols/layer_surface_visibility.xml");
    }
    use self::__interfaces::*;

    wayland_scanner::generate_server_code!("resources/protocols/layer_surface_visibility.xml");
}

use smithay::reexports::wayland_server::{
    Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource, Weak,
    backend::GlobalId, protocol::wl_surface::WlSurface,
};
use std::sync::Mutex;
use tracing::{debug, info, warn};
use wayland_backend::server::ObjectId;

/// User data for the visibility controller
pub struct LayerSurfaceVisibilityControllerData {
    /// The wl_surface this controls (should be a layer-shell surface)
    pub surface: Weak<WlSurface>,
    /// Whether the surface is currently hidden
    pub hidden: Mutex<bool>,
}

/// State for the layer surface visibility manager protocol
pub struct LayerSurfaceVisibilityState {
    global: GlobalId,
}

impl std::fmt::Debug for LayerSurfaceVisibilityState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LayerSurfaceVisibilityState").finish()
    }
}

impl LayerSurfaceVisibilityState {
    /// Create a new layer surface visibility manager global
    pub fn new<D>(dh: &DisplayHandle) -> LayerSurfaceVisibilityState
    where
        D: GlobalDispatch<
                zcosmic_layer_surface_visibility_manager_v1::ZcosmicLayerSurfaceVisibilityManagerV1,
                (),
            > + Dispatch<
                zcosmic_layer_surface_visibility_manager_v1::ZcosmicLayerSurfaceVisibilityManagerV1,
                (),
            > + Dispatch<
                zcosmic_layer_surface_visibility_v1::ZcosmicLayerSurfaceVisibilityV1,
                LayerSurfaceVisibilityControllerData,
            > + LayerSurfaceVisibilityHandler
            + 'static,
    {
        let global = dh.create_global::<
            D,
            zcosmic_layer_surface_visibility_manager_v1::ZcosmicLayerSurfaceVisibilityManagerV1,
            _,
        >(1, ());
        LayerSurfaceVisibilityState { global }
    }

    /// Get the global ID
    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }
}

/// Handler trait for layer surface visibility protocol
pub trait LayerSurfaceVisibilityHandler {
    /// Get the layer surface visibility state
    fn layer_surface_visibility_state(&self) -> &LayerSurfaceVisibilityState;

    /// Set a surface's hidden state in the Shell
    fn set_surface_hidden(&mut self, surface_id: ObjectId, hidden: bool);

    /// Check if a surface is hidden
    fn is_surface_hidden(&self, surface_id: &ObjectId) -> bool;
}

impl<D>
    GlobalDispatch<
        zcosmic_layer_surface_visibility_manager_v1::ZcosmicLayerSurfaceVisibilityManagerV1,
        (),
        D,
    > for LayerSurfaceVisibilityState
where
    D: GlobalDispatch<
            zcosmic_layer_surface_visibility_manager_v1::ZcosmicLayerSurfaceVisibilityManagerV1,
            (),
        > + Dispatch<
            zcosmic_layer_surface_visibility_manager_v1::ZcosmicLayerSurfaceVisibilityManagerV1,
            (),
        > + Dispatch<
            zcosmic_layer_surface_visibility_v1::ZcosmicLayerSurfaceVisibilityV1,
            LayerSurfaceVisibilityControllerData,
        > + LayerSurfaceVisibilityHandler
        + 'static,
{
    fn bind(
        _state: &mut D,
        _dh: &DisplayHandle,
        _client: &Client,
        resource: New<
            zcosmic_layer_surface_visibility_manager_v1::ZcosmicLayerSurfaceVisibilityManagerV1,
        >,
        _global_data: &(),
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }
}

impl<D>
    Dispatch<
        zcosmic_layer_surface_visibility_manager_v1::ZcosmicLayerSurfaceVisibilityManagerV1,
        (),
        D,
    > for LayerSurfaceVisibilityState
where
    D: GlobalDispatch<
            zcosmic_layer_surface_visibility_manager_v1::ZcosmicLayerSurfaceVisibilityManagerV1,
            (),
        > + Dispatch<
            zcosmic_layer_surface_visibility_manager_v1::ZcosmicLayerSurfaceVisibilityManagerV1,
            (),
        > + Dispatch<
            zcosmic_layer_surface_visibility_v1::ZcosmicLayerSurfaceVisibilityV1,
            LayerSurfaceVisibilityControllerData,
        > + LayerSurfaceVisibilityHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        _resource: &zcosmic_layer_surface_visibility_manager_v1::ZcosmicLayerSurfaceVisibilityManagerV1,
        request: zcosmic_layer_surface_visibility_manager_v1::Request,
        _data: &(),
        _dh: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_layer_surface_visibility_manager_v1::Request::Destroy => {
                // Nothing to do, global persists
            }
            zcosmic_layer_surface_visibility_manager_v1::Request::GetVisibilityController {
                id,
                surface,
            } => {
                let surface_id = surface.id();
                debug!(
                    ?surface_id,
                    "Creating visibility controller for layer surface"
                );

                let controller_data = LayerSurfaceVisibilityControllerData {
                    surface: surface.downgrade(),
                    hidden: Mutex::new(false),
                };

                let controller = data_init.init(id, controller_data);

                // Send initial visibility state
                let is_hidden = state.is_surface_hidden(&surface_id);
                controller.visibility_changed(if is_hidden { 0 } else { 1 });
            }
        }
    }
}

impl<D>
    Dispatch<
        zcosmic_layer_surface_visibility_v1::ZcosmicLayerSurfaceVisibilityV1,
        LayerSurfaceVisibilityControllerData,
        D,
    > for LayerSurfaceVisibilityState
where
    D: Dispatch<
            zcosmic_layer_surface_visibility_v1::ZcosmicLayerSurfaceVisibilityV1,
            LayerSurfaceVisibilityControllerData,
        > + LayerSurfaceVisibilityHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        resource: &zcosmic_layer_surface_visibility_v1::ZcosmicLayerSurfaceVisibilityV1,
        request: zcosmic_layer_surface_visibility_v1::Request,
        data: &LayerSurfaceVisibilityControllerData,
        _dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_layer_surface_visibility_v1::Request::Destroy => {
                // When controller is destroyed, make surface visible again
                if let Ok(surface) = data.surface.upgrade() {
                    let surface_id = surface.id();
                    state.set_surface_hidden(surface_id, false);
                }
            }
            zcosmic_layer_surface_visibility_v1::Request::SetHidden => {
                if let Ok(surface) = data.surface.upgrade() {
                    let surface_id = surface.id();
                    info!(?surface_id, "Layer surface hidden by client");
                    *data.hidden.lock().unwrap() = true;
                    state.set_surface_hidden(surface_id, true);
                    resource.visibility_changed(0);
                } else {
                    warn!("SetHidden called on dead surface");
                }
            }
            zcosmic_layer_surface_visibility_v1::Request::SetVisible => {
                if let Ok(surface) = data.surface.upgrade() {
                    let surface_id = surface.id();
                    info!(?surface_id, "Layer surface shown by client");
                    *data.hidden.lock().unwrap() = false;
                    state.set_surface_hidden(surface_id, false);
                    resource.visibility_changed(1);
                } else {
                    warn!("SetVisible called on dead surface");
                }
            }
        }
    }
}

/// Macro to delegate layer surface visibility dispatch to the state
#[macro_export]
macro_rules! delegate_layer_surface_visibility {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::layer_surface_visibility::zcosmic_layer_surface_visibility_manager_v1::ZcosmicLayerSurfaceVisibilityManagerV1: ()
        ] => $crate::wayland::protocols::layer_surface_visibility::LayerSurfaceVisibilityState);

        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::layer_surface_visibility::zcosmic_layer_surface_visibility_manager_v1::ZcosmicLayerSurfaceVisibilityManagerV1: ()
        ] => $crate::wayland::protocols::layer_surface_visibility::LayerSurfaceVisibilityState);

        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::layer_surface_visibility::zcosmic_layer_surface_visibility_v1::ZcosmicLayerSurfaceVisibilityV1: $crate::wayland::protocols::layer_surface_visibility::LayerSurfaceVisibilityControllerData
        ] => $crate::wayland::protocols::layer_surface_visibility::LayerSurfaceVisibilityState);
    };
}
