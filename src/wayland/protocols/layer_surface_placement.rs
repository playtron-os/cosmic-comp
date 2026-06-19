// SPDX-License-Identifier: GPL-3.0-only

//! Implementation of the layer surface placement protocol
//! (layer_surface_placement_manager_v1)
//!
//! Lets a layer shell surface ask the compositor to position it within its
//! output's usable (non-exclusive) area — the output minus every exclusive zone
//! reserved by panels/docks — instead of learning the usable area and applying
//! a margin itself. The compositor positions the surface at map time and keeps
//! it positioned across relayouts, so the surface never jumps from a stale
//! position to the corrected one when it first appears on an output.
//!
//! Placement is expressed as a [`VerticalPlacement`] and applied by smithay's
//! layer-map `arrange()`; this module only translates protocol requests into
//! that placement and asks the compositor to relayout + redraw.

// Re-export generated types
pub use generated::{layer_surface_placement_manager_v1, layer_surface_placement_v1};

#[allow(non_snake_case, non_upper_case_globals, non_camel_case_types)]
mod generated {
    use smithay::reexports::wayland_server::{self, protocol::*};

    pub mod __interfaces {
        use smithay::reexports::wayland_server::protocol::__interfaces::*;
        use wayland_backend;
        wayland_scanner::generate_interfaces!("resources/protocols/layer-surface-placement.xml");
    }
    use self::__interfaces::*;

    wayland_scanner::generate_server_code!("resources/protocols/layer-surface-placement.xml");
}

use smithay::{
    desktop::{MaxHeight, VerticalPlacement},
    reexports::wayland_server::{
        Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource, Weak,
        backend::GlobalId, protocol::wl_surface::WlSurface,
    },
    wayland::compositor::with_states,
};
use std::sync::Mutex;

/// Dispatch data for a per-surface placement object: the surface it controls,
/// so the placement can be cleared when the object is destroyed.
pub type PlacementData = Mutex<PlacementInternal>;

#[derive(Debug)]
pub struct PlacementInternal {
    pub surface: Weak<WlSurface>,
}

/// Stored in the surface's data map purely to enforce the `already_exists`
/// error (one placement object per surface).
type SurfacePlacement = Mutex<SurfacePlacementInner>;

#[derive(Debug, Default)]
struct SurfacePlacementInner {
    object: Option<Weak<layer_surface_placement_v1::LayerSurfacePlacementV1>>,
}

/// State for the placement manager protocol.
#[derive(Debug)]
pub struct LayerSurfacePlacementState {
    global: GlobalId,
}

impl LayerSurfacePlacementState {
    /// Create a new placement manager global.
    pub fn new<D>(dh: &DisplayHandle) -> LayerSurfacePlacementState
    where
        D: GlobalDispatch<layer_surface_placement_manager_v1::LayerSurfacePlacementManagerV1, ()>
            + Dispatch<layer_surface_placement_manager_v1::LayerSurfacePlacementManagerV1, ()>
            + Dispatch<layer_surface_placement_v1::LayerSurfacePlacementV1, PlacementData>
            + LayerSurfacePlacementHandler
            + 'static,
    {
        let global = dh
            .create_global::<D, layer_surface_placement_manager_v1::LayerSurfacePlacementManagerV1, _>(
                1,
                (),
            );
        LayerSurfacePlacementState { global }
    }

    /// Get the global ID of the placement manager.
    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }
}

/// Handler trait for the placement protocol.
pub trait LayerSurfacePlacementHandler {
    /// Get the placement manager state.
    fn layer_surface_placement_state(&mut self) -> &mut LayerSurfacePlacementState;

    /// Apply a placement change for a surface: store the [`VerticalPlacement`]
    /// (or clear it with `None`) and relayout + redraw the surface's output so
    /// the change takes effect immediately. Called for every set/unset request
    /// and when a placement object is destroyed.
    fn update_layer_surface_placement(
        &mut self,
        surface: &WlSurface,
        placement: Option<VerticalPlacement>,
    );

    /// Apply a height-cap change for a surface: store the [`MaxHeight`] (or clear
    /// it with `None`) and relayout + redraw the surface's output.
    fn update_layer_max_height(&mut self, surface: &WlSurface, max_height: Option<MaxHeight>);
}

impl<D> GlobalDispatch<layer_surface_placement_manager_v1::LayerSurfacePlacementManagerV1, (), D>
    for LayerSurfacePlacementState
where
    D: GlobalDispatch<layer_surface_placement_manager_v1::LayerSurfacePlacementManagerV1, ()>
        + Dispatch<layer_surface_placement_manager_v1::LayerSurfacePlacementManagerV1, ()>
        + Dispatch<layer_surface_placement_v1::LayerSurfacePlacementV1, PlacementData>
        + LayerSurfacePlacementHandler
        + 'static,
{
    fn bind(
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<layer_surface_placement_manager_v1::LayerSurfacePlacementManagerV1>,
        _global_data: &(),
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }
}

impl<D> Dispatch<layer_surface_placement_manager_v1::LayerSurfacePlacementManagerV1, (), D>
    for LayerSurfacePlacementState
where
    D: GlobalDispatch<layer_surface_placement_manager_v1::LayerSurfacePlacementManagerV1, ()>
        + Dispatch<layer_surface_placement_manager_v1::LayerSurfacePlacementManagerV1, ()>
        + Dispatch<layer_surface_placement_v1::LayerSurfacePlacementV1, PlacementData>
        + LayerSurfacePlacementHandler
        + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        resource: &layer_surface_placement_manager_v1::LayerSurfacePlacementManagerV1,
        request: layer_surface_placement_manager_v1::Request,
        _data: &(),
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            layer_surface_placement_manager_v1::Request::Destroy => {}
            layer_surface_placement_manager_v1::Request::GetLayerSurfacePlacement {
                id,
                surface,
            } => {
                let exists = with_states(&surface, |surface_data| {
                    surface_data
                        .data_map
                        .get::<SurfacePlacement>()
                        .map(|entry| {
                            entry
                                .lock()
                                .unwrap()
                                .object
                                .as_ref()
                                .is_some_and(|o| o.upgrade().is_ok())
                        })
                        .unwrap_or(false)
                });
                if exists {
                    resource.post_error(
                        layer_surface_placement_manager_v1::Error::AlreadyExists as u32,
                        format!(
                            "{resource:?} LayerSurfacePlacementV1 object already exists for the surface"
                        ),
                    );
                    return;
                }

                let data = Mutex::new(PlacementInternal {
                    surface: surface.downgrade(),
                });
                let obj = data_init.init(id, data);

                with_states(&surface, |surface_data| {
                    let entry = surface_data
                        .data_map
                        .get_or_insert_threadsafe(SurfacePlacement::default);
                    entry.lock().unwrap().object = Some(obj.downgrade());
                });
            }
        }
    }
}

impl<D> Dispatch<layer_surface_placement_v1::LayerSurfacePlacementV1, PlacementData, D>
    for LayerSurfacePlacementState
where
    D: GlobalDispatch<layer_surface_placement_manager_v1::LayerSurfacePlacementManagerV1, ()>
        + Dispatch<layer_surface_placement_manager_v1::LayerSurfacePlacementManagerV1, ()>
        + Dispatch<layer_surface_placement_v1::LayerSurfacePlacementV1, PlacementData>
        + LayerSurfacePlacementHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        _resource: &layer_surface_placement_v1::LayerSurfacePlacementV1,
        request: layer_surface_placement_v1::Request,
        data: &PlacementData,
        _dhandle: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        let surface = data.lock().unwrap().surface.upgrade().ok();
        match request {
            layer_surface_placement_v1::Request::SetVerticalPlacement {
                fraction,
                offset,
                min_margin,
            } => {
                if let Some(surface) = surface {
                    state.update_layer_surface_placement(
                        &surface,
                        Some(VerticalPlacement {
                            fraction,
                            offset,
                            min_margin,
                        }),
                    );
                }
            }
            layer_surface_placement_v1::Request::UnsetVerticalPlacement => {
                if let Some(surface) = surface {
                    state.update_layer_surface_placement(&surface, None);
                }
            }
            layer_surface_placement_v1::Request::SetMaxHeight {
                fraction,
                min_height,
            } => {
                if let Some(surface) = surface {
                    state.update_layer_max_height(
                        &surface,
                        Some(MaxHeight {
                            fraction,
                            min_px: min_height,
                        }),
                    );
                }
            }
            layer_surface_placement_v1::Request::UnsetMaxHeight => {
                if let Some(surface) = surface {
                    state.update_layer_max_height(&surface, None);
                }
            }
            layer_surface_placement_v1::Request::Destroy => {
                if let Some(surface) = surface {
                    with_states(&surface, |surface_data| {
                        if let Some(entry) = surface_data.data_map.get::<SurfacePlacement>() {
                            entry.lock().unwrap().object = None;
                        }
                    });
                    // Revert to standard positioning + sizing when the client
                    // drops control.
                    state.update_layer_surface_placement(&surface, None);
                    state.update_layer_max_height(&surface, None);
                }
            }
        }
    }
}

macro_rules! delegate_layer_surface_placement {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::layer_surface_placement::layer_surface_placement_manager_v1::LayerSurfacePlacementManagerV1: ()
        ] => $crate::wayland::protocols::layer_surface_placement::LayerSurfacePlacementState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::layer_surface_placement::layer_surface_placement_manager_v1::LayerSurfacePlacementManagerV1: ()
        ] => $crate::wayland::protocols::layer_surface_placement::LayerSurfacePlacementState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::layer_surface_placement::layer_surface_placement_v1::LayerSurfacePlacementV1: $crate::wayland::protocols::layer_surface_placement::PlacementData
        ] => $crate::wayland::protocols::layer_surface_placement::LayerSurfacePlacementState);
    };
}
pub(crate) use delegate_layer_surface_placement;
