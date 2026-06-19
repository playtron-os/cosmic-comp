// SPDX-License-Identifier: GPL-3.0-only

//! Implementation of the layer usable-area protocol (layer_usable_area_manager_v1)
//!
//! Reports each layer surface's output usable (non-exclusive) area — the output
//! logical geometry minus every exclusive zone reserved by other layer surfaces
//! (panels, docks, bars). A centered or anchored overlay (launcher, switcher,
//! system dialog) cannot compute this itself: only the compositor knows the
//! other surfaces' exclusive zones, and a non-overlapping surface gets nothing
//! from overlap-notify. With the usable area it can center within the free
//! space rather than the full output, per-output and updated live.

// Re-export generated types
pub use generated::{layer_usable_area_manager_v1, layer_usable_area_v1};

#[allow(non_snake_case, non_upper_case_globals, non_camel_case_types)]
mod generated {
    use smithay::reexports::wayland_server::{self, protocol::*};

    pub mod __interfaces {
        use smithay::reexports::wayland_server::protocol::__interfaces::*;
        use wayland_backend;
        wayland_scanner::generate_interfaces!("resources/protocols/layer-usable-area.xml");
    }
    use self::__interfaces::*;

    wayland_scanner::generate_server_code!("resources/protocols/layer-usable-area.xml");
}

use smithay::{
    desktop::layer_map_for_output,
    output::Output,
    reexports::wayland_server::{
        Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource, Weak,
        backend::GlobalId, protocol::wl_surface::WlSurface,
    },
    utils::{Logical, Rectangle},
    wayland::compositor::with_states,
};
use std::sync::Mutex;

/// Dispatch data for a per-surface usable-area object: the surface it reports
/// for, so the object can be cleaned up on destroy.
pub type UsableAreaData = Mutex<UsableAreaInternal>;

#[derive(Debug)]
pub struct UsableAreaInternal {
    pub surface: Weak<WlSurface>,
}

/// Stored in the surface's data map so the [`UsableAreaState::refresh`] loop can
/// find the protocol object by walking output layer maps, and so duplicate
/// events are suppressed (`last`).
type SurfaceUsableArea = Mutex<SurfaceUsableAreaInner>;

#[derive(Debug, Default)]
struct SurfaceUsableAreaInner {
    object: Option<Weak<layer_usable_area_v1::LayerUsableAreaV1>>,
    last: Option<Rectangle<i32, Logical>>,
}

/// State for the usable-area manager protocol.
#[derive(Debug)]
pub struct UsableAreaState {
    global: GlobalId,
}

impl UsableAreaState {
    /// Create a new usable-area manager global.
    pub fn new<D>(dh: &DisplayHandle) -> UsableAreaState
    where
        D: GlobalDispatch<layer_usable_area_manager_v1::LayerUsableAreaManagerV1, ()>
            + Dispatch<layer_usable_area_manager_v1::LayerUsableAreaManagerV1, ()>
            + Dispatch<layer_usable_area_v1::LayerUsableAreaV1, UsableAreaData>
            + UsableAreaHandler
            + 'static,
    {
        let global =
            dh.create_global::<D, layer_usable_area_manager_v1::LayerUsableAreaManagerV1, _>(1, ());
        UsableAreaState { global }
    }

    /// Get the global ID of the usable-area manager.
    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }

    /// Recompute every output's usable (non-exclusive) area and notify the
    /// registered surfaces on that output whose value changed. Cheap when
    /// nothing moved: the per-surface `last` guard suppresses duplicate events.
    /// Call this whenever layer-shell arrangement may have changed (alongside
    /// the overlap-notify refresh).
    pub fn refresh<D>(state: &mut D)
    where
        D: UsableAreaHandler + 'static,
    {
        for output in state.outputs() {
            let map = layer_map_for_output(&output);
            let zone = map.non_exclusive_zone();
            for layer_surface in map.layers() {
                send_usable_area(layer_surface.wl_surface(), zone);
            }
        }
    }
}

/// Send the current usable area to a surface's object, if registered and changed.
fn send_usable_area(surface: &WlSurface, zone: Rectangle<i32, Logical>) {
    with_states(surface, |states| {
        let Some(entry) = states.data_map.get::<SurfaceUsableArea>() else {
            return;
        };
        let mut guard = entry.lock().unwrap();
        if guard.last == Some(zone) {
            return;
        }
        let Some(weak) = guard.object.as_ref() else {
            return;
        };
        if let Ok(obj) = weak.upgrade() {
            obj.usable_area(zone.loc.x, zone.loc.y, zone.size.w, zone.size.h);
            guard.last = Some(zone);
        }
    });
}

/// Handler trait for the usable-area protocol.
pub trait UsableAreaHandler {
    /// Get the usable-area state.
    fn usable_area_state(&mut self) -> &mut UsableAreaState;

    /// All outputs the compositor currently drives.
    fn outputs(&self) -> impl Iterator<Item = Output>;
}

impl<D> GlobalDispatch<layer_usable_area_manager_v1::LayerUsableAreaManagerV1, (), D>
    for UsableAreaState
where
    D: GlobalDispatch<layer_usable_area_manager_v1::LayerUsableAreaManagerV1, ()>
        + Dispatch<layer_usable_area_manager_v1::LayerUsableAreaManagerV1, ()>
        + Dispatch<layer_usable_area_v1::LayerUsableAreaV1, UsableAreaData>
        + UsableAreaHandler
        + 'static,
{
    fn bind(
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<layer_usable_area_manager_v1::LayerUsableAreaManagerV1>,
        _global_data: &(),
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }
}

impl<D> Dispatch<layer_usable_area_manager_v1::LayerUsableAreaManagerV1, (), D> for UsableAreaState
where
    D: GlobalDispatch<layer_usable_area_manager_v1::LayerUsableAreaManagerV1, ()>
        + Dispatch<layer_usable_area_manager_v1::LayerUsableAreaManagerV1, ()>
        + Dispatch<layer_usable_area_v1::LayerUsableAreaV1, UsableAreaData>
        + UsableAreaHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        resource: &layer_usable_area_manager_v1::LayerUsableAreaManagerV1,
        request: layer_usable_area_manager_v1::Request,
        _data: &(),
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            layer_usable_area_manager_v1::Request::Destroy => {}
            layer_usable_area_manager_v1::Request::GetUsableArea { id, surface } => {
                let exists = with_states(&surface, |surface_data| {
                    surface_data
                        .data_map
                        .get::<SurfaceUsableArea>()
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
                        layer_usable_area_manager_v1::Error::AlreadyExists as u32,
                        format!(
                            "{resource:?} LayerUsableAreaV1 object already exists for the surface"
                        ),
                    );
                    return;
                }

                let data = Mutex::new(UsableAreaInternal {
                    surface: surface.downgrade(),
                });
                let obj = data_init.init(id, data);

                with_states(&surface, |surface_data| {
                    let entry = surface_data
                        .data_map
                        .get_or_insert_threadsafe(SurfaceUsableArea::default);
                    let mut guard = entry.lock().unwrap();
                    guard.object = Some(obj.downgrade());
                    // Force the next refresh to emit even if the value is the
                    // same as a previous object's.
                    guard.last = None;
                });

                // Send the current value immediately if the surface is already
                // mapped on an output; otherwise a later refresh will catch it.
                UsableAreaState::refresh(state);
            }
        }
    }
}

impl<D> Dispatch<layer_usable_area_v1::LayerUsableAreaV1, UsableAreaData, D> for UsableAreaState
where
    D: GlobalDispatch<layer_usable_area_manager_v1::LayerUsableAreaManagerV1, ()>
        + Dispatch<layer_usable_area_manager_v1::LayerUsableAreaManagerV1, ()>
        + Dispatch<layer_usable_area_v1::LayerUsableAreaV1, UsableAreaData>
        + UsableAreaHandler
        + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        _resource: &layer_usable_area_v1::LayerUsableAreaV1,
        request: layer_usable_area_v1::Request,
        data: &UsableAreaData,
        _dhandle: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            layer_usable_area_v1::Request::Destroy => {
                let guard = data.lock().unwrap();
                if let Ok(surface) = guard.surface.upgrade() {
                    with_states(&surface, |surface_data| {
                        if let Some(entry) = surface_data.data_map.get::<SurfaceUsableArea>() {
                            let mut g = entry.lock().unwrap();
                            g.object = None;
                            g.last = None;
                        }
                    });
                }
            }
        }
    }
}

macro_rules! delegate_layer_usable_area {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::layer_usable_area::layer_usable_area_manager_v1::LayerUsableAreaManagerV1: ()
        ] => $crate::wayland::protocols::layer_usable_area::UsableAreaState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::layer_usable_area::layer_usable_area_manager_v1::LayerUsableAreaManagerV1: ()
        ] => $crate::wayland::protocols::layer_usable_area::UsableAreaState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::layer_usable_area::layer_usable_area_v1::LayerUsableAreaV1: $crate::wayland::protocols::layer_usable_area::UsableAreaData
        ] => $crate::wayland::protocols::layer_usable_area::UsableAreaState);
    };
}
pub(crate) use delegate_layer_usable_area;
