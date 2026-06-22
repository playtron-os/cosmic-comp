// SPDX-License-Identifier: GPL-3.0-only

//! Implementation of the layer edge-resize protocol (layer_edge_resize_manager_v1).
//!
//! Lets a layer-shell client opt a surface into a compositor-drawn, VSCode-style
//! edge resize sash on its outer (non-anchored) edge. The compositor infers the
//! resizable edge from the surface anchor, draws a themed indicator on hover near
//! that edge, shows an EW-resize cursor, and on press+drag shows a ghost that the
//! surface springs to on release (see the shell's `EdgeResizeGrab` +
//! `LayerResizeAnim`). The client only opts in and supplies width bounds.
//!
//! Presence of a live `layer_edge_resize_v1` object for a surface is the opt-in
//! signal that replaces the former `namespace == "agentos-chat-panel"` hardcode.

// Re-export generated types
pub use generated::{layer_edge_resize_manager_v1, layer_edge_resize_v1};

#[allow(non_snake_case, non_upper_case_globals, non_camel_case_types)]
mod generated {
    use smithay::reexports::wayland_server::{self, protocol::*};

    pub mod __interfaces {
        use smithay::reexports::wayland_server::protocol::__interfaces::*;
        use wayland_backend;
        wayland_scanner::generate_interfaces!("resources/protocols/layer-edge-resize.xml");
    }
    use self::__interfaces::*;

    wayland_scanner::generate_server_code!("resources/protocols/layer-edge-resize.xml");
}

use smithay::{
    reexports::wayland_server::{
        Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource, Weak,
        backend::GlobalId, protocol::wl_surface::WlSurface,
    },
    wayland::compositor::with_states,
};
use std::sync::Mutex;

/// Default minimum width (logical px) until the client sets one. Mirrors the
/// shell's `MIN_PANEL_WIDTH` safety floor.
const DEFAULT_MIN_WIDTH: i32 = 320;

/// Resolved width bounds for an edge-resizable surface.
#[derive(Debug, Clone, Copy)]
pub struct EdgeResizeConfig {
    /// Minimum width in logical px.
    pub min_width: i32,
    /// Maximum width in logical px; `0` means "full output width".
    pub max_width: i32,
}

/// Per-object dispatch data: the surface this edge-resize object controls.
pub type EdgeResizeData = Mutex<EdgeResizeInternal>;

#[derive(Debug)]
pub struct EdgeResizeInternal {
    pub surface: Weak<WlSurface>,
}

/// Stored in the surface's data map. A live `object` means the surface is opted
/// into edge resize; `min_width`/`max_width` are its current bounds.
type SurfaceEdgeResize = Mutex<SurfaceEdgeResizeInner>;

#[derive(Debug)]
struct SurfaceEdgeResizeInner {
    object: Option<Weak<layer_edge_resize_v1::LayerEdgeResizeV1>>,
    min_width: i32,
    max_width: i32,
}

impl Default for SurfaceEdgeResizeInner {
    fn default() -> Self {
        Self {
            object: None,
            min_width: DEFAULT_MIN_WIDTH,
            max_width: 0,
        }
    }
}

/// Look up the edge-resize bounds for `surface`, if it is currently opted in (a
/// live `layer_edge_resize_v1` object exists for it). This is the replacement for
/// the old `namespace == "agentos-chat-panel"` check.
pub fn get_surface_edge_resize(surface: &WlSurface) -> Option<EdgeResizeConfig> {
    with_states(surface, |states| {
        let entry = states.data_map.get::<SurfaceEdgeResize>()?;
        let inner = entry.lock().unwrap();
        if inner.object.as_ref().is_some_and(|o| o.upgrade().is_ok()) {
            Some(EdgeResizeConfig {
                min_width: inner.min_width,
                max_width: inner.max_width,
            })
        } else {
            None
        }
    })
}

/// State for the edge-resize manager protocol (holds the global alive).
#[derive(Debug)]
pub struct EdgeResizeState {
    global: GlobalId,
}

impl EdgeResizeState {
    pub fn new<D>(dh: &DisplayHandle) -> EdgeResizeState
    where
        D: GlobalDispatch<layer_edge_resize_manager_v1::LayerEdgeResizeManagerV1, ()>
            + Dispatch<layer_edge_resize_manager_v1::LayerEdgeResizeManagerV1, ()>
            + Dispatch<layer_edge_resize_v1::LayerEdgeResizeV1, EdgeResizeData>
            + EdgeResizeHandler
            + 'static,
    {
        let global =
            dh.create_global::<D, layer_edge_resize_manager_v1::LayerEdgeResizeManagerV1, _>(1, ());
        EdgeResizeState { global }
    }

    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }
}

/// Handler trait for the edge-resize protocol.
pub trait EdgeResizeHandler {
    fn edge_resize_state(&mut self) -> &mut EdgeResizeState;
}

impl<D> GlobalDispatch<layer_edge_resize_manager_v1::LayerEdgeResizeManagerV1, (), D>
    for EdgeResizeState
where
    D: GlobalDispatch<layer_edge_resize_manager_v1::LayerEdgeResizeManagerV1, ()>
        + Dispatch<layer_edge_resize_manager_v1::LayerEdgeResizeManagerV1, ()>
        + Dispatch<layer_edge_resize_v1::LayerEdgeResizeV1, EdgeResizeData>
        + EdgeResizeHandler
        + 'static,
{
    fn bind(
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<layer_edge_resize_manager_v1::LayerEdgeResizeManagerV1>,
        _global_data: &(),
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }
}

impl<D> Dispatch<layer_edge_resize_manager_v1::LayerEdgeResizeManagerV1, (), D> for EdgeResizeState
where
    D: GlobalDispatch<layer_edge_resize_manager_v1::LayerEdgeResizeManagerV1, ()>
        + Dispatch<layer_edge_resize_manager_v1::LayerEdgeResizeManagerV1, ()>
        + Dispatch<layer_edge_resize_v1::LayerEdgeResizeV1, EdgeResizeData>
        + EdgeResizeHandler
        + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        resource: &layer_edge_resize_manager_v1::LayerEdgeResizeManagerV1,
        request: layer_edge_resize_manager_v1::Request,
        _data: &(),
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            layer_edge_resize_manager_v1::Request::Destroy => {}
            layer_edge_resize_manager_v1::Request::GetEdgeResize { id, surface } => {
                let exists = with_states(&surface, |surface_data| {
                    surface_data
                        .data_map
                        .get::<SurfaceEdgeResize>()
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
                        layer_edge_resize_manager_v1::Error::AlreadyExists as u32,
                        format!(
                            "{resource:?} LayerEdgeResizeV1 object already exists for the surface"
                        ),
                    );
                    return;
                }

                let data = Mutex::new(EdgeResizeInternal {
                    surface: surface.downgrade(),
                });
                let obj = data_init.init(id, data);

                with_states(&surface, |surface_data| {
                    let entry = surface_data
                        .data_map
                        .get_or_insert_threadsafe(SurfaceEdgeResize::default);
                    entry.lock().unwrap().object = Some(obj.downgrade());
                });
            }
        }
    }
}

impl<D> Dispatch<layer_edge_resize_v1::LayerEdgeResizeV1, EdgeResizeData, D> for EdgeResizeState
where
    D: GlobalDispatch<layer_edge_resize_manager_v1::LayerEdgeResizeManagerV1, ()>
        + Dispatch<layer_edge_resize_manager_v1::LayerEdgeResizeManagerV1, ()>
        + Dispatch<layer_edge_resize_v1::LayerEdgeResizeV1, EdgeResizeData>
        + EdgeResizeHandler
        + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        _resource: &layer_edge_resize_v1::LayerEdgeResizeV1,
        request: layer_edge_resize_v1::Request,
        data: &EdgeResizeData,
        _dhandle: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        let surface = data.lock().unwrap().surface.upgrade().ok();
        match request {
            layer_edge_resize_v1::Request::SetMinWidth { width } => {
                if let Some(surface) = surface {
                    with_states(&surface, |sd| {
                        let entry = sd
                            .data_map
                            .get_or_insert_threadsafe(SurfaceEdgeResize::default);
                        entry.lock().unwrap().min_width = width.max(1);
                    });
                }
            }
            layer_edge_resize_v1::Request::SetMaxWidth { width } => {
                if let Some(surface) = surface {
                    with_states(&surface, |sd| {
                        let entry = sd
                            .data_map
                            .get_or_insert_threadsafe(SurfaceEdgeResize::default);
                        entry.lock().unwrap().max_width = width.max(0);
                    });
                }
            }
            layer_edge_resize_v1::Request::Destroy => {
                if let Some(surface) = surface {
                    with_states(&surface, |sd| {
                        if let Some(entry) = sd.data_map.get::<SurfaceEdgeResize>() {
                            entry.lock().unwrap().object = None;
                        }
                    });
                }
            }
        }
    }
}

macro_rules! delegate_layer_edge_resize {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::layer_edge_resize::layer_edge_resize_manager_v1::LayerEdgeResizeManagerV1: ()
        ] => $crate::wayland::protocols::layer_edge_resize::EdgeResizeState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::layer_edge_resize::layer_edge_resize_manager_v1::LayerEdgeResizeManagerV1: ()
        ] => $crate::wayland::protocols::layer_edge_resize::EdgeResizeState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::layer_edge_resize::layer_edge_resize_v1::LayerEdgeResizeV1: $crate::wayland::protocols::layer_edge_resize::EdgeResizeData
        ] => $crate::wayland::protocols::layer_edge_resize::EdgeResizeState);
    };
}
pub(crate) use delegate_layer_edge_resize;
