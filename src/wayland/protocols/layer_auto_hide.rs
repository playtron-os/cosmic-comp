// SPDX-License-Identifier: GPL-3.0-only

//! Implementation of the layer auto-hide protocol (layer_auto_hide_manager_v1)
//!
//! This protocol allows layer shell surfaces to register for compositor-driven
//! auto-hide behavior. The compositor animates hide/show transitions and handles
//! hover detection internally at frame rate.

// Re-export generated types
pub use generated::{layer_auto_hide_manager_v1, layer_auto_hide_v1};

#[allow(non_snake_case, non_upper_case_globals, non_camel_case_types)]
mod generated {
    use smithay::reexports::wayland_server::{self, protocol::*};

    pub mod __interfaces {
        use smithay::reexports::wayland_server::protocol::__interfaces::*;
        use wayland_backend;
        wayland_scanner::generate_interfaces!("resources/protocols/layer-auto-hide.xml");
    }
    use self::__interfaces::*;

    wayland_scanner::generate_server_code!("resources/protocols/layer-auto-hide.xml");
}

use smithay::utils::HookId;
use smithay::{
    reexports::wayland_server::{
        Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource, Weak,
        backend::GlobalId, protocol::wl_surface::WlSurface,
    },
    wayland::compositor::{Cacheable, add_pre_commit_hook, with_states},
};
use std::sync::Mutex;

use crate::shell::auto_hide::AutoHideEdge;

type SurfaceHookId = Mutex<
    Option<(
        HookId,
        Weak<layer_auto_hide_v1::LayerAutoHideV1>,
    )>,
>;

/// Cacheable edge zone size for surface cached state.
/// When non-zero the compositor extends the surface's effective input area by
/// this many pixels toward the closest output edge.
#[derive(Default, Debug, Copy, Clone)]
pub struct CacheableEdgeZone(pub u32);

impl Cacheable for CacheableEdgeZone {
    fn commit(&mut self, _dh: &DisplayHandle) -> Self {
        *self
    }
    fn merge_into(self, into: &mut Self, _dh: &DisplayHandle) {
        *into = self;
    }
}

/// Get edge zone extension size from a surface.
/// Returns 0 if no edge zone is set.
pub fn get_surface_edge_zone(surface: &WlSurface) -> u32 {
    with_states(surface, |states| {
        let mut guard = states.cached_state.get::<CacheableEdgeZone>();
        guard.current().0
    })
}

/// Cacheable auto-hide registration. When `Some`, the compositor drives
/// hide/show animation for this surface.
#[derive(Default, Debug, Copy, Clone)]
pub struct CacheableAutoHide(pub Option<AutoHideEdge>);

impl Cacheable for CacheableAutoHide {
    fn commit(&mut self, _dh: &DisplayHandle) -> Self {
        *self
    }
    fn merge_into(self, into: &mut Self, _dh: &DisplayHandle) {
        *into = self;
    }
}

/// Check whether a surface has auto-hide enabled.
pub fn get_surface_auto_hide(surface: &WlSurface) -> Option<AutoHideEdge> {
    with_states(surface, |states| {
        let mut guard = states.cached_state.get::<CacheableAutoHide>();
        guard.current().0
    })
}

/// Send the `visibility_changed` event to the client for a surface.
/// Retrieves the protocol object from the surface's data map.
pub fn send_auto_hide_visibility(surface: &WlSurface, visible: bool) {
    with_states(surface, |states| {
        if let Some(hook_ids) = states.data_map.get::<SurfaceHookId>() {
            if let Some((_, weak_obj)) = hook_ids.lock().unwrap().as_ref() {
                if let Ok(obj) = weak_obj.upgrade() {
                    obj.visibility_changed(if visible { 1 } else { 0 });
                }
            }
        }
    });
}

/// State for the layer auto-hide manager protocol
#[derive(Debug)]
pub struct LayerAutoHideState {
    global: GlobalId,
}

impl LayerAutoHideState {
    /// Create a new layer auto-hide manager global
    pub fn new<D>(dh: &DisplayHandle) -> LayerAutoHideState
    where
        D: GlobalDispatch<layer_auto_hide_manager_v1::LayerAutoHideManagerV1, ()>
            + Dispatch<layer_auto_hide_manager_v1::LayerAutoHideManagerV1, ()>
            + Dispatch<
                layer_auto_hide_v1::LayerAutoHideV1,
                LayerAutoHideData,
            > + LayerAutoHideHandler
            + 'static,
    {
        let global = dh
            .create_global::<D, layer_auto_hide_manager_v1::LayerAutoHideManagerV1, _>(
                1,
                (),
            );
        LayerAutoHideState { global }
    }

    /// Get the global ID of the layer auto-hide manager
    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }
}

/// Handler trait for layer auto-hide events
pub trait LayerAutoHideHandler {
    /// Get the layer auto-hide state
    fn layer_auto_hide_state(&mut self) -> &mut LayerAutoHideState;

    /// Called when a surface registers for compositor-driven auto-hide.
    fn auto_hide_registered(&mut self, surface: &WlSurface, edge: AutoHideEdge);

    /// Called when a surface unregisters from auto-hide.
    fn auto_hide_unregistered(&mut self, surface: &WlSurface);
}

impl<D> GlobalDispatch<layer_auto_hide_manager_v1::LayerAutoHideManagerV1, (), D>
    for LayerAutoHideState
where
    D: GlobalDispatch<layer_auto_hide_manager_v1::LayerAutoHideManagerV1, ()>
        + Dispatch<layer_auto_hide_manager_v1::LayerAutoHideManagerV1, ()>
        + Dispatch<layer_auto_hide_v1::LayerAutoHideV1, LayerAutoHideData>
        + LayerAutoHideHandler
        + 'static,
{
    fn bind(
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<layer_auto_hide_manager_v1::LayerAutoHideManagerV1>,
        _global_data: &(),
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }
}

impl<D> Dispatch<layer_auto_hide_manager_v1::LayerAutoHideManagerV1, (), D>
    for LayerAutoHideState
where
    D: GlobalDispatch<layer_auto_hide_manager_v1::LayerAutoHideManagerV1, ()>
        + Dispatch<layer_auto_hide_manager_v1::LayerAutoHideManagerV1, ()>
        + Dispatch<layer_auto_hide_v1::LayerAutoHideV1, LayerAutoHideData>
        + LayerAutoHideHandler
        + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        resource: &layer_auto_hide_manager_v1::LayerAutoHideManagerV1,
        request: layer_auto_hide_manager_v1::Request,
        _data: &(),
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            layer_auto_hide_manager_v1::Request::Destroy => {
                // Global destroyed, nothing to do
            }
            layer_auto_hide_manager_v1::Request::GetAutoHide { id, surface } => {
                // Check if surface already has an auto-hide object
                let exists = with_states(&surface, |surface_data| {
                    let hook_id = surface_data
                        .data_map
                        .get_or_insert_threadsafe(|| SurfaceHookId::new(None));
                    let guard = hook_id.lock().unwrap();
                    guard.as_ref().map(|(_, t)| t.upgrade().is_ok())
                });

                if exists.unwrap_or_default() {
                    resource.post_error(
                        layer_auto_hide_manager_v1::Error::AlreadyExists as u32,
                        format!(
                            "{resource:?} LayerAutoHideV1 object already exists for the surface"
                        ),
                    );
                    return;
                }

                let data = Mutex::new(LayerAutoHideInternal {
                    surface: surface.downgrade(),
                });
                let obj = data_init.init(id, data);
                let obj_downgrade = obj.downgrade();

                let needs_hook = exists.is_none();
                if needs_hook {
                    let hook_id = add_pre_commit_hook::<D, _>(&surface, move |_, _dh, _surface| {
                        // Pre-commit hook placeholder
                    });

                    with_states(&surface, |surface_data| {
                        let hook_ids = surface_data
                            .data_map
                            .get_or_insert_threadsafe(|| SurfaceHookId::new(None));
                        let mut guard = hook_ids.lock().unwrap();
                        *guard = Some((hook_id, obj_downgrade));
                    });
                }
            }
        }
    }
}

impl<D>
    Dispatch<layer_auto_hide_v1::LayerAutoHideV1, LayerAutoHideData, D>
    for LayerAutoHideState
where
    D: GlobalDispatch<layer_auto_hide_manager_v1::LayerAutoHideManagerV1, ()>
        + Dispatch<layer_auto_hide_manager_v1::LayerAutoHideManagerV1, ()>
        + Dispatch<layer_auto_hide_v1::LayerAutoHideV1, LayerAutoHideData>
        + LayerAutoHideHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        resource: &layer_auto_hide_v1::LayerAutoHideV1,
        request: layer_auto_hide_v1::Request,
        data: &LayerAutoHideData,
        _dhandle: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            layer_auto_hide_v1::Request::Destroy => {
                let guard = data.lock().unwrap();
                let Ok(surface) = guard.surface.upgrade() else {
                    return;
                };

                // Clear hook
                with_states(&surface, |surface_data| {
                    if let Some(hook_ids_mutex) = surface_data.data_map.get::<SurfaceHookId>() {
                        let mut hook_id = hook_ids_mutex.lock().unwrap();
                        *hook_id = None;
                    }
                });

                // Clear cached auto-hide state
                with_states(&surface, |s| {
                    let mut cached = s.cached_state.get::<CacheableAutoHide>();
                    let pending = cached.pending();
                    *pending = CacheableAutoHide(None);
                });
                // Clear cached edge zone
                with_states(&surface, |s| {
                    let mut cached = s.cached_state.get::<CacheableEdgeZone>();
                    let pending = cached.pending();
                    *pending = CacheableEdgeZone(0);
                });

                drop(guard);
                state.auto_hide_unregistered(&surface);
            }
            layer_auto_hide_v1::Request::SetAutoHide { edge, edge_zone } => {
                let guard = data.lock().unwrap();
                let Ok(surface) = guard.surface.upgrade() else {
                    resource.post_error(
                        layer_auto_hide_v1::Error::SurfaceDestroyed as u32,
                        format!("{:?} No surface found", resource),
                    );
                    return;
                };
                let edge_value: u32 = match edge {
                    smithay::reexports::wayland_server::WEnum::Value(v) => v as u32,
                    smithay::reexports::wayland_server::WEnum::Unknown(v) => v,
                };
                let Some(auto_edge) = AutoHideEdge::from_protocol(edge_value) else {
                    return;
                };
                // Store auto-hide in surface cached state
                with_states(&surface, |s| {
                    let mut cached = s.cached_state.get::<CacheableAutoHide>();
                    let pending = cached.pending();
                    *pending = CacheableAutoHide(Some(auto_edge));
                });
                // Store edge zone in surface cached state
                with_states(&surface, |s| {
                    let mut cached = s.cached_state.get::<CacheableEdgeZone>();
                    let pending = cached.pending();
                    *pending = CacheableEdgeZone(edge_zone);
                });
                drop(guard);
                // Notify compositor to register auto-hide tracking
                state.auto_hide_registered(&surface, auto_edge);
            }
            layer_auto_hide_v1::Request::UnsetAutoHide => {
                let guard = data.lock().unwrap();
                let Ok(surface) = guard.surface.upgrade() else {
                    resource.post_error(
                        layer_auto_hide_v1::Error::SurfaceDestroyed as u32,
                        format!("{:?} No surface found", resource),
                    );
                    return;
                };
                // Clear surface cached state
                with_states(&surface, |s| {
                    let mut cached = s.cached_state.get::<CacheableAutoHide>();
                    let pending = cached.pending();
                    *pending = CacheableAutoHide(None);
                });
                with_states(&surface, |s| {
                    let mut cached = s.cached_state.get::<CacheableEdgeZone>();
                    let pending = cached.pending();
                    *pending = CacheableEdgeZone(0);
                });
                drop(guard);
                // Notify compositor to unregister auto-hide tracking
                state.auto_hide_unregistered(&surface);
            }
        }
    }
}

pub type LayerAutoHideData = Mutex<LayerAutoHideInternal>;

#[derive(Debug)]
pub struct LayerAutoHideInternal {
    pub surface: Weak<WlSurface>,
}

macro_rules! delegate_layer_auto_hide {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::layer_auto_hide::layer_auto_hide_manager_v1::LayerAutoHideManagerV1: ()
        ] => $crate::wayland::protocols::layer_auto_hide::LayerAutoHideState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::layer_auto_hide::layer_auto_hide_manager_v1::LayerAutoHideManagerV1: ()
        ] => $crate::wayland::protocols::layer_auto_hide::LayerAutoHideState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::layer_auto_hide::layer_auto_hide_v1::LayerAutoHideV1: $crate::wayland::protocols::layer_auto_hide::LayerAutoHideData
        ] => $crate::wayland::protocols::layer_auto_hide::LayerAutoHideState);
    };
}
pub(crate) use delegate_layer_auto_hide;
