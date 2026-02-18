// SPDX-License-Identifier: GPL-3.0-only

//! Implementation of the backdrop color protocol (backdrop_color_manager_v1)
//!
//! This protocol allows clients to specify a compositor-rendered backdrop color
//! behind their surface content. The backdrop is drawn between the wallpaper
//! and the window content, allowing compositor-managed elements to be layered
//! between the backdrop and the window.

pub use generated::{backdrop_color_manager_v1, backdrop_color_surface_v1};

#[allow(non_snake_case, non_upper_case_globals, non_camel_case_types)]
mod generated {
    use smithay::reexports::wayland_server::{self, protocol::*};

    pub mod __interfaces {
        use smithay::reexports::wayland_server::protocol::__interfaces::*;
        use wayland_backend;
        wayland_scanner::generate_interfaces!("resources/protocols/backdrop-color.xml");
    }
    use self::__interfaces::*;

    wayland_scanner::generate_server_code!("resources/protocols/backdrop-color.xml");
}

use smithay::utils::HookId;
use smithay::wayland::compositor::Cacheable;
use smithay::{
    reexports::wayland_server::{
        Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource, Weak,
        backend::GlobalId, protocol::wl_surface::WlSurface,
    },
    wayland::compositor::{add_pre_commit_hook, with_states},
};
use std::sync::Mutex;

type SurfaceHookId = Mutex<
    Option<(
        HookId,
        Weak<backdrop_color_surface_v1::BackdropColorSurfaceV1>,
    )>,
>;

/// RGBA color with components in 0-255 range
#[derive(Debug, Copy, Clone)]
pub struct BackdropColor {
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub a: u8,
}

impl BackdropColor {
    /// Convert to normalized [r, g, b] float array for shader use
    pub fn to_rgb_f32(&self) -> [f32; 3] {
        [
            self.r as f32 / 255.0,
            self.g as f32 / 255.0,
            self.b as f32 / 255.0,
        ]
    }

    /// Convert alpha to normalized float for shader use
    pub fn alpha_f32(&self) -> f32 {
        self.a as f32 / 255.0
    }
}

/// Cacheable backdrop color state for a surface
#[derive(Default, Debug, Copy, Clone)]
pub struct CacheableBackdropColor(pub Option<BackdropColor>);

impl Cacheable for CacheableBackdropColor {
    fn commit(&mut self, _dh: &DisplayHandle) -> Self {
        *self
    }
    fn merge_into(self, into: &mut Self, _dh: &DisplayHandle) {
        *into = self;
    }
}

/// State for the backdrop color manager protocol
#[derive(Debug)]
pub struct BackdropColorState {
    global: GlobalId,
}

impl BackdropColorState {
    /// Create a new backdrop color manager global
    pub fn new<D>(dh: &DisplayHandle) -> BackdropColorState
    where
        D: GlobalDispatch<backdrop_color_manager_v1::BackdropColorManagerV1, ()>
            + Dispatch<backdrop_color_manager_v1::BackdropColorManagerV1, ()>
            + Dispatch<backdrop_color_surface_v1::BackdropColorSurfaceV1, BackdropColorData>
            + BackdropColorHandler
            + 'static,
    {
        let global =
            dh.create_global::<D, backdrop_color_manager_v1::BackdropColorManagerV1, _>(1, ());
        BackdropColorState { global }
    }

    /// Get the global ID of the backdrop color manager
    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }
}

/// Handler trait for backdrop color events
pub trait BackdropColorHandler {
    /// Get the backdrop color state
    fn backdrop_color_state(&mut self) -> &mut BackdropColorState;
}

impl<D> GlobalDispatch<backdrop_color_manager_v1::BackdropColorManagerV1, (), D>
    for BackdropColorState
where
    D: GlobalDispatch<backdrop_color_manager_v1::BackdropColorManagerV1, ()>
        + Dispatch<backdrop_color_manager_v1::BackdropColorManagerV1, ()>
        + Dispatch<backdrop_color_surface_v1::BackdropColorSurfaceV1, BackdropColorData>
        + BackdropColorHandler
        + 'static,
{
    fn bind(
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<backdrop_color_manager_v1::BackdropColorManagerV1>,
        _global_data: &(),
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }
}

impl<D> Dispatch<backdrop_color_manager_v1::BackdropColorManagerV1, (), D> for BackdropColorState
where
    D: GlobalDispatch<backdrop_color_manager_v1::BackdropColorManagerV1, ()>
        + Dispatch<backdrop_color_manager_v1::BackdropColorManagerV1, ()>
        + Dispatch<backdrop_color_surface_v1::BackdropColorSurfaceV1, BackdropColorData>
        + BackdropColorHandler
        + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        resource: &backdrop_color_manager_v1::BackdropColorManagerV1,
        request: backdrop_color_manager_v1::Request,
        _data: &(),
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            backdrop_color_manager_v1::Request::Destroy => {}
            backdrop_color_manager_v1::Request::GetBackdropColor { id, surface } => {
                let color_exists = with_states(&surface, |surface_data| {
                    let hook_id = surface_data
                        .data_map
                        .get_or_insert_threadsafe(|| SurfaceHookId::new(None));
                    let guard = hook_id.lock().unwrap();
                    guard.as_ref().map(|(_, t)| t.upgrade().is_ok())
                });

                if color_exists.unwrap_or_default() {
                    resource.post_error(
                        backdrop_color_manager_v1::Error::BackdropColorExists as u32,
                        format!(
                            "{resource:?} BackdropColorSurfaceV1 object already exists for the surface"
                        ),
                    );
                    return;
                }

                let data = Mutex::new(BackdropColorInternal {
                    surface: surface.downgrade(),
                    color: None,
                });
                let obj = data_init.init(id, data);
                let obj_downgrade = obj.downgrade();

                let needs_hook = color_exists.is_none();
                if needs_hook {
                    let hook_id = add_pre_commit_hook::<D, _>(&surface, move |_, _dh, _surface| {
                        // No additional validation needed
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

impl<D> Dispatch<backdrop_color_surface_v1::BackdropColorSurfaceV1, BackdropColorData, D>
    for BackdropColorState
where
    D: GlobalDispatch<backdrop_color_manager_v1::BackdropColorManagerV1, ()>
        + Dispatch<backdrop_color_manager_v1::BackdropColorManagerV1, ()>
        + Dispatch<backdrop_color_surface_v1::BackdropColorSurfaceV1, BackdropColorData>
        + BackdropColorHandler
        + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        resource: &backdrop_color_surface_v1::BackdropColorSurfaceV1,
        request: backdrop_color_surface_v1::Request,
        data: &BackdropColorData,
        _dhandle: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            backdrop_color_surface_v1::Request::Destroy => {
                let mut guard = data.lock().unwrap();
                guard.color = None;

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

                // Clear cached color
                with_states(&surface, |s| {
                    let mut cached = s.cached_state.get::<CacheableBackdropColor>();
                    let pending = cached.pending();
                    *pending = CacheableBackdropColor(None);
                });
            }
            backdrop_color_surface_v1::Request::SetColor { r, g, b, a } => {
                let mut guard = data.lock().unwrap();
                guard.color = Some(BackdropColor {
                    r: r.clamp(0, 255) as u8,
                    g: g.clamp(0, 255) as u8,
                    b: b.clamp(0, 255) as u8,
                    a: a.clamp(0, 255) as u8,
                });

                let Ok(surface) = guard.surface.upgrade() else {
                    resource.post_error(
                        backdrop_color_surface_v1::Error::SurfaceDestroyed as u32,
                        format!("{:?} No surface found", resource),
                    );
                    return;
                };

                with_states(&surface, |s| {
                    let mut cached = s.cached_state.get::<CacheableBackdropColor>();
                    let pending = cached.pending();
                    *pending = CacheableBackdropColor(guard.color);
                });
            }
            backdrop_color_surface_v1::Request::UnsetColor => {
                let mut guard = data.lock().unwrap();
                guard.color = None;

                let Ok(surface) = guard.surface.upgrade() else {
                    resource.post_error(
                        backdrop_color_surface_v1::Error::SurfaceDestroyed as u32,
                        format!("{:?} No surface found", resource),
                    );
                    return;
                };

                with_states(&surface, |s| {
                    let mut cached = s.cached_state.get::<CacheableBackdropColor>();
                    let pending = cached.pending();
                    *pending = CacheableBackdropColor(None);
                });
            }
        }
    }
}

pub type BackdropColorData = Mutex<BackdropColorInternal>;

#[derive(Debug)]
pub struct BackdropColorInternal {
    pub surface: Weak<WlSurface>,
    pub color: Option<BackdropColor>,
}

macro_rules! delegate_backdrop_color {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::backdrop_color::backdrop_color_manager_v1::BackdropColorManagerV1: ()
        ] => $crate::wayland::protocols::backdrop_color::BackdropColorState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::backdrop_color::backdrop_color_manager_v1::BackdropColorManagerV1: ()
        ] => $crate::wayland::protocols::backdrop_color::BackdropColorState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::backdrop_color::backdrop_color_surface_v1::BackdropColorSurfaceV1: $crate::wayland::protocols::backdrop_color::BackdropColorData
        ] => $crate::wayland::protocols::backdrop_color::BackdropColorState);
    };
}
pub(crate) use delegate_backdrop_color;

/// Get backdrop color from any WlSurface that has CacheableBackdropColor set.
/// Returns None if no backdrop color is set.
pub fn get_surface_backdrop_color(surface: &WlSurface) -> Option<BackdropColor> {
    with_states(surface, |states| {
        let mut guard = states.cached_state.get::<CacheableBackdropColor>();
        guard.current().0
    })
}
