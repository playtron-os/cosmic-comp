// SPDX-License-Identifier: GPL-3.0-only

//! Implementation of the KDE blur protocol (org_kde_kwin_blur_manager)
//!
//! This protocol allows clients to request blur effects for surfaces.
//! The blur effect can be applied to a specific region or the entire surface.

// Re-export only the actual code, and then only use this re-export
// The `generated` module below is just some boilerplate to properly isolate stuff
// and avoid exposing internal details.
pub use generated::{org_kde_kwin_blur, org_kde_kwin_blur_manager};

#[allow(non_snake_case, non_upper_case_globals, non_camel_case_types)]
mod generated {
    use smithay::reexports::wayland_server::{self, protocol::*};

    pub mod __interfaces {
        use smithay::reexports::wayland_server::protocol::__interfaces::*;
        use wayland_backend;
        wayland_scanner::generate_interfaces!("resources/protocols/blur.xml");
    }
    use self::__interfaces::*;

    wayland_scanner::generate_server_code!("resources/protocols/blur.xml");
}

use smithay::{
    reexports::wayland_server::{
        Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource, Weak,
        backend::GlobalId, protocol::wl_surface::WlSurface,
    },
    utils::{Logical, Rectangle},
    wayland::compositor::{Cacheable, with_states},
};
use std::sync::Mutex;

/// Blur region data stored per-surface
#[derive(Debug, Clone, Default)]
pub struct BlurRegionData {
    /// The blur region. If None, the entire surface should be blurred.
    pub region: Option<Vec<Rectangle<i32, Logical>>>,
}

/// Cacheable blur state for double-buffering
#[derive(Debug, Clone, Default)]
pub struct CacheableBlurState {
    /// Whether blur is enabled for this surface
    pub enabled: bool,
    /// The blur region data
    pub data: Option<BlurRegionData>,
}

impl Cacheable for CacheableBlurState {
    fn commit(&mut self, _dh: &DisplayHandle) -> Self {
        self.clone()
    }
    fn merge_into(self, into: &mut Self, _dh: &DisplayHandle) {
        *into = self;
    }
}

/// User data for the blur object
pub struct BlurData {
    surface: Weak<WlSurface>,
    pending_region: Mutex<Option<Vec<Rectangle<i32, Logical>>>>,
}

/// State for the blur manager protocol
#[derive(Debug)]
pub struct BlurState {
    global: GlobalId,
}

impl BlurState {
    /// Create a new blur manager global
    pub fn new<D>(dh: &DisplayHandle) -> BlurState
    where
        D: GlobalDispatch<org_kde_kwin_blur_manager::OrgKdeKwinBlurManager, ()>
            + Dispatch<org_kde_kwin_blur_manager::OrgKdeKwinBlurManager, ()>
            + Dispatch<org_kde_kwin_blur::OrgKdeKwinBlur, BlurData>
            + BlurHandler
            + 'static,
    {
        let global =
            dh.create_global::<D, org_kde_kwin_blur_manager::OrgKdeKwinBlurManager, _>(1, ());
        BlurState { global }
    }

    /// Get the global ID of the blur manager
    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }
}

/// Handler trait for blur events
pub trait BlurHandler {
    /// Get the blur state
    fn blur_state(&mut self) -> &mut BlurState;

    /// Called when blur is set on a surface
    fn blur_set(&mut self, surface: &WlSurface);

    /// Called when blur is unset from a surface
    fn blur_unset(&mut self, surface: &WlSurface);
}

impl<D> GlobalDispatch<org_kde_kwin_blur_manager::OrgKdeKwinBlurManager, (), D> for BlurState
where
    D: GlobalDispatch<org_kde_kwin_blur_manager::OrgKdeKwinBlurManager, ()>
        + Dispatch<org_kde_kwin_blur_manager::OrgKdeKwinBlurManager, ()>
        + Dispatch<org_kde_kwin_blur::OrgKdeKwinBlur, BlurData>
        + BlurHandler
        + 'static,
{
    fn bind(
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<org_kde_kwin_blur_manager::OrgKdeKwinBlurManager>,
        _global_data: &(),
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }
}

impl<D> Dispatch<org_kde_kwin_blur_manager::OrgKdeKwinBlurManager, (), D> for BlurState
where
    D: GlobalDispatch<org_kde_kwin_blur_manager::OrgKdeKwinBlurManager, ()>
        + Dispatch<org_kde_kwin_blur_manager::OrgKdeKwinBlurManager, ()>
        + Dispatch<org_kde_kwin_blur::OrgKdeKwinBlur, BlurData>
        + BlurHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        _resource: &org_kde_kwin_blur_manager::OrgKdeKwinBlurManager,
        request: org_kde_kwin_blur_manager::Request,
        _data: &(),
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            org_kde_kwin_blur_manager::Request::Create { id, surface } => {
                let blur_data = BlurData {
                    surface: surface.downgrade(),
                    pending_region: Mutex::new(None),
                };
                data_init.init(id, blur_data);
            }
            org_kde_kwin_blur_manager::Request::Unset { surface } => {
                // Remove blur from the surface
                with_states(&surface, |surface_data| {
                    let mut cached = surface_data.cached_state.get::<CacheableBlurState>();
                    let pending = cached.pending();
                    pending.enabled = false;
                    pending.data = None;
                });
                state.blur_unset(&surface);
            }
        }
    }
}

impl<D> Dispatch<org_kde_kwin_blur::OrgKdeKwinBlur, BlurData, D> for BlurState
where
    D: GlobalDispatch<org_kde_kwin_blur_manager::OrgKdeKwinBlurManager, ()>
        + Dispatch<org_kde_kwin_blur_manager::OrgKdeKwinBlurManager, ()>
        + Dispatch<org_kde_kwin_blur::OrgKdeKwinBlur, BlurData>
        + BlurHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        _resource: &org_kde_kwin_blur::OrgKdeKwinBlur,
        request: org_kde_kwin_blur::Request,
        data: &BlurData,
        _dhandle: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            org_kde_kwin_blur::Request::Commit => {
                let Ok(surface) = data.surface.upgrade() else {
                    return;
                };

                let pending_region = data.pending_region.lock().unwrap().take();

                tracing::trace!(
                    surface_id = surface.id().protocol_id(),
                    has_region = pending_region.is_some(),
                    "[BLUR-TIMING] 1/5 Client committed blur protocol for surface"
                );

                with_states(&surface, |surface_data| {
                    let mut cached = surface_data.cached_state.get::<CacheableBlurState>();
                    let pending = cached.pending();
                    pending.enabled = true;
                    pending.data = Some(BlurRegionData {
                        region: pending_region,
                    });
                });

                state.blur_set(&surface);
            }
            org_kde_kwin_blur::Request::SetRegion { region } => {
                let regions = region.and_then(|r| {
                    r.data::<smithay::wayland::compositor::RegionAttributes>()
                        .map(|attrs| {
                            attrs
                                .rects
                                .iter()
                                .map(|(_, rect)| *rect)
                                .collect::<Vec<_>>()
                        })
                });
                *data.pending_region.lock().unwrap() = regions;
            }
            org_kde_kwin_blur::Request::Release => {
                // The destructor is handled automatically
                if let Ok(surface) = data.surface.upgrade() {
                    with_states(&surface, |surface_data| {
                        let mut cached = surface_data.cached_state.get::<CacheableBlurState>();
                        let pending = cached.pending();
                        pending.enabled = false;
                        pending.data = None;
                    });
                    state.blur_unset(&surface);
                }
            }
        }
    }
}

/// Helper function to get the blur state for a surface
pub fn get_blur_state(surface: &WlSurface) -> Option<CacheableBlurState> {
    Some(with_states(surface, |surface_data| {
        surface_data
            .cached_state
            .get::<CacheableBlurState>()
            .current()
            .clone()
    }))
}

/// Check if a surface has blur enabled
pub fn has_blur(surface: &WlSurface) -> bool {
    let result = get_blur_state(surface)
        .map(|state| state.enabled)
        .unwrap_or(false);
    // Only log when blur is enabled to avoid spam
    if result {
        tracing::trace!(
            surface_id = surface.id().protocol_id(),
            "Surface has blur enabled"
        );
    }
    result
}

/// Macro to delegate blur protocol handling
#[macro_export]
macro_rules! delegate_blur {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::blur::org_kde_kwin_blur_manager::OrgKdeKwinBlurManager: ()
        ] => $crate::wayland::protocols::blur::BlurState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::blur::org_kde_kwin_blur_manager::OrgKdeKwinBlurManager: ()
        ] => $crate::wayland::protocols::blur::BlurState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::blur::org_kde_kwin_blur::OrgKdeKwinBlur: $crate::wayland::protocols::blur::BlurData
        ] => $crate::wayland::protocols::blur::BlurState);
    };
}
pub(crate) use delegate_blur;
