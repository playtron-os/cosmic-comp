// SPDX-License-Identifier: GPL-3.0-only

//! Implementation of the capture exclusion protocol (kora_capture_exclusion_manager_v1)
//!
//! A client marks one of its own surfaces to be left out of screen captures; it
//! stays on screen. The flag is double-buffered surface state, read by the
//! renderer through [`is_excluded`].

pub use generated::{kora_capture_exclusion_manager_v1, kora_surface_capture_exclusion_v1};

#[allow(non_snake_case, non_upper_case_globals, non_camel_case_types)]
mod generated {
    use smithay::reexports::wayland_server::{self, protocol::*};

    pub mod __interfaces {
        use smithay::reexports::wayland_server::protocol::__interfaces::*;
        use wayland_backend;
        wayland_scanner::generate_interfaces!("resources/protocols/kora-capture-exclusion.xml");
    }
    use self::__interfaces::*;

    wayland_scanner::generate_server_code!("resources/protocols/kora-capture-exclusion.xml");
}

use smithay::{
    reexports::wayland_server::{
        Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource, Weak,
        backend::{ClientId, GlobalId},
        protocol::wl_surface::WlSurface,
    },
    wayland::compositor::{Cacheable, SurfaceData, with_states},
};
use std::sync::Mutex;

use kora_capture_exclusion_manager_v1::KoraCaptureExclusionManagerV1;
use kora_surface_capture_exclusion_v1::KoraSurfaceCaptureExclusionV1;

/// The double-buffered flag on a surface.
#[derive(Debug, Default, Clone, Copy)]
struct Excluded(bool);

impl Cacheable for Excluded {
    fn commit(&mut self, _dh: &DisplayHandle) -> Self {
        *self
    }
    fn merge_into(self, into: &mut Self, _dh: &DisplayHandle) {
        *into = self;
    }
}

/// The surface's one exclusion object, so a second can be refused.
#[derive(Debug, Default)]
struct ExclusionObject(Mutex<Option<Weak<KoraSurfaceCaptureExclusionV1>>>);

/// Whether the surface's applied state leaves it out of captures.
pub fn is_excluded(states: &SurfaceData) -> bool {
    states.cached_state.has::<Excluded>() && states.cached_state.get::<Excluded>().current().0
}

fn set_pending(surface: &WlSurface, excluded: bool) {
    with_states(surface, |states| {
        states.cached_state.get::<Excluded>().pending().0 = excluded;
    });
}

/// Forget `object` as its surface's exclusion object, putting the surface back
/// into captures from its next commit.
fn release(object: &KoraSurfaceCaptureExclusionV1, surface: &Weak<WlSurface>) {
    let Ok(surface) = surface.upgrade() else {
        return;
    };
    set_pending(&surface, false);
    with_states(&surface, |states| {
        if let Some(slot) = states.data_map.get::<ExclusionObject>() {
            let mut slot = slot.0.lock().unwrap();
            if slot.as_ref().is_some_and(|held| held.id() == object.id()) {
                *slot = None;
            }
        }
    });
}

#[derive(Debug)]
pub struct CaptureExclusionState {
    _global: GlobalId,
}

impl CaptureExclusionState {
    pub fn new<D>(dh: &DisplayHandle) -> CaptureExclusionState
    where
        D: GlobalDispatch<KoraCaptureExclusionManagerV1, ()>
            + Dispatch<KoraCaptureExclusionManagerV1, ()>
            + Dispatch<KoraSurfaceCaptureExclusionV1, Weak<WlSurface>>
            + 'static,
    {
        let global = dh.create_global::<D, KoraCaptureExclusionManagerV1, _>(1, ());
        CaptureExclusionState { _global: global }
    }
}

impl<D> GlobalDispatch<KoraCaptureExclusionManagerV1, (), D> for CaptureExclusionState
where
    D: GlobalDispatch<KoraCaptureExclusionManagerV1, ()>
        + Dispatch<KoraCaptureExclusionManagerV1, ()>
        + Dispatch<KoraSurfaceCaptureExclusionV1, Weak<WlSurface>>
        + 'static,
{
    fn bind(
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<KoraCaptureExclusionManagerV1>,
        _global_data: &(),
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }
}

impl<D> Dispatch<KoraCaptureExclusionManagerV1, (), D> for CaptureExclusionState
where
    D: GlobalDispatch<KoraCaptureExclusionManagerV1, ()>
        + Dispatch<KoraCaptureExclusionManagerV1, ()>
        + Dispatch<KoraSurfaceCaptureExclusionV1, Weak<WlSurface>>
        + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        resource: &KoraCaptureExclusionManagerV1,
        request: kora_capture_exclusion_manager_v1::Request,
        _data: &(),
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            kora_capture_exclusion_manager_v1::Request::Destroy => {}
            kora_capture_exclusion_manager_v1::Request::GetSurfaceExclusion { id, surface } => {
                let taken = with_states(&surface, |states| {
                    states
                        .data_map
                        .insert_if_missing_threadsafe(ExclusionObject::default);
                    let slot = states.data_map.get::<ExclusionObject>().unwrap();
                    slot.0
                        .lock()
                        .unwrap()
                        .as_ref()
                        .is_some_and(|held| held.upgrade().is_ok())
                });
                if taken {
                    resource.post_error(
                        kora_capture_exclusion_manager_v1::Error::AlreadyConstructed,
                        "the surface already has a capture exclusion object",
                    );
                    return;
                }
                let object = data_init.init(id, surface.downgrade());
                with_states(&surface, |states| {
                    let slot = states.data_map.get::<ExclusionObject>().unwrap();
                    *slot.0.lock().unwrap() = Some(object.downgrade());
                });
            }
        }
    }
}

impl<D> Dispatch<KoraSurfaceCaptureExclusionV1, Weak<WlSurface>, D> for CaptureExclusionState
where
    D: Dispatch<KoraSurfaceCaptureExclusionV1, Weak<WlSurface>> + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        _resource: &KoraSurfaceCaptureExclusionV1,
        request: kora_surface_capture_exclusion_v1::Request,
        surface: &Weak<WlSurface>,
        _dhandle: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            // `destroyed` follows, for this and for a client that disconnects.
            kora_surface_capture_exclusion_v1::Request::Destroy => {}
            kora_surface_capture_exclusion_v1::Request::SetExcluded { excluded } => {
                if let Ok(surface) = surface.upgrade() {
                    set_pending(&surface, excluded != 0);
                }
            }
        }
    }

    fn destroyed(
        _state: &mut D,
        _client: ClientId,
        resource: &KoraSurfaceCaptureExclusionV1,
        surface: &Weak<WlSurface>,
    ) {
        release(resource, surface);
    }
}

macro_rules! delegate_kora_capture_exclusion {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::kora_capture_exclusion::kora_capture_exclusion_manager_v1::KoraCaptureExclusionManagerV1: ()
        ] => $crate::wayland::protocols::kora_capture_exclusion::CaptureExclusionState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::kora_capture_exclusion::kora_capture_exclusion_manager_v1::KoraCaptureExclusionManagerV1: ()
        ] => $crate::wayland::protocols::kora_capture_exclusion::CaptureExclusionState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::kora_capture_exclusion::kora_surface_capture_exclusion_v1::KoraSurfaceCaptureExclusionV1: smithay::reexports::wayland_server::Weak<smithay::reexports::wayland_server::protocol::wl_surface::WlSurface>
        ] => $crate::wayland::protocols::kora_capture_exclusion::CaptureExclusionState);
    };
}
pub(crate) use delegate_kora_capture_exclusion;

#[cfg(test)]
#[path = "kora_capture_exclusion_tests.rs"]
mod tests;
