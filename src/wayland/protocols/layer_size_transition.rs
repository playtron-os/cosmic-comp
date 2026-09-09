// SPDX-License-Identifier: GPL-3.0-only

//! Tells a layer surface once when its arranged size is about to change over
//! an animation, so it can move its content itself instead of being
//! configured on every frame.

pub use generated::{layer_size_transition_manager_v1, layer_size_transition_v1};

#[allow(non_snake_case, non_upper_case_globals, non_camel_case_types)]
mod generated {
    use smithay::reexports::wayland_server::{self, protocol::*};

    pub mod __interfaces {
        use smithay::reexports::wayland_server::protocol::__interfaces::*;
        use wayland_backend;
        wayland_scanner::generate_interfaces!("resources/protocols/layer-size-transition.xml");
    }
    use self::__interfaces::*;

    wayland_scanner::generate_server_code!("resources/protocols/layer-size-transition.xml");
}

use smithay::{
    reexports::wayland_server::{
        Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource, Weak,
        backend::GlobalId, protocol::wl_surface::WlSurface,
    },
    utils::{Logical, Size},
    wayland::compositor::with_states,
};
use std::sync::Mutex;

pub type SizeTransitionData = Mutex<SizeTransitionInternal>;

#[derive(Debug)]
pub struct SizeTransitionInternal {
    pub surface: Weak<WlSurface>,
}

/// The object a surface's client bound for it, kept in the surface's data map.
type SurfaceSizeTransition = Mutex<Option<Weak<layer_size_transition_v1::LayerSizeTransitionV1>>>;

#[derive(Debug)]
pub struct SizeTransitionState {
    global: GlobalId,
}

impl SizeTransitionState {
    pub fn new<D>(dh: &DisplayHandle) -> SizeTransitionState
    where
        D: GlobalDispatch<layer_size_transition_manager_v1::LayerSizeTransitionManagerV1, ()>
            + Dispatch<layer_size_transition_manager_v1::LayerSizeTransitionManagerV1, ()>
            + Dispatch<layer_size_transition_v1::LayerSizeTransitionV1, SizeTransitionData>
            + SizeTransitionHandler
            + 'static,
    {
        let global = dh
            .create_global::<D, layer_size_transition_manager_v1::LayerSizeTransitionManagerV1, _>(
                1,
                (),
            );
        SizeTransitionState { global }
    }

    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }
}

/// Whether `surface`'s client asked to hear about its size transitions.
pub fn wants_size_transitions(surface: &WlSurface) -> bool {
    with_object(surface, |_| ()).is_some()
}

/// Announce that `surface` goes from `from` to `to` over `duration_ms`.
pub fn send_started(
    surface: &WlSurface,
    from: Size<i32, Logical>,
    to: Size<i32, Logical>,
    duration_ms: u32,
) {
    with_object(surface, |obj| {
        obj.started(from.w, from.h, to.w, to.h, duration_ms)
    });
}

/// Announce that `surface` has been configured to its final `size`.
pub fn send_finished(surface: &WlSurface, size: Size<i32, Logical>) {
    with_object(surface, |obj| obj.finished(size.w, size.h));
}

fn with_object<R>(
    surface: &WlSurface,
    f: impl FnOnce(&layer_size_transition_v1::LayerSizeTransitionV1) -> R,
) -> Option<R> {
    with_states(surface, |states| {
        let entry = states.data_map.get::<SurfaceSizeTransition>()?;
        let guard = entry.lock().unwrap();
        let obj = guard.as_ref()?.upgrade().ok()?;
        Some(f(&obj))
    })
}

pub trait SizeTransitionHandler {
    fn size_transition_state(&mut self) -> &mut SizeTransitionState;
}

impl<D> GlobalDispatch<layer_size_transition_manager_v1::LayerSizeTransitionManagerV1, (), D>
    for SizeTransitionState
where
    D: GlobalDispatch<layer_size_transition_manager_v1::LayerSizeTransitionManagerV1, ()>
        + Dispatch<layer_size_transition_manager_v1::LayerSizeTransitionManagerV1, ()>
        + Dispatch<layer_size_transition_v1::LayerSizeTransitionV1, SizeTransitionData>
        + SizeTransitionHandler
        + 'static,
{
    fn bind(
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<layer_size_transition_manager_v1::LayerSizeTransitionManagerV1>,
        _global_data: &(),
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }
}

impl<D> Dispatch<layer_size_transition_manager_v1::LayerSizeTransitionManagerV1, (), D>
    for SizeTransitionState
where
    D: GlobalDispatch<layer_size_transition_manager_v1::LayerSizeTransitionManagerV1, ()>
        + Dispatch<layer_size_transition_manager_v1::LayerSizeTransitionManagerV1, ()>
        + Dispatch<layer_size_transition_v1::LayerSizeTransitionV1, SizeTransitionData>
        + SizeTransitionHandler
        + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        resource: &layer_size_transition_manager_v1::LayerSizeTransitionManagerV1,
        request: layer_size_transition_manager_v1::Request,
        _data: &(),
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            layer_size_transition_manager_v1::Request::Destroy => {}
            layer_size_transition_manager_v1::Request::GetSizeTransition { id, surface } => {
                if wants_size_transitions(&surface) {
                    resource.post_error(
                        layer_size_transition_manager_v1::Error::AlreadyExists as u32,
                        format!(
                            "{resource:?} LayerSizeTransitionV1 already exists for the surface"
                        ),
                    );
                    return;
                }
                let data = Mutex::new(SizeTransitionInternal {
                    surface: surface.downgrade(),
                });
                let obj = data_init.init(id, data);
                with_states(&surface, |states| {
                    let entry = states
                        .data_map
                        .get_or_insert_threadsafe(SurfaceSizeTransition::default);
                    *entry.lock().unwrap() = Some(obj.downgrade());
                });
            }
        }
    }
}

impl<D> Dispatch<layer_size_transition_v1::LayerSizeTransitionV1, SizeTransitionData, D>
    for SizeTransitionState
where
    D: GlobalDispatch<layer_size_transition_manager_v1::LayerSizeTransitionManagerV1, ()>
        + Dispatch<layer_size_transition_manager_v1::LayerSizeTransitionManagerV1, ()>
        + Dispatch<layer_size_transition_v1::LayerSizeTransitionV1, SizeTransitionData>
        + SizeTransitionHandler
        + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        _resource: &layer_size_transition_v1::LayerSizeTransitionV1,
        request: layer_size_transition_v1::Request,
        data: &SizeTransitionData,
        _dhandle: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            layer_size_transition_v1::Request::Destroy => {
                let guard = data.lock().unwrap();
                if let Ok(surface) = guard.surface.upgrade() {
                    with_states(&surface, |states| {
                        if let Some(entry) = states.data_map.get::<SurfaceSizeTransition>() {
                            *entry.lock().unwrap() = None;
                        }
                    });
                }
            }
        }
    }
}

macro_rules! delegate_layer_size_transition {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::layer_size_transition::layer_size_transition_manager_v1::LayerSizeTransitionManagerV1: ()
        ] => $crate::wayland::protocols::layer_size_transition::SizeTransitionState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::layer_size_transition::layer_size_transition_manager_v1::LayerSizeTransitionManagerV1: ()
        ] => $crate::wayland::protocols::layer_size_transition::SizeTransitionState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::layer_size_transition::layer_size_transition_v1::LayerSizeTransitionV1: $crate::wayland::protocols::layer_size_transition::SizeTransitionData
        ] => $crate::wayland::protocols::layer_size_transition::SizeTransitionState);
    };
}
pub(crate) use delegate_layer_size_transition;
