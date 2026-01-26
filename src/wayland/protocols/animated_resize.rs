// SPDX-License-Identifier: GPL-3.0-only

//! Implementation of the COSMIC animated resize protocol (zcosmic_animated_resize_v1)
//!
//! This protocol allows clients to request smooth animated window resizes.
//! The compositor sends intermediate configure events for smooth animation.

pub use generated::{zcosmic_animated_resize_manager_v1, zcosmic_animated_resize_v1};

#[allow(non_snake_case, non_upper_case_globals, non_camel_case_types)]
mod generated {
    use smithay::reexports::wayland_server::{self, protocol::*};

    pub mod __interfaces {
        use smithay::reexports::wayland_server::protocol::__interfaces::*;
        use wayland_backend;
        wayland_scanner::generate_interfaces!("resources/protocols/animated_resize.xml");
    }
    use self::__interfaces::*;

    wayland_scanner::generate_server_code!("resources/protocols/animated_resize.xml");
}

use smithay::{
    reexports::wayland_server::{
        Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource, Weak,
        backend::GlobalId, protocol::wl_surface::WlSurface,
    },
    utils::{Logical, Size},
};
use std::sync::Mutex;
use std::time::{Duration, Instant};

/// State for an active animated resize
#[derive(Debug, Clone)]
pub struct AnimatedResizeData {
    /// Target size in logical pixels
    pub target_size: Size<i32, Logical>,
    /// Animation duration
    pub duration: Duration,
    /// When the animation started
    pub start_time: Instant,
    /// The protocol resource for sending events
    pub resource: Weak<zcosmic_animated_resize_v1::ZcosmicAnimatedResizeV1>,
}

/// User data for the animated resize controller
pub struct AnimatedResizeControllerData {
    /// The wl_surface this controls (should be root surface of an xdg_toplevel)
    surface: Weak<WlSurface>,
    /// Current animation state
    animation: Mutex<Option<AnimatedResizeData>>,
}

/// State for the animated resize manager protocol
#[derive(Debug)]
pub struct AnimatedResizeState {
    global: GlobalId,
}

impl AnimatedResizeState {
    /// Create a new animated resize manager global
    pub fn new<D>(dh: &DisplayHandle) -> AnimatedResizeState
    where
        D: GlobalDispatch<zcosmic_animated_resize_manager_v1::ZcosmicAnimatedResizeManagerV1, ()>
            + Dispatch<zcosmic_animated_resize_manager_v1::ZcosmicAnimatedResizeManagerV1, ()>
            + Dispatch<
                zcosmic_animated_resize_v1::ZcosmicAnimatedResizeV1,
                AnimatedResizeControllerData,
            > + AnimatedResizeHandler
            + 'static,
    {
        let global = dh.create_global::<
            D,
            zcosmic_animated_resize_manager_v1::ZcosmicAnimatedResizeManagerV1,
            _,
        >(1, ());
        AnimatedResizeState { global }
    }

    /// Get the global ID
    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }
}

/// Handler trait for animated resize events
pub trait AnimatedResizeHandler {
    /// Get the animated resize state
    fn animated_resize_state(&mut self) -> &mut AnimatedResizeState;

    /// Called when a client requests an animated resize (size only)
    fn animated_resize_request(
        &mut self,
        surface: &WlSurface,
        target_width: i32,
        target_height: i32,
        duration_ms: u32,
    );

    /// Called when a client requests an animated resize with position
    fn animated_resize_request_with_position(
        &mut self,
        surface: &WlSurface,
        target_x: i32,
        target_y: i32,
        target_width: i32,
        target_height: i32,
        duration_ms: u32,
    );
}

impl<D> GlobalDispatch<zcosmic_animated_resize_manager_v1::ZcosmicAnimatedResizeManagerV1, (), D>
    for AnimatedResizeState
where
    D: GlobalDispatch<zcosmic_animated_resize_manager_v1::ZcosmicAnimatedResizeManagerV1, ()>
        + Dispatch<zcosmic_animated_resize_manager_v1::ZcosmicAnimatedResizeManagerV1, ()>
        + Dispatch<zcosmic_animated_resize_v1::ZcosmicAnimatedResizeV1, AnimatedResizeControllerData>
        + AnimatedResizeHandler
        + 'static,
{
    fn bind(
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<zcosmic_animated_resize_manager_v1::ZcosmicAnimatedResizeManagerV1>,
        _global_data: &(),
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }
}

impl<D> Dispatch<zcosmic_animated_resize_manager_v1::ZcosmicAnimatedResizeManagerV1, (), D>
    for AnimatedResizeState
where
    D: GlobalDispatch<zcosmic_animated_resize_manager_v1::ZcosmicAnimatedResizeManagerV1, ()>
        + Dispatch<zcosmic_animated_resize_manager_v1::ZcosmicAnimatedResizeManagerV1, ()>
        + Dispatch<zcosmic_animated_resize_v1::ZcosmicAnimatedResizeV1, AnimatedResizeControllerData>
        + AnimatedResizeHandler
        + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        _resource: &zcosmic_animated_resize_manager_v1::ZcosmicAnimatedResizeManagerV1,
        request: zcosmic_animated_resize_manager_v1::Request,
        _data: &(),
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_animated_resize_manager_v1::Request::GetAnimatedResize { id, surface } => {
                // Store the wl_surface reference
                let controller_data = AnimatedResizeControllerData {
                    surface: surface.downgrade(),
                    animation: Mutex::new(None),
                };
                data_init.init(id, controller_data);
            }
            zcosmic_animated_resize_manager_v1::Request::Destroy => {}
        }
    }
}

impl<D>
    Dispatch<zcosmic_animated_resize_v1::ZcosmicAnimatedResizeV1, AnimatedResizeControllerData, D>
    for AnimatedResizeState
where
    D: GlobalDispatch<zcosmic_animated_resize_manager_v1::ZcosmicAnimatedResizeManagerV1, ()>
        + Dispatch<zcosmic_animated_resize_manager_v1::ZcosmicAnimatedResizeManagerV1, ()>
        + Dispatch<zcosmic_animated_resize_v1::ZcosmicAnimatedResizeV1, AnimatedResizeControllerData>
        + AnimatedResizeHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        resource: &zcosmic_animated_resize_v1::ZcosmicAnimatedResizeV1,
        request: zcosmic_animated_resize_v1::Request,
        data: &AnimatedResizeControllerData,
        _dhandle: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_animated_resize_v1::Request::ResizeTo {
                width,
                height,
                duration_ms,
            } => {
                if let Ok(surface) = data.surface.upgrade() {
                    // Store animation state
                    let animation_data = AnimatedResizeData {
                        target_size: Size::from((width, height)),
                        duration: Duration::from_millis(duration_ms as u64),
                        start_time: Instant::now(),
                        resource: resource.downgrade(),
                    };
                    *data.animation.lock().unwrap() = Some(animation_data);

                    // Notify the handler
                    state.animated_resize_request(&surface, width, height, duration_ms);
                }
            }
            zcosmic_animated_resize_v1::Request::ResizeToWithPosition {
                x,
                y,
                width,
                height,
                duration_ms,
            } => {
                if let Ok(surface) = data.surface.upgrade() {
                    // Store animation state
                    let animation_data = AnimatedResizeData {
                        target_size: Size::from((width, height)),
                        duration: Duration::from_millis(duration_ms as u64),
                        start_time: Instant::now(),
                        resource: resource.downgrade(),
                    };
                    *data.animation.lock().unwrap() = Some(animation_data);

                    // Notify the handler with position
                    state.animated_resize_request_with_position(
                        &surface,
                        x,
                        y,
                        width,
                        height,
                        duration_ms,
                    );
                }
            }
            zcosmic_animated_resize_v1::Request::Destroy => {}
        }
    }
}

/// Macro to delegate animated resize protocol handling
#[macro_export]
macro_rules! delegate_animated_resize {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::animated_resize::zcosmic_animated_resize_manager_v1::ZcosmicAnimatedResizeManagerV1: ()
        ] => $crate::wayland::protocols::animated_resize::AnimatedResizeState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::animated_resize::zcosmic_animated_resize_manager_v1::ZcosmicAnimatedResizeManagerV1: ()
        ] => $crate::wayland::protocols::animated_resize::AnimatedResizeState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::animated_resize::zcosmic_animated_resize_v1::ZcosmicAnimatedResizeV1: $crate::wayland::protocols::animated_resize::AnimatedResizeControllerData
        ] => $crate::wayland::protocols::animated_resize::AnimatedResizeState);
    };
}
pub(crate) use delegate_animated_resize;
