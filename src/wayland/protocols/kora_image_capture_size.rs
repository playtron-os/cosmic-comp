// SPDX-License-Identifier: GPL-3.0-only

//! Implementation of the capture-size protocol (kora_image_capture_size_manager_v1)
//!
//! A client asks for captures of a source rendered to fit within a size of its
//! own — a preview's — instead of the source's. The size is kept on the source;
//! the capture handlers read it when a session is created and a frame drawn.

pub use generated::kora_image_capture_size_manager_v1;

#[allow(non_snake_case, non_upper_case_globals, non_camel_case_types)]
mod generated {
    use smithay::reexports::wayland_protocols::ext::image_capture_source::v1::server::*;
    use smithay::reexports::wayland_server;

    pub mod __interfaces {
        use smithay::reexports::wayland_protocols::ext::image_capture_source::v1::server::__interfaces::*;
        use wayland_backend;
        wayland_scanner::generate_interfaces!("resources/protocols/kora-image-capture-size.xml");
    }
    use self::__interfaces::*;

    wayland_scanner::generate_server_code!("resources/protocols/kora-image-capture-size.xml");
}

use smithay::{
    reexports::wayland_server::{
        Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, backend::GlobalId,
    },
    utils::{Buffer as BufferCoords, Size},
    wayland::image_capture_source::ImageCaptureSource,
};
use std::sync::Mutex;

use kora_image_capture_size_manager_v1::KoraImageCaptureSizeManagerV1;

/// The size a source's captures were asked to fit within, kept on the source.
#[derive(Debug, Default)]
struct CaptureSizeHint(Mutex<Option<Size<i32, BufferCoords>>>);

/// The size `source`'s captures were asked to fit within, if any.
pub fn capture_size_hint(source: &ImageCaptureSource) -> Option<Size<i32, BufferCoords>> {
    source
        .user_data()
        .get::<CaptureSizeHint>()
        .and_then(|hint| *hint.0.lock().unwrap())
}

/// `size` shrunk to fit within `bounds`, keeping its aspect; itself when it fits.
pub fn fit_within(
    size: Size<i32, BufferCoords>,
    bounds: Size<i32, BufferCoords>,
) -> Size<i32, BufferCoords> {
    if (size.w <= bounds.w && size.h <= bounds.h) || size.w <= 0 || size.h <= 0 {
        return size;
    }
    let scale =
        (f64::from(bounds.w) / f64::from(size.w)).min(f64::from(bounds.h) / f64::from(size.h));
    let fitted = |side: i32| (f64::from(side) * scale).round().max(1.0) as i32;
    Size::from((fitted(size.w), fitted(size.h)))
}

#[derive(Debug)]
pub struct CaptureSizeState {
    global: GlobalId,
}

impl CaptureSizeState {
    pub fn new<D>(dh: &DisplayHandle) -> CaptureSizeState
    where
        D: GlobalDispatch<KoraImageCaptureSizeManagerV1, ()>
            + Dispatch<KoraImageCaptureSizeManagerV1, ()>
            + 'static,
    {
        let global = dh.create_global::<D, KoraImageCaptureSizeManagerV1, _>(1, ());
        CaptureSizeState { global }
    }

    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }
}

impl<D> GlobalDispatch<KoraImageCaptureSizeManagerV1, (), D> for CaptureSizeState
where
    D: GlobalDispatch<KoraImageCaptureSizeManagerV1, ()>
        + Dispatch<KoraImageCaptureSizeManagerV1, ()>
        + 'static,
{
    fn bind(
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<KoraImageCaptureSizeManagerV1>,
        _global_data: &(),
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }
}

impl<D> Dispatch<KoraImageCaptureSizeManagerV1, (), D> for CaptureSizeState
where
    D: GlobalDispatch<KoraImageCaptureSizeManagerV1, ()>
        + Dispatch<KoraImageCaptureSizeManagerV1, ()>
        + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        _resource: &KoraImageCaptureSizeManagerV1,
        request: kora_image_capture_size_manager_v1::Request,
        _data: &(),
        _dhandle: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            kora_image_capture_size_manager_v1::Request::Destroy => {}
            kora_image_capture_size_manager_v1::Request::SetSize {
                source,
                width,
                height,
            } => {
                let Some(source) = ImageCaptureSource::from_resource(&source) else {
                    return;
                };
                let hint = (width > 0 && height > 0).then(|| Size::from((width, height)));
                source
                    .user_data()
                    .insert_if_missing_threadsafe(CaptureSizeHint::default);
                if let Some(slot) = source.user_data().get::<CaptureSizeHint>() {
                    *slot.0.lock().unwrap() = hint;
                }
            }
        }
    }
}

macro_rules! delegate_kora_image_capture_size {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::kora_image_capture_size::kora_image_capture_size_manager_v1::KoraImageCaptureSizeManagerV1: ()
        ] => $crate::wayland::protocols::kora_image_capture_size::CaptureSizeState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::kora_image_capture_size::kora_image_capture_size_manager_v1::KoraImageCaptureSizeManagerV1: ()
        ] => $crate::wayland::protocols::kora_image_capture_size::CaptureSizeState);
    };
}
pub(crate) use delegate_kora_image_capture_size;

#[cfg(test)]
mod tests {
    use super::*;

    fn size(w: i32, h: i32) -> Size<i32, BufferCoords> {
        Size::from((w, h))
    }

    #[test]
    fn a_source_larger_than_the_bounds_is_shrunk_keeping_its_aspect() {
        assert_eq!(fit_within(size(2560, 1440), size(672, 420)), size(672, 378));
        assert_eq!(fit_within(size(1440, 2560), size(672, 420)), size(236, 420));
    }

    #[test]
    fn a_source_that_fits_keeps_its_own_size() {
        assert_eq!(fit_within(size(600, 300), size(672, 420)), size(600, 300));
        assert_eq!(fit_within(size(0, 0), size(672, 420)), size(0, 0));
    }
}
