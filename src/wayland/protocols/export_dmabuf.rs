// SPDX-License-Identifier: GPL-3.0-only

//! Implementation of the wlr-export-dmabuf-unstable-v1 protocol.
//!
//! This protocol allows clients to capture screen content as DMA-BUFs,
//! which is essential for remote desktop functionality (e.g., sunshine streaming).

use std::sync::{Arc, Mutex};

use smithay::{
    backend::allocator::{Fourcc, Modifier},
    output::Output,
    reexports::wayland_server::{
        protocol::wl_output::WlOutput, Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch,
        New, Resource, Weak,
    },
    utils::IsAlive,
};
use tracing::debug;
use wayland_backend::server::GlobalId;
use wayland_protocols_wlr::export_dmabuf::v1::server::{
    zwlr_export_dmabuf_frame_v1::{self, CancelReason, Flags, ZwlrExportDmabufFrameV1},
    zwlr_export_dmabuf_manager_v1::{self, ZwlrExportDmabufManagerV1},
};

/// Pending export-dmabuf frames stored on the Output's user data.
/// This allows the render thread to access them.
type PendingExportDmabufFrames = Mutex<Vec<PendingFrame>>;

/// State for the wlr-export-dmabuf protocol.
#[derive(Debug)]
pub struct ExportDmabufState {
    global: GlobalId,
}

/// Trait for types that can hold export-dmabuf frames (Output).
pub trait ExportDmabufFrameHolder {
    /// Queue a frame capture for this output.
    fn add_export_dmabuf_frame(&self, frame: PendingFrame);
    /// Take all pending frames for this output.
    fn take_export_dmabuf_frames(&self) -> Vec<FrameRef>;
}

impl ExportDmabufFrameHolder for Output {
    fn add_export_dmabuf_frame(&self, frame: PendingFrame) {
        self.user_data().insert_if_missing_threadsafe(|| {
            Mutex::new(Vec::<PendingFrame>::new())
        });
        if let Some(pending) = self.user_data().get::<PendingExportDmabufFrames>() {
            pending.lock().unwrap().push(frame);
        }
    }

    fn take_export_dmabuf_frames(&self) -> Vec<FrameRef> {
        self.user_data()
            .get::<PendingExportDmabufFrames>()
            .map(|pending| {
                let mut frames = pending.lock().unwrap();
                let refs = frames
                    .iter()
                    .filter(|f| f.alive())
                    .map(|f| FrameRef {
                        obj: f.obj.clone(),
                        output: f.output.clone(),
                        overlay_cursor: f.overlay_cursor,
                        inner: f.inner.clone(),
                    })
                    .collect();
                frames.clear();
                refs
            })
            .unwrap_or_default()
    }
}

/// Global data for the export-dmabuf manager.
pub struct ExportDmabufGlobalData {
    pub filter: Box<dyn for<'a> Fn(&'a Client) -> bool + Send + Sync>,
}

impl std::fmt::Debug for ExportDmabufGlobalData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ExportDmabufGlobalData")
            .field("filter", &"...")
            .finish()
    }
}

/// Data associated with an export-dmabuf manager instance.
#[derive(Debug, Clone, Default)]
pub struct ExportDmabufData;

/// A pending frame capture request.
#[derive(Debug, Clone)]
pub struct PendingFrame {
    obj: ZwlrExportDmabufFrameV1,
    output: Weak<WlOutput>,
    overlay_cursor: bool,
    inner: Arc<Mutex<FrameInner>>,
}

impl PartialEq for PendingFrame {
    fn eq(&self, other: &Self) -> bool {
        self.obj == other.obj
    }
}

impl IsAlive for PendingFrame {
    fn alive(&self) -> bool {
        self.obj.is_alive()
    }
}

/// Internal state for a frame.
#[derive(Debug)]
struct FrameInner {
    cancelled: bool,
}

/// Data associated with an export-dmabuf frame.
#[derive(Debug)]
pub struct FrameData {
    #[allow(dead_code)]
    output: Weak<WlOutput>,
    #[allow(dead_code)]
    overlay_cursor: bool,
    inner: Arc<Mutex<FrameInner>>,
}

/// A reference to a pending frame that can be used by the compositor.
#[derive(Debug, Clone)]
pub struct FrameRef {
    obj: ZwlrExportDmabufFrameV1,
    output: Weak<WlOutput>,
    overlay_cursor: bool,
    inner: Arc<Mutex<FrameInner>>,
}

impl FrameRef {
    /// Returns the output this frame is capturing.
    pub fn output(&self) -> Option<WlOutput> {
        self.output.upgrade().ok()
    }

    /// Returns whether the cursor should be included in the capture.
    pub fn overlay_cursor(&self) -> bool {
        self.overlay_cursor
    }

    /// Send the frame event with buffer metadata.
    /// 
    /// Note: The `Flags::Transient` flag is always sent, indicating that clients
    /// should copy the frame before processing.
    pub fn send_frame(
        &self,
        width: u32,
        height: u32,
        offset_x: u32,
        offset_y: u32,
        buffer_flags: u32,
        format: Fourcc,
        modifier: Modifier,
        num_objects: u32,
    ) {
        if !self.obj.is_alive() {
            return;
        }

        let modifier_val: u64 = modifier.into();
        // Use Transient flag to indicate clients should copy frame before processing
        // This is the safest default for screen capture
        self.obj.frame(
            width,
            height,
            offset_x,
            offset_y,
            buffer_flags,
            Flags::Transient,
            format as u32,
            (modifier_val >> 32) as u32,
            modifier_val as u32,
            num_objects,
        );
    }

    /// Send an object (plane) event with FD and metadata.
    pub fn send_object(
        &self,
        index: u32,
        fd: std::os::unix::io::OwnedFd,
        size: u32,
        offset: u32,
        stride: u32,
        plane_index: u32,
    ) {
        use std::os::unix::io::AsFd;
        if !self.obj.is_alive() {
            return;
        }

        self.obj
            .object(index, fd.as_fd(), size, offset, stride, plane_index);
    }

    /// Send the ready event indicating the frame is available for reading.
    pub fn send_ready(&self, tv_sec: u64, tv_nsec: u32) {
        if !self.obj.is_alive() {
            return;
        }

        self.obj
            .ready((tv_sec >> 32) as u32, tv_sec as u32, tv_nsec);
    }

    /// Cancel the frame capture.
    pub fn cancel(&self, reason: CancelReason) {
        let mut inner = self.inner.lock().unwrap();
        if inner.cancelled {
            return;
        }
        inner.cancelled = true;

        if self.obj.is_alive() {
            self.obj.cancel(reason);
        }
    }
}

impl ExportDmabufState {
    /// Creates a new export-dmabuf state and registers the global.
    pub fn new<D, F>(display: &DisplayHandle, client_filter: F) -> ExportDmabufState
    where
        D: GlobalDispatch<ZwlrExportDmabufManagerV1, ExportDmabufGlobalData>
            + Dispatch<ZwlrExportDmabufManagerV1, ExportDmabufData>
            + Dispatch<ZwlrExportDmabufFrameV1, FrameData>
            + ExportDmabufHandler
            + 'static,
        F: for<'a> Fn(&'a Client) -> bool + Send + Sync + 'static,
    {
        ExportDmabufState {
            global: display.create_global::<D, ZwlrExportDmabufManagerV1, _>(
                1,
                ExportDmabufGlobalData {
                    filter: Box::new(client_filter),
                },
            ),
        }
    }

    /// Returns the global ID.
    pub fn global_id(&self) -> &GlobalId {
        &self.global
    }
}

/// Handler trait for the export-dmabuf protocol.
pub trait ExportDmabufHandler {
    /// Returns a mutable reference to the export-dmabuf state.
    fn export_dmabuf_state(&mut self) -> &mut ExportDmabufState;

    /// Get the Output for a WlOutput resource.
    fn output_from_wl_output(&self, wl_output: &WlOutput) -> Option<Output>;
}

impl<D> GlobalDispatch<ZwlrExportDmabufManagerV1, ExportDmabufGlobalData, D> for ExportDmabufState
where
    D: GlobalDispatch<ZwlrExportDmabufManagerV1, ExportDmabufGlobalData>
        + Dispatch<ZwlrExportDmabufManagerV1, ExportDmabufData>
        + Dispatch<ZwlrExportDmabufFrameV1, FrameData>
        + ExportDmabufHandler
        + 'static,
{
    fn bind(
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<ZwlrExportDmabufManagerV1>,
        _global_data: &ExportDmabufGlobalData,
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ExportDmabufData);
    }

    fn can_view(client: Client, global_data: &ExportDmabufGlobalData) -> bool {
        (global_data.filter)(&client)
    }
}

impl<D> Dispatch<ZwlrExportDmabufManagerV1, ExportDmabufData, D> for ExportDmabufState
where
    D: GlobalDispatch<ZwlrExportDmabufManagerV1, ExportDmabufGlobalData>
        + Dispatch<ZwlrExportDmabufManagerV1, ExportDmabufData>
        + Dispatch<ZwlrExportDmabufFrameV1, FrameData>
        + ExportDmabufHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        _resource: &ZwlrExportDmabufManagerV1,
        request: zwlr_export_dmabuf_manager_v1::Request,
        _data: &ExportDmabufData,
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zwlr_export_dmabuf_manager_v1::Request::CaptureOutput {
                frame,
                overlay_cursor,
                output: wl_output,
            } => {
                let inner = Arc::new(Mutex::new(FrameInner { cancelled: false }));

                let frame_obj = data_init.init(
                    frame,
                    FrameData {
                        output: wl_output.downgrade(),
                        overlay_cursor: overlay_cursor != 0,
                        inner: inner.clone(),
                    },
                );

                let pending = PendingFrame {
                    obj: frame_obj.clone(),
                    output: wl_output.downgrade(),
                    overlay_cursor: overlay_cursor != 0,
                    inner: inner.clone(),
                };

                // Add the frame to the output's user data so the render thread can access it
                if let Some(output) = state.output_from_wl_output(&wl_output) {
                    output.add_export_dmabuf_frame(pending);
                    debug!("Export-dmabuf: capture requested for output {:?}", output.name());
                } else {
                    // Output not found, cancel the frame
                    frame_obj.cancel(CancelReason::Permanent);
                    debug!("Export-dmabuf: capture requested for unknown output");
                }
            }
            zwlr_export_dmabuf_manager_v1::Request::Destroy => {}
            _ => {}
        }
    }
}

impl<D> Dispatch<ZwlrExportDmabufFrameV1, FrameData, D> for ExportDmabufState
where
    D: GlobalDispatch<ZwlrExportDmabufManagerV1, ExportDmabufGlobalData>
        + Dispatch<ZwlrExportDmabufManagerV1, ExportDmabufData>
        + Dispatch<ZwlrExportDmabufFrameV1, FrameData>
        + ExportDmabufHandler
        + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        _resource: &ZwlrExportDmabufFrameV1,
        request: zwlr_export_dmabuf_frame_v1::Request,
        data: &FrameData,
        _dhandle: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zwlr_export_dmabuf_frame_v1::Request::Destroy => {
                // Mark as cancelled if not already done
                let mut inner = data.inner.lock().unwrap();
                inner.cancelled = true;
            }
            _ => {}
        }
    }
}

/// Delegate macro for the export-dmabuf protocol.
#[macro_export]
macro_rules! delegate_export_dmabuf {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            wayland_protocols_wlr::export_dmabuf::v1::server::zwlr_export_dmabuf_manager_v1::ZwlrExportDmabufManagerV1: $crate::wayland::protocols::export_dmabuf::ExportDmabufGlobalData
        ] => $crate::wayland::protocols::export_dmabuf::ExportDmabufState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            wayland_protocols_wlr::export_dmabuf::v1::server::zwlr_export_dmabuf_manager_v1::ZwlrExportDmabufManagerV1: $crate::wayland::protocols::export_dmabuf::ExportDmabufData
        ] => $crate::wayland::protocols::export_dmabuf::ExportDmabufState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            wayland_protocols_wlr::export_dmabuf::v1::server::zwlr_export_dmabuf_frame_v1::ZwlrExportDmabufFrameV1: $crate::wayland::protocols::export_dmabuf::FrameData
        ] => $crate::wayland::protocols::export_dmabuf::ExportDmabufState);
    };
}
pub(crate) use delegate_export_dmabuf;
