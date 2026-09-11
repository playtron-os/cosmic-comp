// SPDX-License-Identifier: GPL-3.0-only

use std::{borrow::Borrow, collections::HashMap, sync::Mutex, time::Duration};

use calloop::{
    LoopHandle, RegistrationToken,
    timer::{TimeoutAction, Timer},
};

use smithay::{
    backend::{
        allocator::{Fourcc, Modifier},
        egl::EGLDevice,
        renderer::{
            damage::OutputDamageTracker,
            gles::{Capability, GlesRenderer},
            glow::GlowRenderer,
            utils::with_renderer_surface_state,
        },
    },
    desktop::space::SpaceElement,
    input::{Seat, pointer::PointerHandle},
    output::Output,
    reexports::wayland_server::protocol::{wl_pointer::WlPointer, wl_shm::Format as ShmFormat},
    utils::{Buffer as BufferCoords, Physical, Point, Size, Transform},
    wayland::{
        dmabuf::get_dmabuf,
        image_capture_source::ImageCaptureSource,
        image_copy_capture::{
            BufferConstraints, CaptureFailureReason, CursorSession, CursorSessionRef,
            DmabufConstraints, Frame, FrameRef, ImageCopyCaptureHandler, ImageCopyCaptureState,
            Session, SessionRef,
        },
        seat::WaylandFocus,
    },
};

use crate::{
    shell::{CosmicSurface, CursorGeometry, Shell},
    state::{BackendData, State},
    utils::prelude::{
        OutputExt, PointExt, PointGlobalExt, PointLocalExt, RectExt, RectLocalExt, SeatExt,
    },
    wayland::protocols::{
        image_capture_source::ImageCaptureSourceKind,
        kora_image_capture_size::{capture_size_hint, fit_within},
        workspace::WorkspaceHandle,
    },
};

mod render;
mod user_data;
pub use self::render::*;
use self::user_data::*;
pub use self::user_data::{FrameHolder, ImageCopySessions, SessionData, SessionHolder};

fn default_cursor_size() -> Size<i32, BufferCoords> {
    Size::new(64, 64)
}

fn seat_for_wl_pointer<'a>(shell: &'a Shell, pointer: &WlPointer) -> Option<&'a Seat<State>> {
    let pointer_handle = PointerHandle::<State>::from_resource(pointer)?;
    shell
        .seats
        .iter()
        .find(|seat| seat.get_pointer().is_some_and(|p| p == pointer_handle))
}

pub fn cursor_capture_constraints(cursor_geometry: Option<CursorGeometry>) -> BufferConstraints {
    let size = if let Some(cursor_geometry) = cursor_geometry {
        let mut size = cursor_geometry.geometry.size;
        // Client shouldn't try to allocate 0x0 buffer
        if size == Size::new(0, 0) {
            size = Size::new(1, 1);
        }
        size
    } else {
        default_cursor_size()
    };
    BufferConstraints {
        size,
        shm: vec![ShmFormat::Argb8888],
        dma: None,
    }
}

impl ImageCopyCaptureHandler for State {
    fn image_copy_capture_state(&mut self) -> &mut ImageCopyCaptureState {
        &mut self.common.image_copy_capture_state
    }

    fn capture_constraints(&mut self, source: &ImageCaptureSource) -> Option<BufferConstraints> {
        match ImageCaptureSourceKind::from_source(source) {
            ImageCaptureSourceKind::Output(weak) => weak
                .upgrade()
                .and_then(|output| constraints_for_output(&output, &mut self.backend)),
            ImageCaptureSourceKind::Workspace(handle) => {
                let hint = capture_size_hint(source);
                let shell = self.common.shell.read();
                let output = shell.space_for_handle_any_realm(&handle)?.output().clone();
                drop(shell);
                let size = workspace_capture_size(&output, hint)?;
                constraints_for_output_sized(&output, size, &mut self.backend)
            }
            ImageCaptureSourceKind::Toplevel(window) => {
                if let Some(window) = window.upgrade() {
                    constraints_for_toplevel(&window, &mut self.backend)
                } else {
                    None
                }
            }
            ImageCaptureSourceKind::Destroyed => None,
        }
    }

    fn cursor_capture_constraints(
        &mut self,
        _source: &ImageCaptureSource,
        pointer: &WlPointer,
    ) -> Option<BufferConstraints> {
        let shell = self.common.shell.read();
        let seat = seat_for_wl_pointer(&shell, pointer)?;
        let cursor_geometry = seat.cursor_geometry((0.0, 0.0), self.common.clock.now());
        Some(cursor_capture_constraints(cursor_geometry))
    }

    fn new_session(&mut self, session: Session) {
        match ImageCaptureSourceKind::from_source(&session.source()) {
            ImageCaptureSourceKind::Output(weak) => {
                let Some(mut output) = weak.upgrade() else {
                    session.stop();
                    return;
                };

                session.user_data().insert_if_missing_threadsafe(|| {
                    Mutex::new(SessionUserData::new(OutputDamageTracker::from_output(
                        &output,
                    )))
                });

                output.add_session(session);
            }
            ImageCaptureSourceKind::Workspace(handle) => {
                let hint = capture_size_hint(&session.source());
                let mut shell = self.common.shell.write();
                let Some(workspace) = shell.space_for_handle_any_realm_mut(&handle) else {
                    session.stop();
                    return;
                };
                let Some(size) = workspace_capture_size(workspace.output(), hint) else {
                    session.stop();
                    return;
                };

                session.user_data().insert_if_missing_threadsafe(|| {
                    Mutex::new(SessionUserData::new(workspace_damage_tracker(
                        workspace.output(),
                        size,
                    )))
                });
                workspace.add_session(session);
            }
            ImageCaptureSourceKind::Toplevel(toplevel) => {
                let Some(mut toplevel) = toplevel.upgrade() else {
                    session.stop();
                    return;
                };

                // A window belonging to another workspace is not capturable —
                // refused here rather than blanked later, so nothing of it ever
                // reaches a buffer. This is W-10's promise that a client demo
                // cannot leak another client's work, and it is enforcement
                // rather than advice: the requester is told no.
                //
                // Machine-plane windows (panel, dock, launcher) have no
                // workspace and stay capturable, and with no workspace registry
                // running nothing is refused at all.
                let capturable = toplevel.wl_surface().is_some_and(|surface| {
                    self.common
                        .shell
                        .read()
                        .surface_in_active_workspace(&surface)
                });
                if !capturable {
                    session.stop();
                    return;
                }

                let size = toplevel.geometry().size.to_physical(1);
                session.user_data().insert_if_missing_threadsafe(|| {
                    Mutex::new(SessionUserData::new(OutputDamageTracker::new(
                        size,
                        1.0,
                        Transform::Normal,
                    )))
                });
                toplevel.add_session(session);
            }
            ImageCaptureSourceKind::Destroyed => {
                session.stop();
            }
        }
    }

    fn new_cursor_session(&mut self, session: CursorSession) {
        let (pointer_loc, pointer_size, hotspot) = {
            let shell = self.common.shell.read();
            if let Some(seat) = seat_for_wl_pointer(&shell, &session.pointer()) {
                let pointer = seat.get_pointer().unwrap();
                let pointer_loc = pointer.current_location().to_i32_round().as_global();

                let (pointer_size, hotspot) = if let Some(CursorGeometry { geometry, hotspot }) =
                    seat.cursor_geometry((0.0, 0.0), self.common.clock.now())
                {
                    (geometry.size, hotspot)
                } else {
                    (default_cursor_size(), Point::from((0, 0)))
                };

                (pointer_loc, pointer_size, hotspot)
            } else {
                (Point::new(0, 0), default_cursor_size(), Point::from((0, 0)))
            }
        };

        session.user_data().insert_if_missing_threadsafe(|| {
            Mutex::new(SessionUserData::new(OutputDamageTracker::new(
                pointer_size.to_logical(1, Transform::Normal).to_physical(1),
                1.0,
                Transform::Normal,
            )))
        });

        match ImageCaptureSourceKind::from_source(&session.source()) {
            ImageCaptureSourceKind::Output(weak) => {
                let Some(mut output) = weak.upgrade() else {
                    return;
                };

                if output.geometry().contains(pointer_loc) {
                    let buffer_pos = pointer_loc
                        .to_local(&output)
                        .as_logical()
                        .to_f64()
                        .to_buffer(
                            output.current_scale().fractional_scale(),
                            output.current_transform(),
                            &output
                                .current_mode()
                                .map(|mode| {
                                    mode.size
                                        .to_f64()
                                        .to_logical(output.current_scale().fractional_scale())
                                })
                                .unwrap_or(Size::from((0.0, 0.0))),
                        )
                        .to_i32_round();
                    session.set_cursor_hotspot(hotspot);
                    session.set_cursor_pos(Some(buffer_pos));
                }

                output.add_cursor_session(session);
            }
            ImageCaptureSourceKind::Workspace(handle) => {
                let mut shell = self.common.shell.write();
                let Some(workspace) = shell.space_for_handle_any_realm_mut(&handle) else {
                    return;
                };

                let output = workspace.output().clone();
                if output.geometry().contains(pointer_loc) {
                    let buffer_pos = pointer_loc
                        .to_local(&output)
                        .as_logical()
                        .to_f64()
                        .to_buffer(
                            output.current_scale().fractional_scale(),
                            output.current_transform(),
                            &output
                                .current_mode()
                                .map(|mode| {
                                    mode.size
                                        .to_f64()
                                        .to_logical(output.current_scale().fractional_scale())
                                })
                                .unwrap_or(Size::from((0.0, 0.0))),
                        )
                        .to_i32_round();
                    session.set_cursor_hotspot(hotspot);
                    session.set_cursor_pos(Some(buffer_pos));
                }

                workspace.add_cursor_session(session);
            }
            ImageCaptureSourceKind::Toplevel(toplevel) => {
                let Some(mut toplevel) = toplevel.upgrade() else {
                    return;
                };

                let shell = self.common.shell.read();
                if let Some(element) = shell.element_for_surface(&toplevel)
                    && element.has_active_window(&toplevel)
                    && let Some(workspace) = shell.space_for(element)
                    && let Some(geometry) = workspace.element_geometry(element)
                {
                    let mut surface_geo = element.active_window_geometry().as_local();
                    surface_geo.loc += geometry.loc;
                    let global_geo = surface_geo.to_global(workspace.output());
                    if global_geo.contains(pointer_loc) {
                        let buffer_pos = (pointer_loc - global_geo.loc).as_logical().to_buffer(
                            1,
                            Transform::Normal,
                            &toplevel.geometry().size,
                        );
                        session.set_cursor_hotspot(hotspot);
                        session.set_cursor_pos(Some(buffer_pos));
                    }
                }

                toplevel.add_cursor_session(session);
            }
            ImageCaptureSourceKind::Destroyed => {
                session.stop();
            }
        }
    }

    fn frame(&mut self, session: &SessionRef, frame: Frame) {
        // A frame that is neither succeeded nor failed leaves the client's
        // buffer permanently in flight, so a vanished source must still answer.
        match ImageCaptureSourceKind::from_source(&session.source()) {
            ImageCaptureSourceKind::Output(weak) => {
                let Some(mut output) = weak.upgrade() else {
                    frame.fail(CaptureFailureReason::Stopped);
                    return;
                };

                output.add_frame(session.clone(), frame);
                self.backend.schedule_render(&output);
            }
            ImageCaptureSourceKind::Workspace(handle) => {
                render_workspace_to_buffer(self, session, frame, handle)
            }
            ImageCaptureSourceKind::Toplevel(toplevel) => {
                let Some(toplevel) = toplevel.upgrade() else {
                    frame.fail(CaptureFailureReason::Stopped);
                    return;
                };

                render_window_to_buffer(self, session, frame, &toplevel)
            }
            ImageCaptureSourceKind::Destroyed => {
                frame.fail(CaptureFailureReason::Stopped);
            }
        }
    }

    fn cursor_frame(&mut self, session: &CursorSessionRef, frame: Frame) {
        if !session.has_cursor() {
            frame.success(Transform::Normal, Vec::new(), self.common.clock.now());
            return;
        }

        let shell = self.common.shell.read();
        if let Some(seat) = seat_for_wl_pointer(&shell, &session.pointer()).cloned() {
            drop(shell);
            render_cursor_to_buffer(self, session, frame, &seat);
        }
    }

    fn frame_aborted(&mut self, frame: FrameRef) {
        self.common.parked_workspace_captures.remove_frame(&frame);
        let shell = self.common.shell.read();
        for mut output in shell.outputs().cloned() {
            output.remove_frame(&frame);
        }
    }

    fn session_destroyed(&mut self, session: SessionRef) {
        match ImageCaptureSourceKind::from_source(&session.source()) {
            ImageCaptureSourceKind::Output(weak) => {
                if let Some(mut output) = weak.upgrade() {
                    output.remove_session(&session);
                }
            }
            ImageCaptureSourceKind::Workspace(handle) => {
                self.common
                    .parked_workspace_captures
                    .remove_session(&session);
                if let Some(workspace) = self
                    .common
                    .shell
                    .write()
                    .space_for_handle_any_realm_mut(&handle)
                {
                    workspace.remove_session(&session)
                }
            }
            ImageCaptureSourceKind::Toplevel(toplevel) => {
                if let Some(mut toplevel) = toplevel.upgrade() {
                    toplevel.remove_session(&session);
                }
            }
            ImageCaptureSourceKind::Destroyed => {}
        }
    }

    fn cursor_session_destroyed(&mut self, session: CursorSessionRef) {
        match ImageCaptureSourceKind::from_source(&session.source()) {
            ImageCaptureSourceKind::Output(weak) => {
                if let Some(mut output) = weak.upgrade() {
                    output.remove_cursor_session(&session);
                }
            }
            ImageCaptureSourceKind::Workspace(handle) => {
                if let Some(workspace) = self
                    .common
                    .shell
                    .write()
                    .space_for_handle_any_realm_mut(&handle)
                {
                    workspace.remove_cursor_session(&session)
                }
            }
            ImageCaptureSourceKind::Toplevel(toplevel) => {
                if let Some(mut toplevel) = toplevel.upgrade() {
                    toplevel.remove_cursor_session(&session)
                }
            }
            ImageCaptureSourceKind::Destroyed => {}
        }
    }
}

fn constraints_for_output(output: &Output, backend: &mut BackendData) -> Option<BufferConstraints> {
    let size = workspace_capture_size(output, None)?;
    constraints_for_output_sized(output, size, backend)
}

/// Constraints for a capture of `output` into a buffer of `size`.
pub fn constraints_for_output_sized(
    output: &Output,
    size: Size<i32, BufferCoords>,
    backend: &mut BackendData,
) -> Option<BufferConstraints> {
    let mut renderer = backend
        .offscreen_renderer(|kms| {
            kms.target_node_for_output(output)
                .or(*kms.primary_node.read().unwrap())
        })
        .ok()?;
    Some(constraints_for_renderer(size, renderer.as_mut()))
}

/// A workspace capture's buffer size: the output's mode, or a preview's size
/// fitted to it when the client asked for one.
pub fn workspace_capture_size(
    output: &Output,
    hint: Option<Size<i32, BufferCoords>>,
) -> Option<Size<i32, BufferCoords>> {
    let mode = output
        .current_mode()?
        .size
        .to_logical(1)
        .to_buffer(1, Transform::Normal);
    Some(hint.map_or(mode, |hint| fit_within(mode, hint)))
}

/// The damage tracker of a workspace capture into a buffer of `size`: the
/// output's own at full size, else one of the preview's size that still
/// measures elements at the output's scale (they are shrunk when drawn).
pub fn workspace_damage_tracker(
    output: &Output,
    size: Size<i32, BufferCoords>,
) -> OutputDamageTracker {
    if workspace_capture_size(output, None) == Some(size) {
        OutputDamageTracker::from_output(output)
    } else {
        OutputDamageTracker::new(
            Size::<i32, Physical>::from((size.w, size.h)),
            output.current_scale().fractional_scale(),
            output.current_transform(),
        )
    }
}

/// At most this often is a held capture looked at again: previews are live
/// at up to thirty frames a second.
const LIVE_CAPTURE_TICK: Duration = Duration::from_millis(33);

/// Workspace captures waiting for their desktop to change. A capture of an
/// unchanged desktop is held rather than answered with the same picture, and
/// a tick looks in on the held ones while there are any; nothing is drawn or
/// copied for a desktop that stays as it was.
#[derive(Debug, Default)]
pub struct ParkedWorkspaceCaptures {
    frames: Vec<ParkedCapture>,
    tick: Option<RegistrationToken>,
}

#[derive(Debug)]
struct ParkedCapture {
    session: SessionRef,
    frame: Frame,
    handle: WorkspaceHandle,
}

impl ParkedWorkspaceCaptures {
    pub fn park(
        &mut self,
        session: SessionRef,
        frame: Frame,
        handle: WorkspaceHandle,
        loop_handle: &LoopHandle<'static, State>,
    ) {
        self.frames.push(ParkedCapture {
            session,
            frame,
            handle,
        });
        if self.tick.is_none() {
            self.tick = loop_handle
                .insert_source(Timer::from_duration(LIVE_CAPTURE_TICK), |_, _, state| {
                    retry_parked_workspace_captures(state);
                    let parked = &mut state.common.parked_workspace_captures;
                    if parked.frames.is_empty() {
                        parked.tick = None;
                        TimeoutAction::Drop
                    } else {
                        TimeoutAction::ToDuration(LIVE_CAPTURE_TICK)
                    }
                })
                .ok();
        }
    }

    pub fn remove_frame(&mut self, frame: &FrameRef) {
        self.frames.retain(|parked| parked.frame != *frame);
    }

    pub fn remove_session(&mut self, session: &SessionRef) {
        self.frames.retain(|parked| parked.session != *session);
    }
}

/// Draw every held capture whose desktop changed; the rest are held again.
fn retry_parked_workspace_captures(state: &mut State) {
    let parked = std::mem::take(&mut state.common.parked_workspace_captures.frames);
    for ParkedCapture {
        session,
        frame,
        handle,
    } in parked
    {
        render_workspace_to_buffer(state, &session, frame, handle);
    }
}

/// Stop every capture session bound to a toplevel that is going away, so that
/// clients are told rather than left waiting on a window that will never render.
pub fn stop_sessions_for_toplevel(surface: &mut CosmicSurface) {
    for session in surface.sessions() {
        surface.remove_session(&session);
    }
    for session in surface.cursor_sessions() {
        surface.remove_cursor_session(&session);
    }
}

fn constraints_for_toplevel(
    surface: &CosmicSurface,
    backend: &mut BackendData,
) -> Option<BufferConstraints> {
    let size = surface.geometry().size.to_buffer(1, Transform::Normal);
    let wl_surface = surface.wl_surface()?;

    let mut renderer = backend
        .offscreen_renderer(|kms| {
            let dma_node = with_renderer_surface_state(&wl_surface, |state| {
                let buffer = state.buffer()?;
                let dmabuf = get_dmabuf(buffer).ok()?;
                dmabuf.node()
            })
            .flatten();

            dma_node.or(*kms.primary_node.read().unwrap())
        })
        .unwrap();

    Some(constraints_for_renderer(size, renderer.as_mut()))
}

fn constraints_for_renderer(
    size: Size<i32, BufferCoords>,
    renderer: &mut GlowRenderer,
) -> BufferConstraints {
    let mut constraints = BufferConstraints {
        size,
        shm: vec![ShmFormat::Abgr8888, ShmFormat::Xbgr8888],
        dma: None,
    };

    if (renderer as &dyn Borrow<GlesRenderer>)
        .borrow()
        .capabilities()
        .contains(&Capability::_10Bit)
    {
        constraints
            .shm
            .extend([ShmFormat::Abgr2101010, ShmFormat::Xbgr2101010]);
    }

    if let Some(node) = EGLDevice::device_for_display(renderer.egl_context().display())
        .ok()
        .and_then(|device| device.try_get_render_node().ok().flatten())
    {
        constraints.dma = Some(DmabufConstraints {
            node,
            formats: renderer
                .egl_context()
                .dmabuf_render_formats()
                .iter()
                .fold(
                    HashMap::<Fourcc, Vec<Modifier>>::new(),
                    |mut map, format| {
                        map.entry(format.code).or_default().push(format.modifier);
                        map
                    },
                )
                .into_iter()
                .collect::<Vec<_>>(),
        });
    }

    constraints
}
