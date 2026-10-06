// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    backend::render,
    config::ScreenFilter,
    shell::{Devices, SeatExt},
    state::{BackendData, Common},
    utils::prelude::*,
    wayland::protocols::drm::WlDrmState,
};
use anyhow::{Context, Result, anyhow};
use cosmic_comp_config::output::comp::{OutputConfig, TransformDef};
use smithay::{
    backend::{
        allocator::Fourcc,
        drm::NodeType,
        egl::EGLDevice,
        renderer::{
            Bind, Blit, ImportDma, Offscreen, Texture, TextureFilter,
            damage::{OutputDamageTracker, RenderOutputResult},
            gles::GlesTexture,
            glow::GlowRenderer,
        },
        winit::{self, WinitEvent, WinitGraphicsBackend, WinitVirtualDevice},
    },
    desktop::layer_map_for_output,
    output::{Mode, Output, PhysicalProperties, Scale, Subpixel},
    reexports::{
        calloop::{EventLoop, ping},
        wayland_protocols::wp::presentation_time::server::wp_presentation_feedback,
        wayland_server::DisplayHandle,
        winit::event_loop::pump_events::PumpStatus,
    },
    utils::{Physical, Rectangle, Size, Transform},
    wayland::{dmabuf::DmabufFeedbackBuilder, presentation::Refresh},
};
use std::{borrow::BorrowMut, cell::RefCell, time::Duration};
use tracing::{error, info, warn};

use super::render::{CursorMode, ScreenFilterStorage, init_shaders};

#[derive(Debug)]
pub struct WinitState {
    pub backend: WinitGraphicsBackend<GlowRenderer>,
    output: Output,
    damage_tracker: OutputDamageTracker,
    screen_filter_state: ScreenFilterStorage,
    /// With `COSMIC_WINIT_OUTPUTS` above 1, every output side by side in the
    /// window, each drawn offscreen and copied into its slice.
    split: Vec<SplitOutput>,
}

#[derive(Debug)]
struct SplitOutput {
    output: Output,
    damage_tracker: OutputDamageTracker,
    screen_filter_state: ScreenFilterStorage,
    texture: Option<GlesTexture>,
}

/// How many outputs `COSMIC_WINIT_OUTPUTS` asks the window to hold, 1 to 4.
fn split_count(value: Option<&str>) -> usize {
    value
        .and_then(|value| value.trim().parse::<usize>().ok())
        .unwrap_or(1)
        .clamp(1, 4)
}

/// Output `index` of `count` across a window of `size`: its mode and position.
fn split_slice(size: Size<i32, Physical>, count: usize, index: usize) -> ((i32, i32), (i32, i32)) {
    let width = size.w / count as i32;
    ((width, size.h), (width * index as i32, 0))
}

fn new_output(index: usize, mode: (i32, i32), position: (i32, i32)) -> Output {
    let name = format!("WINIT-{index}");
    let props = PhysicalProperties {
        size: (0, 0).into(),
        subpixel: Subpixel::Unknown,
        make: "COSMIC".to_string(),
        model: name.clone(),
        serial_number: "Unknown".to_string(),
    };
    let mode = Mode {
        size: mode.into(),
        refresh: 60_000,
    };
    let output = Output::new(name, props);
    output.add_mode(mode);
    output.set_preferred(mode);
    output.change_current_state(
        Some(mode),
        Some(Transform::Flipped180),
        Some(Scale::Integer(1)),
        Some(position.into()),
    );
    output.user_data().insert_if_missing(|| {
        RefCell::new(OutputConfig {
            mode: (mode.size.into(), None),
            transform: TransformDef::Flipped180,
            position: (position.0 as u32, position.1 as u32),
            ..Default::default()
        })
    });
    output
}

/// The frame callbacks and feedback an output gets once it has been drawn.
fn presented(state: &mut Common, output: &Output, result: &RenderOutputResult<'_>) {
    let states = &result.states;
    state.send_frames(output, None);
    state.update_primary_output(output, states);
    state.send_dmabuf_feedback(output, states, |_| None);
    if result.damage.is_some() {
        let mut feedback = state
            .shell
            .read()
            .take_presentation_feedback(output, states);
        feedback.presented(
            state.clock.now(),
            output
                .current_mode()
                .map(|mode| Refresh::Fixed(Duration::from_secs_f64(1_000.0 / mode.refresh as f64)))
                .unwrap_or(Refresh::Unknown),
            0,
            wp_presentation_feedback::Kind::Vsync,
        );
    }
}

impl WinitState {
    #[profiling::function]
    pub fn render_output(&mut self, state: &mut Common) -> Result<()> {
        if !self.split.is_empty() {
            return self.render_split(state);
        }
        let age = self.backend.buffer_age().unwrap_or(0);
        let (renderer, mut fb) = self
            .backend
            .bind()
            .with_context(|| "Failed to bind buffer")?;
        let result = render::render_output(
            None,
            renderer,
            &mut fb,
            &mut self.damage_tracker,
            age,
            &state.shell,
            state.clock.now(),
            &self.output,
            CursorMode::NotDefault,
            &mut self.screen_filter_state,
            &state.event_loop_handle,
        )
        .map_err(|err| anyhow!("Rendering failed: {}", err))?;
        std::mem::drop(fb);
        self.backend
            .submit(result.damage.map(|x| x.as_slice()))
            .with_context(|| "Failed to submit buffer for display")?;
        presented(state, &self.output, &result);
        Ok(())
    }

    /// Draw each output into its own texture, then copy them into the window.
    fn render_split(&mut self, state: &mut Common) -> Result<()> {
        let (renderer, mut fb) = self
            .backend
            .bind()
            .with_context(|| "Failed to bind buffer")?;
        for split in &mut self.split {
            let mode = split
                .output
                .current_mode()
                .context("output without a mode")?;
            let size = Size::<i32, smithay::utils::Buffer>::from((mode.size.w, mode.size.h));
            // A texture kept from the last frame still holds it, so only damage is redrawn.
            let mut age = 1;
            if split.texture.as_ref().is_none_or(|t| t.size() != size) {
                age = 0;
                split.texture = Some(
                    Offscreen::<GlesTexture>::create_buffer(renderer, Fourcc::Abgr8888, size)
                        .map_err(|err| anyhow!("No offscreen buffer: {err}"))?,
                );
            }
            let texture = split.texture.as_mut().unwrap();
            let mut target = renderer
                .bind(texture)
                .map_err(|err| anyhow!("Failed to bind offscreen buffer: {err}"))?;
            let result = render::render_output(
                None,
                renderer,
                &mut target,
                &mut split.damage_tracker,
                age,
                &state.shell,
                state.clock.now(),
                &split.output,
                CursorMode::NotDefault,
                &mut split.screen_filter_state,
                &state.event_loop_handle,
            )
            .map_err(|err| anyhow!("Rendering failed: {}", err))?;
            let at = split.output.current_location();
            let copied = renderer
                .blit(
                    &target,
                    &mut fb,
                    Rectangle::from_size(mode.size),
                    Rectangle::new((at.x, at.y).into(), mode.size),
                    TextureFilter::Nearest,
                )
                .map_err(|err| anyhow!("Failed to copy an output into the window: {err}"))?;
            let _ = copied.wait();
            presented(state, &split.output, &result);
        }
        std::mem::drop(fb);
        self.backend
            .submit(None)
            .with_context(|| "Failed to submit buffer for display")?;
        Ok(())
    }

    pub fn all_outputs(&self) -> Vec<Output> {
        if self.split.is_empty() {
            vec![self.output.clone()]
        } else {
            self.split
                .iter()
                .map(|split| split.output.clone())
                .collect()
        }
    }

    /// The whole window in global coordinates while it holds several outputs,
    /// which is where absolute pointer positions are relative to.
    pub fn split_area(&self) -> Option<Rectangle<i32, smithay::utils::Logical>> {
        (!self.split.is_empty()).then(|| {
            let size = self.backend.window_size();
            Rectangle::from_size((size.w, size.h).into())
        })
    }

    pub fn apply_config_for_outputs(&mut self, test_only: bool) -> Result<(), anyhow::Error> {
        // TODO: if we ever have multiple winit outputs, don't ignore config.enabled
        // reset size
        let size = self.backend.window_size();
        let count = self.split.len().max(1);
        let mut fits = true;
        for (index, output) in self.all_outputs().iter().enumerate() {
            let (mode, _) = split_slice(size, count, index);
            let mut config = output
                .user_data()
                .get::<RefCell<OutputConfig>>()
                .unwrap()
                .borrow_mut();
            if config.mode.0 != mode {
                if !test_only {
                    config.mode = (mode, None);
                }
                fits = false;
            }
        }
        if fits {
            Ok(())
        } else {
            Err(anyhow::anyhow!("Cannot set window size"))
        }
    }

    pub fn update_screen_filter(&mut self, screen_filter: &ScreenFilter) -> Result<()> {
        self.screen_filter_state.filter = screen_filter.clone();
        Ok(())
    }
}

pub fn init_backend(
    dh: &DisplayHandle,
    event_loop: &mut EventLoop<State>,
    state: &mut State,
) -> Result<()> {
    let (mut backend, mut input): (WinitGraphicsBackend<GlowRenderer>, _) =
        winit::init().map_err(|e| anyhow!("Failed to initilize winit backend: {e:?}"))?;
    init_shaders(backend.renderer().borrow_mut()).context("Failed to initialize renderer")?;

    init_egl_client_side(dh, state, &mut backend)?;

    let size = backend.window_size();
    let count = split_count(std::env::var("COSMIC_WINIT_OUTPUTS").ok().as_deref());
    let outputs: Vec<Output> = (0..count)
        .map(|index| {
            let (mode, position) = split_slice(size, count, index);
            new_output(index, mode, position)
        })
        .collect();
    let output = outputs[0].clone();

    let (event_ping, event_source) =
        ping::make_ping().with_context(|| "Failed to init eventloop timer for winit")?;
    let (render_ping, render_source) =
        ping::make_ping().with_context(|| "Failed to init eventloop timer for winit")?;
    let event_ping_handle = event_ping.clone();
    let render_ping_handle = render_ping.clone();
    let mut token = Some(
        event_loop
            .handle()
            .insert_source(render_source, move |_, _, state| {
                if let Err(err) = state.backend.winit().render_output(&mut state.common) {
                    error!(?err, "Failed to render frame.");
                    render_ping.ping();
                }
                profiling::finish_frame!();
            })
            .map_err(|_| anyhow::anyhow!("Failed to init eventloop timer for winit"))?,
    );
    let event_loop_handle = event_loop.handle();
    event_loop
        .handle()
        .insert_source(event_source, move |_, _, state| {
            match input
                .dispatch_new_events(|event| state.process_winit_event(event, &render_ping_handle))
            {
                PumpStatus::Continue => {
                    event_ping_handle.ping();
                    render_ping_handle.ping();
                }
                PumpStatus::Exit(_) => {
                    for output in state.backend.winit().all_outputs() {
                        state.common.remove_output(&output);
                    }
                    if let Some(token) = token.take() {
                        event_loop_handle.remove(token);
                    }
                }
            };
        })
        .map_err(|_| anyhow::anyhow!("Failed to init eventloop timer for winit"))?;
    event_ping.ping();

    let split = if count > 1 {
        outputs
            .iter()
            .map(|output| SplitOutput {
                output: output.clone(),
                damage_tracker: OutputDamageTracker::from_output(output),
                screen_filter_state: ScreenFilterStorage::default(),
                texture: None,
            })
            .collect()
    } else {
        Vec::new()
    };
    state.backend = BackendData::Winit(WinitState {
        backend,
        output: output.clone(),
        damage_tracker: OutputDamageTracker::from_output(&output),
        screen_filter_state: ScreenFilterStorage::default(),
        split,
    });

    state
        .common
        .output_configuration_state
        .add_heads(outputs.iter());
    {
        for output in &outputs {
            state.common.add_output(output);
        }
        if let Err(err) = state.common.config.read_outputs(
            &mut state.common.output_configuration_state,
            &mut state.backend,
            &state.common.shell,
            &state.common.event_loop_handle,
            &mut state.common.workspace_state.update(),
            &state.common.xdg_activation_state,
            state.common.startup_done.clone(),
            &state.common.clock,
        ) {
            error!("Unrecoverable output config error: {}", err);
        }
        state.common.refresh();
    }

    if state.common.with_xwayland {
        state.launch_xwayland(None);
    } else {
        state.notify_ready();
    }

    Ok(())
}

fn init_egl_client_side(
    dh: &DisplayHandle,
    state: &mut State,
    renderer: &mut WinitGraphicsBackend<GlowRenderer>,
) -> Result<()> {
    let render_node = EGLDevice::device_for_display(renderer.renderer().egl_context().display())
        .and_then(|device| device.try_get_render_node());

    let dmabuf_formats = renderer.renderer().dmabuf_formats();

    match render_node {
        Ok(Some(node)) => {
            let feedback = DmabufFeedbackBuilder::new(node.dev_id(), dmabuf_formats.clone())
                .build()
                .unwrap();

            let dmabuf_global = state
                .common
                .dmabuf_state
                .create_global_with_default_feedback::<State>(dh, &feedback);

            let render_node = render_node.unwrap().unwrap();
            state.common.wl_drm_state = Some(WlDrmState::new::<State>(
                dh,
                render_node
                    .dev_path_with_type(NodeType::Render)
                    .or_else(|| render_node.dev_path())
                    .ok_or(anyhow!(
                        "Could not determine path for gpu node: {}",
                        render_node
                    ))?,
                dmabuf_formats,
                &dmabuf_global,
            ));

            info!("EGL hardware-acceleration enabled.");
        }
        Ok(None) => {
            warn!("Failed to query render node. Unable to initialize bind display to EGL.")
        }
        Err(err) => {
            warn!(
                ?err,
                "Failed to egl device for display. Unable to initialize bind display to EGL."
            )
        }
    }

    Ok(())
}

impl State {
    pub fn process_winit_event(&mut self, event: WinitEvent, render_ping: &ping::Ping) {
        // here we can handle special cases for winit inputs
        match event {
            WinitEvent::Focus(true) => {
                for seat in self.common.shell.read().seats.iter() {
                    let devices = seat.user_data().get::<Devices>().unwrap();
                    if devices.has_device(&WinitVirtualDevice) {
                        seat.set_active_output(&self.backend.winit().output);
                        break;
                    }
                }
            }
            WinitEvent::Resized { size, .. } => {
                let outputs = self.backend.winit().all_outputs();
                for (index, output) in outputs.iter().enumerate() {
                    let (slice, position) = split_slice(size, outputs.len(), index);
                    let mode = Mode {
                        size: slice.into(),
                        refresh: 60_000,
                    };
                    {
                        let mut config = output
                            .user_data()
                            .get::<RefCell<OutputConfig>>()
                            .unwrap()
                            .borrow_mut();
                        config.mode.0 = slice;
                        config.position = (position.0 as u32, position.1 as u32);
                    }
                    output.delete_mode(output.current_mode().unwrap());
                    output.set_preferred(mode);
                    output.change_current_state(Some(mode), None, None, Some(position.into()));
                    layer_map_for_output(output).arrange();
                }
                self.common.output_configuration_state.update();
                render_ping.ping();
            }
            WinitEvent::Redraw => render_ping.ping(),
            WinitEvent::Input(event) => self.process_input_event(event),
            WinitEvent::CloseRequested => {
                self.common.should_stop = true;
            }
            _ => {}
        };
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_window_holds_one_output_unless_asked_for_more() {
        assert_eq!(split_count(None), 1);
        assert_eq!(split_count(Some("2")), 2);
        assert_eq!(split_count(Some(" 3 ")), 3);
        assert_eq!(split_count(Some("9")), 4);
        assert_eq!(split_count(Some("0")), 1);
        assert_eq!(split_count(Some("two")), 1);
    }

    #[test]
    fn outputs_split_the_window_side_by_side() {
        let size = Size::from((3840, 1080));
        assert_eq!(split_slice(size, 2, 0), ((1920, 1080), (0, 0)));
        assert_eq!(split_slice(size, 2, 1), ((1920, 1080), (1920, 0)));
        assert_eq!(split_slice(size, 1, 0), ((3840, 1080), (0, 0)));
    }
}
