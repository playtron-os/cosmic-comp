// SPDX-License-Identifier: GPL-3.0-only

//! Voice Orb rendering element
//!
//! This module provides the voice orb shader and render element for the voice mode overlay.
//! The orb is a soft, luminous energy sphere with pastel spectral gradient that appears
//! when voice input mode is active.

use std::{borrow::Borrow, time::Instant};

use keyframe::{ease, functions::EaseOutCubic};
use smithay::{
    backend::renderer::{
        element::Kind,
        gles::{
            GlesError, GlesPixelProgram, GlesRenderer, Uniform, UniformName, UniformType,
            element::PixelShaderElement,
        },
    },
    utils::{Logical, Point, Rectangle, Size},
};

use super::element::AsGlowRenderer;
use crate::wayland::protocols::voice_mode::{OrbState, VoiceState};

/// Include the voice orb shader source
pub static VOICE_ORB_SHADER: &str = include_str!("./shaders/voice_orb.frag");

/// Voice orb shader wrapper
pub struct VoiceOrbShader(pub GlesPixelProgram);

/// Animation state for the voice orb
#[derive(Debug, Clone)]
pub struct VoiceOrbAnimation {
    /// When the animation started
    pub start_time: Instant,
    /// Animation duration in seconds
    pub duration: f32,
    /// Starting scale
    pub start_scale: f32,
    /// Target scale
    pub target_scale: f32,
    /// Starting position (normalized 0-1)
    pub start_position: Point<f32, Logical>,
    /// Target position (normalized 0-1)
    pub target_position: Point<f32, Logical>,
    /// Starting morph progress
    pub start_morph: f32,
    /// Target morph progress
    pub target_morph: f32,
}

impl VoiceOrbAnimation {
    /// Create a new grow-in animation with spring bounce
    pub fn grow_in() -> Self {
        Self {
            start_time: Instant::now(),
            duration: 0.4, // 400ms - longer grow phase with bounces at end
            start_scale: 0.0,
            target_scale: 1.0,
            start_position: Point::from((0.5, 0.5)),
            target_position: Point::from((0.5, 0.5)),
            start_morph: 0.0,
            target_morph: 0.0,
        }
    }

    /// Create a shrink-out animation
    pub fn shrink_out(current_scale: f32, current_position: Point<f32, Logical>) -> Self {
        Self {
            start_time: Instant::now(),
            duration: 0.3,
            start_scale: current_scale,
            target_scale: 0.0,
            start_position: current_position,
            target_position: Point::from((0.5, 0.5)),
            start_morph: 0.0,
            target_morph: 0.0,
        }
    }

    /// Create an animation to attach to a window
    pub fn attach_to_window(
        current_position: Point<f32, Logical>,
        window_center: Point<f32, Logical>,
    ) -> Self {
        Self {
            start_time: Instant::now(),
            duration: 0.5,
            start_scale: 1.0,
            target_scale: 1.0,
            start_position: current_position,
            target_position: window_center,
            start_morph: 0.0,
            target_morph: 1.0,
        }
    }

    /// Create an animation to detach from window
    pub fn detach_from_window(window_center: Point<f32, Logical>) -> Self {
        Self {
            start_time: Instant::now(),
            duration: 0.5,
            start_scale: 1.0,
            target_scale: 1.0,
            start_position: window_center,
            target_position: Point::from((0.5, 0.5)),
            start_morph: 1.0,
            target_morph: 0.0,
        }
    }

    /// Get the animation progress (0.0 to 1.0)
    pub fn progress(&self) -> f32 {
        let elapsed = self.start_time.elapsed().as_secs_f32();
        (elapsed / self.duration).min(1.0)
    }

    /// Check if the animation is complete
    pub fn is_complete(&self) -> bool {
        self.progress() >= 1.0
    }

    /// Get the current scale with easing
    pub fn current_scale(&self) -> f32 {
        let t = self.progress();
        // Use spring bounce for grow-in (0 -> 1), ease-out for shrink
        let eased_t = if self.start_scale < self.target_scale {
            // Growing in: use spring bounce
            ease_spring_bounce(t)
        } else {
            // Shrinking out: use smooth ease-out
            ease(EaseOutCubic, 0.0, 1.0, t)
        };
        self.start_scale + (self.target_scale - self.start_scale) * eased_t
    }

    /// Get the current position with easing
    pub fn current_position(&self) -> Point<f32, Logical> {
        let t = ease(EaseOutCubic, 0.0, 1.0, self.progress());
        Point::from((
            self.start_position.x + (self.target_position.x - self.start_position.x) * t,
            self.start_position.y + (self.target_position.y - self.start_position.y) * t,
        ))
    }

    /// Get the current morph progress with easing
    pub fn current_morph(&self) -> f32 {
        let t = ease(EaseOutCubic, 0.0, 1.0, self.progress());
        self.start_morph + (self.target_morph - self.start_morph) * t
    }
}

/// Spring easing with 3 bounces for pop-in effect
/// Grows from 0 to 1, overshoots, bounces back, settles
fn ease_spring_bounce(t: f32) -> f32 {
    if t <= 0.0 {
        return 0.0;
    }
    if t >= 1.0 {
        return 1.0;
    }

    // Keyframe approach for clear grow + bounce:
    // 0.0  -> 0.0  (start)
    // 0.65 -> 1.08 (slight overshoot)
    // 0.78 -> 0.96 (slight undershoot)
    // 0.90 -> 1.02 (tiny overshoot)
    // 1.0  -> 1.0  (settle)

    if t < 0.65 {
        // Grow phase: 0 to 1.08 with ease-out
        let local_t = t / 0.65;
        let eased = 1.0 - (1.0 - local_t).powi(3);
        eased * 1.08
    } else if t < 0.78 {
        // First bounce back: 1.08 to 0.96
        let local_t = (t - 0.65) / 0.13;
        1.08 - 0.12 * local_t
    } else if t < 0.90 {
        // Second bounce up: 0.96 to 1.02
        let local_t = (t - 0.78) / 0.12;
        0.96 + 0.06 * local_t
    } else {
        // Settle: 1.02 to 1.0
        let local_t = (t - 0.90) / 0.10;
        1.02 - 0.02 * local_t
    }
}

/// State for the voice orb rendering
#[derive(Debug, Clone)]
pub struct VoiceOrbState {
    /// Current orb display state
    pub orb_state: OrbState,
    /// Current voice input state
    pub voice_state: VoiceState,
    /// Current scale (0.0 to 1.0)
    pub scale: f32,
    /// Current position (normalized to output)
    pub position: Point<f32, Logical>,
    /// Morph progress for window attachment (0.0 = orb, 1.0 = fill window)
    pub morph_progress: f32,
    /// Pulse intensity (0.0 to 1.0)
    pub pulse_intensity: f32,
    /// Current animation (if any)
    pub animation: Option<VoiceOrbAnimation>,
    /// Attached window geometry (if attached)
    pub attached_window: Option<Rectangle<i32, Logical>>,
    /// Animation start time for shader time uniform
    pub shader_time_start: Instant,
    /// App ID for chat windows
    pub app_id: Option<String>,
}

impl Default for VoiceOrbState {
    fn default() -> Self {
        Self {
            orb_state: OrbState::Hidden,
            voice_state: VoiceState::Idle,
            scale: 0.0,
            position: Point::from((0.5, 0.5)),
            morph_progress: 0.0,
            pulse_intensity: 0.0,
            animation: None,
            attached_window: None,
            shader_time_start: Instant::now(),
            app_id: None,
        }
    }
}

impl VoiceOrbState {
    /// Start showing the orb (floating in center)
    pub fn show_floating(&mut self) {
        self.orb_state = OrbState::Floating;
        self.animation = Some(VoiceOrbAnimation::grow_in());
        self.shader_time_start = Instant::now();
    }

    /// Hide the orb
    pub fn hide(&mut self) {
        if self.orb_state != OrbState::Hidden {
            self.animation = Some(VoiceOrbAnimation::shrink_out(self.scale, self.position));
            self.orb_state = OrbState::Hidden;
        }
    }

    /// Attach to a window
    pub fn attach_to(
        &mut self,
        window_geo: Rectangle<i32, Logical>,
        output_size: Size<i32, Logical>,
    ) {
        let window_center = Point::from((
            (window_geo.loc.x as f32 + window_geo.size.w as f32 / 2.0) / output_size.w as f32,
            (window_geo.loc.y as f32 + window_geo.size.h as f32 / 2.0) / output_size.h as f32,
        ));
        self.animation = Some(VoiceOrbAnimation::attach_to_window(
            self.position,
            window_center,
        ));
        self.attached_window = Some(window_geo);
        self.orb_state = OrbState::Attached;
    }

    /// Detach from window and return to floating mode
    pub fn detach(&mut self) {
        if let Some(_window) = self.attached_window.take() {
            // Use current position for the detach animation
            self.animation = Some(VoiceOrbAnimation::detach_from_window(self.position));
        }
        self.orb_state = OrbState::Floating;
    }

    /// Transition from floating to attached mode when chat window gains focus
    pub fn transition_to_attached(
        &mut self,
        window_geo: Rectangle<i32, Logical>,
        output_size: Size<i32, Logical>,
    ) {
        if self.orb_state == OrbState::Floating {
            self.attach_to(window_geo, output_size);
        }
    }

    /// Transition from attached to floating mode when chat window loses focus
    pub fn transition_to_floating(&mut self) {
        if self.orb_state == OrbState::Attached {
            self.detach();
        }
    }

    /// Check if voice mode is currently active (not hidden)
    pub fn is_active(&self) -> bool {
        self.orb_state != OrbState::Hidden
    }

    /// Update animation state
    pub fn update(&mut self) {
        if let Some(ref animation) = self.animation {
            self.scale = animation.current_scale();
            self.position = animation.current_position();
            self.morph_progress = animation.current_morph();

            if animation.is_complete() {
                // Animation done, clear it
                self.animation = None;

                // If we animated to hidden, ensure scale is 0
                if self.orb_state == OrbState::Hidden {
                    self.scale = 0.0;
                }
            }
        }
    }

    /// Check if the orb should be rendered
    pub fn should_render(&self) -> bool {
        self.scale > 0.001 || self.animation.is_some()
    }

    /// Get the shader time in seconds
    pub fn shader_time(&self) -> f32 {
        self.shader_time_start.elapsed().as_secs_f32()
    }
}

impl VoiceOrbShader {
    /// Initialize the voice orb shader
    pub fn init(renderer: &mut GlesRenderer) -> Result<(), GlesError> {
        let program = renderer.compile_custom_pixel_shader(
            VOICE_ORB_SHADER,
            &[
                UniformName::new("time", UniformType::_1f),
                UniformName::new("scale", UniformType::_1f),
                UniformName::new("pulse", UniformType::_1f),
                UniformName::new("attached", UniformType::_1f),
                UniformName::new("target_center", UniformType::_2f),
                UniformName::new("morph_progress", UniformType::_1f),
            ],
        )?;

        renderer
            .egl_context()
            .user_data()
            .insert_if_missing(|| VoiceOrbShader(program));

        Ok(())
    }

    /// Get the voice orb shader program
    pub fn get<R: AsGlowRenderer>(renderer: &R) -> Option<GlesPixelProgram> {
        Borrow::<GlesRenderer>::borrow(renderer.glow_renderer())
            .egl_context()
            .user_data()
            .get::<VoiceOrbShader>()
            .map(|s| s.0.clone())
    }

    /// Create a voice orb render element
    pub fn element<R: AsGlowRenderer>(
        renderer: &R,
        orb_state: &VoiceOrbState,
        output_geo: Rectangle<i32, Logical>,
    ) -> Option<PixelShaderElement> {
        if !orb_state.should_render() {
            return None;
        }

        let shader = Self::get(renderer)?;

        // Calculate the orb geometry based on state
        let geo = if let Some(window_geo) = orb_state.attached_window {
            // When attached, use window geometry for rendering area
            window_geo
        } else {
            // When floating, center a square render area
            // Orb is 20% of display width, render area is 50% larger for outer glow
            let orb_size = (output_geo.size.w as f32 * 0.20 * orb_state.scale) as i32;
            let render_size = (orb_size as f32 * 1.5) as i32; // Extra space for blue fog
            let center_x =
                (orb_state.position.x * output_geo.size.w as f32) as i32 + output_geo.loc.x;
            let center_y =
                (orb_state.position.y * output_geo.size.h as f32) as i32 + output_geo.loc.y;

            Rectangle::new(
                Point::from((center_x - render_size / 2, center_y - render_size / 2)),
                Size::from((render_size, render_size)),
            )
        };

        // Time for animation
        let time = orb_state.shader_time();

        // Pulse based on voice state
        let pulse = match orb_state.voice_state {
            VoiceState::Recording => orb_state.pulse_intensity,
            VoiceState::Idle => 0.0,
        };

        let attached = if orb_state.orb_state == OrbState::Attached {
            1.0
        } else {
            0.0
        };

        // Create the shader element
        let elem = PixelShaderElement::new(
            shader,
            geo,
            None,
            1.0, // Alpha
            vec![
                Uniform::new("time", time),
                Uniform::new("scale", orb_state.scale),
                Uniform::new("pulse", pulse),
                Uniform::new("attached", attached),
                Uniform::new(
                    "target_center",
                    [orb_state.position.x, orb_state.position.y],
                ),
                Uniform::new("morph_progress", orb_state.morph_progress),
            ],
            Kind::Unspecified,
        );

        Some(elem)
    }
}
