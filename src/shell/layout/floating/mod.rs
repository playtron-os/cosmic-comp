// SPDX-License-Identifier: GPL-3.0-only

use std::{
    collections::{HashMap, VecDeque},
    sync::atomic::{AtomicBool, Ordering},
    time::{Duration, Instant},
};

use cosmic_comp_config::AppearanceConfig;
use cosmic_settings_config::shortcuts::action::ResizeDirection;
use keyframe::{ease, functions::EaseInOutCubic};
use smithay::{
    backend::renderer::{
        ImportAll, ImportMem, Renderer,
        element::{
            AsRenderElements, Element, RenderElement,
            utils::{Relocate, RelocateRenderElement, RescaleRenderElement},
        },
    },
    desktop::{PopupKind, Space, WindowSurfaceType, layer_map_for_output, space::SpaceElement},
    input::Seat,
    output::Output,
    reexports::wayland_server::Resource,
    utils::{IsAlive, Logical, Physical, Point, Rectangle, Scale, Size},
    wayland::seat::WaylandFocus,
};

use crate::{
    backend::render::{
        BLUR_FALLBACK_ALPHA, BLUR_FALLBACK_COLOR, BLUR_TINT_COLOR, BLUR_TINT_STRENGTH,
        BackdropShader, BlurredBackdropShader, ElementFilter, IndicatorShader, Key, Usage,
        element::AsGlowRenderer, get_cached_blur_texture_for_window,
        voice_orb::{VoiceOrbShader, VoiceOrbState},
    },
    shell::{
        CosmicSurface, Direction, ManagedLayer, MoveResult, ResizeMode,
        element::{
            CosmicMapped, CosmicMappedKey, CosmicMappedRenderElement, CosmicWindow, MaximizedState,
            resize_indicator::ResizeIndicator,
            stack::{CosmicStackRenderElement, MoveResult as StackMoveResult, TAB_HEIGHT},
            window::CosmicWindowRenderElement,
        },
        focus::{
            FocusStackMut,
            target::{KeyboardFocusTarget, PointerFocusTarget},
        },
        grabs::{GrabStartData, ReleaseMode, ResizeEdge},
    },
    state::State,
    utils::{prelude::*, tween::EaseRectangle},
    wayland::handlers::xdg_shell::popup::get_popup_toplevel,
};

mod grabs;
pub use self::grabs::*;

pub const ANIMATION_DURATION: Duration = Duration::from_millis(200);
pub const MINIMIZE_ANIMATION_DURATION: Duration = Duration::from_millis(320);

#[derive(Debug, Default)]
pub struct FloatingLayout {
    pub(crate) space: Space<CosmicMapped>,
    last_output_size: Size<i32, Local>,
    spawn_order: Vec<CosmicMapped>,
    animations: HashMap<CosmicMapped, Animation>,
    hovered_stack: Option<(CosmicMapped, Rectangle<i32, Local>)>,
    dirty: AtomicBool,
    pub theme: cosmic::Theme,
    pub appearance: AppearanceConfig,
}

#[derive(Debug)]
enum Animation {
    Tiled {
        start: Instant,
        previous_geometry: Rectangle<i32, Local>,
    },
    Minimize {
        start: Instant,
        previous_geometry: Rectangle<i32, Local>,
        target_geometry: Rectangle<i32, Local>,
    },
    Unminimize {
        start: Instant,
        previous_geometry: Rectangle<i32, Local>,
        target_geometry: Rectangle<i32, Local>,
    },
    /// Client-driven animated resize: sends intermediate configure events
    /// instead of rescaling the buffer. The client renders at each
    /// intermediate size for smooth resize animation.
    ClientDrivenResize {
        start: Instant,
        previous_geometry: Rectangle<i32, Local>,
        target_geometry: Rectangle<i32, Local>,
        /// Last size we sent in a configure (for change detection)
        last_sent_size: Size<i32, Local>,
    },
}

impl Animation {
    fn start(&self) -> &Instant {
        match self {
            Animation::Tiled { start, .. } => start,
            Animation::Minimize { start, .. } => start,
            Animation::Unminimize { start, .. } => start,
            Animation::ClientDrivenResize { start, .. } => start,
        }
    }

    fn alpha(&self) -> f32 {
        match self {
            Animation::Tiled { .. } => 1.0,
            Animation::ClientDrivenResize { .. } => 1.0,
            Animation::Minimize { start, .. } => {
                let percentage = Instant::now()
                    .duration_since(*start)
                    .min(MINIMIZE_ANIMATION_DURATION)
                    .as_secs_f32()
                    / MINIMIZE_ANIMATION_DURATION.as_secs_f32();
                1.0 - ((percentage - 0.5).max(0.0) * 2.0)
            }
            Animation::Unminimize { start, .. } => {
                let percentage = Instant::now()
                    .duration_since(*start)
                    .min(MINIMIZE_ANIMATION_DURATION)
                    .as_secs_f32()
                    / MINIMIZE_ANIMATION_DURATION.as_secs_f32();
                (percentage * 2.0).min(1.0)
            }
        }
    }

    fn previous_geometry(&self) -> &Rectangle<i32, Local> {
        match self {
            Animation::Tiled {
                previous_geometry, ..
            } => previous_geometry,
            Animation::Minimize {
                previous_geometry, ..
            } => previous_geometry,
            Animation::Unminimize {
                previous_geometry, ..
            } => previous_geometry,
            Animation::ClientDrivenResize {
                previous_geometry, ..
            } => previous_geometry,
        }
    }

    /// Returns true if this animation uses client-driven resize (sends configure events
    /// instead of rescaling buffers)
    fn is_client_driven(&self) -> bool {
        matches!(self, Animation::ClientDrivenResize { .. })
    }

    /// For ClientDrivenResize, calculate the render position based on the client's
    /// actual buffer size. This keeps position and buffer in sync regardless of
    /// how many frames the client takes to respond.
    ///
    /// The key insight: interpolate position based on where the actual buffer size
    /// falls on the animation curve, not based on time.
    fn render_geometry_for_buffer_size(
        &self,
        actual_buffer_size: Size<i32, Logical>,
    ) -> Option<Rectangle<i32, Local>> {
        match self {
            Animation::ClientDrivenResize {
                start,
                previous_geometry,
                target_geometry,
                ..
            } => {
                let buffer_size = actual_buffer_size.as_local();

                // Calculate how far along the size is between previous and target
                let prev_size = previous_geometry.size;
                let target_size = target_geometry.size;

                // If sizes are the same, this is a position-only animation
                // Use time-based easing for smooth position animation
                if prev_size == target_size {
                    let now = Instant::now();
                    let progress = now
                        .duration_since(*start)
                        .min(ANIMATION_DURATION)
                        .as_secs_f64()
                        / ANIMATION_DURATION.as_secs_f64();

                    // Use same easing as size animations
                    let eased_geo: Rectangle<i32, Local> = ease(
                        EaseInOutCubic,
                        EaseRectangle(*previous_geometry),
                        EaseRectangle(*target_geometry),
                        progress,
                    )
                    .unwrap();

                    return Some(eased_geo);
                }

                // Calculate progress based on size (use width as proxy, could average)
                let size_progress = if prev_size.w != target_size.w {
                    (buffer_size.w - prev_size.w) as f64 / (target_size.w - prev_size.w) as f64
                } else if prev_size.h != target_size.h {
                    (buffer_size.h - prev_size.h) as f64 / (target_size.h - prev_size.h) as f64
                } else {
                    1.0
                };

                // Clamp progress to [0, 1]
                let progress = size_progress.clamp(0.0, 1.0);

                // Interpolate position linearly based on size progress
                let x = previous_geometry.loc.x as f64
                    + (target_geometry.loc.x - previous_geometry.loc.x) as f64 * progress;
                let y = previous_geometry.loc.y as f64
                    + (target_geometry.loc.y - previous_geometry.loc.y) as f64 * progress;

                Some(Rectangle::new(
                    Point::new(x.round() as i32, y.round() as i32),
                    buffer_size,
                ))
            }
            _ => None,
        }
    }

    fn geometry(
        &self,
        output_geometry: Rectangle<i32, Logical>,
        current_geometry: Rectangle<i32, Local>,
        tiled_state: Option<&TiledCorners>,
        gaps: (i32, i32),
    ) -> Rectangle<i32, Local> {
        let (duration, target_rect) = match self {
            Animation::Minimize {
                target_geometry, ..
            }
            | Animation::Unminimize {
                target_geometry, ..
            } => (MINIMIZE_ANIMATION_DURATION, *target_geometry),
            Animation::ClientDrivenResize {
                previous_geometry, ..
            } => {
                // For client-driven resize, return the previous geometry as fallback.
                // The actual render geometry should be calculated via
                // render_geometry_for_buffer_size() based on the client's buffer.
                return *previous_geometry;
            }
            Animation::Tiled { .. } => {
                let target_geometry = if let Some(target_rect) =
                    tiled_state.map(|state| state.relative_geometry(output_geometry, gaps))
                {
                    target_rect
                } else {
                    current_geometry
                };
                (ANIMATION_DURATION, target_geometry)
            }
        };
        let previous_rect = *self.previous_geometry();
        let start = *self.start();
        let now = Instant::now();
        let progress =
            now.duration_since(start).min(duration).as_secs_f64() / duration.as_secs_f64();

        ease(
            EaseInOutCubic,
            EaseRectangle(previous_rect),
            EaseRectangle(target_rect),
            progress,
        )
        .unwrap()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TiledCorners {
    Top,
    TopRight,
    Right,
    BottomRight,
    Bottom,
    BottomLeft,
    Left,
    TopLeft,
}

impl TiledCorners {
    pub fn relative_geometry(
        &self,
        output_geometry: Rectangle<i32, Logical>,
        gaps: (i32, i32),
    ) -> Rectangle<i32, Local> {
        let (_, inner) = gaps;
        let (loc, size) = match self {
            TiledCorners::Bottom => (
                Point::from((
                    output_geometry.loc.x + inner,
                    output_geometry.loc.y + (output_geometry.size.h / 2) + inner / 2,
                )),
                Size::from((
                    output_geometry.size.w - inner * 2,
                    output_geometry.size.h / 2 - inner * 3 / 2,
                )),
            ),
            TiledCorners::BottomLeft => (
                Point::from((
                    output_geometry.loc.x + inner,
                    output_geometry.loc.y + (output_geometry.size.h / 2) + inner / 2,
                )),
                Size::from((
                    output_geometry.size.w / 2 - inner * 3 / 2,
                    output_geometry.size.h / 2 - inner * 3 / 2,
                )),
            ),
            TiledCorners::BottomRight => (
                Point::from((
                    output_geometry.loc.x + (output_geometry.size.w / 2) + inner / 2,
                    output_geometry.loc.y + (output_geometry.size.h / 2) + inner / 2,
                )),
                Size::from((
                    output_geometry.size.w / 2 - inner * 3 / 2,
                    output_geometry.size.h / 2 - inner * 3 / 2,
                )),
            ),
            TiledCorners::Left => (
                Point::from((output_geometry.loc.x + inner, output_geometry.loc.y + inner)),
                Size::from((
                    output_geometry.size.w / 2 - inner * 3 / 2,
                    output_geometry.size.h - inner * 2,
                )),
            ),
            TiledCorners::Top => (
                Point::from((output_geometry.loc.x + inner, output_geometry.loc.y + inner)),
                Size::from((
                    output_geometry.size.w - inner * 2,
                    output_geometry.size.h / 2 - inner * 3 / 2,
                )),
            ),
            TiledCorners::TopLeft => (
                Point::from((output_geometry.loc.x + inner, output_geometry.loc.y + inner)),
                Size::from((
                    output_geometry.size.w / 2 - inner * 3 / 2,
                    output_geometry.size.h / 2 - inner * 3 / 2,
                )),
            ),
            TiledCorners::TopRight => (
                Point::from((
                    output_geometry.loc.x + (output_geometry.size.w / 2) + inner / 2,
                    output_geometry.loc.y + inner,
                )),
                Size::from((
                    output_geometry.size.w / 2 - inner * 3 / 2,
                    output_geometry.size.h / 2 - inner * 3 / 2,
                )),
            ),
            TiledCorners::Right => (
                Point::from((
                    output_geometry.loc.x + (output_geometry.size.w / 2) + inner / 2,
                    output_geometry.loc.y + inner,
                )),
                Size::from((
                    output_geometry.size.w / 2 - inner * 3 / 2,
                    output_geometry.size.h - inner * 2,
                )),
            ),
        };

        Rectangle::new(loc, size).as_local()
    }
}

/// A group of consecutive blur windows that can share a single background capture.
/// Windows are grouped when there are no non-blur windows between them.
#[derive(Clone)]
pub struct BlurWindowGroup {
    /// The z-index threshold for capturing (lowest z-index in the group)
    pub capture_z_threshold: usize,
    /// Windows in this group: (key, geometry, alpha, z_index)
    pub windows: Vec<(CosmicMappedKey, Rectangle<i32, Local>, f32, usize)>,
}

impl FloatingLayout {
    pub fn new(
        theme: cosmic::Theme,
        appearance: AppearanceConfig,
        output: &Output,
    ) -> FloatingLayout {
        let mut layout = Self {
            theme,
            last_output_size: output.geometry().size.as_local(),
            appearance,
            ..Default::default()
        };
        layout.space.map_output(output, (0, 0));
        layout
    }

    pub fn set_output(&mut self, output: &Output) {
        let old_output = self.space.outputs().next().unwrap().clone();
        self.space.unmap_output(&old_output);
        self.space.map_output(output, (0, 0));

        let old_output_geometry = {
            let layers = layer_map_for_output(&old_output);
            layers.non_exclusive_zone()
        }
        .to_f64();
        let output_geometry = {
            let layers = layer_map_for_output(output);
            layers.non_exclusive_zone()
        };

        for mapped in self
            .space
            .elements()
            .cloned()
            .collect::<Vec<_>>()
            .into_iter()
        {
            let tiled_state = *mapped.floating_tiled.lock().unwrap();
            if let Some(tiled_state) = tiled_state {
                let geometry = tiled_state.relative_geometry(output_geometry, self.gaps());
                self.map_internal(
                    mapped,
                    Some(geometry.loc),
                    Some(geometry.size.as_logical()),
                    None,
                );
            } else {
                let geometry = self.space.element_geometry(&mapped).unwrap().to_f64();
                let new_loc = (
                    ((geometry.loc.x - old_output_geometry.loc.x).max(0.)
                        / old_output_geometry.size.w
                        * output_geometry.size.w as f64)
                        .round() as i32
                        + output_geometry.loc.x,
                    ((geometry.loc.y - old_output_geometry.loc.y).max(0.)
                        / old_output_geometry.size.h
                        * output_geometry.size.h as f64)
                        .round() as i32
                        + output_geometry.loc.y,
                );
                self.map_internal(mapped, Some(Point::from(new_loc)), None, None);
            }
        }

        self.last_output_size = output.geometry().size.as_local();
        self.recalculate();
    }

    pub fn map(
        &mut self,
        mapped: impl Into<CosmicMapped>,
        position: impl Into<Option<Point<i32, Local>>>,
    ) {
        let mapped = mapped.into();
        let position = position.into();

        self.map_internal(mapped, position, None, None)
    }

    // ─────────────────────────────────────────────────────────────────────────────
    // Embedded child animation helpers
    // ─────────────────────────────────────────────────────────────────────────────

    /// Start animation sync tracking for all embedded children of a parent.
    /// Call this at the beginning of a client-driven animation.
    fn start_embed_animation_tracking(
        &self,
        parent_surface_id: Option<&String>,
        parent_geometry: Rectangle<i32, Local>,
    ) {
        let Some(sid) = parent_surface_id else { return };

        let embedded_children =
            crate::wayland::handlers::surface_embed::get_children_for_parent_by_surface_id(sid);

        for (embedded_surface_id, embed_info) in &embedded_children {
            let initial_embed_size = if let Some(ref anchor) = embed_info.anchor_config {
                anchor.calculate_geometry(parent_geometry.size.w, parent_geometry.size.h)
            } else {
                embed_info.geometry
            };

            crate::wayland::handlers::surface_embed::start_embed_animation_sync(
                embedded_surface_id,
                initial_embed_size.size,
            );

            tracing::debug!(
                embedded_surface_id,
                ?initial_embed_size,
                "Started animation sync for embedded child"
            );
        }
    }

    /// Configure all embedded children to a given parent size.
    /// If `record_for_animation` is true, records the configure for animation sync.
    fn configure_embeds_to_parent_size(
        &self,
        parent_surface_id: Option<&String>,
        parent_width: i32,
        parent_height: i32,
        record_for_animation: bool,
    ) {
        let Some(sid) = parent_surface_id else { return };

        let embeds =
            crate::wayland::handlers::surface_embed::update_embedded_geometry_for_parent_by_surface_id(
                sid,
                parent_width,
                parent_height,
            );

        for (embedded_surface_id, new_geometry) in embeds {
            if let Some(embedded_elem) = self.space.elements().find(|e| {
                e.active_window()
                    .wl_surface()
                    .map(|s| s.id().to_string() == embedded_surface_id)
                    .unwrap_or(false)
            }) {
                let global_geo = Rectangle::new(
                    (new_geometry.loc.x, new_geometry.loc.y).into(),
                    (new_geometry.size.w, new_geometry.size.h).into(),
                );
                embedded_elem.active_window().set_geometry(global_geo, 0);
                embedded_elem.configure();

                if record_for_animation {
                    crate::wayland::handlers::surface_embed::record_embed_configure(
                        &embedded_surface_id,
                        new_geometry.size,
                    );
                }

                tracing::debug!(
                    embedded_surface_id,
                    size = ?new_geometry.size,
                    record_for_animation,
                    "Configured embedded window"
                );
            }
        }
    }

    /// Calculate the lookahead geometry for frame-ahead buffering.
    /// This calculates where the animation will be ~16ms (one frame) ahead.
    fn calculate_lookahead_geometry(
        from: Rectangle<i32, Local>,
        to: Rectangle<i32, Local>,
    ) -> Rectangle<i32, Local> {
        let lookahead_duration = std::time::Duration::from_millis(16);
        let lookahead_progress =
            (lookahead_duration.as_secs_f64() / ANIMATION_DURATION.as_secs_f64()).min(1.0);

        ease(
            EaseInOutCubic,
            EaseRectangle(from),
            EaseRectangle(to),
            lookahead_progress,
        )
        .unwrap()
    }

    /// Calculate lookahead geometry based on animation progress plus estimated latency.
    /// Used during animation updates to configure embedded windows ahead of time.
    fn calculate_lookahead_for_latency(
        previous_geometry: Rectangle<i32, Local>,
        target_geometry: Rectangle<i32, Local>,
        anim_start: Instant,
        estimated_latency: std::time::Duration,
    ) -> Rectangle<i32, Local> {
        let now = Instant::now();
        let lookahead_elapsed = now.duration_since(anim_start) + estimated_latency;
        let lookahead_progress =
            (lookahead_elapsed.as_secs_f64() / ANIMATION_DURATION.as_secs_f64()).min(1.0);

        ease(
            EaseInOutCubic,
            EaseRectangle(previous_geometry),
            EaseRectangle(target_geometry),
            lookahead_progress,
        )
        .unwrap()
    }

    /// Render embedded children of a parent window.
    /// Returns the render elements for all embedded children positioned relative to the parent.
    fn render_embedded_children<R>(
        &self,
        renderer: &mut R,
        parent_elem: &CosmicMapped,
        parent_geometry: Rectangle<i32, Local>,
        output_scale: f64,
        alpha: f32,
    ) -> Vec<CosmicMappedRenderElement<R>>
    where
        R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
        R::TextureId: Send + Clone + 'static,
        CosmicMappedRenderElement<R>: RenderElement<R>,
        CosmicWindowRenderElement<R>: RenderElement<R>,
        CosmicStackRenderElement<R>: RenderElement<R>,
    {
        let mut embedded_elements = Vec::new();

        let parent_window = parent_elem.active_window();
        let Some(parent_surface) = parent_window.wl_surface() else {
            return embedded_elements;
        };

        let parent_surface_id = parent_surface.id().to_string();
        let embedded_children =
            crate::wayland::handlers::surface_embed::get_children_for_parent_by_surface_id(
                &parent_surface_id,
            );

        for (embedded_surface_id, embed_info) in embedded_children {
            // Find the embedded element in the space
            let embedded_elem = self.space.elements().find(|e| {
                e.active_window()
                    .wl_surface()
                    .map(|s| s.id().to_string() == embedded_surface_id)
                    .unwrap_or(false)
            });

            let Some(embedded_elem) = embedded_elem else {
                let output_name = self
                    .space
                    .outputs()
                    .next()
                    .map(|o| o.name())
                    .unwrap_or_else(|| "unknown".to_string());
                tracing::warn!(
                    parent_app_id = %parent_elem.active_window().app_id(),
                    embedded_surface_id = %embedded_surface_id,
                    output = %output_name,
                    "Embedded child not found in this space - may be on different output"
                );
                continue;
            };

            // Calculate actual embed geometry based on parent's (possibly animated) size
            let actual_geometry = if let Some(ref anchor_config) = embed_info.anchor_config {
                anchor_config.calculate_geometry(parent_geometry.size.w, parent_geometry.size.h)
            } else {
                embed_info.geometry
            };

            // Render the embedded window at parent position + embed offset
            let embed_offset = smithay::utils::Point::<i32, smithay::utils::Logical>::from((
                actual_geometry.loc.x,
                actual_geometry.loc.y,
            ));
            let render_location = parent_geometry.loc.as_logical() + embed_offset;

            // Use actual_geometry.size as clip size to handle resize transitions
            let clip_size = Some(smithay::utils::Size::<i32, smithay::utils::Logical>::from(
                (actual_geometry.size.w, actual_geometry.size.h),
            ));

            tracing::debug!(
                embedded_app_id = %embedded_elem.active_window().app_id(),
                parent_app_id = %parent_elem.active_window().app_id(),
                parent_loc = ?parent_geometry.loc,
                parent_size = ?parent_geometry.size,
                embed_offset = ?embed_offset,
                render_location = ?render_location,
                clip_size = ?clip_size,
                "Rendering embedded window in front of parent"
            );

            let elements = embedded_elem.render_elements(
                renderer,
                render_location.to_physical_precise_round(output_scale),
                clip_size,
                output_scale.into(),
                alpha,
                None,
            );
            embedded_elements.extend(elements);
        }

        embedded_elements
    }

    /// Render popups for an embedded window at its correct visual position (inside parent)
    fn render_embedded_popups<R>(
        &self,
        renderer: &mut R,
        embedded_elem: &CosmicMapped,
        output_scale: f64,
        alpha: f32,
    ) -> Vec<CosmicMappedRenderElement<R>>
    where
        R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
        R::TextureId: Send + Clone + 'static,
        CosmicMappedRenderElement<R>: RenderElement<R>,
        CosmicWindowRenderElement<R>: RenderElement<R>,
        CosmicStackRenderElement<R>: RenderElement<R>,
    {
        let mut popup_elements = Vec::new();

        // Get the embedded window's surface ID
        let embedded_window = embedded_elem.active_window();
        let Some(embedded_surface) = embedded_window.wl_surface() else {
            return popup_elements;
        };
        let embedded_surface_id = embedded_surface.id().to_string();

        // Get the embed info to find the parent and embed geometry
        let Some(embed_info) = crate::wayland::handlers::surface_embed::get_embed_render_info_by_id(
            &embedded_surface_id,
        ) else {
            return popup_elements;
        };

        // Find the parent element
        let parent_elem = self.space.elements().find(|e| {
            e.active_window()
                .wl_surface()
                .map(|s| s.id().to_string() == embed_info.parent_surface_id)
                .unwrap_or(false)
        });

        let Some(parent_elem) = parent_elem else {
            return popup_elements;
        };

        // Get parent geometry (possibly animated)
        let parent_geometry = self
            .animations
            .get(parent_elem)
            .map(|anim| *anim.previous_geometry())
            .unwrap_or_else(|| self.space.element_geometry(parent_elem).unwrap().as_local());

        // Calculate actual embed geometry based on parent's size
        let actual_geometry = if let Some(ref anchor_config) = embed_info.anchor_config {
            anchor_config.calculate_geometry(parent_geometry.size.w, parent_geometry.size.h)
        } else {
            embed_info.geometry
        };

        // Calculate where the embedded window is visually rendered
        let embed_offset = smithay::utils::Point::<i32, smithay::utils::Logical>::from((
            actual_geometry.loc.x,
            actual_geometry.loc.y,
        ));
        let render_location = parent_geometry.loc.as_logical() + embed_offset;

        // popup_render_elements expects: target_render_loc - elem.geometry().loc
        let popup_render_offset = render_location - embedded_elem.geometry().loc;
        popup_elements.extend(embedded_elem.popup_render_elements(
            renderer,
            popup_render_offset.to_physical_precise_round(output_scale),
            output_scale.into(),
            alpha,
        ));

        popup_elements
    }

    /// Update embedded children with lookahead sizes during animation.
    /// This ensures embedded windows commit buffers for the next frame ahead of time.
    fn update_embeds_with_lookahead(
        &self,
        mapped: &CosmicMapped,
        previous_geometry: Rectangle<i32, Local>,
        target_geometry: Rectangle<i32, Local>,
    ) {
        let parent_surface_id = mapped
            .active_window()
            .wl_surface()
            .map(|s| s.id().to_string());

        let Some(ref sid) = parent_surface_id else {
            return;
        };

        let embedded_children =
            crate::wayland::handlers::surface_embed::get_children_for_parent_by_surface_id(sid);

        let anim_start = self
            .animations
            .get(mapped)
            .map(|a| *a.start())
            .unwrap_or_else(Instant::now);

        for (embedded_surface_id, _embed_info) in &embedded_children {
            let estimated_latency =
                crate::wayland::handlers::surface_embed::get_embed_animation_sync(
                    embedded_surface_id,
                )
                .map(|sync| sync.estimated_latency)
                .unwrap_or(std::time::Duration::from_millis(16));

            let lookahead_parent_geometry = Self::calculate_lookahead_for_latency(
                previous_geometry,
                target_geometry,
                anim_start,
                estimated_latency,
            );

            // Update embedded geometry for lookahead parent size
            let lookahead_embeds =
                crate::wayland::handlers::surface_embed::update_embedded_geometry_for_parent_by_surface_id(
                    sid,
                    lookahead_parent_geometry.size.w,
                    lookahead_parent_geometry.size.h,
                );

            // Configure the specific embedded window to lookahead size
            for (embed_id, new_geometry) in lookahead_embeds {
                if &embed_id != embedded_surface_id {
                    continue;
                }

                if let Some(embedded_elem) = self.space.elements().find(|e| {
                    e.active_window()
                        .wl_surface()
                        .map(|s| s.id().to_string() == embed_id)
                        .unwrap_or(false)
                }) {
                    let global_geo = Rectangle::new(
                        (new_geometry.loc.x, new_geometry.loc.y).into(),
                        (new_geometry.size.w, new_geometry.size.h).into(),
                    );
                    embedded_elem.active_window().set_geometry(global_geo, 0);
                    embedded_elem.configure();

                    crate::wayland::handlers::surface_embed::record_embed_configure(
                        &embed_id,
                        new_geometry.size,
                    );

                    tracing::trace!(
                        embed_id,
                        ?estimated_latency,
                        lookahead_size = ?new_geometry.size,
                        "Configured embedded with lookahead"
                    );
                }
            }
        }
    }

    pub fn map_maximized(
        &mut self,
        mapped: CosmicMapped,
        previous_geometry: Rectangle<i32, Local>,
        animate: bool,
    ) {
        self.map_maximized_internal(mapped, previous_geometry, animate, false)
    }

    /// Map a window as maximized with client-driven animation.
    /// The client receives intermediate configure events during the animation.
    pub fn map_maximized_client_driven(
        &mut self,
        mapped: CosmicMapped,
        previous_geometry: Rectangle<i32, Local>,
    ) {
        self.map_maximized_internal(mapped, previous_geometry, true, true)
    }

    fn map_maximized_internal(
        &mut self,
        mapped: CosmicMapped,
        previous_geometry: Rectangle<i32, Local>,
        animate: bool,
        client_driven: bool,
    ) {
        let output = self.space.outputs().next().unwrap().clone();
        let layers = layer_map_for_output(&output);
        let target_geometry = layers.non_exclusive_zone().as_local();

        mapped.set_bounds(target_geometry.size.as_logical());
        mapped.set_tiled(true);
        mapped.set_maximized(true);

        let parent_surface_id = mapped
            .active_window()
            .wl_surface()
            .map(|s| s.id().to_string());

        if client_driven && animate {
            // Client-driven: use frame-ahead buffering for embedded children
            self.start_embed_animation_tracking(parent_surface_id.as_ref(), previous_geometry);

            let lookahead_geometry =
                Self::calculate_lookahead_geometry(previous_geometry, target_geometry);
            self.configure_embeds_to_parent_size(
                parent_surface_id.as_ref(),
                lookahead_geometry.size.w,
                lookahead_geometry.size.h,
                true, // record for animation
            );

            self.animations.insert(
                mapped.clone(),
                Animation::ClientDrivenResize {
                    start: Instant::now(),
                    previous_geometry,
                    target_geometry,
                    last_sent_size: previous_geometry.size,
                },
            );
            mapped.set_geometry(previous_geometry.to_global(&output));
            mapped.configure();
        } else {
            // Compositor-driven: configure embedded children to final target size
            self.configure_embeds_to_parent_size(
                parent_surface_id.as_ref(),
                target_geometry.size.w,
                target_geometry.size.h,
                false, // no animation tracking
            );

            mapped.set_geometry(target_geometry.to_global(&output));
            mapped.configure();

            if animate {
                self.update_or_insert_tiled_animation(&mapped, previous_geometry, target_geometry);
            } else {
                self.animations.remove(&mapped);
            }
        }

        self.finalize_maximize_map(mapped, target_geometry);
    }

    /// Update an existing animation's target or insert a new Tiled animation
    fn update_or_insert_tiled_animation(
        &mut self,
        mapped: &CosmicMapped,
        previous_geometry: Rectangle<i32, Local>,
        target_geometry: Rectangle<i32, Local>,
    ) {
        if let Some(existing_anim) = self.animations.get_mut(mapped) {
            match existing_anim {
                Animation::Unminimize {
                    target_geometry: tg,
                    ..
                }
                | Animation::ClientDrivenResize {
                    target_geometry: tg,
                    ..
                } => {
                    *tg = target_geometry;
                }
                Animation::Minimize { .. } | Animation::Tiled { .. } => {}
            }
        } else {
            self.animations.insert(
                mapped.clone(),
                Animation::Tiled {
                    start: Instant::now(),
                    previous_geometry,
                },
            );
        }
    }

    /// Finalize mapping a maximized window (common cleanup for both animation modes)
    fn finalize_maximize_map(
        &mut self,
        mapped: CosmicMapped,
        target_geometry: Rectangle<i32, Local>,
    ) {
        if let Some(pos) = self.spawn_order.iter().position(|m| m == &mapped) {
            self.spawn_order.truncate(pos);
        }

        mapped.moved_since_mapped.store(true, Ordering::SeqCst);

        if mapped.floating_tiled.lock().unwrap().take().is_some() {
            if let Some(state) = mapped.maximized_state.lock().unwrap().as_mut() {
                if let Some(real_old_geo) = *mapped.last_geometry.lock().unwrap() {
                    state.original_geometry = real_old_geo;
                }
            };
        }
        self.space
            .map_element(mapped, target_geometry.loc.as_logical(), true);
        self.space.refresh();
    }

    /// Start a client-driven animated resize from current geometry to target.
    /// Instead of rescaling buffers, this sends intermediate configure events
    /// to the client so it can render at each intermediate size for smooth animation.
    ///
    /// This is useful for smooth maximize/unmaximize transitions when the client
    /// supports rendering at varying sizes quickly.
    pub fn start_client_driven_resize(
        &mut self,
        mapped: CosmicMapped,
        target_geometry: Rectangle<i32, Local>,
    ) {
        let current_geometry = self
            .space
            .element_geometry(&mapped)
            .map(RectExt::as_local)
            .unwrap_or(target_geometry);

        let parent_surface_id = mapped
            .active_window()
            .wl_surface()
            .map(|s| s.id().to_string());

        // Start animation sync and configure embeds to lookahead size
        self.start_embed_animation_tracking(parent_surface_id.as_ref(), current_geometry);

        let lookahead_geometry =
            Self::calculate_lookahead_geometry(current_geometry, target_geometry);
        self.configure_embeds_to_parent_size(
            parent_surface_id.as_ref(),
            lookahead_geometry.size.w,
            lookahead_geometry.size.h,
            true, // record for animation
        );

        // Start animation from current to target
        self.animations.insert(
            mapped.clone(),
            Animation::ClientDrivenResize {
                start: Instant::now(),
                previous_geometry: current_geometry,
                target_geometry,
                last_sent_size: current_geometry.size,
            },
        );

        // Map at current position (will be animated)
        self.space
            .map_element(mapped, current_geometry.loc.as_logical(), true);
        self.dirty.store(true, Ordering::SeqCst);
    }

    /// Check if the given mapped element has a client-driven resize animation active
    pub fn has_client_driven_resize(&self, mapped: &CosmicMapped) -> bool {
        self.animations
            .get(mapped)
            .map(|a| a.is_client_driven())
            .unwrap_or(false)
    }

    pub(in crate::shell) fn map_internal(
        &mut self,
        mapped: CosmicMapped,
        position: Option<Point<i32, Local>>,
        size: Option<Size<i32, Logical>>,
        prev: Option<Rectangle<i32, Local>>,
    ) {
        let already_mapped = self.space.element_geometry(&mapped).map(RectExt::as_local);
        let mut win_geo = mapped.geometry().as_local();

        let output = self.space.outputs().next().unwrap().clone();
        let layers = layer_map_for_output(&output);
        let output_geometry = layers.non_exclusive_zone();
        mapped.set_bounds(output_geometry.size);
        let last_geometry = *mapped.last_geometry.lock().unwrap();
        let min_size = mapped.min_size().unwrap_or((320, 240).into());

        if let Some(size) = size
            .map(SizeExt::as_local)
            .or(last_geometry.map(|g| g.size))
        {
            win_geo.size = size;
        } else {
            let max_size = mapped.max_size().unwrap_or(
                (
                    min_size.w.max(output_geometry.size.w / 3 * 2),
                    min_size.h.max(output_geometry.size.h / 3 * 2),
                )
                    .into(),
            );

            // if the current geometry is too large
            if win_geo.size.w > max_size.w {
                // try a more reasonable size
                let mut width = output_geometry.size.w / 3 * 2;
                if max_size.w != 0 {
                    // don't go larger then the max_size ...
                    width = std::cmp::min(max_size.w, width);
                }
                if min_size.w != 0 {
                    // ... but also don't go smaller than the min_size
                    width = std::cmp::max(min_size.w, width);
                }
                win_geo.size.w = width;
            }
            // but no matter the supported sizes, don't be larger than our non-exclusive-zone
            win_geo.size.w = std::cmp::min(win_geo.size.w, output_geometry.size.w);

            if win_geo.size.h > max_size.h {
                // try a more reasonable size
                let mut height = output_geometry.size.h / 3 * 2;
                if max_size.h != 0 {
                    // don't go larger then the max_size ...
                    height = std::cmp::min(max_size.h, height);
                }
                if min_size.h != 0 {
                    // ... but also don't go smaller than the min_size
                    height = std::cmp::max(min_size.h, height);
                }
                win_geo.size.h = height;
            }
            // but no matter the supported sizes, don't be larger than our non-exclusive-zone
            win_geo.size.h = std::cmp::min(win_geo.size.h, output_geometry.size.h);
        }

        let position = position
            .or_else(|| last_geometry.map(|g| g.loc))
            .unwrap_or_else(|| {
                // cleanup moved windows
                if let Some(pos) = self
                    .spawn_order
                    .iter()
                    .position(|w| !w.alive() || w.moved_since_mapped.load(Ordering::SeqCst))
                {
                    self.spawn_order.truncate(pos);
                }

                let three_fours_width = (output_geometry.size.w / 4 * 3).max(360);

                // figure out new position
                let pos = self
                    .spawn_order
                    .last()
                    .and_then(|window| self.space.element_geometry(window))
                    .filter(|geo| {
                        geo.size.w < three_fours_width
                            && win_geo.size.w < three_fours_width
                            && output_geometry.contains_rect(*geo)
                    })
                    .map(|geometry| {
                        let mut geometry: Rectangle<u32, Logical> = Rectangle::new(
                            (geometry.loc.x as u32, geometry.loc.y as u32).into(),
                            (geometry.size.w as u32, geometry.size.h as u32).into(),
                        );

                        // move down
                        geometry.loc.y += 48;

                        // do we need to address the height?
                        let new_column = if geometry.loc.y + min_size.h as u32
                            <= (output_geometry.loc.y + output_geometry.size.h - 16) as u32
                        {
                            // alternate to the sides
                            let offset = if self
                                .spawn_order
                                .iter()
                                .flat_map(|w| self.space.element_geometry(w))
                                .filter(|geo| geo.size.w < three_fours_width)
                                .count()
                                % 2
                                == 0
                            {
                                (geometry.loc.x + geometry.size.w)
                                    .checked_sub(96 + (win_geo.size.w as u32))
                            } else {
                                (geometry.loc.x + geometry.size.w)
                                    .checked_sub((win_geo.size.w as u32).saturating_sub(48))
                            };

                            if let Some(offset) = offset {
                                geometry.loc.x = offset;
                                // do we need to resize?
                                if geometry.loc.y as i32 + win_geo.size.h
                                    > output_geometry.loc.y + output_geometry.size.h - 16
                                {
                                    win_geo.size.h =
                                        (output_geometry.loc.y + output_geometry.size.h - 16)
                                            - geometry.loc.y as i32;
                                }

                                false
                            } else {
                                true
                            }
                        } else {
                            true
                        };

                        if new_column {
                            let min_y = self
                                .spawn_order
                                .iter()
                                .flat_map(|w| {
                                    self.space
                                        .element_geometry(w)
                                        .filter(|geo| geo.size.w < three_fours_width)
                                        .map(|geo| geo.loc.y)
                                })
                                .min()
                                .unwrap() as u32;
                            geometry.loc.y = min_y.saturating_sub(16);

                            match geometry.loc.x.checked_sub(144) {
                                Some(new_x) => geometry.loc.x = new_x,
                                None => {
                                    // if we go out to the left, cycle around to the right
                                    geometry.loc.x =
                                        ((output_geometry.loc.x + output_geometry.size.w) as u32)
                                            .saturating_sub(geometry.size.w + 16)
                                }
                            };
                        }

                        // check padding again
                        if geometry.loc.x < (output_geometry.loc.x + 16) as u32 {
                            geometry.loc.x = (output_geometry.loc.x + 16) as u32;
                        }
                        if geometry.loc.y < (output_geometry.loc.y + 16) as u32 {
                            geometry.loc.y = (output_geometry.loc.y + 16) as u32;
                        }
                        // if the width would be too high, we wouldn't be here
                        if geometry.loc.y as i32 + win_geo.size.h
                            > (output_geometry.loc.y + output_geometry.size.h - 16)
                        {
                            win_geo.size.h = output_geometry.loc.y + output_geometry.size.h
                                - 16
                                - geometry.loc.y as i32;
                        }

                        Point::<i32, Logical>::from((geometry.loc.x as i32, geometry.loc.y as i32))
                    })
                    .unwrap_or_else(|| {
                        (
                            output_geometry.loc.x + output_geometry.size.w / 2 - win_geo.size.w / 2,
                            output_geometry.loc.y
                                + (output_geometry.size.h / 2 - win_geo.size.h / 2)
                                    .min(output_geometry.size.h / 8),
                        )
                            .into()
                    })
                    .as_local();

                mapped.moved_since_mapped.store(false, Ordering::SeqCst);
                self.spawn_order.push(mapped.clone());

                pos
            });

        mapped.set_tiled(false);
        mapped.set_geometry(Rectangle::new(position, win_geo.size).to_global(&output));
        mapped.configure();

        if let Some(previous_geometry) = prev.or(already_mapped) {
            self.animations.insert(
                mapped.clone(),
                Animation::Tiled {
                    start: Instant::now(),
                    previous_geometry,
                },
            );
        }
        self.space.map_element(mapped, position.as_logical(), false);
        self.space.refresh();
    }

    pub fn remap_minimized(
        &mut self,
        mapped: CosmicMapped,
        from: Rectangle<i32, Local>,
        position: Point<i32, Local>,
    ) {
        let output = self.space.outputs().next().unwrap().clone();
        let layers = layer_map_for_output(&output);
        let geometry = layers.non_exclusive_zone().as_local();
        mapped.set_bounds(geometry.size.as_logical());
        let window_size = mapped.geometry().size;

        if mapped.is_maximized(false) {
            mapped.set_geometry(geometry.to_global(&output));
            mapped.configure();
        } else {
            mapped.set_geometry(Rectangle::new(
                position.to_global(&output),
                window_size.as_global(),
            ));
        }

        self.space
            .map_element(mapped.clone(), position.as_logical(), true);
        self.space.refresh();
        let target_geometry = self.space.element_geometry(&mapped).unwrap().as_local();

        self.animations.insert(
            mapped,
            Animation::Unminimize {
                start: Instant::now(),
                previous_geometry: from,
                target_geometry,
            },
        );
    }

    pub fn unmap(
        &mut self,
        window: &CosmicMapped,
        to: Option<Rectangle<i32, Local>>,
    ) -> Option<Rectangle<i32, Local>> {
        let mut mapped_geometry = self.space.element_geometry(window).map(RectExt::as_local)?;
        let _ = self.animations.remove(window);

        if let Some(to) = to {
            self.animations.insert(
                window.clone(),
                Animation::Minimize {
                    start: Instant::now(),
                    previous_geometry: if window.is_maximized(false) {
                        let output = self.space.outputs().next().unwrap();
                        let layers = layer_map_for_output(output);
                        layers.non_exclusive_zone().as_local()
                    } else {
                        mapped_geometry
                    },
                    target_geometry: to,
                },
            );
        }

        if window.floating_tiled.lock().unwrap().take().is_some() {
            if let Some(last_size) = window.last_geometry.lock().unwrap().map(|geo| geo.size) {
                let geometry = Rectangle::new(mapped_geometry.loc, last_size);
                window.set_tiled(false);
                window.set_geometry(geometry.to_global(self.space.outputs().next().unwrap()));
                window.configure();
                mapped_geometry.size = last_size;
            }
        } else if !window.is_maximized(true) {
            if window.active_window().has_pending_changes() {
                if let Some(pending_size) = window.pending_size() {
                    mapped_geometry.size = pending_size.as_local();
                }
            }
            *window.last_geometry.lock().unwrap() = Some(mapped_geometry);
        }

        self.space.unmap_elem(window);
        if let Some(pos) = self.spawn_order.iter().position(|w| w == window) {
            self.spawn_order.truncate(pos);
        }
        window.moved_since_mapped.store(true, Ordering::SeqCst);
        Some(mapped_geometry)
    }

    pub fn drop_window(
        &mut self,
        window: CosmicMapped,
        position: Point<i32, Local>,
    ) -> (CosmicMapped, Point<i32, Local>) {
        if self
            .hovered_stack
            .as_ref()
            .is_some_and(|(stack, _)| stack == &window || !stack.alive())
        {
            let _ = self.hovered_stack.take();
        }

        if let Some((mapped, geo)) = self.hovered_stack.take() {
            let stack = mapped.stack_ref().unwrap();
            for surface in window.windows().map(|s| s.0) {
                stack.add_window(surface, None, None);
            }
            (mapped, geo.loc)
        } else {
            self.map_internal(window.clone(), Some(position), None, None);
            (window, position)
        }
    }

    pub fn element_geometry(&self, elem: &CosmicMapped) -> Option<Rectangle<i32, Local>> {
        self.space.element_geometry(elem).map(RectExt::as_local)
    }

    // ─────────────────────────────────────────────────────────────────────────────
    // Embedded window hit-testing helpers
    // ─────────────────────────────────────────────────────────────────────────────

    /// Find an embedded element under the given location.
    /// Returns the element and its render location if found.
    /// Only returns an embedded element if its parent window is the topmost
    /// non-embedded window at that location (respects z-order).
    fn embedded_element_under(
        &self,
        location: Point<f64, Local>,
    ) -> Option<(&CosmicMapped, Point<i32, Logical>)> {
        // Find the topmost non-embedded window at this location for z-order checking
        let topmost_surface_id = self.topmost_parent_surface_id_at(location);

        // Now check embedded elements, but only if their parent is the topmost window
        for elem in self.space.elements() {
            if let Some(embed_info) = elem.windows().find_map(|(w, _)| {
                crate::wayland::handlers::surface_embed::get_embed_render_info(&w)
            }) {
                // Only consider this embedded window if its parent is the topmost window at this location
                if topmost_surface_id.as_ref() != Some(&embed_info.parent_surface_id) {
                    continue;
                }

                if let Some(render_location) =
                    self.calculate_embed_render_location(&embed_info, location)
                {
                    return Some((elem, render_location));
                }
            }
        }
        None
    }

    /// Calculate the render location of an embedded window if the given point is within its bounds.
    /// Returns None if the point is outside the embedded window.
    fn calculate_embed_render_location(
        &self,
        embed_info: &crate::wayland::handlers::surface_embed::EmbedRenderInfo,
        location: Point<f64, Local>,
    ) -> Option<Point<i32, Logical>> {
        let parent_geo = self
            .space
            .elements()
            .find(|e| {
                e.active_window()
                    .wl_surface()
                    .map(|s| s.id().to_string() == embed_info.parent_surface_id)
                    .unwrap_or(false)
            })
            .and_then(|parent| self.space.element_geometry(parent))?;

        let actual_geometry = embed_info
            .anchor_config
            .as_ref()
            .map(|anchor| anchor.calculate_geometry(parent_geo.size.w, parent_geo.size.h))
            .unwrap_or(embed_info.geometry);

        let render_location = parent_geo.loc + actual_geometry.loc;
        let embedded_bounds = Rectangle::new(render_location, actual_geometry.size);

        if embedded_bounds.to_f64().contains(location.as_logical()) {
            Some(render_location)
        } else {
            None
        }
    }

    /// Check if an element is embedded (has embed render info).
    fn is_embedded(elem: &CosmicMapped) -> bool {
        elem.windows().any(|(w, _)| {
            crate::wayland::handlers::surface_embed::get_embed_render_info(&w).is_some()
        })
    }

    /// Find the topmost non-embedded window at a given location.
    /// Returns the surface ID of that window if found.
    fn topmost_parent_surface_id_at(&self, location: Point<f64, Local>) -> Option<String> {
        self.space
            .elements()
            .rev()
            .filter(|e| !Self::is_embedded(e))
            .find(|e| {
                let render_location = self.space.element_location(e).unwrap() - e.geometry().loc;
                let mut bbox = e.bbox();
                bbox.loc += render_location;
                bbox.to_f64().contains(location.as_logical())
            })
            .and_then(|e| e.active_window().wl_surface().map(|s| s.id().to_string()))
    }

    pub fn popup_element_under(&self, location: Point<f64, Local>) -> Option<KeyboardFocusTarget> {
        // First check popups for embedded windows - they render on top of their embedded position
        if let Some(target) = self.embedded_popup_element_under(location) {
            return Some(target);
        }

        // Then check regular (non-embedded) windows' popups
        self.space
            .elements()
            .rev()
            .filter(|e| !Self::is_embedded(e))
            .map(|e| {
                (
                    e,
                    self.space.element_location(e).unwrap() - e.geometry().loc,
                )
            })
            .filter(|(e, render_location)| {
                let mut bbox = e.bbox();
                bbox.loc += *render_location;
                bbox.to_f64().contains(location.as_logical())
            })
            .find_map(|(e, render_location)| {
                let render_location = render_location.as_local().to_f64();
                let point = location - render_location;
                if e.focus_under(
                    point.as_logical(),
                    WindowSurfaceType::POPUP | WindowSurfaceType::SUBSURFACE,
                )
                .is_some()
                {
                    Some(e.clone().into())
                } else {
                    None
                }
            })
    }

    /// Check if the location hits a popup for any embedded window.
    /// Embedded windows render at a different location (inside their parent),
    /// so we need to check popups at the adjusted position.
    /// Only returns a popup if its parent window is the topmost non-embedded window
    /// at that location (respects z-order).
    fn embedded_popup_element_under(
        &self,
        location: Point<f64, Local>,
    ) -> Option<KeyboardFocusTarget> {
        // Find the topmost non-embedded window at this location for z-order checking
        let topmost_surface_id = self.topmost_parent_surface_id_at(location);

        // Iterate through all parent windows that have embedded children
        for parent_elem in self.space.elements() {
            let parent_window = parent_elem.active_window();
            let parent_surface = parent_window.wl_surface()?;
            let parent_surface_id = parent_surface.id().to_string();

            // Only consider this parent if it's the topmost window at this location
            if topmost_surface_id.as_ref() != Some(&parent_surface_id) {
                continue;
            }

            let embedded_children =
                crate::wayland::handlers::surface_embed::get_children_for_parent_by_surface_id(
                    &parent_surface_id,
                );

            if embedded_children.is_empty() {
                continue;
            }

            // Get parent's geometry (in Logical coordinates from Space)
            let parent_geometry = self.space.element_geometry(parent_elem)?.as_local();

            for (embedded_surface_id, embed_info) in embedded_children {
                // Find the embedded element in the space
                let embedded_elem = self.space.elements().find(|e| {
                    e.active_window()
                        .wl_surface()
                        .map(|s| s.id().to_string() == embedded_surface_id)
                        .unwrap_or(false)
                })?;

                // Calculate where the embedded window is actually rendered
                // Both parent_geometry.loc and embed_offset are now Local
                let embed_offset = Point::<i32, Local>::from((
                    embed_info.geometry.loc.x,
                    embed_info.geometry.loc.y,
                ));
                let render_location = parent_geometry.loc + embed_offset;

                // Check if the location hits any popup of this embedded window
                let point = location - render_location.to_f64();

                if embedded_elem
                    .focus_under(
                        point.as_logical(),
                        WindowSurfaceType::POPUP | WindowSurfaceType::SUBSURFACE,
                    )
                    .is_some()
                {
                    return Some(embedded_elem.clone().into());
                }
            }
        }

        None
    }

    pub fn toplevel_element_under(
        &self,
        location: Point<f64, Local>,
    ) -> Option<KeyboardFocusTarget> {
        // First check embedded windows - they render on top and should get keyboard focus priority
        if let Some((elem, render_location)) = self.embedded_element_under(location) {
            // Subtract geometry offset to get the coordinate origin for focus_under,
            // same as we do for non-embedded windows
            let adjusted_render_location = render_location - elem.geometry().loc;
            let render_location_local = adjusted_render_location.as_local().to_f64();
            let point = location - render_location_local;
            if elem
                .focus_under(
                    point.as_logical(),
                    WindowSurfaceType::TOPLEVEL | WindowSurfaceType::SUBSURFACE,
                )
                .is_some()
            {
                return Some(elem.clone().into());
            }
        }

        // Then check regular (non-embedded) windows
        self.space
            .elements()
            .rev()
            .filter(|e| !Self::is_embedded(e))
            .map(|e| {
                (
                    e,
                    self.space.element_location(e).unwrap() - e.geometry().loc,
                )
            })
            .filter(|(e, render_location)| {
                let mut bbox = e.bbox();
                bbox.loc += *render_location;
                bbox.to_f64().contains(location.as_logical())
            })
            .find_map(|(e, render_location)| {
                let render_location = render_location.as_local().to_f64();
                let point = location - render_location;
                if e.focus_under(
                    point.as_logical(),
                    WindowSurfaceType::TOPLEVEL | WindowSurfaceType::SUBSURFACE,
                )
                .is_some()
                {
                    Some(e.clone().into())
                } else {
                    None
                }
            })
    }

    pub fn popup_surface_under(
        &self,
        location: Point<f64, Local>,
    ) -> Option<(PointerFocusTarget, Point<f64, Local>)> {
        // First check popups for embedded windows
        if let Some(result) = self.embedded_popup_surface_under(location) {
            return Some(result);
        }

        // Then check regular (non-embedded) windows' popups
        self.space
            .elements()
            .rev()
            .filter(|e| !Self::is_embedded(e))
            .map(|e| {
                (
                    e,
                    self.space.element_location(e).unwrap() - e.geometry().loc,
                )
            })
            .filter(|(e, render_location)| {
                let mut bbox = e.bbox();
                bbox.loc += *render_location;
                bbox.to_f64().contains(location.as_logical())
            })
            .find_map(|(e, render_location)| {
                let render_location = render_location.as_local().to_f64();
                let point = location - render_location;
                e.focus_under(
                    point.as_logical(),
                    WindowSurfaceType::POPUP | WindowSurfaceType::SUBSURFACE,
                )
                .map(|(surface, surface_offset)| {
                    (surface, render_location + surface_offset.as_local())
                })
            })
    }

    /// Check if the location hits a popup surface for any embedded window.
    /// Returns the pointer focus target and the location relative to the window.
    /// Only returns a popup if its parent window is the topmost non-embedded window
    /// at that location (respects z-order).
    fn embedded_popup_surface_under(
        &self,
        location: Point<f64, Local>,
    ) -> Option<(PointerFocusTarget, Point<f64, Local>)> {
        // Find the topmost non-embedded window at this location for z-order checking
        let topmost_surface_id = self.topmost_parent_surface_id_at(location);

        // Iterate through all parent windows that have embedded children
        for parent_elem in self.space.elements() {
            let parent_window = parent_elem.active_window();
            let parent_surface = parent_window.wl_surface()?;
            let parent_surface_id = parent_surface.id().to_string();

            // Only consider this parent if it's the topmost window at this location
            if topmost_surface_id.as_ref() != Some(&parent_surface_id) {
                continue;
            }

            let embedded_children =
                crate::wayland::handlers::surface_embed::get_children_for_parent_by_surface_id(
                    &parent_surface_id,
                );

            if embedded_children.is_empty() {
                continue;
            }

            // Get parent's geometry
            let parent_geometry = self.space.element_geometry(parent_elem)?.as_local();

            for (embedded_surface_id, embed_info) in embedded_children {
                // Find the embedded element in the space
                let embedded_elem = self.space.elements().find(|e| {
                    e.active_window()
                        .wl_surface()
                        .map(|s| s.id().to_string() == embedded_surface_id)
                        .unwrap_or(false)
                })?;

                // Calculate where the embedded window is actually rendered
                let embed_offset = Point::<i32, Local>::from((
                    embed_info.geometry.loc.x,
                    embed_info.geometry.loc.y,
                ));
                let render_location = parent_geometry.loc + embed_offset;
                // Subtract geometry offset to get the coordinate origin for focus_under,
                // same as we do for non-embedded windows
                let adjusted_render_location =
                    render_location - embedded_elem.geometry().loc.as_local();
                let render_location_f64 = adjusted_render_location.to_f64();

                // Check if the location hits any popup of this embedded window
                let point = location - render_location_f64;

                if let Some((surface, surface_offset)) = embedded_elem.focus_under(
                    point.as_logical(),
                    WindowSurfaceType::POPUP | WindowSurfaceType::SUBSURFACE,
                ) {
                    return Some((surface, render_location_f64 + surface_offset.as_local()));
                }
            }
        }

        None
    }

    pub fn toplevel_surface_under(
        &self,
        location: Point<f64, Local>,
    ) -> Option<(PointerFocusTarget, Point<f64, Local>)> {
        // First check embedded windows - they render on top and should get priority
        if let Some((elem, render_location)) = self.embedded_element_under(location) {
            // Subtract geometry offset to get the coordinate origin for focus_under,
            // same as we do for non-embedded windows
            let adjusted_render_location = render_location - elem.geometry().loc;
            let render_location_local = adjusted_render_location.as_local().to_f64();
            let point = location - render_location_local;
            if let Some((surface, surface_offset)) = elem.focus_under(
                point.as_logical(),
                WindowSurfaceType::TOPLEVEL | WindowSurfaceType::SUBSURFACE,
            ) {
                return Some((surface, render_location_local + surface_offset.as_local()));
            }
        }

        // Then check regular (non-embedded) windows
        self.space
            .elements()
            .rev()
            .filter(|e| !Self::is_embedded(e))
            .map(|e| {
                (
                    e,
                    self.space.element_location(e).unwrap() - e.geometry().loc,
                )
            })
            .filter(|(e, render_location)| {
                let mut bbox = e.bbox();
                bbox.loc += *render_location;
                bbox.to_f64().contains(location.as_logical())
            })
            .find_map(|(e, render_location)| {
                let render_location = render_location.as_local().to_f64();
                let point = location - render_location;
                e.focus_under(
                    point.as_logical(),
                    WindowSurfaceType::TOPLEVEL | WindowSurfaceType::SUBSURFACE,
                )
                .map(|(surface, surface_offset)| {
                    (surface, render_location + surface_offset.as_local())
                })
            })
    }

    pub fn update_pointer_position(&mut self, location: Option<Point<f64, Local>>) {
        let Some(location) = location else {
            self.hovered_stack.take();
            return;
        };

        let res = self
            .space
            .element_under(location.as_logical())
            .map(|(mapped, p)| (mapped.clone(), p.as_local()));

        if let Some((mapped, _)) = res.as_ref() {
            let geometry = self.space.element_geometry(mapped).unwrap();
            let offset = location.y.round() as i32 - geometry.loc.y;
            if mapped.is_stack() && offset.is_positive() && offset <= TAB_HEIGHT {
                self.hovered_stack = Some((mapped.clone(), geometry.as_local()));
            } else {
                self.hovered_stack.take();
            }
        } else {
            self.hovered_stack.take();
        }
    }

    pub fn stacking_indicator(&self) -> Option<Rectangle<i32, Local>> {
        self.hovered_stack.as_ref().map(|(_, geo)| *geo)
    }

    pub fn resize_request(
        &mut self,
        mapped: &CosmicMapped,
        seat: &Seat<State>,
        start_data: GrabStartData,
        edges: ResizeEdge,
        edge_snap_threshold: u32,
        release: ReleaseMode,
    ) -> Option<ResizeSurfaceGrab> {
        if seat.get_pointer().is_some() {
            let location = self.space.element_location(mapped)?.as_local();
            let size = mapped.geometry().size;
            mapped.moved_since_mapped.store(true, Ordering::SeqCst);

            Some(grabs::ResizeSurfaceGrab::new(
                start_data,
                mapped.clone(),
                edges,
                self.space.outputs().next().cloned().unwrap(),
                edge_snap_threshold,
                location,
                size,
                seat,
                release,
            ))
        } else {
            None
        }
    }

    pub fn resize(
        &mut self,
        focused: &KeyboardFocusTarget,
        direction: ResizeDirection,
        edge: ResizeEdge,
        amount: i32,
    ) -> bool {
        let Some(toplevel) = focused.toplevel() else {
            return false;
        };
        let Some(mapped) = self
            .space
            .elements()
            .find(|m| m.has_surface(&toplevel, WindowSurfaceType::TOPLEVEL))
        else {
            return false;
        };
        if mapped.is_maximized(true) {
            return false;
        }

        let Some(original_geo) = self.space.element_geometry(mapped) else {
            return false; // we don't have that window
        };
        let mut geo = original_geo;

        if edge.contains(ResizeEdge::RIGHT) || edge.contains(ResizeEdge::LEFT) {
            if direction == ResizeDirection::Inwards {
                geo.size.w = (geo.size.w as u32).saturating_sub(amount as u32) as i32;
            } else {
                geo.size.w += amount;
            }
            if edge.contains(ResizeEdge::LEFT) {
                if direction == ResizeDirection::Inwards {
                    geo.loc.x += amount;
                } else {
                    geo.loc.x = (geo.loc.x as u32).saturating_sub(amount as u32) as i32;
                }
            }
        }
        if edge.contains(ResizeEdge::BOTTOM) || edge.contains(ResizeEdge::TOP) {
            if direction == ResizeDirection::Inwards {
                geo.size.h = (geo.size.h as u32).saturating_sub(amount as u32) as i32;
            } else {
                geo.size.h += amount;
            }
            if edge.contains(ResizeEdge::TOP) {
                if direction == ResizeDirection::Inwards {
                    geo.loc.y += amount;
                } else {
                    geo.loc.y = (geo.loc.y as u32).saturating_sub(amount as u32) as i32;
                }
            }
        }

        let bounding_box = self
            .space
            .output_geometry(self.space.outputs().next().unwrap())
            .unwrap();
        let (min_size, max_size) = (mapped.min_size(), mapped.max_size());
        let min_width = min_size.map(|s| s.w).unwrap_or(360);
        let min_height = min_size.map(|s| s.h).unwrap_or(240);
        let max_width = max_size.map(|s| s.w).unwrap_or(i32::MAX);
        let max_height = max_size.map(|s| s.h).unwrap_or(i32::MAX);

        geo.size.w = min_width.max(geo.size.w).min(max_width);
        geo.size.h = min_height.max(geo.size.h).min(max_height);
        geo = geo.intersection(bounding_box).unwrap();

        *mapped.resize_state.lock().unwrap() = Some(ResizeState::Resizing(ResizeData {
            edges: edge,
            initial_window_location: original_geo.loc.as_local(),
            initial_window_size: original_geo.size,
        }));

        mapped.moved_since_mapped.store(true, Ordering::SeqCst);
        mapped.set_resizing(true);
        mapped.set_geometry(
            geo.as_local()
                .to_global(self.space.outputs().next().unwrap()),
        );
        if mapped.latest_size_committed() {
            mapped.configure();
        }

        true
    }

    pub fn toggle_stacking(
        &mut self,
        mapped: &CosmicMapped,
        mut focus_stack: FocusStackMut,
    ) -> Option<KeyboardFocusTarget> {
        if !self.space.elements().any(|m| m == mapped) {
            return None;
        }

        let output = self.space.outputs().next().unwrap().clone();
        let mut mapped = mapped.clone();
        let geo = self.space.element_geometry(&mapped).unwrap();
        let location = geo.loc;

        if mapped.is_window() {
            // if it is just a window
            self.space.unmap_elem(&mapped);
            mapped.convert_to_stack(
                (&output, mapped.bbox()),
                self.theme.clone(),
                self.appearance,
            );
            self.map_internal(
                mapped.clone(),
                Some(location.as_local()),
                Some(geo.size),
                None,
            );
            focus_stack.append(mapped.clone());
            Some(KeyboardFocusTarget::Element(mapped))
        } else {
            // if we have a stack
            let mut surfaces = mapped.windows().map(|(s, _)| s).collect::<VecDeque<_>>();
            let first = surfaces.pop_front().expect("Stack without a window?");
            let focused = mapped.active_window();

            self.space.unmap_elem(&mapped);
            let handle = mapped.loop_handle();
            mapped.convert_to_surface(
                first,
                (&output, mapped.bbox()),
                self.theme.clone(),
                self.appearance,
            );
            let mut new_elements = vec![mapped.clone()];

            // map the rest
            for other in surfaces {
                other.try_force_undecorated(false);
                other.set_tiled(false);
                let focused = other == focused;
                let window = CosmicMapped::from(CosmicWindow::new(
                    other,
                    handle.clone(),
                    self.theme.clone(),
                    self.appearance,
                ));
                window.output_enter(&output, window.bbox());

                {
                    let layer_map = layer_map_for_output(&output);
                    window.set_bounds(layer_map.non_exclusive_zone().size);
                }

                if focused {
                    new_elements.insert(0, window.clone());
                } else {
                    new_elements.push(window.clone());
                }
                self.map(window, None);
            }
            self.space.map_element(mapped.clone(), location, false);
            self.space.refresh();

            for elem in new_elements.into_iter().rev() {
                focus_stack.append(elem);
            }

            Some(KeyboardFocusTarget::Element(mapped))
        }
    }

    pub fn toggle_stacking_focused(
        &mut self,
        seat: &Seat<State>,
        focus_stack: FocusStackMut,
    ) -> Option<KeyboardFocusTarget> {
        let Some(KeyboardFocusTarget::Element(elem)) = seat.get_keyboard().unwrap().current_focus()
        else {
            return None;
        };

        self.toggle_stacking(&elem, focus_stack)
    }

    pub fn move_element(
        &mut self,
        direction: Direction,
        seat: &Seat<State>,
        layer: ManagedLayer,
        theme: &cosmic::Theme,
        element: &CosmicMapped,
    ) -> MoveResult {
        match element.handle_move(direction) {
            StackMoveResult::Handled => MoveResult::Done,
            StackMoveResult::MoveOut(surface, loop_handle) => {
                let mapped: CosmicMapped =
                    CosmicWindow::new(surface, loop_handle, theme.clone(), self.appearance).into();
                let output = seat.active_output();
                let pos = self.space.element_geometry(element).unwrap().loc
                    + match direction {
                        Direction::Up => Point::from((5, -10)),
                        Direction::Down => Point::from((5, 10)),
                        Direction::Left => Point::from((-10, 5)),
                        Direction::Right => Point::from((10, 5)),
                    };
                let position = self
                    .space
                    .output_geometry(&output)
                    .unwrap()
                    .overlaps({
                        let mut geo = mapped.geometry();
                        geo.loc += pos;
                        geo
                    })
                    .then_some(pos);

                self.map_internal(mapped.clone(), position.map(PointExt::as_local), None, None);
                MoveResult::ShiftFocus(KeyboardFocusTarget::Element(mapped))
            }
            StackMoveResult::Default => {
                let mut tiled_state = element.floating_tiled.lock().unwrap();

                let output = self.space.outputs().next().unwrap().clone();
                let layers = layer_map_for_output(&output);
                let output_geometry = layers.non_exclusive_zone();
                std::mem::drop(layers);

                let current_geometry = self
                    .space
                    .element_geometry(element)
                    .map(RectExt::as_local)
                    .unwrap();
                let start_rectangle = if let Some(anim) = self.animations.remove(element) {
                    anim.geometry(
                        output_geometry,
                        current_geometry,
                        tiled_state.as_ref(),
                        self.gaps(),
                    )
                } else {
                    current_geometry
                };

                let new_state = match (direction, &*tiled_state) {
                    // figure out if we are moving between workspaces/outputs
                    (
                        Direction::Up,
                        Some(TiledCorners::Top)
                        | Some(TiledCorners::TopLeft)
                        | Some(TiledCorners::TopRight),
                    )
                    | (
                        Direction::Down,
                        Some(TiledCorners::Bottom)
                        | Some(TiledCorners::BottomLeft)
                        | Some(TiledCorners::BottomRight),
                    )
                    | (
                        Direction::Left,
                        Some(TiledCorners::Left)
                        | Some(TiledCorners::TopLeft)
                        | Some(TiledCorners::BottomLeft),
                    )
                    | (
                        Direction::Right,
                        Some(TiledCorners::Right)
                        | Some(TiledCorners::TopRight)
                        | Some(TiledCorners::BottomRight),
                    ) => {
                        return MoveResult::MoveFurther(KeyboardFocusTarget::Element(
                            element.clone(),
                        ));
                    }

                    // to we go maximized?
                    (Direction::Up, Some(TiledCorners::Bottom))
                    | (Direction::Down, Some(TiledCorners::Top))
                    | (Direction::Left, Some(TiledCorners::Right))
                    | (Direction::Right, Some(TiledCorners::Left)) => {
                        std::mem::drop(tiled_state);

                        let mut maximized_state = element.maximized_state.lock().unwrap();
                        *maximized_state = Some(MaximizedState {
                            original_geometry: start_rectangle,
                            original_layer: layer,
                        });
                        std::mem::drop(maximized_state);

                        self.map_maximized(element.clone(), start_rectangle, true);
                        return MoveResult::Done;
                    }

                    // figure out if we need to quater tile
                    (Direction::Up, Some(TiledCorners::Left))
                    | (Direction::Left, Some(TiledCorners::Top)) => TiledCorners::TopLeft,
                    (Direction::Right, Some(TiledCorners::Top))
                    | (Direction::Up, Some(TiledCorners::Right)) => TiledCorners::TopRight,
                    (Direction::Down, Some(TiledCorners::Left))
                    | (Direction::Left, Some(TiledCorners::Bottom)) => TiledCorners::BottomLeft,
                    (Direction::Right, Some(TiledCorners::Bottom))
                    | (Direction::Down, Some(TiledCorners::Right)) => TiledCorners::BottomRight,
                    // figure out if we need to extend a quater tile
                    (Direction::Up, Some(TiledCorners::BottomLeft))
                    | (Direction::Down, Some(TiledCorners::TopLeft)) => TiledCorners::Left,
                    (Direction::Up, Some(TiledCorners::BottomRight))
                    | (Direction::Down, Some(TiledCorners::TopRight)) => TiledCorners::Right,
                    (Direction::Left, Some(TiledCorners::TopRight))
                    | (Direction::Right, Some(TiledCorners::TopLeft)) => TiledCorners::Top,
                    (Direction::Left, Some(TiledCorners::BottomRight))
                    | (Direction::Right, Some(TiledCorners::BottomLeft)) => TiledCorners::Bottom,
                    // else we have a simple case
                    (Direction::Up, _) => TiledCorners::Top,
                    (Direction::Right, _) => TiledCorners::Right,
                    (Direction::Down, _) => TiledCorners::Bottom,
                    (Direction::Left, _) => TiledCorners::Left,
                };

                let new_geo = new_state.relative_geometry(output_geometry, self.gaps());
                let (new_pos, new_size) = (new_geo.loc, new_geo.size);
                element.set_tiled(true); // TODO: More fine grained?
                element.set_maximized(false);

                if tiled_state.is_none() {
                    let last_geometry = element
                        .maximized_state
                        .lock()
                        .unwrap()
                        .take()
                        .map(|state| state.original_geometry)
                        .or_else(|| self.space.element_geometry(element).map(RectExt::as_local));

                    *element.last_geometry.lock().unwrap() = last_geometry;
                }

                *tiled_state = Some(new_state);
                std::mem::drop(tiled_state);

                element.moved_since_mapped.store(true, Ordering::SeqCst);
                let element = element.clone();
                self.map_internal(
                    element,
                    Some(new_pos),
                    Some(new_size.as_logical()),
                    Some(start_rectangle),
                );

                MoveResult::Done
            }
        }
    }

    pub fn move_current_element(
        &mut self,
        direction: Direction,
        seat: &Seat<State>,
        layer: ManagedLayer,
        theme: cosmic::Theme,
    ) -> MoveResult {
        let Some(target) = seat.get_keyboard().unwrap().current_focus() else {
            return MoveResult::None;
        };

        let Some(focused) = (match target {
            KeyboardFocusTarget::Popup(popup) => {
                let Some(toplevel_surface) = (match popup {
                    PopupKind::Xdg(xdg) => get_popup_toplevel(&xdg),
                    PopupKind::InputMethod(_) => unreachable!(),
                }) else {
                    return MoveResult::None;
                };
                self.space
                    .elements()
                    .find(|elem| elem.wl_surface().as_deref() == Some(&toplevel_surface))
            }
            KeyboardFocusTarget::Element(elem) => self.space.elements().find(|x| *x == &elem),
            _ => None,
        }) else {
            return MoveResult::None;
        };

        self.move_element(direction, seat, layer, &theme, &focused.clone())
    }

    pub fn mapped(&self) -> impl Iterator<Item = &CosmicMapped> {
        self.space.elements().rev()
    }

    pub fn windows(&self) -> impl Iterator<Item = CosmicSurface> + '_ {
        self.mapped().flat_map(|e| e.windows().map(|(w, _)| w))
    }

    pub fn recalculate(&mut self) {
        let output = self.space.outputs().next().unwrap().clone();
        let output_size = output.geometry().size.as_local();
        let old_output_size = Some(self.last_output_size).filter(|size| *size != output_size);

        let geometry = layer_map_for_output(&output)
            .non_exclusive_zone()
            .as_local();

        // update elements
        for mapped in self
            .space
            .elements()
            .cloned()
            .collect::<Vec<_>>()
            .into_iter()
        {
            mapped.set_bounds(geometry.size.as_logical());
            let prev = self.space.element_geometry(&mapped).map(RectExt::as_local);

            let window_geometry = if mapped.is_maximized(false) {
                geometry
            } else {
                prev.map(|mut rect| {
                    if let Some(old_size) = old_output_size {
                        rect = Rectangle::new(
                            Point::new(
                                (rect.loc.x as f64 + rect.size.w as f64 / 2.) / old_size.w as f64
                                    * output_size.w as f64
                                    - rect.size.w as f64 / 2.,
                                (rect.loc.y as f64 + rect.size.h as f64 / 2.) / old_size.h as f64
                                    * output_size.h as f64
                                    - rect.size.h as f64 / 2.,
                            ),
                            rect.size.to_f64(),
                        )
                        .to_i32_round();
                    }
                    Rectangle::new(rect.loc.constrain(geometry), rect.size)
                })
                .unwrap_or_else(|| {
                    Rectangle::new(Point::from((0, 0)), mapped.geometry().size.as_local())
                })
            };
            mapped.set_geometry(window_geometry.to_global(&output));

            let is_activated = mapped.is_activated(false);
            mapped.configure();
            self.space
                .map_element(mapped, window_geometry.loc.as_logical(), is_activated);
        }

        self.last_output_size = output_size;
        self.refresh();
    }

    #[profiling::function]
    pub fn refresh(&mut self) {
        self.space.refresh();

        if let Some(pos) = self.spawn_order.iter().position(|w| !w.alive()) {
            self.spawn_order.truncate(pos);
        }

        // Cleanup: Check if any parent windows that had embedded children are now gone
        // If so, clear the embedded state so those windows become visible again
        self.cleanup_orphaned_embeds();

        // Update embedded window geometries based on parent window sizes
        self.update_embedded_geometries();

        for element in self
            .space
            .elements()
            .filter(|e| self.space.outputs_for_element(e).is_empty())
            .cloned()
            .collect::<Vec<_>>()
            .into_iter()
        {
            // TODO what about windows leaving to the top with no headerbar to drag? can that happen? (Probably if the user is moving outputs down)
            *element.last_geometry.lock().unwrap() = None;
            self.map_internal(element, None, None, None);
        }
    }

    /// Cleanup orphaned embedded surfaces (where parent has closed)
    /// Uses global embed registry to verify parent is still valid (works across outputs)
    fn cleanup_orphaned_embeds(&mut self) {
        // Check each embedded surface and clear if its parent is gone
        for elem in self.space.elements() {
            let surface_id: Option<String> = elem
                .active_window()
                .wl_surface()
                .map(|s| s.id().to_string());

            if let Some(ref sid) = surface_id {
                if let Some(embed_info) =
                    crate::wayland::handlers::surface_embed::get_embed_render_info(
                        &elem.active_window(),
                    )
                {
                    // This element is embedded - check if its parent still exists
                    // Check globally: parent is valid if it has embeds registered OR is being grabbed
                    let parent_valid =
                        crate::wayland::handlers::surface_embed::is_valid_embed_parent(
                            &embed_info.parent_surface_id,
                        );
                    let parent_grabbed = crate::wayland::handlers::surface_embed::is_parent_grabbed(
                        &embed_info.parent_surface_id,
                    );

                    // Only clear if parent is not found anywhere
                    if !parent_valid && !parent_grabbed {
                        tracing::info!(
                            "Parent surface '{}' no longer valid (not in global registry, not grabbed), clearing embed for surface '{}' (app_id='{}')",
                            embed_info.parent_surface_id,
                            sid,
                            embed_info.embedded_app_id
                        );
                        crate::wayland::handlers::surface_embed::unmark_surface_embedded(sid);
                    }
                }
            }
        }
    }

    /// Update embedded window geometries when parent windows resize
    fn update_embedded_geometries(&mut self) {
        // Collect parent windows and their children that need updating
        let updates: Vec<_> = self
            .space
            .elements()
            .filter_map(|elem| {
                let surface_id = elem.active_window().wl_surface()?.id().to_string();
                let geometry = self.space.element_geometry(elem)?;
                // Check if this window has any embedded children (by surface_id)
                let children =
                    crate::wayland::handlers::surface_embed::get_children_for_parent_by_surface_id(
                        &surface_id,
                    );
                if children.is_empty() {
                    return None;
                }
                Some((surface_id, geometry.size, children))
            })
            .collect();

        // For each parent, update embedded geometry and configure embedded windows
        for (parent_surface_id, parent_size, _children) in updates {
            let updated =
                crate::wayland::handlers::surface_embed::update_embedded_geometry_for_parent_by_surface_id(
                    &parent_surface_id,
                    parent_size.w,
                    parent_size.h,
                );

            // Configure embedded windows with new sizes
            for (embedded_surface_id, new_geometry) in updated {
                // Find the embedded window in the space by matching surface ID
                if let Some(embedded_elem) = self.space.elements().find(|e| {
                    e.active_window()
                        .wl_surface()
                        .map(|s| s.id().to_string() == embedded_surface_id)
                        .unwrap_or(false)
                }) {
                    // Set the embedded window's geometry to match the new calculated size
                    let global_geo = Rectangle::new(
                        (new_geometry.loc.x, new_geometry.loc.y).into(),
                        (new_geometry.size.w, new_geometry.size.h).into(),
                    );
                    embedded_elem.active_window().set_geometry(global_geo, 0);
                    embedded_elem.configure();
                    tracing::trace!(
                        "Configured embedded '{}' to new size {}x{}",
                        embedded_surface_id,
                        new_geometry.size.w,
                        new_geometry.size.h
                    );
                }
            }
        }
    }

    pub fn animations_going(&self) -> bool {
        self.dirty.swap(false, Ordering::SeqCst) || !self.animations.is_empty()
    }

    pub fn update_animation_state(&mut self) {
        let was_empty = self.animations.is_empty();

        // Get output for geometry conversion (needed for client-driven animations)
        let output = self.space.outputs().next().cloned();

        // For client-driven resize animations:
        // Calculate animated geometry and send configures at each frame.
        // The render position is calculated separately in render_elements
        // based on the client's actual buffer size.

        // Collect updates to send
        let updates: Vec<_> = self
            .animations
            .iter()
            .filter_map(|(mapped, anim)| {
                if let Animation::ClientDrivenResize {
                    start,
                    previous_geometry,
                    target_geometry,
                    last_sent_size,
                    ..
                } = anim
                {
                    // Calculate current animated geometry based on time
                    let now = Instant::now();
                    let progress = now
                        .duration_since(*start)
                        .min(ANIMATION_DURATION)
                        .as_secs_f64()
                        / ANIMATION_DURATION.as_secs_f64();

                    let current_geometry: Rectangle<i32, Local> = ease(
                        EaseInOutCubic,
                        EaseRectangle(*previous_geometry),
                        EaseRectangle(*target_geometry),
                        progress,
                    )
                    .unwrap();

                    // Check if we need to send a new configure
                    let elapsed = now.duration_since(*start);
                    let is_complete = elapsed >= ANIMATION_DURATION;
                    // Send if size changed from what we last sent
                    let size_changed = current_geometry.size != *last_sent_size;
                    let final_size_needed = is_complete && *last_sent_size != target_geometry.size;

                    if size_changed || final_size_needed {
                        let geometry_to_send = if is_complete {
                            *target_geometry
                        } else {
                            current_geometry
                        };
                        Some((mapped.clone(), geometry_to_send))
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect();

        // Send configures and update last_sent_size, also update embedded children with lookahead
        if let Some(ref output) = output {
            for (mapped, geometry) in &updates {
                mapped.set_geometry(geometry.to_global(output));
                mapped.force_configure();

                // Update last_sent_size and get animation info for embedded lookahead
                let (previous_geometry, target_geometry) =
                    if let Some(Animation::ClientDrivenResize {
                        last_sent_size,
                        previous_geometry,
                        target_geometry,
                        ..
                    }) = self.animations.get_mut(mapped)
                    {
                        *last_sent_size = geometry.size;
                        (*previous_geometry, *target_geometry)
                    } else {
                        continue;
                    };

                // Update embedded children with lookahead sizes
                self.update_embeds_with_lookahead(mapped, previous_geometry, target_geometry);
            }
        }

        // Before removing completed animations, finalize their geometry and stop embed sync
        if let Some(ref output) = output {
            let completed: Vec<_> = self
                .animations
                .iter()
                .filter_map(|(mapped, anim)| {
                    let duration = match anim {
                        Animation::Tiled { .. } | Animation::ClientDrivenResize { .. } => {
                            ANIMATION_DURATION
                        }
                        _ => MINIMIZE_ANIMATION_DURATION,
                    };
                    if Instant::now().duration_since(*anim.start()) >= duration {
                        // For client-driven animations, get the target geometry
                        if let Animation::ClientDrivenResize {
                            target_geometry, ..
                        } = anim
                        {
                            Some((mapped.clone(), *target_geometry))
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                })
                .collect();

            for (mapped, target_geometry) in completed {
                // Update the element's geometry to the final target
                mapped.set_geometry(target_geometry.to_global(output));
                // Also update the space's element location
                self.space
                    .map_element(mapped.clone(), target_geometry.loc.as_logical(), false);

                // Stop animation sync for embedded children
                let parent_surface_id = mapped
                    .active_window()
                    .wl_surface()
                    .map(|s| s.id().to_string());
                let embedded_children = parent_surface_id
                    .map(|sid| crate::wayland::handlers::surface_embed::get_children_for_parent_by_surface_id(&sid))
                    .unwrap_or_default();
                for (embedded_surface_id, _) in embedded_children {
                    crate::wayland::handlers::surface_embed::stop_embed_animation_sync(
                        &embedded_surface_id,
                    );
                }
            }
        }

        self.animations.retain(|_, anim| {
            let duration = match anim {
                Animation::Tiled { .. } | Animation::ClientDrivenResize { .. } => {
                    ANIMATION_DURATION
                }
                _ => MINIMIZE_ANIMATION_DURATION,
            };
            Instant::now().duration_since(*anim.start()) < duration
        });
        if self.animations.is_empty() != was_empty {
            self.dirty.store(true, Ordering::SeqCst);
        }
    }

    pub fn merge(&mut self, other: FloatingLayout) {
        for element in other.space.elements() {
            let elem_loc = other
                .space
                .element_geometry(element)
                .unwrap()
                .loc
                .as_local();
            self.map_internal(element.clone(), Some(elem_loc), None, None);
        }
        self.refresh(); //fixup any out of bounds elements
    }

    /// Check if any windows in this layout have blur enabled
    pub fn has_blur_windows(&self) -> bool {
        self.space.elements().any(|elem| elem.has_blur())
    }

    /// Get blur windows in Z-order (bottom to top) with their keys
    /// Returns (window_key, geometry, alpha, global_z_index) tuples
    /// global_z_index is the position among ALL windows where 0 = bottom and N-1 = top
    pub fn blur_windows_ordered(
        &self,
        alpha: f32,
    ) -> Vec<(CosmicMappedKey, Rectangle<i32, Local>, f32, usize)> {
        if self.space.outputs().next().is_none() {
            return Vec::new();
        }

        // Count minimizing animations and space elements to get total window count
        let minimizing_count = self
            .animations
            .iter()
            .filter(|(_, anim)| matches!(anim, Animation::Minimize { .. }))
            .count();
        let total_count = minimizing_count + self.space.elements().count();

        if total_count == 0 {
            return Vec::new();
        }

        self.animations
            .iter()
            .filter(|(_, anim)| matches!(anim, Animation::Minimize { .. }))
            .map(|(elem, _)| elem)
            .chain(self.space.elements().rev())
            .enumerate()
            .filter(|(_, elem)| elem.has_blur())
            .filter_map(|(front_to_back_idx, elem)| {
                // Convert front-to-back index to back-to-front z-index
                // Index 0 in iteration = topmost window = z-index (total-1)
                // Index (total-1) in iteration = bottom window = z-index 0
                let global_z_idx = total_count - 1 - front_to_back_idx;

                let anim_opt = self.animations.get(elem);
                let (geometry, elem_alpha) = if let Some(anim) = anim_opt {
                    (*anim.previous_geometry(), alpha * anim.alpha())
                } else {
                    let geo = self.space.element_geometry(elem)?;
                    (geo.as_local(), alpha)
                };
                Some((elem.key(), geometry, elem_alpha, global_z_idx))
            })
            .collect()
    }

    /// Get blur windows grouped by shared capture requirements.
    /// Consecutive blur windows (no non-blur windows between them) share a capture.
    /// This optimizes rendering by reducing the number of scene captures needed.
    ///
    /// Example:
    /// - Windows: [non-blur z=0, blur z=1, blur z=2, non-blur z=3, blur z=4]
    /// - Groups: [{threshold=1, windows=[z=1,z=2]}, {threshold=4, windows=[z=4]}]
    /// - Only 2 captures needed instead of 3 (when windows don't overlap)
    ///
    /// When consecutive blur windows OVERLAP geometrically, they need separate groups
    /// because the top window needs to capture the bottom window in its blur.
    pub fn blur_windows_grouped(&self, alpha: f32) -> Vec<BlurWindowGroup> {
        if self.space.outputs().next().is_none() {
            return Vec::new();
        }

        // Count minimizing animations and space elements to get total window count
        let minimizing_count = self
            .animations
            .iter()
            .filter(|(_, anim)| matches!(anim, Animation::Minimize { .. }))
            .count();
        let space_element_count = self.space.elements().count();
        let total_count = minimizing_count + space_element_count;

        // Debug: Log window counts to track z-order stability
        tracing::debug!(
            minimizing_count,
            space_element_count,
            total_count,
            "blur_windows_grouped: window counts"
        );

        if total_count == 0 {
            return Vec::new();
        }

        // Collect all windows with their blur status and z-index
        // We need to track non-blur windows to detect gaps between blur windows
        let all_windows: Vec<(
            Option<(CosmicMappedKey, Rectangle<i32, Local>, f32)>,
            usize,
            bool,
        )> = self
            .animations
            .iter()
            .filter(|(_, anim)| matches!(anim, Animation::Minimize { .. }))
            .map(|(elem, _)| elem)
            .chain(self.space.elements().rev())
            .enumerate()
            .filter_map(|(front_to_back_idx, elem)| {
                let global_z_idx = total_count - 1 - front_to_back_idx;
                let has_blur = elem.has_blur();

                if has_blur {
                    let anim_opt = self.animations.get(elem);
                    let (geometry, elem_alpha) = if let Some(anim) = anim_opt {
                        (*anim.previous_geometry(), alpha * anim.alpha())
                    } else {
                        let geo = self.space.element_geometry(elem)?;
                        (geo.as_local(), alpha)
                    };
                    Some((Some((elem.key(), geometry, elem_alpha)), global_z_idx, true))
                } else {
                    // Non-blur window - we just need to track its position
                    Some((None, global_z_idx, false))
                }
            })
            .collect();

        // Group consecutive blur windows that don't overlap
        // If windows overlap, the top one needs to see the bottom one in its blur
        let mut groups: Vec<BlurWindowGroup> = Vec::new();
        let mut current_group: Option<BlurWindowGroup> = None;
        let mut last_z_idx: Option<usize> = None;

        // Sort by z-index (ascending = bottom to top)
        let mut sorted_windows = all_windows;
        sorted_windows.sort_by_key(|(_, z_idx, _)| *z_idx);

        for (window_data, z_idx, is_blur) in sorted_windows {
            if is_blur {
                if let Some((key, geometry, elem_alpha)) = window_data {
                    // Check if this blur window is consecutive with the previous
                    let is_consecutive = last_z_idx.map(|last| z_idx == last + 1).unwrap_or(true);

                    // Check if this window overlaps with any window in the current group
                    let overlaps_with_group = current_group
                        .as_ref()
                        .map(|group| {
                            group.windows.iter().any(|(_, group_geo, _, _)| {
                                // Check if rectangles intersect
                                geometry.overlaps(*group_geo)
                            })
                        })
                        .unwrap_or(false);

                    if is_consecutive && !overlaps_with_group {
                        // Add to current group or start new one
                        if let Some(ref mut group) = current_group {
                            group.windows.push((key, geometry, elem_alpha, z_idx));
                        } else {
                            current_group = Some(BlurWindowGroup {
                                capture_z_threshold: z_idx,
                                windows: vec![(key, geometry, elem_alpha, z_idx)],
                            });
                        }
                    } else {
                        // Gap detected OR windows overlap - finish current group and start new one
                        if let Some(group) = current_group.take() {
                            groups.push(group);
                        }
                        current_group = Some(BlurWindowGroup {
                            capture_z_threshold: z_idx,
                            windows: vec![(key, geometry, elem_alpha, z_idx)],
                        });
                    }
                }
            } else {
                // Non-blur window creates a gap - finish current group
                if let Some(group) = current_group.take() {
                    groups.push(group);
                }
            }
            last_z_idx = Some(z_idx);
        }

        // Don't forget the last group
        if let Some(group) = current_group {
            groups.push(group);
        }

        groups
    }

    /// Get the geometries of all windows that have blur enabled
    /// Returns (geometry, alpha) tuples
    pub fn blur_window_geometries(&self, alpha: f32) -> Vec<(Rectangle<i32, Local>, f32)> {
        if self.space.outputs().next().is_none() {
            return Vec::new();
        }

        self.animations
            .iter()
            .filter(|(_, anim)| matches!(anim, Animation::Minimize { .. }))
            .map(|(elem, _)| elem)
            .chain(self.space.elements().rev())
            .filter(|elem| elem.has_blur())
            .filter_map(|elem| {
                let anim_opt = self.animations.get(elem);
                let (geometry, elem_alpha) = if let Some(anim) = anim_opt {
                    (*anim.previous_geometry(), alpha * anim.alpha())
                } else {
                    let geo = self.space.element_geometry(elem)?;
                    (geo.as_local(), alpha)
                };
                Some((geometry, elem_alpha))
            })
            .collect()
    }

    #[profiling::function]
    pub fn render_popups<R>(
        &self,
        renderer: &mut R,
        alpha: f32,
    ) -> Vec<CosmicMappedRenderElement<R>>
    where
        R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
        R::TextureId: Send + Clone + 'static,
        CosmicMappedRenderElement<R>: RenderElement<R>,
        CosmicWindowRenderElement<R>: RenderElement<R>,
        CosmicStackRenderElement<R>: RenderElement<R>,
    {
        let output = self.space.outputs().next().unwrap();
        let output_scale = output.current_scale().fractional_scale();

        let mut elements = Vec::default();

        for elem in self
            .animations
            .iter()
            .filter(|(_, anim)| matches!(anim, Animation::Minimize { .. }))
            .map(|(elem, _)| elem)
            .chain(self.space.elements().rev())
        {
            // Check if this is an embedded window
            let is_embedded = elem
                .windows()
                .any(|(w, _)| crate::wayland::handlers::surface_embed::is_surface_embedded(&w));

            if is_embedded {
                // For embedded windows, we need to render popups at the embedded position
                // (inside the parent window), not at the embedded window's workspace position
                let embedded_popup_elements =
                    self.render_embedded_popups(renderer, elem, output_scale, alpha);
                elements.extend(embedded_popup_elements);
            } else {
                // Normal window - render popups at workspace position
                let (geometry, alpha) = self
                    .animations
                    .get(elem)
                    .map(|anim| (*anim.previous_geometry(), alpha * anim.alpha()))
                    .unwrap_or_else(|| {
                        (self.space.element_geometry(elem).unwrap().as_local(), alpha)
                    });

                let render_location = geometry.loc - elem.geometry().loc.as_local();
                elements.extend(
                    elem.popup_render_elements(
                        renderer,
                        render_location
                            .as_logical()
                            .to_physical_precise_round(output_scale),
                        output_scale.into(),
                        alpha,
                    ),
                );
            }
        }

        elements
    }

    #[profiling::function]
    pub fn render<R>(
        &self,
        renderer: &mut R,
        focused: Option<&CosmicMapped>,
        mut resize_indicator: Option<(ResizeMode, ResizeIndicator)>,
        indicator_thickness: u8,
        alpha: f32,
        theme: &cosmic::theme::CosmicTheme,
        element_filter: ElementFilter,
        attached_orb_state: Option<&VoiceOrbState>,
    ) -> Vec<CosmicMappedRenderElement<R>>
    where
        R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
        R::TextureId: Send + Clone + 'static,
        CosmicMappedRenderElement<R>: RenderElement<R>,
        CosmicWindowRenderElement<R>: RenderElement<R>,
        CosmicStackRenderElement<R>: RenderElement<R>,
    {
        let output = self.space.outputs().next().unwrap();
        let output_geometry = {
            let layers = layer_map_for_output(output);
            layers.non_exclusive_zone()
        };
        let output_scale = output.current_scale().fractional_scale();

        let mut elements = Vec::default();

        // Extract blur capture context if present
        let blur_ctx = match &element_filter {
            ElementFilter::BlurCapture(ctx) => Some(ctx),
            _ => None,
        };

        // Count total windows for z-index calculation
        let minimizing_count = self
            .animations
            .iter()
            .filter(|(_, anim)| matches!(anim, Animation::Minimize { .. }))
            .count();
        let total_window_count = minimizing_count + self.space.elements().count();

        // Iterate front-to-back (topmost first) using enumerate to track iteration index
        for (front_to_back_idx, elem) in self
            .animations
            .iter()
            .filter(|(_, anim)| matches!(anim, Animation::Minimize { .. }))
            .map(|(elem, _)| elem)
            .chain(self.space.elements().rev())
            .enumerate()
        {
            // Check if this is an embedded window - if so, get the embed render info
            let embed_info = elem.windows().find_map(|(w, _)| {
                crate::wayland::handlers::surface_embed::get_embed_render_info(&w)
            });

            // For now, skip embedded windows - we'll render them separately
            // TODO: Render embedded windows at their parent position + offset
            if embed_info.is_some() {
                tracing::info!(
                    app_id = %elem.active_window().app_id(),
                    "Skipping embedded window from floating render (will render at parent)"
                );
                continue;
            }

            // Check if this specific surface has a pending embed (hide until embedded)
            // This prevents the "flash" of window appearing before embed is fulfilled
            // We use surface_id (unique per window) instead of app_id to avoid hiding
            // multiple windows of the same app (only the one being embedded should hide)
            if let Some(surface_id) = elem
                .active_window()
                .wl_surface()
                .map(|s| s.id().to_string())
            {
                if crate::wayland::handlers::surface_embed::is_surface_id_pending_embed(&surface_id)
                {
                    tracing::debug!(
                        app_id = %elem.active_window().app_id(),
                        surface_id = %surface_id,
                        "Hiding window - pending embed for this surface"
                    );
                    continue;
                }
            }

            // Convert front-to-back index to back-to-front z-index
            // Index 0 in iteration = topmost window = z-index (total-1)
            // Index (total-1) in iteration = bottom window = z-index 0
            let z_idx = if total_window_count > 0 {
                total_window_count - 1 - front_to_back_idx
            } else {
                0
            };

            // When capturing background for blur (iterative multi-pass):
            // - Skip grabbed/dragged windows (they're rendered on top and shouldn't blur themselves)
            // - Skip windows at or above the z-index threshold (blur window and everything above)
            // - For final render (not skipping backdrops), render all windows normally
            if let Some(ctx) = blur_ctx {
                // Skip grabbed/dragged window - it's always on top
                if ctx.is_window_grabbed(elem) {
                    tracing::debug!(
                        window_class = %elem.active_window().app_id(),
                        z_idx = z_idx,
                        "Skipping grabbed window during blur capture"
                    );
                    continue;
                }

                // Check if this window is at or above the z-index threshold
                if ctx.is_z_index_excluded(z_idx) {
                    tracing::debug!(
                        window_class = %elem.active_window().app_id(),
                        z_idx = z_idx,
                        "Excluding window during blur capture (z-index threshold)"
                    );
                    continue;
                }

                // This window WILL be rendered in blur capture
                tracing::debug!(
                    window_class = %elem.active_window().app_id(),
                    z_idx = z_idx,
                    "Including window in blur capture"
                );
            }

            let anim_opt = self.animations.get(elem);
            let (mut geometry, alpha) = anim_opt
                .map(|anim| {
                    // For client-driven animations, calculate render position based on
                    // the client's ACTUAL buffer size. This keeps position and buffer
                    // perfectly in sync regardless of how many frames the client takes.
                    let geo = if anim.is_client_driven() {
                        // Get the client's actual committed buffer size
                        let buffer_size = elem.geometry().size;
                        anim.render_geometry_for_buffer_size(buffer_size)
                            .unwrap_or(*anim.previous_geometry())
                    } else {
                        *anim.previous_geometry()
                    };
                    (geo, alpha * anim.alpha())
                })
                .unwrap_or_else(|| (self.space.element_geometry(elem).unwrap().as_local(), alpha));

            // For client-driven animations, use the calculated geometry location directly
            // (which was computed from the actual buffer size)
            let render_location = if anim_opt.map(|a| a.is_client_driven()).unwrap_or(false) {
                // Use the buffer-based geometry position directly
                geometry.loc
            } else {
                geometry.loc - elem.geometry().loc.as_local()
            };

            let mut window_elements = elem.render_elements(
                renderer,
                render_location
                    .as_logical()
                    .to_physical_precise_round(output_scale),
                None,
                output_scale.into(),
                alpha,
                None,
            );
            window_elements.extend(
                elem.shadow_render_element(
                    renderer,
                    render_location
                        .as_logical()
                        .to_physical_precise_round(output_scale),
                    None,
                    output_scale.into(),
                    1.,
                    alpha,
                ),
            );

            // Track animation info for later use (blur needs to be added before rescaling for minimize)
            // Tuple: (original_geo, scale, relocation, buffer_size)
            let minimize_anim_info: Option<(
                Rectangle<i32, Local>,
                Scale<f64>,
                Point<i32, Physical>,
                Size<i32, Local>,
            )> = if let Some(anim) = anim_opt {
                if !anim.is_client_driven()
                    && matches!(
                        anim,
                        Animation::Minimize { .. } | Animation::Unminimize { .. }
                    )
                {
                    let original_geo = anim.previous_geometry();
                    let target_geometry = anim.geometry(
                        output_geometry,
                        self.space
                            .element_geometry(elem)
                            .map(RectExt::as_local)
                            .unwrap_or(geometry),
                        elem.floating_tiled.lock().unwrap().as_ref(),
                        self.gaps(),
                    );

                    let buffer_size = elem.geometry().size.as_local();

                    // Use uniform scaling to maintain aspect ratio
                    let scale_x = target_geometry.size.w as f64 / buffer_size.w as f64;
                    let scale_y = target_geometry.size.h as f64 / buffer_size.h as f64;
                    let uniform_scale = scale_x.min(scale_y);

                    // Calculate centering offset
                    let scaled_w = (buffer_size.w as f64 * uniform_scale) as i32;
                    let scaled_h = (buffer_size.h as f64 * uniform_scale) as i32;
                    let offset_x = (target_geometry.size.w - scaled_w) / 2;
                    let offset_y = (target_geometry.size.h - scaled_h) / 2;

                    let scale = Scale {
                        x: uniform_scale,
                        y: uniform_scale,
                    };
                    let relocation = (target_geometry.loc - original_geo.loc
                        + Point::from((offset_x, offset_y)).as_local())
                    .as_logical()
                    .to_physical_precise_round(output_scale);

                    Some((*original_geo, scale, relocation, buffer_size))
                } else {
                    None
                }
            } else {
                None
            };

            // Add blur backdrop for windows that request KDE blur (independent of focus state)
            // Design spec: background: rgba(255, 255, 255, 0.10), backdrop-filter: blur(50px)
            // Skip adding backdrop if we're capturing background for blur
            if elem.has_blur() && blur_ctx.is_none() {
                // For minimize/unminimize animation, calculate the scaled blur geometry
                // Use buffer_size (actual window size) not original_geo (which could be minimized size for unminimize)
                let blur_geometry = if let Some((_original_geo, scale, _relocation, buffer_size)) =
                    &minimize_anim_info
                {
                    // Calculate the animated size for blur based on buffer size and scale
                    let scaled_w = (buffer_size.w as f64 * scale.x) as i32;
                    let scaled_h = (buffer_size.h as f64 * scale.y) as i32;

                    // Get the target geometry from the animation (this is the interpolated position/size)
                    if let Some(anim) = anim_opt {
                        let anim_geometry = anim.geometry(
                            output_geometry,
                            self.space
                                .element_geometry(elem)
                                .map(RectExt::as_local)
                                .unwrap_or(geometry),
                            elem.floating_tiled.lock().unwrap().as_ref(),
                            self.gaps(),
                        );

                        tracing::debug!(
                            buffer_w = buffer_size.w,
                            buffer_h = buffer_size.h,
                            scale_x = scale.x,
                            scale_y = scale.y,
                            scaled_w = scaled_w,
                            scaled_h = scaled_h,
                            anim_geo_x = anim_geometry.loc.x,
                            anim_geo_y = anim_geometry.loc.y,
                            anim_geo_w = anim_geometry.size.w,
                            anim_geo_h = anim_geometry.size.h,
                            "Minimize/unminimize blur geometry calculation"
                        );

                        // Center the scaled blur within the animation geometry
                        let offset_x = (anim_geometry.size.w - scaled_w) / 2;
                        let offset_y = (anim_geometry.size.h - scaled_h) / 2;
                        Rectangle::new(
                            Point::from((
                                anim_geometry.loc.x + offset_x,
                                anim_geometry.loc.y + offset_y,
                            )),
                            Size::from((scaled_w, scaled_h)),
                        )
                    } else {
                        geometry
                    }
                } else {
                    geometry
                };

                let corner_radius = elem.blur_corner_radius(blur_geometry.size.as_logical());

                // Get the output name for looking up cached blur texture
                let output_name = output.name();
                let window_key = elem.key();
                let output_transform = output.current_transform();
                let output_scale = output.current_scale().fractional_scale();

                // If this window has the attached voice orb, insert it BEFORE blur
                // (In front-to-back rendering: content -> shadow -> orb -> blur)
                // This makes orb visible in front of blur but behind window content
                if let Some(orb_state) = attached_orb_state {
                    if let Some(attached_surface_id) = orb_state.attached_surface_id_for_render() {
                        // Compare window surface ID with attached surface ID for reliable matching
                        let window_surface_id = elem
                            .active_window()
                            .wl_surface()
                            .map(|s| s.id().to_string());
                        
                        if window_surface_id.as_deref() == Some(attached_surface_id) {
                            // Get output geometry for orb rendering
                            let output_geo = output.geometry().as_logical();
                            
                            // Create the voice orb element
                            if let Some(orb_element) = VoiceOrbShader::element(renderer, orb_state, output_geo) {
                                // Insert orb element before blur (behind window content, in front of blur)
                                let orb_geo = orb_element.geometry(output.current_scale().fractional_scale().into());
                                tracing::debug!(
                                    surface_id = %attached_surface_id,
                                    orb_geo_x = orb_geo.loc.x,
                                    orb_geo_y = orb_geo.loc.y,
                                    orb_geo_w = orb_geo.size.w,
                                    orb_geo_h = orb_geo.size.h,
                                    orb_scale = orb_state.scale,
                                    is_burst = orb_state.is_in_burst_phase(),
                                    shrinking_from_attached = orb_state.shrinking_from_attached,
                                    window_elements_count = window_elements.len(),
                                    "Voice orb element created for attached window"
                                );
                                window_elements.push(orb_element.into());
                            }
                        }
                    }
                }

                // Get per-window blur texture (iterative multi-pass blur)
                let blur_info = get_cached_blur_texture_for_window(&output_name, &window_key);

                if let Some(blur_info) = blur_info {
                    // Use BlurredBackdropShader with the cached blurred texture
                    let blur_backdrop = BlurredBackdropShader::element(
                        renderer,
                        &blur_info.texture,
                        blur_geometry,
                        blur_info.size,
                        blur_info.screen_size,
                        output_scale,
                        output_transform,
                        corner_radius,
                        alpha,
                        BLUR_TINT_COLOR,
                        BLUR_TINT_STRENGTH,
                        false, // No blur border for regular windows
                    );
                    window_elements.push(blur_backdrop.into());
                } else {
                    tracing::debug!(
                        output = %output_name,
                        "No cached blur texture available, using fallback"
                    );
                    // Fallback
                    let blur_backdrop = BackdropShader::element(
                        renderer,
                        Key::Window(Usage::Overlay, elem.key()),
                        blur_geometry,
                        corner_radius,
                        alpha * BLUR_FALLBACK_ALPHA,
                        BLUR_FALLBACK_COLOR,
                    );
                    window_elements.push(blur_backdrop.into());
                }
            }

            // Now apply animation transformations
            if let Some(anim) = anim_opt {
                if !anim.is_client_driven() {
                    if let Some((original_geo, scale, relocation, _buffer_size)) =
                        minimize_anim_info
                    {
                        // For minimize/unminimize: scale window elements with uniform scaling
                        // Blur is already rendered at scaled geometry above
                        window_elements = window_elements
                            .into_iter()
                            .map(|element| match element {
                                CosmicMappedRenderElement::Stack(elem) => {
                                    CosmicMappedRenderElement::MovingStack({
                                        let rescaled = RescaleRenderElement::from_element(
                                            elem,
                                            original_geo
                                                .loc
                                                .as_logical()
                                                .to_physical_precise_round(output_scale),
                                            scale,
                                        );

                                        RelocateRenderElement::from_element(
                                            rescaled,
                                            relocation,
                                            Relocate::Relative,
                                        )
                                    })
                                }
                                CosmicMappedRenderElement::Window(elem) => {
                                    CosmicMappedRenderElement::MovingWindow({
                                        let rescaled = RescaleRenderElement::from_element(
                                            elem,
                                            original_geo
                                                .loc
                                                .as_logical()
                                                .to_physical_precise_round(output_scale),
                                            scale,
                                        );

                                        RelocateRenderElement::from_element(
                                            rescaled,
                                            relocation,
                                            Relocate::Relative,
                                        )
                                    })
                                }
                                x => x,
                            })
                            .collect();
                    } else {
                        // For other compositor-driven animations (like Tiled), use per-axis scaling
                        let original_geo = anim.previous_geometry();
                        geometry = anim.geometry(
                            output_geometry,
                            self.space
                                .element_geometry(elem)
                                .map(RectExt::as_local)
                                .unwrap_or(geometry),
                            elem.floating_tiled.lock().unwrap().as_ref(),
                            self.gaps(),
                        );

                        let buffer_size = elem.geometry().size;
                        let scale = Scale {
                            x: geometry.size.w as f64 / buffer_size.w as f64,
                            y: geometry.size.h as f64 / buffer_size.h as f64,
                        };

                        let relocation = (geometry.loc - original_geo.loc)
                            .as_logical()
                            .to_physical_precise_round(output_scale);

                        window_elements = window_elements
                            .into_iter()
                            .map(|element| match element {
                                CosmicMappedRenderElement::Stack(elem) => {
                                    CosmicMappedRenderElement::MovingStack({
                                        let rescaled = RescaleRenderElement::from_element(
                                            elem,
                                            original_geo
                                                .loc
                                                .as_logical()
                                                .to_physical_precise_round(output_scale),
                                            scale,
                                        );

                                        RelocateRenderElement::from_element(
                                            rescaled,
                                            relocation,
                                            Relocate::Relative,
                                        )
                                    })
                                }
                                CosmicMappedRenderElement::Window(elem) => {
                                    CosmicMappedRenderElement::MovingWindow({
                                        let rescaled = RescaleRenderElement::from_element(
                                            elem,
                                            original_geo
                                                .loc
                                                .as_logical()
                                                .to_physical_precise_round(output_scale),
                                            scale,
                                        );

                                        RelocateRenderElement::from_element(
                                            rescaled,
                                            relocation,
                                            Relocate::Relative,
                                        )
                                    })
                                }
                                x => x,
                            })
                            .collect();
                    }
                }
                // Client-driven: The geometry was already set correctly above
                // using client_driven_geometry(). No buffer rescaling or relocation
                // needed - the client renders at the animated size and we already
                // positioned it at the animated location.
            }

            if focused == Some(elem) && !elem.is_maximized(false) {
                if let Some((mode, resize)) = resize_indicator.as_mut() {
                    let mut resize_geometry = geometry;
                    resize_geometry.loc -= (18, 18).into();
                    resize_geometry.size += (36, 36).into();

                    resize.resize(resize_geometry.size.as_logical());
                    resize.output_enter(output, Rectangle::default() /* unused */);
                    window_elements = resize
                        .render_elements::<CosmicWindowRenderElement<R>>(
                            renderer,
                            resize_geometry
                                .loc
                                .as_logical()
                                .to_physical_precise_round(output_scale),
                            output_scale.into(),
                            alpha * mode.alpha().unwrap_or(1.0),
                        )
                        .into_iter()
                        .map(CosmicMappedRenderElement::Window)
                        .chain(window_elements.into_iter())
                        .collect();
                }

                let active_window_hint = crate::theme::active_window_hint(theme);
                let radius = elem.corner_radius(geometry.size.as_logical(), indicator_thickness);
                if indicator_thickness > 0 {
                    let element = IndicatorShader::focus_element(
                        renderer,
                        Key::Window(Usage::FocusIndicator, elem.key()),
                        geometry,
                        indicator_thickness,
                        radius,
                        alpha,
                        output_scale,
                        [
                            active_window_hint.red,
                            active_window_hint.green,
                            active_window_hint.blue,
                        ],
                    );
                    window_elements.insert(0, element.into());
                }
            }

            // Render embedded children in front of parent (they'll be on top in the z-order)
            let embedded_elements =
                self.render_embedded_children(renderer, elem, geometry, output_scale, alpha);

            // Log embedded children during blur capture
            if blur_ctx.is_some() && !embedded_elements.is_empty() {
                tracing::debug!(
                    parent_app_id = %elem.active_window().app_id(),
                    embedded_count = embedded_elements.len(),
                    "Blur capture: including embedded children for parent"
                );
            }

            // Combine: embedded first (on top), then parent's elements (behind)
            let mut all_window_elements = embedded_elements;
            all_window_elements.extend(window_elements);
            elements.extend(all_window_elements);
        }

        elements
    }

    fn gaps(&self) -> (i32, i32) {
        let g = self.theme.cosmic().gaps;
        (g.0 as i32, g.1 as i32)
    }
}
