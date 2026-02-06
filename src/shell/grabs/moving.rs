// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    backend::render::{
        BLUR_BACKDROP_ALPHA, BLUR_BACKDROP_COLOR, BLUR_FALLBACK_ALPHA, BLUR_FALLBACK_COLOR,
        BLUR_TINT_COLOR, BLUR_TINT_STRENGTH, BackdropShader, BlurredBackdropShader,
        IndicatorShader, Key, Usage,
        cursor::CursorState,
        element::AsGlowRenderer,
        get_cached_blur_texture_for_window,
        voice_orb::{VoiceOrbShader, VoiceOrbState},
    },
    shell::{
        CosmicMapped, CosmicSurface, Direction, ManagedLayer,
        element::{
            CosmicMappedRenderElement,
            stack_hover::{StackHover, stack_hover},
        },
        focus::target::{KeyboardFocusTarget, PointerFocusTarget},
        layout::floating::TiledCorners,
    },
    utils::prelude::*,
    wayland::{
        handlers::surface_embed::{
            EmbedRenderInfo, get_embedded_surface_ids_for_parent, mark_parent_grabbed,
            unmark_parent_grabbed,
        },
        protocols::toplevel_info::{toplevel_enter_output, toplevel_enter_workspace},
    },
};

use calloop::LoopHandle;
use cosmic::theme::CosmicTheme;
use smithay::{
    backend::{
        input::ButtonState,
        renderer::{
            ImportAll, ImportMem, Renderer,
            element::{AsRenderElements, RenderElement, utils::RescaleRenderElement},
        },
    },
    desktop::{WindowSurfaceType, layer_map_for_output, space::SpaceElement},
    input::{
        Seat,
        pointer::{
            AxisFrame, ButtonEvent, CursorIcon, GestureHoldBeginEvent, GestureHoldEndEvent,
            GesturePinchBeginEvent, GesturePinchEndEvent, GesturePinchUpdateEvent,
            GestureSwipeBeginEvent, GestureSwipeEndEvent, GestureSwipeUpdateEvent,
            GrabStartData as PointerGrabStartData, MotionEvent, PointerGrab, PointerInnerHandle,
            RelativeMotionEvent,
        },
        touch::{self, GrabStartData as TouchGrabStartData, TouchGrab, TouchInnerHandle},
    },
    output::Output,
    reexports::wayland_server::Resource,
    utils::{IsAlive, Logical, Point, Rectangle, SERIAL_COUNTER, Scale, Serial},
    wayland::seat::WaylandFocus,
};
use std::{
    collections::HashSet,
    sync::{Mutex, atomic::Ordering},
    time::Instant,
};

use super::{GrabStartData, ReleaseMode};

pub type SeatMoveGrabState = Mutex<Option<MoveGrabState>>;

const RESCALE_ANIMATION_DURATION: f64 = 150.0;

pub struct MoveGrabState {
    window: CosmicMapped,
    window_offset: Point<i32, Logical>,
    indicator_thickness: u8,
    start: Instant,
    previous: ManagedLayer,
    snapping_zone: Option<SnappingZone>,
    stacking_indicator: Option<(StackHover, Point<i32, Logical>)>,
    location: Point<f64, Logical>,
    cursor_output: Output,
}

impl MoveGrabState {
    #[profiling::function]
    pub fn render<I, R>(
        &self,
        renderer: &mut R,
        output: &Output,
        theme: &CosmicTheme,
        embedded_children: &[(CosmicMapped, EmbedRenderInfo)],
        attached_orb_state: Option<&VoiceOrbState>,
    ) -> Vec<I>
    where
        R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
        R::TextureId: Send + Clone + 'static,
        CosmicMappedRenderElement<R>: RenderElement<R>,
        I: From<CosmicMappedRenderElement<R>>,
    {
        let scale = if self.previous == ManagedLayer::Tiling {
            0.6 + ((1.0
                - (Instant::now().duration_since(self.start).as_millis() as f64
                    / RESCALE_ANIMATION_DURATION)
                    .min(1.0))
                * 0.4)
        } else {
            1.0
        };
        let alpha = if &self.cursor_output == output {
            1.0
        } else {
            0.4
        };

        let mut window_geo = self.window.geometry();
        window_geo.loc += self.location.to_i32_round() + self.window_offset;
        if output
            .geometry()
            .as_logical()
            .intersection(window_geo)
            .is_none()
        {
            return Vec::new();
        }

        let output_scale: Scale<f64> = output.current_scale().fractional_scale().into();
        let scaling_offset =
            self.window_offset - self.window_offset.to_f64().upscale(scale).to_i32_round();
        let render_location = self.location.to_i32_round() - output.geometry().loc.as_logical()
            + self.window_offset
            - scaling_offset;

        let active_window_hint = crate::theme::active_window_hint(theme);
        let radius = self
            .element()
            .corner_radius(window_geo.size, self.indicator_thickness);

        let focus_element = if self.indicator_thickness > 0 {
            Some(CosmicMappedRenderElement::from(
                IndicatorShader::focus_element(
                    renderer,
                    Key::Window(Usage::MoveGrabIndicator, self.window.key()),
                    Rectangle::new(
                        render_location,
                        self.window
                            .geometry()
                            .size
                            .to_f64()
                            .upscale(scale)
                            .to_i32_round(),
                    )
                    .as_local(),
                    self.indicator_thickness,
                    radius,
                    alpha,
                    output_scale.x,
                    [
                        active_window_hint.red,
                        active_window_hint.green,
                        active_window_hint.blue,
                    ],
                ),
            ))
        } else {
            None
        };

        let non_exclusive_geometry = {
            let layers = layer_map_for_output(output);
            layers.non_exclusive_zone()
        };

        let gaps = (theme.gaps.0 as i32, theme.gaps.1 as i32);
        let thickness = self.indicator_thickness.max(1);

        let snapping_indicator = match &self.snapping_zone {
            Some(t) if &self.cursor_output == output => {
                let base_color = theme.palette.neutral_9;
                let overlay_geometry = t.overlay_geometry(non_exclusive_geometry, gaps);
                vec![
                    CosmicMappedRenderElement::from(IndicatorShader::element(
                        renderer,
                        Key::Window(Usage::SnappingIndicator, self.window.key()),
                        overlay_geometry,
                        thickness,
                        [
                            theme.radius_s()[0] as u8,
                            theme.radius_s()[1] as u8,
                            theme.radius_s()[2] as u8,
                            theme.radius_s()[3] as u8,
                        ],
                        1.0,
                        output_scale.x,
                        [
                            active_window_hint.red,
                            active_window_hint.green,
                            active_window_hint.blue,
                        ],
                    )),
                    CosmicMappedRenderElement::from(BackdropShader::element(
                        renderer,
                        Key::Window(Usage::SnappingIndicator, self.window.key()),
                        t.overlay_geometry(non_exclusive_geometry, gaps),
                        theme.radius_s(),
                        0.4,
                        [base_color.red, base_color.green, base_color.blue],
                    )),
                ]
            }
            _ => vec![],
        };

        // Render blur backdrop if the window has blur enabled
        let blur_backdrop_element: Vec<CosmicMappedRenderElement<R>> = if self.window.has_blur() {
            let window_geo = self.window.geometry();
            // Compute window corner radius from theme: radius_s + 4 for values >= 4
            let radius_s = theme.radius_s()[0];
            let window_radius = (if radius_s < 4.0 {
                radius_s
            } else {
                radius_s + 4.0
            })
            .round() as u8;
            let corner_radius = self
                .window
                .blur_corner_radius(window_geo.size.as_logical(), window_radius);

            let output_name = output.name();
            let window_key = self.window.key();
            let output_transform = output.current_transform();
            let output_scale_f = output.current_scale().fractional_scale();

            // Calculate scaled geometry for the blur backdrop
            let scaled_size = window_geo.size.to_f64().upscale(scale).to_i32_round();
            let backdrop_geometry = Rectangle::new(render_location, scaled_size).as_local();

            // Get per-window blur texture
            let blur_info = get_cached_blur_texture_for_window(&output_name, &window_key);

            if let Some(blur_info) = blur_info {
                let blur_elem = CosmicMappedRenderElement::from(BlurredBackdropShader::element(
                    renderer,
                    &blur_info.texture,
                    backdrop_geometry,
                    blur_info.size,
                    blur_info.screen_size,
                    output_scale_f,
                    output_transform,
                    corner_radius,
                    alpha,
                    BLUR_TINT_COLOR,
                    BLUR_TINT_STRENGTH,
                    false, // No blur border for moving windows
                ));
                // Additional 90% white backdrop on top of blur
                let white_backdrop = CosmicMappedRenderElement::from(BackdropShader::element(
                    renderer,
                    Key::Window(Usage::BlurBackdrop, self.window.key()),
                    backdrop_geometry,
                    corner_radius,
                    alpha * BLUR_BACKDROP_ALPHA,
                    BLUR_BACKDROP_COLOR,
                ));
                vec![white_backdrop, blur_elem]
            } else {
                // Fallback when no blur texture is cached
                vec![CosmicMappedRenderElement::from(BackdropShader::element(
                    renderer,
                    Key::Window(Usage::Overlay, self.window.key()),
                    backdrop_geometry,
                    corner_radius,
                    alpha * BLUR_FALLBACK_ALPHA,
                    BLUR_FALLBACK_COLOR,
                ))]
            }
        } else {
            vec![]
        };

        let w_elements = self
            .window
            .render_elements::<R, CosmicMappedRenderElement<R>>(
                renderer,
                (render_location - self.window.geometry().loc)
                    .to_physical_precise_round(output_scale),
                None,
                output_scale,
                alpha,
                Some(false),
            );
        let p_elements = self
            .window
            .popup_render_elements::<R, CosmicMappedRenderElement<R>>(
                renderer,
                (render_location - self.window.geometry().loc)
                    .to_physical_precise_round(output_scale),
                output_scale,
                alpha,
            );
        let shadow_element = self.window.shadow_render_element(
            renderer,
            (render_location - self.window.geometry().loc).to_physical_precise_round(output_scale),
            None,
            output_scale,
            scale,
            alpha,
        );

        // Render embedded children on top of the parent window
        // They should follow the dragged parent position
        let parent_geo_size = self.window.geometry().size;
        let embedded_elements: Vec<CosmicMappedRenderElement<R>> = embedded_children
            .iter()
            .flat_map(|(embedded_elem, embed_info)| {
                // Calculate actual geometry using anchor config or stored geometry
                let actual_geometry = if let Some(ref anchor_config) = embed_info.anchor_config {
                    anchor_config.calculate_geometry(parent_geo_size.w, parent_geo_size.h)
                } else {
                    embed_info.geometry
                };

                // Embedded window position = parent render location + embed offset
                let embed_render_location = render_location + actual_geometry.loc;

                embedded_elem.render_elements::<R, CosmicMappedRenderElement<R>>(
                    renderer,
                    (embed_render_location - embedded_elem.geometry().loc)
                        .to_physical_precise_round(output_scale),
                    None,
                    output_scale,
                    alpha,
                    None,
                )
            })
            .collect();

        // Render voice orb if attached to this window
        let orb_element: Option<CosmicMappedRenderElement<R>> =
            attached_orb_state.and_then(|orb_state| {
                // Check if orb is attached to this window
                if let Some(attached_surface_id) = orb_state.attached_surface_id_for_render() {
                    let window_surface_id = self
                        .window
                        .active_window()
                        .wl_surface()
                        .map(|s| s.id().to_string());

                    if window_surface_id.as_deref() == Some(attached_surface_id) {
                        // Get output geometry for orb rendering
                        let output_geo = output.geometry().as_logical();

                        // Calculate the current window geometry for the grabbed window
                        let current_window_geo = Rectangle::new(
                            render_location,
                            self.window
                                .geometry()
                                .size
                                .to_f64()
                                .upscale(scale)
                                .to_i32_round(),
                        );

                        // Compute window corner radius from theme: radius_s + 4 for values >= 4
                        let radius_s = theme.radius_s()[0];
                        let window_border_radius = if radius_s < 4.0 {
                            radius_s
                        } else {
                            radius_s + 4.0
                        };

                        // Create the voice orb element with current window geometry
                        VoiceOrbShader::element_with_window_override(
                            renderer,
                            orb_state,
                            output_geo,
                            Some(current_window_geo),
                            Some(window_border_radius),
                        )
                        .map(|e| CosmicMappedRenderElement::from(e))
                    } else {
                        None
                    }
                } else {
                    None
                }
            });

        // Embedded windows at the front (top z-order), then rest of the elements
        embedded_elements
            .into_iter()
            .chain(
                self.stacking_indicator
                    .iter()
                    .flat_map(|(indicator, location)| {
                        indicator.render_elements(
                            renderer,
                            location.to_physical_precise_round(output_scale),
                            output_scale,
                            1.0,
                        )
                    })
                    .chain(p_elements)
                    .chain(focus_element)
                    .chain(
                        w_elements
                            .into_iter()
                            .chain(shadow_element)
                            .map(|elem| match elem {
                                CosmicMappedRenderElement::Stack(stack) => {
                                    CosmicMappedRenderElement::GrabbedStack(
                                        RescaleRenderElement::from_element(
                                            stack,
                                            render_location.to_physical_precise_round(
                                                output.current_scale().fractional_scale(),
                                            ),
                                            scale,
                                        ),
                                    )
                                }
                                CosmicMappedRenderElement::Window(window) => {
                                    CosmicMappedRenderElement::GrabbedWindow(
                                        RescaleRenderElement::from_element(
                                            window,
                                            render_location.to_physical_precise_round(
                                                output.current_scale().fractional_scale(),
                                            ),
                                            scale,
                                        ),
                                    )
                                }
                                x => x,
                            }),
                    )
                    .chain(orb_element)
                    .chain(blur_backdrop_element.into_iter())
                    .chain(snapping_indicator),
            )
            .map(I::from)
            .collect()
    }

    pub fn element(&self) -> CosmicMapped {
        self.window.clone()
    }

    pub fn window(&self) -> CosmicSurface {
        self.window.active_window()
    }
}

struct NotSend<T>(pub T);
unsafe impl<T> Send for NotSend<T> {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SnappingZone {
    Maximize,
    Top,
    TopLeft,
    Left,
    BottomLeft,
    Bottom,
    BottomRight,
    Right,
    TopRight,
}

const SNAP_RANGE: i32 = 32;
const SNAP_RANGE_MAXIMIZE: i32 = 22;
const SNAP_RANGE_TOP: i32 = 16;

impl SnappingZone {
    pub fn contains(
        &self,
        point: Point<i32, Local>,
        output_geometry: Rectangle<i32, Local>,
    ) -> bool {
        if !output_geometry.contains(point) {
            return false;
        }
        let top_zone_32 = point.y < output_geometry.loc.y + SNAP_RANGE_MAXIMIZE;
        let top_zone_56 = point.y < output_geometry.loc.y + SNAP_RANGE_MAXIMIZE + SNAP_RANGE_TOP;
        let left_zone = point.x < output_geometry.loc.x + SNAP_RANGE;
        let right_zone = point.x > output_geometry.loc.x + output_geometry.size.w - SNAP_RANGE;
        let bottom_zone = point.y > output_geometry.loc.y + output_geometry.size.h - SNAP_RANGE;
        let left_6th = point.x < output_geometry.loc.x + (output_geometry.size.w / 6);
        let right_6th = point.x > output_geometry.loc.x + (output_geometry.size.w * 5 / 6);
        let top_4th = point.y < output_geometry.loc.y + (output_geometry.size.h / 4);
        let bottom_4th = point.y > output_geometry.loc.y + (output_geometry.size.h * 3 / 4);
        match self {
            SnappingZone::Maximize => top_zone_32 && !left_6th && !right_6th,
            SnappingZone::Top => top_zone_56 && !top_zone_32 && !left_6th && !right_6th,
            SnappingZone::TopLeft => (top_zone_56 && left_6th) || (left_zone && top_4th),
            SnappingZone::Left => left_zone && !top_4th && !bottom_4th,
            SnappingZone::BottomLeft => (bottom_zone && left_6th) || (left_zone && bottom_4th),
            SnappingZone::Bottom => bottom_zone && !left_6th && !right_6th,
            SnappingZone::BottomRight => (bottom_zone && right_6th) || (right_zone && bottom_4th),
            SnappingZone::Right => right_zone && !top_4th && !bottom_4th,
            SnappingZone::TopRight => (top_zone_56 && right_6th) || (right_zone && top_4th),
        }
    }
    pub fn overlay_geometry(
        &self,
        non_exclusive_geometry: Rectangle<i32, Logical>,
        gaps: (i32, i32),
    ) -> Rectangle<i32, Local> {
        match self {
            SnappingZone::Maximize => non_exclusive_geometry.as_local(),
            SnappingZone::Top => TiledCorners::Top.relative_geometry(non_exclusive_geometry, gaps),
            SnappingZone::TopLeft => {
                TiledCorners::TopLeft.relative_geometry(non_exclusive_geometry, gaps)
            }
            SnappingZone::Left => {
                TiledCorners::Left.relative_geometry(non_exclusive_geometry, gaps)
            }
            SnappingZone::BottomLeft => {
                TiledCorners::BottomLeft.relative_geometry(non_exclusive_geometry, gaps)
            }
            SnappingZone::Bottom => {
                TiledCorners::Bottom.relative_geometry(non_exclusive_geometry, gaps)
            }
            SnappingZone::BottomRight => {
                TiledCorners::BottomRight.relative_geometry(non_exclusive_geometry, gaps)
            }
            SnappingZone::Right => {
                TiledCorners::Right.relative_geometry(non_exclusive_geometry, gaps)
            }
            SnappingZone::TopRight => {
                TiledCorners::TopRight.relative_geometry(non_exclusive_geometry, gaps)
            }
        }
    }
}

pub struct MoveGrab {
    window: CosmicMapped,
    start_data: GrabStartData,
    seat: Seat<State>,
    cursor_output: Output,
    window_outputs: HashSet<Output>,
    previous: ManagedLayer,
    release: ReleaseMode,
    edge_snap_threshold: f64,
    // SAFETY: This is only used on drop which will always be on the main thread
    evlh: NotSend<LoopHandle<'static, State>>,
}

impl MoveGrab {
    fn update_location(&mut self, state: &mut State, location: Point<f64, Logical>) {
        let mut shell = state.common.shell.write();

        let Some(current_output) = shell
            .outputs()
            .find(|output| {
                output
                    .geometry()
                    .as_logical()
                    .overlaps_or_touches(Rectangle::new(location.to_i32_floor(), (0, 0).into()))
            })
            .cloned()
        else {
            return;
        };
        if self.cursor_output != current_output {
            shell
                .workspaces
                .active_mut(&self.cursor_output)
                .unwrap()
                .tiling_layer
                .cleanup_drag();
            self.cursor_output = current_output.clone();
        }

        let mut borrow = self
            .seat
            .user_data()
            .get::<SeatMoveGrabState>()
            .map(|s| s.lock().unwrap());
        if let Some(grab_state) = borrow.as_mut().and_then(|s| s.as_mut()) {
            grab_state.location = location;
            grab_state.cursor_output = self.cursor_output.clone();

            let mut window_geo = self.window.geometry();
            window_geo.loc += location.to_i32_round() + grab_state.window_offset;

            if matches!(self.previous, ManagedLayer::Floating | ManagedLayer::Sticky) {
                let loc = grab_state.window_offset.to_f64() + grab_state.location;
                let size = window_geo.size.to_f64();
                let output_geom = self.cursor_output.geometry().to_f64().as_logical();
                let output_loc = output_geom.loc;
                let output_size = output_geom.size;

                grab_state.location.x = if (loc.x - output_loc.x).abs() < self.edge_snap_threshold {
                    output_loc.x - grab_state.window_offset.x as f64
                } else if ((loc.x + size.w) - (output_loc.x + output_size.w)).abs()
                    < self.edge_snap_threshold
                {
                    output_loc.x + output_size.w - grab_state.window_offset.x as f64 - size.w
                } else {
                    grab_state.location.x
                };
                grab_state.location.y = if (loc.y - output_loc.y).abs() < self.edge_snap_threshold {
                    output_loc.y - grab_state.window_offset.y as f64
                } else if ((loc.y + size.h) - (output_loc.y + output_size.h)).abs()
                    < self.edge_snap_threshold
                {
                    output_loc.y + output_size.h - grab_state.window_offset.y as f64 - size.h
                } else {
                    grab_state.location.y
                };
            }

            for output in shell.outputs() {
                if let Some(overlap) = output.geometry().as_logical().intersection(window_geo) {
                    if self.window_outputs.insert(output.clone()) {
                        self.window.output_enter(output, overlap);
                        if let Some(indicator) =
                            grab_state.stacking_indicator.as_ref().map(|x| &x.0)
                        {
                            indicator.output_enter(output, overlap);
                        }
                    }
                } else if self.window_outputs.remove(output) {
                    self.window.output_leave(output);
                    if let Some(indicator) = grab_state.stacking_indicator.as_ref().map(|x| &x.0) {
                        indicator.output_leave(output);
                    }
                }
            }

            let indicator_location = shell.stacking_indicator(&current_output, self.previous);
            if indicator_location.is_some() != grab_state.stacking_indicator.is_some() {
                grab_state.stacking_indicator = indicator_location.map(|geo| {
                    let element = stack_hover(
                        state.common.event_loop_handle.clone(),
                        geo.size.as_logical(),
                        state.common.theme.clone(),
                    );
                    for output in &self.window_outputs {
                        element.output_enter(
                            output,
                            Rectangle::from_size(output.geometry().size.as_logical()),
                        );
                    }
                    (element, geo.loc.as_logical())
                });
            }

            // Check for overlapping with zones
            if grab_state.previous == ManagedLayer::Floating {
                let output_geometry = current_output.geometry().to_local(&current_output);
                grab_state.snapping_zone = [
                    SnappingZone::Maximize,
                    SnappingZone::Top,
                    SnappingZone::TopLeft,
                    SnappingZone::Left,
                    SnappingZone::BottomLeft,
                    SnappingZone::Bottom,
                    SnappingZone::BottomRight,
                    SnappingZone::Right,
                    SnappingZone::TopRight,
                ]
                .iter()
                .find(|&x| {
                    x.contains(
                        location
                            .as_global()
                            .to_local(&current_output)
                            .to_i32_floor(),
                        output_geometry,
                    )
                })
                .cloned();
            }
        }
        drop(borrow);
    }
}

impl PointerGrab<State> for MoveGrab {
    fn motion(
        &mut self,
        state: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        _focus: Option<(PointerFocusTarget, Point<f64, Logical>)>,
        event: &MotionEvent,
    ) {
        self.update_location(state, event.location);

        // While the grab is active, no client has pointer focus
        handle.motion(state, None, event);
        if !self.window.alive() {
            handle.unset_grab(self, state, event.serial, event.time, true);
        }
    }

    fn relative_motion(
        &mut self,
        state: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        _focus: Option<(PointerFocusTarget, Point<f64, Logical>)>,
        event: &RelativeMotionEvent,
    ) {
        // While the grab is active, no client has pointer focus
        handle.relative_motion(state, None, event);
    }

    fn button(
        &mut self,
        state: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &ButtonEvent,
    ) {
        handle.button(state, event);
        match self.release {
            ReleaseMode::NoMouseButtons => {
                if handle.current_pressed().is_empty() {
                    handle.unset_grab(self, state, event.serial, event.time, true);
                }
            }
            ReleaseMode::Click => {
                if event.state == ButtonState::Pressed {
                    handle.unset_grab(self, state, event.serial, event.time, true);
                }
            }
        }
    }

    fn axis(
        &mut self,
        state: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        details: AxisFrame,
    ) {
        handle.axis(state, details);
    }

    fn frame(&mut self, data: &mut State, handle: &mut PointerInnerHandle<'_, State>) {
        handle.frame(data)
    }

    fn gesture_swipe_begin(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GestureSwipeBeginEvent,
    ) {
        handle.gesture_swipe_begin(data, event)
    }

    fn gesture_swipe_update(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GestureSwipeUpdateEvent,
    ) {
        handle.gesture_swipe_update(data, event)
    }

    fn gesture_swipe_end(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GestureSwipeEndEvent,
    ) {
        handle.gesture_swipe_end(data, event)
    }

    fn gesture_pinch_begin(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GesturePinchBeginEvent,
    ) {
        handle.gesture_pinch_begin(data, event)
    }

    fn gesture_pinch_update(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GesturePinchUpdateEvent,
    ) {
        handle.gesture_pinch_update(data, event)
    }

    fn gesture_pinch_end(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GesturePinchEndEvent,
    ) {
        handle.gesture_pinch_end(data, event)
    }

    fn gesture_hold_begin(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GestureHoldBeginEvent,
    ) {
        handle.gesture_hold_begin(data, event)
    }

    fn gesture_hold_end(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GestureHoldEndEvent,
    ) {
        handle.gesture_hold_end(data, event)
    }

    fn start_data(&self) -> &PointerGrabStartData<State> {
        match &self.start_data {
            GrabStartData::Pointer(start_data) => start_data,
            _ => unreachable!(),
        }
    }

    fn unset(&mut self, _data: &mut State) {}
}

impl TouchGrab<State> for MoveGrab {
    fn down(
        &mut self,
        data: &mut State,
        handle: &mut TouchInnerHandle<'_, State>,
        _focus: Option<(PointerFocusTarget, Point<f64, Logical>)>,
        event: &touch::DownEvent,
        seq: Serial,
    ) {
        handle.down(data, None, event, seq)
    }

    fn up(
        &mut self,
        data: &mut State,
        handle: &mut TouchInnerHandle<'_, State>,
        event: &touch::UpEvent,
        seq: Serial,
    ) {
        if event.slot == <Self as TouchGrab<State>>::start_data(self).slot {
            handle.unset_grab(self, data);
        }

        handle.up(data, event, seq);
    }

    fn motion(
        &mut self,
        data: &mut State,
        handle: &mut TouchInnerHandle<'_, State>,
        _focus: Option<(PointerFocusTarget, Point<f64, Logical>)>,
        event: &touch::MotionEvent,
        seq: Serial,
    ) {
        if event.slot == <Self as TouchGrab<State>>::start_data(self).slot {
            self.update_location(data, event.location);
        }

        handle.motion(data, None, event, seq);
    }

    fn frame(&mut self, data: &mut State, handle: &mut TouchInnerHandle<'_, State>, seq: Serial) {
        handle.frame(data, seq)
    }

    fn cancel(&mut self, data: &mut State, handle: &mut TouchInnerHandle<'_, State>, _seq: Serial) {
        handle.unset_grab(self, data);
    }

    fn shape(
        &mut self,
        data: &mut State,
        handle: &mut TouchInnerHandle<'_, State>,
        event: &touch::ShapeEvent,
        seq: Serial,
    ) {
        handle.shape(data, event, seq)
    }

    fn orientation(
        &mut self,
        data: &mut State,
        handle: &mut TouchInnerHandle<'_, State>,
        event: &touch::OrientationEvent,
        seq: Serial,
    ) {
        handle.orientation(data, event, seq)
    }

    fn start_data(&self) -> &TouchGrabStartData<State> {
        match &self.start_data {
            GrabStartData::Touch(start_data) => start_data,
            _ => unreachable!(),
        }
    }

    fn unset(&mut self, _data: &mut State) {}
}

impl MoveGrab {
    pub fn new(
        start_data: GrabStartData,
        window: CosmicMapped,
        seat: &Seat<State>,
        initial_window_location: Point<i32, Global>,
        cursor_output: Output,
        indicator_thickness: u8,
        edge_snap_threshold: f64,
        previous_layer: ManagedLayer,
        release: ReleaseMode,
        evlh: LoopHandle<'static, State>,
    ) -> MoveGrab {
        let mut outputs = HashSet::new();
        outputs.insert(cursor_output.clone());
        window.output_enter(&cursor_output, window.geometry()); // not accurate but...
        window.moved_since_mapped.store(true, Ordering::SeqCst);

        let grab_state = MoveGrabState {
            window: window.clone(),
            window_offset: (initial_window_location
                - start_data.location().as_global().to_i32_round())
            .as_logical(),
            indicator_thickness,
            start: Instant::now(),
            stacking_indicator: None,
            snapping_zone: None,
            previous: previous_layer,
            location: start_data.location(),
            cursor_output: cursor_output.clone(),
        };

        // Mark this window as grabbed so cleanup_orphaned_embeds won't orphan
        // any embedded children while it's being dragged
        if let Some(surface_id) = window
            .active_window()
            .wl_surface()
            .map(|s| s.id().to_string())
        {
            mark_parent_grabbed(&surface_id);
        }

        *seat
            .user_data()
            .get::<SeatMoveGrabState>()
            .unwrap()
            .lock()
            .unwrap() = Some(grab_state);

        {
            let cursor_state = seat.user_data().get::<CursorState>().unwrap();
            cursor_state.lock().unwrap().set_shape(CursorIcon::Grabbing);
        }

        MoveGrab {
            window,
            start_data,
            seat: seat.clone(),
            cursor_output,
            window_outputs: outputs,
            previous: previous_layer,
            release,
            edge_snap_threshold,
            evlh: NotSend(evlh),
        }
    }

    pub fn is_tiling_grab(&self) -> bool {
        self.previous == ManagedLayer::Tiling
    }

    pub fn is_touch_grab(&self) -> bool {
        match self.start_data {
            GrabStartData::Touch(_) => true,
            GrabStartData::Pointer(_) => false,
        }
    }
}

impl Drop for MoveGrab {
    fn drop(&mut self) {
        // No more buttons are pressed, release the grab.
        let output = self.cursor_output.clone();
        let seat = self.seat.clone();
        let window_outputs = self.window_outputs.drain().collect::<HashSet<_>>();
        let previous = self.previous;
        let window = self.window.clone();
        let is_touch_grab = matches!(self.start_data, GrabStartData::Touch(_));
        let cursor_output = self.cursor_output.clone();

        let _ = self.evlh.0.insert_idle(move |state| {
            let (position, parent_surface_id): (
                Option<(CosmicMapped, Point<i32, Global>)>,
                Option<String>,
            ) = if let Some(grab_state) = seat
                .user_data()
                .get::<SeatMoveGrabState>()
                .and_then(|s| s.lock().unwrap().take())
            {
                // Unmark this window as grabbed - cleanup can now orphan children if window closes
                let parent_surface_id = grab_state
                    .window
                    .active_window()
                    .wl_surface()
                    .map(|s| s.id().to_string());
                if let Some(ref surface_id) = parent_surface_id {
                    unmark_parent_grabbed(surface_id);
                }

                let position = if grab_state.window.alive() {
                    let window_location =
                        (grab_state.location.to_i32_round() + grab_state.window_offset).as_global();
                    let mut shell = state.common.shell.write();

                    let workspace_handle = shell.active_space(&output).unwrap().handle;
                    for old_output in window_outputs.iter().filter(|o| *o != &output) {
                        grab_state.window.output_leave(old_output);
                    }

                    for (window, _) in grab_state.window.windows() {
                        toplevel_enter_output(&window, &output);
                        if previous != ManagedLayer::Sticky {
                            toplevel_enter_workspace(&window, &workspace_handle);
                        }
                    }

                    match previous {
                        ManagedLayer::Sticky => {
                            grab_state.window.set_geometry(Rectangle::new(
                                window_location,
                                grab_state.window.geometry().size.as_global(),
                            ));
                            let set = shell.workspaces.sets.get_mut(&output).unwrap();
                            let (window, location) = set
                                .sticky_layer
                                .drop_window(grab_state.window, window_location.to_local(&output));

                            Some((window, location.to_global(&output)))
                        }
                        ManagedLayer::Tiling
                            if shell.active_space(&output).unwrap().tiling_enabled =>
                        {
                            let (window, location) = shell
                                .active_space_mut(&output)
                                .unwrap()
                                .tiling_layer
                                .drop_window(grab_state.window);
                            Some((window, location.to_global(&output)))
                        }
                        _ => {
                            grab_state.window.set_geometry(Rectangle::new(
                                window_location,
                                grab_state.window.geometry().size.as_global(),
                            ));
                            let theme = shell.theme.clone();
                            let workspace = shell.active_space_mut(&output).unwrap();
                            let (window, location) = workspace.floating_layer.drop_window(
                                grab_state.window,
                                window_location.to_local(&workspace.output),
                            );

                            if matches!(previous, ManagedLayer::Floating) {
                                if let Some(sz) = grab_state.snapping_zone {
                                    if sz == SnappingZone::Maximize {
                                        shell.maximize_toggle(
                                            &window,
                                            &seat,
                                            &state.common.event_loop_handle,
                                        );
                                    } else {
                                        let directions = match sz {
                                            SnappingZone::Maximize => vec![],
                                            SnappingZone::Top => vec![Direction::Up],
                                            SnappingZone::TopLeft => {
                                                vec![Direction::Up, Direction::Left]
                                            }
                                            SnappingZone::Left => vec![Direction::Left],
                                            SnappingZone::BottomLeft => {
                                                vec![Direction::Down, Direction::Left]
                                            }
                                            SnappingZone::Bottom => vec![Direction::Down],
                                            SnappingZone::BottomRight => {
                                                vec![Direction::Down, Direction::Right]
                                            }
                                            SnappingZone::Right => vec![Direction::Right],
                                            SnappingZone::TopRight => {
                                                vec![Direction::Up, Direction::Right]
                                            }
                                        };
                                        for direction in directions {
                                            workspace.floating_layer.move_element(
                                                direction,
                                                &seat,
                                                ManagedLayer::Floating,
                                                &theme,
                                                &window,
                                            );
                                        }
                                    }
                                }
                            }
                            Some((window, location.to_global(&output)))
                        }
                    }
                } else {
                    let mut shell = state.common.shell.write();
                    shell
                        .workspaces
                        .active_mut(&cursor_output)
                        .unwrap()
                        .tiling_layer
                        .cleanup_drag();
                    shell.set_overview_mode(None, state.common.event_loop_handle.clone());
                    None
                };
                (position, parent_surface_id)
            } else {
                (None, None)
            };

            // Move embedded children to follow the parent to the new output
            if position.is_some() {
                if let Some(ref parent_surface_id) = parent_surface_id {
                    let embedded_children = get_embedded_surface_ids_for_parent(parent_surface_id);
                    if !embedded_children.is_empty() {
                        tracing::info!(
                            parent_surface_id = %parent_surface_id,
                            children_count = embedded_children.len(),
                            "Moving embedded children with parent to new output"
                        );

                        // Get the target workspace handle
                        let target_workspace = {
                            let shell = state.common.shell.read();
                            shell.active_space(&output).map(|ws| ws.handle.clone())
                        };

                        if let Some(target_handle) = target_workspace {
                            for embedded_surface_id in embedded_children {
                                // Find and move each embedded child
                                let shell = state.common.shell.read();
                                let embedded_mapped = shell
                                    .workspaces
                                    .spaces()
                                    .flat_map(|s| s.mapped())
                                    .find(|m| {
                                        m.windows().any(|(w, _)| {
                                            w.wl_surface()
                                                .map(|s| s.id().to_string() == embedded_surface_id)
                                                .unwrap_or(false)
                                        })
                                    })
                                    .cloned();

                                let embedded_workspace = embedded_mapped
                                    .as_ref()
                                    .and_then(|m| shell.space_for(m))
                                    .map(|s| s.handle.clone());

                                drop(shell);

                                if let (Some(mapped), Some(from_handle)) =
                                    (embedded_mapped, embedded_workspace)
                                {
                                    if from_handle != target_handle {
                                        let mut workspace_state =
                                            state.common.workspace_state.update();
                                        let mut shell = state.common.shell.write();
                                        let _ = shell.move_element(
                                            None, // No seat
                                            &mapped,
                                            &from_handle,
                                            &target_handle,
                                            false, // Don't follow
                                            None,
                                            &mut workspace_state,
                                        );
                                        tracing::info!(
                                            embedded_surface_id = %embedded_surface_id,
                                            "Moved embedded child to parent's workspace"
                                        );
                                    }
                                }
                            }
                        }
                    }
                }
            }

            {
                let cursor_state = seat.user_data().get::<CursorState>().unwrap();
                cursor_state.lock().unwrap().unset_shape();
            }

            if let Some((mapped, position)) = position {
                let serial = SERIAL_COUNTER.next_serial();
                if !is_touch_grab {
                    let pointer = seat.get_pointer().unwrap();
                    let current_location = pointer.current_location();

                    if let Some((target, offset)) = mapped.focus_under(
                        current_location - position.as_logical().to_f64(),
                        WindowSurfaceType::ALL,
                    ) {
                        pointer.motion(
                            state,
                            Some((
                                target,
                                position.as_logical().to_f64() - window.geometry().loc.to_f64()
                                    + offset,
                            )),
                            &MotionEvent {
                                location: pointer.current_location(),
                                serial,
                                time: state.common.clock.now().as_millis(),
                            },
                        );
                    }
                }
                Shell::set_focus(
                    state,
                    Some(&KeyboardFocusTarget::from(mapped)),
                    &seat,
                    Some(serial),
                    false,
                )
            }
        });
    }
}
