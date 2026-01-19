use std::{ops::ControlFlow, time::Instant};

use cosmic_comp_config::workspace::WorkspaceLayout;
use keyframe::{ease, functions::EaseInOutCubic};
use smithay::{
    desktop::{LayerSurface, PopupKind, PopupManager, layer_map_for_output},
    output::{Output, OutputNoMode},
    reexports::wayland_server::Resource,
    utils::{Logical, Point},
    wayland::{session_lock::LockSurface, shell::wlr_layer::Layer},
    xwayland::X11Surface,
};

use crate::{
    backend::render::{ElementFilter, HomeVisibilityContext},
    shell::{
        SeatExt, Shell, Workspace, WorkspaceDelta,
        focus::target::KeyboardFocusTarget,
        layout::{floating::FloatingLayout, tiling::ANIMATION_DURATION},
    },
    utils::{
        geometry::*,
        prelude::OutputExt,
        quirks::{WORKSPACE_OVERVIEW_NAMESPACE, workspace_overview_is_open},
    },
    wayland::protocols::workspace::WorkspaceHandle,
};

/// Check if windows/workspaces should be included for the given element filter.
/// Returns false for LayerShellOnly and for LayerBlurCapture when capturing for
/// layers below windows (Bottom, Background).
fn should_include_windows(element_filter: &ElementFilter) -> bool {
    match element_filter {
        ElementFilter::LayerShellOnly => false,
        ElementFilter::LayerBlurCapture(layer) => {
            // Only include windows for layers above windows (Top, Overlay)
            // Bottom and Background are below windows, so skip windows when capturing for them
            matches!(layer, Layer::Top | Layer::Overlay)
        }
        _ => true,
    }
}

pub enum Stage<'a> {
    ZoomUI,
    SessionLock(Option<&'a LockSurface>),
    LayerPopup {
        layer: LayerSurface,
        popup: &'a PopupKind,
        location: Point<i32, Global>,
    },
    LayerSurface {
        layer: LayerSurface,
        location: Point<i32, Global>,
        /// Alpha/opacity for the surface (0.0-1.0), used for home visibility animation
        alpha: f32,
        /// Whether blur should be skipped for this surface (home_only or hide_on_home surfaces)
        skip_blur: bool,
    },
    OverrideRedirect {
        surface: &'a X11Surface,
        location: Point<i32, Global>,
    },
    StickyPopups(&'a FloatingLayout),
    Sticky(&'a FloatingLayout),
    WorkspacePopups {
        workspace: &'a Workspace,
        offset: Point<i32, Logical>,
    },
    Workspace {
        workspace: &'a Workspace,
        offset: Point<i32, Logical>,
    },
}
pub fn render_input_order<R: Default + 'static>(
    shell: &Shell,
    output: &Output,
    previous: Option<(WorkspaceHandle, usize, WorkspaceDelta)>,
    current: (WorkspaceHandle, usize),
    element_filter: &ElementFilter,
    callback: impl FnMut(Stage) -> ControlFlow<Result<R, OutputNoMode>, ()>,
) -> Result<R, OutputNoMode> {
    match render_input_order_internal(shell, output, previous, current, element_filter, callback) {
        ControlFlow::Break(result) => result,
        ControlFlow::Continue(_) => Ok(R::default()),
    }
}

fn render_input_order_internal<R: 'static>(
    shell: &Shell,
    output: &Output,
    previous: Option<(WorkspaceHandle, usize, WorkspaceDelta)>,
    current: (WorkspaceHandle, usize),
    element_filter: &ElementFilter,
    mut callback: impl FnMut(Stage) -> ControlFlow<Result<R, OutputNoMode>, ()>,
) -> ControlFlow<Result<R, OutputNoMode>, ()> {
    // Create home visibility context once for all layer surface filtering
    let home_visibility = HomeVisibilityContext::from_shell(shell);

    if shell
        .zoom_state
        .as_ref()
        .is_some_and(|state| state.show_overlay && state.current_level(output) != 1.0)
    {
        callback(Stage::ZoomUI)?;
    }

    // Session Lock
    if let Some(session_lock) = &shell.session_lock {
        return callback(Stage::SessionLock(session_lock.surfaces.get(output)));
    }

    // Overlay-level layer shell
    // overlay is above everything
    for (layer, popup, location, _alpha) in
        layer_popups(output, Layer::Overlay, element_filter, &home_visibility)
    {
        callback(Stage::LayerPopup {
            layer,
            popup: &popup,
            location,
        })?;
    }
    for (layer, location, alpha, skip_blur) in
        layer_surfaces(output, Layer::Overlay, element_filter, &home_visibility)
    {
        callback(Stage::LayerSurface {
            layer,
            location,
            alpha,
            skip_blur,
        })?;
    }

    // calculate a bunch of stuff for workspace transitions

    let Some(set) = shell.workspaces.sets.get(output) else {
        return ControlFlow::Break(Err(OutputNoMode));
    };
    let Some(workspace) = set.workspaces.iter().find(|w| w.handle == current.0) else {
        return ControlFlow::Break(Err(OutputNoMode));
    };
    let output_size = output.geometry().size;

    // this is more hacky than I would like..
    let fullscreen = workspace.fullscreen.as_ref().filter(|f| !f.is_animating());
    let seat = shell.seats.last_active();
    let is_active_workspace = seat.focused_output().is_some_and(|output| {
        shell
            .active_space(&output)
            .is_some_and(|w| w.handle == workspace.handle)
    });
    let focus_stack_is_valid_fullscreen = workspace
        .focus_stack
        .get(seat)
        .last()
        .zip(fullscreen)
        .is_some_and(|(target, fullscreen)| target == &fullscreen.surface);
    let overview_is_open = workspace_overview_is_open(output);
    let has_focused_fullscreen = if is_active_workspace {
        let current_focus = seat.get_keyboard().unwrap().current_focus();
        matches!(current_focus, Some(KeyboardFocusTarget::Fullscreen(_)))
            || (current_focus.is_none()
                && focus_stack_is_valid_fullscreen
                && !workspace_overview_is_open(output))
    } else {
        focus_stack_is_valid_fullscreen && !overview_is_open
    };
    let has_fullscreen = fullscreen.is_some() && !overview_is_open;

    let (previous, current_offset) = match previous.as_ref() {
        Some((previous, previous_idx, start)) => {
            let layout = shell.workspaces.layout;

            let Some(workspace) = shell.workspaces.space_for_handle(previous) else {
                return ControlFlow::Break(Err(OutputNoMode));
            };
            let has_fullscreen = workspace.fullscreen.is_some();

            let (forward, percentage) = match start {
                WorkspaceDelta::Shortcut(st) => (
                    *previous_idx < current.1,
                    ease(
                        EaseInOutCubic,
                        0.0,
                        1.0,
                        Instant::now().duration_since(*st).as_millis() as f32
                            / ANIMATION_DURATION.as_millis() as f32,
                    ),
                ),
                WorkspaceDelta::Gesture {
                    percentage: prog,
                    forward,
                } => (*forward, *prog as f32),
                WorkspaceDelta::GestureEnd {
                    start,
                    spring,
                    forward,
                } => (
                    *forward,
                    (spring.value_at(Instant::now().duration_since(*start)) as f32).clamp(0.0, 1.0),
                ),
            };

            let offset = Point::<i32, Logical>::from(match (layout, forward) {
                (WorkspaceLayout::Vertical, true) => {
                    (0, (-output_size.h as f32 * percentage).round() as i32)
                }
                (WorkspaceLayout::Vertical, false) => {
                    (0, (output_size.h as f32 * percentage).round() as i32)
                }
                (WorkspaceLayout::Horizontal, true) => {
                    ((-output_size.w as f32 * percentage).round() as i32, 0)
                }
                (WorkspaceLayout::Horizontal, false) => {
                    ((output_size.w as f32 * percentage).round() as i32, 0)
                }
            });

            (
                Some((previous, has_fullscreen, offset)),
                Point::<i32, Logical>::from(match (layout, forward) {
                    (WorkspaceLayout::Vertical, true) => (0, output_size.h + offset.y),
                    (WorkspaceLayout::Vertical, false) => (0, -(output_size.h - offset.y)),
                    (WorkspaceLayout::Horizontal, true) => (output_size.w + offset.x, 0),
                    (WorkspaceLayout::Horizontal, false) => (-(output_size.w - offset.x), 0),
                }),
            )
        }
        None => (None, Point::default()),
    };

    // Top-level layer shell popups
    if !has_focused_fullscreen {
        for (layer, popup, location, _alpha) in
            layer_popups(output, Layer::Top, element_filter, &home_visibility)
        {
            callback(Stage::LayerPopup {
                layer,
                popup: &popup,
                location,
            })?;
        }
    }

    if should_include_windows(element_filter) {
        // overlay redirect windows
        // they need to be over sticky windows, because they could be popups of sticky windows,
        // and we can't differenciate that.
        for (surface, location) in shell
            .override_redirect_windows
            .iter()
            .rev()
            .filter(|or| {
                (*or)
                    .geometry()
                    .as_global()
                    .intersection(output.geometry())
                    .is_some()
            })
            .map(|or| (or, or.geometry().loc.as_global()))
        {
            callback(Stage::OverrideRedirect { surface, location })?;
        }

        // sticky window popups
        if !has_focused_fullscreen {
            callback(Stage::StickyPopups(&set.sticky_layer))?;
        }
    }

    if should_include_windows(element_filter) {
        // previous workspace popups
        if let Some((previous_handle, _, offset)) = previous.as_ref() {
            let Some(workspace) = shell.workspaces.space_for_handle(previous_handle) else {
                return ControlFlow::Break(Err(OutputNoMode));
            };

            callback(Stage::WorkspacePopups {
                workspace,
                offset: *offset,
            })?;
        }

        // current workspace popups
        let Some(workspace) = shell.workspaces.space_for_handle(&current.0) else {
            return ControlFlow::Break(Err(OutputNoMode));
        };

        callback(Stage::WorkspacePopups {
            workspace,
            offset: current_offset,
        })?;
    }

    if !has_focused_fullscreen {
        // bottom layer popups
        for (layer, popup, location, _alpha) in
            layer_popups(output, Layer::Bottom, element_filter, &home_visibility)
        {
            callback(Stage::LayerPopup {
                layer,
                popup: &popup,
                location,
            })?;
        }
    }

    if let Some((_, has_fullscreen, offset)) = previous.as_ref() {
        if !has_fullscreen {
            // previous bottom layer popups
            for (layer, popup, location, _alpha) in
                layer_popups(output, Layer::Bottom, element_filter, &home_visibility)
            {
                callback(Stage::LayerPopup {
                    layer,
                    popup: &popup,
                    location: location + offset.as_global(),
                })?;
            }
        }
    }

    if !has_fullscreen {
        // background layer popups
        for (layer, popup, location, _alpha) in
            layer_popups(output, Layer::Background, element_filter, &home_visibility)
        {
            callback(Stage::LayerPopup {
                layer,
                popup: &popup,
                location,
            })?;
        }
    }

    if let Some((_, has_fullscreen, offset)) = previous.as_ref() {
        if !has_fullscreen {
            // previous background layer popups
            for (layer, popup, location, _alpha) in
                layer_popups(output, Layer::Background, element_filter, &home_visibility)
            {
                callback(Stage::LayerPopup {
                    layer,
                    popup: &popup,
                    location: location + offset.as_global(),
                })?;
            }
        }
    }

    if !has_focused_fullscreen {
        // top-layer shell
        for (layer, location, alpha, skip_blur) in
            layer_surfaces(output, Layer::Top, element_filter, &home_visibility)
        {
            callback(Stage::LayerSurface {
                layer,
                location,
                alpha,
                skip_blur,
            })?;
        }

        // sticky windows
        if should_include_windows(element_filter) {
            callback(Stage::Sticky(&set.sticky_layer))?;
        }
    }

    if should_include_windows(element_filter) {
        // workspace windows
        callback(Stage::Workspace {
            workspace,
            offset: current_offset,
        })?;

        // previous workspace windows
        if let Some((previous_handle, _, offset)) = previous.as_ref() {
            let Some(workspace) = shell.workspaces.space_for_handle(previous_handle) else {
                return ControlFlow::Break(Err(OutputNoMode));
            };
            callback(Stage::Workspace {
                workspace,
                offset: *offset,
            })?;
        }
    }

    if !has_focused_fullscreen {
        // bottom layer
        for (layer, mut location, alpha, skip_blur) in
            layer_surfaces(output, Layer::Bottom, element_filter, &home_visibility)
        {
            location += current_offset.as_global();
            callback(Stage::LayerSurface {
                layer,
                location,
                alpha,
                skip_blur,
            })?;
        }
    }

    if let Some((_, has_fullscreen, offset)) = previous.as_ref() {
        if !has_fullscreen {
            // previous bottom layer
            for (layer, mut location, alpha, skip_blur) in
                layer_surfaces(output, Layer::Bottom, element_filter, &home_visibility)
            {
                location += offset.as_global();
                callback(Stage::LayerSurface {
                    layer,
                    location,
                    alpha,
                    skip_blur,
                })?;
            }
        }
    }

    if !has_fullscreen {
        // background layer
        for (layer, mut location, alpha, skip_blur) in
            layer_surfaces(output, Layer::Background, element_filter, &home_visibility)
        {
            location += current_offset.as_global();
            callback(Stage::LayerSurface {
                layer,
                location,
                alpha,
                skip_blur,
            })?;
        }
    }

    if let Some((_, has_fullscreen, offset)) = previous.as_ref() {
        if !has_fullscreen {
            // previous background layer
            for (layer, mut location, alpha, skip_blur) in
                layer_surfaces(output, Layer::Background, element_filter, &home_visibility)
            {
                location += offset.as_global();
                callback(Stage::LayerSurface {
                    layer,
                    location,
                    alpha,
                    skip_blur,
                })?;
            }
        }
    }

    ControlFlow::Continue(())
}

fn layer_popups<'a>(
    output: &'a Output,
    layer: Layer,
    element_filter: &'a ElementFilter,
    home_visibility: &'a HomeVisibilityContext,
) -> impl Iterator<Item = (LayerSurface, PopupKind, Point<i32, Global>, f32)> + 'a {
    layer_surfaces(output, layer, element_filter, home_visibility).flat_map(
        move |(surface, location, alpha, _home_only)| {
            let location_clone = location;
            let surface_clone = surface.clone();
            PopupManager::popups_for_surface(surface.wl_surface()).map(
                move |(popup, popup_offset)| {
                    let offset = (popup_offset - popup.geometry().loc).as_global();
                    (
                        surface_clone.clone(),
                        popup,
                        (location_clone + offset),
                        alpha,
                    )
                },
            )
        },
    )
}

/// Get the z-order level of a layer (higher = closer to viewer)
/// Background=0, Bottom=1, Top=2, Overlay=3
fn layer_z_level(layer: Layer) -> u8 {
    match layer {
        Layer::Background => 0,
        Layer::Bottom => 1,
        Layer::Top => 2,
        Layer::Overlay => 3,
    }
}

fn layer_surfaces<'a>(
    output: &'a Output,
    layer: Layer,
    element_filter: &'a ElementFilter,
    home_visibility: &'a HomeVisibilityContext,
) -> impl Iterator<Item = (LayerSurface, Point<i32, Global>, f32, bool)> + 'a {
    // For BlurCapture and LayerBlurCapture modes, use cached layer surfaces to prevent deadlocks
    let use_cache = matches!(
        element_filter,
        ElementFilter::BlurCapture(_) | ElementFilter::LayerBlurCapture(_)
    );

    // For LayerBlurCapture, skip layers at or above the requesting layer's z-level
    // e.g., Bottom layer blur should only capture Background, not Bottom/Top/Overlay
    let skip_layer = match element_filter {
        ElementFilter::LayerBlurCapture(requesting_layer) => {
            layer_z_level(layer) >= layer_z_level(*requesting_layer)
        }
        _ => false,
    };

    // we want to avoid deadlocks on the layer-map in callbacks, so we need to clone the layer surfaces
    let layers = if skip_layer {
        // Skip this entire layer for layer blur capture
        Vec::new()
    } else if use_cache {
        // Use cached layer surfaces (populated by main thread)
        crate::backend::render::get_cached_layer_surfaces(&output.name(), layer)
            .into_iter()
            .map(|cached| (cached.surface, cached.location))
            .collect::<Vec<_>>()
    } else {
        let layer_map = layer_map_for_output(output);
        layer_map
            .layers_on(layer)
            .rev()
            .map(|s| (s.clone(), layer_map.layer_geometry(s).unwrap().loc))
            .collect::<Vec<_>>()
    };

    layers.into_iter().filter_map(move |(s, loc)| {
        // Filter out workspace overview namespace
        if *element_filter == ElementFilter::ExcludeWorkspaceOverview
            && s.namespace() == WORKSPACE_OVERVIEW_NAMESPACE
        {
            return None;
        }

        // Get visibility and alpha for this surface using home visibility context
        let surface_id = s.wl_surface().id().protocol_id();
        let (visible, alpha) = home_visibility.surface_visibility(surface_id);

        // Only skip blur/shadow when the surface is actually animating (alpha < 1.0)
        // This allows hide_on_home surfaces to have blur when fully visible
        let is_animating = alpha < 1.0;
        let has_home_visibility = home_visibility.home_only_surfaces.contains(&surface_id)
            || home_visibility.hide_on_home_surfaces.contains(&surface_id);
        let skip_blur = has_home_visibility && is_animating;

        // Filter out completely invisible surfaces
        if !visible {
            return None;
        }

        // Use the surface-specific alpha (which considers home_alpha for home-only surfaces)
        // Also return whether blur should be skipped (only when animating)
        Some((s, loc.as_local().to_global(output), alpha, skip_blur))
    })
}
