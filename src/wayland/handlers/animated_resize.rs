// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use crate::utils::prelude::*;
use crate::wayland::protocols::animated_resize::{
    AnimatedResizeHandler, AnimatedResizeState, delegate_animated_resize,
};
use smithay::reexports::wayland_server::Resource;
use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;
use smithay::utils::{Point, Rectangle, Size};
use tracing::{debug, info, warn};

impl AnimatedResizeHandler for State {
    fn animated_resize_state(&mut self) -> &mut AnimatedResizeState {
        &mut self.common.animated_resize_state
    }

    fn animated_resize_request(
        &mut self,
        surface: &WlSurface,
        target_width: i32,
        target_height: i32,
        _duration_ms: u32,
    ) {
        info!(
            surface_id = surface.id().protocol_id(),
            target_width, target_height, _duration_ms, "Animated resize request received"
        );

        // Find the CosmicMapped element for this surface
        let shell = self.common.shell.read();
        let Some(mapped) = shell.element_for_surface(surface).cloned() else {
            warn!(
                surface_id = surface.id().protocol_id(),
                "Could not find mapped element for surface"
            );
            return;
        };

        // Check if the window is maximized - if so, update the restore geometry instead of animating
        {
            let mut maximized_state = mapped.maximized_state.lock().unwrap();
            if let Some(ref mut state) = *maximized_state {
                // Window is maximized - update the original_geometry so it restores to this size
                // Also recalculate position to center on the output
                let target_size: Size<i32, Local> = (target_width, target_height).into();

                // Get output geometry to center the window
                let active_output = shell.seats.last_active().active_output();
                let output_geo = active_output.geometry();

                // Calculate centered position
                let center_x = (output_geo.size.w - target_width) / 2;
                let center_y = (output_geo.size.h - target_height) / 2;
                let centered_loc: Point<i32, Local> = (center_x, center_y).into();

                state.original_geometry = Rectangle::new(centered_loc, target_size);
                info!(
                    target_width,
                    target_height,
                    center_x,
                    center_y,
                    "Window is maximized, updated restore geometry to centered target size"
                );
                return;
            }
        }

        // Get the active output from the last active seat
        let active_output = shell.seats.last_active().active_output();
        let Some(workspace) = shell.active_space(&active_output) else {
            warn!("No active workspace");
            return;
        };
        // element_geometry returns Rectangle<i32, Local>
        let Some(current_geo) = workspace.element_geometry(&mapped) else {
            warn!("Could not get current geometry for mapped element");
            return;
        };

        // Get output geometry for boundary checking
        let output_geo = active_output.geometry();

        // Build target geometry - start with current position, change size
        let target_size: Size<i32, Local> = (target_width, target_height).into();

        // Calculate relative position of window center within output
        // relative_x = 0.0 means window center is at left edge, 1.0 means right edge
        let window_center_x = current_geo.loc.x + current_geo.size.w / 2;
        let relative_x = window_center_x as f32 / output_geo.size.w as f32;

        // Determine target position based on window's relative position:
        // - If window is centered or to the right (relative_x > 0.4), adjust x by half the width change
        //   to keep the window's center point stable during resize
        // - If window is to the left (relative_x <= 0.4), keep x position fixed (normal resize)
        // - Y coordinate is never modified - always keep current y position
        let mut target_x;
        let target_y = current_geo.loc.y;

        const CENTER_THRESHOLD: f32 = 0.4;
        if relative_x > CENTER_THRESHOLD {
            // Window is centered or to the right - move x by half the width difference
            // to keep the window's visual center stable
            let width_diff = current_geo.size.w - target_width;
            target_x = current_geo.loc.x + width_diff / 2;
            debug!(
                relative_x,
                width_diff,
                "Window is centered/right (> {}), adjusting x by half width diff", CENTER_THRESHOLD
            );
        } else {
            // Window is to the left - keep current x position (resize from current location)
            target_x = current_geo.loc.x;
            debug!(
                relative_x,
                "Window is to the left (<= {}), will resize without moving x", CENTER_THRESHOLD
            );
        }

        // Boundary constraints for x - keep window within output bounds horizontally
        // Note: y coordinates are never modified, so we don't apply y boundary constraints
        // Check right edge overflow
        if target_x + target_width > output_geo.size.w {
            target_x = (output_geo.size.w - target_width).max(0);
        }
        // Check left edge (in case window is positioned off-screen)
        if target_x < 0 {
            target_x = 0;
        }

        let target_loc: Point<i32, Local> = (target_x, target_y).into();
        let target_geometry: Rectangle<i32, Local> = Rectangle::new(target_loc, target_size);

        info!(
            current_x = current_geo.loc.x,
            current_y = current_geo.loc.y,
            current_width = current_geo.size.w,
            current_height = current_geo.size.h,
            target_x,
            target_y,
            target_width,
            target_height,
            "Starting client-driven resize animation with boundary constraints"
        );
        drop(shell); // Release read lock before getting write lock

        // Start the animation via the floating layer
        let mut shell = self.common.shell.write();
        if let Some(workspace) = shell.active_space_mut(&active_output) {
            workspace
                .floating_layer
                .start_client_driven_resize(mapped, target_geometry);
        }
    }

    fn animated_resize_request_with_position(
        &mut self,
        surface: &WlSurface,
        target_x: i32,
        target_y: i32,
        target_width: i32,
        target_height: i32,
        _duration_ms: u32,
    ) {
        info!(
            surface_id = surface.id().protocol_id(),
            target_x,
            target_y,
            target_width,
            target_height,
            _duration_ms,
            "Animated resize with position request received"
        );

        // Find the CosmicMapped element for this surface
        let shell = self.common.shell.read();
        let Some(mapped) = shell.element_for_surface(surface).cloned() else {
            warn!(
                surface_id = surface.id().protocol_id(),
                "Could not find mapped element for surface"
            );
            return;
        };

        // Get the active output from the last active seat
        let active_output = shell.seats.last_active().active_output();

        // Client sends logical coordinates - use directly
        let target_size: Size<i32, Local> = (target_width, target_height).into();
        let target_loc: Point<i32, Local> = (target_x, target_y).into();

        // Check if the window is maximized - if so, update the restore geometry instead of animating
        {
            let mut maximized_state = mapped.maximized_state.lock().unwrap();
            if let Some(ref mut state) = *maximized_state {
                // Window is maximized - update the original_geometry with client-specified position and size
                state.original_geometry = Rectangle::new(target_loc, target_size);
                info!(
                    target_x,
                    target_y,
                    target_width,
                    target_height,
                    "Window is maximized, updated restore geometry to client-specified position and size"
                );
                return;
            }
        }

        let Some(workspace) = shell.active_space(&active_output) else {
            warn!("No active workspace");
            return;
        };
        // element_geometry returns Rectangle<i32, Local>
        let Some(current_geo) = workspace.element_geometry(&mapped) else {
            warn!("Could not get current geometry for mapped element");
            return;
        };

        // Get output geometry for boundary clamping
        let output_geo = active_output.geometry();

        // Clamp position to keep window within output bounds
        let mut clamped_x = target_x;
        let mut clamped_y = target_y;

        // Check right edge overflow
        if clamped_x + target_width > output_geo.size.w {
            clamped_x = (output_geo.size.w - target_width).max(0);
        }
        // Check bottom edge overflow
        if clamped_y + target_height > output_geo.size.h {
            clamped_y = (output_geo.size.h - target_height).max(0);
        }
        // Check left edge
        if clamped_x < 0 {
            clamped_x = 0;
        }
        // Check top edge
        if clamped_y < 0 {
            clamped_y = 0;
        }

        let clamped_loc: Point<i32, Local> = (clamped_x, clamped_y).into();
        let target_geometry: Rectangle<i32, Local> = Rectangle::new(clamped_loc, target_size);

        info!(
            current_x = current_geo.loc.x,
            current_y = current_geo.loc.y,
            current_width = current_geo.size.w,
            current_height = current_geo.size.h,
            clamped_x,
            clamped_y,
            target_width,
            target_height,
            "Starting client-driven resize animation with explicit position"
        );
        drop(shell); // Release read lock before getting write lock

        // Start the animation via the floating layer
        let mut shell = self.common.shell.write();
        if let Some(workspace) = shell.active_space_mut(&active_output) {
            workspace
                .floating_layer
                .start_client_driven_resize(mapped, target_geometry);
        }
    }
}

delegate_animated_resize!(State);
