// SPDX-License-Identifier: GPL-3.0-only

use crate::state::{BackendData, State};
use crate::utils::prelude::SeatExt;
use crate::wayland::handlers::surface_embed::get_parent_surface_id;
use crate::wayland::protocols::exclusive_mode::{
    EnableExclusiveModeResult, ExclusiveModeHandler, ExclusiveModeState, delegate_exclusive_mode,
};
use smithay::backend::session::Session;
use smithay::reexports::wayland_server::Resource;
use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;
use smithay::wayland::seat::WaylandFocus;
use tracing::{debug, info, warn};

impl ExclusiveModeHandler for State {
    fn exclusive_mode_state(&mut self) -> &mut ExclusiveModeState {
        &mut self.common.exclusive_mode_state
    }

    fn enable_exclusive_mode(&mut self, surface: &WlSurface) -> EnableExclusiveModeResult {
        info!(
            surface_id = surface.id().protocol_id(),
            "Enabling exclusive mode - minimizing other windows"
        );

        // Check if session is active in KMS mode - don't minimize while session is paused
        // as this can cause state inconsistencies when the session resumes
        let session_active = match &self.backend {
            BackendData::Kms(kms) => kms.session.is_active(),
            _ => true, // Non-KMS backends are always "active"
        };

        if !session_active {
            warn!(
                surface_id = surface.id().protocol_id(),
                "Session is not active, rejecting exclusive mode"
            );
            return EnableExclusiveModeResult::SessionPaused;
        }

        let mut shell = self.common.shell.write();

        // Find the window that requested exclusive mode
        let Some(exclusive_mapped) = shell.element_for_surface(surface).cloned() else {
            warn!(
                surface_id = surface.id().protocol_id(),
                "Could not find mapped element for exclusive mode surface"
            );
            return EnableExclusiveModeResult::SurfaceNotFound;
        };

        let exclusive_surface = exclusive_mapped.active_window();

        // Get the exclusive surface's ID (used to check if other windows are embedded in it)
        let exclusive_surface_id = surface.id().to_string();

        // Get the output for this window
        let active_output = shell.seats.last_active().active_output();

        // Collect surfaces of all other visible windows on this output
        let mut surfaces_to_minimize = Vec::new();

        if let Some(workspace) = shell.active_space(&active_output) {
            // Get all mapped elements except the one requesting exclusive mode
            // and except windows that are embedded in the exclusive surface
            for mapped in workspace.mapped() {
                let surface = mapped.active_window();
                if surface != exclusive_surface {
                    // Skip windows that are embedded in the exclusive surface
                    if let Some(parent_id) = get_parent_surface_id(&surface)
                        && parent_id == exclusive_surface_id
                    {
                        debug!(
                            window_app_id = %surface.app_id(),
                            "Skipping embedded child of exclusive surface"
                        );
                        continue;
                    }
                    surfaces_to_minimize.push(surface.clone());
                }
            }
        }

        // Now minimize each window using the shell's API
        // We need to drop the workspace borrow first
        let mut minimized_ids = Vec::new();

        for surface in surfaces_to_minimize {
            let window_id = surface
                .wl_surface()
                .map(|s| s.id().protocol_id() as u64)
                .unwrap_or(0);

            if window_id == 0 {
                continue;
            }

            debug!(window_id, "Minimizing window for exclusive mode");
            shell.minimize_request(&surface);
            minimized_ids.push(window_id);
        }

        info!(
            count = minimized_ids.len(),
            "Exclusive mode enabled, minimized windows"
        );

        EnableExclusiveModeResult::Enabled(minimized_ids)
    }

    fn disable_exclusive_mode(&mut self, surface: &WlSurface, window_ids: &[u64]) {
        info!(
            surface_id = surface.id().protocol_id(),
            count = window_ids.len(),
            "Disabling exclusive mode - restoring windows"
        );

        let mut shell = self.common.shell.write();
        let seat = shell.seats.last_active().clone();

        // Find minimized surfaces by their IDs and unminimize them
        // We need to collect surfaces first to avoid borrow conflicts
        let mut surfaces_to_restore = Vec::new();

        for set in shell.workspaces.sets.values() {
            for mw in &set.minimized_windows {
                let surface = mw.active_window();
                let window_id = surface
                    .wl_surface()
                    .map(|s| s.id().protocol_id() as u64)
                    .unwrap_or(0);

                if window_ids.contains(&window_id) {
                    surfaces_to_restore.push(surface.clone());
                }
            }
        }

        for workspace in shell.workspaces.spaces() {
            for mw in &workspace.minimized_windows {
                let surface = mw.active_window();
                let window_id = surface
                    .wl_surface()
                    .map(|s| s.id().protocol_id() as u64)
                    .unwrap_or(0);

                if window_ids.contains(&window_id) {
                    surfaces_to_restore.push(surface.clone());
                }
            }
        }

        // Now unminimize each surface
        for surface in surfaces_to_restore {
            let window_id = surface
                .wl_surface()
                .map(|s| s.id().protocol_id() as u64)
                .unwrap_or(0);
            debug!(window_id, "Restoring window from exclusive mode");
            shell.unminimize_request(&surface, &seat, &self.common.event_loop_handle);
        }

        info!("Exclusive mode disabled, windows restored");
    }
}

delegate_exclusive_mode!(State);
