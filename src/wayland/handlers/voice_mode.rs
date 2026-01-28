// SPDX-License-Identifier: GPL-3.0-only

//! Handler implementation for the voice mode protocol

use crate::shell::SeatExt;
use crate::shell::grabs::SeatMoveGrabState;
use crate::state::State;
use crate::utils::geometry::{PointExt, RectGlobalExt, RectLocalExt, SizeExt};
use crate::utils::prelude::OutputExt;
use crate::wayland::protocols::voice_mode::{
    OrbState, VoiceModeHandler, VoiceModeState, delegate_voice_mode,
};
use smithay::desktop::space::SpaceElement;
use smithay::reexports::wayland_server::Resource;
use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;
use smithay::wayland::seat::WaylandFocus;
use tracing::info;

impl VoiceModeHandler for State {
    fn voice_mode_state(&mut self) -> &mut VoiceModeState {
        &mut self.common.voice_mode_state
    }

    fn activate_voice_mode(&mut self, focused_surface: Option<&WlSurface>) -> OrbState {
        info!(?focused_surface, "Activating voice mode");

        // Exit home mode when voice mode activates (orb should appear over everything)
        self.common.home_visibility_state.set_home(false);

        let mut shell = self.common.shell.write();

        // Also exit the shell's internal home mode so home_alpha goes to 0
        // This ensures home-only surfaces are hidden during voice mode
        shell.exit_home_visual_only();

        let seat = shell.seats.last_active().clone();

        // Find output under pointer for floating orb placement
        let pointer_pos = seat.get_pointer().map(|ptr| ptr.current_location());
        let output_under_pointer = pointer_pos.and_then(|pos| {
            shell
                .outputs()
                .find(|out| out.geometry().to_f64().contains(pos.as_global()))
                .cloned()
        });

        // Use the output under the pointer if available, otherwise fall back to active_output
        let output = output_under_pointer.unwrap_or_else(|| seat.active_output());

        // Check if a window is currently being grabbed/dragged
        let grabbed_window_info: Option<(
            WlSurface,
            smithay::utils::Rectangle<i32, smithay::utils::Logical>,
            String,
        )> = seat
            .user_data()
            .get::<SeatMoveGrabState>()
            .and_then(|grab_state| {
                grab_state.lock().ok().and_then(|guard| {
                    guard.as_ref().and_then(|state| {
                        let mapped = state.element();
                        let surface = mapped.active_window().wl_surface()?.into_owned();
                        let surface_id = surface.id().to_string();
                        // Get the grabbed window's current geometry (SpaceElement::geometry returns Logical)
                        let geo = mapped.geometry();
                        Some((surface, geo, surface_id))
                    })
                })
            });

        // Determine which receiver to use and get window geometry if attaching
        // Priority: grabbed window > focused window > default receiver
        let (receiver_surface, window_geo) = if let Some((grabbed_surface, geo, surface_id)) =
            &grabbed_window_info
        {
            // Check if grabbed window has a voice receiver
            if self
                .common
                .voice_mode_state
                .has_receiver_for_surface(grabbed_surface)
            {
                info!("Using grabbed window as voice receiver");
                (
                    Some(grabbed_surface.clone()),
                    Some((geo.clone(), surface_id.clone())),
                )
            } else if let Some(surface) = focused_surface {
                // Fall back to focused surface check
                if self
                    .common
                    .voice_mode_state
                    .has_receiver_for_surface(surface)
                {
                    // Get the focused window's geometry (not grabbed)
                    let keyboard = seat.get_keyboard().unwrap();
                    let geo_and_id = keyboard.current_focus().and_then(|focus| match &focus {
                        crate::shell::focus::target::KeyboardFocusTarget::Element(mapped) => {
                            let workspace = shell.active_space(&output)?;
                            let local_geo = workspace.element_geometry(mapped)?;
                            let geo = local_geo.to_global(&output).as_logical();
                            let surface_id = mapped.active_window().wl_surface()?.id().to_string();
                            Some((geo, surface_id))
                        }
                        _ => None,
                    });
                    (Some(surface.clone()), geo_and_id)
                } else {
                    (None, None)
                }
            } else {
                (None, None)
            }
        } else if let Some(surface) = focused_surface {
            // No grabbed window, check focused surface
            if self
                .common
                .voice_mode_state
                .has_receiver_for_surface(surface)
            {
                // Get the focused window's geometry and surface ID from the workspace
                let keyboard = seat.get_keyboard().unwrap();
                let geo_and_id = keyboard.current_focus().and_then(|focus| match &focus {
                    crate::shell::focus::target::KeyboardFocusTarget::Element(mapped) => {
                        // Get element geometry from workspace (gives position in workspace coordinates)
                        let workspace = shell.active_space(&output)?;
                        let local_geo = workspace.element_geometry(mapped)?;
                        // Convert to global/logical coordinates for rendering
                        let geo = local_geo.to_global(&output).as_logical();
                        // Get surface ID for reliable window matching during render
                        let surface_id = mapped.active_window().wl_surface()?.id().to_string();
                        Some((geo, surface_id))
                    }
                    _ => None,
                });
                (Some(surface.clone()), geo_and_id)
            } else {
                // Focused surface doesn't have a receiver, fall back to default
                (None, None)
            }
        } else {
            // No focused surface - check for maximized windows with voice receivers
            // This handles the case where a chat window is maximized but not focused
            let maximized_receiver = shell.active_space(&output).and_then(|workspace| {
                workspace.mapped().find_map(|mapped| {
                    // Check if window is maximized
                    if !mapped.is_maximized(false) {
                        return None;
                    }
                    // Check if it has a voice receiver
                    let active_window = mapped.active_window();
                    let wl_surface = active_window.wl_surface()?;
                    if !self
                        .common
                        .voice_mode_state
                        .has_receiver_for_surface(&wl_surface)
                    {
                        return None;
                    }
                    // Get geometry
                    let local_geo = workspace.element_geometry(mapped)?;
                    let geo = local_geo.to_global(&output).as_logical();
                    let surface_id = wl_surface.id().to_string();
                    info!("Found maximized window with voice receiver");
                    Some((wl_surface.into_owned(), geo, surface_id))
                })
            });

            if let Some((surface, geo, surface_id)) = maximized_receiver {
                (Some(surface), Some((geo, surface_id)))
            } else {
                (None, None)
            }
        };

        let orb_state = if let Some(ref surface) = receiver_surface {
            if let Some((geo, surface_id)) = window_geo {
                // Attach orb to the focused window
                info!(
                    "Attaching orb to receiver surface at {:?} (surface_id: {})",
                    geo, surface_id
                );
                let output_geo = output.geometry();
                // Request show attached - orb will burst directly in window
                shell.voice_orb_state.request_show_attached(
                    geo,
                    output_geo.size.as_logical(),
                    surface_id,
                );
                shell.enter_voice_mode();

                // Send start to the window-specific receiver
                drop(shell);
                self.common
                    .voice_mode_state
                    .send_start_to_surface(surface, OrbState::Attached);

                // Send orb_attached event
                self.common
                    .voice_mode_state
                    .send_orb_attached(surface, geo.loc.x, geo.loc.y, geo.size.w, geo.size.h);

                OrbState::Attached
            } else {
                // Shouldn't happen, but fall back to floating
                shell.voice_orb_state.request_show_floating(output.name());
                shell.enter_voice_mode();
                drop(shell);
                self.common
                    .voice_mode_state
                    .send_start_to_default(OrbState::Floating);
                OrbState::Floating
            }
        } else {
            // No focused receiver, use default receiver with floating orb
            info!(
                "Showing floating orb (using default receiver) on output: {}",
                output.name()
            );
            // Request orb show - will start after window fade completes
            shell.voice_orb_state.request_show_floating(output.name());
            shell.enter_voice_mode();
            drop(shell);
            self.common
                .voice_mode_state
                .send_start_to_default(OrbState::Floating);
            OrbState::Floating
        };

        orb_state
    }

    fn deactivate_voice_mode(&mut self) {
        info!("Deactivating voice mode - sending will_stop to client");

        // Send will_stop to active receiver and wait for ack
        if self.common.voice_mode_state.send_will_stop().is_none() {
            // No active receiver, just complete deactivation immediately
            info!("No active receiver, completing deactivation immediately");
            self.complete_deactivation();
        }
        // Otherwise, wait for ack_stop from client (or timeout)
    }

    fn cancel_voice_mode(&mut self) {
        info!("Cancelling voice mode");

        // Send cancel to active receiver
        self.common.voice_mode_state.send_cancel();

        let mut shell = self.common.shell.write();

        // Request orb hide - will start shrinking, then windows fade in
        shell.voice_orb_state.request_hide();

        // Start the exit sequence (orb shrinks first, then windows fade in)
        shell.exit_voice_mode();
    }

    fn freeze_orb(&mut self) {
        info!("Freezing orb for transcription processing (client requested freeze via ack_stop)");

        // Send stop to active receiver so it can trigger transcription
        self.common.voice_mode_state.send_stop();

        let mut shell = self.common.shell.write();

        // Freeze the orb in place (stops pulsing, stays visible)
        shell.voice_orb_state.freeze();

        // Update protocol state
        drop(shell);
        self.common.voice_mode_state.set_orb_state(OrbState::Frozen);
    }

    fn complete_deactivation(&mut self) {
        info!("Completing voice mode deactivation (proceeding with hide)");

        // Send stop to active receiver
        self.common.voice_mode_state.send_stop();

        let mut shell = self.common.shell.write();

        // Request orb hide - will start shrinking, then windows fade in
        shell.voice_orb_state.request_hide();

        // Start the exit sequence (orb shrinks first, then windows fade in)
        shell.exit_voice_mode();
    }

    fn check_pending_stop_timeout(&mut self) {
        if self.common.voice_mode_state.has_pending_stop_timeout() {
            info!("will_stop timeout - completing deactivation");
            self.complete_deactivation();
        }
    }

    fn on_voice_receiver_registered(&mut self, surface: &WlSurface) {
        // Check if orb is currently frozen - if so, we need to transition to the new window
        let shell = self.common.shell.read();
        if shell.voice_orb_state.orb_state != OrbState::Frozen {
            return;
        }
        drop(shell);

        info!("Voice receiver registered while orb is frozen - checking for transition");

        // Find the window that owns this surface
        let shell = self.common.shell.read();
        let seat = shell.seats.last_active().clone();
        let output = seat.active_output();

        // Search for the mapped element that contains this surface
        let workspace = shell.active_space(&output);
        let window_info = workspace.and_then(|ws| {
            ws.mapped().find_map(|mapped| {
                if let Some(wl_surface) = mapped.active_window().wl_surface() {
                    if wl_surface.id() == surface.id() {
                        let window_geo = SpaceElement::geometry(mapped);
                        let surface_id = wl_surface.id().to_string();
                        return Some((window_geo, surface_id));
                    }
                }
                None
            })
        });
        drop(shell);

        if let Some((window_geo, surface_id)) = window_info {
            info!(
                ?window_geo,
                surface_id, "Transitioning frozen orb to newly registered receiver window"
            );

            let mut shell = self.common.shell.write();
            let output_size = output.geometry().size.as_logical();

            // Start the attach_and_transition animation
            shell
                .voice_orb_state
                .start_attach_and_transition(window_geo, output_size, surface_id);

            // Fade windows back in
            shell.voice_mode_fade_in_immediately();
        } else {
            info!("Could not find window for registered voice receiver surface");
        }
    }
}

delegate_voice_mode!(State);
