// SPDX-License-Identifier: GPL-3.0-only

//! Handler implementation for the voice mode protocol

use crate::shell::SeatExt;
use crate::state::State;
use crate::utils::geometry::SizeExt;
use crate::utils::prelude::OutputExt;
use crate::wayland::protocols::voice_mode::{
    OrbState, VoiceModeHandler, VoiceModeState, delegate_voice_mode,
};
use smithay::desktop::space::SpaceElement;
use tracing::info;

impl VoiceModeHandler for State {
    fn voice_mode_state(&mut self) -> &mut VoiceModeState {
        &mut self.common.voice_mode_state
    }

    fn activate_voice_mode(&mut self, focused_app_id: Option<&str>) -> OrbState {
        info!(?focused_app_id, "Activating voice mode");

        let mut shell = self.common.shell.write();
        let seat = shell.seats.last_active().clone();
        let output = seat.active_output();

        // Determine which receiver to use and get window geometry if attaching
        let (receiver_app_id, window_geo) = if let Some(app_id) = focused_app_id {
            // Check if focused app has a registered receiver
            if self.common.voice_mode_state.has_receiver(app_id) {
                // Get the focused window's geometry
                let keyboard = seat.get_keyboard().unwrap();
                let geo = keyboard.current_focus().and_then(|focus| match &focus {
                    crate::shell::focus::target::KeyboardFocusTarget::Element(mapped) => {
                        Some(SpaceElement::geometry(mapped))
                    }
                    _ => None,
                });
                (Some(app_id.to_string()), geo)
            } else {
                // Focused app doesn't have a receiver, fall back to default
                (None, None)
            }
        } else {
            (None, None)
        };

        let orb_state = if let Some(app_id) = &receiver_app_id {
            if let Some(geo) = window_geo {
                // Attach orb to the focused window
                info!(app_id = %app_id, "Attaching orb to receiver window");
                let output_geo = output.geometry();
                shell
                    .voice_orb_state
                    .attach_to(geo, output_geo.size.as_logical());
                shell.voice_orb_state.app_id = Some(app_id.clone());

                // Send start to the window-specific receiver
                drop(shell);
                self.common
                    .voice_mode_state
                    .send_start(app_id, OrbState::Attached);

                // Send orb_attached event
                self.common
                    .voice_mode_state
                    .send_orb_attached(app_id, geo.loc.x, geo.loc.y, geo.size.w, geo.size.h);

                OrbState::Attached
            } else {
                // Shouldn't happen, but fall back to floating
                shell.voice_orb_state.show_floating();
                shell.enter_voice_mode();
                drop(shell);
                self.common
                    .voice_mode_state
                    .send_start_to_default(OrbState::Floating);
                OrbState::Floating
            }
        } else {
            // No focused receiver, use default receiver with floating orb
            info!("Showing floating orb (using default receiver)");
            shell.voice_orb_state.show_floating();
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
        info!("Deactivating voice mode");

        // Send stop to active receiver
        self.common.voice_mode_state.send_stop();

        let mut shell = self.common.shell.write();

        // Hide the orb
        shell.voice_orb_state.hide();
        shell.voice_orb_state.app_id = None;

        // Restore faded windows
        shell.exit_voice_mode();
    }

    fn cancel_voice_mode(&mut self) {
        info!("Cancelling voice mode");

        // Send cancel to active receiver
        self.common.voice_mode_state.send_cancel();

        let mut shell = self.common.shell.write();

        // Hide the orb
        shell.voice_orb_state.hide();
        shell.voice_orb_state.app_id = None;

        // Restore faded windows
        shell.exit_voice_mode();
    }
}

delegate_voice_mode!(State);
