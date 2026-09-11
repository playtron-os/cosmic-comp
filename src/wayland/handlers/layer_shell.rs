// SPDX-License-Identifier: GPL-3.0-only

use crate::{shell::PendingLayer, utils::prelude::*};
use smithay::{
    desktop::{LayerSurface, PopupKind, WindowSurfaceType, layer_map_for_output},
    output::Output,
    reexports::wayland_server::{Resource, protocol::wl_output::WlOutput},
    wayland::shell::{
        wlr_layer::{
            Layer, LayerSurface as WlrLayerSurface, WlrLayerShellHandler, WlrLayerShellState,
        },
        xdg::PopupSurface,
    },
};

impl WlrLayerShellHandler for State {
    fn shell_state(&mut self) -> &mut WlrLayerShellState {
        &mut self.common.layer_shell_state
    }

    fn new_layer_surface(
        &mut self,
        surface: WlrLayerSurface,
        wl_output: Option<WlOutput>,
        _layer: Layer,
        namespace: String,
    ) {
        let mut shell = self.common.shell.write();
        let seat = shell.seats.last_active().clone();
        let no_output = wl_output.is_none();
        let output = wl_output
            .as_ref()
            .and_then(Output::from_resource)
            .unwrap_or_else(|| seat.active_output());
        let layer_surface = LayerSurface::new(surface, namespace);
        if no_output {
            shell
                .output_agnostic_layers
                .insert(layer_surface.wl_surface().id());
        }
        shell.pending_layers.push(PendingLayer {
            surface: layer_surface,
            output,
            seat,
        });
    }

    fn new_popup(&mut self, _parent: WlrLayerSurface, popup: PopupSurface) {
        self.common
            .shell
            .read()
            .unconstrain_popup(&PopupKind::from(popup.clone()));

        if let Err(err) = popup.send_configure() {
            tracing::warn!("Unable to configure popup. {err:?}",);
        } else {
            self.common
                .popups
                .track_popup(PopupKind::from(popup))
                .unwrap();
        }
    }

    fn layer_destroyed(&mut self, surface: WlrLayerSurface) {
        let surface_id = surface.wl_surface().id();
        let mut shell = self.common.shell.write();

        // Read before the realm is forgotten below: whether this layer was on
        // screen decides whether its death may latch the frame.
        let torn_off_realm = shell.is_layer_off_realm(&surface_id);

        // Clean up visibility tracking for this surface
        shell.remove_hidden_surface(&surface_id);
        shell.remove_layer_realm(&surface_id);
        shell.remove_client_exclusive_zone(&surface_id);
        shell.remove_layer_fade_in(&surface_id);
        shell.remove_layer_fade_out(&surface_id);
        shell.remove_layer_open(&surface_id);
        shell.remove_layer_close(&surface_id);
        shell.layer_slides.retain(|s| s.surface_id != surface_id);
        shell.output_agnostic_layers.remove(&surface_id);
        shell.exclusive_focus_granted.remove(&surface_id);

        // Release this surface's blurred backdrop. It is a full-output-sized GPU
        // texture, and the cache is otherwise only pruned when a whole output
        // goes away, so skipping this leaks one texture per destroyed layer.

        // Clean up any edge-resize state for this surface: a panel destroyed mid
        // drag/animation must not leave a stuck ghost, grab target, spring or settle.
        // (A stuck settle in particular would keep the dispatch loop re-evaluating
        // edge hover every iteration forever.)
        if shell
            .edge_drag_ghost
            .as_ref()
            .is_some_and(|g| g.surface_id == surface_id)
        {
            shell.edge_drag_ghost = None;
        }
        if shell
            .edge_hover
            .as_ref()
            .is_some_and(|h| h.surface_id == surface_id)
        {
            shell.edge_hover = None;
        }
        if shell
            .active_layer_resize
            .as_ref()
            .is_some_and(|r| r.surface_id == surface_id)
        {
            shell.active_layer_resize = None;
        }
        if shell
            .layer_resize_settle
            .as_ref()
            .is_some_and(|r| r.surface_id == surface_id)
        {
            shell.layer_resize_settle = None;
        }
        if shell
            .active_layer_resize_anim
            .as_ref()
            .is_some_and(|a| a.surface_id == surface_id)
        {
            shell.active_layer_resize_anim = None;
        }
        if shell
            .layer_maximize
            .as_ref()
            .is_some_and(|m| m.surface_id == surface_id)
        {
            shell.layer_maximize = None;
        }

        let maybe_output = shell
            .outputs()
            .find(|o| {
                let map = layer_map_for_output(o);
                map.layer_for_surface(surface.wl_surface(), WindowSurfaceType::TOPLEVEL)
                    .is_some()
            })
            .cloned();

        if let Some(output) = maybe_output {
            let torn_layer;
            {
                let mut map = layer_map_for_output(&output);
                let layer = map
                    .layer_for_surface(surface.wl_surface(), WindowSurfaceType::TOPLEVEL)
                    .unwrap()
                    .clone();
                torn_layer = layer.layer();
                map.unmap_layer(&layer);
            }

            // Latch the current frame the instant the outgoing UI dies, before the
            // schedule_render below composites a content-less frame the freeze would
            // capture. Desktop: the wallpaper (Background) dies — a wallpaper CHANGE also
            // lands here and is released by the fresh map. Greeter: it has no wallpaper, so
            // an Overlay teardown in a session with no Background layer is the signal.
            // A kiosk child means this compositor IS the greeter (`cosmic-comp <greeter>`);
            // a desktop is spawned by cosmic-session with no child. Structural, unlike
            // "has no wallpaper", which is also true of a desktop whose cosmic-bg is
            // disabled or has crashed — there, every popup close would arm the hold.
            // A workspace's own wallpaper dies with it, deleted or gone cold;
            // off screen that changes nothing visible and must not park the
            // output for five seconds.
            let is_kiosk = self.common.kiosk_child.is_some();
            let should_arm = freeze_hold_arms(
                crate::freeze_on_exit_enabled(),
                shell.logout_hold,
                torn_layer,
                torn_off_realm,
                is_kiosk,
            );
            if should_arm {
                shell.logout_hold = true;
                tracing::debug!(
                    ?torn_layer,
                    "freeze hold: latching last frame for session handoff"
                );
                let _ = self.common.event_loop_handle.insert_source(
                    smithay::reexports::calloop::timer::Timer::from_duration(
                        std::time::Duration::from_secs(5),
                    ),
                    |_, _, state: &mut State| {
                        let mut shell = state.common.shell.write();
                        if shell.logout_hold {
                            tracing::warn!("logout hold: timeout, releasing");
                            shell.logout_hold = false;
                            let outputs = shell.outputs().cloned().collect::<Vec<_>>();
                            drop(shell);
                            for output in outputs {
                                state.backend.schedule_render(&output);
                            }
                        }
                        smithay::reexports::calloop::timer::TimeoutAction::Drop
                    },
                );
            }

            // Update layer blur cache after unmapping

            shell.workspaces_mut().recalculate();

            self.backend.schedule_render(&output);
        }
    }
}

/// Whether a layer surface's teardown latches the last frame for the session
/// handoff. Desktop: the wallpaper on screen dying — a wallpaper change lands
/// here too, and the fresh map releases it. The wallpaper of a workspace that
/// is off screen dies with that workspace, and nothing on screen changed.
/// Greeter: it has no wallpaper, so a Top or Overlay teardown is the signal.
fn freeze_hold_arms(
    enabled: bool,
    already_held: bool,
    torn: smithay::wayland::shell::wlr_layer::Layer,
    torn_off_realm: bool,
    is_kiosk: bool,
) -> bool {
    use smithay::wayland::shell::wlr_layer::Layer;
    if !enabled || already_held {
        return false;
    }
    match torn {
        Layer::Background => !torn_off_realm,
        Layer::Top | Layer::Overlay => is_kiosk,
        Layer::Bottom => false,
    }
}

#[cfg(test)]
mod tests {
    use super::freeze_hold_arms;
    use smithay::wayland::shell::wlr_layer::Layer;

    #[test]
    fn the_wallpaper_on_screen_dying_latches_the_frame() {
        assert!(freeze_hold_arms(
            true,
            false,
            Layer::Background,
            false,
            false
        ));
    }

    #[test]
    fn a_workspaces_wallpaper_dying_off_screen_holds_nothing() {
        assert!(!freeze_hold_arms(
            true,
            false,
            Layer::Background,
            true,
            false
        ));
    }

    #[test]
    fn only_the_greeter_arms_on_its_own_ui() {
        assert!(freeze_hold_arms(true, false, Layer::Overlay, false, true));
        assert!(freeze_hold_arms(true, false, Layer::Top, false, true));
        assert!(!freeze_hold_arms(true, false, Layer::Overlay, false, false));
        assert!(!freeze_hold_arms(true, false, Layer::Bottom, false, true));
    }

    #[test]
    fn a_hold_is_neither_doubled_nor_armed_when_disabled() {
        assert!(!freeze_hold_arms(
            true,
            true,
            Layer::Background,
            false,
            false
        ));
        assert!(!freeze_hold_arms(
            false,
            false,
            Layer::Background,
            false,
            false
        ));
    }
}
