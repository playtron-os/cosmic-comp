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
        if namespace == crate::utils::quirks::GREETER_NAMESPACE {
            self.common.greeter_present = true;
            // Latch correction only: the greeter already connected, so the dir was open. Keeps
            // gate state honest if some logout path missed its restore.
            self.common.runtime_dir_gate.restore();
        }
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

        // Clean up visibility tracking for this surface
        shell.remove_surface_visibility(surface_id.clone());
        shell.remove_hidden_surface(&surface_id);
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

        let torn_output = maybe_output.clone();
        let mut torn_was_background = false;
        let mut torn_was_greeter = false;

        if let Some(output) = maybe_output {
            {
                let mut map = layer_map_for_output(&output);
                let layer = map
                    .layer_for_surface(surface.wl_surface(), WindowSurfaceType::TOPLEVEL)
                    .unwrap()
                    .clone();
                torn_was_background = matches!(layer.layer(), Layer::Background);
                torn_was_greeter = layer.namespace() == crate::utils::quirks::GREETER_NAMESPACE;
                map.unmap_layer(&layer);
            }

            // Update layer blur cache after unmapping

            shell.workspaces.recalculate();

            self.backend.schedule_render(&output);
        }

        // Persistent-compositor login handoff: if the greeter was torn down via a
        // non-dismiss path (its own 30s timeout, greetd's alarm, or a crash), the
        // per-commit dismiss gate would otherwise stay armed for the session. The
        // surface being destroyed has already been unmapped above, so re-derive
        // whether ANY greeter surface still remains (one per output) and disarm once
        // none do. Idempotent, and re-armed on re-login via `new_layer_surface`.
        if self.common.greeter_present
            && !shell.outputs().any(|o| {
                layer_map_for_output(o)
                    .layers()
                    .any(|l| l.namespace() == crate::utils::quirks::GREETER_NAMESPACE)
            })
        {
            self.common.greeter_present = false;
        }

        // If the GREETER was torn down while a LOGOUT crossfade latch was still set (greeter
        // crash / its own timeout — never the normal login handoff, which clears it in
        // `dismiss_greeter`), drop the latch + captured snapshot so a later logout starts clean.
        //
        // MUST gate on `torn_was_greeter`: during a NORMAL logout the desktop surfaces tear down
        // in a cascade — the wallpaper (Background) destroys first and arms the latch, then the
        // panel/home/etc.
        if torn_was_greeter
            && shell.greeter_logout_return()
            && !shell.outputs().any(|o| {
                layer_map_for_output(o)
                    .layers()
                    .any(|l| l.namespace() == crate::utils::quirks::GREETER_NAMESPACE)
            })
        {
            shell.clear_greeter_fade_in();
        }

        // Login / logout handoff START.
        std::mem::drop(shell);
        if let Some(output) = torn_output {
            if torn_was_greeter {
                self.note_possible_login_gap(&output);
            } else if torn_was_background {
                self.arm_logout_hold(&output);
            } else {
                self.note_possible_logout(&output);
            }
        }
    }
}
