// SPDX-License-Identifier: GPL-3.0-only

use smithay::{
    desktop::{
        MaxHeight, VerticalPlacement, layer_map_for_output, set_layer_max_height,
        set_layer_vertical_placement,
    },
    reexports::wayland_server::protocol::wl_surface::WlSurface,
};

use crate::{
    state::State,
    wayland::protocols::layer_surface_placement::{
        LayerSurfacePlacementHandler, LayerSurfacePlacementState, delegate_layer_surface_placement,
    },
};

impl LayerSurfacePlacementHandler for State {
    fn layer_surface_placement_state(&mut self) -> &mut LayerSurfacePlacementState {
        &mut self.common.layer_surface_placement_state
    }

    fn update_layer_surface_placement(
        &mut self,
        surface: &WlSurface,
        placement: Option<VerticalPlacement>,
    ) {
        // Record the placement on the surface; smithay's layer-map `arrange()`
        // honors it (and re-applies it on every relayout).
        set_layer_vertical_placement(surface, placement);
        self.relayout_surface_output(surface);
    }

    fn update_layer_max_height(&mut self, surface: &WlSurface, max_height: Option<MaxHeight>) {
        set_layer_max_height(surface, max_height);
        self.relayout_surface_output(surface);
    }
}

impl State {
    /// Re-arrange + redraw the output a layer surface is on, so a placement /
    /// height-cap change takes effect immediately rather than at the next client
    /// commit.
    fn relayout_surface_output(&mut self, surface: &WlSurface) {
        let shell = self.common.shell.read();
        if let Some(output) = shell.visible_output_for_surface(surface) {
            layer_map_for_output(output).arrange();
            self.backend.schedule_render(output);
        }
    }
}

delegate_layer_surface_placement!(State);
