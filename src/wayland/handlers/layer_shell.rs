// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    backend::render::{
        CachedLayerSurface, LayerBlurSurfaceInfo, set_cached_layer_surfaces,
        set_layer_blur_surfaces,
    },
    shell::PendingLayer,
    utils::prelude::*,
    wayland::protocols::blur::has_blur as surface_has_blur,
};
use smithay::{
    delegate_layer_shell,
    desktop::{LayerSurface, PopupKind, WindowSurfaceType, layer_map_for_output},
    output::Output,
    reexports::wayland_server::{Resource, protocol::wl_output::WlOutput},
    utils::IsAlive,
    wayland::shell::{
        wlr_layer::{
            Layer, LayerSurface as WlrLayerSurface, WlrLayerShellHandler, WlrLayerShellState,
        },
        xdg::PopupSurface,
    },
};

/// All layer types that need to be cached for render thread access
const CACHED_LAYERS: [Layer; 4] = [Layer::Background, Layer::Bottom, Layer::Top, Layer::Overlay];

/// Update the layer surface caches for an output.
/// Called from the main thread when layer surfaces change.
/// Populates both the blur cache and the general layer surface cache for render thread.
pub fn update_layer_blur_state(output: &Output) {
    let layer_map = layer_map_for_output(output);

    // Update blur surfaces cache
    let blur_surfaces: Vec<LayerBlurSurfaceInfo> = layer_map
        .layers()
        .filter(|layer| {
            let surface = layer.wl_surface();
            let alive = surface.alive();
            let has_blur = surface_has_blur(surface);
            tracing::trace!(
                surface_id = surface.id().protocol_id(),
                alive,
                has_blur,
                layer = ?layer.layer(),
                "Checking layer surface for blur"
            );
            alive && has_blur
        })
        .filter_map(|layer| {
            let geometry = layer_map.layer_geometry(layer)?;
            let layer_type = layer.layer();
            let surface_id = layer.wl_surface().id().protocol_id();

            Some(LayerBlurSurfaceInfo {
                surface_id,
                geometry,
                layer: layer_type,
            })
        })
        .collect();

    tracing::trace!(
        output = output.name(),
        blur_surface_count = blur_surfaces.len(),
        "Updated layer blur surfaces"
    );

    set_layer_blur_surfaces(&output.name(), blur_surfaces);

    // Update general layer surface cache for each layer type
    for layer_type in CACHED_LAYERS {
        let surfaces: Vec<CachedLayerSurface> = layer_map
            .layers_on(layer_type)
            .rev()
            .filter_map(|layer| {
                let geometry = layer_map.layer_geometry(layer)?;
                Some(CachedLayerSurface {
                    surface: layer.clone(),
                    location: geometry.loc,
                })
            })
            .collect();

        set_cached_layer_surfaces(&output.name(), layer_type, surfaces);
    }
}

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
        let output = wl_output
            .as_ref()
            .and_then(Output::from_resource)
            .unwrap_or_else(|| seat.active_output());
        shell.pending_layers.push(PendingLayer {
            surface: LayerSurface::new(surface, namespace),
            output,
            seat,
        });
    }

    fn new_popup(&mut self, _parent: WlrLayerSurface, popup: PopupSurface) {
        self.common.shell.read().unconstrain_popup(&popup);

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
        let surface_id = surface.wl_surface().id().protocol_id();
        let mut shell = self.common.shell.write();

        // Clean up visibility tracking for this surface
        shell.remove_surface_visibility(surface_id);
        shell.remove_hidden_surface(surface_id);

        let maybe_output = shell
            .outputs()
            .find(|o| {
                let map = layer_map_for_output(o);
                map.layer_for_surface(surface.wl_surface(), WindowSurfaceType::TOPLEVEL)
                    .is_some()
            })
            .cloned();

        if let Some(output) = maybe_output {
            {
                let mut map = layer_map_for_output(&output);
                let layer = map
                    .layer_for_surface(surface.wl_surface(), WindowSurfaceType::TOPLEVEL)
                    .unwrap()
                    .clone();
                map.unmap_layer(&layer);
            }

            // Update layer blur cache after unmapping
            update_layer_blur_state(&output);

            shell.workspaces.recalculate();

            self.backend.schedule_render(&output);
        }
    }
}

delegate_layer_shell!(State);
