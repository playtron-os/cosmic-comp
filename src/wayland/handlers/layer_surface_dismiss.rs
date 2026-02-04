// SPDX-License-Identifier: GPL-3.0-only

use crate::delegate_layer_surface_dismiss;
use crate::state::State;
use crate::wayland::protocols::layer_surface_dismiss::{
    zcosmic_layer_surface_dismiss_v1, LayerSurfaceDismissControllerData, LayerSurfaceDismissHandler,
    LayerSurfaceDismissState,
};
use smithay::reexports::wayland_server::Resource;
use std::collections::HashMap;
use std::sync::Mutex;

/// Runtime state for tracking armed dismiss controllers
pub struct DismissControllerRegistry {
    /// Map from surface ID to dismiss controller
    controllers: Mutex<
        HashMap<u32, zcosmic_layer_surface_dismiss_v1::ZcosmicLayerSurfaceDismissV1>,
    >,
}

impl std::fmt::Debug for DismissControllerRegistry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DismissControllerRegistry").finish()
    }
}

impl Default for DismissControllerRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl DismissControllerRegistry {
    pub fn new() -> Self {
        Self {
            controllers: Mutex::new(HashMap::new()),
        }
    }

    pub fn register(
        &self,
        controller: zcosmic_layer_surface_dismiss_v1::ZcosmicLayerSurfaceDismissV1,
    ) {
        if let Some(data) = controller.data::<LayerSurfaceDismissControllerData>() {
            if let Ok(surface) = data.surface.upgrade() {
                let surface_id = surface.id().protocol_id();
                self.controllers.lock().unwrap().insert(surface_id, controller);
            }
        }
    }

    pub fn unregister(&self, surface_id: u32) {
        self.controllers.lock().unwrap().remove(&surface_id);
    }

    pub fn get_all(&self) -> Vec<zcosmic_layer_surface_dismiss_v1::ZcosmicLayerSurfaceDismissV1> {
        self.controllers.lock().unwrap().values().cloned().collect()
    }
}

impl LayerSurfaceDismissHandler for State {
    fn layer_surface_dismiss_state(&self) -> &LayerSurfaceDismissState {
        &self.common.layer_surface_dismiss_state
    }

    fn register_dismiss_controller(
        &mut self,
        controller: zcosmic_layer_surface_dismiss_v1::ZcosmicLayerSurfaceDismissV1,
    ) {
        self.common.dismiss_controller_registry.register(controller);
    }

    fn unregister_dismiss_controller(&mut self, surface_id: u32) {
        self.common.dismiss_controller_registry.unregister(surface_id);
    }

    fn get_armed_dismiss_controllers(
        &self,
    ) -> Vec<zcosmic_layer_surface_dismiss_v1::ZcosmicLayerSurfaceDismissV1> {
        self.common.dismiss_controller_registry.get_all()
    }
}

delegate_layer_surface_dismiss!(State);
