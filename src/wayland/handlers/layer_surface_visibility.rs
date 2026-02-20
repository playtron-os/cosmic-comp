// SPDX-License-Identifier: GPL-3.0-only

use crate::delegate_layer_surface_visibility;
use crate::shell::Shell;
use crate::shell::focus::target::KeyboardFocusTarget;
use crate::state::State;
use crate::wayland::protocols::layer_surface_visibility::{
    LayerSurfaceVisibilityHandler, LayerSurfaceVisibilityState,
};
use smithay::desktop::layer_map_for_output;
use smithay::reexports::wayland_server::Resource;
use smithay::wayland::compositor::with_states;
use smithay::wayland::shell::wlr_layer::{KeyboardInteractivity, Layer, LayerSurfaceCachedState};
use wayland_backend::server::ObjectId;

impl LayerSurfaceVisibilityHandler for State {
    fn layer_surface_visibility_state(&self) -> &LayerSurfaceVisibilityState {
        &self.common.layer_surface_visibility_state
    }

    fn set_surface_hidden(&mut self, surface_id: ObjectId, hidden: bool) {
        let mut shell = self.common.shell.write();
        shell.set_surface_hidden(surface_id.clone(), hidden);

        if hidden {
            // When hiding a surface, clear keyboard focus from it so it
            // doesn't steal interactivity while invisible.
            let seats_to_clear: Vec<_> = shell
                .seats
                .iter()
                .filter_map(|seat| {
                    let keyboard = seat.get_keyboard()?;
                    if let Some(KeyboardFocusTarget::LayerSurface(ref layer)) =
                        keyboard.current_focus()
                    {
                        if layer.wl_surface().id() == surface_id {
                            return Some(seat.clone());
                        }
                    }
                    None
                })
                .collect();
            std::mem::drop(shell);

            for seat in seats_to_clear {
                Shell::set_focus(self, None, &seat, None, false);
            }
        } else {
            // Surface becoming visible — grant keyboard focus if it has
            // exclusive/on-demand interactivity on a Top or Overlay layer
            // (e.g. the launcher overlay).
            let focus_target = 'target: {
                for output in shell.outputs().cloned().collect::<Vec<_>>() {
                    let map = layer_map_for_output(&output);
                    for layer in map.layers() {
                        if layer.wl_surface().id() == surface_id {
                            let wants_focus = with_states(layer.wl_surface(), |states| {
                                let mut cached =
                                    states.cached_state.get::<LayerSurfaceCachedState>();
                                let current = cached.current();
                                matches!(current.layer, Layer::Top | Layer::Overlay)
                                    && current.keyboard_interactivity != KeyboardInteractivity::None
                            });
                            if wants_focus {
                                let seat = shell.seats.last_active().clone();
                                let target: KeyboardFocusTarget = layer.clone().into();
                                break 'target Some((target, seat));
                            }
                        }
                    }
                }
                None
            };
            std::mem::drop(shell);

            if let Some((target, seat)) = focus_target {
                Shell::set_focus(self, Some(&target), &seat, None, false);
            }
        }
    }

    fn is_surface_hidden(&self, surface_id: &ObjectId) -> bool {
        let shell = self.common.shell.read();
        shell.is_surface_hidden(surface_id)
    }
}

delegate_layer_surface_visibility!(State);
