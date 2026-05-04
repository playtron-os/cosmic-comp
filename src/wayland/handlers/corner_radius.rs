use cosmic_protocols::corner_radius::v1::server::cosmic_corner_radius_toplevel_v1;

use crate::wayland::protocols::corner_radius::{
    CornerRadiusData, CornerRadiusHandler, CornerRadiusState, delegate_corner_radius,
};

use crate::state::State;

impl CornerRadiusHandler for State {
    fn corner_radius_state(&mut self) -> &mut CornerRadiusState {
        &mut self.common.corner_radius_state
    }

    fn set_corner_radius(
        &mut self,
        _: &cosmic_corner_radius_toplevel_v1::CosmicCornerRadiusToplevelV1,
        data: &CornerRadiusData,
    ) {
        force_redraw(self, data);
    }

    fn unset_corner_radius(
        &mut self,
        _: &cosmic_corner_radius_toplevel_v1::CosmicCornerRadiusToplevelV1,
        data: &CornerRadiusData,
    ) {
        force_redraw(self, data);
    }
}

fn force_redraw(state: &mut State, data: &CornerRadiusData) {
    let guard = data.lock().unwrap();
    let Some(toplevel) = guard.toplevel.upgrade().ok() else {
        return;
    };
    let Some(surface) = state.common.xdg_shell_state.get_toplevel(&toplevel) else {
        return;
    };
    let guard = state.common.shell.read();
    let Some(output) = guard.visible_output_for_surface(surface.wl_surface()) else {
        return;
    };
    state.backend.schedule_render(output);
}

delegate_corner_radius!(State);
