// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    shell::Devices,
    shell::focus::target::{KeyboardFocusTarget, PointerFocusTarget},
    state::State,
    utils::prelude::SeatExt,
    wayland::handlers::surface_embed::is_wl_surface_embedded,
};
use smithay::{
    delegate_cursor_shape, delegate_seat,
    input::{
        SeatHandler, SeatState,
        keyboard::LedState,
        pointer::{CursorIcon, CursorImageStatus},
    },
    wayland::seat::WaylandFocus,
};

/// Check if a cursor icon is a resize cursor
fn is_resize_cursor(icon: CursorIcon) -> bool {
    matches!(
        icon,
        CursorIcon::EResize
            | CursorIcon::NResize
            | CursorIcon::NeResize
            | CursorIcon::NwResize
            | CursorIcon::SResize
            | CursorIcon::SeResize
            | CursorIcon::SwResize
            | CursorIcon::WResize
            | CursorIcon::EwResize
            | CursorIcon::NsResize
            | CursorIcon::NeswResize
            | CursorIcon::NwseResize
            | CursorIcon::ColResize
            | CursorIcon::RowResize
    )
}

impl SeatHandler for State {
    type KeyboardFocus = KeyboardFocusTarget;
    type PointerFocus = PointerFocusTarget;
    type TouchFocus = PointerFocusTarget;

    fn seat_state(&mut self) -> &mut SeatState<Self> {
        &mut self.common.seat_state
    }

    fn cursor_image(&mut self, seat: &smithay::input::Seat<Self>, image: CursorImageStatus) {
        // Block resize cursor requests from embedded windows
        if let CursorImageStatus::Named(icon) = &image {
            if is_resize_cursor(*icon) {
                // Check if the current pointer focus is an embedded surface
                if let Some(pointer) = seat.get_pointer() {
                    if let Some(focus) = pointer.current_focus() {
                        if let Some(wl_surface) = focus.wl_surface() {
                            if is_wl_surface_embedded(&wl_surface) {
                                return;
                            }
                        }
                    }
                }
            }
        }
        seat.set_cursor_image_status(image);
    }

    fn focus_changed(
        &mut self,
        _seat: &smithay::input::Seat<Self>,
        _focused: Option<&Self::KeyboardFocus>,
    ) {
    }

    fn led_state_changed(&mut self, seat: &smithay::input::Seat<Self>, led_state: LedState) {
        let userdata = seat.user_data();
        let devices = userdata.get::<Devices>().unwrap();
        devices.update_led_state(led_state);
    }
}

delegate_seat!(State);
delegate_cursor_shape!(State);
