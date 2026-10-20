// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    state::State,
    utils::prelude::{Global, PointExt, PointGlobalExt, RectGlobalExt, SeatExt},
};
use smithay::{
    desktop::utils::bbox_from_surface_tree,
    input::{
        Seat,
        dnd::{DnDGrab, DndGrabHandler, DndTarget, GrabType},
        pointer::{CursorImageStatus, CursorImageSurfaceData, Focus},
    },
    reexports::wayland_server::protocol::wl_surface::WlSurface,
    utils::{IsAlive, Logical, Point, Rectangle},
    wayland::{
        compositor::{self, SurfaceAttributes},
        selection::data_device::{DataDeviceHandler, DataDeviceState, WaylandDndGrabHandler},
    },
};
use std::sync::Mutex;

#[derive(Debug, Clone)]
pub struct DnDIcon {
    pub surface: WlSurface,
    pub offset: Point<i32, Logical>,
}

impl DnDIcon {
    /// The icon's offset from the pointer, kept on screen (see [`keep_on_screen`]).
    pub fn placed_offset(
        &self,
        pointer: Point<f64, Global>,
        outputs: &[Rectangle<i32, Global>],
    ) -> Point<i32, Logical> {
        let bbox = bbox_from_surface_tree(&self.surface, self.offset);
        let outputs = outputs.iter().map(|o| o.as_logical()).collect::<Vec<_>>();
        let at = keep_on_screen(pointer.to_i32_round().as_logical(), bbox, &outputs);
        self.offset + (at - bbox.loc)
    }
}

/// Where an icon at `icon` (relative to the pointer) goes so the person can see it: where its
/// client put it, else mirrored across the pointer (above, left, or both), else pushed inside
/// the pointer's output. Mirroring, unlike pushing, keeps it off what is under the pointer.
pub fn keep_on_screen(
    pointer: Point<i32, Logical>,
    icon: Rectangle<i32, Logical>,
    outputs: &[Rectangle<i32, Logical>],
) -> Point<i32, Logical> {
    let Rectangle { loc, size } = icon;
    if size.w <= 0 || size.h <= 0 || outputs.is_empty() {
        return loc;
    }
    let mirrored = Point::from((-loc.x - size.w, -loc.y - size.h));
    let visible = |at: Point<i32, Logical>| {
        let (x, y) = (pointer.x + at.x, pointer.y + at.y);
        let corners = [
            (x, y),
            (x + size.w - 1, y),
            (x, y + size.h - 1),
            (x + size.w - 1, y + size.h - 1),
        ];
        corners
            .into_iter()
            .all(|c| outputs.iter().any(|o| o.contains(Point::from(c))))
    };
    let candidates = [
        loc,
        Point::from((loc.x, mirrored.y)),
        Point::from((mirrored.x, loc.y)),
        mirrored,
    ];
    if let Some(at) = candidates.into_iter().find(|at| visible(*at)) {
        return at;
    }
    let output = outputs
        .iter()
        .find(|o| o.contains(pointer))
        .unwrap_or(&outputs[0]);
    let x = (pointer.x + loc.x)
        .min(output.loc.x + output.size.w - size.w)
        .max(output.loc.x);
    let y = (pointer.y + loc.y)
        .min(output.loc.y + output.size.h - size.h)
        .max(output.loc.y);
    Point::from((x - pointer.x, y - pointer.y))
}

pub fn get_dnd_icon(seat: &Seat<State>) -> Option<DnDIcon> {
    let userdata = seat.user_data();
    userdata
        .get::<Mutex<Option<DnDIcon>>>()
        .and_then(|x| x.lock().unwrap().clone())
        .filter(|icon| icon.surface.alive())
}

pub fn on_commit(surface: &WlSurface, seat: &Seat<State>) {
    let userdata = seat.user_data();

    let Some(mut guard) = userdata
        .get::<Mutex<Option<DnDIcon>>>()
        .map(|guard| guard.lock().unwrap())
    else {
        return;
    };

    let Some(icon) = guard.as_mut() else {
        return;
    };

    if &icon.surface == surface {
        compositor::with_states(surface, |states| {
            let buffer_delta = states
                .cached_state
                .get::<SurfaceAttributes>()
                .current()
                .buffer_delta
                .take()
                .unwrap_or_default();
            icon.offset += buffer_delta;
        });
    }
}

impl WaylandDndGrabHandler for State {
    fn dnd_requested<S: smithay::input::dnd::Source>(
        &mut self,
        source: S,
        icon: Option<WlSurface>,
        seat: Seat<Self>,
        serial: smithay::utils::Serial,
        type_: GrabType,
    ) {
        let source = crate::input::dnd_hover::started(self, &seat, source);
        let user_data = seat.user_data();
        user_data.insert_if_missing_threadsafe::<Mutex<Option<DnDIcon>>, _>(Default::default);

        let offset = if let CursorImageStatus::Surface(ref surface) = seat.cursor_image_status() {
            compositor::with_states(surface, |states| {
                let hotspot = states
                    .data_map
                    .get::<CursorImageSurfaceData>()
                    .unwrap()
                    .lock()
                    .unwrap()
                    .hotspot;
                Point::from((-hotspot.x, -hotspot.y))
            })
        } else {
            (0, 0).into()
        };

        *user_data
            .get::<Mutex<Option<DnDIcon>>>()
            .unwrap()
            .lock()
            .unwrap() = icon.map(|surface| DnDIcon { surface, offset });

        match type_ {
            GrabType::Pointer => {
                let pointer = seat.get_pointer().unwrap();
                let start_data = pointer.grab_start_data().unwrap();
                pointer.set_grab(
                    self,
                    DnDGrab::new_pointer(&self.common.display_handle, start_data, source, seat),
                    serial,
                    Focus::Keep,
                );
            }
            GrabType::Touch => {
                let touch = seat.get_touch().unwrap();
                let start_data = touch.grab_start_data().unwrap();
                touch.set_grab(
                    self,
                    DnDGrab::new_touch(&self.common.display_handle, start_data, source, seat),
                    serial,
                );
            }
        }
    }
}

impl DndGrabHandler for State {
    fn dropped(
        &mut self,
        target: Option<DndTarget<'_, Self>>,
        validated: bool,
        seat: Seat<Self>,
        location: Point<f64, Logical>,
    ) {
        clear_dnd_icon(&seat);
        if validated && target.is_some() {
            crate::input::dnd_hover::dropped(self, &seat, location.as_global());
        } else {
            crate::input::dnd_hover::ended(self, &seat);
        }
    }

    fn cancelled(&mut self, seat: Seat<Self>, _location: Point<f64, Logical>) {
        crate::input::dnd_hover::ended(self, &seat);
        // A cancelled grab (e.g. the source destroying its data source) never drops, so this is
        // the only chance to drop the icon. Clients commonly keep the icon surface alive for the
        // next drag, so relying on it dying would leave it painted on the cursor.
        clear_dnd_icon(&seat);

        // Unlike a drop, a cancel isn't driven by input, so nothing else damages the cursor plane.
        let outputs = self
            .common
            .shell
            .read()
            .outputs()
            .cloned()
            .collect::<Vec<_>>();
        for output in outputs {
            self.backend.schedule_render(&output);
        }
    }
}

fn clear_dnd_icon(seat: &Seat<State>) {
    if let Some(icon) = seat.user_data().get::<Mutex<Option<DnDIcon>>>() {
        icon.lock().unwrap().take();
    }
}

impl DataDeviceHandler for State {
    fn data_device_state(&mut self) -> &mut DataDeviceState {
        &mut self.common.data_device_state
    }
}

#[cfg(test)]
mod tests {
    use super::keep_on_screen;
    use smithay::utils::{Logical, Rectangle};

    fn screen() -> Rectangle<i32, Logical> {
        Rectangle::new((0, 0).into(), (1440, 1000).into())
    }
    // The capture ghost: drawn from the pointer with a 16px clear margin on every side.
    fn ghost() -> Rectangle<i32, Logical> {
        Rectangle::new((0, 0).into(), (176, 150).into())
    }

    fn place(x: i32, y: i32, icon: Rectangle<i32, Logical>) -> (i32, i32) {
        let at = keep_on_screen((x, y).into(), icon, &[screen()]);
        (at.x, at.y)
    }

    #[test]
    fn an_icon_with_room_stays_where_its_client_put_it() {
        assert_eq!(place(700, 500, ghost()), (0, 0));
        assert_eq!(place(0, 0, ghost()), (0, 0));
    }

    #[test]
    fn over_the_dock_the_icon_goes_above_the_pointer() {
        // A dock icon near the bottom: below the pointer would leave the screen.
        assert_eq!(place(806, 967, ghost()), (0, -150));
    }

    #[test]
    fn at_the_right_edge_the_icon_goes_left_of_the_pointer() {
        assert_eq!(place(1400, 500, ghost()), (-176, 0));
    }

    #[test]
    fn in_the_bottom_right_corner_the_icon_goes_above_and_left() {
        assert_eq!(place(1439, 999, ghost()), (-176, -150));
    }

    #[test]
    fn a_centred_icon_is_pushed_inside_the_screen() {
        // Mirroring a centred icon changes nothing, so it slides inside instead.
        let icon = Rectangle::new((-20, -20).into(), (40, 40).into());
        assert_eq!(place(1430, 995, icon), (-30, -35));
        assert_eq!(place(5, 3, icon), (-5, -3));
    }

    #[test]
    fn an_icon_may_cross_onto_the_next_output() {
        let right = Rectangle::new((1440, 0).into(), (1440, 1000).into());
        let at = keep_on_screen((1400, 500).into(), ghost(), &[screen(), right]);
        assert_eq!((at.x, at.y), (0, 0));
    }
}
