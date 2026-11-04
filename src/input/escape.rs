// SPDX-License-Identifier: GPL-3.0-only

use smithay::input::{
    SeatHandler,
    pointer::{ClickGrab, PointerGrab, PointerHandle},
};

/// Whether Escape ends the pointer's grab instead of reaching the client. A button held on
/// a client's surface is the app's own drag, and `client_grab` names the client's other grabs.
pub fn ends_pointer_grab<D: SeatHandler + 'static>(
    pointer: &PointerHandle<D>,
    client_grab: impl Fn(&dyn PointerGrab<D>) -> bool,
) -> bool {
    pointer.with_grab(|_serial, grab| !grab.is::<ClickGrab<D>>() && !client_grab(grab))
        == Some(true)
}

#[cfg(test)]
mod tests {
    use super::ends_pointer_grab;
    use smithay::{
        backend::input::ButtonState,
        input::{
            Seat, SeatHandler, SeatState,
            pointer::{
                AxisFrame, ButtonEvent, Focus, GestureHoldBeginEvent, GestureHoldEndEvent,
                GesturePinchBeginEvent, GesturePinchEndEvent, GesturePinchUpdateEvent,
                GestureSwipeBeginEvent, GestureSwipeEndEvent, GestureSwipeUpdateEvent,
                GrabStartData, MotionEvent, PointerGrab, PointerHandle, PointerInnerHandle,
                RelativeMotionEvent,
            },
        },
        reexports::wayland_server::{Client, protocol::wl_surface::WlSurface},
        utils::{Logical, Point, SERIAL_COUNTER},
        wayland::compositor::{CompositorClientState, CompositorHandler, CompositorState},
    };

    struct Compositor {
        seats: SeatState<Self>,
    }

    impl SeatHandler for Compositor {
        type KeyboardFocus = WlSurface;
        type PointerFocus = WlSurface;
        type TouchFocus = WlSurface;

        fn seat_state(&mut self) -> &mut SeatState<Self> {
            &mut self.seats
        }
    }

    // A surface's touch focus needs one; these tests never have a client or a surface.
    impl CompositorHandler for Compositor {
        fn compositor_state(&mut self) -> &mut CompositorState {
            unreachable!("no surfaces in these tests")
        }
        fn client_compositor_state<'a>(&self, _: &'a Client) -> &'a CompositorClientState {
            unreachable!("no clients in these tests")
        }
        fn commit(&mut self, _: &WlSurface) {}
    }

    /// A grab of the compositor's own, as a window move or a menu holds.
    struct OwnGrab(GrabStartData<Compositor>);

    impl PointerGrab<Compositor> for OwnGrab {
        fn motion(
            &mut self,
            _: &mut Compositor,
            _: &mut PointerInnerHandle<'_, Compositor>,
            _: Option<(WlSurface, Point<f64, Logical>)>,
            _: &MotionEvent,
        ) {
        }
        fn relative_motion(
            &mut self,
            _: &mut Compositor,
            _: &mut PointerInnerHandle<'_, Compositor>,
            _: Option<(WlSurface, Point<f64, Logical>)>,
            _: &RelativeMotionEvent,
        ) {
        }
        fn button(
            &mut self,
            _: &mut Compositor,
            _: &mut PointerInnerHandle<'_, Compositor>,
            _: &ButtonEvent,
        ) {
        }
        fn axis(
            &mut self,
            _: &mut Compositor,
            _: &mut PointerInnerHandle<'_, Compositor>,
            _: AxisFrame,
        ) {
        }
        fn frame(&mut self, _: &mut Compositor, _: &mut PointerInnerHandle<'_, Compositor>) {}
        fn gesture_swipe_begin(
            &mut self,
            _: &mut Compositor,
            _: &mut PointerInnerHandle<'_, Compositor>,
            _: &GestureSwipeBeginEvent,
        ) {
        }
        fn gesture_swipe_update(
            &mut self,
            _: &mut Compositor,
            _: &mut PointerInnerHandle<'_, Compositor>,
            _: &GestureSwipeUpdateEvent,
        ) {
        }
        fn gesture_swipe_end(
            &mut self,
            _: &mut Compositor,
            _: &mut PointerInnerHandle<'_, Compositor>,
            _: &GestureSwipeEndEvent,
        ) {
        }
        fn gesture_pinch_begin(
            &mut self,
            _: &mut Compositor,
            _: &mut PointerInnerHandle<'_, Compositor>,
            _: &GesturePinchBeginEvent,
        ) {
        }
        fn gesture_pinch_update(
            &mut self,
            _: &mut Compositor,
            _: &mut PointerInnerHandle<'_, Compositor>,
            _: &GesturePinchUpdateEvent,
        ) {
        }
        fn gesture_pinch_end(
            &mut self,
            _: &mut Compositor,
            _: &mut PointerInnerHandle<'_, Compositor>,
            _: &GesturePinchEndEvent,
        ) {
        }
        fn gesture_hold_begin(
            &mut self,
            _: &mut Compositor,
            _: &mut PointerInnerHandle<'_, Compositor>,
            _: &GestureHoldBeginEvent,
        ) {
        }
        fn gesture_hold_end(
            &mut self,
            _: &mut Compositor,
            _: &mut PointerInnerHandle<'_, Compositor>,
            _: &GestureHoldEndEvent,
        ) {
        }
        fn start_data(&self) -> &GrabStartData<Compositor> {
            &self.0
        }
        fn unset(&mut self, _: &mut Compositor) {}
    }

    fn seat() -> (Compositor, Seat<Compositor>, PointerHandle<Compositor>) {
        let mut seats = SeatState::new();
        let mut seat = seats.new_seat("test");
        let pointer = seat.add_pointer();
        (Compositor { seats }, seat, pointer)
    }

    fn left(state: ButtonState) -> ButtonEvent {
        ButtonEvent {
            serial: SERIAL_COUNTER.next_serial(),
            time: 0,
            button: 0x110,
            state,
        }
    }

    #[test]
    fn a_held_button_leaves_escape_to_the_app() {
        let (mut compositor, _seat, pointer) = seat();
        assert!(!ends_pointer_grab(&pointer, |_| false), "no grab at all");
        pointer.button(&mut compositor, &left(ButtonState::Pressed));
        assert!(pointer.is_grabbed(), "a held button grabs the pointer");
        assert!(
            !ends_pointer_grab(&pointer, |_| false),
            "the app's drag keeps Escape"
        );
        pointer.button(&mut compositor, &left(ButtonState::Released));
        assert!(!pointer.is_grabbed());
    }

    #[test]
    fn the_compositors_own_grab_still_takes_escape() {
        let (mut compositor, _seat, pointer) = seat();
        let start = GrabStartData {
            focus: None,
            button: 0x110,
            location: Point::default(),
        };
        pointer.set_grab(
            &mut compositor,
            OwnGrab(start),
            SERIAL_COUNTER.next_serial(),
            Focus::Clear,
        );
        assert!(ends_pointer_grab(&pointer, |_| false));
    }
}
