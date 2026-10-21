//! Keyboard focus in a Halo: Super+F10 moves it from the app into the pill.

use smithay::{
    backend::input::{KeyState, Keycode},
    input::{
        Seat, SeatHandler,
        keyboard::{
            GrabStartData as KeyboardGrabStartData, KeyboardGrab, KeyboardInnerHandle,
            ModifiersState,
        },
    },
    utils::{SERIAL_COUNTER, Serial},
};
use xkbcommon::xkb::Keysym;

use super::{CosmicWindow, Message, halo};
use crate::{shell::element::header_bar::HaloControl, state::State, utils::prelude::*};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum HaloKey {
    Step(bool),
    Press,
    Leave,
}

fn is_modifier(sym: Keysym) -> bool {
    matches!(
        sym,
        Keysym::Shift_L
            | Keysym::Shift_R
            | Keysym::Control_L
            | Keysym::Control_R
            | Keysym::Alt_L
            | Keysym::Alt_R
            | Keysym::Super_L
            | Keysym::Super_R
            | Keysym::Meta_L
            | Keysym::Meta_R
            | Keysym::ISO_Level3_Shift
    )
}

fn halo_key(sym: Keysym) -> Option<HaloKey> {
    match sym {
        Keysym::Tab | Keysym::Right => Some(HaloKey::Step(true)),
        Keysym::ISO_Left_Tab | Keysym::Left => Some(HaloKey::Step(false)),
        Keysym::Return | Keysym::KP_Enter | Keysym::space => Some(HaloKey::Press),
        Keysym::Escape => Some(HaloKey::Leave),
        _ => None,
    }
}

/// The control after (or before) `at`, wrapping; the first when focus is on none of them.
fn step(controls: &[HaloControl], at: Option<HaloControl>, forward: bool) -> Option<HaloControl> {
    let count = controls.len();
    let next = match at.and_then(|at| controls.iter().position(|c| *c == at)) {
        Some(idx) if forward => (idx + 1) % count.max(1),
        Some(idx) => (idx + count - 1) % count,
        None => 0,
    };
    controls.get(next).copied()
}

impl CosmicWindow {
    pub fn halo_keyboard(&self) -> Option<HaloControl> {
        self.0.with_program(|p| *p.halo_keyboard.lock().unwrap())
    }

    fn halo_controls(&self) -> Vec<HaloControl> {
        self.0.with_program(|p| p.halo_header().controls())
    }

    fn set_halo_keyboard(&self, control: Option<HaloControl>) {
        self.0
            .with_program(|p| *p.halo_keyboard.lock().unwrap() = control);
        self.0.force_update();
    }

    /// Move keyboard focus into this window's Halo, onto `at` or its first control.
    pub fn enter_halo_keyboard(
        &self,
        state: &mut State,
        seat: &Seat<State>,
        at: Option<HaloControl>,
    ) {
        if !self.0.with_program(|p| p.uses_halo_header()) {
            return;
        }
        let controls = self.halo_controls();
        let Some(first) = at
            .filter(|at| controls.contains(at))
            .or_else(|| controls.first().copied())
        else {
            return;
        };
        self.set_halo_keyboard(Some(first));
        icetron_p::icetron_themes::set_focus_visible(true);
        if let Some(keyboard) = seat.get_keyboard() {
            keyboard.set_grab(
                state,
                HaloKeyboardGrab::new(seat.clone(), self.clone()),
                SERIAL_COUNTER.next_serial(),
            );
        }
    }

    /// The palette from the glyph; Escape there brings focus back to the glyph.
    fn open_commands_from_keyboard(&self, state: &mut State, seat: &Seat<State>) {
        let surface = self.surface();
        let app = self
            .0
            .with_program(|p| p.desktop_app.lock().unwrap().clone());
        let (seat, window) = (seat.clone(), self.clone());
        state.common.event_loop_handle.insert_idle(move |state| {
            let serial = SERIAL_COUNTER.next_serial();
            let Some(start) = crate::shell::check_grab_preconditions(&seat, Some(serial), None)
            else {
                return;
            };
            let position = start.current_location(&seat).to_i32_round().as_global();
            let back = seat.clone();
            let hook: crate::shell::grabs::EscapeHook = Box::new(move |state: &mut State| {
                window.enter_halo_keyboard(state, &back, Some(HaloControl::Glyph));
            });
            halo::open_commands(
                state,
                &surface,
                &seat,
                serial,
                start,
                position,
                app,
                Some(hook),
            );
        });
    }
}

pub struct HaloKeyboardGrab {
    seat: Seat<State>,
    window: CosmicWindow,
    start_data: KeyboardGrabStartData<State>,
}

impl HaloKeyboardGrab {
    fn new(seat: Seat<State>, window: CosmicWindow) -> Self {
        let focus = seat
            .get_keyboard()
            .and_then(|keyboard| keyboard.current_focus());
        Self {
            seat,
            window,
            start_data: KeyboardGrabStartData { focus },
        }
    }

    pub fn window(&self) -> &CosmicWindow {
        &self.window
    }

    /// Press the focused control. Returns whether focus stays in the Halo.
    fn press(&self, data: &mut State) -> bool {
        let Some(control) = self.window.halo_keyboard() else {
            return false;
        };
        match control {
            HaloControl::Glyph | HaloControl::More => {
                self.window.open_commands_from_keyboard(data, &self.seat);
                false
            }
            // A toggle keeps focus, as the design's quick-setting button does.
            HaloControl::QuickSetting(_) => {
                let message: Option<Message> = self
                    .window
                    .0
                    .with_program(|p| p.halo_header().control_message(control));
                if let Some(message) = message {
                    self.window.0.queue_message(message);
                }
                true
            }
            _ => {
                let message: Option<Message> = self
                    .window
                    .0
                    .with_program(|p| p.halo_header().control_message(control));
                if let Some(message) = message {
                    self.window.0.queue_message(message);
                }
                false
            }
        }
    }
}

impl KeyboardGrab<State> for HaloKeyboardGrab {
    fn input(
        &mut self,
        data: &mut State,
        handle: &mut KeyboardInnerHandle<'_, State>,
        keycode: Keycode,
        state: KeyState,
        modifiers: Option<ModifiersState>,
        serial: Serial,
        time: u32,
    ) {
        let sym = handle.keysym_handle(keycode).modified_sym();
        let key = halo_key(sym);
        // Modifiers and releases reach the app, so it never sees a key stuck down.
        if state != KeyState::Pressed || (key.is_none() && is_modifier(sym)) {
            if key.is_none() {
                handle.input(data, keycode, state, modifiers, serial, time);
            }
            return;
        }
        let stay = match key {
            Some(HaloKey::Step(forward)) => {
                let next = step(
                    &self.window.halo_controls(),
                    self.window.halo_keyboard(),
                    forward,
                );
                self.window.set_halo_keyboard(next);
                true
            }
            Some(HaloKey::Press) => self.press(data),
            Some(HaloKey::Leave) => false,
            None => true,
        };
        if !stay {
            handle.unset_grab(self, data, serial, false);
        }
    }

    fn set_focus(
        &mut self,
        _data: &mut State,
        _handle: &mut KeyboardInnerHandle<'_, State>,
        _focus: Option<<State as SeatHandler>::KeyboardFocus>,
        _serial: Serial,
    ) {
    }

    fn start_data(&self) -> &KeyboardGrabStartData<State> {
        &self.start_data
    }

    fn unset(&mut self, _data: &mut State) {
        self.window.set_halo_keyboard(None);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const CONTROLS: [HaloControl; 4] = [
        HaloControl::Glyph,
        HaloControl::QuickSetting(0),
        HaloControl::Maximize,
        HaloControl::Close,
    ];

    #[test]
    fn tab_and_the_arrows_walk_the_controls_and_wrap() {
        assert_eq!(step(&CONTROLS, None, true), Some(HaloControl::Glyph));
        assert_eq!(
            step(&CONTROLS, Some(HaloControl::Glyph), true),
            Some(HaloControl::QuickSetting(0))
        );
        assert_eq!(
            step(&CONTROLS, Some(HaloControl::Close), true),
            Some(HaloControl::Glyph)
        );
        assert_eq!(
            step(&CONTROLS, Some(HaloControl::Glyph), false),
            Some(HaloControl::Close)
        );
        // A control the Halo no longer shows starts over from the first.
        assert_eq!(
            step(&CONTROLS, Some(HaloControl::Chip), true),
            Some(HaloControl::Glyph)
        );
        assert_eq!(step(&[], None, true), None);
    }

    #[test]
    fn keys_map_to_the_design_s_halo_keyboard() {
        assert_eq!(halo_key(Keysym::Tab), Some(HaloKey::Step(true)));
        assert_eq!(halo_key(Keysym::Right), Some(HaloKey::Step(true)));
        assert_eq!(halo_key(Keysym::ISO_Left_Tab), Some(HaloKey::Step(false)));
        assert_eq!(halo_key(Keysym::Left), Some(HaloKey::Step(false)));
        assert_eq!(halo_key(Keysym::Return), Some(HaloKey::Press));
        assert_eq!(halo_key(Keysym::space), Some(HaloKey::Press));
        assert_eq!(halo_key(Keysym::Escape), Some(HaloKey::Leave));
        assert_eq!(halo_key(Keysym::a), None);
    }
}
