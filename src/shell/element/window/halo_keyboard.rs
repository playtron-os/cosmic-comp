//! Keyboard focus in a Halo: Super+F10 moves it from the app into the pill.

use std::sync::{Arc, Mutex};

use iced_core::{Rectangle as IcedRectangle, widget::Id};
use smithay::{
    backend::input::{KeyState, Keycode},
    input::{
        Seat, SeatHandler,
        keyboard::{
            GrabStartData as KeyboardGrabStartData, KeyboardGrab, KeyboardInnerHandle,
            ModifiersState,
        },
    },
    utils::{Point, SERIAL_COUNTER, Serial},
};
use xkbcommon::xkb::Keysym;

use super::{CosmicWindow, Message, halo};
use crate::{
    shell::{
        element::header_bar::{HaloControl, menu_trigger_id},
        grabs::MenuKey,
    },
    state::State,
    utils::prelude::*,
};

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

/// Where a menu opened from the keyboard hangs: 4px under the trigger's bottom-left corner.
fn menu_anchor(origin: Point<f64, Global>, trigger: IcedRectangle) -> Point<i32, Global> {
    Point::from((
        (origin.x + f64::from(trigger.x)).round() as i32,
        (origin.y + f64::from(trigger.y + trigger.height) + 4.0).round() as i32,
    ))
}

struct FindBounds {
    id: Id,
    found: Arc<Mutex<Option<IcedRectangle>>>,
}

impl iced_core::widget::Operation<()> for FindBounds {
    fn traverse(&mut self, operate: &mut dyn FnMut(&mut dyn iced_core::widget::Operation<()>)) {
        operate(self);
    }

    fn container(&mut self, id: Option<&Id>, bounds: IcedRectangle) {
        if id == Some(&self.id) {
            *self.found.lock().unwrap() = Some(bounds);
        }
    }
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
        icetron_themes::set_focus_visible(true);
        if let Some(keyboard) = seat.get_keyboard() {
            keyboard.set_grab(
                state,
                HaloKeyboardGrab::new(seat.clone(), self.clone()),
                SERIAL_COUNTER.next_serial(),
            );
        }
    }

    fn widget_bounds(&self, id: Id) -> Option<IcedRectangle> {
        let found = Arc::new(Mutex::new(None));
        self.0.queue_operation(FindBounds {
            id,
            found: found.clone(),
        });
        found.lock().unwrap().take()
    }

    /// Where this Halo's own coordinates start on screen.
    fn halo_origin(&self, state: &State) -> Option<Point<f64, Global>> {
        let (surface, fullscreen, origin) = self.0.with_program(|p| {
            (
                p.window.clone(),
                p.fullscreen_output
                    .as_ref()
                    .map(|output| output.lock().unwrap().geometry().loc),
                p.header_origin(),
            )
        });
        let loc = match fullscreen {
            Some(loc) => loc,
            None => {
                let shell = state.common.shell.read();
                let mapped = shell.element_for_surface(&surface)?;
                shell.element_geometry(mapped)?.loc
            }
        };
        Some(loc.to_f64() + Point::from((origin.x, origin.y)))
    }

    /// The app menu, hung under the `⌄` as a keyboard-opened menu is.
    fn open_menu_from_keyboard(&self, state: &mut State, seat: &Seat<State>) {
        let Some(trigger) = self.widget_bounds(menu_trigger_id()) else {
            return;
        };
        let Some(origin) = self.halo_origin(state) else {
            return;
        };
        let position = menu_anchor(origin, trigger);
        let surface = self.surface();
        let app = self
            .0
            .with_program(|p| p.desktop_app.lock().unwrap().clone());
        let seat = seat.clone();
        state.common.event_loop_handle.insert_idle(move |state| {
            let serial = SERIAL_COUNTER.next_serial();
            if let Some(start) = crate::shell::check_grab_preconditions(&seat, Some(serial), None) {
                halo::open_menu(state, &surface, &seat, serial, start, position, app);
                // As in the design, focus goes into the menu's first row.
                crate::shell::grabs::halo_menu_key(&seat, MenuKey::First, state);
            }
        });
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
            HaloControl::Menu => {
                self.window.open_menu_from_keyboard(data, &self.seat);
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
        let menu_open = self
            .window
            .0
            .with_program(|p| p.menu_open.load(std::sync::atomic::Ordering::SeqCst));
        if menu_open {
            let menu_key = match sym {
                Keysym::Down | Keysym::Tab => Some(MenuKey::Step(true)),
                Keysym::Up | Keysym::ISO_Left_Tab => Some(MenuKey::Step(false)),
                _ if key == Some(HaloKey::Press) => Some(MenuKey::Press),
                _ => None,
            };
            if let Some(menu_key) = menu_key {
                // A row the menu ran ends the visit, as a palette command does.
                if crate::shell::grabs::halo_menu_key(&self.seat, menu_key, data) == Some(true) {
                    handle.unset_grab(self, data, serial, false);
                }
                return;
            }
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
        HaloControl::Menu,
        HaloControl::Maximize,
        HaloControl::Close,
    ];

    #[test]
    fn tab_and_the_arrows_walk_the_controls_and_wrap() {
        assert_eq!(step(&CONTROLS, None, true), Some(HaloControl::Glyph));
        assert_eq!(
            step(&CONTROLS, Some(HaloControl::Glyph), true),
            Some(HaloControl::Menu)
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

    #[test]
    fn a_keyboard_opened_menu_hangs_4px_under_its_trigger() {
        let trigger = IcedRectangle {
            x: 300.0,
            y: 18.0,
            width: 28.0,
            height: 28.0,
        };
        assert_eq!(
            menu_anchor(Point::from((100.0, 200.0)), trigger),
            Point::from((400, 250))
        );
    }
}
