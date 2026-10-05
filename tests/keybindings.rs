// SPDX-License-Identifier: GPL-3.0-only

//! `data/keybindings.ron` ships as the shortcut `defaults` and gets no
//! compile-time checking. Worse, an action the config crate cannot parse is
//! silently swapped for `Disable`, so a stale variant name reads at runtime as
//! "that key does nothing". Parse the shipped file here instead.

use cosmic_settings_config::shortcuts::{
    Action, Binding, Shortcuts,
    action::{FocusDirection, System},
};

fn defaults() -> Shortcuts {
    ron::from_str(include_str!("../data/keybindings.ron"))
        .expect("data/keybindings.ron is not valid RON")
}

fn action_for(shortcuts: &Shortcuts, binding: &str) -> Action {
    let key = Binding::from_str_partial(binding).expect("invalid binding in test");
    shortcuts
        .0
        .get(&key)
        .unwrap_or_else(|| panic!("{binding} is unbound"))
        .clone()
}

#[test]
fn every_action_deserializes() {
    let shortcuts = defaults();

    assert!(!shortcuts.0.is_empty());
    assert!(
        !shortcuts
            .0
            .values()
            .any(|action| *action == Action::Disable),
        "an action failed to deserialize and was replaced with Disable"
    );
}

/// The laptop function row emits the Windows chords rather than dedicated
/// keysyms, so F7/F9/F10 are reachable only through these bindings.
#[test]
fn laptop_function_row() {
    let shortcuts = defaults();

    assert_eq!(
        action_for(&shortcuts, "Super+p"),
        Action::System(System::DisplayToggle)
    );
    assert_eq!(
        action_for(&shortcuts, "Super+i"),
        Action::System(System::Settings)
    );
    assert_eq!(
        action_for(&shortcuts, "Super+l"),
        Action::System(System::LockScreen)
    );
    assert_eq!(
        action_for(&shortcuts, "XF86Calculator"),
        Action::System(System::Calculator)
    );
}

#[test]
fn input_source_switch_uses_space_without_replacing_spotlight() {
    let shortcuts = defaults();
    assert_eq!(
        action_for(&shortcuts, "Super+Shift+space"),
        Action::System(System::InputSourceSwitch)
    );
    assert_eq!(
        action_for(&shortcuts, "Super+space"),
        Action::System(System::Spotlight)
    );

    let context = xkbcommon::xkb::Context::new(xkbcommon::xkb::CONTEXT_NO_FLAGS);
    let keymap = xkbcommon::xkb::Keymap::new_from_names(
        &context,
        "",
        "",
        "us,jp",
        ",",
        None,
        xkbcommon::xkb::KEYMAP_COMPILE_NO_FLAGS,
    )
    .expect("US/Japanese keymap must compile");
    let space = keymap.key_by_name("SPCE").expect("space key exists");
    let binding = Binding::from_str_partial("Super+Shift+space").unwrap();
    for layout in 0..keymap.num_layouts() {
        assert!(
            keymap
                .key_get_syms_by_level(space, layout, 0)
                .contains(&binding.key.unwrap()),
            "input-source shortcut must match the same key in layout {layout}"
        );
    }
}

/// Taking Super+i/Super+l for the function row must not cost a focus direction.
/// Which chord reaches a direction is free to move -- Super+arrows walk desktops
/// and realms now, so the tiling focus moves live under Super+Ctrl -- but every
/// direction has to stay reachable by something.
#[test]
fn every_focus_direction_is_reachable() {
    let shortcuts = defaults();

    for direction in [
        FocusDirection::Left,
        FocusDirection::Right,
        FocusDirection::Up,
        FocusDirection::Down,
        FocusDirection::In,
        FocusDirection::Out,
    ] {
        assert!(
            shortcuts
                .0
                .values()
                .any(|action| *action == Action::Focus(direction)),
            "no binding reaches Focus({direction:?})"
        );
    }

    // Super+l is the lock screen and Super+k the window palette, so Right and
    // Up are reached by their arrows; the rest of the vim keys still have to work.
    assert_eq!(
        action_for(&shortcuts, "Super+Ctrl+Right"),
        Action::Focus(FocusDirection::Right)
    );
    assert_eq!(
        action_for(&shortcuts, "Super+Ctrl+Up"),
        Action::Focus(FocusDirection::Up)
    );
    for (binding, direction) in [
        ("Super+h", FocusDirection::Left),
        ("Super+j", FocusDirection::Down),
        ("Super+u", FocusDirection::Out),
        ("Super+Shift+u", FocusDirection::In),
    ] {
        assert_eq!(action_for(&shortcuts, binding), Action::Focus(direction));
    }
}
