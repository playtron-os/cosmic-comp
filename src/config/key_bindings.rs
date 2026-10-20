use cosmic_settings_config::shortcuts::State as KeyState;
use cosmic_settings_config::shortcuts::{self, Modifiers};
use smithay::input::keyboard::ModifiersState;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Action {
    /// Behaviors managed internally by cosmic-comp.
    Private(PrivateAction),
    /// Behaviors managed via cosmic-settings.
    Shortcut(shortcuts::Action),
}

#[derive(Clone, Debug, Eq, PartialEq)]
// Behaviors which are internally defined and emitted.
pub enum PrivateAction {
    Escape,
    Resizing(
        shortcuts::action::ResizeDirection,
        shortcuts::action::ResizeEdge,
        shortcuts::State,
    ),
    /// Capture a UI performance report to `$HOME` (Ctrl+Alt+Super+Shift+F12).
    PerfReport,
    /// Run the cold-start (app-launch) benchmark (Ctrl+Alt+Super+Shift+F11).
    ColdStartBench,
}

/// Convert `cosmic_settings_config::shortcuts::State` to `smithay::backend::input::KeyState`.
pub fn cosmic_keystate_to_smithay(value: KeyState) -> smithay::backend::input::KeyState {
    match value {
        KeyState::Pressed => smithay::backend::input::KeyState::Pressed,
        KeyState::Released => smithay::backend::input::KeyState::Released,
    }
}

/// Convert `smithay::backend::input::KeyState` to `cosmic_settings_config::shortcuts::State`.
pub fn cosmic_keystate_from_smithay(value: smithay::backend::input::KeyState) -> KeyState {
    match value {
        smithay::backend::input::KeyState::Pressed => KeyState::Pressed,
        smithay::backend::input::KeyState::Released => KeyState::Released,
    }
}

/// Compare `cosmic_settings_config::shortcuts::Modifiers` to `smithay::input::keyboard::ModifiersState`.
pub fn cosmic_modifiers_eq_smithay(this: &Modifiers, other: &ModifiersState) -> bool {
    this.ctrl == other.ctrl
        && this.alt == other.alt
        && this.shift == other.shift
        && this.logo == other.logo
}

/// Convert `smithay::input::keyboard::ModifiersState` to `cosmic_settings_config::shortcuts::Modifiers`
pub fn cosmic_modifiers_from_smithay(value: ModifiersState) -> Modifiers {
    Modifiers {
        ctrl: value.ctrl,
        alt: value.alt,
        shift: value.shift,
        logo: value.logo,
    }
}

#[cfg(test)]
mod tests {
    use super::shortcuts;

    /// The shipped defaults are data, and nothing else in the build reads them —
    /// so an action renamed out from under this file would only show up as a
    /// shortcut that quietly stopped working.
    #[test]
    fn the_shipped_defaults_parse() {
        let ron = include_str!("../../data/keybindings.ron");
        let shortcuts: shortcuts::Shortcuts =
            ron::from_str(ron).expect("data/keybindings.ron must deserialize");

        for action in [
            shortcuts::Action::NextRealm,
            shortcuts::Action::PreviousRealm,
            shortcuts::Action::NextWorkspace,
            shortcuts::Action::PreviousWorkspace,
        ] {
            assert!(
                shortcuts.iter().any(|(_, bound)| *bound == action),
                "{action:?} is not bound"
            );
        }
    }

    /// Super and an arrow moves between desktops and workspaces; the tiling
    /// focus lives under Super+Ctrl.
    #[test]
    fn super_arrows_move_between_desktops_and_workspaces() {
        use std::str::FromStr;

        let ron = include_str!("../../data/keybindings.ron");
        let shortcuts: shortcuts::Shortcuts = ron::from_str(ron).unwrap();

        for (combo, want) in [
            ("Super+Left", shortcuts::Action::PreviousWorkspace),
            ("Super+Right", shortcuts::Action::NextWorkspace),
            ("Super+Up", shortcuts::Action::PreviousRealm),
            ("Super+Down", shortcuts::Action::NextRealm),
            (
                "Super+Ctrl+Left",
                shortcuts::Action::Focus(shortcuts::action::FocusDirection::Left),
            ),
        ] {
            let binding = shortcuts::Binding::from_str(combo).unwrap();
            assert_eq!(shortcuts.0.get(&binding), Some(&want), "{combo}");
        }
    }

    #[test]
    fn super_k_opens_the_window_palette() {
        use std::str::FromStr;

        let ron = include_str!("../../data/keybindings.ron");
        let shortcuts: shortcuts::Shortcuts = ron::from_str(ron).unwrap();

        for (combo, want) in [
            ("Super+k", shortcuts::Action::WindowCommands),
            ("Super+Shift+m", shortcuts::Action::MoveToDesktop),
            ("Super+F10", shortcuts::Action::FocusHalo),
            ("Ctrl+space", shortcuts::Action::SpeechToText),
            (
                "Super+Ctrl+Up",
                shortcuts::Action::Focus(shortcuts::action::FocusDirection::Up),
            ),
        ] {
            let binding = shortcuts::Binding::from_str(combo).unwrap();
            assert_eq!(shortcuts.0.get(&binding), Some(&want), "{combo}");
        }
    }

    /// The capture shortcuts follow the Mac's, and Print opens the capture toolbar.
    #[test]
    fn capture_shortcuts_match_the_mac() {
        use std::str::FromStr;

        let ron = include_str!("../../data/keybindings.ron");
        let shortcuts: shortcuts::Shortcuts = ron::from_str(ron).unwrap();

        for (combo, want) in [
            ("Super+Shift+3", shortcuts::Action::CaptureScreen),
            (
                "Super+Ctrl+Shift+3",
                shortcuts::Action::CaptureScreenClipboard,
            ),
            ("Super+Shift+4", shortcuts::Action::CaptureSelection),
            (
                "Super+Ctrl+Shift+4",
                shortcuts::Action::CaptureSelectionClipboard,
            ),
            ("Super+Shift+5", shortcuts::Action::CaptureToolbar),
            ("Print", shortcuts::Action::CaptureToolbar),
            ("Super+Ctrl+Escape", shortcuts::Action::StopRecording),
        ] {
            let binding = shortcuts::Binding::from_str(combo).unwrap();
            assert_eq!(shortcuts.0.get(&binding), Some(&want), "{combo}");
        }
        // Moving a window to a workspace keeps its whole set, under Super+Alt.
        for digit in 1..=9u8 {
            let binding = shortcuts::Binding::from_str(&format!("Super+Alt+{digit}")).unwrap();
            let want = shortcuts::Action::MoveToWorkspace(digit);
            assert_eq!(shortcuts.0.get(&binding), Some(&want), "Super+Alt+{digit}");
            let shifted = shortcuts::Binding::from_str(&format!("Super+Shift+{digit}")).unwrap();
            assert_ne!(
                shortcuts.0.get(&shifted),
                Some(&want),
                "Super+Shift+{digit}"
            );
        }
        let last = shortcuts::Binding::from_str("Super+Alt+0").unwrap();
        assert_eq!(
            shortcuts.0.get(&last),
            Some(&shortcuts::Action::MoveToLastWorkspace)
        );
        assert!(
            !shortcuts.iter().any(|(_, action)| *action
                == shortcuts::Action::System(shortcuts::action::System::Screenshot)),
            "the old screenshot tool is still bound"
        );
    }

    /// Super+Tab is the workspace switch, not a second Alt+Tab.
    #[test]
    fn super_tab_switches_workspaces() {
        use std::str::FromStr;

        let ron = include_str!("../../data/keybindings.ron");
        let shortcuts: shortcuts::Shortcuts = ron::from_str(ron).unwrap();

        for (combo, want) in [
            ("Super+Tab", shortcuts::Action::NextRealm),
            ("Super+Shift+Tab", shortcuts::Action::PreviousRealm),
            // Alt keeps the window switcher.
            (
                "Alt+Tab",
                shortcuts::Action::System(shortcuts::action::System::WindowSwitcher),
            ),
        ] {
            let binding = shortcuts::Binding::from_str(combo).unwrap();
            assert_eq!(shortcuts.0.get(&binding), Some(&want), "{combo}");
        }
    }
}
