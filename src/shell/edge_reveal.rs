// SPDX-License-Identifier: GPL-3.0-only

//! When the edge reveals over a fullscreen window, the Halo at the top and an
//! auto-hiding panel at the bottom, hold back: a game may scroll when the
//! pointer reaches an edge, so nothing pops in over one.

use crate::dbus::game_mode::LAUNCHER_APP_ID;

/// Game mode shows a game, not its launcher, which is a fullscreen app like any other.
pub fn game_mode_shows_game(active: bool, app_id: Option<u32>) -> bool {
    active && app_id.is_some_and(|id| id != LAUNCHER_APP_ID)
}

/// Whether both edge reveals hold back over a fullscreen window.
pub fn reveals_held_back(game_mode_game: bool, pointer_constrained: bool) -> bool {
    game_mode_game || pointer_constrained
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_game_in_game_mode_or_a_held_pointer_holds_both_reveals_back() {
        let game = game_mode_shows_game(true, Some(1237320));
        assert!(game);
        assert!(reveals_held_back(game, false), "a game game mode shows");
        assert!(
            reveals_held_back(false, true),
            "a locked or confined pointer"
        );
        assert!(reveals_held_back(game, true));
    }

    /// The launcher is game mode's active app 769 between games: both reveals stay.
    #[test]
    fn the_launcher_keeps_both_reveals() {
        let launcher = game_mode_shows_game(true, Some(LAUNCHER_APP_ID));
        assert_eq!(LAUNCHER_APP_ID, 769);
        assert!(!launcher);
        assert!(!reveals_held_back(launcher, false));
        assert!(
            reveals_held_back(launcher, true),
            "unless it holds the pointer"
        );
    }

    /// Any other fullscreen window, a game started outside game mode included,
    /// keeps both reveals unless it holds the pointer.
    #[test]
    fn outside_game_mode_only_a_held_pointer_counts() {
        for app_id in [None, Some(LAUNCHER_APP_ID), Some(1237320)] {
            let game = game_mode_shows_game(false, app_id);
            assert!(!game, "{app_id:?}");
            assert!(!reveals_held_back(game, false));
        }
    }
}
