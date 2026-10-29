// SPDX-License-Identifier: GPL-3.0-only

use smithay::utils::{Rectangle, user_data::UserDataMap};

use crate::{
    shell::CosmicSurface,
    state::State,
    utils::prelude::Global,
    wayland::protocols::toplevel_info::{
        ToplevelInfoHandler, ToplevelInfoState, Window, delegate_toplevel_info,
    },
};

impl ToplevelInfoHandler for State {
    type Window = CosmicSurface;

    fn toplevel_info_state(&self) -> &ToplevelInfoState<State, Self::Window> {
        &self.common.toplevel_info_state
    }
    fn toplevel_info_state_mut(&mut self) -> &mut ToplevelInfoState<State, Self::Window> {
        &mut self.common.toplevel_info_state
    }
}

impl Window for CosmicSurface {
    fn title(&self) -> String {
        CosmicSurface::title(self)
    }

    fn app_id(&self) -> String {
        let app_id = CosmicSurface::app_id(self);
        if !app_id.is_empty() {
            return app_id;
        }
        self.launched_app().unwrap_or_default()
    }

    fn is_activated(&self) -> bool {
        listed_active(
            CosmicSurface::is_activated(self, true),
            self.is_minimized(),
            self.is_off_screen(),
        )
    }

    fn is_maximized(&self) -> bool {
        !self.is_minimized() && CosmicSurface::is_maximized(self, false)
    }

    fn is_fullscreen(&self) -> bool {
        !self.is_minimized() && CosmicSurface::is_fullscreen(self, false)
    }

    fn is_minimized(&self) -> bool {
        CosmicSurface::is_minimized(self)
    }

    fn is_sticky(&self) -> bool {
        CosmicSurface::is_sticky(self)
    }

    fn is_resizing(&self) -> bool {
        CosmicSurface::is_resizing(self, true).unwrap_or(false)
    }

    fn global_geometry(&self) -> Option<Rectangle<i32, Global>> {
        CosmicSurface::global_geometry(self)
    }

    fn user_data(&self) -> &UserDataMap {
        CosmicSurface::user_data(self)
    }
}

delegate_toplevel_info!(State, CosmicSurface);

/// What window lists call active: the window with focus. A minimized one or one on a desktop
/// off screen can keep its own activated state, as game mode keeps a hidden game.
fn listed_active(activated: bool, minimized: bool, off_screen: bool) -> bool {
    activated && !minimized && !off_screen
}

#[cfg(test)]
mod tests {
    use super::listed_active;

    /// Alt+Tab orders windows by when each was last listed active. A game kept activated on
    /// its hidden desktop never stopped being listed active, so Alt+Tab never went back to it.
    #[test]
    fn a_window_on_a_desktop_off_screen_is_not_listed_active() {
        assert!(!listed_active(true, false, true));
        assert!(listed_active(true, false, false));
        assert!(!listed_active(true, true, false));
        assert!(!listed_active(false, false, false));
    }
}
