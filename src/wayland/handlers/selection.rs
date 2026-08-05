// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use smithay::{
    input::Seat,
    wayland::selection::{SelectionHandler, SelectionSource, SelectionTarget},
    xwayland::xwm::XwmId,
};
use std::os::unix::io::OwnedFd;
use tracing::warn;

/// User data attached to compositor-owned selections, identifying who owns the
/// data so [`SelectionHandler::send_selection`] knows how to serve it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SelectionUserData {
    /// The selection is owned by an Xwayland (X11) client; serve it through the
    /// XWM bridge.
    Xwayland(XwmId),
    /// The selection is a compositor-cached copy kept alive by clipboard
    /// persistence; serve it from [`crate::clipboard`].
    Persisted,
}

impl SelectionHandler for State {
    type SelectionUserData = SelectionUserData;

    fn new_selection(
        &mut self,
        target: SelectionTarget,
        source: Option<SelectionSource>,
        seat: Seat<State>,
    ) {
        // Clipboard persistence: snapshot client-set clipboard selections so the
        // contents survive the source client exiting. Runs before the Xwayland
        // bridge below (which early-returns when Xwayland is absent), and only
        // for the regular clipboard — never the primary selection.
        if target == SelectionTarget::Clipboard
            && self.common.config.cosmic_conf.clipboard_persistence
        {
            match source.as_ref() {
                Some(source) => {
                    crate::clipboard::on_new_clipboard(self, seat.clone(), source.mime_types())
                }
                None => crate::clipboard::on_clipboard_cleared(self),
            }
        }

        let Some(xwm_id) = self
            .common
            .xwayland_state
            .as_ref()
            .and_then(|xstate| xstate.xwm.as_ref())
            .map(|xwm| xwm.id())
        else {
            return;
        };

        let x_has_focus = self.common.has_x_keyboard_focus(xwm_id);

        let xstate = self.common.xwayland_state.as_mut().unwrap();
        let xwm = xstate.xwm.as_mut().unwrap();

        if let Some(source) = &source {
            if x_has_focus {
                if let Err(err) = xwm.new_selection(target, Some(source.mime_types())) {
                    warn!(?err, "Failed to set Xwayland clipboard selection.");
                }
            } else {
                match target {
                    SelectionTarget::Clipboard => {
                        xstate.clipboard_selection_dirty = Some(source.mime_types())
                    }
                    SelectionTarget::Primary => {
                        xstate.primary_selection_dirty = Some(source.mime_types())
                    }
                };
            }
        } else {
            if let Err(err) = xwm.new_selection(target, None) {
                warn!(?err, "Failed to clear Xwayland selection.");
            }
            xstate.clipboard_selection_dirty = None;
            xstate.primary_selection_dirty = None;
        }
    }

    fn send_selection(
        &mut self,
        target: SelectionTarget,
        mime_type: String,
        fd: OwnedFd,
        _seat: Seat<State>,
        user_data: &Self::SelectionUserData,
    ) {
        match user_data {
            SelectionUserData::Persisted => crate::clipboard::serve(self, &mime_type, fd),
            SelectionUserData::Xwayland(_) => {
                if let Some(xwm) = self
                    .common
                    .xwayland_state
                    .as_mut()
                    .and_then(|xstate| xstate.xwm.as_mut())
                    && let Err(err) = xwm.send_selection(target, mime_type, fd)
                {
                    warn!(?err, "Failed to send selection (X11 -> Wayland).");
                }
            }
        }
    }
}
