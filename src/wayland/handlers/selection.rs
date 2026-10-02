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

impl State {
    /// Mirror a Wayland selection into every Xwayland: applied now in the one
    /// whose client has keyboard focus, held in the others until one of theirs
    /// gains it. `None` clears.
    pub fn bridge_selection_to_xwayland(
        &mut self,
        target: SelectionTarget,
        mime_types: Option<Vec<String>>,
    ) {
        self.bridge_selection_to_other_xwayland(target, mime_types, None);
    }

    /// As [`Self::bridge_selection_to_xwayland`], for a selection that `owner`'s
    /// server set or cleared itself.
    pub fn bridge_selection_to_other_xwayland(
        &mut self,
        target: SelectionTarget,
        mime_types: Option<Vec<String>>,
        owner: Option<XwmId>,
    ) {
        let xwm_ids = self
            .common
            .xwayland_states()
            .filter_map(|xstate| xstate.xwm.as_ref().map(|xwm| xwm.id()))
            .filter(|id| Some(*id) != owner)
            .collect::<Vec<_>>();

        for xwm_id in xwm_ids {
            let x_has_focus = self.common.has_x_keyboard_focus(xwm_id);
            let Some(xstate) = self.common.xwayland_for(xwm_id) else {
                continue;
            };
            let xwm = xstate.xwm.as_mut().unwrap();

            match mime_types.clone() {
                Some(mime_types) if !x_has_focus => match target {
                    SelectionTarget::Clipboard => {
                        xstate.clipboard_selection_dirty = Some(mime_types)
                    }
                    SelectionTarget::Primary => xstate.primary_selection_dirty = Some(mime_types),
                },
                Some(mime_types) => {
                    if let Err(err) = xwm.new_selection(target, Some(mime_types)) {
                        warn!(?err, "Failed to set Xwayland clipboard selection.");
                    }
                }
                None => {
                    if let Err(err) = xwm.new_selection(target, None) {
                        warn!(?err, "Failed to clear Xwayland selection.");
                    }
                    xstate.clipboard_selection_dirty = None;
                    xstate.primary_selection_dirty = None;
                }
            }
        }
    }
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

        self.bridge_selection_to_xwayland(target, source.as_ref().map(|s| s.mime_types()));
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
            SelectionUserData::Xwayland(xwm_id) => {
                if let Some(xwm) = self.common.xwm_for(*xwm_id)
                    && let Err(err) = xwm.send_selection(target, mime_type, fd)
                {
                    warn!(?err, "Failed to send selection (X11 -> Wayland).");
                }
            }
        }
    }
}
