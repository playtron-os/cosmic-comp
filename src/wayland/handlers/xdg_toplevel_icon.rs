// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use smithay::wayland::xdg_toplevel_icon::XdgToplevelIconHandler;

// The committed icon (name or pixel buffers) lives in the surface's
// `ToplevelIconCachedState`; the SSD header reads it during the window's
// `refresh()` (see `shell::element::window`). The default `set_icon` callback
// is therefore sufficient — no extra bookkeeping is required here.
impl XdgToplevelIconHandler for State {}
