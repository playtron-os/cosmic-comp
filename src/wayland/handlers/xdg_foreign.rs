// SPDX-License-Identifier: GPL-3.0-only

use crate::{shell::X11Key, state::State, workspace_tag};
use smithay::{
    reexports::wayland_server::{Client, protocol::wl_surface::WlSurface},
    wayland::xdg_foreign::{ForeignToplevel, XdgForeignHandler, XdgForeignState},
};

impl XdgForeignHandler for State {
    fn xdg_foreign_state(&mut self) -> &mut XdgForeignState {
        &mut self.common.xdg_foreign_state
    }

    /// `x11:<window id>`, as the desktop portal names an X11 parent. One not mapped, such as a
    /// hidden leader or a window about to map, leaves the dialog without a parent.
    fn foreign_toplevel(&mut self, importer: &Client, handle: &str) -> ForeignToplevel {
        let realm = workspace_tag::of_client(importer);
        match x11_parent(handle, |window| {
            self.mapped_x11_surface(realm.as_deref(), window)
        }) {
            None => ForeignToplevel::Unknown,
            Some(Some(surface)) => ForeignToplevel::Surface(surface),
            Some(None) => ForeignToplevel::Unparented,
        }
    }
}

impl State {
    /// X11 `window`'s surface while it is mapped, on `realm`'s own server: every workspace's
    /// hands out the same ids, and one must not reach another's.
    fn mapped_x11_surface(&self, realm: Option<&str>, window: u32) -> Option<WlSurface> {
        let xwm = server_of(
            realm,
            self.common
                .xwayland_states()
                .filter_map(|xstate| Some((xstate.realm.as_deref(), xstate.xwm.as_ref()?.id()))),
        )?;
        let shell = self.common.shell.read();
        let mapped = shell.element_for_x11_window(X11Key {
            xwm: Some(xwm),
            window,
        })?;
        mapped.windows().find_map(|(surface, _)| {
            surface
                .x11_surface()
                .filter(|x11| x11.window_id() == window)?
                .wl_surface()
        })
    }
}

/// `None` for a handle that is no X11 one, else the surface `surface_of` finds for its window:
/// `Some(None)` when there is none to parent to.
fn x11_parent<S>(handle: &str, surface_of: impl FnOnce(u32) -> Option<S>) -> Option<Option<S>> {
    handle.strip_prefix("x11:")?;
    Some(x11_window_id(handle).and_then(surface_of))
}

/// The window id in the portal's `x11:<hex id>`.
fn x11_window_id(handle: &str) -> Option<u32> {
    let hex = handle.strip_prefix("x11:")?;
    let hex = hex
        .strip_prefix("0x")
        .or_else(|| hex.strip_prefix("0X"))
        .unwrap_or(hex);
    if hex.is_empty() || !hex.bytes().all(|b| b.is_ascii_hexdigit()) {
        return None;
    }
    u32::from_str_radix(hex, 16).ok().filter(|&id| id != 0)
}

/// The server of `realm` (`None` is the session's) among every running one.
fn server_of<'a, T>(
    realm: Option<&str>,
    servers: impl IntoIterator<Item = (Option<&'a str>, T)>,
) -> Option<T> {
    servers
        .into_iter()
        .find(|(server_realm, _)| *server_realm == realm)
        .map(|(_, server)| server)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_portal_x11_parent_names_its_window() {
        assert_eq!(x11_window_id("x11:4a00003"), Some(0x4a0_0003));
        assert_eq!(x11_window_id("x11:0x4A00003"), Some(0x4a0_0003));
        assert_eq!(x11_window_id("x11:ffffffff"), Some(u32::MAX));
    }

    #[test]
    fn anything_else_names_no_x11_window() {
        for handle in [
            "",
            "x11:",
            "x11:0",
            "x11:0x",
            "x11:+12",
            "x11:12 ",
            "x11:g1",
            "x11:100000000",
            "wayland:4a00003",
            "4a00003",
        ] {
            assert_eq!(x11_window_id(handle), None, "{handle:?}");
        }
    }

    #[test]
    fn an_x11_parent_that_is_not_mapped_leaves_the_dialog_without_a_parent() {
        assert_eq!(x11_parent("x11:4a", |_| None::<u32>), Some(None));
        assert_eq!(x11_parent("x11:zz", Some), Some(None));
        assert_eq!(x11_parent("x11:4a", Some), Some(Some(0x4a)));
        assert_eq!(x11_parent("wayland:4a", Some), None);
        assert_eq!(x11_parent("4a", Some), None);
    }

    #[test]
    fn an_importer_reaches_only_its_own_x_server() {
        let servers = [
            (None, "session"),
            (Some("work"), "work"),
            (Some("play"), "play"),
        ];
        assert_eq!(server_of(None, servers), Some("session"));
        assert_eq!(server_of(Some("work"), servers), Some("work"));
        assert_eq!(server_of(Some("games"), servers), None);
        assert_eq!(server_of(None, [(Some("work"), "work")]), None);
    }
}
