//! What a client the workspace registry boxed may use of the guarded protocols.
//!
//! The registry gives each app and agent it launches a socket of its own
//! (`wp_security_context_v1`), tagged with the app's desktop id or the agent's
//! id. Like any client with a security context, such a client is not offered
//! input injection, screen capture, the window list or the clipboard outside a
//! paste, unless its workspace grants it that class. The registry lists the
//! grants (`DisplayGrants`); `dbus::display_grants` follows them.
//!
//! A grant reaches a client's next registry; taking one back disconnects a
//! client that holds one of its objects, since nothing else would stop it.

use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use smithay::reexports::wayland_server::{
    Client, DisplayHandle,
    backend::{ClientId, DisconnectReason, ObjectId},
};
use smithay::wayland::security_context::SecurityContext;
use tracing::info;

use crate::state::ClientState;

/// The sandbox engine the registry tags an app's socket with.
pub const APP_ENGINE: &str = "one.playtron.Workspaces.App";
/// The sandbox engine the registry tags an agent's socket with.
pub const AGENT_ENGINE: &str = "one.playtron.Workspaces.Agent";

/// What a grant opens.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Class {
    /// Typing and clicking as the person (virtual keyboard), and managing
    /// other windows.
    Input,
    /// Capturing outputs and windows, and listing windows.
    Screen,
    /// Reading and setting the clipboard outside a paste (data control).
    Clipboard,
}

impl Class {
    fn parse(name: &str) -> Option<Self> {
        match name {
            "input" => Some(Class::Input),
            "screen" => Some(Class::Screen),
            "clipboard" => Some(Class::Clipboard),
            _ => None,
        }
    }

    /// The interfaces a client holding any object of keeps using this class.
    fn interfaces(self) -> &'static [&'static str] {
        match self {
            Class::Input => &[
                "zwp_virtual_keyboard_manager_v1",
                "zwp_virtual_keyboard_v1",
                "zcosmic_toplevel_manager_v1",
            ],
            Class::Screen => &[
                "ext_image_copy_capture_manager_v1",
                "ext_image_copy_capture_session_v1",
                "ext_image_copy_capture_cursor_session_v1",
                "ext_output_image_capture_source_manager_v1",
                "ext_foreign_toplevel_image_capture_source_manager_v1",
                "zcosmic_workspace_image_capture_source_manager_v1",
                "zcosmic_toplevel_info_v1",
                "ext_foreign_toplevel_list_v1",
            ],
            Class::Clipboard => &[
                "ext_data_control_manager_v1",
                "ext_data_control_device_v1",
                "zwlr_data_control_manager_v1",
                "zwlr_data_control_device_v1",
            ],
        }
    }
}

/// Who a registry-made socket is for.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Subject {
    App(String),
    Agent(String),
}

impl Subject {
    /// `None` for a context the registry did not make.
    pub fn of(context: &SecurityContext) -> Option<Self> {
        Self::tagged(context.sandbox_engine.as_deref(), context.app_id.as_deref())
    }

    fn tagged(engine: Option<&str>, app_id: Option<&str>) -> Option<Self> {
        let id = app_id.filter(|id| !id.is_empty())?.to_string();
        match engine? {
            APP_ENGINE => Some(Subject::App(id)),
            AGENT_ENGINE => Some(Subject::Agent(id)),
            _ => None,
        }
    }

    fn parse(kind: &str, id: String) -> Option<Self> {
        match kind {
            "app" => Some(Subject::App(id)),
            "agent" => Some(Subject::Agent(id)),
            _ => None,
        }
    }
}

/// Each workspace's grants, by subject.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Table(HashMap<(String, Subject), HashSet<Class>>);

impl Table {
    /// From the registry's `DisplayGrants` rows; unknown kinds and classes are
    /// skipped, so a newer registry grants nothing this compositor does not know.
    pub fn from_rows(rows: Vec<(String, String, String, Vec<String>)>) -> Self {
        let mut table = HashMap::<_, HashSet<Class>>::new();
        for (workspace, kind, id, classes) in rows {
            let Some(subject) = Subject::parse(&kind, id) else {
                continue;
            };
            table
                .entry((workspace, subject))
                .or_default()
                .extend(classes.iter().filter_map(|c| Class::parse(c)));
        }
        Table(table)
    }

    pub fn allows(&self, workspace: Option<&str>, subject: &Subject, class: Class) -> bool {
        let Some(workspace) = workspace else {
            return false;
        };
        self.0
            .get(&(workspace.to_string(), subject.clone()))
            .is_some_and(|classes| classes.contains(&class))
    }
}

/// The grants every global filter reads, kept current by the registry follower.
#[derive(Debug, Clone, Default)]
pub struct DisplayGrants(Arc<parking_lot::RwLock<Table>>);

impl DisplayGrants {
    /// Whether `client` may see a global of `class`: anything not sandboxed may,
    /// a registry-boxed client may when its workspace grants it, and any other
    /// sandboxed client may not.
    pub fn may(&self, client: &Client, class: Class) -> bool {
        let Some(state) = client.get_data::<ClientState>() else {
            return false;
        };
        if state.not_sandboxed() {
            return true;
        }
        self.allows(state, class)
    }

    fn allows(&self, state: &ClientState, class: Class) -> bool {
        state
            .security_context
            .as_ref()
            .and_then(Subject::of)
            .is_some_and(|subject| {
                self.0
                    .read()
                    .allows(state.workspace.as_deref(), &subject, class)
            })
    }

    /// A filter for a global of `class`.
    pub fn filter(&self, class: Class) -> impl Fn(&Client) -> bool + Send + Sync + Clone + 'static {
        let grants = self.clone();
        move |client: &Client| grants.may(client, class)
    }

    /// Take `table`, and disconnect every boxed client that holds an object of
    /// a class it no longer has.
    pub fn replace(&self, table: Table, dh: &DisplayHandle) {
        let old = std::mem::replace(&mut *self.0.write(), table);
        let new = self.0.read().clone();
        let handle = dh.backend_handle();
        let mut clients = Vec::new();
        handle.with_all_clients(|id| clients.push(id));
        for id in clients {
            let Ok(data) = handle.get_client_data(id.clone()) else {
                continue;
            };
            let Some(state) = data.downcast_ref::<ClientState>() else {
                continue;
            };
            let Some(subject) = state.security_context.as_ref().and_then(Subject::of) else {
                continue;
            };
            let workspace = state.workspace.as_deref();
            let lost: Vec<Class> = [Class::Input, Class::Screen, Class::Clipboard]
                .into_iter()
                .filter(|&c| {
                    old.allows(workspace, &subject, c) && !new.allows(workspace, &subject, c)
                })
                .collect();
            if !lost.is_empty() && holds_any(dh, &id, &lost) {
                info!(
                    ?subject,
                    ?lost,
                    "a grant was taken back; disconnecting the client using it"
                );
                handle.kill_client(id, DisconnectReason::ConnectionClosed);
            }
        }
    }
}

/// Whether `client` holds an object of any of `classes`.
fn holds_any(dh: &DisplayHandle, client: &ClientId, classes: &[Class]) -> bool {
    let handle = dh.backend_handle();
    let mut objects: Vec<ObjectId> = Vec::new();
    if handle
        .with_all_objects_for(client.clone(), |id| objects.push(id))
        .is_err()
    {
        return false;
    }
    objects.iter().any(|id| {
        let name = id.interface().name;
        classes.iter().any(|c| c.interfaces().contains(&name))
    })
}

/// The interface name of a resource type, for checking [`Class::interfaces`].
#[cfg(test)]
fn name_of<R: smithay::reexports::wayland_server::Resource>() -> &'static str {
    R::interface().name
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn only_the_registrys_engines_name_a_subject() {
        assert_eq!(
            Subject::tagged(Some(APP_ENGINE), Some("org.example.Mail")),
            Some(Subject::App("org.example.Mail".into()))
        );
        assert_eq!(
            Subject::tagged(Some(AGENT_ENGINE), Some("kora")),
            Some(Subject::Agent("kora".into()))
        );
        assert_eq!(
            Subject::tagged(Some("org.flatpak"), Some("org.example.Mail")),
            None
        );
        assert_eq!(Subject::tagged(Some(APP_ENGINE), Some("")), None);
    }

    #[test]
    fn a_grant_is_one_workspaces_one_subjects_one_class() {
        let table = Table::from_rows(vec![
            (
                "work".into(),
                "agent".into(),
                "kora".into(),
                vec!["input".into()],
            ),
            (
                "work".into(),
                "app".into(),
                "org.example.Clips".into(),
                vec!["clipboard".into(), "telepathy".into()],
            ),
            (
                "work".into(),
                "robot".into(),
                "x".into(),
                vec!["input".into()],
            ),
        ]);
        let kora = Subject::Agent("kora".into());
        let clips = Subject::App("org.example.Clips".into());
        assert!(table.allows(Some("work"), &kora, Class::Input));
        assert!(!table.allows(Some("work"), &kora, Class::Screen));
        assert!(
            !table.allows(Some("home"), &kora, Class::Input),
            "another workspace's agent"
        );
        assert!(
            !table.allows(None, &kora, Class::Input),
            "a client in no workspace"
        );
        assert!(!table.allows(Some("work"), &Subject::App("kora".into()), Class::Input));
        assert!(table.allows(Some("work"), &clips, Class::Clipboard));
        assert!(!table.allows(Some("work"), &clips, Class::Input));
    }

    #[test]
    fn the_interfaces_watched_are_the_ones_the_protocols_name() {
        use smithay::reexports::wayland_protocols::ext::data_control::v1::server::ext_data_control_manager_v1::ExtDataControlManagerV1;
        use smithay::reexports::wayland_protocols::ext::foreign_toplevel_list::v1::server::ext_foreign_toplevel_list_v1::ExtForeignToplevelListV1;
        use smithay::reexports::wayland_protocols::ext::image_copy_capture::v1::server::ext_image_copy_capture_manager_v1::ExtImageCopyCaptureManagerV1;
        use smithay::reexports::wayland_protocols_misc::zwp_virtual_keyboard_v1::server::zwp_virtual_keyboard_manager_v1::ZwpVirtualKeyboardManagerV1;
        use smithay::reexports::wayland_protocols_wlr::data_control::v1::server::zwlr_data_control_manager_v1::ZwlrDataControlManagerV1;
        assert!(
            Class::Input
                .interfaces()
                .contains(&name_of::<ZwpVirtualKeyboardManagerV1>())
        );
        assert!(
            Class::Clipboard
                .interfaces()
                .contains(&name_of::<ExtDataControlManagerV1>())
        );
        assert!(
            Class::Clipboard
                .interfaces()
                .contains(&name_of::<ZwlrDataControlManagerV1>())
        );
        assert!(
            Class::Screen
                .interfaces()
                .contains(&name_of::<ExtImageCopyCaptureManagerV1>())
        );
        assert!(
            Class::Screen
                .interfaces()
                .contains(&name_of::<ExtForeignToplevelListV1>())
        );
    }
}
