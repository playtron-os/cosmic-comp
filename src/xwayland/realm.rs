//! An X server per workspace.
//!
//! X gives every client its peers' windows and input, and the session's
//! Xwayland can be reached from anywhere on the machine, so each workspace gets
//! a server of its own. It listens only on a socket in the workspace's runtime
//! dir, which the workspace's sandbox shows at `/tmp/.X11-unix`, and this
//! compositor is its window manager as it is the session's. It starts on the
//! first connection and stops with the workspace.

use std::{
    collections::HashMap,
    io,
    os::unix::{fs::PermissionsExt, io::OwnedFd, net::UnixListener},
    path::{Path, PathBuf},
    process::Stdio,
    time::Duration,
};

use calloop::{
    Interest, Mode, PostAction, RegistrationToken,
    generic::Generic,
    timer::{TimeoutAction, Timer},
};
use smithay::{
    reexports::wayland_server::Client,
    xwayland::{X11Wm, XWayland, XWaylandClientData, XWaylandEvent, xwm::XwmId},
};
use tracing::{debug, error, info, warn};

use super::XWaylandState;
use crate::state::{BackendData, State};

/// On the Xwayland client of a workspace's server: the workspace it is for.
#[derive(Debug, Clone)]
pub struct XwaylandRealm(pub String);

/// The longest a workspace waits to try its server again after it failed.
const MAX_RETRY: Duration = Duration::from_secs(30);

/// Where a workspace's X clients find its server. The workspace's launcher
/// mounts the directory at `/tmp/.X11-unix` and sets `DISPLAY=:0`.
pub fn socket_path(runtime: &Path, workspace: &str) -> PathBuf {
    runtime
        .join("kora-workspaces")
        .join(workspace)
        .join("X11-unix")
        .join("X0")
}

#[derive(Debug)]
pub struct RealmXwayland {
    socket: PathBuf,
    /// Kept across servers, so a client that connects between two is queued.
    listener: UnixListener,
    /// Watching for the first client; `None` while a server starts or runs.
    waiting: Option<RegistrationToken>,
    /// The server's own source; removing it ends the server.
    server: Option<RegistrationToken>,
    /// The server, once it is ready.
    pub state: Option<XWaylandState>,
    /// Starts in a row that ended before the server was ready or soon after.
    failures: u32,
}

impl RealmXwayland {
    fn bind(socket: PathBuf) -> io::Result<Self> {
        let dir = socket.parent().expect("a socket path has a directory");
        std::fs::create_dir_all(dir)?;
        std::fs::set_permissions(dir, std::fs::Permissions::from_mode(0o700))?;
        // Ours to replace: one left by an earlier session, or by whatever served
        // this workspace before.
        match std::fs::remove_file(&socket) {
            Ok(()) => {}
            Err(err) if err.kind() == io::ErrorKind::NotFound => {}
            Err(err) => return Err(err),
        }
        let listener = UnixListener::bind(&socket)?;
        listener.set_nonblocking(true)?;
        Ok(RealmXwayland {
            socket,
            listener,
            waiting: None,
            server: None,
            state: None,
            failures: 0,
        })
    }

    /// Turn away whoever queued for a server that is not coming, rather than
    /// leave them waiting on it.
    fn refuse_queued(&self) {
        while self.listener.accept().is_ok() {}
    }
}

/// The ids that name a workspace safely as one path component.
fn valid_id(id: &str) -> bool {
    !id.is_empty()
        && id
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_')
}

impl State {
    /// Give every running workspace a server to connect to, and end the
    /// servers of the ones that stopped or went away.
    pub fn sync_realm_xwayland(&mut self, running: &[String]) {
        let stopped = self
            .common
            .realm_xwayland
            .keys()
            .filter(|id| !running.contains(id))
            .cloned()
            .collect::<Vec<_>>();
        for id in stopped {
            self.stop_realm_xwayland(&id);
        }

        let Some(runtime) = std::env::var_os("XDG_RUNTIME_DIR").map(PathBuf::from) else {
            return;
        };
        for id in running {
            if self.common.realm_xwayland.contains_key(id) || !valid_id(id) {
                continue;
            }
            match RealmXwayland::bind(socket_path(&runtime, id)) {
                Ok(realm) => {
                    debug!(workspace = id, socket = %realm.socket.display(), "X server socket ready");
                    self.common.realm_xwayland.insert(id.clone(), realm);
                    self.wait_for_realm_client(id);
                }
                Err(err) => warn!(workspace = id, ?err, "No X server for this workspace"),
            }
        }
    }

    fn wait_for_realm_client(&mut self, id: &str) {
        let handle = self.common.event_loop_handle.clone();
        let Some(realm) = self.common.realm_xwayland.get_mut(id) else {
            return;
        };
        if realm.waiting.is_some() || realm.server.is_some() {
            return;
        }
        let listener = match realm.listener.try_clone() {
            Ok(listener) => listener,
            Err(err) => {
                warn!(workspace = id, ?err, "Cannot watch the X server socket");
                return;
            }
        };
        let watched = id.to_string();
        match handle.insert_source(
            Generic::new(listener, Interest::READ, Mode::Level),
            move |_, _, state| {
                if let Some(realm) = state.common.realm_xwayland.get_mut(&watched) {
                    realm.waiting = None;
                }
                state.start_realm_xwayland(&watched);
                Ok(PostAction::Remove)
            },
        ) {
            Ok(token) => realm.waiting = Some(token),
            Err(err) => warn!(workspace = id, ?err, "Cannot watch the X server socket"),
        }
    }

    fn start_realm_xwayland(&mut self, id: &str) {
        let render_node = match &self.backend {
            BackendData::Kms(kms) => *kms.primary_node.read().unwrap(),
            _ => None,
        };
        let Some(realm) = self.common.realm_xwayland.get(id) else {
            return;
        };
        if realm.server.is_some() {
            return;
        }
        let listener = match realm.listener.try_clone() {
            Ok(listener) => OwnedFd::from(listener),
            Err(err) => {
                error!(
                    workspace = id,
                    ?err,
                    "Cannot hand the socket to an X server"
                );
                self.realm_xwayland_failed(id);
                return;
            }
        };

        info!(workspace = id, "Starting the workspace's X server");
        let tag = XwaylandRealm(id.to_string());
        let spawned = XWayland::spawn_on(
            &self.common.display_handle,
            [listener],
            std::iter::empty::<(&str, &str)>(),
            std::iter::empty::<&str>(),
            Stdio::null(),
            Stdio::null(),
            |user_data| {
                if let Some(node) = render_node {
                    user_data.insert_if_missing_threadsafe(|| node);
                }
                user_data.insert_if_missing_threadsafe(|| tag);
            },
        );
        let (xwayland, client) = match spawned {
            Ok(spawned) => spawned,
            Err(err) => {
                error!(
                    workspace = id,
                    ?err,
                    "Failed to start the workspace's X server"
                );
                self.realm_xwayland_failed(id);
                return;
            }
        };
        // Before the server reads its outputs: the client that started it is
        // already connecting, and would see the screen at its unscaled size.
        if let Some(data) = client.get_data::<XWaylandClientData>() {
            data.compositor_state
                .set_client_scale(self.common.xwayland_target_scale());
        }

        let started = id.to_string();
        let inserted = self.common.event_loop_handle.insert_source(
            xwayland,
            move |event, _, state| match event {
                XWaylandEvent::Ready {
                    x11_socket,
                    display_number,
                } => {
                    state.realm_xwayland_ready(&started, client.clone(), x11_socket, display_number)
                }
                XWaylandEvent::Error => {
                    // Not from inside the source the teardown removes.
                    let failed = started.clone();
                    state
                        .common
                        .event_loop_handle
                        .insert_idle(move |state| state.realm_xwayland_failed(&failed));
                }
            },
        );
        match inserted {
            Ok(token) => {
                if let Some(realm) = self.common.realm_xwayland.get_mut(id) {
                    realm.server = Some(token);
                }
            }
            Err(err) => {
                error!(
                    workspace = id,
                    ?err,
                    "Failed to watch the workspace's X server"
                );
                self.realm_xwayland_failed(id);
            }
        }
    }

    fn realm_xwayland_ready(
        &mut self,
        id: &str,
        client: Client,
        x11_socket: std::os::unix::net::UnixStream,
        display_number: u32,
    ) {
        let Some(realm) = self.common.realm_xwayland.get(id) else {
            return;
        };
        let display_name = realm.socket.to_string_lossy().into_owned();
        let wm = match X11Wm::start_wm(
            self.common.event_loop_handle.clone(),
            &self.common.display_handle,
            x11_socket,
            client.clone(),
        ) {
            Ok(wm) => wm,
            Err(err) => {
                error!(
                    workspace = id,
                    ?err,
                    "Failed to manage the workspace's X server"
                );
                // Not from inside the source the teardown removes.
                let failed = id.to_string();
                self.common
                    .event_loop_handle
                    .insert_idle(move |state| state.realm_xwayland_failed(&failed));
                return;
            }
        };
        let mut state =
            XWaylandState::new(client, Some(id.to_string()), display_number, display_name);
        state.xwm = Some(wm);
        state.reload_cursor(1.);
        if let Some(realm) = self.common.realm_xwayland.get_mut(id) {
            realm.state = Some(state);
            realm.failures = 0;
        }
        info!(
            workspace = id,
            display_number, "The workspace's X server is ready"
        );

        self.common.update_xwayland_settings();
        self.common.update_xwayland_primary_output();
    }

    /// The server died, or never came up: clear it away and wait for the next
    /// client, later each time it keeps failing.
    pub(super) fn realm_xwayland_failed(&mut self, id: &str) {
        self.end_realm_server(id);
        let Some(realm) = self.common.realm_xwayland.get_mut(id) else {
            return;
        };
        realm.refuse_queued();
        realm.failures += 1;
        let delay = Duration::from_secs(1 << realm.failures.min(5)).min(MAX_RETRY);
        warn!(
            workspace = id,
            failures = realm.failures,
            ?delay,
            "The workspace's X server stopped"
        );
        let retry = id.to_string();
        if let Err(err) = self.common.event_loop_handle.insert_source(
            Timer::from_duration(delay),
            move |_, _, state| {
                state.wait_for_realm_client(&retry);
                TimeoutAction::Drop
            },
        ) {
            warn!(
                workspace = id,
                ?err,
                "Cannot schedule the X server's next start"
            );
        }
    }

    /// End a workspace's server and take its socket away.
    fn stop_realm_xwayland(&mut self, id: &str) {
        self.end_realm_server(id);
        let Some(realm) = self.common.realm_xwayland.remove(id) else {
            return;
        };
        if let Some(token) = realm.waiting {
            self.common.event_loop_handle.remove(token);
        }
        let _ = std::fs::remove_file(&realm.socket);
        info!(workspace = id, "The workspace's X server is gone");
    }

    /// Unmap a server's windows and end it, keeping its socket.
    fn end_realm_server(&mut self, id: &str) {
        let xwm = self
            .common
            .realm_xwayland
            .get(id)
            .and_then(|realm| realm.state.as_ref())
            .and_then(|state| state.xwm.as_ref())
            .map(X11Wm::id);
        if let Some(xwm) = xwm {
            self.unmap_x11_windows_of(xwm);
        }
        let Some(realm) = self.common.realm_xwayland.get_mut(id) else {
            return;
        };
        // The client first: dropping the source drops it, and the server with
        // it, so no request of theirs reaches the WM about to go.
        if let Some(token) = realm.server.take() {
            self.common.event_loop_handle.remove(token);
        }
        realm.state = None;
    }

    /// The workspace whose server `xwm` manages, if it is a workspace's.
    pub fn realm_of_xwm(&self, xwm: XwmId) -> Option<String> {
        realm_of_xwm(&self.common.realm_xwayland, xwm)
    }
}

fn realm_of_xwm(realms: &HashMap<String, RealmXwayland>, xwm: XwmId) -> Option<String> {
    realms
        .iter()
        .find(|(_, realm)| {
            realm
                .state
                .as_ref()
                .and_then(|state| state.xwm.as_ref())
                .is_some_and(|wm| wm.id() == xwm)
        })
        .map(|(id, _)| id.clone())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A runtime dir of the test's own, short enough for a socket path.
    fn runtime(name: &str) -> PathBuf {
        let dir = PathBuf::from(format!("/tmp/realm-x-{}-{name}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        dir
    }

    #[test]
    fn the_socket_is_where_the_workspace_mounts_its_x_dir() {
        assert_eq!(
            socket_path(Path::new("/run/user/1000"), "meridian"),
            Path::new("/run/user/1000/kora-workspaces/meridian/X11-unix/X0")
        );
    }

    #[test]
    fn an_id_never_leaves_the_runtime_dir() {
        assert!(valid_id("meridian"));
        assert!(valid_id("workspace-2"));
        assert!(!valid_id(""));
        assert!(!valid_id(".."));
        assert!(!valid_id("a/b"));
    }

    #[test]
    fn a_stale_socket_is_replaced_and_its_dir_kept_private() {
        let tmp = runtime("stale");
        let socket = socket_path(&tmp, "meridian");
        std::fs::create_dir_all(socket.parent().unwrap()).unwrap();
        drop(UnixListener::bind(&socket).unwrap());
        let realm = RealmXwayland::bind(socket.clone()).unwrap();
        assert!(std::os::unix::net::UnixStream::connect(&socket).is_ok());
        let mode = std::fs::metadata(socket.parent().unwrap())
            .unwrap()
            .permissions()
            .mode();
        assert_eq!(mode & 0o777, 0o700);
        drop(realm);
        let _ = std::fs::remove_dir_all(&tmp);
    }

    #[test]
    fn whoever_queued_for_a_failed_server_is_turned_away() {
        let tmp = runtime("refuse");
        let socket = socket_path(&tmp, "meridian");
        let realm = RealmXwayland::bind(socket.clone()).unwrap();
        let mut queued = std::os::unix::net::UnixStream::connect(&socket).unwrap();
        realm.refuse_queued();
        let mut buf = [0u8; 1];
        use std::io::Read;
        assert_eq!(
            queued.read(&mut buf).unwrap(),
            0,
            "closed, not left hanging"
        );
        let _ = std::fs::remove_dir_all(&tmp);
    }
}
