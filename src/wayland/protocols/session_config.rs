// SPDX-License-Identifier: GPL-3.0-only

//! Implementation of the session config protocol (agentos_session_config_v1)
//!
//! In the persistent-compositor model the compositor runs as its own system user and outlives
//! every session, so it reads its own configuration and never sees the config of whoever is logged
//! in. Nothing the user changes -- night shift, shortcuts, tiling -- reaches it.
//!
//! This lets the logged-in session push settings in over the wayland socket it is already
//! connected to. Received entries are written into the compositor's OWN cosmic-config store, so
//! the existing config watcher fires and the existing apply path runs unchanged; neither side
//! needs filesystem access to the other.
//!
//! AUTHORIZATION: the global is advertised only to clients whose uid matches the session that
//! currently holds the screen, which the compositor learns at the login handoff. A client of any
//! other user never sees the global, so the check is structural rather than a request-time test.
//! `set_desktop_uid(None)` at logout withdraws it.

use smithay::{
    reexports::wayland_server::{
        Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource, backend::GlobalId,
    },
    wayland::{Dispatch2, GlobalDispatch2},
};
use std::collections::HashSet;
use std::sync::{
    Arc, Mutex,
    atomic::{AtomicI64, Ordering},
};
use tracing::{debug, warn};

mod generated {
    use smithay::reexports::wayland_server;

    pub mod __interfaces {
        use wayland_backend;
        wayland_scanner::generate_interfaces!("resources/protocols/agentos-session-config.xml");
    }
    use self::__interfaces::*;

    wayland_scanner::generate_server_code!("resources/protocols/agentos-session-config.xml");
}

pub use generated::agentos_session_config_v1;

/// Domains a session may push. Anything else is refused: the compositor writes what it receives
/// into its own store, so an unbounded domain would let a client create arbitrary files there.
///
/// DELIBERATELY EXCLUDES com.system76.CosmicSettings.Shortcuts. Its `custom`/`defaults` keys
/// deserialize into shortcut actions including `Action::Spawn(String)`, and `system_actions` is a
/// map of raw shell commands; both reach spawn_command (src/input/actions.rs:1183,1187), which
/// runs `/bin/sh -c` IN THE COMPOSITOR PROCESS. Since the compositor runs as agentos-display --
/// holder of seat0, DRM master and the `input` group, i.e. raw evdev for every session including
/// the greeter's password prompt -- accepting that domain would let any client of the desktop uid
/// execute code across the exact privilege boundary this model exists to create.
///
/// Re-adding it requires parsing the value and rejecting every Spawn action first, not just
/// trusting the sender.
const ALLOWED_DOMAINS: &[&str] = &[
    "com.system76.CosmicComp",
    "com.system76.CosmicTk",
    "com.system76.CosmicSettings.WindowRules",
    "com.playtron.VoiceMode",
];

/// Sentinel for "no session owns the screen" — no uid may bind.
const NO_UID: i64 = -1;

/// Bounds on what one session may push. The store lives in /run, which is tmpfs, so an unbounded
/// sender spends the machine's RAM and the shared inode pool -- and every write wakes the
/// compositor's own config watcher, so it costs main-loop time too. A real sender pushes a handful
/// of small keys; these are far above that and only bite a runaway or hostile client.
const MAX_ENTRIES: usize = 256;
const MAX_VALUE_BYTES: usize = 64 * 1024;

pub struct SessionConfigState {
    /// Created at login and removed at logout, NOT merely hidden: `can_view` is consulted only
    /// when a client binds its registry and when a global is created, so flipping it cannot
    /// advertise anything to a client that connected earlier — which every session client does.
    global: Option<GlobalId>,
    desktop_uid: Arc<AtomicI64>,
    dh: DisplayHandle,
    /// What this session pushed, so logout removes exactly those and nothing the compositor
    /// wrote for itself (autotile, pinned workspaces, zoom all live in the same store).
    received: Received,
}

type Received = Arc<Mutex<HashSet<(String, String)>>>;

impl std::fmt::Debug for SessionConfigState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SessionConfigState")
            .field("desktop_uid", &self.desktop_uid.load(Ordering::Relaxed))
            .finish()
    }
}

pub struct SessionConfigGlobalData {
    desktop_uid: Arc<AtomicI64>,
    received: Received,
}

impl SessionConfigState {
    pub fn new(dh: &DisplayHandle) -> SessionConfigState {
        SessionConfigState {
            global: None,
            desktop_uid: Arc::new(AtomicI64::new(NO_UID)),
            dh: dh.clone(),
            received: Received::default(),
        }
    }

    /// Offer the protocol to the session that currently holds the screen; `None` withdraws it.
    ///
    /// Creating the global is what makes clients hear about it: an existing global is announced
    /// only to registries bound afterwards, and session clients are already connected by the time
    /// a login completes. Removing it at logout sends `global_remove`, stopping the outgoing
    /// session from pushing.
    pub fn set_desktop_uid<D>(&mut self, uid: Option<u32>)
    where
        D: GlobalDispatch<
                agentos_session_config_v1::AgentosSessionConfigV1,
                SessionConfigGlobalData,
            > + Dispatch<agentos_session_config_v1::AgentosSessionConfigV1, SessionConfigData>
            + 'static,
    {
        self.desktop_uid
            .store(uid.map_or(NO_UID, i64::from), Ordering::Relaxed);
        match (uid, self.global.take()) {
            (Some(uid), None) => {
                self.global = Some(
                    self.dh
                        .create_global::<D, agentos_session_config_v1::AgentosSessionConfigV1, _>(
                            1,
                            SessionConfigGlobalData {
                                desktop_uid: self.desktop_uid.clone(),
                                received: self.received.clone(),
                            },
                        ),
                );
                debug!(uid, "session-config: offered to the session");
            }
            // Re-login as the same or a different user: the global stands, and can_view already
            // keys on the uid we just stored.
            (Some(_), Some(existing)) => self.global = Some(existing),
            (None, Some(existing)) => {
                self.dh.remove_global::<D>(existing);
                self.discard_entries();
                debug!("session-config: withdrawn");
            }
            (None, None) => self.discard_entries(),
        }
    }

    pub fn global_id(&self) -> Option<&GlobalId> {
        self.global.as_ref()
    }

    /// Drop everything the ending session pushed. Removing a key file makes the compositor's own
    /// watcher fire and `get_config` fall back to the default, so the setting is actually undone
    /// rather than merely forgotten -- otherwise the greeter, and any user who logs in next
    /// without that key set, would silently inherit it.
    fn discard_entries(&self) {
        let Ok(mut received) = self.received.lock() else {
            return;
        };
        for (domain, key) in received.drain() {
            if let Some(dir) = domain_dir(&domain)
                && let Err(err) = std::fs::remove_file(dir.join(&key))
                && err.kind() != std::io::ErrorKind::NotFound
            {
                warn!(domain, key, ?err, "session-config: failed to discard entry");
            }
        }
    }
}

impl<D> GlobalDispatch2<agentos_session_config_v1::AgentosSessionConfigV1, D>
    for SessionConfigGlobalData
where
    D: Dispatch<agentos_session_config_v1::AgentosSessionConfigV1, SessionConfigData> + 'static,
{
    fn bind(
        &self,
        _state: &mut D,
        _dh: &DisplayHandle,
        _client: &Client,
        resource: New<agentos_session_config_v1::AgentosSessionConfigV1>,
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(
            resource,
            SessionConfigData {
                received: self.received.clone(),
                desktop_uid: self.desktop_uid.clone(),
            },
        );
    }

    /// Only the session holding the screen ever sees the global. Re-checked per request, since
    /// binding is not a lasting permission -- see `is_desktop_client`.
    fn can_view(&self, client: &Client) -> bool {
        is_desktop_client(&self.desktop_uid, client)
    }
}

/// Whether `client` belongs to the session that currently holds the screen.
///
/// The uid comes from the client's own data, captured at accept. Asking the backend instead
/// (get_credentials) deadlocks: this runs from `can_view`, which the backend calls while holding
/// its state lock.
fn is_desktop_client(desktop_uid: &AtomicI64, client: &Client) -> bool {
    let target = desktop_uid.load(Ordering::Relaxed);
    if target == NO_UID {
        return false;
    }
    client
        .get_data::<crate::state::ClientState>()
        .and_then(|s| s.uid)
        .is_some_and(|uid| i64::from(uid) == target)
}

pub struct SessionConfigData {
    received: Received,
    desktop_uid: Arc<AtomicI64>,
}

impl<D> Dispatch2<agentos_session_config_v1::AgentosSessionConfigV1, D> for SessionConfigData
where
    D: 'static,
{
    fn request(
        &self,
        _state: &mut D,
        client: &Client,
        _resource: &agentos_session_config_v1::AgentosSessionConfigV1,
        request: <agentos_session_config_v1::AgentosSessionConfigV1 as Resource>::Request,
        _dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            agentos_session_config_v1::Request::SetEntry { domain, key, value } => {
                // Re-checked here, not just at bind: an object outlives the session that created
                // it, so without this a client from the previous login keeps writing into the
                // greeter and into whoever logs in next.
                if !is_desktop_client(&self.desktop_uid, client) {
                    warn!(
                        domain,
                        key, "session-config: ignoring entry from a stale session"
                    );
                    return;
                }
                store_entry(&self.received, &domain, &key, &value);
            }
            agentos_session_config_v1::Request::Destroy => {}
        }
    }
}

/// Write one entry into the compositor's own cosmic-config store, which makes its existing
/// watcher fire and the normal apply path run. Rejects anything that could escape the store.
fn store_entry(received: &Received, domain: &str, key: &str, value: &str) {
    if !ALLOWED_DOMAINS.contains(&domain) {
        warn!(domain, "session-config: refusing unknown domain");
        return;
    }
    // The key becomes a filename. Anything but a plain name could escape the domain directory.
    if key.is_empty() || !key.bytes().all(|b| b.is_ascii_alphanumeric() || b == b'_') {
        warn!(key, "session-config: refusing malformed key");
        return;
    }
    if value.len() > MAX_VALUE_BYTES {
        warn!(
            domain,
            key,
            bytes = value.len(),
            "session-config: refusing oversized value"
        );
        return;
    }
    // Counted on distinct keys, so repeatedly updating one key stays free.
    if let Ok(received) = received.lock()
        && received.len() >= MAX_ENTRIES
        && !received.contains(&(domain.to_string(), key.to_string()))
    {
        warn!(domain, key, "session-config: entry limit reached, refusing");
        return;
    }
    let Some(dir) = domain_dir(domain) else {
        warn!(domain, "session-config: no writable config path");
        return;
    };
    if let Err(err) = write_atomic(&dir, key, value) {
        warn!(domain, key, ?err, "session-config: failed to store entry");
        return;
    }
    if let Ok(mut received) = received.lock() {
        received.insert((domain.to_string(), key.to_string()));
    }
    debug!(domain, key, "session-config: stored entry");
}

/// Mirrors cosmic_config::Config::new's layout (<config dir>/cosmic/<domain>/v<version>); it
/// exposes no path accessor. Callers pass an allowlisted domain, never raw input.
fn domain_dir(domain: &str) -> Option<std::path::PathBuf> {
    dirs::config_dir().map(|d| d.join("cosmic").join(domain).join("v1"))
}

/// Temp file + rename, matching how cosmic-config writes, so a reader never sees a partial value
/// and the watcher sees a single rename rather than a growing file.
fn write_atomic(dir: &std::path::Path, key: &str, value: &str) -> std::io::Result<()> {
    use std::io::Write as _;
    use std::os::unix::fs::{OpenOptionsExt as _, PermissionsExt as _};
    std::fs::create_dir_all(dir)?;
    // Explicit modes rather than trusting the process umask: this store is the compositor's own
    // configuration, and anything writable by another user is a way to change what the compositor
    // does. The dir is under /run/cosmic-comp, which the whole `compositor` group can traverse.
    std::fs::set_permissions(dir, std::fs::Permissions::from_mode(0o700))?;
    let tmp = dir.join(format!(".{key}.tmp"));
    {
        // O_EXCL so a pre-planted temp path is an error rather than something we write through.
        let mut f = std::fs::OpenOptions::new()
            .write(true)
            .create_new(true)
            .mode(0o600)
            .open(&tmp)
            .or_else(|_| {
                let _ = std::fs::remove_file(&tmp);
                std::fs::OpenOptions::new()
                    .write(true)
                    .create_new(true)
                    .mode(0o600)
                    .open(&tmp)
            })?;
        f.write_all(value.as_bytes())?;
        f.sync_all()?;
    }
    std::fs::rename(&tmp, dir.join(key))
}
