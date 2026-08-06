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
use std::sync::{
    Arc,
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
const ALLOWED_DOMAINS: &[&str] = &[
    "com.system76.CosmicComp",
    "com.system76.CosmicTk",
    "com.system76.CosmicSettings.Shortcuts",
    "com.system76.CosmicSettings.WindowRules",
    "com.playtron.VoiceMode",
];

/// Sentinel for "no session owns the screen" — no uid may bind.
const NO_UID: i64 = -1;

pub struct SessionConfigState {
    global: GlobalId,
    desktop_uid: Arc<AtomicI64>,
}

impl std::fmt::Debug for SessionConfigState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SessionConfigState")
            .field("desktop_uid", &self.desktop_uid.load(Ordering::Relaxed))
            .finish()
    }
}

pub struct SessionConfigGlobalData {
    desktop_uid: Arc<AtomicI64>,
}

impl SessionConfigState {
    pub fn new<D>(dh: &DisplayHandle) -> SessionConfigState
    where
        D: GlobalDispatch<
                agentos_session_config_v1::AgentosSessionConfigV1,
                SessionConfigGlobalData,
            > + Dispatch<agentos_session_config_v1::AgentosSessionConfigV1, SessionConfigData>
            + 'static,
    {
        let desktop_uid = Arc::new(AtomicI64::new(NO_UID));
        let global = dh.create_global::<D, agentos_session_config_v1::AgentosSessionConfigV1, _>(
            1,
            SessionConfigGlobalData {
                desktop_uid: desktop_uid.clone(),
            },
        );
        SessionConfigState {
            global,
            desktop_uid,
        }
    }

    /// Restrict the global to the session that currently holds the screen. `None` withdraws it.
    /// Already-bound objects of a previous user are not force-destroyed; they stop mattering
    /// because their session is gone, and the compositor drops the received entries at logout.
    pub fn set_desktop_uid(&self, uid: Option<u32>) {
        self.desktop_uid
            .store(uid.map_or(NO_UID, i64::from), Ordering::Relaxed);
        debug!(?uid, "session-config: visible to");
    }

    pub fn global_id(&self) -> &GlobalId {
        &self.global
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
        data_init.init(resource, SessionConfigData);
    }

    /// The whole authorization model: only the session holding the screen ever sees the global.
    ///
    /// The uid comes from the client's own data, captured at accept. Asking the backend here
    /// (get_credentials) deadlocks: can_view runs while the backend holds its state lock.
    fn can_view(&self, client: &Client) -> bool {
        let target = self.desktop_uid.load(Ordering::Relaxed);
        if target == NO_UID {
            return false;
        }
        client
            .get_data::<crate::state::ClientState>()
            .and_then(|s| s.uid)
            .is_some_and(|uid| i64::from(uid) == target)
    }
}

pub struct SessionConfigData;

impl<D> Dispatch2<agentos_session_config_v1::AgentosSessionConfigV1, D> for SessionConfigData
where
    D: 'static,
{
    fn request(
        &self,
        _state: &mut D,
        _client: &Client,
        _resource: &agentos_session_config_v1::AgentosSessionConfigV1,
        request: <agentos_session_config_v1::AgentosSessionConfigV1 as Resource>::Request,
        _dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            agentos_session_config_v1::Request::SetEntry { domain, key, value } => {
                store_entry(&domain, &key, &value);
            }
            agentos_session_config_v1::Request::Destroy => {}
        }
    }
}

/// Write one entry into the compositor's own cosmic-config store, which makes its existing
/// watcher fire and the normal apply path run. Rejects anything that could escape the store.
fn store_entry(domain: &str, key: &str, value: &str) {
    if !ALLOWED_DOMAINS.contains(&domain) {
        warn!(domain, "session-config: refusing unknown domain");
        return;
    }
    // The key becomes a filename. Anything but a plain name could escape the domain directory.
    if key.is_empty() || !key.bytes().all(|b| b.is_ascii_alphanumeric() || b == b'_') {
        warn!(key, "session-config: refusing malformed key");
        return;
    }
    // Mirrors cosmic_config::Config::new's layout (<config dir>/cosmic/<domain>/v<version>);
    // it exposes no path accessor. The domain is from the allowlist above, never raw input.
    let Some(dir) = dirs::config_dir().map(|d| d.join("cosmic").join(domain).join("v1")) else {
        warn!(domain, "session-config: no writable config path");
        return;
    };
    if let Err(err) = write_atomic(&dir, key, value) {
        warn!(domain, key, ?err, "session-config: failed to store entry");
        return;
    }
    debug!(domain, key, "session-config: stored entry");
}

/// Temp file + rename, matching how cosmic-config writes, so a reader never sees a partial value
/// and the watcher sees a single rename rather than a growing file.
fn write_atomic(dir: &std::path::Path, key: &str, value: &str) -> std::io::Result<()> {
    use std::io::Write as _;
    std::fs::create_dir_all(dir)?;
    let tmp = dir.join(format!(".{key}.tmp"));
    {
        let mut f = std::fs::File::create(&tmp)?;
        f.write_all(value.as_bytes())?;
        f.sync_all()?;
    }
    std::fs::rename(&tmp, dir.join(key))
}
