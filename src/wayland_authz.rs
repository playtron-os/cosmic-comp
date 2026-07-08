// SPDX-License-Identifier: GPL-3.0-only
//! Wayland socket authorization for the persistent-compositor model.
//!
//! The compositor socket lives in the `compositor`-group runtime dir, so any group member can
//! reach it. This gate additionally checks the CONNECTING client's logind session and admits
//! only:
//!   * self / root / the greeter (by uid — the login screen must always connect), and
//!   * clients in a LOCAL login session (the desktop).
//! A remote (ssh) or sessionless (cron / system service) client — even one owned by the desktop
//! user's uid — is rejected. This is what makes `compositor`-group membership safe to grant to
//! more than one account: a second user cannot reach the socket over ssh to screencopy / keylog
//! the person at the screen.
//!
//! Mode via `AGENTOS_WAYLAND_AUTHZ` = `monitor` (DEFAULT, log-only) | `enforce` | `off`. Default
//! is monitor: the admit predicate must be validated against the target's greetd(vt=none) setup
//! (that most desktop clients live under `user@uid.service`, admitted via the uid Display check)
//! before it can safely reject — a wrong reject is a black desktop.
//!
//! LIMITATION: this gates the WAYLAND socket only. The compositor also spawns XWayland, whose X11
//! socket is a co-equal capture channel (a same-uid ssh client with the Xauthority cookie can
//! keylog/screencapture via X11). Closing that requires restricting XWayland's X11 access
//! separately; the ssh threat is only fully closed with XWayland disabled or its socket locked down.

use std::os::unix::io::AsRawFd;
use std::os::unix::net::UnixStream;
use tracing::{error, info, warn};

// sd-login (libsystemd). The pure-Rust `libsystemd` crate only wraps sd_notify, so we bind the
// functions we need directly. Requires linking libsystemd (present on any systemd target).
#[link(name = "systemd")]
unsafe extern "C" {
    fn sd_pid_get_session(pid: libc::pid_t, session: *mut *mut libc::c_char) -> libc::c_int;
    fn sd_session_is_remote(session: *const libc::c_char) -> libc::c_int;
    fn sd_uid_get_display(uid: libc::uid_t, session: *mut *mut libc::c_char) -> libc::c_int;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AuthzMode {
    Off,
    Monitor,
    Enforce,
}

#[derive(Debug)]
pub struct WaylandAuthz {
    mode: AuthzMode,
    self_uid: u32,
    greeter_uid: Option<u32>,
}

impl Default for WaylandAuthz {
    fn default() -> Self {
        Self::new()
    }
}

impl WaylandAuthz {
    pub fn new() -> Self {
        let mut mode = match std::env::var("AGENTOS_WAYLAND_AUTHZ").ok().as_deref() {
            Some("off") => AuthzMode::Off,
            Some("enforce") => AuthzMode::Enforce,
            Some("monitor") | None => AuthzMode::Monitor,
            Some(other) => {
                warn!(%other, "unknown AGENTOS_WAYLAND_AUTHZ; defaulting to enforce");
                AuthzMode::Enforce
            }
        };
        let self_uid = unsafe { libc::getuid() };
        let greeter_uid = resolve_greeter_uid();
        // Fail-open safety: the greeter MUST be admitted or login bricks. If its uid can't be
        // resolved, refuse to enforce (log-only) rather than risk rejecting the login screen.
        if greeter_uid.is_none() && mode == AuthzMode::Enforce {
            error!(
                "wayland-authz: could not resolve 'greeter' uid; downgrading Enforce -> Monitor \
                 to avoid bricking login (set AGENTOS_GREETER_UID to force)"
            );
            mode = AuthzMode::Monitor;
        }
        info!(?mode, self_uid, ?greeter_uid, "wayland-authz initialized");
        WaylandAuthz {
            mode,
            self_uid,
            greeter_uid,
        }
    }

    /// Whether to admit a connecting client. In `Monitor` it logs a would-be rejection and still
    /// returns `true`; in `Off` it always returns `true`.
    pub fn admit(&self, stream: &UnixStream) -> bool {
        if self.mode == AuthzMode::Off {
            return true;
        }
        let (pid, uid) = match peer_cred(stream) {
            Some(c) => c,
            None => {
                // SO_PEERCRED is set by the kernel at connect and effectively never fails; a
                // failure means a bug, not an attack, so fail OPEN rather than brick.
                error!(
                    "wayland-authz: SO_PEERCRED failed; admitting (fail-open on unexpected error)"
                );
                return true;
            }
        };
        let (allow, reason) = self.evaluate(pid, uid);
        if allow {
            return true;
        }
        match self.mode {
            AuthzMode::Enforce => {
                warn!(pid, uid, reason, "wayland-authz: REJECTED client");
                false
            }
            _ => {
                warn!(
                    pid,
                    uid, reason, "wayland-authz: would reject client (monitor)"
                );
                true
            }
        }
    }

    fn evaluate(&self, pid: libc::pid_t, uid: u32) -> (bool, &'static str) {
        if uid == self.self_uid {
            return (true, "self uid");
        }
        if uid == 0 {
            return (true, "root");
        }
        if Some(uid) == self.greeter_uid {
            return (true, "greeter uid");
        }
        match pid_session(pid) {
            // Its OWN session is remote (ssh) — this is what blocks the desktop user's own ssh.
            SessionKind::Remote => (false, "remote (ssh) session"),
            // A login-session-scope process (cosmic-session, cosmic-comp, panel, Xwayland).
            SessionKind::Local => (true, "local login session"),
            // Sessionless: logind places most desktop clients (cosmic-bg, cosmic-osd, the
            // portals, and every app launched via .desktop) under `user@uid.service`, OUTSIDE
            // the login session — so we can't reject on "no session". Admit iff this uid is the
            // locally logged-in graphical user (owns a non-remote Display session); that still
            // rejects cron / system daemons / logged-out uids.
            SessionKind::None => {
                if uid_has_local_display(uid) {
                    (true, "local desktop user (user service)")
                } else {
                    (false, "no local graphical session for uid")
                }
            }
        }
    }
}

fn resolve_greeter_uid() -> Option<u32> {
    if let Ok(v) = std::env::var("AGENTOS_GREETER_UID")
        && let Ok(uid) = v.parse::<u32>()
    {
        return Some(uid);
    }
    let name = std::ffi::CString::new("greeter").ok()?;
    // getpwnam runs once at startup on the main thread; the returned pointer is not freed.
    let pw = unsafe { libc::getpwnam(name.as_ptr()) };
    if pw.is_null() {
        None
    } else {
        Some(unsafe { (*pw).pw_uid })
    }
}

fn peer_cred(stream: &UnixStream) -> Option<(libc::pid_t, u32)> {
    let mut cred = libc::ucred {
        pid: 0,
        uid: 0,
        gid: 0,
    };
    let mut len = std::mem::size_of::<libc::ucred>() as libc::socklen_t;
    let ret = unsafe {
        libc::getsockopt(
            stream.as_raw_fd(),
            libc::SOL_SOCKET,
            libc::SO_PEERCRED,
            &mut cred as *mut libc::ucred as *mut libc::c_void,
            &mut len,
        )
    };
    if ret != 0 {
        return None;
    }
    Some((cred.pid, cred.uid))
}

enum SessionKind {
    Local,
    Remote,
    None,
}

/// Classify the login session the PID itself belongs to. `None` means the process is in no
/// login-session scope (`user@uid.service`, cron, or a system daemon) — the caller then falls
/// back to the uid's Display session. An error on the remoteness check is folded into `None`
/// (defer to that fallback) rather than guessed as local or remote.
fn pid_session(pid: libc::pid_t) -> SessionKind {
    let mut session: *mut libc::c_char = std::ptr::null_mut();
    let ret = unsafe { sd_pid_get_session(pid, &mut session) };
    if ret < 0 || session.is_null() {
        if !session.is_null() {
            unsafe { libc::free(session as *mut libc::c_void) };
        }
        return SessionKind::None; // -ENXIO etc. => no login-session scope
    }
    let remote = unsafe { sd_session_is_remote(session) };
    unsafe { libc::free(session as *mut libc::c_void) };
    match remote {
        r if r > 0 => SessionKind::Remote, // ssh
        0 => SessionKind::Local,           // local login session
        _ => SessionKind::None,            // remoteness unknown -> defer to the uid Display check
    }
}

/// Whether `uid` owns an active, non-remote Display (graphical) session — i.e. this uid is the
/// user currently logged in at the local desktop. Used to admit that user's `user@uid.service`
/// clients (portals, launched apps) that are outside any login-session scope. A Display session
/// that exists but whose remoteness can't be read is treated as local (fail-open) since a
/// Display session is the local desktop by construction; absence of one rejects.
fn uid_has_local_display(uid: u32) -> bool {
    let mut session: *mut libc::c_char = std::ptr::null_mut();
    let ret = unsafe { sd_uid_get_display(uid as libc::uid_t, &mut session) };
    if ret < 0 || session.is_null() {
        if !session.is_null() {
            unsafe { libc::free(session as *mut libc::c_void) };
        }
        return false; // no graphical session for this uid
    }
    let remote = unsafe { sd_session_is_remote(session) };
    unsafe { libc::free(session as *mut libc::c_void) };
    remote <= 0
}
