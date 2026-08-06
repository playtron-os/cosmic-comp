// SPDX-License-Identifier: GPL-3.0-only
//! Narrow the compositor runtime dir to the logged-in desktop user after login.
//!
//! `/run/cosmic-comp` ships 0750 agentos-display:compositor, so EVERY `compositor`-group member
//! (the greeter and every other desktop user) can traverse it and reach the wayland socket. Once
//! a desktop session owns the screen, only that user needs in — so we swap the group grant for a
//! POSIX ACL naming just that uid, and restore the group grant on logout so the greeter returns.
//!
//! DAC only. It complements [`crate::wayland_authz`] (which can fail open) but does NOT stop a
//! same-uid attacker: that needs MAC or no shell access.
//!
//! Mechanism is an ACL because no other option fits: chmod cannot express "owner + one named
//! uid", and chown/chgrp need CAP_CHOWN or the target gid in our group set — neither is granted.
//! setxattr on a non-`security.*` attribute only requires owning the file, which we do.
//!
//! Mode via `AGENTOS_RUNTIME_DIR_NARROW` = `off` (DEFAULT) | `monitor` | `enforce`.
//! Recovery if it ever misfires: `systemctl restart cosmic-comp-global` — systemd deletes and
//! recreates RuntimeDirectory, dropping the ACL.

use std::path::PathBuf;
use tracing::{error, info, warn};

const ACL_XATTR: &[u8] = b"system.posix_acl_access\0";

// posix_acl_xattr entry tags/perms (uapi/linux/posix_acl_xattr.h). Entries must be tag-sorted or
// the kernel rejects the blob with EINVAL.
const ACL_VERSION: u32 = 2;
const TAG_USER_OBJ: u16 = 0x01;
const TAG_USER: u16 = 0x02;
const TAG_GROUP_OBJ: u16 = 0x04;
const TAG_MASK: u16 = 0x10;
const TAG_OTHER: u16 = 0x20;
const PERM_RX: u16 = 5;
const PERM_RWX: u16 = 7;
const PERM_NONE: u16 = 0;
const ID_UNDEFINED: u32 = 0xFFFF_FFFF;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NarrowMode {
    Off,
    Monitor,
    Enforce,
}

#[derive(Debug)]
pub struct RuntimeDirGate {
    mode: NarrowMode,
    path: Option<PathBuf>,
    narrowed_for: Option<u32>,
}

impl Default for RuntimeDirGate {
    fn default() -> Self {
        Self::new()
    }
}

impl RuntimeDirGate {
    pub fn new() -> Self {
        let mode = match std::env::var("AGENTOS_RUNTIME_DIR_NARROW").ok().as_deref() {
            Some("enforce") => NarrowMode::Enforce,
            Some("monitor") => NarrowMode::Monitor,
            Some("off") | None => NarrowMode::Off,
            Some(other) => {
                warn!(%other, "unknown AGENTOS_RUNTIME_DIR_NARROW; defaulting to off");
                NarrowMode::Off
            }
        };
        // Only manage a dir we own; XDG_RUNTIME_DIR is /run/cosmic-comp under the unit.
        let path = std::env::var("XDG_RUNTIME_DIR").ok().map(PathBuf::from);
        let mut gate = RuntimeDirGate {
            mode,
            path,
            narrowed_for: None,
        };
        // A stale ACL from a previous run would lock the greeter out before anything else runs.
        if mode != NarrowMode::Off {
            gate.clear_acl();
        }
        info!(?mode, path = ?gate.path, "runtime-dir gate initialized");
        gate
    }

    pub fn is_narrowed(&self) -> bool {
        self.narrowed_for.is_some()
    }

    /// Restrict the runtime dir to `uid` (plus the owning compositor). Idempotent.
    pub fn narrow_to(&mut self, uid: u32) {
        if self.mode == NarrowMode::Off || self.narrowed_for == Some(uid) {
            return;
        }
        if let Some(prev) = self.narrowed_for.filter(|p| *p != uid) {
            warn!(
                prev,
                uid, "runtime-dir gate: re-narrowing to a different uid"
            );
        }
        if self.mode == NarrowMode::Monitor {
            // Track state in monitor too, or restore/deadman have nothing to react to and the
            // dry-run silently cannot exercise the half that matters.
            self.narrowed_for = Some(uid);
            info!(uid, "runtime-dir gate: would narrow (monitor)");
            return;
        }
        // Whole-blob write, never incremental, so a re-narrow replaces rather than accumulates.
        let mut acl = Vec::with_capacity(44);
        acl.extend_from_slice(&ACL_VERSION.to_ne_bytes());
        push_entry(&mut acl, TAG_USER_OBJ, PERM_RWX, ID_UNDEFINED);
        push_entry(&mut acl, TAG_USER, PERM_RX, uid);
        push_entry(&mut acl, TAG_GROUP_OBJ, PERM_NONE, ID_UNDEFINED); // drops `compositor`
        push_entry(&mut acl, TAG_MASK, PERM_RX, ID_UNDEFINED); // must not mask off the uid entry
        push_entry(&mut acl, TAG_OTHER, PERM_NONE, ID_UNDEFINED);

        match self.set_acl(&acl) {
            Ok(()) => {
                self.narrowed_for = Some(uid);
                info!(uid, "runtime-dir gate: narrowed to desktop uid");
            }
            // Leave narrowed_for unset so a later restore doesn't claim to undo what never applied.
            Err(e) => error!(uid, error = %e, "runtime-dir gate: narrow failed; staying open"),
        }
    }

    /// Restore group access so the greeter can reach the socket again. Idempotent, and always
    /// attempts the syscalls — tracked state may be wrong, and a missed restore bricks login.
    pub fn restore(&mut self) {
        if self.mode == NarrowMode::Off {
            return;
        }
        let was = self.narrowed_for.take();
        if self.mode == NarrowMode::Monitor {
            if was.is_some() {
                info!("runtime-dir gate: would restore (monitor)");
            }
            return;
        }
        if self.clear_acl() && was.is_some() {
            info!("runtime-dir gate: restored group access");
        }
    }

    /// Drop any ACL and put the mode back to systemd's RuntimeDirectoryMode=0750.
    fn clear_acl(&mut self) -> bool {
        use std::os::unix::fs::PermissionsExt;
        let Some(path) = self.path.clone() else {
            return false;
        };
        let Ok(cpath) = path_to_c(&path) else {
            return false;
        };
        let ret = unsafe { libc::removexattr(cpath.as_ptr(), ACL_XATTR.as_ptr() as *const _) };
        // ENODATA just means there was no ACL — the normal case.
        if ret != 0 {
            let e = std::io::Error::last_os_error();
            if e.raw_os_error() != Some(libc::ENODATA) {
                error!(error = %e, "runtime-dir gate: removexattr failed");
                return false;
            }
        }
        if let Err(e) = std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o750)) {
            error!(error = %e, "runtime-dir gate: chmod 0750 failed");
            return false;
        }
        true
    }

    fn set_acl(&self, acl: &[u8]) -> std::io::Result<()> {
        let path = self
            .path
            .as_ref()
            .ok_or_else(|| std::io::Error::other("no XDG_RUNTIME_DIR"))?;
        let cpath = path_to_c(path)?;
        let ret = unsafe {
            libc::setxattr(
                cpath.as_ptr(),
                ACL_XATTR.as_ptr() as *const _,
                acl.as_ptr() as *const _,
                acl.len(),
                0,
            )
        };
        if ret != 0 {
            return Err(std::io::Error::last_os_error());
        }
        Ok(())
    }
}

fn push_entry(buf: &mut Vec<u8>, tag: u16, perm: u16, id: u32) {
    buf.extend_from_slice(&tag.to_ne_bytes());
    buf.extend_from_slice(&perm.to_ne_bytes());
    buf.extend_from_slice(&id.to_ne_bytes());
}

fn path_to_c(path: &std::path::Path) -> std::io::Result<std::ffi::CString> {
    use std::os::unix::ffi::OsStrExt;
    std::ffi::CString::new(path.as_os_str().as_bytes()).map_err(std::io::Error::other)
}
