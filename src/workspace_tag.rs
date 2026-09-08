//! Which workspace a Wayland client belongs to.
//!
//! A *workspace* here is the user-facing one — the vertical axis, the context
//! boundary. (What this codebase's `Workspace` type calls a workspace is what
//! the user calls a Desktop; see the note on `Shell::workspaces`.)
//!
//! Every workspace process is launched into a `workspace-<id>.slice`, so a
//! client can be traced back to its workspace from its pid:
//!
//! ```text
//! peer pid → /proc/<pid>/cgroup → workspace-<id>.slice → <id>
//! ```
//!
//! Resolved **once, at connect**, and kept on `ClientState` — not per surface
//! and not per frame.
//!
//! Two properties make this work, and both are load-bearing:
//!
//! - It survives the sandbox. `SO_PEERCRED` reports the pid in the *receiving*
//!   process's namespace, so a client inside `bwrap --unshare-pid` — which
//!   believes it is pid 2 — still resolves here, because the compositor lives
//!   outside every sandbox. Measured, not assumed.
//! - A client in **no** workspace slice is machine-plane: the panel, dock,
//!   launcher, notifications and OSD are visible from every workspace. That is
//!   a default rather than an allowlist, so nothing has to be enumerated, and
//!   it means this changes no behaviour until workspaces are switched on.

use std::os::unix::io::AsRawFd;
use std::os::unix::net::UnixStream;

/// The workspace a client on this socket belongs to, or `None` for
/// machine-plane clients.
pub fn of_stream(stream: &UnixStream) -> Option<String> {
    of_pid(peer_pid(stream)?)
}

/// The workspace a pid belongs to, or `None` for machine-plane.
pub fn of_pid(pid: u32) -> Option<String> {
    let cgroup = std::fs::read_to_string(format!("/proc/{pid}/cgroup")).ok()?;
    of_cgroup(&cgroup)
}

/// Pull the workspace id out of a cgroup file's contents.
///
/// Split out from the filesystem so the parsing is testable on its own: this is
/// the function that decides whether a client is inside a boundary, and the
/// capture gating downstream is a launch-blocking guarantee.
pub fn of_cgroup(cgroup: &str) -> Option<String> {
    // The deepest match: systemd nests `workspace-a-b.slice` inside
    // `workspace-a.slice`, so the first segment names a prefix, not the id.
    cgroup
        .split(['/', ':', '\n'])
        .filter_map(|segment| {
            segment
                .strip_prefix("workspace-")
                .and_then(|rest| rest.strip_suffix(".slice"))
        })
        .next_back()
        .filter(|id| !id.is_empty() && id.chars().all(is_id_char))
        .map(str::to_string)
}

fn is_id_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '-' || c == '_'
}

fn peer_pid(stream: &UnixStream) -> Option<u32> {
    // SAFETY: `ucred` is written by the kernel; the fd outlives the call.
    unsafe {
        let mut cred: libc::ucred = std::mem::zeroed();
        let mut len = std::mem::size_of::<libc::ucred>() as libc::socklen_t;
        let rc = libc::getsockopt(
            stream.as_raw_fd(),
            libc::SOL_SOCKET,
            libc::SO_PEERCRED,
            (&raw mut cred).cast(),
            &raw mut len,
        );
        (rc == 0 && cred.pid > 0).then_some(cred.pid as u32)
    }
}

/// Is a client belonging to `client` visible while `active` is on screen?
///
/// Either side being `None` shows it: a machine-plane client belongs to no
/// workspace and is visible everywhere, and an unknown active workspace means
/// no workspace registry is running — in which case nothing should change from
/// today's behaviour.
pub fn visible_in(client: Option<&str>, active: Option<&str>) -> bool {
    match (client, active) {
        (Some(client), Some(active)) => client == active,
        _ => true,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn resolves_a_workspace_from_its_slice() {
        let cgroup = "0::/user.slice/user-1000.slice/user@1000.service/workspace.slice/workspace-meridian.slice/app.scope\n";
        assert_eq!(of_cgroup(cgroup).as_deref(), Some("meridian"));
    }

    #[test]
    fn a_dashed_id_is_read_from_the_deepest_slice() {
        let cgroup = "0::/user.slice/user-1000.slice/user@1000.service/workspace.slice/workspace-e2e.slice/workspace-e2e-a.slice/app.scope\n";
        assert_eq!(of_cgroup(cgroup).as_deref(), Some("e2e-a"));
    }

    #[test]
    fn a_client_outside_every_workspace_is_machine_plane() {
        // The panel, dock, launcher, notifications and OSD all land here and
        // are meant to be visible from every workspace.
        let cgroup = "0::/user.slice/user-1000.slice/user@1000.service/app.slice/app-panel.scope\n";
        assert_eq!(of_cgroup(cgroup), None);
    }

    #[test]
    fn the_parent_workspace_slice_alone_is_not_a_workspace() {
        // systemd nests the per-workspace slices under an implicit
        // `workspace.slice`; that parent is not itself a workspace.
        assert_eq!(
            of_cgroup("0::/user.slice/user@1000.service/workspace.slice\n"),
            None
        );
    }

    #[test]
    fn ids_with_dashes_and_underscores_survive() {
        assert_eq!(
            of_cgroup("0::/x/workspace-client_acme-co.slice/y").as_deref(),
            Some("client_acme-co")
        );
    }

    #[test]
    fn a_lookalike_does_not_resolve() {
        assert_eq!(of_cgroup("0::/x/workspace-meridian.scope/y"), None);
        assert_eq!(of_cgroup("0::/x/notaworkspace.slice/y"), None);
    }

    #[test]
    fn a_clients_own_workspace_is_visible() {
        assert!(visible_in(Some("meridian"), Some("meridian")));
    }

    #[test]
    fn another_workspaces_client_is_not() {
        // This is the capture guarantee: a client demo can never leak another
        // client's work (W-10).
        assert!(!visible_in(Some("personal"), Some("meridian")));
    }

    #[test]
    fn machine_plane_clients_are_visible_everywhere() {
        assert!(visible_in(None, Some("meridian")));
    }

    #[test]
    fn nothing_is_hidden_when_no_registry_is_running() {
        assert!(visible_in(Some("meridian"), None));
        assert!(visible_in(None, None));
    }

    #[test]
    fn our_own_pid_is_machine_plane_in_a_normal_test_run() {
        assert_eq!(of_pid(std::process::id()), None);
    }
}
