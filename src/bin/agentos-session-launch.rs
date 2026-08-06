//! agentos-session-launch — set up the user manager for a seatless greetd desktop
//! session, then drop privileges and exec the user's desktop.

use std::ffi::{CStr, CString};
use std::os::unix::process::CommandExt;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitCode};
use std::time::Duration;

/// Absolute paths: this binary is setuid root, so $PATH comes from whoever invoked it. Resolving
/// a helper through it would run an attacker's binary as root while we hold euid=ruid=0.
const SYSTEMCTL: &str = "/usr/bin/systemctl";
/// Handed to every child so nothing we spawn inherits the caller's search path either.
const SAFE_PATH: &str = "/usr/sbin:/usr/bin:/sbin:/bin";
/// The compositor's X auth file, and the non-secret file naming the display it serves. The auth
/// file is 0600 plus an ACL the compositor grants to the session that holds the screen.
const XAUTH_SERVER: &str = "/run/cosmic-comp/xauth";
const XDISPLAY_HANDOFF: &str = "/run/cosmic-comp/xcookie";

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().collect();
    let prog = args
        .first()
        .map(String::as_str)
        .unwrap_or("agentos-session-launch");
    if args.len() < 2 {
        eprintln!("usage: {prog} <cmd> [args...]");
        return ExitCode::from(2);
    }

    // SAFETY: the libc uid/gid/passwd/priv-drop calls below are FFI; each return
    // value is checked. Ordering matches the validated C helper.
    unsafe {
        if libc::geteuid() != 0 {
            eprintln!("agentos-session-launch: must be setuid root");
            return ExitCode::from(2);
        }

        // Target user = the real uid greetd already authenticated + set.
        let uid = libc::getuid();
        let gid = libc::getgid();
        let pw = libc::getpwuid(uid);
        let user: Option<CString> = if pw.is_null() || (*pw).pw_name.is_null() {
            None
        } else {
            Some(CStr::from_ptr((*pw).pw_name).to_owned())
        };
        // Become fully root (ruid too) so systemctl runs privileged — a shell drops
        // euid when ruid != euid. uid/gid captured above, so we can still drop back.
        if libc::setreuid(0, 0) < 0 {
            eprintln!(
                "agentos-session-launch: setreuid root: {}",
                std::io::Error::last_os_error()
            );
        }

        // Ensure the user runtime dir + session bus exist. greetd's vt="none" session
        // makes pam_systemd fail, so /run/user/<uid> and user@<uid>.service are NOT
        // set up automatically — the desktop then dies creating /run/user/<uid>.
        // Start the user manager here, on auth, as root, then wait for the bus.
        let _ = Command::new(SYSTEMCTL)
            .env("PATH", SAFE_PATH)
            .arg("start")
            .arg(format!("user@{uid}.service"))
            .status()
            .map_err(|e| {
                eprintln!("agentos-session-launch: warning: failed to start user service: {e}")
            });
        let bus = format!("/run/user/{uid}/bus");
        for _ in 0..40 {
            if Path::new(&bus).exists() {
                break;
            }
            std::thread::sleep(Duration::from_millis(50)); // up to ~2s for the user bus
        }

        // Drop privileges COMPLETELY back to the authenticated user.
        if let Some(ref u) = user
            && libc::initgroups(u.as_ptr(), gid) < 0
        {
            eprintln!(
                "agentos-session-launch: initgroups: {}",
                std::io::Error::last_os_error()
            );
            return ExitCode::from(1);
        }

        if libc::setgid(gid) < 0 {
            eprintln!(
                "agentos-session-launch: setgid: {}",
                std::io::Error::last_os_error()
            );
            return ExitCode::from(1);
        }
        if libc::setuid(uid) < 0 {
            eprintln!(
                "agentos-session-launch: setuid: {}",
                std::io::Error::last_os_error()
            );
            return ExitCode::from(1);
        }
        // Belt-and-suspenders: with ruid/euid/suid all == uid we must NOT be able to
        // regain root.
        if libc::setuid(0) == 0 {
            eprintln!("agentos-session-launch: refused to drop root");
            return ExitCode::from(1);
        }

        // Expose the compositor's socket at the standard $XDG_RUNTIME_DIR/wayland-0.
        //
        // WAYLAND_DISPLAY is an absolute path here (the socket lives in the compositor's runtime
        // dir, not the user's). libwayland accepts that, so native clients work -- but flatpak
        // builds $XDG_RUNTIME_DIR/$WAYLAND_DISPLAY itself, which for an absolute value yields a
        // path that cannot exist, so it silently skips binding the socket and every flatpak app
        // loses wayland. Give it the relative name it expects and point it here with a symlink.
        //
        // Runs as the user, in the user's own 0700 runtime dir; logind removes it at logout.
        if let Some(sock) = std::env::var_os("WAYLAND_DISPLAY")
            .map(PathBuf::from)
            .filter(|p| p.is_absolute())
        {
            let link = PathBuf::from(format!("/run/user/{uid}/wayland-0"));
            // Replace via rename so a client never observes a missing link.
            let tmp = PathBuf::from(format!("/run/user/{uid}/.wayland-0.new"));
            let _ = std::fs::remove_file(&tmp);
            match std::os::unix::fs::symlink(&sock, &tmp)
                .and_then(|()| std::fs::rename(&tmp, &link))
            {
                Ok(()) => std::env::set_var("WAYLAND_DISPLAY", "wayland-0"),
                Err(e) => {
                    let _ = std::fs::remove_file(&tmp);
                    eprintln!(
                        "agentos-session-launch: link {}: {e}; leaving WAYLAND_DISPLAY absolute (flatpak apps will have no wayland)",
                        link.display()
                    );
                }
            }
        }

        // Now running as the desktop user. Point the session at the compositor's X auth file and
        // fix DISPLAY propagation: import both into THIS user's systemd manager so `user@uid` X
        // clients get them too — the compositor's own import runs as agentos-display and never
        // reaches this manager, which is why X clients otherwise have no DISPLAY.
        //
        // The cookie itself is never copied here. The compositor grants THIS uid read access to
        // its own auth file once the session takes the screen; previously this code installed the
        // cookie into ~/.Xauthority, which handed X access to anyone who ran this world-executable
        // setuid binary, not just the person logging in.
        if let Some(display) = std::fs::read_to_string(XDISPLAY_HANDOFF)
            .ok()
            .and_then(|s| s.trim().parse::<u32>().ok())
        {
            let disp = format!(":{display}");
            // Single-threaded here, so set_var is sound.
            std::env::set_var("DISPLAY", &disp);
            std::env::set_var("XAUTHORITY", XAUTH_SERVER);
            let _ = Command::new(SYSTEMCTL)
                .env("PATH", SAFE_PATH)
                .args([
                    "--user",
                    "import-environment",
                    "DISPLAY",
                    "XAUTHORITY",
                    "WAYLAND_DISPLAY",
                ])
                .status();
        }
    }

    // Re-activate the user's services. Normally a no-op: logout exits the user manager, so the
    // next login starts it fresh and default.target activates on its own. This covers the case
    // where the manager survived -- logind restarts it for any other session the user has, such
    // as ssh -- in which case nothing else would re-trigger default.target.
    let _ = Command::new(SYSTEMCTL)
        .env("PATH", SAFE_PATH)
        .args(["--user", "--no-block", "start", "default.target"])
        .status();

    // exec the target with the inherited environment (preserves COSMIC_SESSION_ATTACH,
    // WAYLAND_DISPLAY, etc. that the greeter set). Only returns on failure.
    let err = Command::new(&args[1]).args(&args[2..]).exec();
    eprintln!("agentos-session-launch: exec {}: {err}", args[1]);
    ExitCode::from(1)
}
