//! agentos-session-launch — set up the user manager for a seatless greetd desktop
//! session, then drop privileges and exec the user's desktop.

use std::ffi::{CStr, CString};
use std::os::unix::process::CommandExt;
use std::path::Path;
use std::process::{Command, ExitCode};
use std::time::Duration;

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
        let home: Option<String> = if pw.is_null() || (*pw).pw_dir.is_null() {
            None
        } else {
            CStr::from_ptr((*pw).pw_dir).to_str().ok().map(String::from)
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
        let _ = Command::new("systemctl")
            .arg("start")
            .arg(format!("user@{uid}.service"))
            .status()
            .map_err(|e| {
                eprintln!("agentos-session-launch: warning: failed to start user@{uid}: {e}")
            });
        let bus = format!("/run/user/{uid}/bus");
        for _ in 0..40 {
            if Path::new(&bus).exists() {
                break;
            }
            std::thread::sleep(Duration::from_millis(50)); // up to ~2s for the user bus
        }

        // While root: read the X11 cookie the compositor handed off (0600 agentos-display).
        // Installed into the user's ~/.Xauthority after we drop privileges, below.
        let xcookie = std::fs::read_to_string("/run/cosmic-comp/xcookie").ok();

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

        // Now running as the desktop user. Install X11 cookie auth + fix DISPLAY propagation:
        // write the cookie into the user's ~/.Xauthority (0600), export DISPLAY + XAUTHORITY for
        // the session, and import them into THIS user's systemd manager so `user@uid` X clients
        // get them too — the compositor's own import runs as agentos-display and never reaches
        // this manager, which is why X clients currently have no DISPLAY.
        if let (Some(line), Some(home)) = (xcookie.as_deref(), home.as_deref())
            && let (Some(display), Some(hex)) = (
                line.split_whitespace().next(),
                line.split_whitespace().nth(1),
            )
        {
            let xauthority = format!("{home}/.Xauthority");
            let disp = format!(":{display}");
            let _ = Command::new("xauth")
                .args(["-f", &xauthority, "add", &disp, "MIT-MAGIC-COOKIE-1", hex])
                .status();
            // Single-threaded here, so set_var is sound.
            std::env::set_var("DISPLAY", &disp);
            std::env::set_var("XAUTHORITY", &xauthority);
            let _ = Command::new("systemctl")
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

    // exec the target with the inherited environment (preserves COSMIC_SESSION_ATTACH,
    // WAYLAND_DISPLAY, etc. that the greeter set). Only returns on failure.
    let err = Command::new(&args[1]).args(&args[2..]).exec();
    eprintln!("agentos-session-launch: exec {}: {err}", args[1]);
    ExitCode::from(1)
}
