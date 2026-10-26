// SPDX-License-Identifier: GPL-3.0-only

//! The app a Wayland client was launched as, from the systemd unit it runs in.
//!
//! Launchers start an app in `app-[<launcher>-]<id>-<random>.scope` or
//! `app-[<launcher>-]<id>[@<random>].service`, systemd's desktop convention,
//! which Flatpak and kora-launch follow. A window that sets no app id of its
//! own, such as Chrome's first-run dialog, is reported under that id, so a shell
//! still shows it as its app.

/// The app a pid was launched as, or `None` outside an app's unit.
pub fn of_pid(pid: u32) -> Option<String> {
    let cgroup = std::fs::read_to_string(format!("/proc/{pid}/cgroup")).ok()?;
    of_cgroup(&cgroup)
}

/// The app id in a `/proc/<pid>/cgroup` file's contents.
pub fn of_cgroup(cgroup: &str) -> Option<String> {
    let path = cgroup.lines().find_map(|line| line.strip_prefix("0::"))?;
    path.rsplit('/').find_map(of_unit)
}

fn of_unit(unit: &str) -> Option<String> {
    let (name, scope) = match unit.strip_suffix(".scope") {
        Some(name) => (name, true),
        None => (unit.strip_suffix(".service")?, false),
    };
    let name = name.strip_prefix("app-")?;
    let mut parts: Vec<&str> = if scope {
        name.split('-').collect()
    } else {
        name.split('@').next()?.split('-').collect()
    };
    if scope {
        // A scope always ends in its random part.
        parts.pop();
    }
    let id = match parts.as_slice() {
        [id] | [_, id] => unescape(id)?,
        _ => return None,
    };
    (!id.is_empty()).then_some(id)
}

/// Undo `systemd-escape`: `\x2d` back to `-`.
fn unescape(part: &str) -> Option<String> {
    let bytes = part.as_bytes();
    let mut out = Vec::with_capacity(bytes.len());
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'\\' && bytes.get(i + 1) == Some(&b'x') {
            out.push(u8::from_str_radix(part.get(i + 2..i + 4)?, 16).ok()?);
            i += 4;
        } else {
            out.push(bytes[i]);
            i += 1;
        }
    }
    String::from_utf8(out).ok()
}

#[cfg(test)]
mod tests {
    use super::of_cgroup;

    fn cgroup(unit: &str) -> String {
        format!(
            "0::/user.slice/user-1000.slice/user@1000.service/workspace.slice/workspace-work.slice/{unit}\n"
        )
    }

    #[test]
    fn an_app_scope_names_its_app() {
        let launched = |unit: &str| of_cgroup(&cgroup(unit));
        assert_eq!(
            launched("app-kora-google\\x2dchrome-1f2e.scope").as_deref(),
            Some("google-chrome")
        );
        assert_eq!(
            launched("app-kora-org.telegram.desktop-9a.scope").as_deref(),
            Some("org.telegram.desktop")
        );
        assert_eq!(
            launched("app-flatpak-org.gnome.Calculator-1234.scope").as_deref(),
            Some("org.gnome.Calculator")
        );
        assert_eq!(launched("app-firefox-77.scope").as_deref(), Some("firefox"));
        assert_eq!(
            launched("app-gnome-slack@3a.service").as_deref(),
            Some("slack")
        );
        assert_eq!(
            launched("app-slack@autostart.service").as_deref(),
            Some("slack")
        );
    }

    #[test]
    fn other_units_name_no_app() {
        let launched = |unit: &str| of_cgroup(&cgroup(unit));
        assert_eq!(launched("run-p10360-i53821.scope"), None);
        assert_eq!(launched("agentos-panel.service"), None);
        assert_eq!(launched("app-graphical.slice"), None);
        // An unescaped dash leaves the parts ambiguous.
        assert_eq!(launched("app-kora-google-chrome-1f2e.scope"), None);
        assert_eq!(launched("app-1f2e.scope"), None);
        assert_eq!(of_cgroup("0::/\n"), None);
    }
}
