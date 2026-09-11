//! Where a window's captures go, and what they are called.
//!
//! A capture is saved into the home of the workspace the window belongs to —
//! `~/Workspaces/<id>` on the machine plane, which the sandbox binds onto
//! `$HOME` — so it shows up in that workspace's own files. Outside a workspace
//! it is the user's home. One environment variable per kind moves it.

use std::{
    ffi::OsStr,
    path::{Path, PathBuf},
};

use crate::{shell::element::CosmicSurface, state::State};

/// Same variable and values as kora-workspaces' `DefaultRoot`: `home`/`HOME`
/// roots the default workspace at `$HOME` itself; anything else, unset
/// included, at `~/Workspaces/default`.
const DEFAULT_ROOT_ENV: &str = "KORA_DEFAULT_WORKSPACE_ROOT";
const WORKSPACES_DIR: &str = "Workspaces";
const DEFAULT_WORKSPACE: &str = "default";
/// Longest file stem: the 255-byte name limit less a counter and extension.
const MAX_STEM_BYTES: usize = 247;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CaptureKind {
    Screenshot,
    Recording,
}

impl CaptureKind {
    /// The variable that moves this kind of capture. Relative, it is taken
    /// under the window's home; absolute, every capture lands in that one place.
    pub const fn env(self) -> &'static str {
        match self {
            CaptureKind::Screenshot => "COSMIC_SCREENSHOT_DIR",
            CaptureKind::Recording => "COSMIC_RECORDING_DIR",
        }
    }

    const fn default_dir(self) -> &'static str {
        match self {
            CaptureKind::Screenshot => "Captures/Screenshots",
            CaptureKind::Recording => "Captures/Recordings",
        }
    }
}

pub fn default_root_is_home(value: Option<&str>) -> bool {
    matches!(value, Some("home" | "HOME"))
}

/// The home a window's files live in, seen from the machine plane: its
/// workspace's root, or the user's home outside a workspace.
pub fn workspace_home(home: &Path, workspace: Option<&str>, default_root_is_home: bool) -> PathBuf {
    match workspace {
        None => home.to_path_buf(),
        Some(DEFAULT_WORKSPACE) if default_root_is_home => home.to_path_buf(),
        Some(id) => home.join(WORKSPACES_DIR).join(id),
    }
}

/// Where a capture of `kind` for a window in `workspace` goes, with
/// `configured` being the kind's environment variable.
pub fn directory(
    kind: CaptureKind,
    home: &Path,
    workspace: Option<&str>,
    default_root_is_home: bool,
    configured: Option<&OsStr>,
) -> PathBuf {
    let dir = configured
        .filter(|dir| !dir.is_empty())
        .map_or_else(|| PathBuf::from(kind.default_dir()), PathBuf::from);
    if dir.is_absolute() {
        dir
    } else {
        workspace_home(home, workspace, default_root_is_home).join(dir)
    }
}

/// The directory for a capture of `surface`, from the environment and the
/// workspace registry. `None` without a `HOME`.
pub fn directory_for(state: &State, surface: &CosmicSurface, kind: CaptureKind) -> Option<PathBuf> {
    // A machine-plane window has no workspace of its own; its capture goes
    // where the user is standing, so it is reachable from there.
    let workspace = {
        let shell = state.common.shell.read();
        shell
            .client_workspace(surface)
            .or_else(|| shell.active_workspace().map(ToString::to_string))
    };
    let home = std::env::var_os("HOME")?;
    Some(directory(
        kind,
        Path::new(&home),
        workspace.as_deref(),
        default_root_is_home(std::env::var(DEFAULT_ROOT_ENV).ok().as_deref()),
        std::env::var_os(kind.env()).as_deref(),
    ))
}

/// `<title>_<date>_<time>`, made safe for the filesystem and short enough to
/// leave room for a counter and the extension.
pub fn file_stem(title: &str, time: &jiff::Zoned) -> String {
    let mut stem =
        sanitize_filename::sanitize(format!("{}_{}", title, time.strftime("%Y-%m-%d_%H-%M-%S")));
    let mut end = MAX_STEM_BYTES.min(stem.len());
    while !stem.is_char_boundary(end) {
        end -= 1;
    }
    stem.truncate(end);
    stem
}

#[cfg(test)]
mod tests {
    use super::*;

    fn at(hour: i8, minute: i8, second: i8) -> jiff::Zoned {
        jiff::civil::date(2026, 9, 10)
            .at(hour, minute, second, 0)
            .in_tz("UTC")
            .unwrap()
    }

    #[test]
    fn captures_land_in_the_window_workspace_home() {
        let home = Path::new("/home/u");
        let dir = |kind, workspace, root_is_home, env: Option<&str>| {
            directory(kind, home, workspace, root_is_home, env.map(OsStr::new))
        };
        assert_eq!(
            dir(CaptureKind::Screenshot, None, false, None),
            Path::new("/home/u/Captures/Screenshots")
        );
        assert_eq!(
            dir(CaptureKind::Recording, Some("meridian"), false, None),
            Path::new("/home/u/Workspaces/meridian/Captures/Recordings")
        );
        assert_eq!(
            dir(CaptureKind::Screenshot, Some("default"), false, None),
            Path::new("/home/u/Workspaces/default/Captures/Screenshots")
        );
        assert_eq!(
            dir(CaptureKind::Screenshot, Some("default"), true, None),
            Path::new("/home/u/Captures/Screenshots")
        );
        assert!(default_root_is_home(Some("home")));
        assert!(default_root_is_home(Some("HOME")));
        assert!(!default_root_is_home(Some("workspaces")));
        assert!(!default_root_is_home(None));
    }

    #[test]
    fn the_directory_variable_moves_captures() {
        let home = Path::new("/home/u");
        let dir =
            |kind, workspace, env| directory(kind, home, workspace, false, Some(OsStr::new(env)));
        assert_eq!(
            dir(CaptureKind::Recording, Some("meridian"), "Clips"),
            Path::new("/home/u/Workspaces/meridian/Clips")
        );
        assert_eq!(
            dir(CaptureKind::Screenshot, Some("meridian"), "/srv/shots"),
            Path::new("/srv/shots")
        );
        assert_eq!(
            dir(CaptureKind::Screenshot, None, ""),
            Path::new("/home/u/Captures/Screenshots")
        );
        assert_eq!(CaptureKind::Recording.env(), "COSMIC_RECORDING_DIR");
    }

    #[test]
    fn file_stem_is_title_and_time_without_path_hostile_characters() {
        assert_eq!(
            file_stem("Notes: draft/final", &at(14, 25, 30)),
            "Notes draftfinal_2026-09-10_14-25-30"
        );
    }

    #[test]
    fn file_stem_is_cut_on_a_character_boundary() {
        let stem = file_stem(&"é".repeat(300), &at(0, 0, 0));
        assert!(stem.len() <= MAX_STEM_BYTES);
        assert!(stem.len() > MAX_STEM_BYTES - 2);
        assert!(stem.chars().all(|c| c == 'é'));
    }
}
