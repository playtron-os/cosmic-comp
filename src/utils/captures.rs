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
        "Captures"
    }

    /// What a file of this kind is called before its time.
    const fn noun(self) -> &'static str {
        match self {
            CaptureKind::Screenshot => "Screenshot",
            CaptureKind::Recording => "Screen Recording",
        }
    }

    pub const fn preview_kind(self) -> &'static str {
        match self {
            CaptureKind::Screenshot => "image",
            CaptureKind::Recording => "video",
        }
    }
}

/// Where a capture waits while its preview is up, under the window's home.
/// Accepted drops leave it here for the app that took it.
const PENDING_DIR: &str = ".local/share/kora/captures";

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

/// The workspace a capture of `surface` belongs to. A machine-plane window
/// has none of its own; its capture goes where the user is standing.
fn workspace_for(state: &State, surface: &CosmicSurface) -> Option<String> {
    let shell = state.common.shell.read();
    shell
        .client_workspace(surface)
        .or_else(|| shell.active_workspace().map(ToString::to_string))
}

/// The directory for a capture of `surface`, from the environment and the
/// workspace registry. `None` without a `HOME`.
pub fn directory_for(state: &State, surface: &CosmicSurface, kind: CaptureKind) -> Option<PathBuf> {
    let workspace = workspace_for(state, surface);
    let home = std::env::var_os("HOME")?;
    Some(directory(
        kind,
        Path::new(&home),
        workspace.as_deref(),
        default_root_is_home(std::env::var(DEFAULT_ROOT_ENV).ok().as_deref()),
        std::env::var_os(kind.env()).as_deref(),
    ))
}

/// Everything a capture's preview needs to know besides the file itself.
#[derive(Debug, Clone, PartialEq)]
pub struct Destination {
    pub kind: CaptureKind,
    /// Where the file waits while previewed.
    pub pending: PathBuf,
    /// Where it is filed when the preview runs out.
    pub save_to: PathBuf,
    /// The window's home on the machine, and the home apps there see it as.
    pub home: PathBuf,
    pub seen_as: PathBuf,
    pub workspace: Option<String>,
}

impl Destination {
    pub fn new(
        kind: CaptureKind,
        home: &Path,
        workspace: Option<&str>,
        default_root_is_home: bool,
        configured: Option<&OsStr>,
    ) -> Self {
        let window_home = workspace_home(home, workspace, default_root_is_home);
        Self {
            kind,
            pending: window_home.join(PENDING_DIR),
            save_to: directory(kind, home, workspace, default_root_is_home, configured),
            seen_as: home.to_path_buf(),
            home: window_home,
            workspace: workspace.map(str::to_string),
        }
    }

    pub fn for_surface(state: &State, surface: &CosmicSurface, kind: CaptureKind) -> Option<Self> {
        let workspace = workspace_for(state, surface);
        let home = std::env::var_os("HOME")?;
        Some(Self::new(
            kind,
            Path::new(&home),
            workspace.as_deref(),
            default_root_is_home(std::env::var(DEFAULT_ROOT_ENV).ok().as_deref()),
            std::env::var_os(kind.env()).as_deref(),
        ))
    }

    /// `path` as apps in the window's workspace name it: the sandbox mounts
    /// the workspace's home as the user's.
    pub fn seen_inside(&self, path: &Path) -> PathBuf {
        match path.strip_prefix(&self.home) {
            Ok(rest) => self.seen_as.join(rest),
            Err(_) => path.to_path_buf(),
        }
    }

    /// What the preview's `ShowCapture` is told about the file at `path`.
    pub fn handoff(
        &self,
        path: &Path,
        app: &str,
        poster: Option<&Path>,
        duration_ms: Option<u64>,
    ) -> Handoff {
        let text = |p: &Path| p.to_string_lossy().into_owned();
        let mut entries = vec![
            ("path", text(path)),
            ("uri", file_uri(&self.seen_inside(path))),
            ("kind", self.kind.preview_kind().to_string()),
            ("app", app.to_string()),
            ("save-to", text(&self.save_to)),
            ("downloads", text(&self.home.join("Downloads"))),
            ("workspace", self.workspace.clone().unwrap_or_default()),
        ];
        if let Some(poster) = poster {
            entries.push(("poster", text(poster)));
        }
        Handoff {
            path: path.to_path_buf(),
            save_to: self.save_to.clone(),
            entries,
            duration_ms,
        }
    }
}

/// A capture on its way to the preview: the `ShowCapture` dictionary, and
/// where the file goes if nobody previews it.
#[derive(Debug, Clone, PartialEq)]
pub struct Handoff {
    pub path: PathBuf,
    pub save_to: PathBuf,
    pub entries: Vec<(&'static str, String)>,
    pub duration_ms: Option<u64>,
}

impl Handoff {
    pub fn get(&self, key: &str) -> Option<&str> {
        self.entries
            .iter()
            .find_map(|(k, v)| (*k == key).then_some(v.as_str()))
    }
}

/// `file://` and the path, percent-encoded where a URI needs it.
pub fn file_uri(path: &Path) -> String {
    use std::fmt::Write as _;
    let mut out = String::from("file://");
    for byte in path.as_os_str().as_encoded_bytes() {
        match byte {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'/' | b'-' | b'_' | b'.' | b'~' => {
                out.push(char::from(*byte));
            }
            _ => {
                let _ = write!(out, "%{byte:02X}");
            }
        }
    }
    out
}

/// Move `path` into `dir` under a name not taken there; the new path.
pub fn file_into(path: &Path, dir: &Path) -> std::io::Result<PathBuf> {
    std::fs::create_dir_all(dir)?;
    let name = path
        .file_name()
        .ok_or_else(|| std::io::Error::new(std::io::ErrorKind::InvalidInput, "no file name"))?
        .to_string_lossy()
        .into_owned();
    let (stem, ext) = match name.rsplit_once('.') {
        Some((stem, ext)) if !stem.is_empty() => (stem.to_string(), format!(".{ext}")),
        _ => (name.clone(), String::new()),
    };
    let to = std::iter::once(dir.join(&name))
        .chain((2..100).map(|n| dir.join(format!("{stem} ({n}){ext}"))))
        .find(|p| !p.exists())
        .ok_or_else(|| std::io::Error::new(std::io::ErrorKind::AlreadyExists, "no free name"))?;
    if std::fs::rename(path, &to).is_err() {
        std::fs::copy(path, &to)?;
        std::fs::remove_file(path)?;
    }
    Ok(to)
}

/// `Screenshot 2026-09-10 at 14.25.30`, as a capture is named.
pub fn capture_stem(kind: CaptureKind, time: &jiff::Zoned) -> String {
    format!("{} {}", kind.noun(), time.strftime("%Y-%m-%d at %H.%M.%S"))
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
            Path::new("/home/u/Captures")
        );
        assert_eq!(
            dir(CaptureKind::Recording, Some("meridian"), false, None),
            Path::new("/home/u/Workspaces/meridian/Captures")
        );
        assert_eq!(
            dir(CaptureKind::Screenshot, Some("default"), false, None),
            Path::new("/home/u/Workspaces/default/Captures")
        );
        assert_eq!(
            dir(CaptureKind::Screenshot, Some("default"), true, None),
            Path::new("/home/u/Captures")
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
            Path::new("/home/u/Captures")
        );
        assert_eq!(CaptureKind::Recording.env(), "COSMIC_RECORDING_DIR");
    }

    #[test]
    fn a_capture_waits_in_its_workspace_and_is_named_there_as_its_apps_see_it() {
        let home = Path::new("/home/u");
        let dest = Destination::new(CaptureKind::Recording, home, Some("work"), false, None);
        assert_eq!(
            dest.pending,
            Path::new("/home/u/Workspaces/work/.local/share/kora/captures")
        );
        assert_eq!(dest.save_to, Path::new("/home/u/Workspaces/work/Captures"));
        let file = dest.pending.join("Screen Recording 1.mp4");
        assert_eq!(
            dest.seen_inside(&file),
            Path::new("/home/u/.local/share/kora/captures/Screen Recording 1.mp4")
        );
        let handoff = dest.handoff(&file, "Slate", None, Some(5400));
        assert_eq!(
            handoff.get("uri"),
            Some("file:///home/u/.local/share/kora/captures/Screen%20Recording%201.mp4")
        );
        assert_eq!(
            handoff.get("downloads"),
            Some("/home/u/Workspaces/work/Downloads")
        );
        assert_eq!(handoff.get("workspace"), Some("work"));
        assert_eq!(handoff.get("kind"), Some("video"));
        assert_eq!(handoff.duration_ms, Some(5400));

        let outside = Destination::new(CaptureKind::Screenshot, home, None, false, None);
        assert_eq!(
            outside.pending,
            Path::new("/home/u/.local/share/kora/captures")
        );
        assert_eq!(outside.seen_inside(&outside.pending), outside.pending);
    }

    #[test]
    fn filing_never_replaces_an_earlier_capture() {
        let dir =
            std::env::temp_dir().join(format!("cosmic-comp-file-into-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(dir.join("Captures")).unwrap();
        std::fs::write(dir.join("Captures/Shot.png"), b"old").unwrap();
        std::fs::write(dir.join("Shot.png"), b"new").unwrap();
        let to = file_into(&dir.join("Shot.png"), &dir.join("Captures")).unwrap();
        assert_eq!(to, dir.join("Captures/Shot (2).png"));
        assert_eq!(
            std::fs::read(dir.join("Captures/Shot.png")).unwrap(),
            b"old"
        );
        assert!(!dir.join("Shot.png").exists());
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn captures_are_named_by_kind_and_time() {
        assert_eq!(
            capture_stem(CaptureKind::Screenshot, &at(14, 25, 30)),
            "Screenshot 2026-09-10 at 14.25.30"
        );
        assert_eq!(
            capture_stem(CaptureKind::Recording, &at(9, 5, 0)),
            "Screen Recording 2026-09-10 at 09.05.00"
        );
    }
}
