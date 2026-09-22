//! App identity and the optional New Window action from its desktop entry.

use std::{collections::HashSet, path::Path};

use freedesktop_desktop_entry::DesktopEntry;

/// Match the AppIdMatch extension used by the shell's desktop overrides.
pub(crate) fn glob_match(pattern: &str, text: &str) -> bool {
    let (p, t) = (pattern.as_bytes(), text.as_bytes());
    let (mut px, mut tx) = (0usize, 0usize);
    let (mut star_px, mut star_tx) = (usize::MAX, 0usize);
    while tx < t.len() {
        if px < p.len() && (p[px] == b'?' || p[px] == t[tx]) {
            px += 1;
            tx += 1;
        } else if px < p.len() && p[px] == b'*' {
            star_px = px;
            star_tx = tx;
            px += 1;
        } else if star_px != usize::MAX {
            px = star_px + 1;
            star_tx += 1;
            tx = star_tx;
        } else {
            return false;
        }
    }
    while px < p.len() && p[px] == b'*' {
        px += 1;
    }
    px == p.len()
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NewWindowAction {
    argv: Vec<String>,
}

/// How many of an entry's own actions the window surfaces.
///
/// The header's menu is not a menu bar: the four rows it already owns plus this
/// many is still one glance, and at fifteen it is a File/Edit/View bar wearing a
/// dropdown. An entry declaring more keeps the first few, in its own order.
pub const APP_ACTION_CAP: usize = 6;

/// One `[Desktop Action …]` group — the freedesktop way an application declares
/// a verb the desktop may offer outside the app itself.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DesktopAction {
    /// The group id, as `Actions=` names it. Stable across restarts, so a
    /// header pin may reference it.
    pub id: String,
    /// The action's localised `Name`.
    pub name: String,
    argv: Vec<String>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DesktopApp {
    pub name: Option<String>,
    pub new_window: Option<NewWindowAction>,
    /// Every action the entry declares, in declaration order, capped at
    /// [`APP_ACTION_CAP`]. Includes the one `new_window` was derived from: the
    /// `+` is a shortcut to it, not a reason to hide the row.
    pub actions: Vec<DesktopAction>,
}

impl DesktopApp {
    pub fn from_content(path: &Path, content: &str) -> Option<Self> {
        let entry = DesktopEntry::from_str(path, content, None::<&[&str]>).ok()?;
        Self::from_entry(&entry)
    }

    fn from_entry(entry: &DesktopEntry) -> Option<Self> {
        if entry.hidden() || entry.desktop_entry("Type") != Some("Application") {
            return None;
        }
        let locales = freedesktop_desktop_entry::get_languages_from_env();
        Some(Self {
            name: entry
                .name(&locales)
                .filter(|name| !name.is_empty())
                .map(|name| name.to_string()),
            new_window: NewWindowAction::from_entry(entry),
            actions: DesktopAction::all(entry, &locales),
        })
    }
}

impl DesktopAction {
    fn all(entry: &DesktopEntry, locales: &[String]) -> Vec<Self> {
        let Some(actions) = entry.actions() else {
            return Vec::new();
        };
        actions
            .into_iter()
            .filter_map(|action| {
                // An action with no runnable Exec is not offered at all: a row
                // that cannot fire is worse than an absent one, because the
                // entry is claiming a capability the desktop cannot deliver.
                let argv = action_argv(entry, action)?;
                let name = entry
                    .action_name(action, locales)
                    .map(|name| name.to_string())
                    .filter(|name| !name.is_empty())?;
                Some(Self {
                    id: action.to_owned(),
                    name,
                    argv,
                })
            })
            .take(APP_ACTION_CAP)
            .collect()
    }

    pub fn launch(&self) {
        launch_argv(&self.argv, "desktop entry action");
    }
}

/// An action's command line, with the entry's working directory and terminal
/// wrapper applied — the same expansion a launcher would do.
fn action_argv(entry: &DesktopEntry, action: &str) -> Option<Vec<String>> {
    let mut argv = expand_exec(entry.action_exec(action)?, entry)?;
    if let Some(path) = entry.path().filter(|p| !p.is_empty()) {
        argv.splice(
            0..0,
            ["env".into(), "--chdir".into(), path.into(), "--".into()],
        );
    }
    if entry.terminal() {
        argv.splice(0..0, ["xdg-terminal-exec".into(), "--".into()]);
    }
    Some(argv)
}

fn launch_argv(argv: &[String], what: &str) {
    if let Some((program, args)) = argv.split_first()
        && let Err(error) = super::process::spawn_app_in_workspace(program, args)
    {
        tracing::warn!(?error, "Failed to launch {what}");
    }
}

impl NewWindowAction {
    #[cfg(test)]
    fn from_content(path: &Path, content: &str) -> Option<Self> {
        DesktopApp::from_content(path, content)?.new_window
    }

    fn from_entry(entry: &DesktopEntry) -> Option<Self> {
        if entry.hidden() || entry.desktop_entry("Type") != Some("Application") {
            return None;
        }
        for action in entry.actions()? {
            let id = action.to_ascii_lowercase().replace('_', "-");
            let name = entry.action_entry(action, "Name").unwrap_or_default();
            if !matches!(id.as_str(), "new-window" | "window-new" | "newwindow")
                && !name.eq_ignore_ascii_case("New Window")
            {
                continue;
            }
            return Some(Self {
                argv: action_argv(entry, action)?,
            });
        }
        None
    }

    pub fn launch(&self) {
        launch_argv(&self.argv, "desktop New Window action");
    }
}

pub fn for_app(app_id: &str) -> Option<DesktopApp> {
    if app_id.is_empty() || app_id.contains('/') {
        return None;
    }
    let id = app_id.trim_end_matches(".desktop");
    let dirs = super::xdg_dirs::data_dirs("applications");
    // A user entry also masks the system entry when it has no New Window action.
    for dir in &dirs {
        let path = dir.join(format!("{id}.desktop"));
        if let Ok(content) = std::fs::read_to_string(&path) {
            return DesktopApp::from_content(&path, &content);
        }
    }
    let mut seen = HashSet::new();
    for path in freedesktop_desktop_entry::Iter::new(dirs.into_iter()) {
        let Ok(content) = std::fs::read_to_string(&path) else {
            continue;
        };
        let Ok(entry) = DesktopEntry::from_str(&path, &content, None::<&[&str]>) else {
            continue;
        };
        if !seen.insert(entry.appid.clone()) {
            continue;
        }
        if matches_app(&entry, id) {
            return DesktopApp::from_entry(&entry);
        }
    }
    None
}

fn matches_app(entry: &DesktopEntry, id: &str) -> bool {
    entry.appid.eq_ignore_ascii_case(id)
        || entry
            .startup_wm_class()
            .is_some_and(|class| class.eq_ignore_ascii_case(id))
        || ["X-Playtron-AppIdMatch", "X-Cosmic-AppIdMatch"]
            .into_iter()
            .filter_map(|key| entry.desktop_entry(key))
            .any(|pattern| glob_match(pattern, id))
}

fn expand_exec(exec: &str, entry: &DesktopEntry) -> Option<Vec<String>> {
    let mut argv = Vec::new();
    for word in shlex::split(exec)? {
        match word.as_str() {
            "%f" | "%F" | "%u" | "%U" => {}
            "%i" => {
                if let Some(icon) = entry.icon() {
                    argv.extend(["--icon".into(), icon.into()]);
                }
            }
            "%c" => argv.push(
                entry
                    .name(&freedesktop_desktop_entry::get_languages_from_env())?
                    .into(),
            ),
            "%k" => argv.push(entry.path.to_string_lossy().into_owned()),
            _ => {
                let mut expanded = String::new();
                let mut chars = word.chars();
                while let Some(c) = chars.next() {
                    if c == '%' && chars.next()? != '%' {
                        return None;
                    }
                    expanded.push(c);
                }
                argv.push(expanded);
            }
        }
    }
    (!argv.is_empty() && !argv[0].is_empty() && argv.iter().all(|arg| !arg.contains('\0')))
        .then_some(argv)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn action(extra: &str, exec: &str) -> Option<NewWindowAction> {
        NewWindowAction::from_content(
            Path::new("/apps/example.desktop"),
            &format!(
                "[Desktop Entry]\nType=Application\nName=Example App\nIcon=example\nActions=new-window;\n{extra}\n[Desktop Action new-window]\nName=New Window\nExec={exec}\n"
            ),
        )
    }

    #[test]
    fn quoted_arguments_and_field_codes_are_not_shell_commands() {
        let action = action(
            "",
            "example --new-window \"a b\" %U %i %c %k %% \"$(touch nope)\"",
        )
        .unwrap();
        assert_eq!(
            action.argv,
            [
                "example",
                "--new-window",
                "a b",
                "--icon",
                "example",
                "Example App",
                "/apps/example.desktop",
                "%",
                "$(touch nope)"
            ]
        );
    }

    #[test]
    fn hidden_missing_and_malformed_actions_are_not_offered() {
        assert!(action("Hidden=true", "example").is_none());
        assert!(action("", "example \"bad").is_none());
        assert!(action("", "example %x").is_none());
        assert!(action("", "").is_none());
        let undeclared = "[Desktop Entry]\nType=Application\nName=Example\n[Desktop Action new-window]\nExec=example --new-window\n";
        assert!(NewWindowAction::from_content(Path::new("example.desktop"), undeclared).is_none());
    }

    #[test]
    fn working_directory_and_terminal_are_preserved() {
        let action = action("Path=/tmp\nTerminal=true", "example --new-window").unwrap();
        assert_eq!(
            action.argv,
            [
                "xdg-terminal-exec",
                "--",
                "env",
                "--chdir",
                "/tmp",
                "--",
                "example",
                "--new-window"
            ]
        );
    }

    /// The entry is the app's own declaration of what it can do; the header
    /// offers those verbs rather than inventing a parallel set.
    #[test]
    fn every_declared_action_is_offered_in_order_and_capped() {
        let mut groups = String::new();
        let mut ids = String::new();
        for index in 0..8 {
            ids.push_str(&format!("act{index};"));
            groups.push_str(&format!(
                "[Desktop Action act{index}]\nName=Action {index}\nExec=example --do {index}\n"
            ));
        }
        let app = DesktopApp::from_content(
            Path::new("/apps/example.desktop"),
            &format!("[Desktop Entry]\nType=Application\nName=Example\nActions={ids}\n{groups}"),
        )
        .unwrap();
        assert_eq!(app.actions.len(), APP_ACTION_CAP);
        assert_eq!(app.actions[0].id, "act0");
        assert_eq!(app.actions[0].name, "Action 0");
        assert_eq!(app.actions[0].argv, ["example", "--do", "0"]);
        assert_eq!(app.actions[APP_ACTION_CAP - 1].id, "act5");
    }

    /// An action the desktop cannot run is not a row: the entry is claiming a
    /// capability nothing can deliver.
    #[test]
    fn an_action_with_no_name_or_no_runnable_exec_is_dropped() {
        let app = DesktopApp::from_content(
            Path::new("/apps/example.desktop"),
            "[Desktop Entry]\nType=Application\nName=Example\nActions=good;noexec;noname;bad;\n\
             [Desktop Action good]\nName=Good\nExec=example --good\n\
             [Desktop Action noexec]\nName=No exec\n\
             [Desktop Action noname]\nExec=example --noname\n\
             [Desktop Action bad]\nName=Bad\nExec=example \"unterminated\n",
        )
        .unwrap();
        assert_eq!(app.actions.len(), 1);
        assert_eq!(app.actions[0].id, "good");
    }

    /// The `+` is a shortcut to an action that is still its own row.
    #[test]
    fn the_new_window_action_is_also_an_ordinary_action() {
        let app = DesktopApp::from_content(
            Path::new("/apps/example.desktop"),
            "[Desktop Entry]\nType=Application\nName=Example\nActions=new-window;\n\
             [Desktop Action new-window]\nName=New Window\nExec=example --new-window\n",
        )
        .unwrap();
        assert!(app.new_window.is_some());
        assert_eq!(app.actions.len(), 1);
        assert_eq!(app.actions[0].id, "new-window");
    }

    /// Sanity check against a real installed entry: the compositor must read
    /// what applications actually ship, not only what the tests invent.
    #[test]
    fn a_real_installed_entry_yields_its_actions() {
        let Some(path) = std::fs::read_dir("/usr/share/applications")
            .ok()
            .into_iter()
            .flatten()
            .flatten()
            .map(|entry| entry.path())
            .filter(|path| path.extension().is_some_and(|ext| ext == "desktop"))
            .find(|path| {
                std::fs::read_to_string(path)
                    .is_ok_and(|content| content.lines().any(|line| line.starts_with("Actions=")))
            })
        else {
            // No entry on this machine declares actions; nothing to check.
            return;
        };
        let content = std::fs::read_to_string(&path).unwrap();
        let app = DesktopApp::from_content(&path, &content).expect("a real application entry");
        assert!(
            !app.actions.is_empty(),
            "{path:?} declares actions but none were read"
        );
        for action in &app.actions {
            assert!(!action.id.is_empty());
            assert!(!action.name.is_empty());
        }
    }

    #[test]
    fn app_name_does_not_require_a_new_window_action() {
        let app = DesktopApp::from_content(
            Path::new("org.example.Files.desktop"),
            "[Desktop Entry]\nType=Application\nName=Files\nExec=files\n",
        )
        .unwrap();
        assert_eq!(app.name.as_deref(), Some("Files"));
        assert!(app.new_window.is_none());
    }
}
