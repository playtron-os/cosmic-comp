//! The screen capture shortcuts: each runs the capture program with the matching command.

use std::{collections::BTreeMap, ffi::OsStr, os::unix::fs::PermissionsExt, path::Path};

use cosmic_settings_config::shortcuts::{self, action::System};

const DEFAULT_PROGRAM: &str = "kora-capture";
/// Names another capture tool, or a full path, to run in place of Kora Capture.
const PROGRAM_VAR: &str = "KORA_CAPTURE_PROGRAM";

/// The capture program's arguments for a capture shortcut, `None` for any other action.
pub fn arguments(action: &shortcuts::Action) -> Option<&'static str> {
    use shortcuts::Action;
    Some(match action {
        Action::CaptureScreen => "screen",
        Action::CaptureScreenClipboard => "screen --clipboard",
        Action::CaptureSelection => "selection",
        Action::CaptureSelectionClipboard => "selection --clipboard",
        Action::CaptureToolbar => "toolbar",
        Action::StopRecording => "stop",
        _ => return None,
    })
}

/// The command a capture shortcut spawns. Without an installed capture program the
/// captures fall back to the system screenshot tool, so Print keeps working.
pub fn command(
    action: &shortcuts::Action,
    program: Option<&str>,
    system_actions: &BTreeMap<System, String>,
) -> Option<String> {
    let arguments = arguments(action)?;
    if let Some(program) = program {
        return Some(format!("{program} {arguments}"));
    }
    match action {
        shortcuts::Action::StopRecording => None,
        _ => system_actions.get(&System::Screenshot).cloned(),
    }
}

/// The capture program, if installed. Read on each press, so installing it or setting
/// `KORA_CAPTURE_PROGRAM` needs no compositor restart.
pub fn installed_program() -> Option<String> {
    let program = std::env::var(PROGRAM_VAR)
        .ok()
        .filter(|program| !program.trim().is_empty())
        .unwrap_or_else(|| DEFAULT_PROGRAM.to_owned());
    let path = std::env::var_os("PATH").unwrap_or_default();
    is_installed(&program, &path).then_some(program)
}

fn is_installed(program: &str, path: &OsStr) -> bool {
    if program.contains('/') {
        return executable(Path::new(program));
    }
    std::env::split_paths(path).any(|dir| executable(&dir.join(program)))
}

fn executable(path: &Path) -> bool {
    std::fs::metadata(path)
        .is_ok_and(|meta| meta.is_file() && meta.permissions().mode() & 0o111 != 0)
}

#[cfg(test)]
mod tests {
    use super::*;
    use shortcuts::Action;

    const CAPTURES: [(Action, &str); 6] = [
        (Action::CaptureScreen, "kora-capture screen"),
        (
            Action::CaptureScreenClipboard,
            "kora-capture screen --clipboard",
        ),
        (Action::CaptureSelection, "kora-capture selection"),
        (
            Action::CaptureSelectionClipboard,
            "kora-capture selection --clipboard",
        ),
        (Action::CaptureToolbar, "kora-capture toolbar"),
        (Action::StopRecording, "kora-capture stop"),
    ];

    fn screenshot_tool() -> BTreeMap<System, String> {
        BTreeMap::from([(System::Screenshot, "cosmic-screenshot".to_string())])
    }

    #[test]
    fn each_capture_shortcut_runs_its_kora_capture_command() {
        for (action, want) in CAPTURES {
            let got = command(&action, Some(DEFAULT_PROGRAM), &screenshot_tool());
            assert_eq!(got.as_deref(), Some(want), "{action:?}");
        }
    }

    #[test]
    fn other_actions_are_not_captures() {
        assert_eq!(arguments(&Action::Close), None);
        assert_eq!(
            command(
                &Action::System(System::Screenshot),
                Some(DEFAULT_PROGRAM),
                &screenshot_tool()
            ),
            None
        );
    }

    #[test]
    fn without_kora_capture_the_screenshot_tool_takes_the_captures() {
        for (action, _) in CAPTURES {
            let got = command(&action, None, &screenshot_tool());
            let want = match action {
                Action::StopRecording => None,
                _ => Some("cosmic-screenshot"),
            };
            assert_eq!(got.as_deref(), want, "{action:?}");
        }
        assert_eq!(
            command(&Action::CaptureToolbar, None, &BTreeMap::new()),
            None
        );
    }

    #[test]
    fn another_capture_program_gets_the_same_commands() {
        let got = command(
            &Action::CaptureSelectionClipboard,
            Some("/opt/shot/bin/shot"),
            &screenshot_tool(),
        );
        assert_eq!(
            got.as_deref(),
            Some("/opt/shot/bin/shot selection --clipboard")
        );
    }

    #[test]
    fn only_an_executable_file_on_the_path_counts_as_installed() {
        let root = std::env::temp_dir().join(format!("capture-path-{}", std::process::id()));
        let (bin, data) = (root.join("bin"), root.join("data"));
        std::fs::create_dir_all(&bin).unwrap();
        std::fs::create_dir_all(data.join(DEFAULT_PROGRAM)).unwrap();
        let path = std::env::join_paths([&data, &bin]).unwrap();

        // A directory of that name, then a plain file, are not the program.
        assert!(!is_installed(DEFAULT_PROGRAM, &path));
        let program = bin.join(DEFAULT_PROGRAM);
        std::fs::write(&program, "#!/bin/sh\n").unwrap();
        assert!(!is_installed(DEFAULT_PROGRAM, &path));

        std::fs::set_permissions(&program, std::fs::Permissions::from_mode(0o755)).unwrap();
        assert!(is_installed(DEFAULT_PROGRAM, &path));
        // A full path is checked as given, whatever PATH holds.
        assert!(is_installed(program.to_str().unwrap(), OsStr::new("")));
        assert!(!is_installed("/nonexistent/shot", &path));

        std::fs::remove_dir_all(&root).unwrap();
    }
}
