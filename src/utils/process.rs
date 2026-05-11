use std::path::Path;
use std::sync::LazyLock;

static WORKSPACES_INSTALLED: LazyLock<bool> =
    LazyLock::new(|| is_binary_installed("cosmic-workspaces"));

/// Returns whether cosmic-workspaces is installed on the system.
pub fn workspaces_enabled() -> bool {
    *WORKSPACES_INSTALLED
}

fn is_binary_installed(name: &str) -> bool {
    std::env::var_os("PATH")
        .map(|paths| std::env::split_paths(&paths).any(|dir| dir.join(name).is_file()))
        .unwrap_or(false)
        || Path::new(&format!("/usr/bin/{name}")).is_file()
}
