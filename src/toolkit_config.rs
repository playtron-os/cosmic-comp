//! Toolkit configuration — replaces `cosmic::config::CosmicTk` and related globals.
//!
//! Provides compositor-wide toolkit settings (icon theme, show_minimize, etc.)
//! without depending on the libcosmic crate.

use std::sync::{LazyLock, RwLock};

/// Global toolkit configuration (replaces `cosmic::config::COSMIC_TK`).
pub static TOOLKIT_CONFIG: LazyLock<RwLock<ToolkitConfig>> =
    LazyLock::new(|| RwLock::new(ToolkitConfig::default()));

/// Get the current icon theme name.
pub fn icon_theme_default() -> String {
    icetron_themes::icon_theme::get_icon_theme()
}

/// Set the current icon theme name.
pub fn icon_theme_set_default(name: String) {
    icetron_themes::icon_theme::set_icon_theme(name);
}

/// Toolkit-wide configuration (replaces `cosmic::config::CosmicTk`).
#[derive(Debug, Clone, PartialEq)]
pub struct ToolkitConfig {
    pub icon_theme: String,
    pub show_minimize: bool,
    pub show_maximize: bool,
    pub apply_theme_global: bool,
}

impl Default for ToolkitConfig {
    fn default() -> Self {
        Self {
            icon_theme: "Adwaita".to_string(),
            show_minimize: true,
            show_maximize: true,
            apply_theme_global: true,
        }
    }
}
