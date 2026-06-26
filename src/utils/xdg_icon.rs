//! XDG icon resolution — delegates to `icetron_utils::icons::lookup_icon`.
//!
//! Provides convenience wrappers that produce iced `Svg` widgets from
//! freedesktop icon names. The underlying `lookup_icon` caches resolved paths
//! so repeated lookups are sub-microsecond.

use std::collections::HashMap;
use std::sync::{LazyLock, Mutex};

use iced_core::{Color, Length, svg};
use iced_widget::Svg;

/// Cache of SVG handles built from resolved paths (avoids re-reading files each frame).
static ICON_HANDLE_CACHE: LazyLock<Mutex<HashMap<String, Option<svg::Handle>>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

/// Resolve a symbolic icon name to a filesystem path (cached inside `lookup_icon`).
pub fn resolve_icon_path(name: &str, size: f32) -> Option<std::path::PathBuf> {
    icetron_p::utils::icons::lookup_icon(name, size)
}

/// Create an SVG handle for a named icon (cached).
pub fn icon_handle(name: &str) -> Option<svg::Handle> {
    let mut cache = ICON_HANDLE_CACHE.lock().unwrap();
    if let Some(cached) = cache.get(name) {
        return cached.clone();
    }
    let handle = icetron_p::utils::icons::lookup_icon(name, 48.0).map(svg::Handle::from_path);
    cache.insert(name.to_string(), handle.clone());
    handle
}

/// Create an Svg widget for a named icon at the given size.
/// If the icon is not found, returns a placeholder empty widget.
pub fn named_icon<'a>(name: &str, size: impl Into<Length>) -> Svg<'a, iced_core::Theme> {
    let size = size.into();
    match icon_handle(name) {
        Some(handle) => Svg::new(handle).width(size).height(size),
        None => {
            let empty = svg::Handle::from_memory(
                b"<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 1 1\"/>" as &[u8],
            );
            Svg::new(empty).width(size).height(size)
        }
    }
}

/// Create an Svg widget for a named icon at the given size, tinted with a color.
pub fn named_icon_colored<'a>(
    name: &str,
    size: impl Into<Length>,
    _color: Color,
) -> Svg<'a, iced_core::Theme> {
    named_icon(name, size)
}
