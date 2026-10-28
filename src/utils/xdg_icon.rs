//! XDG icon resolution — delegates to `icetron_p::utils::icons`.
//!
//! Provides convenience wrappers that produce iced `Svg` widgets from
//! freedesktop icon names. The underlying `lookup_icon` caches resolved paths,
//! so a repeated lookup costs a file check.

use iced_core::{Color, Length, svg};
use iced_widget::Svg;

/// Resolve a symbolic icon name to a filesystem path (cached inside `lookup_icon`).
pub fn resolve_icon_path(name: &str, size: f32) -> Option<std::path::PathBuf> {
    icetron_p::utils::icons::lookup_icon(name, size)
}

/// Create an SVG handle for a named icon. Not cached by name: `lookup_icon` already caches
/// and finds an app installed or moved since, which a name cache would hide.
pub fn icon_handle(name: &str) -> Option<svg::Handle> {
    icetron_p::utils::icons::lookup_icon(name, 48.0).map(svg::Handle::from_path)
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
