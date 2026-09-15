//! Compositor theme backed by icetron's `ThemeInterface`.
//!
//! `CompTheme` wraps an `Arc<dyn ThemeInterface>` to provide design tokens.
//! It is NOT the iced widget theme type — `iced_core::Theme` is used for that.
//! `CompTheme` is passed explicitly to view functions so they can extract
//! design tokens and pre-bake them into widget style closures.
//!
//! The theme is loaded at runtime from a `.ron` file installed by the
//! active theme package (e.g., `icetron-theme-playtron`).

use iced_core::{Background, Border, Color};
use icetron_themes::ThemeInterface;
use icetron_themes::dynamic::{DEFAULT_THEME_PAIR, DynamicTheme};
use std::ops::Deref;
use std::sync::Arc;
use tracing::info;

/// CSS `color-mix(in oklch, accent 45%, border)` premultiplies L/C but not hue.
fn mix_border_accent(border: Color, accent: Color) -> Color {
    use palette::{FromColor, Mix, Oklch, Srgb};
    let weight = 0.45;
    let alpha = border.a * (1.0 - weight) + accent.a * weight;
    if alpha <= f32::EPSILON {
        return Color::TRANSPARENT;
    }
    let mut base = Oklch::from_color(Srgb::new(border.r, border.g, border.b));
    let mut tint = Oklch::from_color(Srgb::new(accent.r, accent.g, accent.b));
    // A powerless hue (achromatic or fully transparent) takes the other hue.
    if base.chroma < 0.00001 || border.a == 0.0 {
        base.hue = tint.hue;
    }
    if tint.chroma < 0.00001 || accent.a == 0.0 {
        tint.hue = base.hue;
    }
    base.l *= border.a;
    base.chroma *= border.a;
    tint.l *= accent.a;
    tint.chroma *= accent.a;
    let mut mixed = base.mix(tint, weight);
    mixed.l /= alpha;
    mixed.chroma /= alpha;
    let rgb = Srgb::from_color(mixed);
    Color::from_rgba(
        rgb.red.clamp(0.0, 1.0),
        rgb.green.clamp(0.0, 1.0),
        rgb.blue.clamp(0.0, 1.0),
        alpha,
    )
}

/// Compositor theme replacing `cosmic::Theme`.
/// Wraps a `ThemeInterface` for design tokens and carries compositor-specific settings.
#[derive(Clone)]
pub struct CompTheme {
    /// The underlying design token provider.
    theme: Arc<dyn ThemeInterface>,
    /// The compositor supplies the Halo outline; keep its widget's fill,
    /// layout and shadow, but do not rasterize a second static hairline.
    halo_chrome: Arc<dyn ThemeInterface>,
    /// Whether this is a dark color scheme.
    pub is_dark: bool,
    /// Active window highlight border thickness (pixels).
    pub active_hint: u32,
    /// Gap sizes: (outer_gap, inner_gap) in logical pixels.
    pub gaps: (u32, u32),
    /// Motion tokens (durations, easing curves, springs) resolved from `theme`.
    /// Read by every animation instead of hardcoded constants.
    pub motion: crate::backend::render::animations::motion::Motion,
    /// Accent of the workspace currently on screen; brand accent is the fallback.
    pub workspace_accent: Option<Color>,
    /// The user asked for less motion: status marks hold still.
    pub reduced_motion: bool,
}

impl std::fmt::Debug for CompTheme {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CompTheme")
            .field("is_dark", &self.is_dark)
            .field("active_hint", &self.active_hint)
            .field("gaps", &self.gaps)
            .finish_non_exhaustive()
    }
}

impl Deref for CompTheme {
    type Target = dyn ThemeInterface;
    fn deref(&self) -> &Self::Target {
        &*self.theme
    }
}

impl Default for CompTheme {
    fn default() -> Self {
        let is_dark = true;
        let theme: Arc<dyn ThemeInterface> = Arc::new(DEFAULT_THEME_PAIR.load(is_dark));
        Self::new(theme, is_dark)
    }
}

impl CompTheme {
    pub fn new(theme: Arc<dyn ThemeInterface>, is_dark: bool) -> Self {
        let mut halo_chrome = DynamicTheme::from_theme(&*theme);
        halo_chrome.window_border_color = Color::TRANSPARENT;
        Self {
            halo_chrome: Arc::new(halo_chrome),
            motion: crate::backend::render::animations::motion::Motion::from_theme(&*theme),
            theme,
            is_dark,
            active_hint: 3,
            gaps: (4, 4),
            workspace_accent: None,
            reduced_motion: false,
        }
    }

    /// Load the current system theme (brand + color mode from disk).
    /// Falls back to the embedded playtron dark theme if nothing is configured.
    pub fn from_current() -> Self {
        use icetron_themes::color_mode::{Mode, get_color_mode};

        let mode = get_color_mode();
        let is_dark = matches!(mode, Mode::Dark);
        let theme_name = DynamicTheme::current_theme_name();

        info!(
            ?mode,
            is_dark,
            ?theme_name,
            "Loading compositor theme from current system config"
        );

        match theme_name {
            Some(ref name) => Self::from_file(name, is_dark),
            None => {
                // No symlink configured — use first available installed theme
                // (matches `icetron-theme current` fallback behavior).
                let fallback_name = DynamicTheme::list_available()
                    .first()
                    .map(|e| e.name.clone())
                    .unwrap_or_else(|| "playtron".to_string());
                info!(
                    fallback_name,
                    "No active theme symlink, using first available"
                );
                Self::from_file(&fallback_name, is_dark)
            }
        }
    }

    /// Load a theme by name from the system search paths.
    pub fn from_file(theme_name: &str, is_dark: bool) -> Self {
        let loaded = DynamicTheme::load_by_name(theme_name, is_dark);
        let using_fallback = loaded.is_none();
        let theme: Arc<dyn ThemeInterface> = Arc::new(loaded.unwrap_or_else(|| {
            let fallback = if is_dark {
                DEFAULT_THEME_PAIR.dark_fallback
            } else {
                DEFAULT_THEME_PAIR.light_fallback
            };
            ron::from_str(fallback).expect("embedded fallback theme RON is invalid")
        }));

        info!(
            theme_name,
            is_dark,
            using_fallback,
            header_style = ?theme.window_header_style(),
            radius_window = theme.radius_window(),
            bg_color = ?theme.fill_default(),
            text_color = ?theme.text_primary(),
            "CompTheme loaded, window radius: {}", theme.radius_window()
        );

        Self::new(theme, is_dark)
    }

    /// Access the underlying theme interface.
    pub fn theme(&self) -> &dyn ThemeInterface {
        &*self.theme
    }

    pub(crate) fn halo_chrome_theme(&self) -> &dyn ThemeInterface {
        &*self.halo_chrome
    }

    /// Neutral mid-gray color for backdrop/indicator effects.
    pub fn neutral_color(&self) -> WindowHintColor {
        let c = self.theme.text_tertiary();
        WindowHintColor {
            red: c.r,
            green: c.g,
            blue: c.b,
        }
    }

    pub fn accent_color(&self) -> Color {
        self.theme.primary()
    }

    pub fn halo_accent(&self) -> Color {
        self.workspace_accent.unwrap_or_else(|| self.primary())
    }

    pub fn halo_accent_background(&self) -> Color {
        let mut accent = self.halo_accent();
        accent.a = self.primary_lighter().a;
        accent
    }

    /// WindowFrame's Halo focus treatment; other chrome styles keep their border.
    pub(crate) fn focused_window_border(&self, focused: bool) -> Color {
        let border = self.window_border_color();
        if focused && self.window_header_style() == icetron_themes::WindowHeaderStyle::Halo {
            mix_border_accent(border, self.halo_accent())
        } else {
            border
        }
    }

    pub(crate) fn focused_window_ring(&self, focused: bool) -> Option<Color> {
        (focused && self.window_header_style() == icetron_themes::WindowHeaderStyle::Halo).then(
            || {
                let mut accent = self.halo_accent();
                accent.a *= 0.22;
                accent
            },
        )
    }

    pub fn on_accent_color(&self) -> Color {
        self.theme.primary_foreground()
    }

    pub fn on_bg_color(&self) -> Color {
        self.theme.text_primary()
    }

    pub fn bg_color(&self) -> Color {
        self.theme.fill_default()
    }

    pub fn surface_color(&self) -> Color {
        self.theme.surface_1()
    }

    pub fn on_surface_color(&self) -> Color {
        self.theme.text_primary()
    }

    pub fn divider_color(&self) -> Color {
        self.theme.stroke_subtle()
    }

    pub fn radius_s(&self) -> [f32; 4] {
        let r = self.theme.radius_sm();
        [r; 4]
    }

    pub fn radius_m(&self) -> [f32; 4] {
        let r = self.theme.radius_md();
        [r; 4]
    }

    /// Window corner radius for compositor decorations.
    /// Delegates to the theme's `radius_window()` token.
    pub fn radius_window(&self) -> [f32; 4] {
        [self.theme.radius_window(); 4]
    }

    /// Active window hint color (used for focus indication borders).
    pub fn active_window_hint(&self) -> WindowHintColor {
        let c = self.theme.primary();
        WindowHintColor {
            red: c.r,
            green: c.g,
            blue: c.b,
        }
    }

    /// Container style for accent-colored indicator boxes.
    pub fn accent_container_style(&self) -> ContainerStyle {
        ContainerStyle {
            text_color: Some(self.theme.primary_foreground()),
            background: Some(Background::Color(self.theme.primary())),
            border: Border {
                radius: 18.0.into(),
                width: 0.0,
                color: Color::TRANSPARENT,
                ..Default::default()
            },
        }
    }

    /// Container style for surface-colored floating panels (zoom bar, menus).
    pub fn surface_container_style(&self) -> ContainerStyle {
        ContainerStyle {
            text_color: Some(self.theme.text_primary()),
            background: Some(Background::Color(self.theme.surface_1())),
            border: Border {
                radius: radius_from_array(self.radius_s()),
                width: 1.0,
                color: self.theme.stroke_subtle(),
                ..Default::default()
            },
        }
    }

    /// Create an `iced_core::Theme` with a palette mapped from this theme's tokens.
    /// Used as the iced widget theme type for `UserInterface::draw()`.
    pub fn to_iced_theme(&self) -> iced_core::Theme {
        iced_core::Theme::custom(
            "compositor",
            iced_core::theme::Palette {
                background: self.bg_color(),
                text: self.on_bg_color(),
                primary: self.accent_color(),
                success: Color::from_rgb(0.0, 0.8, 0.0),
                warning: Color::from_rgb(0.8, 0.6, 0.0),
                danger: Color::from_rgb(0.8, 0.0, 0.0),
            },
        )
    }
}

/// Convert a `[f32; 4]` corner radius array to `iced_core::border::Radius`.
pub fn radius_from_array(r: [f32; 4]) -> iced_core::border::Radius {
    iced_core::border::Radius {
        top_left: r[0],
        top_right: r[1],
        bottom_right: r[2],
        bottom_left: r[3],
    }
}

/// Simple container style (replaces cosmic's theme::Container::custom closures).
#[derive(Debug, Clone)]
pub struct ContainerStyle {
    pub text_color: Option<Color>,
    pub background: Option<Background>,
    pub border: Border,
}

impl From<ContainerStyle> for iced_widget::container::Style {
    fn from(s: ContainerStyle) -> Self {
        iced_widget::container::Style {
            text_color: s.text_color,
            background: s.background,
            border: s.border,
            shadow: Default::default(),
            snap: false,
            border_only: false,
        }
    }
}

/// Color struct for active window hint (replaces `palette::Srgba`).
#[derive(Debug, Clone, Copy)]
pub struct WindowHintColor {
    pub red: f32,
    pub green: f32,
    pub blue: f32,
}

#[cfg(test)]
mod focus_border_tests {
    use super::*;
    use icetron_themes::WindowHeaderStyle;

    fn theme(style: WindowHeaderStyle) -> CompTheme {
        let mut tokens = DEFAULT_THEME_PAIR.load(true);
        tokens.window_header_style = style;
        tokens.window_border_color = Color::from_rgba(0.96, 0.94, 0.96, 0.08);
        CompTheme::new(Arc::new(tokens), true)
    }

    #[test]
    fn halo_focus_uses_workspace_accent_with_prototype_opacities() {
        let mut theme = theme(WindowHeaderStyle::Halo);
        let neutral = theme.focused_window_border(false);
        let blue = Color::from_rgb(0.0, 0.4, 1.0);
        theme.workspace_accent = Some(blue);
        let focused = theme.focused_window_border(true);
        assert!((focused.a - (0.45 + 0.55 * neutral.a)).abs() < 0.00001);
        assert_ne!(focused, neutral);
        assert_eq!(
            theme.focused_window_ring(true),
            Some(Color { a: 0.22, ..blue })
        );
        assert_eq!(theme.focused_window_ring(false), None);
        theme.workspace_accent = Some(Color::from_rgb(1.0, 0.3, 0.0));
        assert_ne!(theme.focused_window_border(true), focused);
        assert_eq!(theme.focused_window_border(false), neutral);
    }

    #[test]
    fn absent_workspace_accent_uses_brand_and_bar_chrome_stays_unchanged() {
        let theme = theme(WindowHeaderStyle::Halo);
        assert_eq!(
            theme.focused_window_border(true),
            mix_border_accent(theme.window_border_color(), theme.primary())
        );
        let bar = super::focus_border_tests::theme(WindowHeaderStyle::Bar);
        assert_eq!(bar.focused_window_border(true), bar.window_border_color());
        assert!(bar.focused_window_ring(true).is_none());
    }

    #[test]
    fn transparent_border_does_not_darken_the_accent() {
        let accent = Color::from_rgb(0.2, 0.5, 0.8);
        let color = mix_border_accent(Color::TRANSPARENT, accent);
        for (actual, expected) in [
            (color.r, accent.r),
            (color.g, accent.g),
            (color.b, accent.b),
        ] {
            assert!((actual - expected).abs() < 0.0001);
        }
        assert!((color.a - 0.45).abs() < 0.00001);
        assert_eq!(
            mix_border_accent(Color::TRANSPARENT, Color::TRANSPARENT),
            Color::TRANSPARENT
        );
    }
}
