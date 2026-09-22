//! Compositor window chrome — wraps icetron's `app_header`.
//!
//! Provides a thin adapter between icetron's `app_header` component and the
//! compositor's SSD decoration system. The header uses icetron's theme tokens
//! so SSD windows match CSD visual design.

use iced_core::Alignment;
use iced_core::{Element, Length};
use iced_widget::{Svg, button, container, row, svg, tooltip};
use icetron_p::prelude::{
    animated_opacity, animated_tooltip, app_header, header_height_for, header_render_height_for,
    styled_text,
};
use icetron_p::{
    animation::transition::ButtonTransition,
    components::{draggable::draggable, icons::icon_svg_inherit},
};
use icetron_themes::WindowHeaderStyle;
use icetron_themes::icons;

use crate::comp_theme::CompTheme;
use crate::fl;

/// Fullscreen Halo sits 10 px inside the output in the prototype, excluding both
/// the raster's shadow padding and the widget's own top inset.
pub(crate) fn fullscreen_header_offset(theme: &CompTheme) -> f64 {
    10.0 - f64::from(halo_shadow_padding(theme).top + theme.halo_style().top_inset)
}

pub(crate) fn halo_is_visible(
    fullscreen: bool,
    hovered: bool,
    focused: bool,
    menu_open: bool,
) -> bool {
    hovered || (!fullscreen && focused) || menu_open
}

/// Motion of the live design prototype's `components/halo/halo.css`:
/// translateY(3px -> 0), opacity via --ease-standard, slide via --ease-spring.
/// Icetron calls that non-overshooting slide curve `ease_out_expo`; its
/// `ease_spring` is a different, bouncing curve. The prototype's standard fade
/// curve has no matching ThemeInterface role, so retain its control points here.
pub(crate) fn halo_visibility(theme: &CompTheme, visible: bool) -> crate::utils::iced::Visibility {
    crate::utils::iced::Visibility {
        visible,
        duration: crate::backend::render::animations::motion::ms(
            theme.animation_transition_duration_fade_default(),
        ),
        opacity_curve: [0.4, 0.0, 0.2, 1.0],
        translation_curve: theme.ease_out_expo(),
        hidden_offset: iced_core::Vector::new(0.0, 3.0),
        hidden_scale: 1.0,
        animate_initial: false,
    }
}

/// Visibility of a window's Halo, joined to the window or overlapping it.
pub(crate) fn window_halo_visibility(
    theme: &CompTheme,
    visible: bool,
    joined: bool,
) -> crate::utils::iced::Visibility {
    let mut visibility = halo_visibility(theme, visible);
    if joined {
        // An attached Halo fades in place.
        visibility.hidden_offset = iced_core::Vector::ZERO;
    }
    visibility
}

/// WindowFrame's focus draw uses Kora's --duration-slow (420ms) and
/// --ease-standard, shared with the Halo fill. Icetron's similarly named
/// tokens currently mean 500ms and a different ease-out curve; do not
/// substitute them here. A zero-duration theme still disables the sweep.
pub(crate) fn halo_focus_outline(
    theme: &CompTheme,
    focused: bool,
    fullscreen: bool,
) -> crate::utils::iced::FocusOutline {
    crate::utils::iced::FocusOutline {
        focused,
        bottom_border: true,
        animate: !fullscreen && theme.duration_slower() > 0.0,
        duration: std::time::Duration::from_millis(420),
        curve: halo_visibility(theme, true).opacity_curve,
    }
}

/// Baseline decoration space; overlay Halo reserves none.
pub fn ssd_header_height(theme: &CompTheme) -> u32 {
    let style = theme.window_header_style();
    if style == WindowHeaderStyle::Halo {
        0
    } else {
        header_height_for(&**theme, style) as u32
    }
}

/// Joined Halo is real decoration space; protocol opt-in Halo stays an overlay.
pub(crate) fn ssd_header_height_for(theme: &CompTheme, joined: bool) -> u32 {
    if uses_halo_header(theme) && joined {
        theme.halo_style().pill_height().ceil() as u32
    } else {
        ssd_header_height(theme)
    }
}

/// Height of the compositor chrome render layer.
pub fn ssd_header_render_height(theme: &CompTheme) -> u32 {
    let padding = halo_shadow_padding(theme);
    ssd_header_input_height(theme) + padding.top as u32 + padding.bottom as u32
}

/// Height routed to compositor chrome and its drag region.
pub fn ssd_header_input_height(theme: &CompTheme) -> u32 {
    header_render_height_for(&**theme, theme.window_header_style()) as u32
}

/// Render-only space for the pill shadow; it does not enlarge the drag region.
fn halo_shadow_padding(theme: &CompTheme) -> iced_core::Padding {
    let mut padding = iced_core::Padding::ZERO;
    if uses_halo_header(theme) {
        for shadow in theme
            .shadow_popover()
            .iter()
            .filter(|s| !s.inset && s.color.a > 0.0)
        {
            let reach = shadow.blur_radius.max(0.0) + shadow.spread_radius.max(0.0);
            padding.top = padding.top.max((reach - shadow.offset.y).ceil());
            padding.bottom = padding.bottom.max((reach + shadow.offset.y).ceil());
        }
    }
    padding
}

/// Width of a row of controls that must never be squeezed: the widths they
/// already occupy, plus the gap between them.
fn rigid_width(content: f32, items: u32, gap: f32) -> f32 {
    content + gap * items.saturating_sub(1) as f32
}

/// Side margin the Halo pill keeps clear so it never reaches the curve of the
/// window's own top corners, where the two roundings would leave a notch.
///
/// A window that squares its top corners has no curve to clear, so the pill is
/// free to use the full width — the margin follows the radius actually in
/// effect rather than a constant.
fn halo_corner_margin(theme: &CompTheme, square_top: bool) -> f32 {
    if square_top || !uses_halo_header(theme) {
        return 0.0;
    }
    let radii = theme.radius_window();
    radii[0].max(radii[1]).max(0.0)
}

/// Offset of the raster buffer, including shadow padding, above the client.
pub fn ssd_header_render_overhang(theme: &CompTheme) -> u32 {
    ssd_header_overhang(theme) + halo_shadow_padding(theme).top as u32
}

/// Distance Halo chrome renders above the client surface.
pub fn ssd_header_overhang(theme: &CompTheme) -> u32 {
    if uses_halo_header(theme) {
        theme.halo_style().overhang as u32
    } else {
        0
    }
}

/// Whether the active theme selects Halo chrome.
pub fn uses_halo_header(theme: &CompTheme) -> bool {
    theme.window_header_style() == WindowHeaderStyle::Halo
}

/// Extra lift for clients that have not opted into an overlapping Halo.
/// The pill's bottom edge sits flush against the top of the client.
pub(crate) fn halo_header_lift(theme: &CompTheme, allows_overlay: bool) -> i32 {
    if !uses_halo_header(theme) || allows_overlay {
        return 0;
    }
    let metrics = theme.halo_style();
    (metrics.top_inset + metrics.pill_height() - ssd_header_overhang(theme) as f32)
        .ceil()
        .max(0.0) as i32
}

/// Position relative to the outer window, whose joined header is above the client.
pub(crate) fn halo_header_offset(theme: &CompTheme, joined: bool) -> i32 {
    if uses_halo_header(theme) {
        halo_header_lift(theme, !joined) - ssd_header_height_for(theme, joined) as i32
    } else {
        0
    }
}

/// The pill's bottom edge, relative to the top of the window it decorates.
pub(crate) fn halo_pill_bottom(theme: &CompTheme, joined: bool) -> i32 {
    let metrics = theme.halo_style();
    (metrics.top_inset + metrics.pill_height()).ceil() as i32
        - ssd_header_overhang(theme) as i32
        - halo_header_offset(theme, joined)
}

/// A fullscreen window's pill hangs from the output's top edge instead.
pub(crate) fn fullscreen_pill_bottom(theme: &CompTheme) -> i32 {
    let metrics = theme.halo_style();
    let pill_top = fullscreen_header_offset(theme)
        + f64::from(halo_shadow_padding(theme).top + metrics.top_inset);
    (pill_top + f64::from(metrics.pill_height())).ceil() as i32
}

/// Application icon for the SSD header — leaked static SVG bytes or a raster image handle.
///
/// SVG bytes are leaked once per window to obtain `&'static [u8]` for icetron's
/// `title_icon()` API. This is bounded (one allocation per window lifetime).
#[derive(Clone, Debug)]
pub enum AppIcon {
    /// Leaked SVG bytes — can be passed directly to `title_icon()`.
    ///
    /// `symbolic` marks a single-colour glyph that should take the title
    /// colour. A full-colour app mark must not: iced's `svg::Style::color` is
    /// an RGB *replacement* filter, so tinting one flattens the whole mark into
    /// a solid block of that colour, keeping only its alpha silhouette.
    Svg {
        bytes: &'static [u8],
        symbolic: bool,
    },
    /// Pre-scaled RGBA raster image (not usable with title_icon, ignored for now).
    Image(iced_core::image::Handle),
}

/// One pinned command in the Halo's tray.
///
/// Resolved by the window rather than the header: what is pinned is a property
/// of the app, and only the window knows which of its verbs an id names.
#[derive(Clone, Debug)]
pub struct TrayEntry<Message> {
    pub icon: icetron_themes::Icon,
    pub message: Message,
    pub label: String,
    /// A stateful command that is currently running.
    pub on: bool,
}

/// Builder for the compositor SSD header bar.
pub struct HeaderBar<'a, Message> {
    title: String,
    app_name: Option<String>,
    on_drag: Option<Message>,
    on_close: Option<Message>,
    on_minimize: Option<Message>,
    on_maximize: Option<Message>,
    on_right_click: Option<Message>,
    /// The app glyph's own press — the window's commands.
    on_commands: Option<Message>,
    tray: Vec<TrayEntry<Message>>,
    on_new_window: Option<Message>,
    on_fullscreen: Option<Message>,
    fullscreen: bool,
    menu_open: bool,
    commands_open: bool,
    focused: bool,
    hovered: bool,
    maximized: bool,
    /// Whether the top corners sit in SCREEN corners, so the header must square
    /// them. Distinct from `maximized`: a maximized window laid out into an
    /// inset non-exclusive zone is still maximized (and still shows the restore
    /// button) while no longer touching the top edge.
    square_top: bool,
    compositor_outline: bool,
    joined_to_window: bool,
    theme: Option<&'a CompTheme>,
    app_icon: Option<AppIcon>,
}

impl<'a, Message: Clone + 'static> Default for HeaderBar<'a, Message> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a, Message: Clone + 'static> HeaderBar<'a, Message> {
    pub fn new() -> Self {
        Self {
            title: String::new(),
            app_name: None,
            on_drag: None,
            on_close: None,
            on_minimize: None,
            on_maximize: None,
            on_right_click: None,
            on_commands: None,
            tray: Vec::new(),
            on_new_window: None,
            on_fullscreen: None,
            fullscreen: false,
            menu_open: false,
            commands_open: false,
            focused: false,
            hovered: false,
            maximized: false,
            square_top: false,
            compositor_outline: false,
            joined_to_window: false,
            theme: None,
            app_icon: None,
        }
    }

    pub fn title(mut self, title: impl Into<String>) -> Self {
        self.title = title.into();
        self
    }

    pub fn app_name(mut self, app_name: impl Into<String>) -> Self {
        self.app_name = Some(app_name.into());
        self
    }

    pub fn on_drag(mut self, msg: Message) -> Self {
        self.on_drag = Some(msg);
        self
    }

    pub fn on_close(mut self, msg: Message) -> Self {
        self.on_close = Some(msg);
        self
    }

    pub fn on_minimize(mut self, msg: Message) -> Self {
        self.on_minimize = Some(msg);
        self
    }

    pub fn on_maximize(mut self, msg: Message) -> Self {
        self.on_maximize = Some(msg);
        self
    }

    pub fn on_right_click(mut self, msg: Message) -> Self {
        self.on_right_click = Some(msg);
        self
    }

    /// Pressing the app glyph. Without it the glyph is a plain mark.
    pub fn on_commands(mut self, msg: Message) -> Self {
        self.on_commands = Some(msg);
        self
    }

    /// The commands pinned to this window's header, in pin order.
    pub fn tray(mut self, tray: Vec<TrayEntry<Message>>) -> Self {
        self.tray = tray;
        self
    }

    pub fn on_new_window(mut self, msg: Message) -> Self {
        self.on_new_window = Some(msg);
        self
    }

    pub fn on_fullscreen(mut self, msg: Message, fullscreen: bool) -> Self {
        self.on_fullscreen = Some(msg);
        self.fullscreen = fullscreen;
        self
    }

    pub fn menu_open(mut self, open: bool) -> Self {
        self.menu_open = open;
        self
    }

    /// Hold the app glyph in its pressed state while its palette is open.
    pub fn commands_open(mut self, open: bool) -> Self {
        self.commands_open = open;
        self
    }

    pub fn focused(mut self, focused: bool) -> Self {
        self.focused = focused;
        self
    }

    pub fn hovered(mut self, hovered: bool) -> Self {
        self.hovered = hovered;
        self
    }

    /// Square the header's top corners. Must agree with the frame drawn around
    /// it — see `CosmicWindowInternal::squares_top_corners`.
    pub fn square_top(mut self, square_top: bool) -> Self {
        self.square_top = square_top;
        self
    }

    pub fn maximized(mut self, maximized: bool) -> Self {
        self.maximized = maximized;
        self
    }

    pub fn app_icon(mut self, icon: AppIcon) -> Self {
        self.app_icon = Some(icon);
        self
    }

    pub fn theme(mut self, theme: &'a CompTheme) -> Self {
        self.theme = Some(theme);
        self
    }

    pub fn compositor_outline(mut self, enabled: bool) -> Self {
        self.compositor_outline = enabled;
        self
    }

    pub(crate) fn joined_to_window(mut self, joined: bool) -> Self {
        self.joined_to_window = joined;
        self
    }

    /// Natural widths of the pill's two control groups, before anything is
    /// dropped: the tray (divider, capture pair, chevron, new window) and the
    /// window controls. The identity keeps room for them so it gives way
    /// first, and the tray keeps room for the controls for the same reason.
    fn halo_control_widths(&self, theme: &CompTheme) -> (f32, f32) {
        let metrics = theme.halo_style();
        let pinned = self.tray.len() as f32;
        // The divider only exists to separate the identity from the pins, so a
        // header with nothing pinned pays for neither.
        let mut tray = if self.tray.is_empty() {
            0.0
        } else {
            metrics.border_width
                + pinned * metrics.control_size
                + (pinned - 1.0) * theme.spacing_0_5()
        };
        // Two, not one: the divider and the run of pins are separate items in
        // the tray row, so they are separated by a gap of their own.
        let mut tray_items = if self.tray.is_empty() { 0 } else { 2 };
        for present in [self.on_right_click.is_some(), self.on_new_window.is_some()] {
            if present {
                tray += metrics.control_size;
                tray_items += 1;
            }
        }
        let mut actions = 0.0_f32;
        let mut action_items = 0_u32;
        for present in [
            self.on_minimize.is_some(),
            self.on_maximize.is_some(),
            self.on_fullscreen.is_some(),
            self.on_close.is_some(),
        ] {
            if present {
                actions += metrics.control_size;
                action_items += 1;
            }
        }
        (
            rigid_width(tray, tray_items, metrics.gap),
            rigid_width(actions, action_items, metrics.gap),
        )
    }

    /// Convert to an iced Element using icetron's app_header.
    pub fn into_element(self) -> Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer> {
        let theme = self.theme.expect("HeaderBar requires .theme()");
        let window_header_style = theme.window_header_style();
        let halo = uses_halo_header(theme);

        let chrome_theme = if self.compositor_outline && uses_halo_header(theme) {
            theme.halo_chrome_theme()
        } else {
            &**theme
        };
        let mut header = app_header(chrome_theme)
            .joined_to_window(self.joined_to_window)
            .window_header_style(window_header_style)
            .title(Some(&self.title))
            .focused(self.focused)
            // Halo visibility is composited with its blur by IcedElement.
            .hovered(uses_halo_header(theme) || self.hovered || self.focused)
            .is_windowed(!self.maximized)
            .backdrop_blur(uses_halo_header(theme))
            .opaque(true)
            .show_border(true);
        if self.compositor_outline && halo {
            // The outline shader draws the pill's edge. Icetron mixes its own from
            // the border AND the accent, so blanking the border alone leaves a
            // hairline, and at 2x that hairline streaks straight past the corner.
            header = header.accent(iced_core::Color::TRANSPARENT);
        }
        // A Halo pill lays its own identity out — see `title_row` below — so
        // the app name is rendered here, not handed to icetron, which would
        // otherwise append it in a plain row that starves it of width. Icetron
        // hides a name that adds nothing; mirror that rule rather than move it.
        let subtitle = self
            .app_name
            .as_deref()
            .filter(|name| halo && !name.is_empty() && *name != self.title);
        if !halo && let Some(name) = self.app_name.as_deref() {
            header = header.app_name(name.to_owned());
        }

        // Pass application icon with native SVG colors via title_content.
        // We wrap in animated_opacity to replicate app_header's title fade behavior
        // (0.8 when unfocused, animates to 1.0 on hover/focus).
        {
            let (title_style, title_color, title_gap, icon_size, glyph_size) = if halo {
                let mut style = theme.text_styles().caption();
                let metrics = theme.halo_style();
                style.font_weight = metrics.title_font_weight;
                (
                    style,
                    theme.text_primary(),
                    metrics.gap,
                    metrics.control_size,
                    metrics.glyph_icon_size,
                )
            } else {
                (
                    theme.header_title_text_style(),
                    theme.header_title_color(),
                    theme.header_title_gap(),
                    theme.ui_size_icon_sm(),
                    theme.ui_size_icon_sm(),
                )
            };

            let text_element: Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer> =
                styled_text(&self.title, title_style, title_color)
                    .wrapping(iced_widget::text::Wrapping::None)
                    .ellipsis(iced_widget::text::Ellipsis::End)
                    .into();

            // A window whose mark never resolved still needs the button: the
            // glyph is the route to its commands, so it falls back to a generic
            // one rather than leaving the pill with nothing to press.
            let app_icon = self.app_icon.clone().or_else(|| {
                halo.then_some(AppIcon::Svg {
                    bytes: icons::APP_WINDOW.bytes,
                    symbolic: true,
                })
            });
            let icon_element: Option<
                Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer>,
            > = app_icon.as_ref().map(|icon| {
                let icon_element: Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer> =
                    match icon {
                        AppIcon::Svg { bytes, symbolic } => {
                            let handle = iced_core::svg::Handle::from_memory(*bytes);
                            // A fully transparent tint is this iced fork's
                            // opt-out from the colour filter. `None` does *not*
                            // mean "leave the artwork alone" here — the fork
                            // reads it as "inherit the parent's text colour",
                            // which is what painted full-colour app marks flat
                            // title-grey. Only a symbolic glyph is recoloured.
                            let icon_tint = if *symbolic {
                                if uses_halo_header(theme) {
                                    theme.halo_accent()
                                } else {
                                    title_color
                                }
                            } else {
                                iced_core::Color::TRANSPARENT
                            };
                            Svg::new(handle)
                                .width(glyph_size)
                                .height(glyph_size)
                                .style(move |_theme, _status| svg::Style {
                                    color: Some(icon_tint),
                                })
                                .into()
                        }
                        AppIcon::Image(handle) => iced_widget::image::Image::new(handle.clone())
                            .width(glyph_size)
                            .height(glyph_size)
                            .into(),
                    };
                let icon_element: Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer> =
                    if halo {
                        halo_glyph_disc(
                            icon_element,
                            icon_size,
                            self.on_commands.clone(),
                            self.commands_open,
                            self.menu_open || self.commands_open,
                            theme,
                        )
                    } else {
                        icon_element
                    };
                icon_element
            });

            // The Halo pill shrinks to its identity, so a long title otherwise
            // takes every pixel iced offers it and leaves the app name none.
            // `ElasticRow` shares the shortfall instead: the icon keeps its
            // size, and the two labels ellipsize by max-min fairness.
            let title_row: Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer> =
                if halo {
                    let metrics = theme.halo_style();
                    let (tray_natural, actions_natural) = self.halo_control_widths(theme);
                    // Only the siblings iced does not already account for: the
                    // divider is Fixed and every gap is taken off the limit
                    // before this row is measured, so reserving them again
                    // would shrink the pill while it still fits.
                    let after = tray_natural + actions_natural;
                    let mut identity = crate::utils::iced::ElasticRow::new()
                        .spacing(title_gap)
                        .reserve(after)
                        // Close never leaves, so its room outranks the floor.
                        .reserve_min(
                            metrics.control_size + metrics.border_width + 2.0 * metrics.gap,
                        )
                        // Keep the name readable before anything else goes: a
                        // title cut to two letters is no use, so controls give
                        // way first and the title only shrinks past this once
                        // they have. Roughly ten characters at this size.
                        .floor(title_style.font_size * 6.0);
                    if let Some(icon) = icon_element {
                        identity = identity.push_rigid(icon);
                    }
                    identity = identity.push_elastic(text_element);
                    if let Some(name) = subtitle {
                        // Icetron's Halo subtitle: the regular body face, with
                        // only the size and line height of the micro role.
                        let mut style = theme.text_styles().body();
                        let micro = theme.text_styles().micro();
                        style.font_size = micro.font_size;
                        style.line_height = micro.line_height;
                        // First to go: it ellipsizes while that still helps,
                        // then leaves rather than sit there as an ellipsis.
                        identity = identity.push_elastic_droppable(
                            1,
                            styled_text(name, style, theme.text_quaternary())
                                .wrapping(iced_widget::text::Wrapping::None)
                                .ellipsis(iced_widget::text::Ellipsis::End),
                        );
                    }
                    identity.into_single().unwrap_or_else(Into::into)
                } else if let Some(icon) = icon_element {
                    row![icon, text_element]
                        .spacing(title_gap)
                        .align_y(Alignment::Center)
                        .into()
                } else {
                    // No icon yet (async resolution pending) — show title only
                    text_element
                };

            // The Halo animates as one pill; bar headers animate just the title.
            let target_opacity = if halo || self.hovered || self.focused {
                1.0
            } else {
                0.8
            };
            let title_content: Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer> =
                if target_opacity == 1.0 {
                    title_row
                } else {
                    animated_opacity(title_row, &**theme, target_opacity).into()
                };

            header = header.title_content(title_content);
        }

        if halo {
            let metrics = theme.halo_style();
            let (_, actions_natural) = self.halo_control_widths(theme);
            let mut tray = crate::utils::iced::ElasticRow::new()
                .spacing(metrics.gap)
                // The tray's own buttons rank below every window control, so it
                // keeps the whole control row's width — not just the close
                // button's — and sheds completely before one of them goes.
                // Anything less and the tray starves the controls at one width
                // and hands them back at a narrower one.
                .reserve(actions_natural)
                .reserve_min(actions_natural);
            if !self.tray.is_empty() {
                let pinned = row(self.tray.iter().map(|entry| {
                    halo_button(
                        entry.icon,
                        Some(entry.message.clone()),
                        entry.label.clone(),
                        HaloButtonRole::Tray { on: entry.on },
                        self.menu_open,
                        theme,
                    )
                }))
                .spacing(theme.spacing_0_5());
                let divider = container(iced_widget::Space::new())
                    .width(metrics.border_width)
                    .height(metrics.divider_height)
                    .style(move |_| container::Style {
                        background: Some(iced_core::Background::Color(theme.stroke_subtle())),
                        ..Default::default()
                    });
                // The divider only separates the title from the tray, so it
                // goes with the pins rather than lingering as a stray hairline.
                tray = tray.push_droppable(
                    4,
                    row![divider, pinned]
                        .spacing(metrics.gap)
                        .align_y(Alignment::Center),
                );
            }
            if let Some(message) = self.on_right_click.clone() {
                tray = tray.push_droppable(
                    2,
                    halo_button(
                        icons::CHEVRON_DOWN,
                        Some(message),
                        fl!("halo-window-menu"),
                        HaloButtonRole::Menu,
                        self.menu_open,
                        theme,
                    ),
                );
            }
            if let Some(message) = self.on_new_window.clone() {
                tray = tray.push_droppable(
                    3,
                    halo_button(
                        icons::PLUS,
                        Some(message),
                        fl!("window-menu-new-window"),
                        HaloButtonRole::Window,
                        self.menu_open,
                        theme,
                    ),
                );
            }
            // Close is the one control that never leaves; the rest go in this
            // order as the pill runs out of room.
            let mut actions = crate::utils::iced::ElasticRow::new().spacing(metrics.gap);
            let restore = self.maximized || self.fullscreen;
            for (icon, message, label, destructive, rank) in [
                (
                    icons::MINUS,
                    self.on_minimize.clone(),
                    fl!("window-menu-minimize"),
                    false,
                    Some(1),
                ),
                (
                    if restore {
                        icons::MINIMIZE_2
                    } else {
                        icons::MAXIMIZE_2
                    },
                    self.on_maximize.clone(),
                    if restore {
                        fl!("window-menu-restore")
                    } else {
                        fl!("window-menu-maximize")
                    },
                    false,
                    Some(3),
                ),
                (
                    if self.fullscreen {
                        icons::SHRINK
                    } else {
                        icons::FULLSCREEN
                    },
                    self.on_fullscreen.clone(),
                    if self.fullscreen {
                        fl!("window-menu-leave-fullscreen")
                    } else {
                        fl!("window-menu-fullscreen")
                    },
                    false,
                    Some(2),
                ),
                (
                    icons::X,
                    self.on_close.clone(),
                    fl!("window-menu-close"),
                    true,
                    None,
                ),
            ] {
                if let Some(message) = message {
                    let button = halo_button(
                        icon,
                        Some(message),
                        label,
                        if destructive {
                            HaloButtonRole::Close
                        } else {
                            HaloButtonRole::Window
                        },
                        self.menu_open,
                        theme,
                    );
                    actions = match rank {
                        Some(rank) => actions.push_droppable(rank, button),
                        None => actions.push_rigid(button),
                    };
                }
            }
            header = header
                .trailing(tray)
                .action_buttons(actions)
                .menu_open(self.menu_open);
        }
        if !halo && let Some(msg) = self.on_drag.clone() {
            header = header.on_drag(msg);
        }
        if let Some(msg) = self.on_close {
            header = header.on_close(msg);
        }
        if let Some(msg) = self.on_minimize {
            header = header.on_minimize(msg);
        }
        // app_header uses on_toggle_window for maximize/unmaximize
        if !halo && let Some(msg) = self.on_maximize.clone() {
            header = header.on_toggle_window(msg);
        }
        if !halo && let Some(msg) = self.on_right_click.clone() {
            header = header.on_right_click(msg);
        }

        let header_render_height = header_render_height_for(&**theme, window_header_style);
        // Force header background to fully opaque — the blur backdrop renders
        // behind the header and should not bleed through.
        let header_bg = theme.header_background();
        let top_radius = if self.square_top {
            0.0
        } else {
            theme.radius_window()[0]
        };
        let header_elem: Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer> =
            header.into();
        if uses_halo_header(theme) {
            // Own all gestures beside the custom controls. Do not also wire
            // AppHeader's inner drag area, which would consume these events.
            let mut gestures = draggable(header_elem);
            if let Some(message) = self.on_drag {
                gestures = gestures.on_drag(message);
            }
            if let Some(message) = self.on_maximize {
                gestures = gestures.on_double_click(message);
            }
            if let Some(message) = self.on_right_click {
                gestures = gestures.on_right_click(message);
            }
            let header_elem: Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer> =
                gestures.into();
            // Hold the pill inside the window's rounded top corners. Capping the
            // space it is offered, rather than its own width, keeps it a Shrink
            // pill that still hugs a short title — and needs no window width.
            let mut padding = halo_shadow_padding(theme);
            let margin = halo_corner_margin(theme, self.square_top);
            padding.left = padding.left.max(margin);
            padding.right = padding.right.max(margin);
            return container(header_elem)
                .width(Length::Fill)
                .height(Length::Fixed(ssd_header_render_height(theme) as f32))
                .padding(padding)
                .into();
        }
        container(header_elem)
            .width(Length::Fill)
            .height(Length::Fixed(header_render_height))
            .style(move |_theme| container::Style {
                background: Some(iced_core::Background::Color(header_bg)),
                border: iced_core::Border {
                    radius: iced_core::border::Radius {
                        top_left: top_radius,
                        top_right: top_radius,
                        bottom_right: 0.0,
                        bottom_left: 0.0,
                    },
                    ..Default::default()
                },
                ..Default::default()
            })
            .into()
    }
}

/// The prototype's "on" mark (`.kora-halo__tray-item--on::after` in
/// `halo.css`): a 5px dot 1px in from the corner, glowing 5px. No token
/// carries these, so they are literal.
const RECORD_DOT_PX: f32 = 5.0;
const RECORD_DOT_INSET_PX: f32 = 1.0;

/// The app glyph's accent disc, and the button around it when the window has
/// commands to offer.
fn halo_glyph_disc<'a, Message: Clone + 'static>(
    mark: Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer>,
    size: f32,
    on_press: Option<Message>,
    active: bool,
    surface_open: bool,
    theme: &'a CompTheme,
) -> Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer> {
    let bg = theme.halo_accent_background();
    let radius = theme.radii_max();
    let content = container(mark)
        .align_x(Alignment::Center)
        .align_y(Alignment::Center);
    let Some(message) = on_press else {
        return content
            .width(Length::Fixed(size))
            .height(Length::Fixed(size))
            .style(move |_theme| container::Style {
                background: Some(iced_core::Background::Color(bg)),
                border: iced_core::Border::default().rounded(radius),
                ..Default::default()
            })
            .into();
    };
    // The design's `0 0 0 1.5px color-mix(--ws-accent 50%, transparent)` hover
    // ring, drawn as a border because the disc's mark is far smaller than it.
    let ring = {
        let mut ring = theme.halo_accent();
        ring.a *= HALO_GLYPH_RING_ACCENT;
        ring
    };
    let button = button(content)
        .on_press(message)
        .padding(0)
        .width(Length::Fixed(size))
        .height(Length::Fixed(size))
        .standard_transition(&**theme)
        .style(move |_, status| {
            let lit = active || matches!(status, button::Status::Hovered | button::Status::Pressed);
            button::Style {
                background: Some(iced_core::Background::Color(bg)),
                border: iced_core::Border {
                    color: if lit {
                        ring
                    } else {
                        iced_core::Color::TRANSPARENT
                    },
                    width: if lit { HALO_GLYPH_RING_WIDTH } else { 0.0 },
                    radius: radius.into(),
                    ..Default::default()
                },
                ..Default::default()
            }
        });
    animated_tooltip(button, fl!("halo-commands-hint"), &**theme)
        .position(tooltip::Position::Bottom)
        .enabled(!surface_open)
        .animation_duration(std::time::Duration::ZERO)
        .compositor_managed(true)
        .into()
}

/// The glyph's hover ring, from the design's `1.5px` at 50% accent.
const HALO_GLYPH_RING_WIDTH: f32 = 1.5;
const HALO_GLYPH_RING_ACCENT: f32 = 0.5;

enum HaloButtonRole {
    /// A pinned command; `on` is a stateful one currently active.
    Tray {
        on: bool,
    },
    Menu,
    Window,
    Close,
}

fn halo_button<'a, Message: Clone + 'static>(
    icon: icetron_themes::Icon,
    message: Option<Message>,
    label: impl ToString,
    role: HaloButtonRole,
    menu_open: bool,
    theme: &'a CompTheme,
) -> Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer> {
    let metrics = theme.halo_style();
    let active = menu_open && matches!(role, HaloButtonRole::Menu);
    let icon_size = match role {
        HaloButtonRole::Tray { .. } => metrics.glyph_icon_size,
        HaloButtonRole::Menu => metrics.menu_icon_size,
        HaloButtonRole::Window | HaloButtonRole::Close => metrics.control_icon_size,
    };
    let destructive = matches!(role, HaloButtonRole::Close);
    // A stateful command that is on wears the destructive colour, hovered
    // or not: the tint says "capturing", and hovering is how it is stopped.
    let on = matches!(role, HaloButtonRole::Tray { on: true });
    let text_color = theme.text_tertiary();
    let background = theme.overlay_5();
    let disabled = theme.text_quaternary();
    let glyph = container(icon_svg_inherit(icon, icon_size))
        .center_x(Length::Fill)
        .center_y(Length::Fill);
    let content: Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer> = if on {
        let mark = crate::utils::iced::pulse::PulsingDot::new(
            RECORD_DOT_PX,
            theme.feedback_error_primary(),
            RECORD_DOT_PX,
            theme.motion.ease_standard_cp,
        )
        .still(theme.reduced_motion);
        iced_widget::stack![
            glyph,
            container(mark)
                .width(Length::Fill)
                .height(Length::Fill)
                .align_x(iced_core::alignment::Horizontal::Right)
                .align_y(iced_core::alignment::Vertical::Top)
                .padding(RECORD_DOT_INSET_PX),
        ]
        .into()
    } else {
        glyph.into()
    };
    let button = button(content)
        .width(metrics.control_size)
        .height(metrics.control_size)
        .padding(0)
        .on_press_maybe(message)
        .standard_transition(&**theme)
        .style(move |_, status| {
            let hovered = matches!(status, button::Status::Hovered | button::Status::Pressed);
            button::Style {
                text_color: if matches!(status, button::Status::Disabled) {
                    disabled
                } else if on {
                    theme.feedback_error_primary()
                } else if hovered || active {
                    theme.text_primary()
                } else {
                    text_color
                },
                background: (hovered || active).then_some(iced_core::Background::Color(
                    if destructive && hovered {
                        theme.feedback_error_primary()
                    } else {
                        background
                    },
                )),
                border: iced_core::Border::default().rounded(theme.radii_max()),
                ..Default::default()
            }
        });
    animated_tooltip(button, label, &**theme)
        .position(tooltip::Position::Bottom)
        .enabled(!menu_open)
        // Icetron owns the hover/focus delay and suppression; the compositor
        // fades the separate surface and its backdrop in AND out together.
        .animation_duration(std::time::Duration::ZERO)
        .compositor_managed(true)
        .into()
}

impl<'a, Message: Clone + 'static> From<HeaderBar<'a, Message>>
    for Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer>
{
    fn from(header: HeaderBar<'a, Message>) -> Self {
        header.into_element()
    }
}

/// Create a new header bar builder.
pub fn header_bar<'a, Message: Clone + 'static>() -> HeaderBar<'a, Message> {
    HeaderBar::new()
}

/// The default tray — the capture pair, as every window starts out pinned.
#[cfg(test)]
pub(crate) fn capture_tray(recording: bool) -> Vec<TrayEntry<()>> {
    vec![
        TrayEntry {
            icon: icons::CAMERA,
            message: (),
            label: fl!("halo-screenshot-window"),
            on: false,
        },
        TrayEntry {
            icon: icons::CIRCLE,
            message: (),
            label: if recording {
                fl!("window-menu-stop-recording")
            } else {
                fl!("window-menu-record")
            },
            on: recording,
        },
    ]
}

#[cfg(test)]
mod snapshot_tests;

#[cfg(test)]
mod tests {
    use super::*;
    use icetron_themes::dynamic::DEFAULT_THEME_PAIR;
    use std::sync::Arc;

    #[test]
    fn overlay_halo_does_not_reserve_client_layout_space() {
        let mut theme = DEFAULT_THEME_PAIR.load(false);
        theme.window_header_style = WindowHeaderStyle::Halo;
        let theme = CompTheme::new(Arc::new(theme), false);

        assert_eq!(ssd_header_height(&theme), 0);
        assert_eq!(ssd_header_height_for(&theme, false), 0);
        assert_eq!(ssd_header_overhang(&theme), 18);
        let padding = halo_shadow_padding(&theme);
        assert_eq!(
            ssd_header_render_height(&theme),
            34 + padding.top as u32 + padding.bottom as u32
        );
        assert_eq!(ssd_header_input_height(&theme), 34);
    }

    #[test]
    fn joined_halo_reserves_only_the_visible_header() {
        let mut tokens = DEFAULT_THEME_PAIR.load(false);
        tokens.window_header_style = WindowHeaderStyle::Halo;
        let theme = CompTheme::new(Arc::new(tokens), false);
        let reserved = ssd_header_height_for(&theme, true) as i32;
        assert_eq!(reserved, 31);
        let overhang = ssd_header_overhang(&theme) as i32 + halo_header_offset(&theme, true);
        assert_eq!(
            overhang, 3,
            "only the widget's invisible top inset lies outside the frame"
        );
        assert_eq!(ssd_header_input_height(&theme) as i32 - overhang, reserved);
        assert!(reserved < ssd_header_render_height(&theme) as i32);
        assert_eq!(halo_header_offset(&theme, false), 0);
    }

    /// The palette hangs from the pill: 16px into a floating window, at the
    /// foot of a joined one's reserved band, and under a fullscreen inset.
    #[test]
    fn the_pill_bottom_follows_where_the_halo_sits() {
        let mut theme = DEFAULT_THEME_PAIR.load(false);
        theme.window_header_style = WindowHeaderStyle::Halo;
        let theme = CompTheme::new(Arc::new(theme), false);
        assert_eq!(halo_pill_bottom(&theme, false), 16);
        assert_eq!(halo_pill_bottom(&theme, true), 31);
        assert_eq!(fullscreen_pill_bottom(&theme), 41);
    }

    /// A joined Halo slid 3px as it hid; only overlay and fullscreen chrome move.
    #[test]
    fn joined_halo_fades_without_sliding() {
        let mut tokens = DEFAULT_THEME_PAIR.load(false);
        tokens.window_header_style = WindowHeaderStyle::Halo;
        let theme = CompTheme::new(Arc::new(tokens), false);
        for visible in [false, true] {
            let joined = window_halo_visibility(&theme, visible, true);
            assert_eq!(joined.hidden_offset, iced_core::Vector::ZERO);
            assert_eq!(
                joined.hidden_scale, 1.0,
                "scale would move it around its centre"
            );
            assert_eq!(
                window_halo_visibility(&theme, visible, false),
                halo_visibility(&theme, visible)
            );
        }
        assert_eq!(
            halo_visibility(&theme, false).hidden_offset,
            iced_core::Vector::new(0.0, 3.0)
        );
    }
}
