//! Compositor window chrome — wraps icetron's `app_header`.
//!
//! Provides a thin adapter between icetron's `app_header` component and the
//! compositor's SSD decoration system. The header uses icetron's theme tokens
//! so SSD windows match CSD visual design.

use iced_core::Alignment;
use iced_core::{Element, Length};
use iced_widget::{Svg, button, container, row, svg, tooltip};
use icetron_p::prelude::{
    animated_opacity, animated_tooltip, app_header, halo_close_hover, header_height_for,
    header_render_height_for, measure_text_width, styled_text,
};
use icetron_p::{
    animation::transition::ButtonTransition,
    components::{draggable::draggable, icons::icon_svg_inherit},
};
use icetron_themes::icons;
use icetron_themes::{TextRole, WindowHeaderStyle};

use crate::comp_theme::CompTheme;
use crate::fl;
pub use crate::shell::element::window::runs::{HaloRun, RunState};

/// Fullscreen Halo sits 10 px inside the output in the prototype, excluding both
/// the raster's shadow padding and the widget's own top inset.
pub(crate) fn fullscreen_header_offset(theme: &CompTheme) -> f64 {
    10.0 - f64::from(halo_shadow_padding(theme).top + theme.halo_style().top_inset)
}

pub(crate) fn halo_intro_hold(theme: &CompTheme) -> std::time::Duration {
    std::time::Duration::from_millis(theme.halo_intro_hold().max(0.0).round() as u64)
}

pub(crate) fn halo_intro_fade(theme: &CompTheme) -> std::time::Duration {
    std::time::Duration::from_millis(theme.halo_intro_fade().max(0.0).round() as u64)
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
        kit: None,
    }
}

/// WindowFrame's focus draw on --ease-standard, over the theme's sweep; a theme
/// with no sweep, or zero durations, draws the outline at once.
pub(crate) fn halo_focus_outline(
    theme: &CompTheme,
    focused: bool,
    fullscreen: bool,
) -> crate::utils::iced::FocusOutline {
    crate::utils::iced::FocusOutline {
        focused,
        bottom_border: true,
        animate: !fullscreen && theme.duration_slower() > 0.0 && theme.window_focus_sweep() > 0.0,
        duration: std::time::Duration::from_millis(
            theme.window_focus_sweep().max(0.0).round() as u64
        ),
        curve: halo_visibility(theme, true).opacity_curve,
    }
}

/// A Halo floats above the window and reserves nothing inside it.
pub fn ssd_header_height(theme: &CompTheme) -> u32 {
    let style = theme.window_header_style();
    if style == WindowHeaderStyle::Halo {
        0
    } else {
        header_height_for(&**theme, style) as u32
    }
}

pub fn ssd_top_reserve(theme: &CompTheme) -> i32 {
    if uses_halo_header(theme) {
        halo_clearance(theme)
    } else {
        ssd_header_height(theme) as i32
    }
}

/// Height of the compositor chrome render layer.
pub fn ssd_header_render_height(theme: &CompTheme) -> u32 {
    let padding = halo_shadow_padding(theme);
    ssd_header_input_height(theme) + padding.top as u32 + padding.bottom as u32
}

pub fn ssd_header_input_height(theme: &CompTheme) -> u32 {
    header_render_height_for(&**theme, theme.window_header_style()) as u32
}

/// Render-only space for the pill shadow; it does not enlarge the drag region.
fn halo_shadow_padding(theme: &CompTheme) -> iced_core::Padding {
    let mut padding = iced_core::Padding::ZERO;
    if uses_halo_header(theme) {
        let shadow = icetron_p::prelude::halo_shadow();
        // A CSS blur reaches three sigmas, 1.5x its radius, as iced draws it.
        let reach = 1.5 * shadow.blur_radius.max(0.0) + shadow.spread_radius.max(0.0);
        padding.top = (reach - shadow.offset.y).ceil().max(0.0);
        padding.bottom = (reach + shadow.offset.y).ceil().max(0.0);
    }
    padding
}

/// Width of a row of controls that must never be squeezed: the widths they
/// already occupy, plus the gap between them.
fn rigid_width(content: f32, items: u32, gap: f32) -> f32 {
    content + gap * items.saturating_sub(1) as f32
}

/// Offset of the raster buffer, including shadow padding, above the client.
pub fn ssd_header_render_overhang(theme: &CompTheme) -> u32 {
    ssd_header_overhang(theme) + halo_shadow_padding(theme).top as u32
}

/// Distance Halo chrome renders above the client surface.
pub fn ssd_header_overhang(theme: &CompTheme) -> u32 {
    if uses_halo_header(theme) {
        theme.halo_style().overhang.ceil() as u32
    } else {
        0
    }
}

/// Room kept free above a Halo window: 4 above the pill, the 32px pill, 4 below.
pub fn halo_clearance(theme: &CompTheme) -> i32 {
    ssd_header_overhang(theme) as i32
}

/// Whether the active theme selects Halo chrome.
pub fn uses_halo_header(theme: &CompTheme) -> bool {
    theme.window_header_style() == WindowHeaderStyle::Halo
}

pub(crate) fn halo_pill_rows(theme: &CompTheme) -> (i32, i32) {
    let metrics = theme.halo_style();
    let overhang = ssd_header_overhang(theme) as i32;
    (
        metrics.top_inset.floor() as i32 - overhang,
        (metrics.top_inset + metrics.pill_height()).ceil() as i32 - overhang,
    )
}

pub(crate) fn halo_pill_bottom(theme: &CompTheme) -> i32 {
    halo_pill_rows(theme).1
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HaloBand {
    pub pill: Option<(i32, i32)>,
    pub rows: (i32, i32),
    pub strip: i32,
}

impl HaloBand {
    pub(crate) fn new(theme: &CompTheme, pill: Option<(i32, i32)>, overlay: bool) -> Self {
        Self {
            pill,
            rows: halo_pill_rows(theme),
            strip: if overlay {
                theme.halo_style().hot_zone_height.ceil() as i32
            } else {
                0
            },
        }
    }

    /// The pill, a full-width bridge in the gap below it, and an overlay client's drag strip.
    pub(crate) fn hit(&self, width: i32, x: i32, y: i32) -> bool {
        let (top, bottom) = self.rows;
        let on_pill = self
            .pill
            .is_some_and(|(left, right)| (left..right).contains(&x))
            && (top..bottom).contains(&y);
        on_pill || ((0..width).contains(&x) && (bottom.min(0)..self.strip).contains(&y))
    }
}

/// A fullscreen window's pill hangs from the output's top edge instead.
pub(crate) fn fullscreen_pill_bottom(theme: &CompTheme) -> i32 {
    let metrics = theme.halo_style();
    let pill_top = fullscreen_header_offset(theme)
        + f64::from(halo_shadow_padding(theme).top + metrics.top_inset);
    (pill_top + f64::from(metrics.pill_height())).ceil() as i32
}

pub(crate) fn halo_tier(width: f32, panel: bool) -> u8 {
    if panel {
        4
    } else if width >= 680.0 {
        1
    } else if width >= 480.0 {
        2
    } else if width >= 280.0 {
        3
    } else {
        4
    }
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
    /// Screen corners, not maximized: a maximized window in an inset zone keeps them.
    square_top: bool,
    compositor_outline: bool,
    window_width: Option<f32>,
    panel: bool,
    theme: Option<&'a CompTheme>,
    app_icon: Option<AppIcon>,
    run: Option<(HaloRun, Message)>,
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
            window_width: None,
            panel: false,
            theme: None,
            app_icon: None,
            run: None,
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

    pub fn window_width(mut self, width: f32) -> Self {
        self.window_width = Some(width);
        self
    }

    pub fn panel(mut self, panel: bool) -> Self {
        self.panel = panel;
        self
    }

    /// The window's most urgent run, and what a press on its chip sends.
    pub fn run(mut self, run: Option<HaloRun>, on_press: Message) -> Self {
        self.run = run.map(|run| (run, on_press));
        self
    }

    fn tier(&self) -> u8 {
        halo_tier(self.window_width.unwrap_or(f32::INFINITY), self.panel)
    }

    fn identity(&self) -> (&str, Option<&str>) {
        match self.app_name.as_deref().filter(|name| !name.is_empty()) {
            Some(name) => (
                name,
                Some(self.title.as_str()).filter(|title| !title.is_empty() && *title != name),
            ),
            None => (self.title.as_str(), None),
        }
    }

    /// Convert to an iced Element using icetron's app_header.
    pub fn into_element(self) -> Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer> {
        let theme = self.theme.expect("HeaderBar requires .theme()");
        if uses_halo_header(theme) {
            return self.into_halo_element(theme);
        }
        let window_header_style = theme.window_header_style();

        let mut header = app_header(&**theme)
            .window_header_style(window_header_style)
            .title(Some(&self.title))
            .focused(self.focused)
            .hovered(self.hovered || self.focused)
            .is_windowed(!self.maximized)
            .backdrop_blur(false)
            .opaque(true)
            .show_border(true);
        if let Some(name) = self.app_name.as_deref() {
            header = header.app_name(name.to_owned());
        }

        // Pass application icon with native SVG colors via title_content.
        // We wrap in animated_opacity to replicate app_header's title fade behavior
        // (0.8 when unfocused, animates to 1.0 on hover/focus).
        {
            let title_style = theme.header_title_text_style();
            let title_color = theme.header_title_color();
            let icon_size = theme.ui_size_icon_sm();
            let text_element: Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer> =
                styled_text(&self.title, title_style, title_color)
                    .wrapping(iced_widget::text::Wrapping::None)
                    .ellipsis(iced_widget::text::Ellipsis::End)
                    .into();
            let title_row: Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer> =
                match self.app_icon.as_ref() {
                    Some(icon) => row![app_mark(icon, icon_size, title_color), text_element]
                        .spacing(theme.header_title_gap())
                        .align_y(Alignment::Center)
                        .into(),
                    // No icon yet (async resolution pending) — show title only
                    None => text_element,
                };
            let title_content: Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer> =
                if self.hovered || self.focused {
                    title_row
                } else {
                    animated_opacity(title_row, &**theme, 0.8).into()
                };
            header = header.title_content(title_content);
        }

        if let Some(msg) = self.on_drag.clone() {
            header = header.on_drag(msg);
        }
        if let Some(msg) = self.on_close {
            header = header.on_close(msg);
        }
        if let Some(msg) = self.on_minimize {
            header = header.on_minimize(msg);
        }
        // app_header uses on_toggle_window for maximize/unmaximize
        if let Some(msg) = self.on_maximize.clone() {
            header = header.on_toggle_window(msg);
        }
        if let Some(msg) = self.on_right_click.clone() {
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

    fn into_halo_element(
        self,
        theme: &'a CompTheme,
    ) -> Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer> {
        let metrics = theme.halo_style();
        let tier = self.tier();
        let chrome_theme = if self.compositor_outline {
            theme.halo_chrome_theme()
        } else {
            &**theme
        };
        let mut header = app_header(chrome_theme)
            .window_header_style(WindowHeaderStyle::Halo)
            .title(Some(&self.title))
            .focused(self.focused)
            .hovered(true)
            .is_windowed(!self.maximized)
            .backdrop_blur(true)
            .opaque(true)
            .show_border(true)
            .halo_divider(tier < 4)
            .menu_open(self.menu_open);
        if self.compositor_outline {
            // Icetron mixes the edge from border and accent, so the accent goes too.
            header = header.accent(iced_core::Color::TRANSPARENT);
        }

        let (name, selection) = self.identity();
        let name = name.to_owned();
        let selection = selection.map(str::to_owned);

        // The glyph is the route to the commands, so a missing mark falls back.
        let app_icon = self.app_icon.clone().unwrap_or(AppIcon::Svg {
            bytes: icons::APP_WINDOW.bytes,
            symbolic: true,
        });
        let glyph = halo_glyph(
            app_mark(&app_icon, metrics.glyph_icon_size, theme.halo_accent()),
            self.on_commands.clone(),
            self.commands_open,
            self.menu_open || self.commands_open,
            fl!("halo-commands-hint", app = name.as_str()),
            self.run
                .as_ref()
                .filter(|_| tier >= 3)
                .map(|(run, _)| run.state),
            theme,
        );

        let mut name_style = theme.text_styles().role(TextRole::Label);
        name_style.font_weight = metrics.title_font_weight;
        let name_text = container(
            styled_text(name.clone(), name_style, theme.text_primary())
                .wrapping(iced_widget::text::Wrapping::None)
                .ellipsis(iced_widget::text::Ellipsis::End),
        )
        .max_width(HALO_NAME_MAX);
        let identity_gap = theme.spacing_2();
        let mut identity = crate::utils::iced::ElasticRow::new()
            .spacing(metrics.gap)
            .reserve(self.halo_trailing_width(theme, tier))
            .reserve_min(2.0 * metrics.control_size + metrics.border_width + 3.0 * metrics.gap)
            .floor(name_style.font_size * 6.0)
            .push_rigid(glyph);
        identity = if tier == 4 {
            identity.push_elastic(name_text)
        } else {
            identity.push_rigid(name_text)
        };
        let name_width = measure_text_width(&name, &name_style)
            .ceil()
            .min(HALO_NAME_MAX);
        let selection_max = HALO_SELECTION_MAX.min(HALO_IDENTITY_MAX - identity_gap - name_width);
        if let Some(selection) = selection.as_deref().filter(|_| selection_max > 0.0) {
            identity = identity.push_elastic_droppable(
                1,
                container(
                    styled_text(
                        selection.to_owned(),
                        theme.text_styles().role(TextRole::Caption),
                        theme.text_secondary(),
                    )
                    .wrapping(iced_widget::text::Wrapping::None)
                    .ellipsis(iced_widget::text::Ellipsis::End),
                )
                .max_width(selection_max + identity_gap - metrics.gap)
                .padding(iced_core::Padding {
                    left: identity_gap - metrics.gap,
                    ..iced_core::Padding::ZERO
                }),
            );
        }
        if let Some((run, message)) = self.run.as_ref().filter(|_| tier < 4) {
            let chip = halo_chip(run, message.clone(), tier >= 3, self.menu_open, theme);
            identity = identity.push_rigid(chip);
        }
        let identity: Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer> =
            identity.into_single().unwrap_or_else(Into::into);
        let identity_tip = match selection.as_deref() {
            Some(selection) => format!("{name} — {selection}"),
            None => name.clone(),
        };
        let identity = animated_tooltip(identity, identity_tip, &**theme)
            .position(tooltip::Position::Bottom)
            .enabled(!(self.menu_open || self.commands_open))
            .animation_duration(std::time::Duration::ZERO)
            .compositor_managed(true);
        header = header.title_content(identity);

        let mut trailing = row![].spacing(metrics.gap).align_y(Alignment::Center);
        let pins: Vec<_> = self
            .tray
            .iter()
            .filter(|entry| tier == 1 || entry.on)
            .collect();
        if !pins.is_empty() {
            let pinned = row(pins.into_iter().map(|entry| {
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
            if tier == 1 {
                trailing = trailing.push(halo_divider(theme));
            }
            trailing = trailing.push(pinned);
        }
        if tier <= 2 {
            if let Some(message) = self.on_right_click.clone() {
                trailing = trailing.push(halo_button(
                    icons::CHEVRON_DOWN,
                    Some(message),
                    fl!("halo-app-menu", app = name.as_str()),
                    HaloButtonRole::Menu,
                    self.menu_open,
                    theme,
                ));
            }
            if let Some(message) = self.on_new_window.clone() {
                trailing = trailing.push(halo_button(
                    icons::PLUS,
                    Some(message),
                    fl!("halo-new-window", app = name.as_str()),
                    HaloButtonRole::Window,
                    self.menu_open,
                    theme,
                ));
            }
        }
        header = header.trailing(trailing);

        let mut actions = row![].spacing(metrics.gap).align_y(Alignment::Center);
        if tier >= 3 {
            if let Some(message) = self.on_commands.clone() {
                actions = actions.push(halo_button(
                    icons::MORE_HORIZONTAL,
                    Some(message),
                    fl!("halo-all-commands"),
                    HaloButtonRole::Window,
                    self.menu_open,
                    theme,
                ));
            }
        } else {
            for (icon, message, label) in [
                (icons::MINUS, self.on_minimize.clone(), fl!("halo-park")),
                (
                    icons::MAXIMIZE_2,
                    self.on_maximize.clone(),
                    fl!("halo-fill"),
                ),
                (
                    if self.fullscreen {
                        icons::SHRINK
                    } else {
                        icons::FULLSCREEN
                    },
                    self.on_fullscreen.clone(),
                    if self.fullscreen {
                        fl!("halo-exit-fullscreen")
                    } else {
                        fl!("halo-fullscreen")
                    },
                ),
            ] {
                if let Some(message) = message {
                    actions = actions.push(halo_button(
                        icon,
                        Some(message),
                        label,
                        HaloButtonRole::Window,
                        self.menu_open,
                        theme,
                    ));
                }
            }
        }
        if let Some(message) = self.on_close.clone() {
            actions = actions.push(halo_button(
                icons::X,
                Some(message),
                fl!("halo-close"),
                HaloButtonRole::Close,
                self.menu_open,
                theme,
            ));
        }
        header = header.action_buttons(actions);

        // Own every gesture here; AppHeader's drag area would consume them.
        let header_elem: Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer> =
            header.into();
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
        container(gestures)
            .width(Length::Fill)
            .height(Length::Fixed(ssd_header_render_height(theme) as f32))
            .padding(halo_shadow_padding(theme))
            .into()
    }

    fn halo_trailing_width(&self, theme: &CompTheme, tier: u8) -> f32 {
        let metrics = theme.halo_style();
        let pins = self
            .tray
            .iter()
            .filter(|entry| tier == 1 || entry.on)
            .count() as f32;
        let mut items = 0_u32;
        let mut width = 0.0_f32;
        if pins > 0.0 {
            width += pins * metrics.control_size + (pins - 1.0) * theme.spacing_0_5();
            items += 1;
            if tier == 1 {
                width += metrics.border_width;
                items += 1;
            }
        }
        let mut controls = |present: bool| {
            if present {
                width += metrics.control_size;
                items += 1;
            }
        };
        if tier <= 2 {
            controls(self.on_right_click.is_some());
            controls(self.on_new_window.is_some());
            controls(self.on_minimize.is_some());
            controls(self.on_maximize.is_some());
            controls(self.on_fullscreen.is_some());
        } else {
            controls(self.on_commands.is_some());
        }
        controls(self.on_close.is_some());
        if tier < 4 {
            width += metrics.border_width;
            items += 1;
        }
        rigid_width(width, items, metrics.gap)
    }
}

const HALO_NAME_MAX: f32 = 144.0;
const HALO_SELECTION_MAX: f32 = 128.0;
const HALO_IDENTITY_MAX: f32 = 192.0;

/// The prototype's "on" mark (`.kora-halo__tray-item--on::after` in
/// `halo.css`): a 5px dot 1px in from the corner, glowing 5px. No token
/// carries these, so they are literal.
const RECORD_DOT_PX: f32 = 5.0;
const RECORD_DOT_INSET_PX: f32 = 1.0;

fn app_mark<'a, Message: 'a>(
    icon: &AppIcon,
    size: f32,
    tint: iced_core::Color,
) -> Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer> {
    match icon {
        AppIcon::Svg { bytes, symbolic } => {
            // A transparent tint opts out of the colour filter; None would inherit the text colour.
            let tint = if *symbolic {
                tint
            } else {
                iced_core::Color::TRANSPARENT
            };
            Svg::new(iced_core::svg::Handle::from_memory(*bytes))
                .width(size)
                .height(size)
                .style(move |_theme, _status| svg::Style { color: Some(tint) })
                .into()
        }
        AppIcon::Image(handle) => iced_widget::image::Image::new(handle.clone())
            .width(size)
            .height(size)
            .into(),
    }
}

fn halo_divider<'a, Message: 'a>(
    theme: &'a CompTheme,
) -> Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer> {
    let metrics = theme.halo_style();
    let color = theme.stroke_subtler();
    container(iced_widget::Space::new())
        .width(metrics.border_width)
        .height(metrics.divider_height)
        .style(move |_| container::Style {
            background: Some(iced_core::Background::Color(color)),
            ..Default::default()
        })
        .into()
}

fn halo_glyph<'a, Message: Clone + 'static>(
    mark: Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer>,
    on_press: Option<Message>,
    active: bool,
    surface_open: bool,
    hint: String,
    status: Option<RunState>,
    theme: &'a CompTheme,
) -> Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer> {
    let size = theme.halo_style().control_size;
    let radius = theme.radii_max();
    let mut content = container(mark)
        .align_x(Alignment::Center)
        .align_y(Alignment::Center);
    if let Some(state) = status {
        // The prototype's `.kora-halo__sdot`: 6px, 1px outside the glyph's corner.
        let dot = run_dot(state, HALO_STATUS_DOT_PX, theme);
        content = container(iced_widget::stack![
            content.width(Length::Fill).height(Length::Fill),
            iced_widget::pin(dot)
                .x(size - HALO_STATUS_DOT_PX + 1.0)
                .y(-1.0),
        ]);
    }
    let Some(message) = on_press else {
        return content
            .width(Length::Fixed(size))
            .height(Length::Fixed(size))
            .into();
    };
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
                background: None,
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
    animated_tooltip(button, hint, &**theme)
        .position(tooltip::Position::Bottom)
        .enabled(!surface_open)
        .animation_duration(std::time::Duration::ZERO)
        .compositor_managed(true)
        .into()
}

/// The run chip of the prototype's `HaloBar.tsx`: a pulsing dot and the
/// run's words; a bare dot at tier 3, where the verbs fold behind ⋯.
fn halo_chip<'a, Message: Clone + 'static>(
    run: &HaloRun,
    on_press: Message,
    bare: bool,
    menu_open: bool,
    theme: &'a CompTheme,
) -> Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer> {
    let size = theme.halo_style().control_size;
    let color = run_color(run.state, theme);
    let dot = run_dot(run.state, HALO_CHIP_DOT_PX, theme);
    let content: Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer> = if bare {
        container(dot).center(Length::Fixed(size)).into()
    } else {
        let mut style = theme.text_styles().role(TextRole::Caption);
        style.font_weight = 600;
        container(
            row![
                dot,
                styled_text(run.chip_text(), style, color)
                    .wrapping(iced_widget::text::Wrapping::None),
            ]
            .spacing(HALO_CHIP_GAP)
            .align_y(Alignment::Center),
        )
        .height(Length::Fixed(size))
        .padding([0.0, theme.spacing_2()])
        .align_y(Alignment::Center)
        .into()
    };
    let chip = button(content)
        .on_press(on_press)
        .padding(0)
        .height(Length::Fixed(size))
        .style(|_, _| button::Style::default());
    animated_tooltip(chip, run.tooltip(), &**theme)
        .position(tooltip::Position::Bottom)
        .enabled(!menu_open)
        .animation_duration(std::time::Duration::ZERO)
        .compositor_managed(true)
        .into()
}

/// Violet while it runs, indigo while it waits its turn, green when done.
fn run_color(state: RunState, theme: &CompTheme) -> iced_core::Color {
    match state {
        RunState::Running => theme.ai_strong(),
        RunState::Queued => theme.color_queued(),
        RunState::Done => theme.feedback_success_primary(),
    }
}

/// A queued run's dot holds still: nothing is being spent on it yet.
fn run_dot(
    state: RunState,
    diameter: f32,
    theme: &CompTheme,
) -> crate::utils::iced::pulse::PulsingDot {
    crate::utils::iced::pulse::PulsingDot::new(
        diameter,
        run_color(state, theme),
        0.0,
        theme.motion.ease_standard_cp,
    )
    .period(HALO_RUN_PULSE)
    .still(theme.reduced_motion || state == RunState::Queued)
}

const HALO_CHIP_DOT_PX: f32 = 4.0;
const HALO_CHIP_GAP: f32 = 5.0;
const HALO_STATUS_DOT_PX: f32 = 6.0;
const HALO_RUN_PULSE: std::time::Duration = std::time::Duration::from_millis(1800);

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
    let text_color = theme.text_secondary();
    let background = theme.overlay_5();
    let close_hover = halo_close_hover(&**theme);
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
                        close_hover
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
pub(crate) fn design_halo_style() -> icetron_themes::HaloStyle {
    icetron_themes::HaloStyle {
        overhang: 40.0,
        hot_zone_height: 16.0,
        top_inset: 4.0,
        horizontal_overhang: 0.0,
        padding_horizontal: 3.0,
        padding_vertical: 1.5,
        gap: 4.0,
        control_size: 28.0,
        control_icon_size: 14.0,
        menu_icon_size: 12.0,
        glyph_icon_size: 14.0,
        title_font_weight: 500,
        divider_height: 12.0,
        border_width: 0.5,
        ..icetron_themes::HaloStyle::default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use icetron_themes::dynamic::DEFAULT_THEME_PAIR;
    use std::sync::Arc;

    fn halo_theme() -> CompTheme {
        let mut theme = DEFAULT_THEME_PAIR.load(false);
        theme.window_header_style = WindowHeaderStyle::Halo;
        theme.halo_style = design_halo_style();
        CompTheme::new(Arc::new(theme), false)
    }

    #[test]
    fn every_halo_floats_its_pill_4px_above_the_window() {
        let theme = halo_theme();
        assert_eq!(halo_pill_rows(&theme), (-36, -4));
        assert_eq!(halo_pill_bottom(&theme), -4);
        assert_eq!(halo_clearance(&theme), 40);
        assert_eq!(ssd_header_overhang(&theme), 40);
        assert_eq!(ssd_top_reserve(&theme), 40);
        assert_eq!(ssd_header_height(&theme), 0);
        assert_eq!(ssd_header_input_height(&theme), 56);
        let padding = halo_shadow_padding(&theme);
        assert_eq!((padding.top, padding.bottom), (17.0, 25.0));
        assert_eq!(ssd_header_render_overhang(&theme), 57);
        assert_eq!(ssd_header_render_height(&theme), 56 + 17 + 25);
    }

    #[test]
    fn a_bar_keeps_its_band_inside_the_window() {
        let mut tokens = DEFAULT_THEME_PAIR.load(false);
        tokens.window_header_style = WindowHeaderStyle::Bar;
        let theme = CompTheme::new(Arc::new(tokens), false);
        assert!(ssd_header_height(&theme) > 0);
        assert_eq!(ssd_top_reserve(&theme), ssd_header_height(&theme) as i32);
        assert_eq!(halo_clearance(&theme), 0);
    }

    #[test]
    fn the_band_takes_input_on_the_pill_the_bridge_and_an_overlay_strip() {
        let theme = halo_theme();
        let width = 800;
        let pill = Some((300, 500));
        for overlay in [false, true] {
            let band = HaloBand::new(&theme, pill, overlay);
            let hit = |x, y| band.hit(width, x, y);
            let case = format!("overlay={overlay}");
            for (x, y) in [(300, -36), (499, -36), (400, -20), (300, -5)] {
                assert!(hit(x, y), "pill at ({x}, {y}) {case}");
            }
            for (x, y) in [
                (299, -20),
                (500, -20),
                (0, -36),
                (799, -10),
                (400, -37),
                (400, -40),
            ] {
                assert!(!hit(x, y), "see-through at ({x}, {y}) {case}");
            }
            for x in [0, 150, 799] {
                assert!(hit(x, -4) && hit(x, -1), "bridge at {x} {case}");
            }
            assert!(
                !hit(-1, -2) && !hit(800, -2),
                "the bridge ends at the window {case}"
            );
            assert_eq!(hit(10, 0), overlay, "{case}");
            assert_eq!(hit(10, 15), overlay, "{case}");
            assert!(!hit(10, 16), "{case}");
        }
        let band = HaloBand::new(&theme, None, false);
        assert!(!band.hit(width, 400, -20));
        assert!(band.hit(width, 400, -2));
    }

    #[test]
    fn the_overflow_tier_follows_the_design_thresholds() {
        for (width, tier) in [
            (2400.0, 1),
            (680.0, 1),
            (679.9, 2),
            (480.0, 2),
            (479.0, 3),
            (280.0, 3),
            (279.0, 4),
            (0.0, 4),
        ] {
            assert_eq!(halo_tier(width, false), tier, "{width}px");
        }
        assert_eq!(
            halo_tier(2400.0, true),
            4,
            "a secondary panel is always tier 4"
        );
    }

    #[test]
    fn a_fullscreen_pill_hangs_10px_inside_the_output() {
        let theme = halo_theme();
        let padding = halo_shadow_padding(&theme);
        assert_eq!(
            fullscreen_header_offset(&theme)
                + f64::from(padding.top + theme.halo_style().top_inset),
            10.0
        );
        assert_eq!(fullscreen_pill_bottom(&theme), 42);
    }

    #[test]
    fn a_hidden_halo_rests_3px_low() {
        let theme = halo_theme();
        for visible in [false, true] {
            let visibility = halo_visibility(&theme, visible);
            assert_eq!(visibility.hidden_offset, iced_core::Vector::new(0.0, 3.0));
            assert_eq!(visibility.hidden_scale, 1.0);
        }
    }
}
