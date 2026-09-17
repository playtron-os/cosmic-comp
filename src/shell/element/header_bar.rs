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

/// Builder for the compositor SSD header bar.
pub struct HeaderBar<'a, Message> {
    title: String,
    app_name: Option<String>,
    on_drag: Option<Message>,
    on_close: Option<Message>,
    on_minimize: Option<Message>,
    on_maximize: Option<Message>,
    on_right_click: Option<Message>,
    on_screenshot: Option<Message>,
    on_record: Option<Message>,
    recording: bool,
    on_new_window: Option<Message>,
    on_fullscreen: Option<Message>,
    fullscreen: bool,
    menu_open: bool,
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
            on_screenshot: None,
            on_record: None,
            recording: false,
            on_new_window: None,
            on_fullscreen: None,
            fullscreen: false,
            menu_open: false,
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

    pub fn on_screenshot(mut self, msg: Message) -> Self {
        self.on_screenshot = Some(msg);
        self
    }

    pub fn on_record(mut self, msg: Message) -> Self {
        self.on_record = Some(msg);
        self
    }

    /// Whether the window is being recorded: the Record glyph turns
    /// destructive and carries a dot, and its tooltip offers to stop.
    pub fn recording(mut self, recording: bool) -> Self {
        self.recording = recording;
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

    /// Convert to an iced Element using icetron's app_header.
    pub fn into_element(self) -> Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer> {
        let theme = self.theme.expect("HeaderBar requires .theme()");
        let window_header_style = theme.window_header_style();

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
        if let Some(name) = self.app_name {
            header = header.app_name(name);
        }

        // Pass application icon with native SVG colors via title_content.
        // We wrap in animated_opacity to replicate app_header's title fade behavior
        // (0.8 when unfocused, animates to 1.0 on hover/focus).
        {
            let (title_style, title_color, title_gap, icon_size, glyph_size) =
                if uses_halo_header(theme) {
                    let mut style = theme.text_styles().caption();
                    let halo = theme.halo_style();
                    style.font_weight = halo.title_font_weight;
                    (
                        style,
                        theme.text_primary(),
                        halo.gap,
                        halo.control_size,
                        halo.glyph_icon_size,
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

            let title_row: Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer> =
                if let Some(ref icon) = self.app_icon {
                    let icon_element: Element<
                        'a,
                        Message,
                        iced_core::Theme,
                        iced_tiny_skia::Renderer,
                    > = match icon {
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
                    let icon_element: Element<
                        'a,
                        Message,
                        iced_core::Theme,
                        iced_tiny_skia::Renderer,
                    > = if uses_halo_header(theme) {
                        let bg = theme.halo_accent_background();
                        let radius = theme.radii_max();
                        container(icon_element)
                            .width(Length::Fixed(icon_size))
                            .height(Length::Fixed(icon_size))
                            .align_x(Alignment::Center)
                            .align_y(Alignment::Center)
                            .style(move |_theme| container::Style {
                                background: Some(iced_core::Background::Color(bg)),
                                border: iced_core::Border::default().rounded(radius),
                                ..Default::default()
                            })
                            .into()
                    } else {
                        icon_element
                    };
                    row![icon_element, text_element]
                        .spacing(title_gap)
                        .align_y(Alignment::Center)
                        .into()
                } else {
                    // No icon yet (async resolution pending) — show title only
                    text_element
                };

            // The Halo animates as one pill; bar headers animate just the title.
            let target_opacity = if uses_halo_header(theme) || self.hovered || self.focused {
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

        let halo = uses_halo_header(theme);
        if halo {
            let metrics = theme.halo_style();
            let capture = row![
                halo_button(
                    icons::CAMERA,
                    self.on_screenshot.clone(),
                    fl!("halo-screenshot-window"),
                    HaloButtonRole::Tray { on: false },
                    self.menu_open,
                    theme
                ),
                halo_button(
                    icons::CIRCLE,
                    self.on_record.clone(),
                    if self.recording {
                        fl!("window-menu-stop-recording")
                    } else {
                        fl!("window-menu-record")
                    },
                    HaloButtonRole::Tray { on: self.recording },
                    self.menu_open,
                    theme
                ),
            ]
            .spacing(theme.spacing_0_5());
            let divider = container(iced_widget::Space::new())
                .width(metrics.border_width)
                .height(metrics.divider_height)
                .style(move |_| container::Style {
                    background: Some(iced_core::Background::Color(theme.stroke_subtle())),
                    ..Default::default()
                });
            let mut tray = row![divider, capture]
                .spacing(metrics.gap)
                .align_y(Alignment::Center);
            if let Some(message) = self.on_right_click.clone() {
                tray = tray.push(halo_button(
                    icons::CHEVRON_DOWN,
                    Some(message),
                    fl!("halo-window-menu"),
                    HaloButtonRole::Menu,
                    self.menu_open,
                    theme,
                ));
            }
            if let Some(message) = self.on_new_window.clone() {
                tray = tray.push(halo_button(
                    icons::PLUS,
                    Some(message),
                    fl!("window-menu-new-window"),
                    HaloButtonRole::Window,
                    self.menu_open,
                    theme,
                ));
            }
            let mut actions = row![].spacing(metrics.gap);
            let restore = self.maximized || self.fullscreen;
            for (icon, message, label, destructive) in [
                (
                    icons::MINUS,
                    self.on_minimize.clone(),
                    fl!("window-menu-minimize"),
                    false,
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
                ),
                (
                    icons::X,
                    self.on_close.clone(),
                    fl!("window-menu-close"),
                    true,
                ),
            ] {
                if let Some(message) = message {
                    actions = actions.push(halo_button(
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
                    ));
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
            return container(header_elem)
                .width(Length::Fill)
                .height(Length::Fixed(ssd_header_render_height(theme) as f32))
                .padding(halo_shadow_padding(theme))
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
