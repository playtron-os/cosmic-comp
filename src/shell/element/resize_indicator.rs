use std::sync::Mutex;

use crate::{
    comp_theme::CompTheme,
    config::Config,
    fl,
    shell::grabs::ResizeEdge,
    utils::{
        apply::Apply,
        iced::{IcedElement, Program},
        xdg_icon::named_icon,
    },
};

use calloop::LoopHandle;
use cosmic_settings_config::shortcuts::action::{Action, ResizeDirection};
use iced_core::{Alignment, Background, Border, Color, Element, Length};
use iced_widget::container::Container;
use iced_widget::{Space, column, container, row};
use icetron::prelude::styled_text;
use smithay::utils::Size;

use crate::utils::iced::CompElement;

/// Helper to create a container with concrete types (avoids type inference issues).
fn comp_container<'a, Message: 'a>(
    content: impl Into<Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer>>,
) -> Container<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer> {
    Container::new(content)
}

/// Helper to create a Space element with concrete types.
fn space_element<'a>(
    width: impl Into<Length>,
    height: impl Into<Length>,
) -> Element<'a, (), iced_core::Theme, iced_tiny_skia::Renderer> {
    Space::new().width(width).height(height).into()
}

/// Helper to convert a Container into an Element with concrete types.
fn into_element<'a>(
    c: Container<'a, (), iced_core::Theme, iced_tiny_skia::Renderer>,
) -> Element<'a, (), iced_core::Theme, iced_tiny_skia::Renderer> {
    c.into()
}

pub type ResizeIndicator = IcedElement<ResizeIndicatorInternal>;

pub fn resize_indicator(
    direction: ResizeDirection,
    config: &Config,
    evlh: LoopHandle<'static, crate::state::State>,
    theme: CompTheme,
) -> ResizeIndicator {
    ResizeIndicator::new(
        ResizeIndicatorInternal {
            edges: Mutex::new(ResizeEdge::all()),
            direction,
            shortcut1: config
                .shortcuts
                .iter()
                .find_map(|(pattern, action)| {
                    (*action == Action::Resizing(ResizeDirection::Outwards)).then_some(pattern)
                })
                .map(|pattern| format!("{}: ", pattern.to_string()))
                .unwrap_or_else(|| crate::fl!("unknown-keybinding")),
            shortcut2: config
                .shortcuts
                .iter()
                .find_map(|(pattern, action)| {
                    (*action == Action::Resizing(ResizeDirection::Inwards)).then_some(pattern)
                })
                .map(|pattern| format!("{}: ", pattern.to_string()))
                .unwrap_or_else(|| crate::fl!("unknown-keybinding")),
        },
        Size::from((1, 1)),
        evlh,
        theme,
    )
}

pub struct ResizeIndicatorInternal {
    pub edges: Mutex<ResizeEdge>,
    pub direction: ResizeDirection,
    pub shortcut1: String,
    pub shortcut2: String,
}

/// Accent-colored container style for icon badges.
fn accent_container_class(theme: &CompTheme) -> Box<dyn Fn(&iced_core::Theme) -> container::Style> {
    let on_accent = theme.on_accent_color();
    let accent = theme.accent_color();
    Box::new(move |_: &iced_core::Theme| container::Style {
        text_color: Some(on_accent),
        background: Some(Background::Color(accent)),
        border: Border {
            radius: 18.0.into(),
            width: 0.0,
            color: Color::TRANSPARENT,
        },
        shadow: Default::default(),
        snap: false,
        border_only: false,
    })
}

impl Program for ResizeIndicatorInternal {
    type Message = ();

    fn view<'a>(&'a self, theme: &'a CompTheme) -> CompElement<'a, Self::Message> {
        let edges = self.edges.lock().unwrap();
        let on_accent = theme.on_accent_color();

        column![
            if edges.contains(ResizeEdge::TOP) {
                named_icon(
                    if self.direction == ResizeDirection::Outwards {
                        "go-up-symbolic"
                    } else {
                        "go-down-symbolic"
                    },
                    20.0,
                )
                .apply(comp_container)
                .padding(8)
                .class(accent_container_class(theme))
                .width(Length::Shrink)
                .apply(comp_container)
                .center_x(Length::Fill)
                .apply(into_element)
            } else {
                space_element(Length::Shrink, 36.0)
            },
            row![
                if edges.contains(ResizeEdge::LEFT) {
                    named_icon(
                        if self.direction == ResizeDirection::Outwards {
                            "go-previous-symbolic"
                        } else {
                            "go-next-symbolic"
                        },
                        20.0,
                    )
                    .apply(comp_container)
                    .padding(8)
                    .class(accent_container_class(theme))
                    .width(Length::Shrink)
                    .apply(comp_container)
                    .center_y(Length::Fill)
                    .apply(into_element)
                } else {
                    space_element(36.0, Length::Shrink)
                },
                row![
                    styled_text(
                        &self.shortcut1,
                        theme.text_styles().label_default_strong(),
                        on_accent,
                    ),
                    styled_text(fl!("grow-window"), theme.text_styles().body(), on_accent),
                    Space::new().width(40.0).height(Length::Shrink),
                    styled_text(
                        &self.shortcut2,
                        theme.text_styles().label_default_strong(),
                        on_accent,
                    ),
                    styled_text(fl!("shrink-window"), theme.text_styles().body(), on_accent),
                ]
                .apply(comp_container)
                .align_x(Alignment::Center)
                .align_y(Alignment::Center)
                .padding(16)
                .apply(comp_container)
                .class(accent_container_class(theme))
                .width(Length::Shrink)
                .height(Length::Shrink)
                .apply(comp_container)
                .center_x(Length::Fill)
                .center_y(Length::Fill)
                .apply(into_element),
                if edges.contains(ResizeEdge::RIGHT) {
                    named_icon(
                        if self.direction == ResizeDirection::Outwards {
                            "go-next-symbolic"
                        } else {
                            "go-previous-symbolic"
                        },
                        20.0,
                    )
                    .apply(comp_container)
                    .padding(8)
                    .class(accent_container_class(theme))
                    .height(Length::Shrink)
                    .apply(comp_container)
                    .center_y(Length::Fill)
                    .apply(into_element)
                } else {
                    space_element(36.0, Length::Shrink)
                },
            ]
            .width(Length::Fill)
            .height(Length::Fill)
            .apply(|r| -> Element<'_, (), iced_core::Theme, iced_tiny_skia::Renderer> { r.into() }),
            if edges.contains(ResizeEdge::BOTTOM) {
                named_icon(
                    if self.direction == ResizeDirection::Outwards {
                        "go-down-symbolic"
                    } else {
                        "go-up-symbolic"
                    },
                    20.0,
                )
                .apply(comp_container)
                .padding(8)
                .class(accent_container_class(theme))
                .width(Length::Shrink)
                .apply(comp_container)
                .center_x(Length::Fill)
                .apply(into_element)
            } else {
                space_element(Length::Shrink, 36.0)
            },
        ]
        .into()
    }
}
