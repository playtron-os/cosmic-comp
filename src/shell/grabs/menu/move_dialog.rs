//! The Move to Desktop chooser: the design's Modal, drawn by the shell.

use iced_core::{Alignment, Background, Border, Length, Padding};
use iced_widget::{Column, Row, Space, column, container, row};
use icetron_p::components::buttons::{Button, ButtonSize, ButtonType};
use icetron_p::components::divider::rule;
use icetron_p::components::focus_ring::focus_ring;
use icetron_p::components::shadow::with_elevation_shadow;
use icetron_p::prelude::{icon_svg, styled_text};
use icetron_themes::{TextRole, icons};

use super::Message;
use crate::{comp_theme::CompTheme, utils::iced::CompElement};

/// The Modal's `md` width.
pub const WIDTH: f32 = 420.0;

/// One destination row, index-aligned with the menu's items.
#[derive(Debug, Clone, PartialEq)]
pub struct MoveRow {
    pub label: String,
    /// "Current desktop" or the window count; none for "New desktop".
    pub detail: Option<String>,
    pub current: bool,
}

/// What the keyboard can stand on, in Tab order.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Target {
    Close,
    Row(usize),
    Cancel,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MoveDialog {
    /// The app and its window, as "App · title".
    pub identity: String,
    /// Where the window is now.
    pub source: String,
    /// The desktops, then "New desktop" last.
    pub rows: Vec<MoveRow>,
    /// Index into [`Self::targets`]; the Modal starts on its close button.
    pub focus: usize,
}

impl MoveDialog {
    pub fn targets(&self) -> Vec<Target> {
        std::iter::once(Target::Close)
            .chain(
                self.rows
                    .iter()
                    .enumerate()
                    .filter(|(_, row)| !row.current)
                    .map(|(idx, _)| Target::Row(idx)),
            )
            .chain([Target::Cancel])
            .collect()
    }

    pub fn focused(&self) -> Target {
        let targets = self.targets();
        targets[self.focus.min(targets.len() - 1)]
    }

    /// Tab or Shift+Tab, wrapping as the design's focus trap does.
    pub fn step(&mut self, forward: bool) {
        let count = self.targets().len();
        self.focus = (self.focus + if forward { 1 } else { count - 1 }) % count;
    }
}

/// Room the card leaves around itself for its shadow.
pub fn padding(theme: &CompTheme) -> Padding {
    super::shadow_padding(&theme.shadow_modal())
}

pub fn view<'a>(dialog: &'a MoveDialog, theme: &'a CompTheme) -> CompElement<'a, Message> {
    let label = theme.text_styles().role(TextRole::Label);
    let caption = theme.text_styles().role(TextRole::Caption);
    let hairline = theme.border_width_hairline();
    let divider = || rule(Length::Fill, hairline, theme.dropdown_divider());
    let focused = dialog.focused();
    let ring = |target: Target, button: Button<'a, Message>| -> CompElement<'a, Message> {
        focus_ring(button, &**theme)
            .radius(theme.radii_sm())
            .force_shown(focused == target && icetron_themes::get_focus_visible())
            .into()
    };

    let close = Button::new("", &**theme)
        .button_type(ButtonType::Ghost)
        .size(ButtonSize::Small)
        .content(icon_svg(icons::X, 12.0, theme.text_secondary()))
        .width(Length::Fixed(theme.btn_height_sm()))
        .padding((theme.btn_height_sm() - caption_line(theme)) / 2.0, 0.0)
        .center_content(true)
        .on_press(Message::Dismiss);
    let header = row![
        styled_text(
            crate::fl!("move-desktop-title"),
            label,
            theme.text_primary()
        )
        .width(Length::Fill),
        ring(Target::Close, close),
    ]
    .align_y(Alignment::Center)
    .spacing(theme.spacing_3())
    .padding(Padding::from([theme.spacing_3_5(), theme.spacing_5()]));

    let mut rows = Column::new().width(Length::Fill);
    let last = dialog.rows.len().saturating_sub(1);
    for (idx, entry) in dialog.rows.iter().enumerate() {
        let button = if idx == last {
            new_desktop(entry, idx, theme)
        } else {
            desktop(entry, idx, theme)
        };
        rows = rows.push(ring(Target::Row(idx), button));
    }
    let body = column![
        styled_text(dialog.identity.clone(), label, theme.text_primary())
            .wrapping(iced_core::text::Wrapping::None)
            .width(Length::Fill),
        Space::new().height(theme.spacing_1()),
        styled_text(dialog.source.clone(), caption, theme.text_tertiary()),
        styled_text(
            crate::fl!("move-desktop-hint"),
            caption,
            theme.text_tertiary()
        ),
        Space::new().height(theme.spacing_3()),
        rows,
    ]
    .padding(theme.spacing_4());

    let cancel = Button::new(crate::fl!("move-desktop-cancel"), &**theme)
        .button_type(ButtonType::Ghost)
        .size(ButtonSize::Small)
        .on_press(Message::Dismiss);
    let footer = row![
        Space::new().width(Length::Fill),
        ring(Target::Cancel, cancel)
    ]
    .padding(Padding::from([theme.spacing_3(), theme.spacing_5()]));

    let radius = theme.radii_md();
    let glass = theme.glass_modal();
    let border = theme.border();
    let card = container(column![header, divider(), body, divider(), footer])
        .width(Length::Fixed(WIDTH))
        .style(move |_| container::Style {
            background: Some(Background::Color(glass)),
            border: Border {
                color: border,
                width: hairline,
                radius: radius.into(),
                ..Default::default()
            },
            ..Default::default()
        });
    container(with_elevation_shadow(
        card,
        &theme.shadow_modal(),
        Some(radius),
    ))
    .padding(padding(theme))
    .into()
}

/// A desktop: Monitor, its name over its window count, and the arrow when it is a destination.
fn desktop<'a>(entry: &MoveRow, idx: usize, theme: &'a CompTheme) -> Button<'a, Message> {
    let label = theme.text_styles().role(TextRole::Label);
    let caption = theme.text_styles().role(TextRole::Caption);
    let mut line = Row::new()
        .align_y(Alignment::Center)
        .spacing(theme.spacing_1())
        .push(icon_svg(icons::MONITOR, 16.0, theme.text_secondary()))
        .push(
            column![
                styled_text(entry.label.clone(), label, theme.text_secondary()),
                styled_text(
                    entry.detail.clone().unwrap_or_default(),
                    caption,
                    theme.text_tertiary()
                ),
            ]
            .width(Length::Fill),
        );
    if !entry.current {
        line = line.push(icon_svg(icons::ARROW_RIGHT, 14.0, theme.text_secondary()));
    }
    Button::new("", &**theme)
        .button_type(ButtonType::Ghost)
        .size(ButtonSize::Small)
        .content(line)
        .width(Length::Fill)
        .height(row_height(theme))
        .disabled(entry.current)
        .on_press(Message::ItemPressed(idx))
}

fn new_desktop<'a>(entry: &MoveRow, idx: usize, theme: &'a CompTheme) -> Button<'a, Message> {
    let line = row![
        icon_svg(icons::PLUS, 16.0, theme.text_secondary()),
        styled_text(
            entry.label.clone(),
            theme.text_styles().role(TextRole::Caption),
            theme.text_secondary()
        ),
    ]
    .align_y(Alignment::Center)
    .spacing(theme.spacing_1());
    Button::new("", &**theme)
        .button_type(ButtonType::Ghost)
        .size(ButtonSize::Small)
        .content(line)
        .width(Length::Fill)
        .on_press(Message::ItemPressed(idx))
}

/// 12px above and below two lines: a label over a caption.
fn row_height(theme: &CompTheme) -> f32 {
    let label = theme.text_styles().role(TextRole::Label).line_height;
    2.0 * theme.spacing_3() + label + caption_line(theme)
}

fn caption_line(theme: &CompTheme) -> f32 {
    theme.text_styles().role(TextRole::Caption).line_height
}

#[cfg(test)]
mod tests {
    use super::*;

    fn dialog() -> MoveDialog {
        let row = |label: &str, current| MoveRow {
            label: label.into(),
            detail: None,
            current,
        };
        MoveDialog {
            identity: "Foot".into(),
            source: "Main".into(),
            rows: vec![
                row("Main", true),
                row("Desktop 2", false),
                row("New desktop", false),
            ],
            focus: 0,
        }
    }

    #[test]
    fn tab_walks_close_the_destinations_and_cancel_skipping_the_current_desktop() {
        let mut dialog = dialog();
        let mut seen = vec![dialog.focused()];
        for _ in 0..4 {
            dialog.step(true);
            seen.push(dialog.focused());
        }
        assert_eq!(
            seen,
            [
                Target::Close,
                Target::Row(1),
                Target::Row(2),
                Target::Cancel,
                Target::Close
            ]
        );
        dialog.step(false);
        assert_eq!(dialog.focused(), Target::Cancel);
    }
}
