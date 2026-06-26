use crate::{
    comp_theme::CompTheme,
    fl,
    utils::{
        apply::Apply,
        iced::{IcedElement, Program},
        xdg_icon::named_icon,
    },
};

use calloop::LoopHandle;
use iced_core::{Alignment, Length};
use iced_widget::{Space, container, row};
use icetron_p::prelude::styled_text;
use smithay::utils::Size;

use crate::utils::iced::CompElement;

pub type SwapIndicator = IcedElement<SwapIndicatorInternal>;

pub fn swap_indicator(
    evlh: LoopHandle<'static, crate::state::State>,
    theme: CompTheme,
) -> SwapIndicator {
    SwapIndicator::new(SwapIndicatorInternal, Size::from((1, 1)), evlh, theme)
}

pub struct SwapIndicatorInternal;

impl Program for SwapIndicatorInternal {
    type Message = ();

    fn program_name() -> &'static str {
        "SwapIndicator"
    }

    fn view<'a>(&'a self, theme: &'a CompTheme) -> CompElement<'a, Self::Message> {
        let on_accent = theme.on_accent_color();
        let accent = theme.accent_color();

        row![
            named_icon("window-swap-symbolic", 32.0),
            Space::new().width(16.0).height(Length::Shrink),
            styled_text(fl!("swap-windows"), theme.text_styles().title2(), on_accent,),
        ]
        .align_y(Alignment::Center)
        .apply(container)
        .align_x(Alignment::Center)
        .align_y(Alignment::Center)
        .padding(16)
        .apply(container)
        .class(Box::new(move |_: &iced_core::Theme| container::Style {
            text_color: Some(on_accent),
            background: Some(iced_core::Background::Color(accent)),
            border: iced_core::Border {
                radius: 18.0.into(),
                width: 0.0,
                color: iced_core::Color::TRANSPARENT,
                ..Default::default()
            },
            shadow: Default::default(),
            snap: false,
            border_only: false,
        })
            as Box<dyn Fn(&iced_core::Theme) -> container::Style>)
        .width(Length::Shrink)
        .height(Length::Shrink)
        .apply(container)
        .center_x(Length::Fill)
        .center_y(Length::Fill)
        .into()
    }
}
