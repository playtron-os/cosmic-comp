//! `layer-card ROLE [WIDTHxHEIGHT]`: a layer surface that names a role and shows itself after
//! 800ms, so a harness can film the compositor's motion for that role.

use std::time::Duration;

use iced::widget::{column, container, text};
use iced::{Border, Color, Element, Length, Task};
use iced_layershell::build_pattern::application;
use iced_layershell::reexport::{Anchor, Layer, LayerTransition};
use iced_layershell::settings::{LayerShellSettings, Settings};
use iced_layershell::to_layer_message;

#[to_layer_message]
#[derive(Debug, Clone)]
enum Message {
    Show,
}

fn role(name: &str) -> LayerTransition {
    match name {
        "popover" => LayerTransition::Popover,
        "panel" => LayerTransition::Panel,
        "control_panel" => LayerTransition::ControlPanel,
        "launcher" => LayerTransition::Launcher,
        "spotlight" => LayerTransition::Spotlight,
        "notification" => LayerTransition::Notification,
        "context_menu" => LayerTransition::ContextMenu,
        "modal" => LayerTransition::Modal,
        _ => LayerTransition::Fade,
    }
}

fn main() -> Result<(), iced_layershell::Error> {
    let mut args = std::env::args().skip(1);
    let name = args.next().unwrap_or_else(|| "modal".into());
    let (w, h) = args
        .next()
        .and_then(|s| s.split_once('x').map(|(w, h)| (w.parse(), h.parse())))
        .and_then(|(w, h)| Some((w.ok()?, h.ok()?)))
        .unwrap_or((420, 240));
    let notification = name == "notification";
    let title = name.clone();
    application(
        move || {
            let show = Task::perform(tokio::time::sleep(Duration::from_millis(800)), |()| {
                Message::Show
            });
            (
                title.clone(),
                Task::batch([Task::done(Message::HideWindow), show]),
            )
        },
        || "layer-card".to_string(),
        |_: &mut String, message| match message {
            Message::Show => Task::done(Message::ShowWindow),
            _ => Task::none(),
        },
        view,
    )
    .style(|_, _| iced::theme::Style {
        background_color: Color::TRANSPARENT,
        text_color: Color::BLACK,
    })
    .settings(Settings {
        layer_settings: LayerShellSettings {
            size: Some((w, h)),
            layer: Layer::Overlay,
            anchor: if notification {
                Anchor::Top | Anchor::Right
            } else {
                Anchor::empty()
            },
            margin: if notification {
                (16, 16, 0, 0)
            } else {
                (0, 0, 0, 0)
            },
            exclusive_zone: 0,
            transition: Some(role(&name)),
            ..Default::default()
        },
        ..Default::default()
    })
    .run()
}

fn view(title: &String) -> Element<'_, Message> {
    let line = |width: f32| {
        container(text("")).width(width).height(10).style(|_| {
            container::background(Color::from_rgb8(0xb8, 0xbf, 0xcc))
                .border(Border::default().rounded(5))
        })
    };
    container(
        column![
            text(title.clone())
                .size(18)
                .color(Color::from_rgb8(0x1e, 0x22, 0x2b)),
            line(300.0),
            line(240.0),
            line(270.0)
        ]
        .spacing(16),
    )
    .padding(24)
    .width(Length::Fill)
    .height(Length::Fill)
    .style(|_| {
        container::background(Color::from_rgb8(0xe8, 0xeb, 0xf2)).border(
            Border::default()
                .rounded(16)
                .width(1)
                .color(Color::from_rgb8(0x8a, 0x93, 0xa6)),
        )
    })
    .into()
}
