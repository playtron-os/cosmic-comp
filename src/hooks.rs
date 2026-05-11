// SPDX-License-Identifier: GPL-3.0-only

use crate::comp_theme::CompTheme;
use crate::shell::element::stack::{
    CosmicStackInternal, DefaultDecorations as DefaultStackDecorations, Message as StackMessage,
};
use crate::shell::element::window::{
    CosmicWindowInternal, DefaultDecorations as DefaultWindowDecorations, Message as WindowMessage,
};
use crate::utils::iced::CompElement;
use std::sync::{Arc, OnceLock};

/// An _unstable_ interface to customize cosmic-comp at compile-time by providing
/// hooks to be run in specific code paths.
#[derive(Default, Debug, Clone)]
pub struct Hooks {
    pub window_decorations:
        Option<Arc<dyn Decorations<CosmicWindowInternal, WindowMessage> + Send + Sync>>,
    pub stack_decorations:
        Option<Arc<dyn Decorations<CosmicStackInternal, StackMessage> + Send + Sync>>,
}

pub static HOOKS: OnceLock<Hooks> = OnceLock::new();

pub trait Decorations<Internal, Message>: std::fmt::Debug {
    fn view<'a>(&'a self, state: &'a Internal, theme: &'a CompTheme) -> CompElement<'a, Message>;
}

impl Decorations<CosmicWindowInternal, WindowMessage>
    for Option<Arc<dyn Decorations<CosmicWindowInternal, WindowMessage> + Send + Sync>>
{
    fn view<'a>(
        &'a self,
        window: &'a CosmicWindowInternal,
        theme: &'a CompTheme,
    ) -> CompElement<'a, WindowMessage> {
        match self {
            None => DefaultWindowDecorations.view(window, theme),
            Some(deco) => deco.view(window, theme),
        }
    }
}

impl Decorations<CosmicStackInternal, StackMessage>
    for Option<Arc<dyn Decorations<CosmicStackInternal, StackMessage> + Send + Sync>>
{
    fn view<'a>(
        &'a self,
        window: &'a CosmicStackInternal,
        theme: &'a CompTheme,
    ) -> CompElement<'a, StackMessage> {
        match self {
            None => DefaultStackDecorations.view(window, theme),
            Some(deco) => deco.view(window, theme),
        }
    }
}
