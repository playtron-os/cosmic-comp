// SPDX-License-Identifier: MIT

//! The read side of `one.playtron.GameMode`: whether a game holds the screen. The compositor
//! owns game mode and only the game launcher drives it; anyone on the session bus may watch it,
//! for example to hold back something that would close the game.

use futures_util::{Stream, StreamExt};

/// The compositor's name.
pub const BUS_NAME: &str = "one.playtron.GameMode";
/// Where the compositor serves it.
pub const OBJECT_PATH: &str = "/one/playtron/GameMode";
/// The game launcher's own app id: game mode on it is the launcher, not a game.
pub const LAUNCHER_APP_ID: u32 = 769;

/// What game mode holds the screen for.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct State {
    pub active: bool,
    /// The app game mode is on; 0 for none.
    pub app_id: u32,
}

impl State {
    /// A game holds the screen, not the launcher or nothing.
    pub fn game_running(self) -> bool {
        self.active && self.app_id != 0 && self.app_id != LAUNCHER_APP_ID
    }
}

#[zbus::proxy(
    interface = "one.playtron.GameMode",
    default_service = "one.playtron.GameMode",
    default_path = "/one/playtron/GameMode",
    gen_blocking = false
)]
trait GameMode {
    #[zbus(property)]
    fn active(&self) -> zbus::Result<bool>;

    #[zbus(property)]
    fn active_app_id(&self) -> zbus::Result<u32>;

    #[zbus(signal)]
    fn state_changed(&self, active: bool, app_id: u32) -> zbus::Result<()>;
}

/// Game mode's state now, then each time it changes, for as long as `connection` lasts. Without
/// a compositor serving it, game mode is off.
pub async fn states(
    connection: &zbus::Connection,
) -> zbus::Result<impl Stream<Item = State> + Send + use<>> {
    let proxy = GameModeProxy::builder(connection)
        .cache_properties(zbus::proxy::CacheProperties::No)
        .build()
        .await?;
    // Subscribed first, so a change in between is not missed.
    let changes = proxy.receive_state_changed().await?.filter_map(|signal| {
        std::future::ready(signal.args().ok().map(|args| State {
            active: *args.active(),
            app_id: *args.app_id(),
        }))
    });
    let now = State {
        active: proxy.active().await.unwrap_or(false),
        app_id: proxy.active_app_id().await.unwrap_or(0),
    };
    Ok(futures_util::stream::once(std::future::ready(now)).chain(changes))
}

#[cfg(test)]
mod tests;
