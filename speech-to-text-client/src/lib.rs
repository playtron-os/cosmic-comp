// SPDX-License-Identifier: MIT

//! The `one.playtron.SpeechToText1` D-Bus contract. The compositor owns the Speech to
//! Text binding and announces each hold of it; the desktop's dictation host records,
//! transcribes and types the words.
//!
//! The compositor takes the binding only while a host owns [`HOST_NAME`]. Without one
//! the key goes to the focused app untouched, so a host claims the name with [`Host`]
//! while it can dictate, and the compositor follows it with [`host_present`].
//!
//! An app that shows dictation in its own field takes the holds over its windows with
//! [`claim_inline`]; the host then never hears them (see [`INLINE_INTERFACE`]).

use futures_util::{Stream, StreamExt};
use zbus::fdo::{DBusProxy, RequestNameFlags};
use zbus::names::BusName;
use zbus::object_server::SignalEmitter;

mod inline;
pub use inline::{INLINE_INTERFACE, Inline, InlineClaims, claim_inline};

/// The compositor's name.
pub const BUS_NAME: &str = "one.playtron.SpeechToText1";
/// Where the compositor announces holds.
pub const OBJECT_PATH: &str = "/one/playtron/SpeechToText1";
/// The interface.
pub const INTERFACE: &str = "one.playtron.SpeechToText1";
/// Owned by the dictation host while it can dictate. Nothing is served under it: owning
/// it is what hands the binding to dictation.
pub const HOST_NAME: &str = "one.playtron.Dictation1";

/// One step of a hold. `session` tells a late step of an earlier hold from this one.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Hold {
    /// The binding went down over `app_id`'s window: start listening.
    Started { session: u32, app_id: String },
    /// The binding came up, or Enter was pressed: stop and type what was heard.
    Finished(u32),
    /// Escape, another key, or the host leaving: drop what was heard.
    Cancelled(u32),
}

/// The compositor's side: serve it at [`OBJECT_PATH`] and announce with [`Hold::emit`].
pub struct Announcer;

#[zbus::interface(name = "one.playtron.SpeechToText1")]
impl Announcer {
    /// The binding went down over `app_id`'s window: start listening.
    #[zbus(signal)]
    async fn started(emitter: &SignalEmitter<'_>, session: u32, app_id: &str) -> zbus::Result<()>;

    /// The binding came up, or Enter was pressed: stop listening and type what was heard.
    #[zbus(signal)]
    async fn finished(emitter: &SignalEmitter<'_>, session: u32) -> zbus::Result<()>;

    /// Escape, another key, or the host leaving: drop what was heard.
    #[zbus(signal)]
    async fn cancelled(emitter: &SignalEmitter<'_>, session: u32) -> zbus::Result<()>;
}

impl Hold {
    pub fn session(&self) -> u32 {
        match self {
            Self::Started { session, .. } | Self::Finished(session) | Self::Cancelled(session) => {
                *session
            }
        }
    }

    /// Announce this step from the object at [`OBJECT_PATH`].
    pub async fn emit(&self, emitter: &SignalEmitter<'_>) -> zbus::Result<()> {
        match self {
            Self::Started { session, app_id } => {
                Announcer::started(emitter, *session, app_id).await
            }
            Self::Finished(session) => Announcer::finished(emitter, *session).await,
            Self::Cancelled(session) => Announcer::cancelled(emitter, *session).await,
        }
    }

    /// The step a signal announces, or `None` for any other message.
    pub fn from_message(message: &zbus::Message) -> Option<Self> {
        let header = message.header();
        if header.interface()?.as_str() != INTERFACE {
            return None;
        }
        Self::from_body(header.member()?.as_str(), message)
    }

    fn from_body(member: &str, message: &zbus::Message) -> Option<Self> {
        let body = message.body();
        match member {
            "Started" => {
                let (session, app_id): (u32, String) = body.deserialize().ok()?;
                Some(Self::Started { session, app_id })
            }
            "Finished" => Some(Self::Finished(body.deserialize().ok()?)),
            "Cancelled" => Some(Self::Cancelled(body.deserialize().ok()?)),
            _ => None,
        }
    }
}

/// Every hold the compositor announces, in order, for as long as `connection` lasts.
pub async fn holds(
    connection: &zbus::Connection,
) -> zbus::Result<impl Stream<Item = Hold> + Send + use<>> {
    // One stream for all three signals: separate ones could hand a Finished over
    // before the Started it follows.
    let rule = zbus::MatchRule::builder()
        .msg_type(zbus::message::Type::Signal)
        .sender(BUS_NAME)?
        .interface(INTERFACE)?
        .path(OBJECT_PATH)?
        .build();
    let messages = zbus::MessageStream::for_match_rule(rule, connection, None).await?;
    Ok(messages.filter_map(|message| {
        std::future::ready(message.ok().as_ref().and_then(Hold::from_message))
    }))
}

/// Whether a dictation host owns [`HOST_NAME`]: now, then each time that changes.
pub async fn host_present(
    connection: &zbus::Connection,
) -> zbus::Result<impl Stream<Item = bool> + Send + use<>> {
    let dbus = DBusProxy::new(connection).await?;
    // Subscribed first, so a host arriving in between is not missed.
    let changes = dbus
        .receive_name_owner_changed_with_args(&[(0, HOST_NAME)])
        .await?
        .filter_map(|changed| {
            std::future::ready(changed.args().ok().map(|args| args.new_owner().is_some()))
        });
    let now = dbus
        .name_has_owner(BusName::from_static_str(HOST_NAME)?)
        .await?;
    Ok(futures_util::stream::once(std::future::ready(now)).chain(changes))
}

/// [`HOST_NAME`], held for as long as this lives.
#[derive(Debug)]
pub struct Host {
    connection: zbus::Connection,
}

impl Host {
    /// Claims the name on a connection of its own, so dropping the host closes it and
    /// gives the binding back to apps. Fails while another host holds the name.
    pub async fn claim(builder: zbus::connection::Builder<'_>) -> zbus::Result<Self> {
        let connection = builder.build().await?;
        // Nothing is served under the name; an empty server keeps zbus from warning so.
        let _ = connection.object_server();
        // Never queued: a second host would think it hosts while the first does.
        connection
            .request_name_with_flags(HOST_NAME, RequestNameFlags::DoNotQueue.into())
            .await?;
        Ok(Self { connection })
    }

    /// Lets go of the name now, rather than when the connection closes.
    pub async fn release(self) -> zbus::Result<()> {
        self.connection.release_name(HOST_NAME).await.map(drop)
    }
}

#[cfg(test)]
mod tests;
