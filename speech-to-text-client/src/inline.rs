// SPDX-License-Identifier: MIT

//! Dictation an app shows in its own field. While an app claims its app id, a hold over
//! one of its windows is announced on [`INLINE_INTERFACE`] instead, so the desktop's
//! host never starts on it and the app records and shows the hold itself.

use crate::{BUS_NAME, Hold, OBJECT_PATH};
use futures_util::{Stream, StreamExt};
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, Mutex};
use zbus::fdo::DBusProxy;
use zbus::message::Header;
use zbus::names::BusName;
use zbus::object_server::SignalEmitter;

/// The holds of claimed apps, served next to the compositor's own at [`OBJECT_PATH`].
pub const INLINE_INTERFACE: &str = "one.playtron.SpeechToText1.Inline";

/// Who claimed which app id, by unique bus name. Shared between the served interface and
/// the compositor deciding where a hold goes.
#[derive(Debug, Clone, Default)]
pub struct InlineClaims(Arc<Mutex<HashMap<String, HashSet<String>>>>);

impl InlineClaims {
    /// Whether holds over `app_id`'s windows belong to the app.
    pub fn claimed(&self, app_id: &str) -> bool {
        !app_id.is_empty() && self.0.lock().unwrap().contains_key(app_id)
    }

    /// Records `claimer`'s claim on `app_id`, as a call to the served interface does.
    pub fn add(&self, app_id: &str, claimer: &str) {
        let mut claims = self.0.lock().unwrap();
        claims
            .entry(app_id.to_owned())
            .or_default()
            .insert(claimer.to_owned());
    }

    fn remove(&self, app_id: &str, claimer: &str) {
        let mut claims = self.0.lock().unwrap();
        if let Some(claimers) = claims.get_mut(app_id) {
            claimers.remove(claimer);
            if claimers.is_empty() {
                claims.remove(app_id);
            }
        }
    }

    /// Everything `claimer` claimed, once it has left the bus.
    fn forget(&self, claimer: &str) {
        self.0.lock().unwrap().retain(|_, claimers| {
            claimers.remove(claimer);
            !claimers.is_empty()
        });
    }

    /// Forgets `claimer` when it leaves the bus, or now if it already has.
    async fn forget_when_gone(self, connection: zbus::Connection, claimer: String) {
        let gone = async {
            let dbus = DBusProxy::new(&connection).await?;
            // Subscribed before asking, so a departure in between is not missed.
            let mut changes = dbus
                .receive_name_owner_changed_with_args(&[(0, claimer.as_str())])
                .await?;
            if !dbus
                .name_has_owner(BusName::try_from(claimer.as_str())?)
                .await?
            {
                return zbus::Result::Ok(());
            }
            while let Some(change) = changes.next().await {
                if change.args().is_ok_and(|args| args.new_owner().is_none()) {
                    break;
                }
            }
            Ok(())
        };
        // A watch that failed cannot tell when the claimer goes, so it goes now.
        let _ = gone.await;
        self.forget(&claimer);
    }
}

/// The compositor's side: serve it at [`OBJECT_PATH`] with the same [`InlineClaims`] the
/// compositor reads, and announce claimed holds with [`Hold::emit_inline`].
#[derive(Debug, Default)]
pub struct Inline {
    claims: InlineClaims,
}

impl Inline {
    pub fn new(claims: InlineClaims) -> Self {
        Self { claims }
    }
}

#[zbus::interface(name = "one.playtron.SpeechToText1.Inline")]
impl Inline {
    /// Holds over `app_id`'s windows go to the caller from now on, until it releases them
    /// or leaves the bus.
    async fn claim(
        &self,
        app_id: String,
        #[zbus(header)] header: Header<'_>,
        #[zbus(connection)] connection: &zbus::Connection,
    ) -> zbus::fdo::Result<()> {
        let claimer = caller(&header)?;
        if app_id.is_empty() {
            return Err(zbus::fdo::Error::InvalidArgs("no app id".into()));
        }
        self.claims.add(&app_id, &claimer);
        // Watched off this task: asking the bus from a handler can stall the interface.
        let watch = self
            .claims
            .clone()
            .forget_when_gone(connection.clone(), claimer);
        connection
            .executor()
            .spawn(watch, "speech-to-text inline claim")
            .detach();
        Ok(())
    }

    /// Holds over `app_id`'s windows go to the desktop's host again.
    async fn release(
        &self,
        app_id: String,
        #[zbus(header)] header: Header<'_>,
    ) -> zbus::fdo::Result<()> {
        self.claims.remove(&app_id, &caller(&header)?);
        Ok(())
    }

    /// The binding went down over the claimed `app_id`'s window: start listening.
    #[zbus(signal)]
    async fn started(emitter: &SignalEmitter<'_>, session: u32, app_id: &str) -> zbus::Result<()>;

    /// The binding came up, or Enter was pressed: stop listening and use what was heard.
    #[zbus(signal)]
    async fn finished(emitter: &SignalEmitter<'_>, session: u32) -> zbus::Result<()>;

    /// Escape, another key, or the host leaving: drop what was heard.
    #[zbus(signal)]
    async fn cancelled(emitter: &SignalEmitter<'_>, session: u32) -> zbus::Result<()>;
}

fn caller(header: &Header<'_>) -> zbus::fdo::Result<String> {
    header
        .sender()
        .map(|sender| sender.to_string())
        .ok_or_else(|| zbus::fdo::Error::Failed("no sender".into()))
}

impl Hold {
    /// Announce this step of a claimed app's hold, which the host does not hear.
    pub async fn emit_inline(&self, emitter: &SignalEmitter<'_>) -> zbus::Result<()> {
        match self {
            Self::Started { session, app_id } => Inline::started(emitter, *session, app_id).await,
            Self::Finished(session) => Inline::finished(emitter, *session).await,
            Self::Cancelled(session) => Inline::cancelled(emitter, *session).await,
        }
    }

    /// The claimed hold step a signal announces, or `None` for any other message.
    pub fn from_inline_message(message: &zbus::Message) -> Option<Self> {
        let header = message.header();
        if header.interface()?.as_str() != INLINE_INTERFACE {
            return None;
        }
        Self::from_body(header.member()?.as_str(), message)
    }
}

/// Claims `app_id`'s holds and yields them: a `Started` over one of its windows, then that
/// hold's `Finished` or `Cancelled`. The claim lasts as long as the stream, on a connection
/// of its own, so dropping the stream hands the holds back to the desktop's host.
pub async fn claim_inline(
    builder: zbus::connection::Builder<'_>,
    app_id: &str,
) -> zbus::Result<impl Stream<Item = Hold> + Send + use<>> {
    let connection = builder.build().await?;
    let rule = zbus::MatchRule::builder()
        .msg_type(zbus::message::Type::Signal)
        .sender(BUS_NAME)?
        .interface(INLINE_INTERFACE)?
        .path(OBJECT_PATH)?
        .build();
    // Subscribed before claiming, so the first hold cannot slip past.
    let messages = zbus::MessageStream::for_match_rule(rule, &connection, None).await?;
    connection
        .call_method(
            Some(BUS_NAME),
            OBJECT_PATH,
            Some(INLINE_INTERFACE),
            "Claim",
            &(app_id,),
        )
        .await?;
    let app_id = app_id.to_owned();
    // Another app's claimed holds come through the same rule; only this one's sessions pass.
    let ours = messages.scan(None, move |session: &mut Option<u32>, message| {
        let hold = message.ok().as_ref().and_then(Hold::from_inline_message);
        let pass = match &hold {
            Some(Hold::Started {
                session: s,
                app_id: a,
            }) if *a == app_id => {
                *session = Some(*s);
                hold
            }
            Some(Hold::Finished(s) | Hold::Cancelled(s)) if *session == Some(*s) => {
                *session = None;
                hold
            }
            _ => None,
        };
        std::future::ready(Some(pass))
    });
    Ok(ours.filter_map(std::future::ready))
}
