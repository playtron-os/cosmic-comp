// SPDX-License-Identifier: GPL-3.0-only

//! Speech to Text, `one.playtron.SpeechToText1`.
//!
//! The binding is held, and its release never reaches the focused client, so the
//! compositor tells the desktop's dictation host when a hold starts, finishes or is
//! called off. Recording, transcribing and typing the words into the field are the
//! host's: Kora's panel, through input-method-v2.

use cosmic_settings_config::shortcuts;
use futures_executor::ThreadPool;
use futures_util::StreamExt;
use smithay::input::keyboard::{Keysym, ModifiersState};
use tracing::{debug, warn};
use zbus::object_server::SignalEmitter;

const BUS_NAME: &str = "one.playtron.SpeechToText1";
const OBJECT_PATH: &str = "/one/playtron/SpeechToText1";

/// What a hold did, in the order it happened.
#[derive(Debug, Clone, PartialEq, Eq)]
enum Event {
    Started { session: u32, app_id: String },
    Finished(u32),
    Cancelled(u32),
}

struct Interface;

#[zbus::interface(name = "one.playtron.SpeechToText1")]
impl Interface {
    /// The binding went down over `app_id`'s window: start listening.
    #[zbus(signal)]
    async fn started(emitter: &SignalEmitter<'_>, session: u32, app_id: &str) -> zbus::Result<()>;

    /// The binding came up, or Enter was pressed: stop listening and type what was heard.
    #[zbus(signal)]
    async fn finished(emitter: &SignalEmitter<'_>, session: u32) -> zbus::Result<()>;

    /// Escape or another key was pressed: drop what was heard.
    #[zbus(signal)]
    async fn cancelled(emitter: &SignalEmitter<'_>, session: u32) -> zbus::Result<()>;
}

/// What the keyboard filter does with a key pressed or released during a hold.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KeyOutcome {
    /// The key goes where it would have gone.
    Pass,
    /// The key ends the hold and nobody else sees it.
    Swallow,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Hold {
    session: u32,
    key: Keysym,
    modifiers: shortcuts::Modifiers,
}

#[derive(Debug)]
pub struct SpeechToText {
    events: futures_channel::mpsc::UnboundedSender<Event>,
    hold: Option<Hold>,
    last_session: u32,
}

impl SpeechToText {
    /// Owns `one.playtron.SpeechToText1` and emits each event in turn.
    pub fn init(executor: &ThreadPool) -> Self {
        let (events, mut queue) = futures_channel::mpsc::unbounded::<Event>();
        executor.spawn_ok(async move {
            let conn = match serve().await {
                Ok(conn) => conn,
                Err(err) => {
                    warn!(
                        ?err,
                        "Failed to serve {BUS_NAME}; Speech to Text is unavailable"
                    );
                    return;
                }
            };
            debug!("Serving {BUS_NAME}");
            while let Some(event) = queue.next().await {
                let Ok(emitter) = SignalEmitter::new(&conn, OBJECT_PATH) else {
                    continue;
                };
                let sent = match &event {
                    Event::Started { session, app_id } => {
                        Interface::started(&emitter, *session, app_id).await
                    }
                    Event::Finished(session) => Interface::finished(&emitter, *session).await,
                    Event::Cancelled(session) => Interface::cancelled(&emitter, *session).await,
                };
                if let Err(err) = sent {
                    warn!(?err, ?event, "Failed to signal Speech to Text");
                }
            }
        });
        Self::with_events(events)
    }

    fn with_events(events: futures_channel::mpsc::UnboundedSender<Event>) -> Self {
        Self {
            events,
            hold: None,
            last_session: 0,
        }
    }

    pub fn is_holding(&self) -> bool {
        self.hold.is_some()
    }

    pub fn held_key(&self) -> Option<Keysym> {
        self.hold.as_ref().map(|hold| hold.key)
    }

    /// The binding was pressed over `app_id`. A binding without a key cannot be held.
    pub fn start(&mut self, binding: &shortcuts::Binding, app_id: String) {
        let Some(key) = binding.key else {
            return;
        };
        if self.hold.is_some() {
            return;
        }
        self.last_session = self.last_session.wrapping_add(1);
        self.hold = Some(Hold {
            session: self.last_session,
            key,
            modifiers: binding.modifiers.clone(),
        });
        self.send(Event::Started {
            session: self.last_session,
            app_id,
        });
    }

    /// A key during a hold. Releasing the binding's key or one of its modifiers
    /// finishes, as Enter does; Escape or any other key calls it off.
    pub fn key(
        &mut self,
        pressed: bool,
        is_binding_key: bool,
        sym: Keysym,
        modifiers: &ModifiersState,
    ) -> KeyOutcome {
        let Some(hold) = &self.hold else {
            return KeyOutcome::Pass;
        };
        let session = hold.session;
        if !pressed {
            if is_binding_key || !still_held(&hold.modifiers, modifiers) {
                self.end(Event::Finished(session));
            }
            return KeyOutcome::Pass;
        }
        if sym.is_modifier_key() {
            return KeyOutcome::Pass;
        }
        // The held key pressed again is a repeat (winit and some keyboards send them).
        if is_binding_key {
            return KeyOutcome::Swallow;
        }
        match sym {
            Keysym::Escape => {
                self.end(Event::Cancelled(session));
                KeyOutcome::Swallow
            }
            Keysym::Return | Keysym::KP_Enter => {
                self.end(Event::Finished(session));
                KeyOutcome::Swallow
            }
            _ => {
                self.end(Event::Cancelled(session));
                KeyOutcome::Pass
            }
        }
    }

    fn end(&mut self, event: Event) {
        self.hold = None;
        self.send(event);
    }

    fn send(&self, event: Event) {
        let _ = self.events.unbounded_send(event);
    }
}

async fn serve() -> zbus::Result<zbus::Connection> {
    zbus::connection::Builder::session()?
        .serve_at(OBJECT_PATH, Interface)?
        .name(BUS_NAME)?
        .build()
        .await
}

fn still_held(binding: &shortcuts::Modifiers, now: &ModifiersState) -> bool {
    (!binding.ctrl || now.ctrl)
        && (!binding.alt || now.alt)
        && (!binding.shift || now.shift)
        && (!binding.logo || now.logo)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    fn speech() -> (
        SpeechToText,
        futures_channel::mpsc::UnboundedReceiver<Event>,
    ) {
        let (events, queue) = futures_channel::mpsc::unbounded();
        (SpeechToText::with_events(events), queue)
    }

    fn sent(queue: &mut futures_channel::mpsc::UnboundedReceiver<Event>) -> Vec<Event> {
        std::iter::from_fn(|| queue.try_recv().ok()).collect()
    }

    fn ctrl(held: bool) -> ModifiersState {
        ModifiersState {
            ctrl: held,
            ..Default::default()
        }
    }

    fn ctrl_space() -> shortcuts::Binding {
        shortcuts::Binding::from_str("Ctrl+space").unwrap()
    }

    #[test]
    fn releasing_the_key_finishes() {
        let (mut speech, mut queue) = speech();
        speech.start(&ctrl_space(), "one.playtron.Mail".into());
        let outcome = speech.key(false, true, Keysym::space, &ctrl(true));
        assert_eq!(outcome, KeyOutcome::Pass);
        assert!(!speech.is_holding());
        assert_eq!(
            sent(&mut queue),
            [
                Event::Started {
                    session: 1,
                    app_id: "one.playtron.Mail".into()
                },
                Event::Finished(1)
            ]
        );
    }

    #[test]
    fn releasing_a_modifier_first_finishes() {
        let (mut speech, mut queue) = speech();
        speech.start(&ctrl_space(), String::new());
        speech.key(false, false, Keysym::Control_L, &ctrl(false));
        assert!(!speech.is_holding());
        assert_eq!(sent(&mut queue).last(), Some(&Event::Finished(1)));
        // The key's own release afterwards belongs to no hold.
        assert_eq!(
            speech.key(false, true, Keysym::space, &ctrl(false)),
            KeyOutcome::Pass
        );
        assert!(sent(&mut queue).is_empty());
    }

    #[test]
    fn escape_cancels_and_enter_finishes_without_reaching_the_app() {
        let (mut speech, mut queue) = speech();
        speech.start(&ctrl_space(), String::new());
        assert_eq!(
            speech.key(true, false, Keysym::Escape, &ctrl(true)),
            KeyOutcome::Swallow
        );
        speech.start(&ctrl_space(), String::new());
        assert_eq!(
            speech.key(true, false, Keysym::Return, &ctrl(true)),
            KeyOutcome::Swallow
        );
        let events = sent(&mut queue);
        assert_eq!(events[1], Event::Cancelled(1));
        assert_eq!(events[3], Event::Finished(2));
    }

    #[test]
    fn another_key_cancels_and_still_reaches_the_app() {
        let (mut speech, mut queue) = speech();
        speech.start(&ctrl_space(), String::new());
        assert_eq!(
            speech.key(true, false, Keysym::Shift_L, &ctrl(true)),
            KeyOutcome::Pass
        );
        assert!(speech.is_holding(), "a modifier joining is not another key");
        assert_eq!(
            speech.key(true, false, Keysym::v, &ctrl(true)),
            KeyOutcome::Pass
        );
        assert!(!speech.is_holding());
        assert_eq!(sent(&mut queue).last(), Some(&Event::Cancelled(1)));
    }

    #[test]
    fn a_repeat_of_the_held_key_keeps_listening() {
        let (mut speech, mut queue) = speech();
        speech.start(&ctrl_space(), String::new());
        assert_eq!(
            speech.key(true, true, Keysym::space, &ctrl(true)),
            KeyOutcome::Swallow
        );
        assert!(speech.is_holding());
        assert_eq!(sent(&mut queue).len(), 1);
    }

    #[test]
    fn a_second_press_while_holding_starts_nothing() {
        let (mut speech, mut queue) = speech();
        speech.start(&ctrl_space(), String::new());
        speech.start(&ctrl_space(), String::new());
        assert_eq!(sent(&mut queue).len(), 1);
    }
}
