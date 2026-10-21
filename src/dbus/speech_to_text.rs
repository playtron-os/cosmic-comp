// SPDX-License-Identifier: GPL-3.0-only

//! Speech to Text, `one.playtron.SpeechToText1` (contract in `speech-to-text-client`).
//!
//! The binding is held, and its release never reaches the focused client, so the
//! compositor tells the desktop's dictation host when a hold starts, finishes or is
//! called off. Recording, transcribing and typing the words into the field are the
//! host's, through input-method-v2. With no host on the bus the binding is not ours:
//! the key goes to the focused app. A hold over an app that claimed its holds goes to
//! that app alone, which dictates into its own field.

use calloop::LoopHandle;
use cosmic_settings_config::shortcuts;
use futures_executor::ThreadPool;
use futures_util::StreamExt;
use smithay::input::keyboard::{Keysym, ModifiersState};
use kora_stt_client::{
    Announcer, BUS_NAME, HOST_NAME, Hold, Inline, InlineClaims, OBJECT_PATH,
};
use tracing::{debug, warn};
use zbus::object_server::SignalEmitter;

use crate::state::State;

/// What the keyboard filter does with a key pressed or released during a hold.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KeyOutcome {
    /// The key goes where it would have gone.
    Pass,
    /// The key ends the hold and nobody else sees it.
    Swallow,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Held {
    session: u32,
    /// Over an app that claimed its holds.
    inline: bool,
    key: Keysym,
    modifiers: shortcuts::Modifiers,
}

#[derive(Debug)]
pub struct SpeechToText {
    events: futures_channel::mpsc::UnboundedSender<Step>,
    hold: Option<Held>,
    last_session: u32,
    /// Whether a dictation host owns its name, so the binding is ours to take.
    host: bool,
    claims: InlineClaims,
}

/// A hold's step, and whether it goes to the app that claimed the window instead.
#[derive(Debug, Clone, PartialEq, Eq)]
struct Step {
    hold: Hold,
    inline: bool,
}

impl SpeechToText {
    /// Owns `one.playtron.SpeechToText1`, emits each hold in turn and follows the host.
    pub fn init(handle: &LoopHandle<'static, State>, executor: &ThreadPool) -> Self {
        let (presence, heard) = calloop::channel::channel::<bool>();
        if let Err(err) = handle.insert_source(heard, |event, _, state| {
            if let calloop::channel::Event::Msg(present) = event {
                state.common.speech_to_text.set_host(present);
            }
        }) {
            warn!(?err, "Failed to follow the dictation host");
        }

        let (events, mut queue) = futures_channel::mpsc::unbounded::<Step>();
        let follower = executor.clone();
        let claims = InlineClaims::default();
        let served = claims.clone();
        executor.spawn_ok(async move {
            let conn = match serve(served).await {
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
            follower.spawn_ok(follow_host(conn.clone(), presence));
            while let Some(Step { hold, inline }) = queue.next().await {
                let Ok(emitter) = SignalEmitter::new(&conn, OBJECT_PATH) else {
                    continue;
                };
                let sent = if inline {
                    hold.emit_inline(&emitter).await
                } else {
                    hold.emit(&emitter).await
                };
                if let Err(err) = sent {
                    warn!(?err, ?hold, inline, "Failed to signal Speech to Text");
                }
            }
        });
        Self::with_events(events, claims)
    }

    fn with_events(
        events: futures_channel::mpsc::UnboundedSender<Step>,
        claims: InlineClaims,
    ) -> Self {
        Self {
            events,
            hold: None,
            last_session: 0,
            host: false,
            claims,
        }
    }

    /// Whether the keyboard filter leaves `action`'s binding to the focused app.
    pub fn passes_through(&self, action: &shortcuts::Action) -> bool {
        *action == shortcuts::Action::SpeechToText && !self.host
    }

    /// A host came or went. One leaving mid-hold calls the hold off.
    pub fn set_host(&mut self, present: bool) {
        self.host = present;
        if !present && let Some(hold) = &self.hold {
            let session = hold.session;
            self.end(Hold::Cancelled(session));
        }
    }

    pub fn is_holding(&self) -> bool {
        self.hold.is_some()
    }

    pub fn held_key(&self) -> Option<Keysym> {
        self.hold.as_ref().map(|hold| hold.key)
    }

    /// The binding was pressed over `app_id`. A binding without a key cannot be held,
    /// and nothing is held without a host, even over an app that claimed its holds.
    pub fn start(&mut self, binding: &shortcuts::Binding, app_id: String) {
        let Some(key) = binding.key else {
            return;
        };
        if self.hold.is_some() || !self.host {
            return;
        }
        self.last_session = self.last_session.wrapping_add(1);
        let inline = self.claims.claimed(&app_id);
        self.hold = Some(Held {
            session: self.last_session,
            inline,
            key,
            modifiers: binding.modifiers.clone(),
        });
        self.send(Hold::Started {
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
                self.end(Hold::Finished(session));
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
                self.end(Hold::Cancelled(session));
                KeyOutcome::Swallow
            }
            Keysym::Return | Keysym::KP_Enter => {
                self.end(Hold::Finished(session));
                KeyOutcome::Swallow
            }
            _ => {
                self.end(Hold::Cancelled(session));
                KeyOutcome::Pass
            }
        }
    }

    fn end(&mut self, hold: Hold) {
        self.send(hold);
        self.hold = None;
    }

    /// Every step of a hold goes where its start went.
    fn send(&self, hold: Hold) {
        let inline = self.hold.as_ref().is_some_and(|held| held.inline);
        let _ = self.events.unbounded_send(Step { hold, inline });
    }
}

async fn serve(claims: InlineClaims) -> zbus::Result<zbus::Connection> {
    zbus::connection::Builder::session()?
        .serve_at(OBJECT_PATH, Announcer)?
        .serve_at(OBJECT_PATH, Inline::new(claims))?
        .name(BUS_NAME)?
        .build()
        .await
}

/// Tells the loop whenever a dictation host comes or goes.
async fn follow_host(conn: zbus::Connection, presence: calloop::channel::Sender<bool>) {
    let mut present = match kora_stt_client::host_present(&conn).await {
        Ok(present) => Box::pin(present),
        Err(err) => {
            warn!(
                ?err,
                "Failed to follow {HOST_NAME}; the binding stays with apps"
            );
            return;
        }
    };
    while let Some(now) = present.next().await {
        debug!(present = now, "Dictation host");
        if presence.send(now).is_err() {
            return;
        }
    }
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

    type Queue = futures_channel::mpsc::UnboundedReceiver<Step>;

    fn speech() -> (SpeechToText, Queue) {
        let (mut speech, queue) = without_host();
        speech.set_host(true);
        (speech, queue)
    }

    fn without_host() -> (SpeechToText, Queue) {
        let (events, queue) = futures_channel::mpsc::unbounded();
        (
            SpeechToText::with_events(events, InlineClaims::default()),
            queue,
        )
    }

    fn sent(queue: &mut Queue) -> Vec<Hold> {
        std::iter::from_fn(|| queue.try_recv().ok().map(|step| step.hold)).collect()
    }

    fn steps(queue: &mut Queue) -> Vec<Step> {
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
                Hold::Started {
                    session: 1,
                    app_id: "one.playtron.Mail".into()
                },
                Hold::Finished(1)
            ]
        );
    }

    #[test]
    fn releasing_a_modifier_first_finishes() {
        let (mut speech, mut queue) = speech();
        speech.start(&ctrl_space(), String::new());
        speech.key(false, false, Keysym::Control_L, &ctrl(false));
        assert!(!speech.is_holding());
        assert_eq!(sent(&mut queue).last(), Some(&Hold::Finished(1)));
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
        assert_eq!(events[1], Hold::Cancelled(1));
        assert_eq!(events[3], Hold::Finished(2));
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
        assert_eq!(sent(&mut queue).last(), Some(&Hold::Cancelled(1)));
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

    /// Without a host the binding is not the compositor's: Ctrl+Space reaches the app.
    #[test]
    fn without_a_host_the_key_passes_through() {
        let (mut speech, mut queue) = without_host();
        assert!(speech.passes_through(&shortcuts::Action::SpeechToText));
        speech.start(&ctrl_space(), "one.playtron.Mail".into());
        assert!(!speech.is_holding());
        assert!(sent(&mut queue).is_empty());
    }

    #[test]
    fn with_a_host_the_key_is_held() {
        let (mut speech, mut queue) = speech();
        assert!(!speech.passes_through(&shortcuts::Action::SpeechToText));
        speech.start(&ctrl_space(), String::new());
        assert!(speech.is_holding());
        assert_eq!(sent(&mut queue).len(), 1);
    }

    #[test]
    fn only_speech_to_text_waits_for_a_host() {
        let (speech, _queue) = without_host();
        assert!(!speech.passes_through(&shortcuts::Action::WindowCommands));
    }

    /// A host leaving mid-hold must not leave the hold swallowing keys.
    #[test]
    fn the_host_coming_and_going_mid_session() {
        let (mut speech, mut queue) = without_host();
        speech.set_host(true);
        speech.start(&ctrl_space(), String::new());
        assert!(speech.is_holding());

        speech.set_host(false);
        assert!(!speech.is_holding());
        assert!(speech.passes_through(&shortcuts::Action::SpeechToText));
        assert_eq!(
            sent(&mut queue),
            [
                Hold::Started {
                    session: 1,
                    app_id: String::new()
                },
                Hold::Cancelled(1)
            ]
        );
        assert_eq!(
            speech.key(true, false, Keysym::v, &ctrl(true)),
            KeyOutcome::Pass
        );

        speech.set_host(true);
        speech.start(&ctrl_space(), String::new());
        assert_eq!(
            sent(&mut queue),
            [Hold::Started {
                session: 2,
                app_id: String::new()
            }]
        );
    }

    #[test]
    fn a_host_leaving_between_holds_announces_nothing() {
        let (mut speech, mut queue) = speech();
        speech.set_host(false);
        assert!(sent(&mut queue).is_empty());
    }

    #[test]
    fn a_hold_over_a_claimed_app_goes_to_it_alone() {
        let (mut speech, mut queue) = speech();
        speech.claims.add("one.playtron.Game", ":1.7");
        speech.start(&ctrl_space(), "one.playtron.Game".into());
        speech.key(false, true, Keysym::space, &ctrl(true));
        speech.start(&ctrl_space(), "one.playtron.Mail".into());
        speech.key(true, false, Keysym::Escape, &ctrl(true));
        let step = |hold, inline| Step { hold, inline };
        let started = |session, app_id: &str| Hold::Started {
            session,
            app_id: app_id.into(),
        };
        assert_eq!(
            steps(&mut queue),
            [
                step(started(1, "one.playtron.Game"), true),
                step(Hold::Finished(1), true),
                step(started(2, "one.playtron.Mail"), false),
                step(Hold::Cancelled(2), false),
            ]
        );
    }

    /// A host leaving mid-hold cancels a claimed hold where it started, too.
    #[test]
    fn a_claimed_hold_is_cancelled_inline_when_the_host_goes() {
        let (mut speech, mut queue) = speech();
        speech.claims.add("one.playtron.Game", ":1.7");
        speech.start(&ctrl_space(), "one.playtron.Game".into());
        speech.set_host(false);
        let inline: Vec<bool> = steps(&mut queue).iter().map(|s| s.inline).collect();
        assert_eq!(inline, [true, true]);
    }

    /// The claim does not take the binding: with no host, Ctrl+Space stays the app's.
    #[test]
    fn without_a_host_a_claimed_app_still_gets_the_key() {
        let (mut speech, mut queue) = without_host();
        speech.claims.add("one.playtron.Game", ":1.7");
        assert!(speech.passes_through(&shortcuts::Action::SpeechToText));
        speech.start(&ctrl_space(), "one.playtron.Game".into());
        assert!(!speech.is_holding());
        assert!(sent(&mut queue).is_empty());
    }
}
