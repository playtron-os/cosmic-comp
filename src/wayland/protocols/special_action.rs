// SPDX-License-Identifier: GPL-3.0-only

//! Implementation of the special action protocol (zcosmic_special_action_v1)
//!
//! The device's special key — the HUMAIN button — is a gesture the compositor
//! has to resolve itself: it is usually bound to Super, which the compositor
//! must also keep for its own chords, so only it can tell the key being held
//! from the start of `Super+L`. This protocol carries the *resolved* meaning to
//! a client, as `hold_start`/`hold_end` around a hold.
//!
//! Both gestures start the same way. The key going down is announced as
//! `hold_start` at once, because the common case is speech and a threshold spent
//! deciding is a threshold spoken into a microphone that has not been asked to
//! listen. If the key then comes back up inside [`PRESS_THRESHOLD`] it was a
//! press after all: the hold is `cancel`led — so whatever was captured is thrown
//! away rather than transcribed — and `activate` follows.
//!
//! Clients register surfaces as receivers. The focused receiver wins; failing
//! that the default receiver does. Registration is how the compositor avoids
//! having to identify surfaces itself — home is a layer surface and carries no
//! app id to match on.

pub use generated::{zcosmic_special_action_manager_v1, zcosmic_special_action_v1};

#[allow(
    non_snake_case,
    non_upper_case_globals,
    non_camel_case_types,
    unused_imports
)]
mod generated {
    use smithay::reexports::wayland_server::{self, protocol::*};

    pub mod __interfaces {
        use smithay::reexports::wayland_server::protocol::__interfaces::*;
        wayland_scanner::generate_interfaces!("resources/protocols/special_action.xml");
    }
    use self::__interfaces::*;

    wayland_scanner::generate_server_code!("resources/protocols/special_action.xml");
}

use smithay::reexports::wayland_server::{
    Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource, Weak,
    backend::GlobalId, protocol::wl_surface::WlSurface,
};
use std::sync::Mutex;
use std::time::{Duration, Instant};
use tracing::debug;

/// Per-receiver data attached to a `zcosmic_special_action_v1`.
#[derive(Debug, Clone)]
pub struct SpecialActionReceiverData {
    /// The surface this receiver speaks for.
    pub surface: WlSurface,
    /// Whether it also serves as the fallback receiver.
    pub is_default: bool,
}

/// A registered receiver.
#[derive(Debug)]
struct Receiver {
    surface: WlSurface,
    resource: zcosmic_special_action_v1::ZcosmicSpecialActionV1,
    is_default: bool,
}

/// How quickly the key must come back up for the press to count as one.
///
/// Speech does not fit in this, so anything shorter is read as a press rather
/// than as a very short thing said.
pub const PRESS_THRESHOLD: Duration = Duration::from_millis(300);

/// Where the current gesture has got to.
///
/// Two states, not three. There used to be a `Pending` for the window in which a
/// press might still turn out to be a hold, and every hold paid that window
/// before it could start. The key is now announced as a hold on the edge and
/// reinterpreted on release if it did not last, so nothing waits.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum Gesture {
    /// Key is up; nothing in flight.
    #[default]
    Idle,
    /// Key is down and `hold_start` has been sent.
    Holding,
}

/// Registry of receivers plus the in-flight gesture.
pub struct SpecialActionState {
    global: GlobalId,
    inner: Mutex<Inner>,
}

#[derive(Debug, Default)]
struct Inner {
    receivers: Vec<Receiver>,
    /// The receiver a hold was announced to, so `hold_end` reaches the same one
    /// even if focus moved while the key was down.
    holding: Option<Weak<zcosmic_special_action_v1::ZcosmicSpecialActionV1>>,
    /// When the key went down, for telling a press from a hold on release.
    pressed_at: Option<Instant>,
    gesture: Gesture,
}

impl std::fmt::Debug for SpecialActionState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let inner = self.inner.lock().unwrap();
        f.debug_struct("SpecialActionState")
            .field("receivers", &inner.receivers.len())
            .field("gesture", &inner.gesture)
            .finish_non_exhaustive()
    }
}

/// What the compositor should do with a key edge, decided by [`SpecialActionState`].
#[derive(Debug, Clone, PartialEq)]
pub enum KeyOutcome {
    /// A hold ran its course and ended. Nothing further is required.
    HoldEnded,
    /// The key came back up inside [`PRESS_THRESHOLD`]: the hold was cancelled
    /// and `activate` sent instead.
    ///
    /// Carries the surface that received it, because whoever is asked to take
    /// the caret also needs the keyboard — a caret on a surface the keystrokes
    /// never reach is worthless.
    Pressed(WlSurface),
    /// Nothing was in flight; ignore the edge.
    Ignored,
}

impl SpecialActionState {
    pub fn new<D>(dh: &DisplayHandle) -> Self
    where
        D: GlobalDispatch<zcosmic_special_action_manager_v1::ZcosmicSpecialActionManagerV1, ()>
            + Dispatch<zcosmic_special_action_manager_v1::ZcosmicSpecialActionManagerV1, ()>
            + Dispatch<zcosmic_special_action_v1::ZcosmicSpecialActionV1, SpecialActionReceiverData>
            + 'static,
    {
        let global = dh
            .create_global::<D, zcosmic_special_action_manager_v1::ZcosmicSpecialActionManagerV1, _>(
                1,
                (),
            );
        Self {
            global,
            inner: Mutex::new(Inner::default()),
        }
    }

    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }

    fn register(
        &self,
        data: &SpecialActionReceiverData,
        resource: zcosmic_special_action_v1::ZcosmicSpecialActionV1,
    ) {
        let mut inner = self.inner.lock().unwrap();
        if data.is_default {
            // Only one fallback at a time; a new default demotes the old one
            // rather than leaving two surfaces claiming the same role.
            for receiver in inner.receivers.iter_mut() {
                receiver.is_default = false;
            }
        }
        inner.receivers.push(Receiver {
            surface: data.surface.clone(),
            resource,
            is_default: data.is_default,
        });
        debug!(
            is_default = data.is_default,
            total = inner.receivers.len(),
            "Registered special action receiver"
        );
    }

    fn unregister(&self, resource: &zcosmic_special_action_v1::ZcosmicSpecialActionV1) {
        let mut inner = self.inner.lock().unwrap();
        inner.receivers.retain(|r| &r.resource != resource);
    }

    /// Drop receivers whose client is gone, so a dead surface never wins routing.
    fn prune(inner: &mut Inner) {
        inner.receivers.retain(|r| r.resource.is_alive());
    }

    /// The receiver that should get this gesture: the focused one, else the default.
    fn target(
        inner: &Inner,
        focused: Option<&WlSurface>,
    ) -> Option<zcosmic_special_action_v1::ZcosmicSpecialActionV1> {
        if let Some(focused) = focused
            && let Some(receiver) = inner
                .receivers
                .iter()
                .find(|r| &r.surface == focused && r.resource.is_alive())
        {
            return Some(receiver.resource.clone());
        }
        inner
            .receivers
            .iter()
            .find(|r| r.is_default && r.resource.is_alive())
            .map(|r| r.resource.clone())
    }

    /// The surface behind the default receiver, for the caller to focus.
    pub fn default_surface(&self) -> Option<WlSurface> {
        let mut inner = self.inner.lock().unwrap();
        Self::prune(&mut inner);
        inner
            .receivers
            .iter()
            .find(|r| r.is_default)
            .map(|r| r.surface.clone())
    }

    /// Whether any registered receiver speaks for this surface.
    pub fn has_receiver(&self, surface: &WlSurface) -> bool {
        let inner = self.inner.lock().unwrap();
        inner
            .receivers
            .iter()
            .any(|r| &r.surface == surface && r.resource.is_alive())
    }

    /// Record that the key went down, announcing the hold at once.
    ///
    /// Returns whether a hold actually began — `false` if nothing is registered
    /// to receive it.
    ///
    /// No threshold. The key means one thing now, so there is nothing to wait to
    /// find out, and waiting was not free: every hold paid the tap threshold
    /// before the receiver heard about it, which is a quarter of a second of
    /// speech spoken into a microphone that had not been asked to listen yet.
    pub fn key_pressed(&self, focused: Option<&WlSurface>) -> bool {
        let mut inner = self.inner.lock().unwrap();

        // A second special key going down while one is already held is NOT a
        // new gesture, and must not restart the one in flight.
        //
        // The bindings match by key rather than by keycode — the default
        // `Super_L` answers to Super_R as well — so brushing the other Super
        // mid-sentence arrives here as another press rather than as a joining
        // key. Restarting on it announced a second `hold_start`, which the
        // client reads as a new turn and which throws away everything captured
        // so far; it also re-stamped the clock, so letting go of the second key
        // read as a press and cancelled the dictation outright.
        //
        // Ignored rather than cancelled: the hold in flight is what the user is
        // speaking into.
        if inner.gesture == Gesture::Holding {
            debug!("Special action pressed while a hold is in flight - ignored");
            return false;
        }

        Self::prune(&mut inner);
        let Some(resource) = Self::target(&inner, focused) else {
            debug!("Special action pressed with no receiver registered");
            return false;
        };
        resource.hold_start();
        inner.holding = Some(resource.downgrade());
        // Stamped even though the hold has already been announced: the release
        // is where a press is told from a hold, and it needs to know how long
        // ago this was.
        inner.pressed_at = Some(Instant::now());
        inner.gesture = Gesture::Holding;
        true
    }

    /// Record that the key came up, closing out whatever was in flight.
    ///
    /// A hold that lasted is ended. One that did not was a press: it is
    /// `cancel`led rather than ended, so the receiver throws away what it
    /// captured instead of sending a fraction of a second of nothing off to be
    /// transcribed, and `activate` follows.
    pub fn key_released(&self) -> KeyOutcome {
        let mut inner = self.inner.lock().unwrap();
        let held_for = inner.pressed_at.take().map(|at| at.elapsed());
        // `holding` is set only by an announced hold and cleared by `cancel`, so
        // its presence IS "a hold is live" — there is no second flag to check.
        let resource = inner.holding.take().and_then(|w| w.upgrade().ok());
        inner.gesture = Gesture::Idle;

        let Some(resource) = resource else {
            // A chord cancelled the gesture, nothing was registered when the key
            // went down, or the receiver died mid-hold. No hold left to close.
            return KeyOutcome::Ignored;
        };

        // `None` cannot happen alongside a live hold, and if it somehow did,
        // reading it as a hold is the safe way round: the recording is kept.
        if held_for.is_some_and(|held| held < PRESS_THRESHOLD) {
            resource.cancel();
            resource.activate();
            let surface = inner
                .receivers
                .iter()
                .find(|r| r.resource == resource)
                .map(|r| r.surface.clone());
            debug!(
                ?held_for,
                "Special action released quickly - read as a press"
            );
            return match surface {
                Some(surface) => KeyOutcome::Pressed(surface),
                // The receiver answered the hold but its surface has since gone;
                // `activate` is sent either way, there is just nobody to focus.
                None => KeyOutcome::Ignored,
            };
        }

        resource.hold_end();
        KeyOutcome::HoldEnded
    }

    /// Abandon whatever is in flight, telling a mid-hold receiver to discard.
    pub fn cancel(&self) {
        let mut inner = self.inner.lock().unwrap();
        if inner.gesture == Gesture::Holding
            && let Some(resource) = inner.holding.take().and_then(|w| w.upgrade().ok())
        {
            resource.cancel();
        }
        inner.gesture = Gesture::Idle;
        inner.holding = None;
        inner.pressed_at = None;
    }

    /// Whether a hold is currently in flight.
    pub fn is_holding(&self) -> bool {
        self.inner.lock().unwrap().gesture == Gesture::Holding
    }
}

impl<D> GlobalDispatch<zcosmic_special_action_manager_v1::ZcosmicSpecialActionManagerV1, (), D>
    for SpecialActionState
where
    D: GlobalDispatch<zcosmic_special_action_manager_v1::ZcosmicSpecialActionManagerV1, ()>
        + Dispatch<zcosmic_special_action_manager_v1::ZcosmicSpecialActionManagerV1, ()>
        + Dispatch<zcosmic_special_action_v1::ZcosmicSpecialActionV1, SpecialActionReceiverData>
        + SpecialActionHandler
        + 'static,
{
    fn bind(
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<zcosmic_special_action_manager_v1::ZcosmicSpecialActionManagerV1>,
        _global_data: &(),
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }
}

impl<D> Dispatch<zcosmic_special_action_manager_v1::ZcosmicSpecialActionManagerV1, (), D>
    for SpecialActionState
where
    D: GlobalDispatch<zcosmic_special_action_manager_v1::ZcosmicSpecialActionManagerV1, ()>
        + Dispatch<zcosmic_special_action_manager_v1::ZcosmicSpecialActionManagerV1, ()>
        + Dispatch<zcosmic_special_action_v1::ZcosmicSpecialActionV1, SpecialActionReceiverData>
        + SpecialActionHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        _resource: &zcosmic_special_action_manager_v1::ZcosmicSpecialActionManagerV1,
        request: zcosmic_special_action_manager_v1::Request,
        _data: &(),
        _dh: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_special_action_manager_v1::Request::GetSpecialAction {
                id,
                surface,
                is_default,
            } => {
                let data = SpecialActionReceiverData {
                    surface: surface.clone(),
                    is_default: is_default != 0,
                };
                let resource = data_init.init(id, data.clone());
                state.special_action_state().register(&data, resource);
                state.special_action_receiver_registered(&surface);
            }
            zcosmic_special_action_manager_v1::Request::Destroy => {}
        }
    }
}

impl<D> Dispatch<zcosmic_special_action_v1::ZcosmicSpecialActionV1, SpecialActionReceiverData, D>
    for SpecialActionState
where
    D: GlobalDispatch<zcosmic_special_action_manager_v1::ZcosmicSpecialActionManagerV1, ()>
        + Dispatch<zcosmic_special_action_manager_v1::ZcosmicSpecialActionManagerV1, ()>
        + Dispatch<zcosmic_special_action_v1::ZcosmicSpecialActionV1, SpecialActionReceiverData>
        + SpecialActionHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        resource: &zcosmic_special_action_v1::ZcosmicSpecialActionV1,
        request: zcosmic_special_action_v1::Request,
        _data: &SpecialActionReceiverData,
        _dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        // `destroy` is the only request; the receiver goes with it.
        let zcosmic_special_action_v1::Request::Destroy = request;
        state.special_action_state().unregister(resource);
    }

    fn destroyed(
        state: &mut D,
        _client: smithay::reexports::wayland_server::backend::ClientId,
        resource: &zcosmic_special_action_v1::ZcosmicSpecialActionV1,
        _data: &SpecialActionReceiverData,
    ) {
        state.special_action_state().unregister(resource);
    }
}

/// Compositor hooks for the protocol.
pub trait SpecialActionHandler {
    fn special_action_state(&mut self) -> &mut SpecialActionState;

    /// A surface just registered. Lets the compositor react (e.g. nothing yet).
    fn special_action_receiver_registered(&mut self, _surface: &WlSurface) {}
}

#[macro_export]
macro_rules! delegate_special_action {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::special_action::zcosmic_special_action_manager_v1::ZcosmicSpecialActionManagerV1: ()
        ] => $crate::wayland::protocols::special_action::SpecialActionState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::special_action::zcosmic_special_action_manager_v1::ZcosmicSpecialActionManagerV1: ()
        ] => $crate::wayland::protocols::special_action::SpecialActionState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::special_action::zcosmic_special_action_v1::ZcosmicSpecialActionV1: $crate::wayland::protocols::special_action::SpecialActionReceiverData
        ] => $crate::wayland::protocols::special_action::SpecialActionState);
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The press window is short enough that a deliberate hold is never
    /// mistaken for one, and long enough to be reachable by a human finger.
    #[test]
    fn press_threshold_is_in_a_sane_range() {
        assert!(PRESS_THRESHOLD >= Duration::from_millis(150));
        assert!(PRESS_THRESHOLD <= Duration::from_millis(400));
    }

    /// Nothing in flight is the resting state, so a stray release is ignored
    /// rather than read as either gesture.
    #[test]
    fn gesture_defaults_to_idle() {
        assert_eq!(Gesture::default(), Gesture::Idle);
        assert_eq!(Inner::default().gesture, Gesture::Idle);
        assert!(Inner::default().pressed_at.is_none());
        assert!(Inner::default().holding.is_none());
    }

    /// Which side of the threshold a release falls on is the whole decision, so
    /// pin the comparison rather than only the constant's range.
    #[test]
    fn a_release_is_a_press_only_inside_the_threshold() {
        let press = |held: Duration| held < PRESS_THRESHOLD;
        assert!(press(Duration::from_millis(0)));
        assert!(press(PRESS_THRESHOLD - Duration::from_millis(1)));
        // The boundary itself is a hold: speech is the common case, and keeping
        // a recording that was not wanted costs less than dropping one that was.
        assert!(!press(PRESS_THRESHOLD));
        assert!(!press(Duration::from_secs(2)));
    }
}
