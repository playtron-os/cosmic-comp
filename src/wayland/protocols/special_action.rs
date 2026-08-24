// SPDX-License-Identifier: GPL-3.0-only

//! Implementation of the special action protocol (zcosmic_special_action_v1)
//!
//! The device's special key — the HUMAIN button — is a gesture the compositor
//! has to resolve itself: it is usually bound to Super, which the compositor
//! must also keep for its own chords, so only it can tell a tap from the start
//! of `Super+L`. This protocol carries the *resolved* meaning to a client:
//! `activate` for a tap, `hold_start`/`hold_end` around a hold.
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
use std::time::Duration;
use tracing::debug;

/// How long the key may be held and still count as a tap.
///
/// Also the delay before a hold is announced: the compositor cannot know which
/// gesture it is until either the key comes back up or this elapses.
pub const TAP_THRESHOLD: Duration = Duration::from_millis(250);

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

/// Where the current gesture has got to.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum Gesture {
    /// Key is up; nothing in flight.
    #[default]
    Idle,
    /// Key is down and it is still too early to say what it is.
    Pending,
    /// Key is down past the threshold, and `hold_start` has been sent.
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KeyOutcome {
    /// The key went down. Ask again after [`TAP_THRESHOLD`] via
    /// [`SpecialActionState::hold_elapsed`].
    Pending,
    /// A tap completed; the caller should summon home and focus the receiver.
    Tap,
    /// A hold ended. Nothing further is required of the caller.
    HoldEnded,
    /// Nothing was in flight; ignore the edge.
    Ignored,
}

/// What a release means, given where the gesture had got to.
///
/// A release only counts as a tap if the gesture is still pending — a chord
/// cancels it first, so `Super+L` does not also summon home when Super comes up.
fn outcome_for_release(gesture: Gesture) -> KeyOutcome {
    match gesture {
        Gesture::Pending => KeyOutcome::Tap,
        Gesture::Holding => KeyOutcome::HoldEnded,
        Gesture::Idle => KeyOutcome::Ignored,
    }
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

    /// Record that the key went down.
    pub fn key_pressed(&self) -> KeyOutcome {
        let mut inner = self.inner.lock().unwrap();
        inner.gesture = Gesture::Pending;
        KeyOutcome::Pending
    }

    /// Record that the key came up, sending `hold_end` if a hold was in flight.
    pub fn key_released(&self) -> KeyOutcome {
        let mut inner = self.inner.lock().unwrap();
        let outcome = outcome_for_release(inner.gesture);
        if outcome == KeyOutcome::HoldEnded
            && let Some(resource) = inner.holding.take().and_then(|w| w.upgrade().ok())
        {
            resource.hold_end();
        }
        inner.gesture = Gesture::Idle;
        inner.holding = None;
        outcome
    }

    /// Called once [`TAP_THRESHOLD`] has passed with the key still down.
    ///
    /// Sends `hold_start` to the routed receiver and returns whether a hold
    /// actually began — `false` if the key came back up first, or if nothing is
    /// registered to receive it.
    pub fn hold_elapsed(&self, focused: Option<&WlSurface>) -> bool {
        let mut inner = self.inner.lock().unwrap();
        if inner.gesture != Gesture::Pending {
            return false;
        }
        Self::prune(&mut inner);
        let Some(resource) = Self::target(&inner, focused) else {
            debug!("Special action held with no receiver registered");
            return false;
        };
        resource.hold_start();
        inner.holding = Some(resource.downgrade());
        inner.gesture = Gesture::Holding;
        true
    }

    /// Which surface a gesture would go to right now, without sending anything.
    ///
    /// The caller needs this before it acts: whoever receives the action also
    /// needs the keyboard, or the caret they are about to take sits on a surface
    /// the keystrokes never reach.
    pub fn routed_surface(&self, focused: Option<&WlSurface>) -> Option<WlSurface> {
        let mut inner = self.inner.lock().unwrap();
        Self::prune(&mut inner);
        let resource = Self::target(&inner, focused)?;
        inner
            .receivers
            .iter()
            .find(|r| r.resource == resource)
            .map(|r| r.surface.clone())
    }

    /// Send `activate` for a completed tap. Returns the surface that got it.
    pub fn send_activate(&self, focused: Option<&WlSurface>) -> Option<WlSurface> {
        let mut inner = self.inner.lock().unwrap();
        Self::prune(&mut inner);
        let resource = Self::target(&inner, focused)?;
        resource.activate();
        inner
            .receivers
            .iter()
            .find(|r| r.resource == resource)
            .map(|r| r.surface.clone())
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

    /// A release only summons home when the gesture is still pending. A chord
    /// cancels first (back to `Idle`), so `Super+L` must not also fire a tap
    /// when Super finally comes up.
    #[test]
    fn only_a_pending_gesture_releases_into_a_tap() {
        assert_eq!(outcome_for_release(Gesture::Pending), KeyOutcome::Tap);
        assert_eq!(outcome_for_release(Gesture::Idle), KeyOutcome::Ignored);
        assert_eq!(outcome_for_release(Gesture::Holding), KeyOutcome::HoldEnded);
    }

    /// The tap window is short enough to feel like a press, long enough that a
    /// deliberate hold is not mistaken for one.
    #[test]
    fn tap_threshold_is_in_a_sane_range() {
        assert!(TAP_THRESHOLD >= Duration::from_millis(150));
        assert!(TAP_THRESHOLD <= Duration::from_millis(400));
    }

    /// Nothing in flight is the resting state, so a stray release is ignored
    /// rather than read as a tap.
    #[test]
    fn gesture_defaults_to_idle() {
        assert_eq!(Gesture::default(), Gesture::Idle);
        assert_eq!(Inner::default().gesture, Gesture::Idle);
        assert_eq!(
            outcome_for_release(Inner::default().gesture),
            KeyOutcome::Ignored
        );
    }
}
