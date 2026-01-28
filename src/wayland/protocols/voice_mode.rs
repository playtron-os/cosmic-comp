// SPDX-License-Identifier: GPL-3.0-only

//! Implementation of the COSMIC voice mode protocol (zcosmic_voice_mode_v1)
//!
//! This protocol allows clients to register surfaces (windows) as voice input receivers.
//! The compositor has full control over the voice mode orb - clients receive
//! start/stop/cancel events based on surface focus.

pub use generated::{zcosmic_voice_mode_manager_v1, zcosmic_voice_mode_v1};

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
        wayland_scanner::generate_interfaces!("resources/protocols/voice_mode.xml");
    }
    use self::__interfaces::*;

    wayland_scanner::generate_server_code!("resources/protocols/voice_mode.xml");
}

use smithay::reexports::wayland_server::{
    Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource, Weak,
    backend::GlobalId, protocol::wl_surface::WlSurface,
};
use std::sync::Mutex;
use std::sync::atomic::{AtomicU32, Ordering};
use std::time::Instant;
use tracing::{debug, info, warn};

/// Orb display state
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum OrbState {
    #[default]
    Hidden,
    Floating,
    Attached,
    /// Orb is frozen in place waiting for transcription (non-interruptible)
    Frozen,
    /// Orb is transitioning to attached with fade animation (non-interruptible)
    Transitioning,
}

impl OrbState {
    /// Check if this state is non-interruptible (hotkeys should be ignored)
    pub fn is_non_interruptible(&self) -> bool {
        matches!(self, OrbState::Frozen | OrbState::Transitioning)
    }
}

impl From<u32> for OrbState {
    fn from(value: u32) -> Self {
        match value {
            1 => OrbState::Floating,
            2 => OrbState::Attached,
            3 => OrbState::Frozen,
            4 => OrbState::Transitioning,
            _ => OrbState::Hidden,
        }
    }
}

impl From<OrbState> for u32 {
    fn from(state: OrbState) -> Self {
        match state {
            OrbState::Hidden => 0,
            OrbState::Floating => 1,
            OrbState::Attached => 2,
            OrbState::Frozen => 3,
            OrbState::Transitioning => 4,
        }
    }
}

impl From<OrbState> for zcosmic_voice_mode_v1::OrbState {
    fn from(state: OrbState) -> Self {
        match state {
            OrbState::Hidden => zcosmic_voice_mode_v1::OrbState::Hidden,
            OrbState::Floating => zcosmic_voice_mode_v1::OrbState::Floating,
            OrbState::Attached => zcosmic_voice_mode_v1::OrbState::Attached,
            OrbState::Frozen => zcosmic_voice_mode_v1::OrbState::Frozen,
            OrbState::Transitioning => zcosmic_voice_mode_v1::OrbState::Transitioning,
        }
    }
}

/// Voice input state (internal tracking)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum VoiceState {
    #[default]
    Idle,
    Recording,
}

/// User data for a voice mode receiver (per-surface)
pub struct VoiceModeReceiverData {
    /// The surface this receiver is registered for
    pub surface: WlSurface,
    /// Whether this is the default receiver
    pub is_default: bool,
}

/// A registered voice input receiver (per-surface)
#[derive(Debug, Clone)]
pub struct VoiceReceiver {
    /// Weak reference to the protocol object
    pub resource: Weak<zcosmic_voice_mode_v1::ZcosmicVoiceModeV1>,
    /// Weak reference to the wl_surface
    pub surface: Weak<WlSurface>,
    /// Whether this is the default receiver
    pub is_default: bool,
}

/// Pending stop state - waiting for client to ack will_stop
#[derive(Debug, Clone)]
pub struct PendingStop {
    /// Serial sent with will_stop
    pub serial: u32,
    /// Time when will_stop was sent
    pub sent_at: Instant,
}

/// Timeout for will_stop ack (200ms - needs to account for event loop latency)
pub const WILL_STOP_TIMEOUT_MS: u128 = 200;

/// State for the voice mode manager protocol
pub struct VoiceModeState {
    global: GlobalId,
    /// Registered receivers (one per surface)
    receivers: Mutex<Vec<VoiceReceiver>>,
    /// Current orb state
    current_orb_state: Mutex<OrbState>,
    /// Currently active receiver surface
    active_receiver_surface: Mutex<Option<Weak<WlSurface>>>,
    /// Current audio level from client (0-1000)
    audio_level: Mutex<u32>,
    /// Serial counter for will_stop events
    serial_counter: AtomicU32,
    /// Pending stop state (waiting for ack_stop)
    pending_stop: Mutex<Option<PendingStop>>,
}

impl std::fmt::Debug for VoiceModeState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("VoiceModeState")
            .field("receivers_count", &self.receivers.lock().unwrap().len())
            .field("current_orb_state", &self.current_orb_state.lock().unwrap())
            .field(
                "has_active_receiver",
                &self.active_receiver_surface.lock().unwrap().is_some(),
            )
            .field("audio_level", &self.audio_level.lock().unwrap())
            .field("pending_stop", &self.pending_stop.lock().unwrap())
            .finish()
    }
}

impl VoiceModeState {
    /// Create a new voice mode manager global
    pub fn new<D>(dh: &DisplayHandle) -> VoiceModeState
    where
        D: GlobalDispatch<zcosmic_voice_mode_manager_v1::ZcosmicVoiceModeManagerV1, ()>
            + Dispatch<zcosmic_voice_mode_manager_v1::ZcosmicVoiceModeManagerV1, ()>
            + Dispatch<zcosmic_voice_mode_v1::ZcosmicVoiceModeV1, VoiceModeReceiverData>
            + VoiceModeHandler
            + 'static,
    {
        let global = dh
            .create_global::<D, zcosmic_voice_mode_manager_v1::ZcosmicVoiceModeManagerV1, _>(1, ());

        VoiceModeState {
            global,
            receivers: Mutex::new(Vec::new()),
            current_orb_state: Mutex::new(OrbState::Hidden),
            active_receiver_surface: Mutex::new(None),
            audio_level: Mutex::new(0),
            serial_counter: AtomicU32::new(1),
            pending_stop: Mutex::new(None),
        }
    }

    /// Get the global ID
    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }

    /// Check if voice mode is currently active
    pub fn is_active(&self) -> bool {
        *self.current_orb_state.lock().unwrap() != OrbState::Hidden
    }

    /// Get current orb state
    pub fn orb_state(&self) -> OrbState {
        *self.current_orb_state.lock().unwrap()
    }

    /// Set current orb state
    pub fn set_orb_state(&self, state: OrbState) {
        *self.current_orb_state.lock().unwrap() = state;
    }

    /// Find a receiver by surface
    fn find_receiver_by_surface(&self, surface: &WlSurface) -> Option<VoiceReceiver> {
        let receivers = self.receivers.lock().unwrap();
        receivers
            .iter()
            .find(|r| r.surface.upgrade().map(|s| s == *surface).unwrap_or(false))
            .cloned()
    }

    /// Find the default receiver
    fn find_default_receiver(&self) -> Option<VoiceReceiver> {
        let receivers = self.receivers.lock().unwrap();
        receivers.iter().find(|r| r.is_default).cloned()
    }

    /// Check if a surface has a registered receiver
    pub fn has_receiver_for_surface(&self, surface: &WlSurface) -> bool {
        self.find_receiver_by_surface(surface).is_some()
    }

    /// Send start event to a specific surface's receiver
    pub fn send_start_to_surface(&self, surface: &WlSurface, orb_state: OrbState) -> bool {
        *self.current_orb_state.lock().unwrap() = orb_state;

        if let Some(receiver) = self.find_receiver_by_surface(surface) {
            *self.active_receiver_surface.lock().unwrap() = Some(receiver.surface.clone());
            if let Ok(resource) = receiver.resource.upgrade() {
                info!(?orb_state, "Sending voice start to surface receiver");
                resource.start(orb_state.into());
                return true;
            }
        }
        warn!("No receiver found for surface");
        false
    }

    /// Send start event to the default receiver
    pub fn send_start_to_default(&self, orb_state: OrbState) -> bool {
        *self.current_orb_state.lock().unwrap() = orb_state;

        if let Some(receiver) = self.find_default_receiver() {
            *self.active_receiver_surface.lock().unwrap() = Some(receiver.surface.clone());
            if let Ok(resource) = receiver.resource.upgrade() {
                info!(?orb_state, "Sending voice start to default receiver");
                resource.start(orb_state.into());
                return true;
            }
        }
        warn!("No default receiver registered");
        false
    }

    /// Send stop event to the active receiver (called after ack_stop with freeze=false, or timeout)
    pub fn send_stop(&self) {
        let active_surface = self.active_receiver_surface.lock().unwrap().clone();
        *self.current_orb_state.lock().unwrap() = OrbState::Hidden;
        *self.active_receiver_surface.lock().unwrap() = None;
        *self.pending_stop.lock().unwrap() = None;
        self.reset_audio_level();

        if let Some(surface_weak) = active_surface {
            if let Ok(surface) = surface_weak.upgrade() {
                if let Some(receiver) = self.find_receiver_by_surface(&surface) {
                    if let Ok(resource) = receiver.resource.upgrade() {
                        info!("Sending voice stop to receiver");
                        resource.stop();
                        return;
                    }
                }
            }
        }
        debug!("No active receiver to send stop to");
    }

    /// Send will_stop event to the active receiver and start waiting for ack
    /// Returns the serial used, or None if no active receiver
    pub fn send_will_stop(&self) -> Option<u32> {
        let active_surface = self.active_receiver_surface.lock().unwrap().clone();

        if let Some(surface_weak) = active_surface {
            if let Ok(surface) = surface_weak.upgrade() {
                if let Some(receiver) = self.find_receiver_by_surface(&surface) {
                    if let Ok(resource) = receiver.resource.upgrade() {
                        let serial = self.serial_counter.fetch_add(1, Ordering::SeqCst);
                        info!(serial, "Sending will_stop to receiver");
                        resource.will_stop(serial);

                        // Store pending stop state
                        *self.pending_stop.lock().unwrap() = Some(PendingStop {
                            serial,
                            sent_at: Instant::now(),
                        });

                        return Some(serial);
                    }
                }
            }
        }
        debug!("No active receiver to send will_stop to");
        None
    }

    /// Check if we have a pending stop that has timed out
    pub fn has_pending_stop_timeout(&self) -> bool {
        if let Some(ref pending) = *self.pending_stop.lock().unwrap() {
            pending.sent_at.elapsed().as_millis() >= WILL_STOP_TIMEOUT_MS
        } else {
            false
        }
    }

    /// Check if we have any pending stop (waiting for ack)
    pub fn has_pending_stop(&self) -> bool {
        self.pending_stop.lock().unwrap().is_some()
    }

    /// Handle ack_stop from client
    /// Returns true if serial matches and freeze was requested
    pub fn handle_ack_stop(&self, serial: u32, freeze: bool) -> Option<bool> {
        let mut pending = self.pending_stop.lock().unwrap();
        if let Some(ref pending_stop) = *pending {
            if pending_stop.serial == serial {
                info!(serial, freeze, "Received ack_stop from client");
                *pending = None;
                return Some(freeze);
            } else {
                warn!(
                    expected = pending_stop.serial,
                    received = serial,
                    "ack_stop serial mismatch"
                );
            }
        } else {
            warn!(serial, "ack_stop received but no pending stop");
        }
        None
    }

    /// Clear pending stop without sending stop (used when freezing)
    pub fn clear_pending_stop(&self) {
        *self.pending_stop.lock().unwrap() = None;
    }

    /// Send cancel event to the active receiver
    pub fn send_cancel(&self) {
        let active_surface = self.active_receiver_surface.lock().unwrap().clone();
        *self.current_orb_state.lock().unwrap() = OrbState::Hidden;
        *self.active_receiver_surface.lock().unwrap() = None;
        *self.pending_stop.lock().unwrap() = None;
        self.reset_audio_level();

        if let Some(surface_weak) = active_surface {
            if let Ok(surface) = surface_weak.upgrade() {
                if let Some(receiver) = self.find_receiver_by_surface(&surface) {
                    if let Ok(resource) = receiver.resource.upgrade() {
                        info!("Sending voice cancel to receiver");
                        resource.cancel();
                        return;
                    }
                }
            }
        }
        debug!("No active receiver to send cancel to");
    }

    /// Send orb_attached event to a surface's receiver
    pub fn send_orb_attached(&self, surface: &WlSurface, x: i32, y: i32, width: i32, height: i32) {
        if let Some(receiver) = self.find_receiver_by_surface(surface) {
            if let Ok(resource) = receiver.resource.upgrade() {
                resource.orb_attached(x, y, width, height);
            }
        }
    }

    /// Send orb_detached event to a surface's receiver
    pub fn send_orb_detached(&self, surface: &WlSurface) {
        if let Some(receiver) = self.find_receiver_by_surface(surface) {
            if let Ok(resource) = receiver.resource.upgrade() {
                resource.orb_detached();
            }
        }
    }

    /// Add a receiver
    fn add_receiver(&self, receiver: VoiceReceiver) {
        info!(
            is_default = receiver.is_default,
            "Registering voice receiver for surface"
        );
        // If this is a default receiver, remove any existing default
        if receiver.is_default {
            let mut receivers = self.receivers.lock().unwrap();
            receivers.retain(|r| !r.is_default);
            receivers.push(receiver);
        } else {
            self.receivers.lock().unwrap().push(receiver);
        }
    }

    /// Clean up dead receivers
    pub fn cleanup_dead_receivers(&self) {
        let mut receivers = self.receivers.lock().unwrap();
        receivers.retain(|r| r.resource.upgrade().is_ok() && r.surface.upgrade().is_ok());
    }

    /// Get current audio level (0-1000)
    pub fn audio_level(&self) -> u32 {
        *self.audio_level.lock().unwrap()
    }

    /// Get current audio level as a normalized float (0.0-1.0)
    pub fn audio_level_normalized(&self) -> f32 {
        (*self.audio_level.lock().unwrap() as f32 / 1000.0).clamp(0.0, 1.0)
    }

    /// Set audio level (called from protocol handler)
    fn set_audio_level(&self, level: u32) {
        *self.audio_level.lock().unwrap() = level.min(1000);
    }

    /// Reset audio level to 0 (called when voice mode ends)
    pub fn reset_audio_level(&self) {
        *self.audio_level.lock().unwrap() = 0;
    }
}

/// Handler trait for voice mode events (compositor-side control)
pub trait VoiceModeHandler {
    /// Get the voice mode state
    fn voice_mode_state(&mut self) -> &mut VoiceModeState;

    /// Called when voice mode should be activated (compositor decides)
    /// The handler should determine which receiver to send start to
    /// Returns the orb state based on which receiver was activated
    fn activate_voice_mode(&mut self, focused_surface: Option<&WlSurface>) -> OrbState;

    /// Called when voice mode should be deactivated (key released)
    /// This should send will_stop and wait for ack_stop
    fn deactivate_voice_mode(&mut self);

    /// Called when voice mode should be cancelled (focus change, escape, etc)
    fn cancel_voice_mode(&mut self);

    /// Called when client acknowledges will_stop with freeze=true
    /// Orb stops pulsing and stays visible. Compositor handles auto-transition on focus change.
    fn freeze_orb(&mut self);

    /// Called when client acknowledges will_stop with freeze=false, or timeout
    /// Proceeds with hiding the orb
    fn complete_deactivation(&mut self);

    /// Called periodically to check for pending stop timeout
    fn check_pending_stop_timeout(&mut self);

    /// Called when a new voice receiver is registered
    /// If orb is frozen, this can trigger transition to the new window
    fn on_voice_receiver_registered(&mut self, surface: &WlSurface);

    /// Called when client sends dismiss request to hide the frozen orb
    /// This happens when transcription completes with empty result or error
    fn dismiss_orb(&mut self);
}

impl<D> GlobalDispatch<zcosmic_voice_mode_manager_v1::ZcosmicVoiceModeManagerV1, (), D>
    for VoiceModeState
where
    D: GlobalDispatch<zcosmic_voice_mode_manager_v1::ZcosmicVoiceModeManagerV1, ()>
        + Dispatch<zcosmic_voice_mode_manager_v1::ZcosmicVoiceModeManagerV1, ()>
        + Dispatch<zcosmic_voice_mode_v1::ZcosmicVoiceModeV1, VoiceModeReceiverData>
        + VoiceModeHandler
        + 'static,
{
    fn bind(
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<zcosmic_voice_mode_manager_v1::ZcosmicVoiceModeManagerV1>,
        _global_data: &(),
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }
}

impl<D> Dispatch<zcosmic_voice_mode_manager_v1::ZcosmicVoiceModeManagerV1, (), D> for VoiceModeState
where
    D: GlobalDispatch<zcosmic_voice_mode_manager_v1::ZcosmicVoiceModeManagerV1, ()>
        + Dispatch<zcosmic_voice_mode_manager_v1::ZcosmicVoiceModeManagerV1, ()>
        + Dispatch<zcosmic_voice_mode_v1::ZcosmicVoiceModeV1, VoiceModeReceiverData>
        + VoiceModeHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        _resource: &zcosmic_voice_mode_manager_v1::ZcosmicVoiceModeManagerV1,
        request: zcosmic_voice_mode_manager_v1::Request,
        _data: &(),
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_voice_mode_manager_v1::Request::GetVoiceMode {
                id,
                surface,
                is_default,
            } => {
                let is_default = is_default != 0;
                debug!(is_default, "Client registering surface as voice receiver");

                let receiver_data = VoiceModeReceiverData {
                    surface: surface.clone(),
                    is_default,
                };
                let resource = data_init.init(id, receiver_data);

                // Add to receivers list
                let receiver = VoiceReceiver {
                    resource: resource.downgrade(),
                    surface: surface.downgrade(),
                    is_default,
                };
                state.voice_mode_state().add_receiver(receiver);

                info!(is_default, "Voice receiver registered for surface");

                // Notify handler that a new receiver was registered
                // This allows transitioning frozen orb to the new window
                state.on_voice_receiver_registered(&surface);
            }
            zcosmic_voice_mode_manager_v1::Request::Destroy => {}
        }
    }
}

impl<D> Dispatch<zcosmic_voice_mode_v1::ZcosmicVoiceModeV1, VoiceModeReceiverData, D>
    for VoiceModeState
where
    D: GlobalDispatch<zcosmic_voice_mode_manager_v1::ZcosmicVoiceModeManagerV1, ()>
        + Dispatch<zcosmic_voice_mode_manager_v1::ZcosmicVoiceModeManagerV1, ()>
        + Dispatch<zcosmic_voice_mode_v1::ZcosmicVoiceModeV1, VoiceModeReceiverData>
        + VoiceModeHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        _resource: &zcosmic_voice_mode_v1::ZcosmicVoiceModeV1,
        request: zcosmic_voice_mode_v1::Request,
        data: &VoiceModeReceiverData,
        _dhandle: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_voice_mode_v1::Request::Destroy => {
                debug!(is_default = data.is_default, "Voice receiver destroyed");
            }
            zcosmic_voice_mode_v1::Request::SetAudioLevel { level } => {
                // Only accept audio level updates if voice mode is active
                if state.voice_mode_state().is_active() {
                    state.voice_mode_state().set_audio_level(level);
                }
            }
            zcosmic_voice_mode_v1::Request::AckStop { serial, freeze } => {
                let freeze = freeze != 0;
                info!(serial, freeze, "Client ack_stop received");

                if let Some(should_freeze) =
                    state.voice_mode_state().handle_ack_stop(serial, freeze)
                {
                    if should_freeze {
                        // Client wants to freeze the orb
                        state.freeze_orb();
                    } else {
                        // Client wants to proceed with hide
                        state.complete_deactivation();
                    }
                }
            }
            zcosmic_voice_mode_v1::Request::Dismiss => {
                info!("Client dismiss request received - hiding frozen orb");
                // Only valid when orb is frozen
                if state.voice_mode_state().orb_state() == OrbState::Frozen {
                    state.dismiss_orb();
                } else {
                    debug!("Dismiss request ignored - orb not in frozen state");
                }
            }
        }
    }
}

/// Macro to delegate voice mode protocol handling
#[macro_export]
macro_rules! delegate_voice_mode {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::voice_mode::zcosmic_voice_mode_manager_v1::ZcosmicVoiceModeManagerV1: ()
        ] => $crate::wayland::protocols::voice_mode::VoiceModeState);

        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::voice_mode::zcosmic_voice_mode_manager_v1::ZcosmicVoiceModeManagerV1: ()
        ] => $crate::wayland::protocols::voice_mode::VoiceModeState);

        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::voice_mode::zcosmic_voice_mode_v1::ZcosmicVoiceModeV1: $crate::wayland::protocols::voice_mode::VoiceModeReceiverData
        ] => $crate::wayland::protocols::voice_mode::VoiceModeState);
    };
}

pub use delegate_voice_mode;
