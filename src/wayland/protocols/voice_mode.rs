// SPDX-License-Identifier: GPL-3.0-only

//! Implementation of the COSMIC voice mode protocol (zcosmic_voice_mode_v1)
//!
//! This protocol allows clients to register as voice input receivers.
//! The compositor has full control over the voice mode orb - clients receive
//! start/stop/cancel events based on focus.

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
    backend::GlobalId,
};
use std::sync::Mutex;
use tracing::{debug, info, warn};

/// Orb display state
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum OrbState {
    #[default]
    Hidden,
    Floating,
    Attached,
}

impl From<u32> for OrbState {
    fn from(value: u32) -> Self {
        match value {
            1 => OrbState::Floating,
            2 => OrbState::Attached,
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
        }
    }
}

impl From<OrbState> for zcosmic_voice_mode_v1::OrbState {
    fn from(state: OrbState) -> Self {
        match state {
            OrbState::Hidden => zcosmic_voice_mode_v1::OrbState::Hidden,
            OrbState::Floating => zcosmic_voice_mode_v1::OrbState::Floating,
            OrbState::Attached => zcosmic_voice_mode_v1::OrbState::Attached,
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

/// User data for a voice mode receiver
pub struct VoiceModeReceiverData {
    /// App ID for this receiver
    pub app_id: String,
    /// Whether this is the default receiver
    pub is_default: bool,
}

/// A registered voice input receiver
#[derive(Debug, Clone)]
pub struct VoiceReceiver {
    /// Weak reference to the protocol object
    pub resource: Weak<zcosmic_voice_mode_v1::ZcosmicVoiceModeV1>,
    /// App ID
    pub app_id: String,
    /// Whether this is the default receiver
    pub is_default: bool,
}

/// State for the voice mode manager protocol
pub struct VoiceModeState {
    global: GlobalId,
    /// Registered receivers
    receivers: Mutex<Vec<VoiceReceiver>>,
    /// Current orb state
    current_orb_state: Mutex<OrbState>,
    /// Currently active receiver (receiving events)
    active_receiver_app_id: Mutex<Option<String>>,
    /// Default receiver app ID from env var
    default_receiver_app_id: Option<String>,
}

impl std::fmt::Debug for VoiceModeState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("VoiceModeState")
            .field("receivers_count", &self.receivers.lock().unwrap().len())
            .field("current_orb_state", &self.current_orb_state.lock().unwrap())
            .field(
                "active_receiver",
                &self.active_receiver_app_id.lock().unwrap(),
            )
            .field("default_receiver", &self.default_receiver_app_id)
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

        // Get default receiver from env var
        let default_receiver_app_id = std::env::var("COSMIC_VOICE_DEFAULT_RECEIVER").ok();
        if let Some(ref app_id) = default_receiver_app_id {
            info!(app_id = %app_id, "Default voice receiver configured");
        }

        VoiceModeState {
            global,
            receivers: Mutex::new(Vec::new()),
            current_orb_state: Mutex::new(OrbState::Hidden),
            active_receiver_app_id: Mutex::new(None),
            default_receiver_app_id,
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

    /// Get the default receiver app ID
    pub fn default_receiver_app_id(&self) -> Option<&str> {
        self.default_receiver_app_id.as_deref()
    }

    /// Get the currently active receiver app ID
    pub fn active_receiver_app_id(&self) -> Option<String> {
        self.active_receiver_app_id.lock().unwrap().clone()
    }

    /// Find a receiver by app_id
    fn find_receiver(&self, app_id: &str) -> Option<VoiceReceiver> {
        let receivers = self.receivers.lock().unwrap();
        receivers.iter().find(|r| r.app_id == app_id).cloned()
    }

    /// Find the default receiver
    fn find_default_receiver(&self) -> Option<VoiceReceiver> {
        let receivers = self.receivers.lock().unwrap();
        receivers.iter().find(|r| r.is_default).cloned()
    }

    /// Send start event to a specific receiver
    pub fn send_start(&self, app_id: &str, orb_state: OrbState) -> bool {
        *self.current_orb_state.lock().unwrap() = orb_state;
        *self.active_receiver_app_id.lock().unwrap() = Some(app_id.to_string());

        if let Some(receiver) = self.find_receiver(app_id) {
            if let Ok(resource) = receiver.resource.upgrade() {
                info!(app_id = %app_id, ?orb_state, "Sending voice start to receiver");
                resource.start(orb_state.into());
                return true;
            }
        }
        warn!(app_id = %app_id, "No receiver found for app_id");
        false
    }

    /// Send start event to the default receiver
    pub fn send_start_to_default(&self, orb_state: OrbState) -> bool {
        *self.current_orb_state.lock().unwrap() = orb_state;

        if let Some(receiver) = self.find_default_receiver() {
            *self.active_receiver_app_id.lock().unwrap() = Some(receiver.app_id.clone());
            if let Ok(resource) = receiver.resource.upgrade() {
                info!(app_id = %receiver.app_id, ?orb_state, "Sending voice start to default receiver");
                resource.start(orb_state.into());
                return true;
            }
        }
        warn!("No default receiver registered");
        false
    }

    /// Send stop event to the active receiver
    pub fn send_stop(&self) {
        let active_app_id = self.active_receiver_app_id.lock().unwrap().clone();
        *self.current_orb_state.lock().unwrap() = OrbState::Hidden;
        *self.active_receiver_app_id.lock().unwrap() = None;

        if let Some(app_id) = active_app_id {
            if let Some(receiver) = self.find_receiver(&app_id) {
                if let Ok(resource) = receiver.resource.upgrade() {
                    info!(app_id = %app_id, "Sending voice stop to receiver");
                    resource.stop();
                    return;
                }
            }
        }
        debug!("No active receiver to send stop to");
    }

    /// Send cancel event to the active receiver
    pub fn send_cancel(&self) {
        let active_app_id = self.active_receiver_app_id.lock().unwrap().clone();
        *self.current_orb_state.lock().unwrap() = OrbState::Hidden;
        *self.active_receiver_app_id.lock().unwrap() = None;

        if let Some(app_id) = active_app_id {
            if let Some(receiver) = self.find_receiver(&app_id) {
                if let Ok(resource) = receiver.resource.upgrade() {
                    info!(app_id = %app_id, "Sending voice cancel to receiver");
                    resource.cancel();
                    return;
                }
            }
        }
        debug!("No active receiver to send cancel to");
    }

    /// Send orb_attached event to a receiver
    pub fn send_orb_attached(&self, app_id: &str, x: i32, y: i32, width: i32, height: i32) {
        if let Some(receiver) = self.find_receiver(app_id) {
            if let Ok(resource) = receiver.resource.upgrade() {
                resource.orb_attached(x, y, width, height);
            }
        }
    }

    /// Send orb_detached event to a receiver
    pub fn send_orb_detached(&self, app_id: &str) {
        if let Some(receiver) = self.find_receiver(app_id) {
            if let Ok(resource) = receiver.resource.upgrade() {
                resource.orb_detached();
            }
        }
    }

    /// Add a receiver
    fn add_receiver(&self, receiver: VoiceReceiver) {
        info!(
            app_id = %receiver.app_id,
            is_default = receiver.is_default,
            "Registering voice receiver"
        );
        self.receivers.lock().unwrap().push(receiver);
    }

    /// Check if an app_id has a registered receiver
    pub fn has_receiver(&self, app_id: &str) -> bool {
        let receivers = self.receivers.lock().unwrap();
        receivers
            .iter()
            .any(|r| r.app_id == app_id && r.resource.upgrade().is_ok())
    }
}

/// Handler trait for voice mode events (compositor-side control)
pub trait VoiceModeHandler {
    /// Get the voice mode state
    fn voice_mode_state(&mut self) -> &mut VoiceModeState;

    /// Called when voice mode should be activated (compositor decides)
    /// The handler should determine which receiver to send start to
    /// Returns the orb state based on which receiver was activated
    fn activate_voice_mode(&mut self, focused_app_id: Option<&str>) -> OrbState;

    /// Called when voice mode should be deactivated (key released)
    fn deactivate_voice_mode(&mut self);

    /// Called when voice mode should be cancelled (focus change, escape, etc)
    fn cancel_voice_mode(&mut self);
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
            zcosmic_voice_mode_manager_v1::Request::RegisterReceiver { id, app_id } => {
                debug!(app_id = %app_id, "Client registering as voice receiver");

                // Check if this is the default receiver
                let is_default = state
                    .voice_mode_state()
                    .default_receiver_app_id
                    .as_ref()
                    .map(|default| default == &app_id)
                    .unwrap_or(false);

                let receiver_data = VoiceModeReceiverData {
                    app_id: app_id.clone(),
                    is_default,
                };
                let resource = data_init.init(id, receiver_data);

                // Add to receivers list
                let receiver = VoiceReceiver {
                    resource: resource.downgrade(),
                    app_id: app_id.clone(),
                    is_default,
                };
                state.voice_mode_state().add_receiver(receiver);

                // Send registered confirmation
                resource.registered(if is_default { 1 } else { 0 });

                info!(
                    app_id = %app_id,
                    is_default,
                    "Voice receiver registered"
                );
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
        _state: &mut D,
        _client: &Client,
        _resource: &zcosmic_voice_mode_v1::ZcosmicVoiceModeV1,
        request: zcosmic_voice_mode_v1::Request,
        data: &VoiceModeReceiverData,
        _dhandle: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_voice_mode_v1::Request::Destroy => {
                debug!(app_id = %data.app_id, "Voice receiver destroyed");
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
