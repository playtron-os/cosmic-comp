// SPDX-License-Identifier: GPL-3.0-only

//! Implementation of the COSMIC home visibility protocol (zcosmic_home_visibility_v1)
//!
//! This protocol allows layer-shell clients to control their visibility based on
//! whether the compositor is in "home" mode. Home mode represents the state where
//! no regular toplevel windows are visible - similar to iOS home screen.
//!
//! Layer surfaces can be:
//! - "always visible" (default): Rendered regardless of home state (e.g., top panel, dock)
//! - "home only": Only rendered when in home mode (e.g., humainos-home desktop)

pub use generated::{zcosmic_home_visibility_manager_v1, zcosmic_home_visibility_v1};

#[allow(non_snake_case, non_upper_case_globals, non_camel_case_types)]
mod generated {
    use smithay::reexports::wayland_server::{self, protocol::*};

    pub mod __interfaces {
        use smithay::reexports::wayland_server::protocol::__interfaces::*;
        use wayland_backend;
        wayland_scanner::generate_interfaces!("resources/protocols/home_visibility.xml");
    }
    use self::__interfaces::*;

    wayland_scanner::generate_server_code!("resources/protocols/home_visibility.xml");
}

use smithay::reexports::wayland_server::{
    Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource, WEnum, Weak,
    backend::GlobalId, protocol::wl_surface::WlSurface,
};
use std::sync::Mutex;
use tracing::{debug, info};

/// Visibility mode for a layer surface
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum VisibilityMode {
    /// Always visible regardless of home state
    #[default]
    Always,
    /// Only visible when compositor is in home mode
    HomeOnly,
    /// Hidden when compositor is in home mode (inverse of HomeOnly)
    HideOnHome,
}

impl From<WEnum<zcosmic_home_visibility_v1::VisibilityMode>> for VisibilityMode {
    fn from(value: WEnum<zcosmic_home_visibility_v1::VisibilityMode>) -> Self {
        match value.into_result() {
            Ok(zcosmic_home_visibility_v1::VisibilityMode::HomeOnly) => VisibilityMode::HomeOnly,
            Ok(zcosmic_home_visibility_v1::VisibilityMode::HideOnHome) => {
                VisibilityMode::HideOnHome
            }
            _ => VisibilityMode::Always,
        }
    }
}

/// User data for the home visibility controller
pub struct HomeVisibilityControllerData {
    /// The wl_surface this controls (should be a layer-shell surface)
    pub surface: Weak<WlSurface>,
    /// Current visibility mode
    pub mode: Mutex<VisibilityMode>,
}

/// State for the home visibility manager protocol
/// Note: The actual home mode state and surface tracking is managed by Shell.
/// This struct manages protocol-specific state (client notifications).
pub struct HomeVisibilityState {
    global: GlobalId,
    /// Whether compositor is currently in home mode (for protocol notifications)
    is_home: Mutex<bool>,
    /// All bound manager instances (to send home_state events)
    managers: Mutex<Vec<Weak<zcosmic_home_visibility_manager_v1::ZcosmicHomeVisibilityManagerV1>>>,
}

impl std::fmt::Debug for HomeVisibilityState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HomeVisibilityState")
            .field("is_home", &self.is_home)
            .finish()
    }
}

impl HomeVisibilityState {
    /// Create a new home visibility manager global
    pub fn new<D>(dh: &DisplayHandle) -> HomeVisibilityState
    where
        D: GlobalDispatch<zcosmic_home_visibility_manager_v1::ZcosmicHomeVisibilityManagerV1, ()>
            + Dispatch<zcosmic_home_visibility_manager_v1::ZcosmicHomeVisibilityManagerV1, ()>
            + Dispatch<
                zcosmic_home_visibility_v1::ZcosmicHomeVisibilityV1,
                HomeVisibilityControllerData,
            > + HomeVisibilityHandler
            + 'static,
    {
        use crate::shell::home_enabled;
        let global = dh.create_global::<
            D,
            zcosmic_home_visibility_manager_v1::ZcosmicHomeVisibilityManagerV1,
            _,
        >(1, ());
        HomeVisibilityState {
            global,
            // Start in home mode only if HOME_ENABLED is set
            is_home: Mutex::new(home_enabled()),
            managers: Mutex::new(Vec::new()),
        }
    }

    /// Get the global ID
    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }

    /// Check if compositor is currently in home mode (protocol state)
    pub fn is_home(&self) -> bool {
        *self.is_home.lock().unwrap()
    }

    /// Set home mode state and notify all clients
    pub fn set_home(&self, new_is_home: bool) {
        let mut guard = self.is_home.lock().unwrap();
        if *guard == new_is_home {
            return;
        }
        *guard = new_is_home;
        drop(guard);

        info!(
            is_home = new_is_home,
            "Home state changed, notifying clients"
        );
        self.broadcast_home_state(new_is_home);
    }

    /// Broadcast home state to all bound managers
    fn broadcast_home_state(&self, is_home: bool) {
        let mut managers = self.managers.lock().unwrap();
        managers.retain(|weak| {
            if let Some(manager) = weak.upgrade().ok() {
                manager.home_state(if is_home { 1 } else { 0 });
                true
            } else {
                false
            }
        });
    }

    /// Register a manager instance
    fn register_manager(
        &self,
        manager: &zcosmic_home_visibility_manager_v1::ZcosmicHomeVisibilityManagerV1,
    ) {
        let mut managers = self.managers.lock().unwrap();
        managers.push(manager.downgrade());

        // Send initial state
        let is_home = *self.is_home.lock().unwrap();
        manager.home_state(if is_home { 1 } else { 0 });
    }
}

/// Handler trait for home visibility protocol
pub trait HomeVisibilityHandler {
    /// Get the home visibility state
    fn home_visibility_state(&self) -> &HomeVisibilityState;

    /// Set a surface's visibility mode in the Shell
    fn set_surface_visibility_mode(&mut self, surface_id: u32, mode: VisibilityMode);

    /// Remove a surface from visibility tracking in the Shell
    fn remove_surface_visibility(&mut self, surface_id: u32);
}

impl<D> GlobalDispatch<zcosmic_home_visibility_manager_v1::ZcosmicHomeVisibilityManagerV1, (), D>
    for HomeVisibilityState
where
    D: GlobalDispatch<zcosmic_home_visibility_manager_v1::ZcosmicHomeVisibilityManagerV1, ()>
        + Dispatch<zcosmic_home_visibility_manager_v1::ZcosmicHomeVisibilityManagerV1, ()>
        + Dispatch<zcosmic_home_visibility_v1::ZcosmicHomeVisibilityV1, HomeVisibilityControllerData>
        + HomeVisibilityHandler
        + 'static,
{
    fn bind(
        _state: &mut D,
        _dh: &DisplayHandle,
        _client: &Client,
        resource: New<zcosmic_home_visibility_manager_v1::ZcosmicHomeVisibilityManagerV1>,
        _global_data: &(),
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }
}

impl<D> Dispatch<zcosmic_home_visibility_manager_v1::ZcosmicHomeVisibilityManagerV1, (), D>
    for HomeVisibilityState
where
    D: Dispatch<zcosmic_home_visibility_manager_v1::ZcosmicHomeVisibilityManagerV1, ()>
        + Dispatch<zcosmic_home_visibility_v1::ZcosmicHomeVisibilityV1, HomeVisibilityControllerData>
        + HomeVisibilityHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        resource: &zcosmic_home_visibility_manager_v1::ZcosmicHomeVisibilityManagerV1,
        request: zcosmic_home_visibility_manager_v1::Request,
        _data: &(),
        _dh: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_home_visibility_manager_v1::Request::Destroy => {
                // Nothing to clean up
            }
            zcosmic_home_visibility_manager_v1::Request::GetHomeVisibility { id, surface } => {
                let controller_data = HomeVisibilityControllerData {
                    surface: surface.downgrade(),
                    mode: Mutex::new(VisibilityMode::Always),
                };
                data_init.init(id, controller_data);

                // Register this manager for home state events
                state.home_visibility_state().register_manager(resource);

                debug!(
                    surface_id = surface.id().protocol_id(),
                    "Created home visibility controller"
                );
            }
        }
    }
}

impl<D>
    Dispatch<zcosmic_home_visibility_v1::ZcosmicHomeVisibilityV1, HomeVisibilityControllerData, D>
    for HomeVisibilityState
where
    D: Dispatch<zcosmic_home_visibility_v1::ZcosmicHomeVisibilityV1, HomeVisibilityControllerData>
        + HomeVisibilityHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        _resource: &zcosmic_home_visibility_v1::ZcosmicHomeVisibilityV1,
        request: zcosmic_home_visibility_v1::Request,
        data: &HomeVisibilityControllerData,
        _dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_home_visibility_v1::Request::Destroy => {
                // Remove surface from home-only tracking in Shell
                if let Some(surface) = data.surface.upgrade().ok() {
                    let surface_id = surface.id().protocol_id();
                    state.remove_surface_visibility(surface_id);
                    debug!(surface_id, "Home visibility controller destroyed");
                }
            }
            zcosmic_home_visibility_v1::Request::SetVisibilityMode { mode } => {
                let visibility_mode = VisibilityMode::from(mode);
                *data.mode.lock().unwrap() = visibility_mode;

                if let Some(surface) = data.surface.upgrade().ok() {
                    let surface_id = surface.id().protocol_id();
                    state.set_surface_visibility_mode(surface_id, visibility_mode);
                    debug!(
                        surface_id,
                        ?visibility_mode,
                        "Surface visibility mode changed"
                    );
                }
            }
        }
    }
}

/// Macro to delegate home visibility protocol to the state
#[macro_export]
macro_rules! delegate_home_visibility {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::home_visibility::zcosmic_home_visibility_manager_v1::ZcosmicHomeVisibilityManagerV1: ()
        ] => $crate::wayland::protocols::home_visibility::HomeVisibilityState);

        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::home_visibility::zcosmic_home_visibility_manager_v1::ZcosmicHomeVisibilityManagerV1: ()
        ] => $crate::wayland::protocols::home_visibility::HomeVisibilityState);

        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::home_visibility::zcosmic_home_visibility_v1::ZcosmicHomeVisibilityV1: $crate::wayland::protocols::home_visibility::HomeVisibilityControllerData
        ] => $crate::wayland::protocols::home_visibility::HomeVisibilityState);
    };
}
