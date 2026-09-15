// SPDX-License-Identifier: GPL-3.0-only

//! An explicit, per-toplevel opt-in; never infer native support from an app ID.

use std::sync::Mutex;

use crate::state::State;
use smithay::{
    reexports::wayland_server::{
        Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource, WEnum,
        backend::GlobalId, protocol::wl_surface::WlSurface,
    },
    wayland::{
        compositor::{get_role, with_states},
        shell::xdg::{XDG_TOPLEVEL_ROLE, XdgToplevelSurfaceRoleAttributes},
    },
};

#[allow(non_snake_case, non_upper_case_globals, non_camel_case_types)]
mod generated {
    use smithay::reexports::wayland_server::{self, protocol::*};
    pub mod __interfaces {
        use smithay::reexports::wayland_server::protocol::__interfaces::*;
        use wayland_backend;
        wayland_scanner::generate_interfaces!("resources/protocols/kora-halo-header-v1.xml");
    }
    use self::__interfaces::*;
    wayland_scanner::generate_server_code!("resources/protocols/kora-halo-header-v1.xml");
}

use generated::kora_halo_header_manager_v1::{self as manager, KoraHaloHeaderManagerV1};

#[derive(Default)]
struct HaloHeaderMode(Mutex<bool>);

pub(crate) fn allows_overlay(surface: &WlSurface) -> bool {
    with_states(surface, |states| {
        states
            .data_map
            .get::<HaloHeaderMode>()
            .is_some_and(|mode| *mode.0.lock().unwrap())
    })
}

#[derive(Debug)]
pub struct HaloHeaderState {
    pub global: GlobalId,
}

impl HaloHeaderState {
    pub fn new(dh: &DisplayHandle) -> Self {
        Self {
            global: dh.create_global::<State, KoraHaloHeaderManagerV1, _>(1, ()),
        }
    }
}

impl GlobalDispatch<KoraHaloHeaderManagerV1, ()> for State {
    fn bind(
        _state: &mut Self,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<KoraHaloHeaderManagerV1>,
        _data: &(),
        data_init: &mut DataInit<'_, Self>,
    ) {
        data_init.init(resource, ());
    }
}

impl Dispatch<KoraHaloHeaderManagerV1, ()> for State {
    fn request(
        _state: &mut Self,
        _client: &Client,
        resource: &KoraHaloHeaderManagerV1,
        request: manager::Request,
        _data: &(),
        _handle: &DisplayHandle,
        _data_init: &mut DataInit<'_, Self>,
    ) {
        match request {
            manager::Request::Destroy => {}
            manager::Request::SetMode { surface, mode } => {
                if get_role(&surface) != Some(XDG_TOPLEVEL_ROLE) {
                    resource.post_error(manager::Error::InvalidSurface, "expected an xdg_toplevel");
                    return;
                }
                let overlay = match mode {
                    WEnum::Value(manager::Mode::Reserved) => false,
                    WEnum::Value(manager::Mode::Overlay) => true,
                    _ => {
                        resource.post_error(manager::Error::InvalidMode, "unknown Halo mode");
                        return;
                    }
                };
                with_states(&surface, |states| {
                    let configured = states
                        .data_map
                        .get::<Mutex<XdgToplevelSurfaceRoleAttributes>>()
                        .is_none_or(|attrs| attrs.lock().unwrap().initial_configure_sent);
                    if configured {
                        resource.post_error(
                            manager::Error::AlreadyConfigured,
                            "set Halo mode before the initial wl_surface.commit",
                        );
                        return;
                    }
                    *states
                        .data_map
                        .get_or_insert_threadsafe(HaloHeaderMode::default)
                        .0
                        .lock()
                        .unwrap() = overlay;
                });
            }
        }
    }
}
