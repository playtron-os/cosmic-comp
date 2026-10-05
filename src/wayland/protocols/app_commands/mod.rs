// SPDX-License-Identifier: GPL-3.0-only

//! `kora_app_commands_v1`: a window publishes its commands and recent items,
//! and the Halo invokes them in that window.

pub mod catalog;

use std::sync::{
    Mutex,
    atomic::{AtomicBool, Ordering},
};

use catalog::{Catalog, Command, Invalid, Recent, Staged};
use cosmic_protocols::kora_app_commands::v1::server::{
    kora_app_commands_handle_v1::{self as handle, KoraAppCommandsHandleV1},
    kora_app_commands_v1::{self as manager, KoraAppCommandsV1},
};
use smithay::{
    reexports::wayland_server::{
        Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource, WEnum, Weak,
        backend::GlobalId, protocol::wl_surface::WlSurface,
    },
    utils::Serial,
    wayland::compositor::with_states,
};

use crate::state::State;

#[derive(Debug)]
pub struct AppCommandsState {
    _global: GlobalId,
}

impl AppCommandsState {
    pub fn new(dh: &DisplayHandle) -> Self {
        Self {
            _global: dh.create_global::<State, KoraAppCommandsV1, _>(1, ()),
        }
    }
}

/// The registration a toplevel's surface holds, if any.
#[derive(Default)]
struct Slot(Mutex<Option<KoraAppCommandsHandleV1>>);

#[derive(Debug)]
pub struct HandleData {
    surface: Weak<WlSurface>,
    staged: Mutex<Staged>,
    closed: AtomicBool,
}

fn registration(surface: &WlSurface) -> Option<KoraAppCommandsHandleV1> {
    with_states(surface, |states| {
        states
            .data_map
            .get::<Slot>()
            .and_then(|slot| slot.0.lock().unwrap().clone())
    })
    .filter(|handle| handle.is_alive())
}

/// The catalog `surface` last committed, if its client registered one.
pub fn committed(surface: &WlSurface) -> Option<Catalog> {
    let handle = registration(surface)?;
    let data = handle.data::<HandleData>()?;
    if data.closed.load(Ordering::SeqCst) {
        return None;
    }
    data.staged.lock().unwrap().committed.clone()
}

/// What the shell selected, as the window will receive it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Selection {
    Command(String),
    Recent(String),
}

/// Deliver `selection`, picked from a menu or palette built from catalog
/// `generation`. A catalog that changed since, or no longer offers it, gets nothing.
pub fn invoke(surface: &WlSurface, selection: &Selection, generation: u32, serial: Serial) -> bool {
    let Some(handle) = registration(surface) else {
        return false;
    };
    let Some(data) = handle.data::<HandleData>() else {
        return false;
    };
    let (id, recent) = match selection {
        Selection::Command(id) => (id, false),
        Selection::Recent(id) => (id, true),
    };
    if data.closed.load(Ordering::SeqCst)
        || !data
            .staged
            .lock()
            .unwrap()
            .can_invoke(generation, id, recent)
    {
        return false;
    }
    if recent {
        handle.recent_selected(id.clone(), generation, serial.into());
    } else {
        handle.invoke(id.clone(), generation, serial.into());
    }
    true
}

/// The toplevel is gone: the registration goes inert and says so.
pub fn toplevel_destroyed(surface: &WlSurface) {
    let handle = with_states(surface, |states| {
        states
            .data_map
            .get::<Slot>()
            .and_then(|slot| slot.0.lock().unwrap().take())
    });
    if let Some(handle) = handle.filter(|handle| handle.is_alive()) {
        close(&handle);
    }
}

fn close(handle: &KoraAppCommandsHandleV1) {
    if let Some(data) = handle.data::<HandleData>()
        && !data.closed.swap(true, Ordering::SeqCst)
    {
        data.staged.lock().unwrap().committed = None;
        handle.closed();
    }
}

impl GlobalDispatch<KoraAppCommandsV1, ()> for State {
    fn bind(
        _state: &mut Self,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<KoraAppCommandsV1>,
        _data: &(),
        data_init: &mut DataInit<'_, Self>,
    ) {
        data_init.init(resource, ());
    }
}

impl Dispatch<KoraAppCommandsV1, ()> for State {
    fn request(
        state: &mut Self,
        _client: &Client,
        resource: &KoraAppCommandsV1,
        request: manager::Request,
        _data: &(),
        _handle: &DisplayHandle,
        data_init: &mut DataInit<'_, Self>,
    ) {
        match request {
            manager::Request::Destroy => {}
            manager::Request::GetCommands { id, toplevel } => {
                let surface = state
                    .common
                    .xdg_shell_state
                    .get_toplevel(&toplevel)
                    .map(|toplevel| toplevel.wl_surface().clone())
                    .filter(|surface| surface.id().same_client_as(&resource.id()));
                let Some(surface) = surface else {
                    resource.post_error(
                        manager::Error::InvalidToplevel,
                        "the toplevel is not this client's",
                    );
                    return;
                };
                if registration(&surface).is_some_and(|handle| {
                    handle
                        .data::<HandleData>()
                        .is_some_and(|data| !data.closed.load(Ordering::SeqCst))
                }) {
                    resource.post_error(
                        manager::Error::AlreadyRegistered,
                        "this toplevel already publishes its commands",
                    );
                    return;
                }
                let handle = data_init.init(
                    id,
                    HandleData {
                        surface: surface.downgrade(),
                        staged: Mutex::new(Staged::default()),
                        closed: AtomicBool::new(false),
                    },
                );
                with_states(&surface, |states| {
                    *states
                        .data_map
                        .get_or_insert_threadsafe(Slot::default)
                        .0
                        .lock()
                        .unwrap() = Some(handle);
                });
            }
            _ => {}
        }
    }
}

fn post(resource: &KoraAppCommandsHandleV1, invalid: Invalid) {
    let (code, message) = match invalid {
        Invalid::Value => (
            handle::Error::InvalidValue,
            "a value breaks the catalog contract",
        ),
        Invalid::Reserved => (handle::Error::ReservedId, "the id belongs to the shell"),
        Invalid::Unknown => (handle::Error::UnknownId, "no command has this id"),
        Invalid::TooMany => (handle::Error::TooManyCommands, "more than 256 commands"),
    };
    resource.post_error(code, message);
}

impl Dispatch<KoraAppCommandsHandleV1, HandleData> for State {
    fn request(
        state: &mut Self,
        _client: &Client,
        resource: &KoraAppCommandsHandleV1,
        request: handle::Request,
        data: &HandleData,
        _handle: &DisplayHandle,
        _data_init: &mut DataInit<'_, Self>,
    ) {
        if let handle::Request::Destroy = request {
            data.closed.store(true, Ordering::SeqCst);
            if let Ok(surface) = data.surface.upgrade() {
                with_states(&surface, |states| {
                    if let Some(slot) = states.data_map.get::<Slot>() {
                        let mut slot = slot.0.lock().unwrap();
                        if slot.as_ref() == Some(resource) {
                            *slot = None;
                        }
                    }
                });
                state.app_commands_changed(&surface);
            }
            return;
        }
        if data.closed.load(Ordering::SeqCst) {
            return;
        }
        let done = matches!(request, handle::Request::Done { .. });
        let result = {
            let mut staged = data.staged.lock().unwrap();
            match request {
                handle::Request::Destroy => Ok(()),
                handle::Request::Clear => {
                    staged.pending = Catalog::default();
                    Ok(())
                }
                handle::Request::Handle { id, enabled } => staged.pending.handle(id, enabled),
                handle::Request::AddCommand {
                    id,
                    name,
                    keys,
                    section,
                    icon,
                    flags,
                } => match flags {
                    WEnum::Value(flags) => staged.pending.add(Command {
                        id,
                        name,
                        keys,
                        section,
                        icon,
                        flags: flags.bits(),
                        enabled: true,
                        active: false,
                    }),
                    WEnum::Unknown(_) => Err(Invalid::Value),
                },
                handle::Request::SetState { id, active } => staged.pending.set_state(&id, active),
                handle::Request::SetEnabled { id, enabled } => {
                    staged.pending.set_enabled(&id, enabled)
                }
                handle::Request::AddRecent {
                    id,
                    label,
                    sublabel,
                    timestamp_hi,
                    timestamp_lo,
                } => staged.pending.add_recent(Recent {
                    id,
                    label,
                    sublabel,
                    timestamp: (u64::from(timestamp_hi) << 32) | u64::from(timestamp_lo),
                }),
                handle::Request::Done { generation } => staged.commit(generation),
                handle::Request::RequestPalette { seat, serial } => {
                    drop(staged);
                    if let Ok(surface) = data.surface.upgrade() {
                        state.app_commands_palette(&surface, &seat, Serial::from(serial));
                    }
                    return;
                }
                _ => Ok(()),
            }
        };
        match result {
            Err(invalid) => post(resource, invalid),
            Ok(()) if done => {
                if let Ok(surface) = data.surface.upgrade() {
                    state.app_commands_changed(&surface);
                }
            }
            Ok(()) => {}
        }
    }

    fn destroyed(
        _state: &mut Self,
        _client: smithay::reexports::wayland_server::backend::ClientId,
        _resource: &KoraAppCommandsHandleV1,
        data: &HandleData,
    ) {
        data.closed.store(true, Ordering::SeqCst);
    }
}
