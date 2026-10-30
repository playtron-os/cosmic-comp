use super::CLIENT_MAX_BUFFER_SIZE;
use smithay::reexports::wayland_server::{
    Client, DataInit, Dispatch, Display, DisplayHandle,
    backend::{ClientData, ClientId, DisconnectReason},
    protocol::wl_pointer::{self, WlPointer},
};
use std::{
    os::unix::net::UnixStream,
    sync::{Arc, Mutex},
};

struct Model;

impl Dispatch<WlPointer, ()> for Model {
    fn request(
        _: &mut Self,
        _: &Client,
        _: &WlPointer,
        _: wl_pointer::Request,
        _: &(),
        _: &DisplayHandle,
        _: &mut DataInit<'_, Self>,
    ) {
    }
}

#[derive(Default)]
struct Peer(Mutex<Option<DisconnectReason>>);

impl ClientData for Peer {
    fn disconnected(&self, _: ClientId, reason: DisconnectReason) {
        *self.0.lock().unwrap() = Some(reason);
    }
}

/// A minute of 120 Hz pointer motion to a client that never reads, flushed once
/// per frame like the event loop does. Returns whether the client was dropped.
fn dropped_after_stalled_minute(max_buffer_size: Option<usize>) -> bool {
    let mut display = Display::<Model>::new().unwrap();
    let mut dh = display.handle();
    if let Some(size) = max_buffer_size {
        dh.set_default_max_buffer_size(size);
    }
    let (server, _unread) = UnixStream::pair().unwrap();
    let peer = Arc::new(Peer::default());
    let client = dh.insert_client(server, peer.clone()).unwrap();
    let pointer = client
        .create_resource::<WlPointer, _, Model>(&dh, 9, ())
        .unwrap();
    for frame in 0..60 * 120 {
        pointer.motion(frame, 1.0, 1.0);
        pointer.frame();
        let _ = display.flush_clients();
    }
    peer.0.lock().unwrap().is_some()
}

#[test]
fn stalled_client_outlasts_a_minute_of_pointer_motion() {
    assert!(!dropped_after_stalled_minute(Some(CLIENT_MAX_BUFFER_SIZE)));
}

// Guards the test above: with the library default the same stall is fatal.
#[test]
fn default_buffer_drops_a_stalled_client() {
    assert!(dropped_after_stalled_minute(None));
}
