use super::CLIENT_MAX_BUFFER_SIZE;
use crate::client_socket::ClientSocket;
use smithay::reexports::wayland_server::{
    Client, DataInit, Dispatch, Display, DisplayHandle,
    backend::{ClientData, ClientId, DisconnectReason},
    protocol::wl_pointer::{self, WlPointer},
};
use std::{
    os::{fd::AsRawFd, unix::net::UnixStream},
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

/// What the client's socket said when the server dropped it.
#[derive(Default)]
struct Watched {
    socket: ClientSocket,
    unread: Mutex<Option<Option<usize>>>,
}

impl ClientData for Watched {
    fn disconnected(&self, _: ClientId, _: DisconnectReason) {
        *self.unread.lock().unwrap() = Some(self.socket.close());
    }
}

fn watched_client(display: &Display<Model>) -> (UnixStream, Arc<Watched>, WlPointer) {
    let mut dh = display.handle();
    let (server, client_end) = UnixStream::pair().unwrap();
    let watched = Arc::new(Watched {
        socket: ClientSocket::new(&server),
        ..Default::default()
    });
    let client = dh.insert_client(server, watched.clone()).unwrap();
    let pointer = client
        .create_resource::<WlPointer, _, Model>(&dh, 9, ())
        .unwrap();
    (client_end, watched, pointer)
}

fn hung_up(stream: &UnixStream) -> bool {
    let mut poll = libc::pollfd {
        fd: stream.as_raw_fd(),
        events: 0,
        revents: 0,
    };
    // Another test starting a process holds a copy of every fd until it execs, so the
    // hang-up can come a moment later. SAFETY: one valid pollfd.
    unsafe { libc::poll(&raw mut poll, 1, 5000) };
    poll.revents & libc::POLLHUP != 0
}

// The library reports both as `ConnectionClosed`; only the socket tells a
// stalled client from one that quit.
#[test]
fn a_client_dropped_for_not_reading_is_told_from_one_that_hung_up() {
    let mut display = Display::<Model>::new().unwrap();
    let (stalled_end, stalled, pointer) = watched_client(&display);
    for frame in 0..60 * 120 {
        pointer.motion(frame, 1.0, 1.0);
        pointer.frame();
        let _ = display.flush_clients();
        if stalled.unread.lock().unwrap().is_some() {
            break;
        }
    }
    let unread = stalled.unread.lock().unwrap().expect("dropped");
    assert!(unread.is_some_and(|bytes| bytes > 0), "{unread:?}");

    let (quitting_end, quitting, _pointer) = watched_client(&display);
    drop(quitting_end);
    display.dispatch_clients(&mut Model).unwrap();
    assert_eq!(*quitting.unread.lock().unwrap(), Some(None));

    // The duplicate must not keep the stalled client connected once the server lets go.
    assert!(hung_up(&stalled_end));
}
