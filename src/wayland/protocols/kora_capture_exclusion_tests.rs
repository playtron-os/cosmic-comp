use super::*;
use crate::backend::render::capture_exclusion::{CapturePass, leaves_out, take_drew_excluded};
use smithay::{
    reexports::wayland_server::{
        Display,
        backend::{ClientData, DisconnectReason},
    },
    wayland::compositor::{CompositorClientState, CompositorHandler, CompositorState},
};
use std::{
    collections::HashMap,
    io::{ErrorKind, Read, Write},
    os::unix::net::UnixStream,
    sync::Arc,
    time::Duration,
};

#[derive(Default)]
struct Peer(CompositorClientState);

impl ClientData for Peer {
    fn initialized(&self, _: ClientId) {}
    fn disconnected(&self, _: ClientId, _: DisconnectReason) {}
}

struct Model {
    compositor: CompositorState,
    _exclusion: CaptureExclusionState,
}

impl CompositorHandler for Model {
    fn compositor_state(&mut self) -> &mut CompositorState {
        &mut self.compositor
    }
    fn client_compositor_state<'a>(&self, client: &'a Client) -> &'a CompositorClientState {
        &client.get_data::<Peer>().unwrap().0
    }
    fn commit(&mut self, _: &WlSurface) {}
}

smithay::delegate_dispatch2!(Model);
delegate_kora_capture_exclusion!(Model);

const DISPLAY: u32 = 1;
const REGISTRY: u32 = 2;
const COMPOSITOR: u32 = 3;
const MANAGER: u32 = 4;
const SURFACE: u32 = 5;
const EXCLUSION: u32 = 6;

enum Arg<'a> {
    Uint(u32),
    Str(&'a str),
}

/// A client speaking the wire protocol over a private socket pair.
struct World {
    display: Display<Model>,
    model: Model,
    client: Client,
    socket: UnixStream,
}

impl World {
    fn new() -> Self {
        let display = Display::<Model>::new().unwrap();
        let mut dh = display.handle();
        let model = Model {
            compositor: CompositorState::new::<Model>(&dh),
            _exclusion: CaptureExclusionState::new::<Model>(&dh),
        };
        let (server, socket) = UnixStream::pair().unwrap();
        socket
            .set_read_timeout(Some(Duration::from_millis(100)))
            .unwrap();
        let client = dh.insert_client(server, Arc::new(Peer::default())).unwrap();
        let mut world = World {
            display,
            model,
            client,
            socket,
        };
        world.send(DISPLAY, 1, &[Arg::Uint(REGISTRY)]);
        world.dispatch();
        let globals: HashMap<String, u32> = world
            .events()
            .into_iter()
            .filter(|(object, opcode, _)| (*object, *opcode) == (REGISTRY, 0))
            .map(|(_, _, body)| {
                let len = u32::from_ne_bytes(body[4..8].try_into().unwrap()) as usize;
                let interface = String::from_utf8(body[8..8 + len - 1].to_vec()).unwrap();
                (interface, u32::from_ne_bytes(body[..4].try_into().unwrap()))
            })
            .collect();
        for (interface, id) in [
            ("wl_compositor", COMPOSITOR),
            ("kora_capture_exclusion_manager_v1", MANAGER),
        ] {
            let name = globals[interface];
            world.send(
                REGISTRY,
                0,
                &[
                    Arg::Uint(name),
                    Arg::Str(interface),
                    Arg::Uint(1),
                    Arg::Uint(id),
                ],
            );
        }
        world.send(COMPOSITOR, 0, &[Arg::Uint(SURFACE)]);
        world
    }

    fn send(&mut self, object: u32, opcode: u16, args: &[Arg]) {
        let mut body = Vec::new();
        for arg in args {
            match arg {
                Arg::Uint(value) => body.extend(value.to_ne_bytes()),
                Arg::Str(text) => {
                    body.extend((text.len() as u32 + 1).to_ne_bytes());
                    body.extend(text.as_bytes());
                    body.push(0);
                    body.resize(body.len().next_multiple_of(4), 0);
                }
            }
        }
        let header = ((8 + body.len() as u32) << 16) | u32::from(opcode);
        let mut message = object.to_ne_bytes().to_vec();
        message.extend(header.to_ne_bytes());
        message.extend(body);
        self.socket.write_all(&message).unwrap();
    }

    fn dispatch(&mut self) {
        self.display.dispatch_clients(&mut self.model).unwrap();
        self.display.flush_clients().unwrap();
    }

    /// Every event waiting on the socket, as (object, opcode, body).
    fn events(&mut self) -> Vec<(u32, u16, Vec<u8>)> {
        let mut bytes = Vec::new();
        let mut chunk = [0; 4096];
        loop {
            match self.socket.read(&mut chunk) {
                Ok(0) => break,
                Ok(n) => bytes.extend(&chunk[..n]),
                Err(err) if matches!(err.kind(), ErrorKind::WouldBlock | ErrorKind::TimedOut) => {
                    break;
                }
                Err(err) => panic!("{err}"),
            }
        }
        let mut events = Vec::new();
        let mut rest = bytes.as_slice();
        while rest.len() >= 8 {
            let object = u32::from_ne_bytes(rest[..4].try_into().unwrap());
            let word = u32::from_ne_bytes(rest[4..8].try_into().unwrap());
            let size = (word >> 16) as usize;
            events.push((object, (word & 0xffff) as u16, rest[8..size].to_vec()));
            rest = &rest[size..];
        }
        events
    }

    fn surface(&self) -> WlSurface {
        self.client
            .object_from_protocol_id(&self.display.handle(), SURFACE)
            .unwrap()
    }

    fn excluded(&mut self) -> bool {
        self.dispatch();
        with_states(&self.surface(), is_excluded)
    }

    fn set_excluded(&mut self, excluded: bool) {
        self.send(EXCLUSION, 1, &[Arg::Uint(u32::from(excluded))]);
    }

    fn commit(&mut self) {
        self.send(SURFACE, 6, &[]);
    }
}

#[test]
fn the_flag_applies_on_commit_and_destroying_the_object_puts_the_surface_back() {
    let mut world = World::new();
    world.commit();
    assert!(!world.excluded(), "a surface starts in captures");
    world.send(MANAGER, 1, &[Arg::Uint(EXCLUSION), Arg::Uint(SURFACE)]);
    world.set_excluded(true);
    assert!(!world.excluded(), "double-buffered until the commit");
    world.commit();
    assert!(world.excluded());
    world.set_excluded(false);
    world.commit();
    assert!(!world.excluded());
    world.set_excluded(true);
    world.commit();
    world.send(EXCLUSION, 0, &[]);
    assert!(world.excluded(), "destroying waits for the commit too");
    world.commit();
    assert!(!world.excluded());
}

#[test]
fn a_surface_has_one_exclusion_object_at_a_time() {
    let mut world = World::new();
    world.send(MANAGER, 1, &[Arg::Uint(EXCLUSION), Arg::Uint(SURFACE)]);
    world.send(EXCLUSION, 0, &[]);
    // The first is gone, so a second is allowed.
    world.send(MANAGER, 1, &[Arg::Uint(EXCLUSION + 1), Arg::Uint(SURFACE)]);
    world.dispatch();
    assert!(
        world
            .events()
            .iter()
            .all(|(object, opcode, _)| (*object, *opcode) != (DISPLAY, 0)),
        "no protocol error"
    );
    world.send(MANAGER, 1, &[Arg::Uint(EXCLUSION + 2), Arg::Uint(SURFACE)]);
    world.dispatch();
    let error = world
        .events()
        .into_iter()
        .find(|(object, opcode, _)| (*object, *opcode) == (DISPLAY, 0))
        .expect("a protocol error");
    assert_eq!(
        u32::from_ne_bytes(error.2[..4].try_into().unwrap()),
        MANAGER
    );
    assert_eq!(
        u32::from_ne_bytes(error.2[4..8].try_into().unwrap()),
        kora_capture_exclusion_manager_v1::Error::AlreadyConstructed as u32
    );
}

#[test]
fn only_a_capture_pass_leaves_an_excluded_surface_out_and_the_screen_notes_it() {
    let mut world = World::new();
    world.commit();
    world.dispatch();
    let surface = world.surface();
    take_drew_excluded();
    assert!(!with_states(&surface, leaves_out));
    assert!(
        !take_drew_excluded(),
        "an unmarked surface needs no capture of its own"
    );

    world.send(MANAGER, 1, &[Arg::Uint(EXCLUSION), Arg::Uint(SURFACE)]);
    world.set_excluded(true);
    world.commit();
    world.dispatch();
    assert!(!with_states(&surface, leaves_out), "the screen keeps it");
    assert!(take_drew_excluded());
    assert!(!take_drew_excluded(), "taking the note clears it");
    {
        let _capture = CapturePass::begin();
        assert!(with_states(&surface, leaves_out));
        {
            let _nested = CapturePass::begin();
        }
        assert!(
            with_states(&surface, leaves_out),
            "a nested pass ends inside the outer one"
        );
    }
    assert!(!take_drew_excluded(), "a capture is no screen draw");
    assert!(!with_states(&surface, leaves_out));
}
