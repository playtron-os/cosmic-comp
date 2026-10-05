use super::*;
use smithay::reexports::wayland_server::{
    Display,
    backend::{ClientData, DisconnectReason, ObjectId},
};
use std::{collections::HashMap, io::Read, os::unix::net::UnixStream, sync::Arc, time::Duration};

#[derive(Debug)]
struct Peer(Option<String>);

impl ClientData for Peer {
    fn initialized(&self, _: ClientId) {}
    fn disconnected(&self, _: ClientId, _: DisconnectReason) {}
}

struct Model {
    identity: IdentityState,
    owned: HashMap<ObjectId, ToplevelIdentity>,
    foreign: HashMap<ObjectId, ToplevelIdentity>,
}

impl IdentityHandler for Model {
    fn identity_state(&mut self) -> &mut IdentityState {
        &mut self.identity
    }
    fn client_workspace(&self, client: &Client) -> Option<String> {
        client.get_data::<Peer>().unwrap().0.clone()
    }
    fn owned_identity(&self, target: &XdgToplevel) -> Option<ToplevelIdentity> {
        self.owned.get(&target.id()).cloned()
    }
    fn foreign_identity(&self, target: &ExtForeignToplevelHandleV1) -> Option<ToplevelIdentity> {
        self.foreign.get(&target.id()).cloned()
    }
}

delegate_kora_toplevel_identity!(Model);

impl Dispatch<XdgToplevel, ()> for Model {
    fn request(
        _: &mut Self,
        _: &Client,
        _: &XdgToplevel,
        _: <XdgToplevel as Resource>::Request,
        _: &(),
        _: &DisplayHandle,
        _: &mut DataInit<'_, Self>,
    ) {
    }
}

impl Dispatch<ExtForeignToplevelHandleV1, ()> for Model {
    fn request(
        _: &mut Self,
        _: &Client,
        _: &ExtForeignToplevelHandleV1,
        _: <ExtForeignToplevelHandleV1 as Resource>::Request,
        _: &(),
        _: &DisplayHandle,
        _: &mut DataInit<'_, Self>,
    ) {
    }
}

struct World {
    display: Display<Model>,
    model: Model,
}

impl World {
    fn new() -> Self {
        let display = Display::new().unwrap();
        let model = Model {
            identity: IdentityState::new::<Model>(&display.handle()),
            owned: HashMap::new(),
            foreign: HashMap::new(),
        };
        Self { display, model }
    }

    fn client(&mut self, workspace: Option<&str>) -> (Client, UnixStream) {
        let (server, socket) = UnixStream::pair().unwrap();
        socket
            .set_read_timeout(Some(Duration::from_millis(100)))
            .unwrap();
        let client = self
            .display
            .handle()
            .insert_client(server, Arc::new(Peer(workspace.map(str::to_owned))))
            .unwrap();
        (client, socket)
    }

    fn owned(&self, client: &Client) -> XdgToplevel {
        client
            .create_resource::<XdgToplevel, _, Model>(&self.display.handle(), 1, ())
            .unwrap()
    }

    fn foreign(&self, client: &Client) -> ExtForeignToplevelHandleV1 {
        client
            .create_resource::<ExtForeignToplevelHandleV1, _, Model>(&self.display.handle(), 1, ())
            .unwrap()
    }

    fn subscribe(&mut self, client: &Client, target: Target) -> KoraToplevelIdentityHandleV1 {
        let handle = client
            .create_resource::<KoraToplevelIdentityHandleV1, _, Model>(
                &self.display.handle(),
                1,
                (),
            )
            .unwrap();
        self.model.identity.subscriptions.push(Subscription {
            handle: handle.clone(),
            target,
            sent: None,
        });
        self.refresh();
        handle
    }

    fn refresh(&mut self) {
        IdentityState::refresh(&mut self.model);
        self.display.flush_clients().unwrap();
    }
}

fn identity(identifier: &str, workspace: &str) -> ToplevelIdentity {
    ToplevelIdentity {
        identifier: identifier.into(),
        workspace: workspace.into(),
    }
}

// Read the events actually serialized onto this fixture's private Wayland socket.
fn event(socket: &mut UnixStream, handle: &KoraToplevelIdentityHandleV1) -> (u16, Option<String>) {
    let mut header = [0; 8];
    socket.read_exact(&mut header).unwrap();
    assert_eq!(
        u32::from_ne_bytes(header[..4].try_into().unwrap()),
        handle.id().protocol_id()
    );
    let word = u32::from_ne_bytes(header[4..].try_into().unwrap());
    let opcode = (word & 0xffff) as u16;
    let mut body = vec![0; (word >> 16) as usize - 8];
    socket.read_exact(&mut body).unwrap();
    let text = if opcode < 2 {
        let size = u32::from_ne_bytes(body[..4].try_into().unwrap()) as usize;
        Some(String::from_utf8(body[4..4 + size - 1].to_vec()).unwrap())
    } else {
        assert!(body.is_empty());
        None
    };
    (opcode, text)
}

fn pair(
    socket: &mut UnixStream,
    handle: &KoraToplevelIdentityHandleV1,
    expected: &ToplevelIdentity,
) {
    assert_eq!(
        event(socket, handle),
        (0, Some(expected.identifier.clone()))
    );
    assert_eq!(event(socket, handle), (1, Some(expected.workspace.clone())));
    assert_eq!(event(socket, handle), (2, None));
}

#[test]
fn only_the_owner_can_get_an_owned_identity_and_the_pair_is_atomic() {
    let mut world = World::new();
    let (a, mut sa) = world.client(Some("alpha"));
    let (b, mut sb) = world.client(Some("beta"));
    let top = world.owned(&a);
    let expected = identity("mapped-window", "alpha");
    world.model.owned.insert(top.id(), expected.clone());
    let handle = world.subscribe(&a, Target::Owned(top.downgrade()));
    pair(&mut sa, &handle, &expected);
    let stolen = world.subscribe(&b, Target::Owned(top.downgrade()));
    assert_eq!(event(&mut sb, &stolen), (3, None));
}

#[test]
fn an_owned_identity_waits_for_map_and_actual_unmap_requires_a_fresh_pair() {
    let mut world = World::new();
    let (client, mut socket) = world.client(Some("alpha"));
    let top = world.owned(&client);
    let first = world.subscribe(&client, Target::Owned(top.downgrade()));
    let mut byte = [0];
    assert!(socket.read(&mut byte).is_err(), "no premature attribution");
    let before = identity("before-unmap", "alpha");
    world.model.owned.insert(top.id(), before.clone());
    world.refresh();
    pair(&mut socket, &first, &before);
    world.model.owned.remove(&top.id());
    world.refresh();
    assert_eq!(event(&mut socket, &first), (3, None));
    let after = identity("after-remap", "alpha");
    world.model.owned.insert(top.id(), after.clone());
    let second = world.subscribe(&client, Target::Owned(top.downgrade()));
    pair(&mut socket, &second, &after);
}

#[test]
fn foreign_identity_uses_the_owners_workspace_and_rejects_other_realms() {
    for (caller, owner, allowed) in [
        (Some("alpha"), "alpha", true),
        (Some("alpha"), "beta", false),
        (None, "beta", true),
        (Some("alpha"), "", true),
    ] {
        let mut world = World::new();
        let (client, mut socket) = world.client(caller);
        let top = world.foreign(&client);
        let expected = identity("mapped-window", owner);
        world.model.foreign.insert(top.id(), expected.clone());
        let handle = world.subscribe(&client, Target::Foreign(top.downgrade()));
        if allowed {
            pair(&mut socket, &handle, &expected);
        } else {
            assert_eq!(event(&mut socket, &handle), (3, None));
        }
    }
}

#[test]
fn withdrawing_a_foreign_handle_revokes_it_without_reviving_it_when_reshown() {
    let mut world = World::new();
    let (client, mut socket) = world.client(None);
    let old = world.foreign(&client);
    let expected = identity("same-mapped-window", "alpha");
    world.model.foreign.insert(old.id(), expected.clone());
    let first = world.subscribe(&client, Target::Foreign(old.downgrade()));
    pair(&mut socket, &first, &expected);
    world.model.foreign.remove(&old.id());
    world.refresh();
    assert_eq!(event(&mut socket, &first), (3, None));
    let shown = world.foreign(&client);
    world.model.foreign.insert(shown.id(), expected.clone());
    let second = world.subscribe(&client, Target::Foreign(shown.downgrade()));
    pair(&mut socket, &second, &expected);
    let stale = world.subscribe(&client, Target::Foreign(old.downgrade()));
    assert_eq!(event(&mut socket, &stale), (3, None));
}
