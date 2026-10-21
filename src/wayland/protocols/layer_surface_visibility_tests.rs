use super::*;
use smithay::reexports::wayland_server::{
    Display,
    backend::{ClientData, ClientId, DisconnectReason},
};
use std::{io::Write, os::unix::net::UnixStream, sync::Arc};

struct Peer;

impl ClientData for Peer {
    fn initialized(&self, _: ClientId) {}
    fn disconnected(&self, _: ClientId, _: DisconnectReason) {}
}

#[derive(Debug, PartialEq)]
enum Call {
    Hidden(bool),
    Released,
}

struct Model {
    state: LayerSurfaceVisibilityState,
    calls: Vec<Call>,
}

impl LayerSurfaceVisibilityHandler for Model {
    fn layer_surface_visibility_state(&self) -> &LayerSurfaceVisibilityState {
        &self.state
    }
    fn set_surface_hidden(&mut self, _: ObjectId, hidden: bool) {
        self.calls.push(Call::Hidden(hidden));
    }
    fn release_surface(&mut self, _: ObjectId) {
        self.calls.push(Call::Released);
    }
    fn set_surface_hidden_immediately(&mut self, _: ObjectId) {}
    fn set_surface_transition(&mut self, _: ObjectId, _: LayerTransition) {}
    fn is_surface_hidden(&self, _: &ObjectId) -> bool {
        false
    }
}

crate::delegate_layer_surface_visibility!(Model);

impl Dispatch<WlSurface, ()> for Model {
    fn request(
        _: &mut Self,
        _: &Client,
        _: &WlSurface,
        _: <WlSurface as Resource>::Request,
        _: &(),
        _: &DisplayHandle,
        _: &mut DataInit<'_, Self>,
    ) {
    }
}

/// Send `opcode` with no arguments to `controller`, as its client would.
fn request(
    display: &mut Display<Model>,
    model: &mut Model,
    socket: &mut UnixStream,
    controller: &zcosmic_layer_surface_visibility_v1::ZcosmicLayerSurfaceVisibilityV1,
    opcode: u32,
) {
    let mut message = controller.id().protocol_id().to_ne_bytes().to_vec();
    message.extend_from_slice(&((8 << 16) | opcode).to_ne_bytes());
    socket.write_all(&message).unwrap();
    display.dispatch_clients(model).unwrap();
}

// A client hides a surface, then drops its controller just before destroying the
// surface. Showing it must not take keyboard focus from the surface that has it.
#[test]
fn dropping_a_controller_shows_its_surface_without_asking_for_focus() {
    const DESTROY: u32 = 0;
    const SET_HIDDEN: u32 = 1;
    const SET_VISIBLE: u32 = 2;

    let mut display = Display::<Model>::new().unwrap();
    let mut dh = display.handle();
    let mut model = Model {
        state: LayerSurfaceVisibilityState::new::<Model>(&dh),
        calls: Vec::new(),
    };
    let (server, mut socket) = UnixStream::pair().unwrap();
    let client = dh.insert_client(server, Arc::new(Peer)).unwrap();
    let surface = client
        .create_resource::<WlSurface, _, Model>(&dh, 6, ())
        .unwrap();
    let controller = client
        .create_resource::<zcosmic_layer_surface_visibility_v1::ZcosmicLayerSurfaceVisibilityV1, _, Model>(
            &dh,
            5,
            LayerSurfaceVisibilityControllerData {
                surface: surface.downgrade(),
                hidden: Mutex::new(false),
            },
        )
        .unwrap();

    request(
        &mut display,
        &mut model,
        &mut socket,
        &controller,
        SET_HIDDEN,
    );
    request(
        &mut display,
        &mut model,
        &mut socket,
        &controller,
        SET_VISIBLE,
    );
    assert_eq!(model.calls, [Call::Hidden(true), Call::Hidden(false)]);

    model.calls.clear();
    request(
        &mut display,
        &mut model,
        &mut socket,
        &controller,
        SET_HIDDEN,
    );
    request(&mut display, &mut model, &mut socket, &controller, DESTROY);
    assert_eq!(model.calls, [Call::Hidden(true), Call::Released]);
}
