use super::{bottom_pin_deficit, layer_bottom_pin};
use smithay::{
    desktop::{LayerSurface, layer_map_for_output},
    output::{Mode, Output, PhysicalProperties, Subpixel},
    reexports::{
        wayland_protocols_wlr::layer_shell::v1::server::zwlr_layer_shell_v1::ZwlrLayerShellV1,
        wayland_server::{
            Client, Display, Resource,
            backend::ClientData,
            protocol::{wl_compositor::WlCompositor, wl_output::WlOutput, wl_surface::WlSurface},
        },
    },
    wayland::{
        GlobalData,
        compositor::{CompositorClientState, CompositorHandler, CompositorState},
        shell::wlr_layer::{
            Anchor, Layer, LayerSurface as WlrLayerSurface, WlrLayerShellHandler,
            WlrLayerShellState,
        },
    },
};
use std::{io::Write, os::unix::net::UnixStream, sync::Arc};

struct Model {
    compositor: CompositorState,
    layer_shell: WlrLayerShellState,
    layers: Vec<WlrLayerSurface>,
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

impl WlrLayerShellHandler for Model {
    fn shell_state(&mut self) -> &mut WlrLayerShellState {
        &mut self.layer_shell
    }
    fn new_layer_surface(
        &mut self,
        surface: WlrLayerSurface,
        _: Option<WlOutput>,
        _: Layer,
        _: String,
    ) {
        self.layers.push(surface);
    }
}

smithay::delegate_dispatch2!(Model);

#[derive(Default)]
struct Peer(CompositorClientState);
impl ClientData for Peer {}

const SURFACE: u32 = 2;
const LAYER: u32 = 3;

/// A client that commits a bottom-anchored layer surface asking for `height`,
/// speaking the wire format to objects the test makes for it.
fn commit_popover(height: u32) -> (Display<Model>, Model, UnixStream, LayerSurface) {
    let mut display = Display::<Model>::new().unwrap();
    let mut dh = display.handle();
    let mut model = Model {
        compositor: CompositorState::new::<Model>(&dh),
        layer_shell: WlrLayerShellState::new::<Model>(&dh),
        layers: Vec::new(),
    };
    let (server, mut socket) = UnixStream::pair().unwrap();
    let client = dh.insert_client(server, Arc::new(Peer::default())).unwrap();
    let compositor = client
        .create_resource::<WlCompositor, _, Model>(&dh, 6, GlobalData)
        .unwrap()
        .id()
        .protocol_id();
    let shell = client
        .create_resource::<ZwlrLayerShellV1, _, Model>(&dh, 4, GlobalData)
        .unwrap()
        .id()
        .protocol_id();
    let namespace = u32::from_ne_bytes(*b"pop\0");
    for (object, opcode, args) in [
        (compositor, 0, vec![SURFACE]),
        // get_layer_surface(id, surface, no output, top, "pop")
        (shell, 0, vec![LAYER, SURFACE, 0, 2, 4, namespace]),
        (LAYER, 0, vec![400, height]),
        (LAYER, 1, vec![Anchor::BOTTOM.bits()]),
        (SURFACE, 6, vec![]),
    ] {
        let mut message: Vec<u8> = object.to_ne_bytes().to_vec();
        message.extend((((8 + 4 * args.len() as u32) << 16) | opcode).to_ne_bytes());
        message.extend(args.iter().flat_map(|word| word.to_ne_bytes()));
        socket.write_all(&message).unwrap();
        display.dispatch_clients(&mut model).unwrap();
    }
    let layer = LayerSurface::new(model.layers.pop().unwrap(), "pop".into());
    (display, model, socket, layer)
}

fn screen(height: i32) -> Output {
    let output = Output::new(
        "test".into(),
        PhysicalProperties {
            size: (300, 200).into(),
            subpixel: Subpixel::Unknown,
            make: "test".into(),
            model: "test".into(),
            serial_number: String::new(),
        },
    );
    output.change_current_state(
        Some(Mode {
            size: (1280, height).into(),
            refresh: 60_000,
        }),
        None,
        None,
        Some((0, 0).into()),
    );
    output
}

/// A 900px request on a 720px screen is arranged and drawn at 720; pinning by the
/// 180px it never got drew it below where clicks land.
#[test]
fn a_clamped_popover_is_drawn_where_it_takes_input() {
    let (_display, _model, _socket, layer) = commit_popover(900);
    layer_map_for_output(&screen(720))
        .map_layer(&layer)
        .unwrap();
    let arranged = layer
        .layer_surface()
        .with_pending_state(|state| state.size.map(|size| size.h));
    assert_eq!(arranged, Some(720));

    assert_eq!(layer_bottom_pin(&layer, 720), 0);
}

/// While the buffer catches up to a new size, its bottom edge stays put.
#[test]
fn a_resizing_popover_keeps_its_bottom_edge() {
    let (_display, _model, _socket, layer) = commit_popover(500);
    layer_map_for_output(&screen(720))
        .map_layer(&layer)
        .unwrap();

    assert_eq!(layer_bottom_pin(&layer, 400), 100);
    assert_eq!(layer_bottom_pin(&layer, 600), -100);
}

#[test]
fn only_bottom_anchored_surfaces_are_pinned() {
    assert_eq!(bottom_pin_deficit(Anchor::TOP, 500, 400), 0);
    assert_eq!(
        bottom_pin_deficit(Anchor::TOP | Anchor::BOTTOM, 500, 400),
        0
    );
    assert_eq!(bottom_pin_deficit(Anchor::BOTTOM, 0, 400), 0);
}
