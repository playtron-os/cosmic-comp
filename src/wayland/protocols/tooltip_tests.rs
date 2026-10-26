use super::*;
use smithay::reexports::wayland_server::{
    Display, backend::ClientData, protocol::wl_compositor::WlCompositor,
};
use smithay::wayland::compositor::{CompositorClientState, CompositorHandler, CompositorState};
use std::{
    io::Write,
    os::unix::net::UnixStream,
    sync::Arc,
    time::{Duration, Instant},
};

struct Model {
    compositor: CompositorState,
    tooltips: TooltipManagerState,
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

impl TooltipHandler for Model {
    fn tooltip_state(&mut self) -> &mut TooltipManagerState {
        &mut self.tooltips
    }
}

crate::delegate_tooltip!(Model);

#[derive(Default)]
struct Peer(CompositorClientState);
impl ClientData for Peer {}

/// A client with a parent surface (2), a tooltip surface (3) and another surface (4), speaking
/// the wire format to compositor and tooltip-manager objects the test makes for it.
struct Fixture {
    display: Display<Model>,
    model: Model,
    client: Client,
    socket: UnixStream,
    manager: u32,
}

const PARENT: u32 = 2;
const TOOLTIP: u32 = 3;
const OTHER: u32 = 4;
const CONTROLLER: u32 = 5;

impl Fixture {
    fn new() -> Self {
        let display = Display::<Model>::new().unwrap();
        let mut dh = display.handle();
        let model = Model {
            compositor: CompositorState::new::<Model>(&dh),
            tooltips: TooltipManagerState::new::<Model>(&dh),
        };
        let (server, socket) = UnixStream::pair().unwrap();
        let client = dh.insert_client(server, Arc::new(Peer::default())).unwrap();
        let compositor = client
            .create_resource::<WlCompositor, _, Model>(&dh, 6, smithay::wayland::GlobalData)
            .unwrap()
            .id()
            .protocol_id();
        let manager = client
            .create_resource::<zcosmic_tooltip_manager_v1::ZcosmicTooltipManagerV1, _, Model>(
                &dh,
                1,
                (),
            )
            .unwrap()
            .id()
            .protocol_id();
        let mut fixture = Fixture {
            display,
            model,
            client,
            socket,
            manager,
        };
        for surface in [PARENT, TOOLTIP, OTHER] {
            fixture.send(compositor, 0, &[surface]);
        }
        fixture
    }

    fn send(&mut self, object: u32, opcode: u32, args: &[u32]) {
        let mut message = object.to_ne_bytes().to_vec();
        message.extend((((8 + 4 * args.len() as u32) << 16) | opcode).to_ne_bytes());
        message.extend(args.iter().flat_map(|word| word.to_ne_bytes()));
        self.socket.write_all(&message).unwrap();
        self.display.dispatch_clients(&mut self.model).unwrap();
    }

    fn surface(&self, id: u32) -> WlSurface {
        self.client
            .object_from_protocol_id(&self.display.handle(), id)
            .unwrap()
    }

    /// get_tooltip for the tooltip surface over the parent, then set_offset(16, 20).
    fn get_tooltip(&mut self) {
        self.send(self.manager, 1, &[CONTROLLER, TOOLTIP, PARENT]);
        self.send(CONTROLLER, 1, &[16, 20]);
    }

    /// The pointer at (x, y), over `surface` with its origin at (100, 200).
    fn pointer(&mut self, surface: Option<u32>, (x, y): (i32, i32)) {
        let surface = surface.map(|id| self.surface(id));
        self.model.tooltips.update_positions(
            surface.as_ref(),
            Point::from((f64::from(x), f64::from(y))),
            surface.as_ref().map(|_| Point::from((x - 100, y - 200))),
        );
    }

    fn placement(&self) -> Option<TooltipPlacement> {
        get_tooltip_position(&self.surface(TOOLTIP))
    }
}

#[test]
fn a_tooltip_goes_away_while_the_pointer_is_off_its_parent() {
    let mut f = Fixture::new();
    f.pointer(Some(OTHER), (40, 40));
    f.get_tooltip();
    assert!(
        matches!(f.placement(), Some(TooltipPlacement::Hidden)),
        "made with the pointer elsewhere"
    );

    f.pointer(Some(PARENT), (500, 300));
    let at = f.placement().and_then(|p| p.shown(Instant::now()));
    assert_eq!(at.map(|at| at.position), Some(Point::from((516, 320))));
    assert_eq!(
        at.map(|at| at.parent_relative),
        Some(Point::from((416, 120)))
    );

    f.pointer(Some(OTHER), (900, 300));
    assert!(matches!(f.placement(), Some(TooltipPlacement::Hidden)));
    f.pointer(None, (900, 300));
    assert!(matches!(f.placement(), Some(TooltipPlacement::Hidden)));

    f.pointer(Some(PARENT), (510, 310));
    let at = f.placement().and_then(|p| p.shown(Instant::now()));
    assert_eq!(at.map(|at| at.position), Some(Point::from((526, 330))));
}

// The pointer may rest on the parent while its tooltip is made; it is placed on the next motion.
#[test]
fn a_tooltip_made_under_the_pointer_is_not_hidden() {
    let mut f = Fixture::new();
    f.pointer(Some(PARENT), (500, 300));
    f.get_tooltip();
    assert!(f.placement().is_none());
    f.pointer(Some(PARENT), (501, 300));
    assert!(
        f.placement()
            .and_then(|p| p.shown(Instant::now()))
            .is_some()
    );
}

#[test]
fn a_delayed_tooltip_stays_out_of_sight_until_its_delay_ends() {
    let mut f = Fixture::new();
    f.pointer(Some(PARENT), (500, 300));
    f.get_tooltip();
    f.send(CONTROLLER, 3, &[400]);
    f.pointer(Some(PARENT), (501, 300));
    let placement = f.placement().expect("placed");
    let now = Instant::now();
    assert!(placement.shown(now).is_none());
    assert!(placement.shown(now + Duration::from_millis(500)).is_some());
}
