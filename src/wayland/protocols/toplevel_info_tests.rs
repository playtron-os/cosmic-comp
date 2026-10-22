use super::*;
use crate::wayland::protocols::workspace::{Request, delegate_workspace};
use std::sync::Arc;

#[derive(Clone, Default)]
struct TestWindow(Arc<UserDataMap>);

impl PartialEq for TestWindow {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

impl IsAlive for TestWindow {
    fn alive(&self) -> bool {
        true
    }
}

impl Window for TestWindow {
    fn title(&self) -> String {
        "Test".into()
    }
    fn app_id(&self) -> String {
        "test".into()
    }
    fn is_activated(&self) -> bool {
        false
    }
    fn is_maximized(&self) -> bool {
        false
    }
    fn is_fullscreen(&self) -> bool {
        false
    }
    fn is_minimized(&self) -> bool {
        false
    }
    fn is_sticky(&self) -> bool {
        false
    }
    fn is_resizing(&self) -> bool {
        false
    }
    fn global_geometry(&self) -> Option<Rectangle<i32, Global>> {
        self.0
            .get::<Place>()
            .and_then(|place| *place.0.lock().unwrap())
    }
    fn user_data(&self) -> &UserDataMap {
        &self.0
    }
}

/// Where the shell last put a test window.
struct Place(Mutex<Option<Rectangle<i32, Global>>>);

fn put(window: &TestWindow, x: i32, y: i32) {
    window
        .0
        .insert_if_missing_threadsafe(|| Place(Mutex::new(None)));
    *window.0.get::<Place>().unwrap().0.lock().unwrap() =
        Some(Rectangle::new((x, y).into(), (300, 200).into()));
}

struct Model {
    info: ToplevelInfoState<Self, TestWindow>,
    workspaces: WorkspaceState<Self>,
}

smithay::delegate_dispatch2!(Model);
delegate_toplevel_info!(Model, TestWindow);
delegate_workspace!(Model);

impl ForeignToplevelListHandler for Model {
    fn foreign_toplevel_list_state(&mut self) -> &mut ForeignToplevelListState {
        &mut self.info.foreign_toplevel_list
    }
}

impl ToplevelInfoHandler for Model {
    type Window = TestWindow;
    fn toplevel_info_state(&self) -> &ToplevelInfoState<Self, TestWindow> {
        &self.info
    }
    fn toplevel_info_state_mut(&mut self) -> &mut ToplevelInfoState<Self, TestWindow> {
        &mut self.info
    }
}

impl smithay::wayland::output::OutputHandler for Model {}

impl WorkspaceHandler for Model {
    fn workspace_state(&self) -> &WorkspaceState<Self> {
        &self.workspaces
    }
    fn workspace_state_mut(&mut self) -> &mut WorkspaceState<Self> {
        &mut self.workspaces
    }
    fn commit_requests(&mut self, _: &DisplayHandle, _: Vec<Request>) {}
}

#[test]
fn hide_show_and_parking_keep_identity_but_actual_unmap_does_not() {
    let display = smithay::reexports::wayland_server::Display::<Model>::new().unwrap();
    let dh = display.handle();
    let mut model = Model {
        info: ToplevelInfoState::new(&dh, |_| true),
        workspaces: WorkspaceState::new(&dh, |_| true),
    };
    let window = TestWindow::default();
    model.info.new_toplevel(&window, &model.workspaces);
    let original = foreign_toplevel_identifier(&window).unwrap();
    let first = window
        .user_data()
        .get::<ToplevelState>()
        .unwrap()
        .lock()
        .unwrap()
        .foreign_handle
        .clone()
        .unwrap();
    for _ in 0..3 {
        model.info.set_visible(&model.workspaces, |_| false);
        assert!(first.is_closed());
        assert!(foreign_toplevel_identifier(&window).is_none());
        assert_eq!(
            mapped_toplevel_identifier(&window).as_deref(),
            Some(original.as_str())
        );
        model.info.set_visible(&model.workspaces, |_| true);
        assert_eq!(
            foreign_toplevel_identifier(&window).as_deref(),
            Some(original.as_str())
        );
        assert!(
            window_from_ext::<TestWindow, Model>(&model, first.clone()).is_none(),
            "the first closed handle stays retired"
        );
    }
    model.info.remove_toplevel(&window);
    assert!(mapped_toplevel_identifier(&window).is_none());
    model.info.new_toplevel(&window, &model.workspaces);
    assert_ne!(foreign_toplevel_identifier(&window).unwrap(), original);
}

#[test]
fn a_window_shown_again_keeps_its_output_and_workspace() {
    use cosmic_protocols::workspace::v2::server::zcosmic_workspace_handle_v2;
    use smithay::output::{Output, PhysicalProperties, Subpixel};
    let display = smithay::reexports::wayland_server::Display::<Model>::new().unwrap();
    let dh = display.handle();
    let mut model = Model {
        info: ToplevelInfoState::new(&dh, |_| true),
        workspaces: WorkspaceState::new(&dh, |_| true),
    };
    let output = Output::new(
        "eDP-test".into(),
        PhysicalProperties {
            size: (300, 200).into(),
            subpixel: Subpixel::Unknown,
            make: "test".into(),
            model: "test".into(),
            serial_number: String::new(),
        },
    );
    let workspace = {
        let mut update = model.workspaces.update();
        let group = update.create_workspace_group();
        update
            .create_workspace(
                &group,
                zcosmic_workspace_handle_v2::TilingState::FloatingOnly,
                None,
            )
            .unwrap()
    };
    let window = TestWindow::default();
    model.info.new_toplevel(&window, &model.workspaces);
    toplevel_enter_output(&window, &output);
    toplevel_enter_workspace(&window, &workspace);

    // A realm switch hides the window and shows it again; it never left its place.
    model.info.set_visible(&model.workspaces, |_| false);
    model.info.set_visible(&model.workspaces, |_| true);

    let state = window.user_data().get::<ToplevelState>().unwrap();
    let state = state.lock().unwrap();
    assert_eq!(state.outputs, vec![output]);
    assert!(state.in_workspace(&workspace));
}

#[derive(Default)]
struct Peer(smithay::wayland::compositor::CompositorClientState);
impl smithay::reexports::wayland_server::backend::ClientData for Peer {}

// wl_output's bind reads the client's scale; the test makes no surfaces.
impl smithay::wayland::compositor::CompositorHandler for Model {
    fn compositor_state(&mut self) -> &mut smithay::wayland::compositor::CompositorState {
        unreachable!("no surfaces")
    }
    fn client_compositor_state<'a>(
        &self,
        client: &'a Client,
    ) -> &'a smithay::wayland::compositor::CompositorClientState {
        &client.get_data::<Peer>().unwrap().0
    }
    fn commit(&mut self, _: &WlSurface) {}
}

// The server crate has no wl_display or wl_registry objects to take these from.
const GET_REGISTRY: u16 = 1;
const GLOBAL: u16 = 0;
const BIND: u16 = 0;

/// A client on a private socket, speaking just enough of the wire format to bind
/// globals and read the events the compositor sends.
struct Wire(std::os::unix::net::UnixStream);

fn word(value: u32) -> Vec<u8> {
    value.to_ne_bytes().to_vec()
}

fn string(text: &str) -> Vec<u8> {
    let mut bytes = word(text.len() as u32 + 1);
    bytes.extend(text.as_bytes());
    bytes.push(0);
    bytes.resize(bytes.len().next_multiple_of(4), 0);
    bytes
}

impl Wire {
    fn send(&mut self, object: u32, opcode: u16, args: &[u8]) {
        use std::io::Write;
        let mut message = word(object);
        message.extend(word(((8 + args.len() as u32) << 16) | u32::from(opcode)));
        message.extend(args);
        self.0.write_all(&message).unwrap();
    }

    /// Every event waiting on the socket, as (object, opcode, arguments).
    fn events(&mut self) -> Vec<(u32, u16, Vec<u8>)> {
        use std::io::Read;
        let mut events = Vec::new();
        let mut header = [0; 8];
        while self.0.read_exact(&mut header).is_ok() {
            let object = u32::from_ne_bytes(header[..4].try_into().unwrap());
            let size_and_opcode = u32::from_ne_bytes(header[4..].try_into().unwrap());
            let mut args = vec![0; (size_and_opcode >> 16) as usize - 8];
            self.0.read_exact(&mut args).unwrap();
            events.push((object, size_and_opcode as u16, args));
        }
        events
    }
}

#[test]
fn a_window_moved_by_a_scale_change_is_committed_with_done() {
    use smithay::{
        output::{Mode, PhysicalProperties, Scale, Subpixel},
        reexports::{
            wayland_protocols::ext::foreign_toplevel_list::v1::server::{
                ext_foreign_toplevel_handle_v1, ext_foreign_toplevel_list_v1,
            },
            wayland_server::Display,
        },
    };
    let mut display = Display::<Model>::new().unwrap();
    let mut dh = display.handle();
    let mut model = Model {
        info: ToplevelInfoState::new(&dh, |_| true),
        workspaces: WorkspaceState::new(&dh, |_| true),
    };
    let output = Output::new(
        "eDP-test".into(),
        PhysicalProperties {
            size: (300, 200).into(),
            subpixel: Subpixel::Unknown,
            make: "test".into(),
            model: "test".into(),
            serial_number: String::new(),
        },
    );
    let _global = output.create_global::<Model>(&dh);
    let mode = Mode {
        size: (1600, 1000).into(),
        refresh: 60_000,
    };
    output.change_current_state(
        Some(mode),
        None,
        Some(Scale::Fractional(1.0)),
        Some((0, 0).into()),
    );
    let (server, socket) = std::os::unix::net::UnixStream::pair().unwrap();
    socket
        .set_read_timeout(Some(std::time::Duration::from_millis(100)))
        .unwrap();
    dh.insert_client(server, std::sync::Arc::new(Peer::default()))
        .unwrap();
    let mut wire = Wire(socket);
    let mut roundtrip = |model: &mut Model, wire: &mut Wire| {
        display.dispatch_clients(model).unwrap();
        display.flush_clients().unwrap();
        wire.events()
    };

    // Bind wl_output (3), the foreign toplevel list (4) and cosmic toplevel info (5).
    wire.send(1, GET_REGISTRY, &word(2));
    let globals = roundtrip(&mut model, &mut wire);
    for (id, interface, version) in [
        (3, "wl_output", 4),
        (4, "ext_foreign_toplevel_list_v1", 1),
        (5, "zcosmic_toplevel_info_v1", 3),
    ] {
        let name = globals
            .iter()
            .find(|(object, opcode, args)| {
                *object == 2 && *opcode == GLOBAL && args[4..].starts_with(&string(interface))
            })
            .map(|(_, _, args)| u32::from_ne_bytes(args[..4].try_into().unwrap()))
            .unwrap();
        let mut args = word(name);
        args.extend(string(interface));
        args.extend(word(version));
        args.extend(word(id));
        wire.send(2, BIND, &args);
    }
    roundtrip(&mut model, &mut wire);

    let window = TestWindow::default();
    put(&window, 650, 126);
    model.info.new_toplevel(&window, &model.workspaces);
    toplevel_enter_output(&window, &output);
    let foreign = roundtrip(&mut model, &mut wire)
        .into_iter()
        .find(|(object, opcode, _)| {
            *object == 4 && *opcode == ext_foreign_toplevel_list_v1::EVT_TOPLEVEL_OPCODE
        })
        .map(|(_, _, args)| u32::from_ne_bytes(args[..4].try_into().unwrap()))
        .unwrap();
    let mut args = word(6);
    args.extend(word(foreign));
    wire.send(
        5,
        zcosmic_toplevel_info_v1::REQ_GET_COSMIC_TOPLEVEL_OPCODE,
        &args,
    );
    roundtrip(&mut model, &mut wire);
    model.info.refresh(&model.workspaces);
    roundtrip(&mut model, &mut wire);

    // The scale goes to 150%, and the shell moves the window to keep its place on screen.
    output.change_current_state(None, None, Some(Scale::Fractional(1.5)), None);
    put(&window, 384, 47);
    model.info.refresh(&model.workspaces);
    let events = roundtrip(&mut model, &mut wire);
    let geometry = events
        .iter()
        .position(|(object, opcode, _)| {
            *object == 6 && *opcode == zcosmic_toplevel_handle_v1::EVT_GEOMETRY_OPCODE
        })
        .expect("the new place is sent");
    assert!(
        events[geometry..].iter().any(|(object, opcode, _)| {
            *object == foreign && *opcode == ext_foreign_toplevel_handle_v1::EVT_DONE_OPCODE
        }),
        "a done follows it, or clients keep the old place"
    );
}
