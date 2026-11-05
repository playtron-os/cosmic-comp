//! `grants-check client [ENGINE APP_ID]`: the guarded and shell-only globals a client sees, on the
//! compositor's own socket or, given a sandbox engine and app id, on a `wp_security_context_v1`
//! socket tagged with them; then whether a clipboard copy (data control) from that client goes
//! through (`kept`), is refused (`refused`), or cannot be made (`hidden`). Prints
//! `globals=a,b,...` and `copy=...`.
//!
//! `grants-check paste ENGINE APP_ID [window]`: whether a boxed app's paste on a connection that
//! hangs up once it has asked gets the session's text; see [`paste`]. `grants-check type ENGINE
//! APP_ID`: whether keys sent on such a connection reach the app's window; see [`type_key`].
//! `grants-check box ENGINE APP_ID` keeps such a socket for a real app; see [`boxed`].
//!
//! `grants-check registry current|off|older|newer|allow|deny WORKSPACE APP_ID CLASS,...`: a
//! stand-in workspace registry on the session bus. `current` lists APP_ID's CLASSES open in
//! WORKSPACE and answers every held request with a deny; `off` lists them off; `older` is a
//! released registry without either call; `newer` adds a field to each row. `allow` and `deny`
//! also show WORKSPACE on screen and answer each held request that way after half a second, as the
//! person would.

use std::{
    collections::HashMap,
    io::{Read, Write},
    os::{
        fd::AsFd,
        unix::net::{UnixListener, UnixStream},
    },
    path::PathBuf,
    process::exit,
    time::{Duration, Instant},
};

use wayland_client::{
    Connection, Dispatch, EventQueue, Proxy, QueueHandle,
    backend::ObjectId,
    delegate_noop,
    globals::{GlobalList, GlobalListContents, registry_queue_init},
    protocol::{
        wl_buffer, wl_compositor, wl_keyboard, wl_registry, wl_seat, wl_shm, wl_shm_pool,
        wl_surface,
    },
};
use wayland_protocols::{
    ext::data_control::v1::client::{
        ext_data_control_device_v1::{self, ExtDataControlDeviceV1},
        ext_data_control_manager_v1::ExtDataControlManagerV1,
        ext_data_control_offer_v1::{self, ExtDataControlOfferV1},
        ext_data_control_source_v1::{self, ExtDataControlSourceV1},
    },
    wp::security_context::v1::client::{
        wp_security_context_manager_v1::WpSecurityContextManagerV1,
        wp_security_context_v1::WpSecurityContextV1,
    },
    xdg::shell::client::{xdg_surface, xdg_toplevel, xdg_wm_base},
};
use wayland_protocols_misc::zwp_virtual_keyboard_v1::client::{
    zwp_virtual_keyboard_manager_v1::ZwpVirtualKeyboardManagerV1,
    zwp_virtual_keyboard_v1::ZwpVirtualKeyboardV1,
};

/// The globals whose filters grants or the boxing change.
const WATCHED: &[&str] = &[
    "zwp_virtual_keyboard_manager_v1",
    "ext_image_copy_capture_manager_v1",
    "ext_output_image_capture_source_manager_v1",
    "ext_data_control_manager_v1",
    "zwlr_data_control_manager_v1",
    "zcosmic_toplevel_info_v1",
    "ext_foreign_toplevel_list_v1",
    "zcosmic_toplevel_manager_v1",
    "cosmic_session_hold_manager_v1",
    "zcosmic_special_action_manager_v1",
    "workspace_transition_manager_v1",
    "kora_workspace_realm_manager_v1",
];

#[derive(Default)]
struct App {
    cancelled: bool,
    offers: HashMap<ObjectId, Vec<String>>,
    selection: Option<Vec<String>>,
    offer: Option<ExtDataControlOfferV1>,
    /// What its own source gives a paste.
    serves: Option<Vec<u8>>,
}

impl Dispatch<wl_registry::WlRegistry, GlobalListContents> for App {
    fn event(
        _: &mut Self,
        _: &wl_registry::WlRegistry,
        _: wl_registry::Event,
        _: &GlobalListContents,
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
    }
}

delegate_noop!(App: ignore wl_seat::WlSeat);
delegate_noop!(App: WpSecurityContextManagerV1);
delegate_noop!(App: WpSecurityContextV1);
delegate_noop!(App: ExtDataControlManagerV1);
delegate_noop!(App: ZwpVirtualKeyboardManagerV1);
delegate_noop!(App: ZwpVirtualKeyboardV1);

impl Dispatch<ExtDataControlSourceV1, ()> for App {
    fn event(
        state: &mut Self,
        _: &ExtDataControlSourceV1,
        event: ext_data_control_source_v1::Event,
        _: &(),
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
        match event {
            ext_data_control_source_v1::Event::Send { fd, .. } => {
                let text = state.serves.as_deref().unwrap_or(b"grants-check");
                let _ = std::fs::File::from(fd).write_all(text);
            }
            ext_data_control_source_v1::Event::Cancelled => state.cancelled = true,
            _ => {}
        }
    }
}

impl Dispatch<ExtDataControlDeviceV1, ()> for App {
    fn event(
        state: &mut Self,
        _: &ExtDataControlDeviceV1,
        event: ext_data_control_device_v1::Event,
        _: &(),
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
        match event {
            ext_data_control_device_v1::Event::DataOffer { id } => {
                state.offers.insert(id.id(), Vec::new());
            }
            ext_data_control_device_v1::Event::Selection { id } => {
                state.selection = Some(
                    id.as_ref()
                        .and_then(|offer| state.offers.get(&offer.id()).cloned())
                        .unwrap_or_default(),
                );
                state.offer = id;
            }
            _ => {}
        }
    }

    wayland_client::event_created_child!(App, ExtDataControlDeviceV1, [
        ext_data_control_device_v1::EVT_DATA_OFFER_OPCODE => (ExtDataControlOfferV1, ()),
    ]);
}

impl Dispatch<ExtDataControlOfferV1, ()> for App {
    fn event(
        state: &mut Self,
        offer: &ExtDataControlOfferV1,
        event: ext_data_control_offer_v1::Event,
        _: &(),
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
        if let ext_data_control_offer_v1::Event::Offer { mime_type } = event {
            state.offers.entry(offer.id()).or_default().push(mime_type);
        }
    }
}

fn fail(what: &str) -> ! {
    eprintln!("grants-check: {what}");
    exit(1);
}

/// Refuse to touch the real session: only a nested compositor's private runtime dir will do.
fn nested_only() {
    let runtime = std::env::var("XDG_RUNTIME_DIR").unwrap_or_default();
    let display = std::env::var("WAYLAND_DISPLAY").unwrap_or_default();
    if runtime.is_empty() || runtime.starts_with("/run/user/") || display.is_empty() {
        fail("XDG_RUNTIME_DIR and WAYLAND_DISPLAY must name a nested compositor");
    }
    if display.contains('/') || !PathBuf::from(&runtime).join(&display).exists() {
        fail("WAYLAND_DISPLAY must be a socket in XDG_RUNTIME_DIR");
    }
}

fn names(globals: &GlobalList) -> String {
    let mut seen: Vec<&str> = WATCHED
        .iter()
        .copied()
        .filter(|name| {
            globals
                .contents()
                .with_list(|list| list.iter().any(|g| g.interface == *name))
        })
        .collect();
    seen.sort_unstable();
    seen.join(",")
}

fn connect(conn: Connection) -> (Connection, GlobalList, EventQueue<App>) {
    let (globals, queue) =
        registry_queue_init::<App>(&conn).unwrap_or_else(|_| fail("no registry"));
    (conn, globals, queue)
}

/// Dispatch `queue` until `done` or `timeout`.
fn pump(
    queue: &mut EventQueue<App>,
    app: &mut App,
    timeout: Duration,
    done: impl Fn(&App) -> bool,
) {
    let end = Instant::now() + timeout;
    while !done(app) && Instant::now() < end {
        queue
            .roundtrip(app)
            .unwrap_or_else(|_| fail("the compositor hung up"));
        std::thread::sleep(Duration::from_millis(20));
    }
}

fn client(tag: Option<(&str, &str)>) {
    nested_only();
    let session = Connection::connect_to_env().unwrap_or_else(|_| fail("cannot connect"));
    let (_session, session_globals, mut session_queue) = connect(session);
    let mut app = App::default();
    // The tagged socket's listener and the end of its close pipe live as long as the check.
    let mut _socket = None;
    let (_conn, globals, mut queue) = match tag {
        // A second client of the session, so the copy is read back by another.
        None => connect(Connection::connect_to_env().unwrap_or_else(|_| fail("cannot connect"))),
        Some((engine, app_id)) => {
            let socket = Tagged::new(&session_globals, &mut session_queue, engine, app_id);
            let conn = socket.connect();
            _socket = Some(socket);
            connect(conn)
        }
    };
    println!("globals={}", names(&globals));

    let qh = queue.handle();
    let copy = match (
        globals.bind::<ExtDataControlManagerV1, _, _>(&qh, 1..=1, ()),
        globals.bind::<wl_seat::WlSeat, _, _>(&qh, 1..=1, ()),
    ) {
        (Ok(manager), Ok(seat)) => {
            // Our own type: the compositor keeps earlier copies once their clients are gone.
            let mime = format!("text/x-grants-check-{}", std::process::id());
            let device = manager.get_data_device(&seat, &qh, ());
            let source = manager.create_data_source(&qh, ());
            source.offer(mime.clone());
            device.set_selection(Some(&source));
            // Refused or taken over by the compositor's clipboard copy, the source is cancelled.
            pump(&mut queue, &mut app, Duration::from_secs(2), |app| {
                app.cancelled
            });
            // Read back on the session's own socket, as any clipboard manager would.
            let sqh = session_queue.handle();
            let manager: ExtDataControlManagerV1 = session_globals
                .bind(&sqh, 1..=1, ())
                .unwrap_or_else(|_| fail("the session has no data control"));
            let seat: wl_seat::WlSeat = session_globals.bind(&sqh, 1..=1, ()).unwrap();
            let mut observer = App::default();
            let _device = manager.get_data_device(&seat, &sqh, ());
            pump(
                &mut session_queue,
                &mut observer,
                Duration::from_secs(3),
                |o| o.selection.is_some(),
            );
            match observer.selection {
                Some(mimes) if mimes.contains(&mime) => "kept",
                _ => "refused",
            }
        }
        _ => "hidden",
    };
    println!("copy={copy}");
}

/// A `wp_security_context_v1` socket tagged with an engine and app id, open while this lives.
struct Tagged {
    path: PathBuf,
    _listener: UnixListener,
    _close: UnixStream,
}

impl Tagged {
    fn new(globals: &GlobalList, queue: &mut EventQueue<App>, engine: &str, app_id: &str) -> Self {
        let qh = queue.handle();
        let manager: WpSecurityContextManagerV1 = globals
            .bind(&qh, 1..=1, ())
            .unwrap_or_else(|_| fail("no wp_security_context_manager_v1"));
        let path = PathBuf::from(std::env::var("XDG_RUNTIME_DIR").unwrap())
            .join(format!("grants-check-{}", std::process::id()));
        let _ = std::fs::remove_file(&path);
        let listener = UnixListener::bind(&path).unwrap_or_else(|_| fail("cannot listen"));
        let (close, kept) = UnixStream::pair().unwrap();
        let context = manager.create_listener(listener.as_fd(), close.as_fd(), &qh, ());
        context.set_sandbox_engine(engine.into());
        context.set_app_id(app_id.into());
        context.set_instance_id("grants-check".into());
        context.commit();
        queue.roundtrip(&mut App::default()).unwrap();
        Self {
            path,
            _listener: listener,
            _close: kept,
        }
    }

    fn connect(&self) -> Connection {
        let stream = UnixStream::connect(&self.path).unwrap_or_else(|_| fail("cannot connect"));
        Connection::from_socket(stream).unwrap()
    }
}

impl Drop for Tagged {
    fn drop(&mut self) {
        let _ = std::fs::remove_file(&self.path);
    }
}

/// A window of the boxed app, whether it has the keyboard, and where the keys it gets go.
#[derive(Default)]
struct Window {
    configured: bool,
    focused: bool,
    keys: Option<std::sync::mpsc::Sender<u32>>,
}

impl Dispatch<wl_registry::WlRegistry, GlobalListContents> for Window {
    fn event(
        _: &mut Self,
        _: &wl_registry::WlRegistry,
        _: wl_registry::Event,
        _: &GlobalListContents,
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
    }
}

delegate_noop!(Window: wl_compositor::WlCompositor);
delegate_noop!(Window: wl_shm_pool::WlShmPool);
delegate_noop!(Window: ignore wl_shm::WlShm);
delegate_noop!(Window: ignore wl_buffer::WlBuffer);
delegate_noop!(Window: ignore wl_surface::WlSurface);
delegate_noop!(Window: ignore wl_seat::WlSeat);
delegate_noop!(Window: ignore xdg_toplevel::XdgToplevel);

impl Dispatch<xdg_wm_base::XdgWmBase, ()> for Window {
    fn event(
        _: &mut Self,
        base: &xdg_wm_base::XdgWmBase,
        event: xdg_wm_base::Event,
        _: &(),
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
        if let xdg_wm_base::Event::Ping { serial } = event {
            base.pong(serial);
        }
    }
}

impl Dispatch<xdg_surface::XdgSurface, ()> for Window {
    fn event(
        state: &mut Self,
        surface: &xdg_surface::XdgSurface,
        event: xdg_surface::Event,
        _: &(),
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
        if let xdg_surface::Event::Configure { serial } = event {
            surface.ack_configure(serial);
            state.configured = true;
        }
    }
}

impl Dispatch<wl_keyboard::WlKeyboard, ()> for Window {
    fn event(
        state: &mut Self,
        _: &wl_keyboard::WlKeyboard,
        event: wl_keyboard::Event,
        _: &(),
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
        match event {
            wl_keyboard::Event::Enter { .. } => state.focused = true,
            wl_keyboard::Event::Leave { .. } => state.focused = false,
            wl_keyboard::Event::Key {
                key,
                state: wayland_client::WEnum::Value(wl_keyboard::KeyState::Pressed),
                ..
            } => {
                if let Some(keys) = &state.keys {
                    let _ = keys.send(key);
                }
            }
            _ => {}
        }
    }
}

/// Map a window on its own connection of the tagged socket, and wait for it to get the
/// keyboard; the keys it gets go to `keys`. Returns the connection, kept open while the check runs.
fn open_window(socket: &Tagged, keys: Option<std::sync::mpsc::Sender<u32>>) -> Connection {
    const SIZE: i32 = 200;
    let conn = socket.connect();
    let (globals, mut queue) =
        registry_queue_init::<Window>(&conn).unwrap_or_else(|_| fail("no registry"));
    let qh = queue.handle();
    let compositor: wl_compositor::WlCompositor = globals
        .bind(&qh, 1..=4, ())
        .unwrap_or_else(|_| fail("the window has no wl_compositor"));
    let shm: wl_shm::WlShm = globals
        .bind(&qh, 1..=1, ())
        .unwrap_or_else(|_| fail("the window has no wl_shm"));
    let base: xdg_wm_base::XdgWmBase = globals
        .bind(&qh, 1..=1, ())
        .unwrap_or_else(|_| fail("the window has no xdg_wm_base"));
    let seat: wl_seat::WlSeat = globals
        .bind(&qh, 1..=1, ())
        .unwrap_or_else(|_| fail("the window has no wl_seat"));
    let _keyboard = seat.get_keyboard(&qh, ());
    let surface = compositor.create_surface(&qh, ());
    let xdg = base.get_xdg_surface(&surface, &qh, ());
    let toplevel = xdg.get_toplevel(&qh, ());
    toplevel.set_title("grants-check".into());
    surface.commit();
    let mut window = Window {
        keys,
        ..Window::default()
    };
    pump_window(&mut queue, &mut window, |w| w.configured);
    if !window.configured {
        fail("the window was never configured");
    }

    // A grey buffer in a file of its own: shared memory needs a file, not its name.
    let path = socket.path.with_extension("buffer");
    let file = std::fs::OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .truncate(true)
        .open(&path)
        .unwrap_or_else(|_| fail("cannot make a buffer"));
    let _ = std::fs::remove_file(&path);
    let pixels = vec![0x80u8; (SIZE * SIZE * 4) as usize];
    (&file).write_all(&pixels).unwrap();
    let pool = shm.create_pool(file.as_fd(), SIZE * SIZE * 4, &qh, ());
    let buffer = pool.create_buffer(0, SIZE, SIZE, SIZE * 4, wl_shm::Format::Xrgb8888, &qh, ());
    surface.attach(Some(&buffer), 0, 0);
    surface.damage(0, 0, SIZE, SIZE);
    surface.commit();
    pump_window(&mut queue, &mut window, |w| w.focused);
    if !window.focused {
        fail("the window never got the keyboard");
    }
    std::thread::spawn(
        move || {
            while queue.blocking_dispatch(&mut window).is_ok() {}
        },
    );
    conn
}

fn pump_window(
    queue: &mut EventQueue<Window>,
    window: &mut Window,
    done: impl Fn(&Window) -> bool,
) {
    let end = Instant::now() + Duration::from_secs(5);
    while !done(window) && Instant::now() < end {
        queue
            .roundtrip(window)
            .unwrap_or_else(|_| fail("the compositor hung up on the window"));
        std::thread::sleep(Duration::from_millis(20));
    }
}

/// Put `text` on the clipboard from the session's own socket, served from a thread.
fn serve_selection(text: &str, mime: &str) {
    let conn = Connection::connect_to_env().unwrap_or_else(|_| fail("cannot connect"));
    let (conn, globals, mut queue) = connect(conn);
    let qh = queue.handle();
    let manager: ExtDataControlManagerV1 = globals
        .bind(&qh, 1..=1, ())
        .unwrap_or_else(|_| fail("the session has no data control"));
    let seat: wl_seat::WlSeat = globals.bind(&qh, 1..=1, ()).unwrap();
    let device = manager.get_data_device(&seat, &qh, ());
    let source = manager.create_data_source(&qh, ());
    source.offer(mime.into());
    device.set_selection(Some(&source));
    let mut app = App {
        serves: Some(text.as_bytes().to_vec()),
        ..App::default()
    };
    pump(&mut queue, &mut app, Duration::from_secs(3), |app| {
        app.selection.is_some()
    });
    std::thread::spawn(move || {
        let _kept = (conn, device, source);
        while queue.blocking_dispatch(&mut app).is_ok() {}
    });
}

/// `grants-check paste ENGINE APP_ID [window]`: a boxed app pastes the session's text through
/// data control on a connection of its own that hangs up once it has asked, as paste tools do,
/// then reads its pipe; with `window` while its own window has the keyboard. Prints
/// `paste=pasted`, `paste=empty` (refused) or `paste=waiting` (still held after 10 s).
fn paste(engine: &str, app_id: &str, window: bool) {
    nested_only();
    let mime = "text/plain;charset=utf-8";
    let text = format!("grants-check paste {}", std::process::id());
    serve_selection(&text, mime);
    let session = Connection::connect_to_env().unwrap_or_else(|_| fail("cannot connect"));
    let (_session, session_globals, mut session_queue) = connect(session);
    let socket = Tagged::new(&session_globals, &mut session_queue, engine, app_id);
    let _window = window.then(|| open_window(&socket, None));

    let read = {
        let (conn, globals, mut queue) = connect(socket.connect());
        let qh = queue.handle();
        let manager: ExtDataControlManagerV1 = globals
            .bind(&qh, 1..=1, ())
            .unwrap_or_else(|_| fail("the boxed app has no data control"));
        let seat: wl_seat::WlSeat = globals.bind(&qh, 1..=1, ()).unwrap();
        let mut app = App::default();
        let _device = manager.get_data_device(&seat, &qh, ());
        pump(&mut queue, &mut app, Duration::from_secs(3), |app| {
            app.selection.is_some()
        });
        let Some(offer) = app.offer.take() else {
            fail("the boxed app was offered no selection");
        };
        let (read, write) = std::io::pipe().unwrap();
        offer.receive(mime.into(), write.as_fd());
        drop(write);
        queue
            .roundtrip(&mut app)
            .unwrap_or_else(|_| fail("the compositor hung up"));
        drop((queue, conn));
        read
    };

    let (tx, rx) = std::sync::mpsc::channel();
    std::thread::spawn(move || {
        let mut read = read;
        let mut got = Vec::new();
        let _ = read.read_to_end(&mut got);
        let _ = tx.send(got);
    });
    let result = match rx.recv_timeout(Duration::from_secs(10)) {
        Ok(got) if got == text.as_bytes() => "pasted",
        Ok(got) if got.is_empty() => "empty",
        Ok(_) => "other",
        Err(_) => "waiting",
    };
    println!("paste={result}");
}

/// `grants-check type ENGINE APP_ID`: a boxed app types a key through the virtual keyboard on a
/// connection of its own that hangs up at once, as `wtype` does, while its own window has the
/// keyboard. Prints `type=typed` once the window gets the key, else `type=nothing` after 10 s.
fn type_key(engine: &str, app_id: &str) {
    // `a` on an evdev keyboard.
    const KEY: u32 = 30;
    nested_only();
    let session = Connection::connect_to_env().unwrap_or_else(|_| fail("cannot connect"));
    let (_session, session_globals, mut session_queue) = connect(session);
    let socket = Tagged::new(&session_globals, &mut session_queue, engine, app_id);
    let (keys, typed) = std::sync::mpsc::channel();
    let _window = open_window(&socket, Some(keys));
    {
        let (conn, globals, mut queue) = connect(socket.connect());
        let qh = queue.handle();
        let manager: ZwpVirtualKeyboardManagerV1 = globals
            .bind(&qh, 1..=1, ())
            .unwrap_or_else(|_| fail("the boxed app has no virtual keyboard"));
        let seat: wl_seat::WlSeat = globals.bind(&qh, 1..=1, ()).unwrap();
        let keyboard = manager.create_virtual_keyboard(&seat, &qh, ());
        let keymap = b"xkb_keymap {\n  xkb_keycodes { include \"evdev\" };\n  xkb_types { include \"complete\" };\n  xkb_compatibility { include \"complete\" };\n  xkb_symbols { include \"pc+us\" };\n};\n\0";
        let path = socket.path.with_extension("keymap");
        let file = std::fs::OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(true)
            .open(&path)
            .unwrap_or_else(|_| fail("cannot make a keymap"));
        let _ = std::fs::remove_file(&path);
        (&file).write_all(keymap).unwrap();
        keyboard.keymap(
            wl_keyboard::KeymapFormat::XkbV1.into(),
            file.as_fd(),
            keymap.len() as u32,
        );
        let mut app = App::default();
        for state in [1, 0] {
            keyboard.key(0, KEY, state);
            queue
                .roundtrip(&mut app)
                .unwrap_or_else(|_| fail("the compositor hung up"));
        }
        keyboard.destroy();
        drop((queue, conn));
    }
    let result = match typed.recv_timeout(Duration::from_secs(10)) {
        Ok(KEY) => "typed",
        Ok(_) => "other",
        Err(_) => "nothing",
    };
    println!("type={result}");
}

/// `grants-check box ENGINE APP_ID`: a tagged socket for a real app, until stdin closes. Prints
/// its name, for the app's `WAYLAND_DISPLAY`.
fn boxed(engine: &str, app_id: &str) {
    nested_only();
    let session = Connection::connect_to_env().unwrap_or_else(|_| fail("cannot connect"));
    let (_session, globals, mut queue) = connect(session);
    let socket = Tagged::new(&globals, &mut queue, engine, app_id);
    println!("{}", socket.path.file_name().unwrap().to_string_lossy());
    let _ = std::io::stdin().read_to_end(&mut Vec::new());
}

type Row = (String, String, String, Vec<String>, Vec<String>);
/// A row with a field this compositor does not know.
type NewerRow = (String, String, String, Vec<String>, Vec<String>, u32);

struct Current(Vec<Row>);

#[zbus::interface(name = "one.playtron.Workspaces1")]
impl Current {
    fn display_grants(&self) -> Vec<Row> {
        self.0.clone()
    }
}

struct Deny;

#[zbus::interface(name = "one.playtron.AgentAccess1")]
impl Deny {
    fn judge_display(
        &self,
        _pidfd: zbus::zvariant::OwnedFd,
        _workspace: String,
        _kind: String,
        _id: String,
        _class: String,
        _change: bool,
    ) -> String {
        "deny".into()
    }
}

/// The person's answer to every held request, as the dialog gives it.
struct Answers(&'static str);

#[zbus::interface(name = "one.playtron.AgentAccess1")]
impl Answers {
    fn judge_display(
        &self,
        _pidfd: zbus::zvariant::OwnedFd,
        _workspace: String,
        _kind: String,
        _id: String,
        _class: String,
        _change: bool,
    ) -> String {
        // Nobody answers before a paste tool has hung up.
        std::thread::sleep(Duration::from_millis(500));
        self.0.into()
    }
}

/// A registry whose workspace is on screen, so the boxed app's windows show and take the keyboard.
struct Shown(Vec<Row>);

#[zbus::interface(name = "one.playtron.Workspaces1")]
impl Shown {
    fn display_grants(&self) -> Vec<Row> {
        self.0.clone()
    }

    fn active(&self) -> String {
        self.0[0].0.clone()
    }

    fn list(&self) -> Vec<(String, String, String, String, bool)> {
        let id = self.0[0].0.clone();
        vec![(id, "Work".into(), "#00b4d7".into(), "active".into(), true)]
    }
}

struct Older;

#[zbus::interface(name = "one.playtron.Workspaces1")]
impl Older {
    fn active(&self) -> String {
        String::new()
    }
}

struct Newer(Vec<Row>);

#[zbus::interface(name = "one.playtron.Workspaces1")]
impl Newer {
    fn display_grants(&self) -> Vec<NewerRow> {
        self.0
            .iter()
            .cloned()
            .map(|(w, k, i, open, off)| (w, k, i, open, off, 7))
            .collect()
    }
}

fn registry(kind: &str, workspace: &str, app_id: &str, open: &str) -> zbus::Result<()> {
    let bus = std::env::var("DBUS_SESSION_BUS_ADDRESS").unwrap_or_default();
    let runtime = std::env::var("XDG_RUNTIME_DIR").unwrap_or_default();
    if runtime.is_empty() || runtime.starts_with("/run/user/") || !bus.contains(&runtime) {
        fail("DBUS_SESSION_BUS_ADDRESS must be the nested session's private bus");
    }
    let classes: Vec<String> = open
        .split(',')
        .filter(|c| !c.is_empty())
        .map(str::to_owned)
        .collect();
    // `off` turns the classes off instead of opening them.
    let (open, off) = match kind {
        "off" => (Vec::new(), classes),
        _ => (classes, Vec::new()),
    };
    let rows = vec![(
        workspace.to_owned(),
        "app".to_owned(),
        app_id.to_owned(),
        open,
        off,
    )];
    let path = "/one/playtron/Workspaces1";
    let builder = zbus::blocking::connection::Builder::session()?;
    let builder = match kind {
        "current" | "off" => builder
            .serve_at(path, Current(rows))?
            .serve_at(path, Deny)?,
        "older" => builder.serve_at(path, Older)?,
        "newer" => builder.serve_at(path, Newer(rows))?.serve_at(path, Deny)?,
        "allow" => builder
            .serve_at(path, Shown(rows))?
            .serve_at(path, Answers("allow"))?,
        "deny" => builder
            .serve_at(path, Shown(rows))?
            .serve_at(path, Answers("deny"))?,
        _ => fail("registry: current, off, older, newer, allow or deny"),
    };
    let _conn = builder.name("one.playtron.Workspaces1")?.build()?;
    println!("registry ready");
    loop {
        std::thread::park();
    }
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let args: Vec<&str> = args.iter().map(String::as_str).collect();
    match args.as_slice() {
        ["client"] => client(None),
        ["client", engine, app_id] => client(Some((engine, app_id))),
        ["paste", engine, app_id] => paste(engine, app_id, false),
        ["paste", engine, app_id, "window"] => paste(engine, app_id, true),
        ["type", engine, app_id] => type_key(engine, app_id),
        ["box", engine, app_id] => boxed(engine, app_id),
        ["registry", kind, workspace, app_id, open] => {
            if let Err(err) = registry(kind, workspace, app_id, open) {
                fail(&format!("registry: {err}"));
            }
        }
        _ => fail(
            "usage: client [ENGINE APP_ID] | paste ENGINE APP_ID [window] | type ENGINE APP_ID \
             | box ENGINE APP_ID | registry KIND WORKSPACE APP_ID CLASSES",
        ),
    }
}
