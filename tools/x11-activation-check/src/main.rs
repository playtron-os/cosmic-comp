//! `x11-activation-check SOCKET ask|quiet`: an X11 window on the server at SOCKET that maps as
//! some programs' windows do: it asks for `_NET_ACTIVE_WINDOW` (source pager, no timestamp) as
//! soon as the window manager puts it in NormalState, and draws its first frame only after that.
//! With `quiet` it never asks. Prints `asked`, `focus in|out` and `active true|false` (the root's
//! `_NET_ACTIVE_WINDOW` naming it or not) as they happen.

use std::{os::unix::net::UnixStream, process::exit};

use x11rb::{
    connection::Connection,
    protocol::{
        Event,
        xproto::{
            AtomEnum, ChangeWindowAttributesAux, ClientMessageEvent, ConnectionExt as _,
            CreateGCAux, CreateWindowAux, EventMask, NotifyDetail, PropMode, Rectangle,
            WindowClass,
        },
    },
    rust_connection::{DefaultStream, RustConnection},
    wrapper::ConnectionExt as _,
};

const COLOUR: u32 = 0x00e0_5020;
const NORMAL_STATE: u32 = 1;

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let (socket, ask) = match &args.iter().map(String::as_str).collect::<Vec<_>>()[..] {
        [socket, "ask"] => (socket.to_string(), true),
        [socket, "quiet"] => (socket.to_string(), false),
        _ => {
            eprintln!("usage: x11-activation-check SOCKET ask|quiet");
            exit(2);
        }
    };
    let stream = UnixStream::connect(&socket).expect("no X server at the socket");
    let (stream, _) = DefaultStream::from_unix_stream(stream).expect("stream");
    let conn = RustConnection::connect_to_stream_with_auth_info(stream, 0, Vec::new(), Vec::new())
        .expect("X connection");
    let screen = conn.setup().roots[0].clone();
    let root = screen.root;
    let atom = |name: &str| {
        conn.intern_atom(false, name.as_bytes())
            .unwrap()
            .reply()
            .unwrap()
            .atom
    };
    let wm_state = atom("WM_STATE");
    let net_active_window = atom("_NET_ACTIVE_WINDOW");
    let window_type = atom("_NET_WM_WINDOW_TYPE");
    let type_normal = atom("_NET_WM_WINDOW_TYPE_NORMAL");

    let window = conn.generate_id().unwrap();
    conn.create_window(
        screen.root_depth,
        window,
        root,
        100,
        100,
        480,
        320,
        0,
        WindowClass::INPUT_OUTPUT,
        screen.root_visual,
        // No background: nothing reaches the screen until the first frame is drawn.
        &CreateWindowAux::new().event_mask(
            EventMask::EXPOSURE
                | EventMask::STRUCTURE_NOTIFY
                | EventMask::PROPERTY_CHANGE
                | EventMask::FOCUS_CHANGE,
        ),
    )
    .unwrap();
    conn.change_property8(
        PropMode::REPLACE,
        window,
        AtomEnum::WM_NAME,
        AtomEnum::STRING,
        b"Setup",
    )
    .unwrap();
    conn.change_property8(
        PropMode::REPLACE,
        window,
        AtomEnum::WM_CLASS,
        AtomEnum::STRING,
        b"setup.exe\0steam_app_default\0",
    )
    .unwrap();
    // WM_HINTS: input, so the window manager gives it X focus when it focuses it.
    conn.change_property32(
        PropMode::REPLACE,
        window,
        AtomEnum::WM_HINTS,
        AtomEnum::WM_HINTS,
        &[1, 1, 0, 0, 0, 0, 0, 0, 0],
    )
    .unwrap();
    conn.change_property32(
        PropMode::REPLACE,
        window,
        window_type,
        AtomEnum::ATOM,
        &[type_normal],
    )
    .unwrap();
    conn.change_window_attributes(
        root,
        &ChangeWindowAttributesAux::new().event_mask(EventMask::PROPERTY_CHANGE),
    )
    .unwrap();
    let gc = conn.generate_id().unwrap();
    conn.create_gc(gc, window, &CreateGCAux::new().foreground(COLOUR))
        .unwrap();
    conn.map_window(window).unwrap();
    conn.flush().unwrap();

    let mut asked = !ask;
    let mut active = None;
    loop {
        let event = conn.wait_for_event().expect("X connection lost");
        match event {
            Event::PropertyNotify(e) if e.window == window && e.atom == wm_state && !asked => {
                let state = conn
                    .get_property(false, window, wm_state, wm_state, 0, 2)
                    .unwrap()
                    .reply()
                    .unwrap();
                if state.value32().and_then(|mut v| v.next()) == Some(NORMAL_STATE) {
                    let request =
                        ClientMessageEvent::new(32, window, net_active_window, [2, 0, 0, 0, 0]);
                    conn.send_event(
                        false,
                        root,
                        EventMask::SUBSTRUCTURE_REDIRECT | EventMask::SUBSTRUCTURE_NOTIFY,
                        request,
                    )
                    .unwrap();
                    asked = true;
                    println!("asked");
                    draw(&conn, window, gc);
                }
            }
            Event::Expose(e) if e.window == window && asked => draw(&conn, window, gc),
            Event::PropertyNotify(e) if e.window == root && e.atom == net_active_window => {
                let now = conn
                    .get_property(false, root, net_active_window, AtomEnum::WINDOW, 0, 1)
                    .unwrap()
                    .reply()
                    .unwrap()
                    .value32()
                    .and_then(|mut v| v.next())
                    == Some(window);
                if active != Some(now) {
                    active = Some(now);
                    println!("active {now}");
                }
            }
            Event::FocusIn(e) if e.event == window && e.detail != NotifyDetail::POINTER => {
                println!("focus in");
            }
            Event::FocusOut(e) if e.event == window && e.detail != NotifyDetail::POINTER => {
                println!("focus out");
            }
            _ => {}
        }
    }
}

fn draw(conn: &RustConnection, window: u32, gc: u32) {
    let all = Rectangle {
        x: 0,
        y: 0,
        width: 480,
        height: 320,
    };
    conn.poly_fill_rectangle(window, gc, &[all]).unwrap();
    conn.flush().unwrap();
}
