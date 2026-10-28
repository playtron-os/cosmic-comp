//! `focus-check window`: a flat-colour window that prints `activated true|false` when its
//! state changes, and `keyboard enter|leave` and `key CODE` as its keyboard focus does.
//!
//! `focus-check overlay`: a clear overlay-layer surface on every output that takes the
//! keyboard, as a capture overlay does; prints the same keyboard lines and leaves on Esc.
//!
//! `focus-check diff A B WIDTH`: how many pixels differ between two captures (PPM) in their
//! leftmost WIDTH columns, the first output.

use std::{
    fs::File,
    io::{Read, Write},
    os::fd::AsFd,
    process::exit,
};

use wayland_client::{
    Connection, Dispatch, QueueHandle, WEnum, delegate_noop,
    globals::{GlobalListContents, registry_queue_init},
    protocol::{
        wl_buffer, wl_compositor, wl_keyboard, wl_output, wl_registry, wl_seat, wl_shm,
        wl_shm_pool, wl_surface,
    },
};
use wayland_protocols::xdg::shell::client::{xdg_surface, xdg_toplevel, xdg_wm_base};
use wayland_protocols_wlr::layer_shell::v1::client::{zwlr_layer_shell_v1, zwlr_layer_surface_v1};

const WINDOW: [u8; 4] = [0x20, 0x50, 0xe0, 0xff];
const CLEAR: [u8; 4] = [0, 0, 0, 0];
const WINDOW_SIZE: (u32, u32) = (640, 400);
const ESC: u32 = 1;

struct App {
    qh: QueueHandle<App>,
    shm: wl_shm::WlShm,
    surfaces: Vec<wl_surface::WlSurface>,
    overlay: bool,
    activated: Option<bool>,
    pending_activated: bool,
    done: bool,
    // The pools' files stay open until the compositor has their fds.
    files: Vec<File>,
}

impl App {
    fn draw(&mut self, surface: &wl_surface::WlSurface, (w, h): (u32, u32), rgba: [u8; 4]) {
        let dir = std::env::var("XDG_RUNTIME_DIR").expect("XDG_RUNTIME_DIR");
        let path = format!(
            "{dir}/focus-check-{}-{}",
            std::process::id(),
            self.files.len()
        );
        let mut file = File::options()
            .read(true)
            .write(true)
            .create_new(true)
            .open(&path)
            .expect("shm file");
        std::fs::remove_file(&path).expect("unlink shm file");
        let pixel = [rgba[2], rgba[1], rgba[0], rgba[3]];
        let row: Vec<u8> = pixel.iter().copied().cycle().take(w as usize * 4).collect();
        for _ in 0..h {
            file.write_all(&row).expect("shm write");
        }
        let pool = self
            .shm
            .create_pool(file.as_fd(), (w * h * 4) as i32, &self.qh, ());
        let buffer = pool.create_buffer(
            0,
            w as i32,
            h as i32,
            w as i32 * 4,
            wl_shm::Format::Argb8888,
            &self.qh,
            (),
        );
        pool.destroy();
        self.files.push(file);
        surface.attach(Some(&buffer), 0, 0);
        surface.damage_buffer(0, 0, w as i32, h as i32);
        surface.commit();
    }
}

fn say(line: &str) {
    println!("{line}");
    std::io::stdout().flush().ok();
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    match args.first().map(String::as_str) {
        Some("window") => show(false),
        Some("overlay") => show(true),
        Some("diff") if args.len() == 4 => diff(&args[1], &args[2], &args[3]),
        _ => {
            eprintln!("usage: focus-check window | overlay | diff A B WIDTH");
            exit(2);
        }
    }
}

fn show(overlay: bool) {
    let conn = Connection::connect_to_env().expect("no Wayland display");
    let (globals, mut queue) = registry_queue_init::<App>(&conn).expect("registry");
    let qh = queue.handle();
    let compositor: wl_compositor::WlCompositor = globals.bind(&qh, 4..=6, ()).expect("compositor");
    let shm = globals.bind(&qh, 1..=1, ()).expect("wl_shm");
    let seat: wl_seat::WlSeat = globals.bind(&qh, 5..=9, ()).expect("wl_seat");
    seat.get_keyboard(&qh, ());

    let mut surfaces = Vec::new();
    if overlay {
        let layer_shell: zwlr_layer_shell_v1::ZwlrLayerShellV1 =
            globals.bind(&qh, 3..=4, ()).expect("layer shell");
        let outputs: Vec<wl_output::WlOutput> = globals.contents().with_list(|list| {
            list.iter()
                .filter(|global| global.interface == "wl_output")
                .map(|global| {
                    globals
                        .registry()
                        .bind(global.name, global.version.min(4), &qh, ())
                })
                .collect()
        });
        for output in &outputs {
            let surface = compositor.create_surface(&qh, ());
            let layer = layer_shell.get_layer_surface(
                &surface,
                Some(output),
                zwlr_layer_shell_v1::Layer::Overlay,
                "focus-check".into(),
                &qh,
                surfaces.len(),
            );
            layer.set_anchor(
                zwlr_layer_surface_v1::Anchor::Top
                    | zwlr_layer_surface_v1::Anchor::Bottom
                    | zwlr_layer_surface_v1::Anchor::Left
                    | zwlr_layer_surface_v1::Anchor::Right,
            );
            layer.set_exclusive_zone(-1);
            layer.set_keyboard_interactivity(
                zwlr_layer_surface_v1::KeyboardInteractivity::Exclusive,
            );
            surface.commit();
            surfaces.push(surface);
        }
    } else {
        let wm_base: xdg_wm_base::XdgWmBase = globals.bind(&qh, 2..=6, ()).expect("xdg_wm_base");
        let surface = compositor.create_surface(&qh, ());
        let toplevel = wm_base
            .get_xdg_surface(&surface, &qh, ())
            .get_toplevel(&qh, ());
        toplevel.set_title("focus-check".into());
        toplevel.set_app_id("focus-check".into());
        surface.commit();
        surfaces.push(surface);
    }

    let mut app = App {
        qh,
        shm,
        surfaces,
        overlay,
        activated: None,
        pending_activated: false,
        done: false,
        files: Vec::new(),
    };
    while !app.done {
        queue.blocking_dispatch(&mut app).expect("dispatch");
    }
    for surface in app.surfaces.drain(..) {
        surface.destroy();
    }
    conn.roundtrip().ok();
    say("exit");
}

/// Pixels that differ between two binary PPMs in their leftmost `width` columns.
fn diff(a: &str, b: &str, width: &str) {
    let width: usize = width.parse().expect("WIDTH");
    let (wa, pa) = ppm(a);
    let (wb, pb) = ppm(b);
    assert!(wa == wb && pa.len() == pb.len(), "captures differ in size");
    let changed = pa
        .iter()
        .zip(&pb)
        .enumerate()
        .filter(|(i, (x, y))| i % wa < width && x != y)
        .count();
    println!("{changed}");
}

fn ppm(path: &str) -> (usize, Vec<[u8; 3]>) {
    let mut data = Vec::new();
    File::open(path)
        .and_then(|mut f| f.read_to_end(&mut data))
        .expect("read the shot");
    assert!(data.starts_with(b"P6"), "not a binary PPM");
    let mut fields = Vec::new();
    let mut at = 0;
    while fields.len() < 4 {
        while data[at].is_ascii_whitespace() {
            at += 1;
        }
        if data[at] == b'#' {
            while data[at] != b'\n' {
                at += 1;
            }
            continue;
        }
        let start = at;
        while !data[at].is_ascii_whitespace() {
            at += 1;
        }
        fields.push(String::from_utf8_lossy(&data[start..at]).into_owned());
    }
    assert!(fields[3] == "255", "not an 8-bit PPM");
    let width = fields[1].parse().expect("PPM width");
    let pixels = data[at + 1..]
        .chunks_exact(3)
        .map(|p| [p[0], p[1], p[2]])
        .collect();
    (width, pixels)
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

impl Dispatch<wl_keyboard::WlKeyboard, ()> for App {
    fn event(
        app: &mut Self,
        _: &wl_keyboard::WlKeyboard,
        event: wl_keyboard::Event,
        _: &(),
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
        match event {
            wl_keyboard::Event::Enter { .. } => say("keyboard enter"),
            wl_keyboard::Event::Leave { .. } => say("keyboard leave"),
            wl_keyboard::Event::Key {
                key,
                state: WEnum::Value(wl_keyboard::KeyState::Released),
                ..
            } => {
                say(&format!("key {key}"));
                if app.overlay && key == ESC {
                    app.done = true;
                }
            }
            _ => {}
        }
    }
}

impl Dispatch<xdg_wm_base::XdgWmBase, ()> for App {
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

impl Dispatch<xdg_toplevel::XdgToplevel, ()> for App {
    fn event(
        app: &mut Self,
        _: &xdg_toplevel::XdgToplevel,
        event: xdg_toplevel::Event,
        _: &(),
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
        match event {
            xdg_toplevel::Event::Configure { states, .. } => {
                app.pending_activated = states
                    .chunks_exact(4)
                    .map(|c| u32::from_ne_bytes([c[0], c[1], c[2], c[3]]))
                    .any(|s| s == xdg_toplevel::State::Activated as u32);
            }
            xdg_toplevel::Event::Close => app.done = true,
            _ => {}
        }
    }
}

impl Dispatch<xdg_surface::XdgSurface, ()> for App {
    fn event(
        app: &mut Self,
        xdg: &xdg_surface::XdgSurface,
        event: xdg_surface::Event,
        _: &(),
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
        let xdg_surface::Event::Configure { serial } = event else {
            return;
        };
        xdg.ack_configure(serial);
        if app.activated != Some(app.pending_activated) {
            app.activated = Some(app.pending_activated);
            say(&format!("activated {}", app.pending_activated));
        }
        let window = app.surfaces[0].clone();
        if app.files.is_empty() {
            app.draw(&window, WINDOW_SIZE, WINDOW);
        } else {
            window.commit();
        }
    }
}

impl Dispatch<zwlr_layer_surface_v1::ZwlrLayerSurfaceV1, usize> for App {
    fn event(
        app: &mut Self,
        layer: &zwlr_layer_surface_v1::ZwlrLayerSurfaceV1,
        event: zwlr_layer_surface_v1::Event,
        index: &usize,
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
        match event {
            zwlr_layer_surface_v1::Event::Configure {
                serial,
                width,
                height,
            } => {
                layer.ack_configure(serial);
                let surface = app.surfaces[*index].clone();
                app.draw(&surface, (width.max(1), height.max(1)), CLEAR);
            }
            zwlr_layer_surface_v1::Event::Closed => app.done = true,
            _ => {}
        }
    }
}

delegate_noop!(App: ignore wl_compositor::WlCompositor);
delegate_noop!(App: ignore wl_shm::WlShm);
delegate_noop!(App: ignore wl_shm_pool::WlShmPool);
delegate_noop!(App: ignore wl_buffer::WlBuffer);
delegate_noop!(App: ignore wl_surface::WlSurface);
delegate_noop!(App: ignore wl_seat::WlSeat);
delegate_noop!(App: ignore wl_output::WlOutput);
delegate_noop!(App: ignore zwlr_layer_shell_v1::ZwlrLayerShellV1);
