//! `capture-check show [--exclude] [--no-global]`: a panel-like layer bar, a window and a
//! tooltip-like popup on it, each one flat colour; prints `ready` once all three are on screen.
//! `--exclude` leaves the popup out of captures; `--no-global` acts as a client that never
//! heard of the protocol.
//!
//! `capture-check count SHOT`: how many pixels of each colour a capture (PPM) or the X screen
//! under the nested compositor (XWD) holds.

use std::{
    fs::File,
    io::{Read, Write},
    os::fd::AsFd,
    process::exit,
};

use wayland_client::{
    Connection, Dispatch, QueueHandle, delegate_noop,
    globals::{GlobalListContents, registry_queue_init},
    protocol::{
        wl_buffer, wl_callback, wl_compositor, wl_registry, wl_shm, wl_shm_pool, wl_surface,
    },
};
use wayland_protocols::xdg::shell::client::{
    xdg_popup, xdg_positioner, xdg_surface, xdg_toplevel, xdg_wm_base,
};
use wayland_protocols_wlr::layer_shell::v1::client::{zwlr_layer_shell_v1, zwlr_layer_surface_v1};

#[allow(
    dead_code,
    non_upper_case_globals,
    non_camel_case_types,
    unused_imports,
    clippy::single_component_path_imports
)]
mod exclusion {
    use wayland_client;
    use wayland_client::protocol::*;
    pub mod __interfaces {
        use wayland_client::protocol::__interfaces::*;
        wayland_scanner::generate_interfaces!(
            "../../resources/protocols/kora-capture-exclusion.xml"
        );
    }
    use self::__interfaces::*;
    wayland_scanner::generate_client_code!("../../resources/protocols/kora-capture-exclusion.xml");
}
use exclusion::{
    kora_capture_exclusion_manager_v1::KoraCaptureExclusionManagerV1,
    kora_surface_capture_exclusion_v1::KoraSurfaceCaptureExclusionV1,
};

const BAR: [u8; 3] = [0x20, 0xc0, 0x40];
const WINDOW: [u8; 3] = [0x20, 0x50, 0xe0];
const TOOLTIP: [u8; 3] = [0xe0, 0x20, 0x20];
const BAR_HEIGHT: u32 = 48;
const WINDOW_SIZE: (u32, u32) = (640, 400);
const TOOLTIP_SIZE: (u32, u32) = (240, 64);

#[derive(Clone, Copy)]
enum Role {
    Window,
    Tooltip,
}

struct App {
    qh: QueueHandle<App>,
    compositor: wl_compositor::WlCompositor,
    shm: wl_shm::WlShm,
    wm_base: xdg_wm_base::XdgWmBase,
    manager: Option<KoraCaptureExclusionManagerV1>,
    exclude: bool,
    bar: wl_surface::WlSurface,
    window: wl_surface::WlSurface,
    window_xdg: xdg_surface::XdgSurface,
    tooltip: Option<(wl_surface::WlSurface, xdg_popup::XdgPopup)>,
    drawn: (bool, bool, bool),
    // The pools' files stay open until the compositor has their fds.
    files: Vec<File>,
}

impl App {
    fn draw(&mut self, surface: &wl_surface::WlSurface, (w, h): (u32, u32), rgb: [u8; 3]) {
        let dir = std::env::var("XDG_RUNTIME_DIR").expect("XDG_RUNTIME_DIR");
        let path = format!(
            "{dir}/capture-check-{}-{}",
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
        let pixel = [rgb[2], rgb[1], rgb[0], 0xff];
        let row: Vec<u8> = pixel.iter().copied().cycle().take(w as usize * 4).collect();
        for _ in 0..h {
            file.write_all(&row).expect("shm write");
        }
        let size = (w * h * 4) as i32;
        let pool = self.shm.create_pool(file.as_fd(), size, &self.qh, ());
        let buffer = pool.create_buffer(
            0,
            w as i32,
            h as i32,
            w as i32 * 4,
            wl_shm::Format::Xrgb8888,
            &self.qh,
            (),
        );
        pool.destroy();
        self.files.push(file);
        surface.attach(Some(&buffer), 0, 0);
        surface.damage_buffer(0, 0, w as i32, h as i32);
        surface.commit();
    }

    fn open_tooltip(&mut self) {
        let surface = self.compositor.create_surface(&self.qh, ());
        if self.exclude {
            match &self.manager {
                Some(manager) => {
                    let exclusion = manager.get_surface_exclusion(&surface, &self.qh, ());
                    exclusion.set_excluded(1);
                }
                None => eprintln!("no capture exclusion global: the tooltip stays in captures"),
            }
        }
        let positioner = self.wm_base.create_positioner(&self.qh, ());
        positioner.set_size(TOOLTIP_SIZE.0 as i32, TOOLTIP_SIZE.1 as i32);
        positioner.set_anchor_rect(40, 40, 1, 1);
        positioner.set_anchor(xdg_positioner::Anchor::TopLeft);
        positioner.set_gravity(xdg_positioner::Gravity::BottomRight);
        let xdg = self
            .wm_base
            .get_xdg_surface(&surface, &self.qh, Role::Tooltip);
        let popup = xdg.get_popup(Some(&self.window_xdg), &positioner, &self.qh, ());
        positioner.destroy();
        surface.commit();
        self.tooltip = Some((surface, popup));
    }
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    match args.first().map(String::as_str) {
        Some("show") => show(
            args.iter().any(|a| a == "--exclude"),
            args.iter().any(|a| a == "--no-global"),
        ),
        Some("count") if args.len() == 2 => count(&args[1]),
        _ => {
            eprintln!("usage: capture-check show [--exclude] [--no-global] | count SHOT");
            exit(2);
        }
    }
}

fn show(exclude: bool, no_global: bool) {
    let conn = Connection::connect_to_env().expect("no Wayland display");
    let (globals, mut queue) = registry_queue_init::<App>(&conn).expect("registry");
    let qh = queue.handle();
    let compositor: wl_compositor::WlCompositor = globals.bind(&qh, 4..=6, ()).expect("compositor");
    let shm = globals.bind(&qh, 1..=1, ()).expect("wl_shm");
    let wm_base: xdg_wm_base::XdgWmBase = globals.bind(&qh, 2..=6, ()).expect("xdg_wm_base");
    let layer_shell: zwlr_layer_shell_v1::ZwlrLayerShellV1 =
        globals.bind(&qh, 3..=4, ()).expect("layer shell");
    let manager = if no_global {
        None
    } else {
        globals.bind(&qh, 1..=1, ()).ok()
    };

    let bar = compositor.create_surface(&qh, ());
    let layer = layer_shell.get_layer_surface(
        &bar,
        None,
        zwlr_layer_shell_v1::Layer::Top,
        "capture-check-bar".into(),
        &qh,
        (),
    );
    layer.set_anchor(
        zwlr_layer_surface_v1::Anchor::Top
            | zwlr_layer_surface_v1::Anchor::Left
            | zwlr_layer_surface_v1::Anchor::Right,
    );
    layer.set_size(0, BAR_HEIGHT);
    layer.set_exclusive_zone(BAR_HEIGHT as i32);
    bar.commit();

    let window = compositor.create_surface(&qh, ());
    let window_xdg = wm_base.get_xdg_surface(&window, &qh, Role::Window);
    let toplevel = window_xdg.get_toplevel(&qh, ());
    toplevel.set_title("capture-check".into());
    toplevel.set_app_id("capture-check".into());
    window.commit();

    let mut app = App {
        qh,
        compositor,
        shm,
        wm_base,
        manager,
        exclude,
        bar,
        window,
        window_xdg,
        tooltip: None,
        drawn: (false, false, false),
        files: Vec::new(),
    };
    loop {
        queue.blocking_dispatch(&mut app).expect("dispatch");
    }
}

/// Pixels of the bar's, the window's and the tooltip's colour in a binary PPM (a capture) or a
/// 32-bit ZPixmap XWD (what the X server shows, so what is on the nested screen).
fn count(path: &str) {
    let mut data = Vec::new();
    File::open(path)
        .and_then(|mut f| f.read_to_end(&mut data))
        .expect("read the shot");
    let pixels: Vec<[u8; 3]> = if data.starts_with(b"P6") {
        ppm(&data)
    } else {
        xwd(&data)
    };
    let n = |rgb: [u8; 3]| pixels.iter().filter(|p| **p == rgb).count();
    println!("bar={} window={} tooltip={}", n(BAR), n(WINDOW), n(TOOLTIP));
}

fn ppm(data: &[u8]) -> Vec<[u8; 3]> {
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
    data[at + 1..]
        .chunks_exact(3)
        .map(|p| [p[0], p[1], p[2]])
        .collect()
}

fn xwd(data: &[u8]) -> Vec<[u8; 3]> {
    let field = |i: usize| u32::from_be_bytes(data[i * 4..i * 4 + 4].try_into().unwrap()) as usize;
    let (header, format, width, height) = (field(0), field(2), field(4), field(5));
    let (lsb_first, bits, stride, colors) = (field(7) == 0, field(11), field(12), field(19));
    assert!(format == 2 && bits == 32, "not a 32-bit ZPixmap XWD");
    let start = header + colors * 12;
    let mut pixels = Vec::with_capacity(width * height);
    for y in 0..height {
        for x in 0..width {
            let p = &data[start + y * stride + x * 4..][..4];
            // Masks are 0xff0000, 0xff00 and 0xff for a 24-bit visual.
            pixels.push(if lsb_first {
                [p[2], p[1], p[0]]
            } else {
                [p[1], p[2], p[3]]
            });
        }
    }
    pixels
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

impl Dispatch<zwlr_layer_surface_v1::ZwlrLayerSurfaceV1, ()> for App {
    fn event(
        app: &mut Self,
        layer: &zwlr_layer_surface_v1::ZwlrLayerSurfaceV1,
        event: zwlr_layer_surface_v1::Event,
        _: &(),
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
        match event {
            zwlr_layer_surface_v1::Event::Configure { serial, width, .. } => {
                layer.ack_configure(serial);
                if !app.drawn.0 {
                    app.drawn.0 = true;
                    let bar = app.bar.clone();
                    app.draw(&bar, (width.max(1), BAR_HEIGHT), BAR);
                } else {
                    app.bar.commit();
                }
            }
            zwlr_layer_surface_v1::Event::Closed => exit(0),
            _ => {}
        }
    }
}

impl Dispatch<xdg_surface::XdgSurface, Role> for App {
    fn event(
        app: &mut Self,
        xdg: &xdg_surface::XdgSurface,
        event: xdg_surface::Event,
        role: &Role,
        _: &Connection,
        qh: &QueueHandle<Self>,
    ) {
        let xdg_surface::Event::Configure { serial } = event else {
            return;
        };
        xdg.ack_configure(serial);
        match role {
            Role::Window if !app.drawn.1 => {
                app.drawn.1 = true;
                let window = app.window.clone();
                app.draw(&window, WINDOW_SIZE, WINDOW);
                app.open_tooltip();
            }
            Role::Window => app.window.commit(),
            Role::Tooltip => {
                let Some((surface, _)) = app.tooltip.clone() else {
                    return;
                };
                if app.drawn.2 {
                    surface.commit();
                    return;
                }
                app.drawn.2 = true;
                // Ready once the compositor has drawn the tooltip's first frame.
                surface.frame(qh, ());
                app.draw(&surface, TOOLTIP_SIZE, TOOLTIP);
            }
        }
    }
}

impl Dispatch<wl_callback::WlCallback, ()> for App {
    fn event(
        _: &mut Self,
        _: &wl_callback::WlCallback,
        event: wl_callback::Event,
        _: &(),
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
        if let wl_callback::Event::Done { .. } = event {
            println!("ready");
            std::io::stdout().flush().ok();
        }
    }
}

impl Dispatch<xdg_toplevel::XdgToplevel, ()> for App {
    fn event(
        _: &mut Self,
        _: &xdg_toplevel::XdgToplevel,
        event: xdg_toplevel::Event,
        _: &(),
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
        if let xdg_toplevel::Event::Close = event {
            exit(0);
        }
    }
}

impl Dispatch<xdg_popup::XdgPopup, ()> for App {
    fn event(
        _: &mut Self,
        _: &xdg_popup::XdgPopup,
        event: xdg_popup::Event,
        _: &(),
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
        if let xdg_popup::Event::PopupDone = event {
            eprintln!("the compositor dismissed the tooltip");
            exit(1);
        }
    }
}

delegate_noop!(App: ignore wl_compositor::WlCompositor);
delegate_noop!(App: ignore wl_shm::WlShm);
delegate_noop!(App: ignore wl_shm_pool::WlShmPool);
delegate_noop!(App: ignore wl_buffer::WlBuffer);
delegate_noop!(App: ignore wl_surface::WlSurface);
delegate_noop!(App: ignore xdg_positioner::XdgPositioner);
delegate_noop!(App: ignore zwlr_layer_shell_v1::ZwlrLayerShellV1);
delegate_noop!(App: ignore KoraCaptureExclusionManagerV1);
delegate_noop!(App: ignore KoraSurfaceCaptureExclusionV1);
