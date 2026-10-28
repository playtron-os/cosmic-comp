//! `focus-check window`: a flat-colour window that prints `activated true|false` when its
//! state changes, and `keyboard enter|leave` and `key CODE` as its keyboard focus does.
//!
//! `focus-check overlay [unmap|hide]`: a clear overlay-layer surface on every output that takes
//! the keyboard, as a capture overlay does; prints the same keyboard lines plus `keyboard on N`
//! (the surface that got it). On Esc it takes its surfaces down one at a time, the one with
//! the keyboard first, and prints `exit`. With `unmap` it attaches no buffer instead, and with
//! `hide` it hides them and gives up the keyboard as the launcher does; either way it then
//! prints `unmapped` or `hidden` and stays connected.
//!
//! `focus-check popup`: a bar at the bottom of the output that never takes the keyboard, as
//! the panel is, printing `bar up` once drawn. A click on it opens a popup that grabs the
//! keyboard; Esc destroys the popup and prints `closed`, and the bar stays.
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
        wl_buffer, wl_compositor, wl_keyboard, wl_output, wl_pointer, wl_registry, wl_seat, wl_shm,
        wl_shm_pool, wl_surface,
    },
};
use wayland_protocols::xdg::shell::client::{
    xdg_popup, xdg_positioner, xdg_surface, xdg_toplevel, xdg_wm_base,
};
use wayland_protocols_wlr::layer_shell::v1::client::{zwlr_layer_shell_v1, zwlr_layer_surface_v1};

/// The compositor's layer surface visibility protocol, from its own XML.
#[allow(non_upper_case_globals, non_camel_case_types, dead_code, clippy::all)]
mod visibility {
    use wayland_client::{self, protocol::*};

    pub mod __interfaces {
        use wayland_client::{backend as wayland_backend, protocol::__interfaces::*};
        wayland_scanner::generate_interfaces!(
            "../../resources/protocols/layer_surface_visibility.xml"
        );
    }
    use self::__interfaces::*;

    wayland_scanner::generate_client_code!(
        "../../resources/protocols/layer_surface_visibility.xml"
    );
}
use visibility::{
    zcosmic_layer_surface_visibility_manager_v1 as visibility_manager,
    zcosmic_layer_surface_visibility_v1 as visibility_controller,
};

const WINDOW: [u8; 4] = [0x20, 0x50, 0xe0, 0xff];
const CLEAR: [u8; 4] = [0, 0, 0, 0];
const BAR: [u8; 4] = [0x30, 0x30, 0x34, 0xff];
const MENU: [u8; 4] = [0xe0, 0xe0, 0xe8, 0xff];
const WINDOW_SIZE: (u32, u32) = (640, 400);
const BAR_HEIGHT: u32 = 40;
const MENU_SIZE: (u32, u32) = (200, 120);
const ESC: u32 = 1;

#[derive(Clone, Copy, PartialEq)]
enum Mode {
    Window,
    Overlay(Close),
    Bar,
}

/// How the overlay goes away on Esc.
#[derive(Clone, Copy, PartialEq)]
enum Close {
    Destroy,
    Unmap,
    Hide,
}

/// The bar's popup: its surface and roles, destroyed together on Esc.
struct Menu {
    surface: wl_surface::WlSurface,
    xdg: xdg_surface::XdgSurface,
    popup: xdg_popup::XdgPopup,
    drawn: bool,
}

/// User data that tells the popup's `xdg_surface` from the window's.
struct MenuSurface;

struct App {
    qh: QueueHandle<App>,
    shm: wl_shm::WlShm,
    surfaces: Vec<wl_surface::WlSurface>,
    // The layer surfaces of `surfaces`, by index; destroyed before them, as a client does.
    layers: Vec<zwlr_layer_surface_v1::ZwlrLayerSurfaceV1>,
    mode: Mode,
    activated: Option<bool>,
    pending_activated: bool,
    // The overlay surface holding the keyboard, closed first.
    keyboard_on: Option<usize>,
    done: bool,
    seat: wl_seat::WlSeat,
    wm_base: xdg_wm_base::XdgWmBase,
    compositor: wl_compositor::WlCompositor,
    bar: Option<(zwlr_layer_surface_v1::ZwlrLayerSurfaceV1, u32)>,
    menu: Option<Menu>,
    close_menu: bool,
    visibility: Option<visibility_manager::ZcosmicLayerSurfaceVisibilityManagerV1>,
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

    /// Opens the bar's popup above it, grabbing the keyboard with the click's serial.
    fn open_menu(&mut self, serial: u32) {
        let Some((bar, width)) = &self.bar else {
            return;
        };
        let (wm_base, compositor, seat) = (&self.wm_base, &self.compositor, &self.seat);
        let positioner = wm_base.create_positioner(&self.qh, ());
        positioner.set_size(MENU_SIZE.0 as i32, MENU_SIZE.1 as i32);
        positioner.set_anchor_rect(0, 0, *width as i32, 1);
        positioner.set_anchor(xdg_positioner::Anchor::TopLeft);
        positioner.set_gravity(xdg_positioner::Gravity::TopRight);
        let surface = compositor.create_surface(&self.qh, ());
        let xdg = wm_base.get_xdg_surface(&surface, &self.qh, MenuSurface);
        let popup = xdg.get_popup(None, &positioner, &self.qh, ());
        bar.get_popup(&popup);
        popup.grab(seat, serial);
        surface.commit();
        positioner.destroy();
        self.menu = Some(Menu {
            surface,
            xdg,
            popup,
            drawn: false,
        });
    }
}

fn say(line: &str) {
    println!("{line}");
    std::io::stdout().flush().ok();
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    match args.iter().map(String::as_str).collect::<Vec<_>>()[..] {
        ["window"] => show(Mode::Window),
        ["overlay"] => show(Mode::Overlay(Close::Destroy)),
        ["overlay", "unmap"] => show(Mode::Overlay(Close::Unmap)),
        ["overlay", "hide"] => show(Mode::Overlay(Close::Hide)),
        ["popup"] => show(Mode::Bar),
        ["diff", a, b, width] => diff(a, b, width),
        _ => {
            eprintln!("usage: focus-check window | overlay [unmap|hide] | popup | diff A B WIDTH");
            exit(2);
        }
    }
}

fn show(mode: Mode) {
    let conn = Connection::connect_to_env().expect("no Wayland display");
    let (globals, mut queue) = registry_queue_init::<App>(&conn).expect("registry");
    let qh = queue.handle();
    let compositor: wl_compositor::WlCompositor = globals.bind(&qh, 4..=6, ()).expect("compositor");
    let shm = globals.bind(&qh, 1..=1, ()).expect("wl_shm");
    let seat: wl_seat::WlSeat = globals.bind(&qh, 5..=9, ()).expect("wl_seat");
    seat.get_keyboard(&qh, ());
    let wm_base: xdg_wm_base::XdgWmBase = globals.bind(&qh, 2..=6, ()).expect("xdg_wm_base");

    let mut surfaces = Vec::new();
    let mut layers = Vec::new();
    let mut bar = None;
    match mode {
        Mode::Overlay(_) => {
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
                layers.push(layer);
            }
        }
        Mode::Bar => {
            let layer_shell: zwlr_layer_shell_v1::ZwlrLayerShellV1 =
                globals.bind(&qh, 3..=4, ()).expect("layer shell");
            seat.get_pointer(&qh, ());
            let surface = compositor.create_surface(&qh, ());
            // On the output under the pointer, as the compositor picks one.
            let layer = layer_shell.get_layer_surface(
                &surface,
                None,
                zwlr_layer_shell_v1::Layer::Top,
                "focus-check-bar".into(),
                &qh,
                0,
            );
            layer.set_anchor(
                zwlr_layer_surface_v1::Anchor::Bottom
                    | zwlr_layer_surface_v1::Anchor::Left
                    | zwlr_layer_surface_v1::Anchor::Right,
            );
            layer.set_size(0, BAR_HEIGHT);
            layer.set_keyboard_interactivity(zwlr_layer_surface_v1::KeyboardInteractivity::None);
            surface.commit();
            surfaces.push(surface);
            layers.push(layer.clone());
            bar = Some((layer, 0));
        }
        Mode::Window => {
            let surface = compositor.create_surface(&qh, ());
            let toplevel = wm_base
                .get_xdg_surface(&surface, &qh, ())
                .get_toplevel(&qh, ());
            toplevel.set_title("focus-check".into());
            toplevel.set_app_id("focus-check".into());
            surface.commit();
            surfaces.push(surface);
        }
    }

    let visibility = globals.bind(&qh, 1..=4, ()).ok();
    let mut app = App {
        qh,
        shm,
        surfaces,
        layers,
        mode,
        activated: None,
        pending_activated: false,
        keyboard_on: None,
        done: false,
        seat,
        wm_base,
        compositor,
        bar,
        menu: None,
        close_menu: false,
        visibility,
        files: Vec::new(),
    };
    while !app.done {
        queue.blocking_dispatch(&mut app).expect("dispatch");
        if app.close_menu {
            app.close_menu = false;
            if let Some(menu) = app.menu.take() {
                menu.popup.destroy();
                menu.xdg.destroy();
                menu.surface.destroy();
                conn.roundtrip().expect("roundtrip");
                say("closed");
            }
        }
    }

    // One at a time, the surface with the keyboard first: the compositor must not lose the
    // keyboard while another of them is still up.
    let mut order: Vec<usize> = (0..app.surfaces.len()).collect();
    if let Some(focused) = app.keyboard_on {
        order.retain(|&i| i != focused);
        order.insert(0, focused);
    }
    let close = match app.mode {
        Mode::Overlay(close) => close,
        _ => Close::Destroy,
    };
    for i in order {
        let surface = &app.surfaces[i];
        match close {
            Close::Destroy => {
                if let Some(layer) = app.layers.get(i) {
                    layer.destroy();
                }
                surface.destroy();
            }
            Close::Unmap => {
                surface.attach(None, 0, 0);
                surface.commit();
            }
            // The hide comes first and the surface still asks for the keyboard in a commit
            // after it, before it lets go, as a client's separate requests can land.
            Close::Hide => {
                let manager = app.visibility.as_ref().expect("layer surface visibility");
                manager
                    .get_visibility_controller(surface, &app.qh, ())
                    .set_hidden();
                conn.roundtrip().expect("roundtrip");
                surface.commit();
                conn.roundtrip().expect("roundtrip");
                app.layers[i]
                    .set_keyboard_interactivity(zwlr_layer_surface_v1::KeyboardInteractivity::None);
                surface.commit();
            }
        }
        conn.roundtrip().expect("roundtrip");
    }
    if close == Close::Destroy {
        say("exit");
        return;
    }
    say(if close == Close::Unmap {
        "unmapped"
    } else {
        "hidden"
    });
    loop {
        queue.blocking_dispatch(&mut app).expect("dispatch");
    }
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
            wl_keyboard::Event::Enter { surface, .. } => {
                say("keyboard enter");
                if let Some(i) = app.surfaces.iter().position(|s| s == &surface) {
                    app.keyboard_on = Some(i);
                    say(&format!("keyboard on {i}"));
                }
            }
            wl_keyboard::Event::Leave { .. } => say("keyboard leave"),
            wl_keyboard::Event::Key {
                key,
                state: WEnum::Value(wl_keyboard::KeyState::Released),
                ..
            } => {
                say(&format!("key {key}"));
                if key == ESC {
                    match app.mode {
                        Mode::Overlay(_) => app.done = true,
                        Mode::Bar => app.close_menu = app.menu.is_some(),
                        Mode::Window => {}
                    }
                }
            }
            _ => {}
        }
    }
}

impl Dispatch<wl_pointer::WlPointer, ()> for App {
    fn event(
        app: &mut Self,
        _: &wl_pointer::WlPointer,
        event: wl_pointer::Event,
        _: &(),
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
        if let wl_pointer::Event::Button {
            serial,
            state: WEnum::Value(state),
            ..
        } = event
            && state == wl_pointer::ButtonState::Pressed
            && app.menu.is_none()
        {
            app.open_menu(serial);
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

impl Dispatch<xdg_surface::XdgSurface, MenuSurface> for App {
    fn event(
        app: &mut Self,
        xdg: &xdg_surface::XdgSurface,
        event: xdg_surface::Event,
        _: &MenuSurface,
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
        let xdg_surface::Event::Configure { serial } = event else {
            return;
        };
        xdg.ack_configure(serial);
        let Some(menu) = app.menu.as_mut().filter(|m| &m.xdg == xdg) else {
            return;
        };
        let surface = menu.surface.clone();
        if menu.drawn {
            surface.commit();
        } else {
            menu.drawn = true;
            app.draw(&surface, MENU_SIZE, MENU);
        }
    }
}

impl Dispatch<xdg_popup::XdgPopup, ()> for App {
    fn event(
        app: &mut Self,
        popup: &xdg_popup::XdgPopup,
        event: xdg_popup::Event,
        _: &(),
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
        if let xdg_popup::Event::PopupDone = event
            && app.menu.as_ref().is_some_and(|m| &m.popup == popup)
        {
            say("popup done");
            app.close_menu = true;
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
                let size = (width.max(1), height.max(1));
                if let Some((_, bar_width)) = app.bar.as_mut() {
                    *bar_width = size.0;
                    app.draw(&surface, size, BAR);
                    say("bar up");
                } else {
                    app.draw(&surface, size, CLEAR);
                }
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
delegate_noop!(App: ignore xdg_positioner::XdgPositioner);
delegate_noop!(App: ignore visibility_manager::ZcosmicLayerSurfaceVisibilityManagerV1);
delegate_noop!(App: ignore visibility_controller::ZcosmicLayerSurfaceVisibilityV1);
