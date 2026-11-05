//! `modal-check parent`: a large window that exports itself with xdg-foreign and prints
//! `handle H`. It prints `keyboard enter|leave`, `button`, `axis` and `touch` as its input
//! arrives, so a test can tell which window a click, a scroll or a touch reached.
//!
//! `modal-check dialog H`: a small window that imports H, makes itself H's child and asks
//! to be modal (xdg-dialog), as a file picker from another app does. Prints the same lines.
//!
//! `modal-check window`: a plain small window, to move the keyboard elsewhere.
//!
//! `modal-check count FILE RRGGBB`: how many pixels of a capture (PPM) have that colour.
//!
//! Each prints `mapped` once drawn, and `exit` when its window is closed. A dialog whose
//! parent goes away prints `parent destroyed` and exits, as a picker closes with its app.

use std::{
    fs::File,
    io::Write,
    os::fd::AsFd,
    process::exit,
};

use wayland_client::{
    Connection, Dispatch, QueueHandle, delegate_noop,
    globals::{GlobalListContents, registry_queue_init},
    protocol::{
        wl_buffer, wl_compositor, wl_keyboard, wl_pointer, wl_registry, wl_seat, wl_shm,
        wl_shm_pool, wl_surface, wl_touch,
    },
};
use wayland_protocols::xdg::{
    dialog::v1::client::{xdg_dialog_v1, xdg_wm_dialog_v1},
    foreign::zv2::client::{
        zxdg_exported_v2, zxdg_exporter_v2, zxdg_imported_v2, zxdg_importer_v2,
    },
    shell::client::{xdg_surface, xdg_toplevel, xdg_wm_base},
};

struct App {
    qh: QueueHandle<App>,
    shm: wl_shm::WlShm,
    surface: wl_surface::WlSurface,
    size: (u32, u32),
    rgba: [u8; 4],
    drawn: bool,
    done: bool,
    // The pool's file stays open until the compositor has its fd.
    files: Vec<File>,
}

impl App {
    fn draw(&mut self) {
        let (w, h) = self.size;
        let dir = std::env::var("XDG_RUNTIME_DIR").expect("XDG_RUNTIME_DIR");
        let path = format!("{dir}/modal-check-{}", std::process::id());
        let mut file = File::options()
            .read(true)
            .write(true)
            .create_new(true)
            .open(&path)
            .expect("shm file");
        std::fs::remove_file(&path).expect("unlink shm file");
        let [r, g, b, a] = self.rgba;
        let row: Vec<u8> = [b, g, r, a].iter().copied().cycle().take(w as usize * 4).collect();
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
        self.surface.attach(Some(&buffer), 0, 0);
        self.surface.damage_buffer(0, 0, w as i32, h as i32);
        self.surface.commit();
    }
}

fn say(line: &str) {
    println!("{line}");
    std::io::stdout().flush().ok();
}

enum Role {
    Parent,
    Dialog(String),
    Window,
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let role = match args.iter().map(String::as_str).collect::<Vec<_>>()[..] {
        ["parent"] => Role::Parent,
        ["dialog", handle] => Role::Dialog(handle.to_owned()),
        ["window"] => Role::Window,
        ["count", file, colour] => return count(file, colour),
        _ => {
            eprintln!("usage: modal-check parent | dialog HANDLE | window | count FILE RRGGBB");
            exit(2);
        }
    };

    let conn = Connection::connect_to_env().expect("no Wayland display");
    let (globals, mut queue) = registry_queue_init::<App>(&conn).expect("registry");
    let qh = queue.handle();
    let compositor: wl_compositor::WlCompositor = globals.bind(&qh, 4..=6, ()).expect("compositor");
    let shm = globals.bind(&qh, 1..=1, ()).expect("wl_shm");
    let seat: wl_seat::WlSeat = globals.bind(&qh, 5..=9, ()).expect("wl_seat");
    seat.get_keyboard(&qh, ());
    seat.get_pointer(&qh, ());
    seat.get_touch(&qh, ());
    let wm_base: xdg_wm_base::XdgWmBase = globals.bind(&qh, 2..=6, ()).expect("xdg_wm_base");

    let surface = compositor.create_surface(&qh, ());
    let xdg = wm_base.get_xdg_surface(&surface, &qh, ());
    let toplevel = xdg.get_toplevel(&qh, ());
    let (size, rgba, title) = match role {
        Role::Parent => ((1200, 760), [0x20, 0x50, 0xe0, 0xff], "Parent"),
        Role::Dialog(_) => ((480, 300), [0xe0, 0x90, 0x20, 0xff], "Dialog"),
        Role::Window => ((360, 240), [0x30, 0xa0, 0x50, 0xff], "Other"),
    };
    toplevel.set_title(title.into());
    toplevel.set_app_id(format!("modal-check-{}", title.to_lowercase()));

    let mut _keep = Vec::new();
    match &role {
        Role::Parent => {
            let exporter: zxdg_exporter_v2::ZxdgExporterV2 =
                globals.bind(&qh, 1..=1, ()).expect("zxdg_exporter_v2");
            exporter.export_toplevel(&surface, &qh, ());
        }
        Role::Dialog(handle) => {
            let importer: zxdg_importer_v2::ZxdgImporterV2 =
                globals.bind(&qh, 1..=1, ()).expect("zxdg_importer_v2");
            let imported = importer.import_toplevel(handle.clone(), &qh, ());
            imported.set_parent_of(&surface);
            let dialogs: xdg_wm_dialog_v1::XdgWmDialogV1 =
                globals.bind(&qh, 1..=1, ()).expect("xdg_wm_dialog_v1");
            let dialog = dialogs.get_xdg_dialog(&toplevel, &qh, ());
            dialog.set_modal();
            _keep.push(imported);
        }
        Role::Window => {}
    }
    surface.commit();

    let mut app = App {
        qh,
        shm,
        surface,
        size,
        rgba,
        drawn: false,
        done: false,
        files: Vec::new(),
    };
    while !app.done {
        queue.blocking_dispatch(&mut app).expect("dispatch");
    }
    say("exit");
}

fn count(file: &str, colour: &str) {
    let data = std::fs::read(file).expect("capture");
    // P6 <width> <height> <max>, each followed by one whitespace byte.
    let mut at = 0;
    for _ in 0..4 {
        while !data[at].is_ascii_whitespace() {
            at += 1;
        }
        at += 1;
    }
    let want: Vec<u8> = (0..3)
        .map(|i| u8::from_str_radix(&colour[i * 2..i * 2 + 2], 16).expect("RRGGBB"))
        .collect();
    let n = data[at..].chunks_exact(3).filter(|p| *p == want.as_slice()).count();
    say(&n.to_string());
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
        _: &mut Self,
        _: &wl_keyboard::WlKeyboard,
        event: wl_keyboard::Event,
        _: &(),
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
        match event {
            wl_keyboard::Event::Enter { .. } => say("keyboard enter"),
            wl_keyboard::Event::Leave { .. } => say("keyboard leave"),
            _ => {}
        }
    }
}

impl Dispatch<wl_pointer::WlPointer, ()> for App {
    fn event(
        _: &mut Self,
        _: &wl_pointer::WlPointer,
        event: wl_pointer::Event,
        _: &(),
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
        match event {
            wl_pointer::Event::Enter { .. } => say("pointer enter"),
            wl_pointer::Event::Leave { .. } => say("pointer leave"),
            wl_pointer::Event::Button { .. } => say("button"),
            wl_pointer::Event::Axis { .. } => say("axis"),
            _ => {}
        }
    }
}

impl Dispatch<wl_touch::WlTouch, ()> for App {
    fn event(
        _: &mut Self,
        _: &wl_touch::WlTouch,
        event: wl_touch::Event,
        _: &(),
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
        if let wl_touch::Event::Down { .. } = event {
            say("touch");
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
        if let xdg_toplevel::Event::Close = event {
            app.done = true;
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
        if app.drawn {
            app.surface.commit();
        } else {
            app.drawn = true;
            app.draw();
            say("mapped");
        }
    }
}

impl Dispatch<zxdg_exported_v2::ZxdgExportedV2, ()> for App {
    fn event(
        _: &mut Self,
        _: &zxdg_exported_v2::ZxdgExportedV2,
        event: zxdg_exported_v2::Event,
        _: &(),
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
        if let zxdg_exported_v2::Event::Handle { handle } = event {
            say(&format!("handle {handle}"));
        }
    }
}

impl Dispatch<zxdg_imported_v2::ZxdgImportedV2, ()> for App {
    fn event(
        app: &mut Self,
        _: &zxdg_imported_v2::ZxdgImportedV2,
        event: zxdg_imported_v2::Event,
        _: &(),
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
        if let zxdg_imported_v2::Event::Destroyed = event {
            say("parent destroyed");
            app.done = true;
        }
    }
}

delegate_noop!(App: ignore wl_compositor::WlCompositor);
delegate_noop!(App: ignore wl_shm::WlShm);
delegate_noop!(App: ignore wl_shm_pool::WlShmPool);
delegate_noop!(App: ignore wl_buffer::WlBuffer);
delegate_noop!(App: ignore wl_surface::WlSurface);
delegate_noop!(App: ignore wl_seat::WlSeat);
delegate_noop!(App: ignore zxdg_exporter_v2::ZxdgExporterV2);
delegate_noop!(App: ignore zxdg_importer_v2::ZxdgImporterV2);
delegate_noop!(App: ignore xdg_wm_dialog_v1::XdgWmDialogV1);
delegate_noop!(App: ignore xdg_dialog_v1::XdgDialogV1);
