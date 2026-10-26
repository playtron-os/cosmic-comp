//! `grants-check client [ENGINE APP_ID]`: the guarded and shell-only globals a client sees, on the
//! compositor's own socket or, given a sandbox engine and app id, on a `wp_security_context_v1`
//! socket tagged with them; then whether a clipboard copy (data control) from that client goes
//! through (`kept`), is refused (`refused`), or cannot be made (`hidden`). Prints
//! `globals=a,b,...` and `copy=...`.
//!
//! `grants-check registry current|older|newer WORKSPACE APP_ID CLASS,...`: a stand-in workspace
//! registry on the session bus. `current` lists APP_ID's CLASSES open in WORKSPACE and answers
//! every held request with a deny; `older` is a released registry without either call; `newer`
//! adds a field to each row.

use std::{
    collections::HashMap,
    io::Write,
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
    protocol::{wl_registry, wl_seat},
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
                let _ = std::fs::File::from(fd).write_all(b"grants-check");
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
                    id.and_then(|offer| state.offers.get(&offer.id()).cloned())
                        .unwrap_or_default(),
                );
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
            let qh = session_queue.handle();
            let manager: WpSecurityContextManagerV1 = session_globals
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
            session_queue.roundtrip(&mut app).unwrap();
            let stream = UnixStream::connect(&path).unwrap_or_else(|_| fail("cannot connect"));
            let _ = std::fs::remove_file(&path);
            _socket = Some((listener, kept));
            connect(Connection::from_socket(stream).unwrap())
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
    let rows = vec![(
        workspace.to_owned(),
        "app".to_owned(),
        app_id.to_owned(),
        open.split(',')
            .filter(|c| !c.is_empty())
            .map(str::to_owned)
            .collect(),
        Vec::new(),
    )];
    let path = "/one/playtron/Workspaces1";
    let builder = zbus::blocking::connection::Builder::session()?;
    let builder = match kind {
        "current" => builder
            .serve_at(path, Current(rows))?
            .serve_at(path, Deny)?,
        "older" => builder.serve_at(path, Older)?,
        "newer" => builder.serve_at(path, Newer(rows))?.serve_at(path, Deny)?,
        _ => fail("registry: current, older or newer"),
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
        ["registry", kind, workspace, app_id, open] => {
            if let Err(err) = registry(kind, workspace, app_id, open) {
                fail(&format!("registry: {err}"));
            }
        }
        _ => fail("usage: client [ENGINE APP_ID] | registry KIND WORKSPACE APP_ID CLASSES"),
    }
}
