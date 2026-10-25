//! What a client the workspace registry boxed may use of the guarded protocols.
//!
//! The registry gives each app and agent it launches a socket of its own
//! (`wp_security_context_v1`), tagged with the app's desktop id or the agent's
//! id. Three classes of protocols are guarded on such a client by its
//! workspace's rows (`DisplayGrants`, followed by `dbus::display_grants`): a
//! row at its top level opens its class, Off hides it, and "Ask me first"
//! offers the class's request protocols and holds the first request that
//! does something (a key, a frame, reading or setting the clipboard) while the
//! registry asks the person (`JudgeDisplay`). On allow the held requests
//! replay; on deny they fail as the protocol can, and later ones are refused
//! unasked until the row changes. The window list and window management have
//! no request to hold, so they open only at the row's top level.
//!
//! Keys of a boxed client reach a window only while one of its own
//! workspace's windows has the keyboard: the access dialog holds the keyboard
//! while it is up, and the row's words are "windows in this workspace".

use std::collections::{HashMap, HashSet, VecDeque};
use std::os::fd::OwnedFd;
use std::sync::Arc;
use std::time::{Duration, Instant};

use kora_workspaces_client::display::{Grant, Verdict};
use smithay::{
    reexports::wayland_server::{
        Client, DisplayHandle, Resource,
        backend::{ClientId, DisconnectReason, ObjectId},
    },
    wayland::{
        image_copy_capture::{CaptureFailureReason, CursorSessionRef, Frame, FrameRef, SessionRef},
        pending::{Effect, Pending},
        seat::WaylandFocus,
        security_context::SecurityContext,
    },
};
use tracing::{debug, info};

use crate::shell::focus::target::KeyboardFocusTarget;
use crate::state::{ClientState, State};

/// The sandbox engine the registry tags an app's socket with.
pub const APP_ENGINE: &str = "one.playtron.Workspaces.App";
/// The sandbox engine the registry tags an agent's socket with.
pub const AGENT_ENGINE: &str = "one.playtron.Workspaces.Agent";

/// How long an app's "Allow once" lasts at most, as the registry keeps it.
const APP_ONCE: Duration = Duration::from_secs(10 * 60);

/// What a grant opens.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Class {
    /// Typing as the person (virtual keyboard), and managing other windows.
    Input,
    /// Capturing outputs and windows, and listing windows.
    Screen,
    /// Reading and setting the clipboard outside a paste (data control).
    Clipboard,
}

impl Class {
    const ALL: [Class; 3] = [Class::Input, Class::Screen, Class::Clipboard];

    fn parse(name: &str) -> Option<Self> {
        match name {
            "input" => Some(Class::Input),
            "screen" => Some(Class::Screen),
            "clipboard" => Some(Class::Clipboard),
            _ => None,
        }
    }

    /// The name the registry gives it.
    pub fn name(self) -> &'static str {
        match self {
            Class::Input => "input",
            Class::Screen => "screen",
            Class::Clipboard => "clipboard",
        }
    }

    /// The interfaces whose requests can be held at "Ask me first".
    fn askable(self) -> &'static [&'static str] {
        match self {
            Class::Input => &["zwp_virtual_keyboard_manager_v1", "zwp_virtual_keyboard_v1"],
            Class::Screen => &[
                "ext_image_copy_capture_manager_v1",
                "ext_image_copy_capture_session_v1",
                "ext_image_copy_capture_cursor_session_v1",
                "ext_output_image_capture_source_manager_v1",
                "ext_foreign_toplevel_image_capture_source_manager_v1",
                "zcosmic_workspace_image_capture_source_manager_v1",
            ],
            Class::Clipboard => &[
                "ext_data_control_manager_v1",
                "ext_data_control_device_v1",
                "zwlr_data_control_manager_v1",
                "zwlr_data_control_device_v1",
            ],
        }
    }

    /// The interfaces that only send events, open only at the row's top level.
    fn open_only(self) -> &'static [&'static str] {
        match self {
            Class::Input => &["zcosmic_toplevel_manager_v1"],
            Class::Screen => &["zcosmic_toplevel_info_v1", "ext_foreign_toplevel_list_v1"],
            Class::Clipboard => &[],
        }
    }
}

/// Where a row leaves a class.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Standing {
    Open,
    Off,
    Ask,
}

/// Who a registry-made socket is for.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Subject {
    App(String),
    Agent(String),
}

impl Subject {
    /// `None` for a context the registry did not make.
    pub fn of(context: &SecurityContext) -> Option<Self> {
        Self::tagged(context.sandbox_engine.as_deref(), context.app_id.as_deref())
    }

    fn tagged(engine: Option<&str>, app_id: Option<&str>) -> Option<Self> {
        let id = app_id.filter(|id| !id.is_empty())?.to_string();
        match engine? {
            APP_ENGINE => Some(Subject::App(id)),
            AGENT_ENGINE => Some(Subject::Agent(id)),
            _ => None,
        }
    }

    fn parse(kind: &str, id: String) -> Option<Self> {
        match kind {
            "app" => Some(Subject::App(id)),
            "agent" => Some(Subject::Agent(id)),
            _ => None,
        }
    }

    pub fn kind(&self) -> &'static str {
        match self {
            Subject::App(_) => "app",
            Subject::Agent(_) => "agent",
        }
    }

    pub fn id(&self) -> &str {
        match self {
            Subject::App(id) | Subject::Agent(id) => id,
        }
    }
}

/// Each workspace's subjects' standing per class; a class not listed asks.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Table(HashMap<(String, Subject), HashMap<Class, Standing>>);

impl Table {
    /// From the registry's `DisplayGrants`; unknown kinds and classes are
    /// skipped, so a newer registry opens nothing this compositor does not know.
    pub fn from_grants(grants: Vec<Grant>) -> Self {
        let mut table = HashMap::<_, HashMap<Class, Standing>>::new();
        for grant in grants {
            let Some(subject) = Subject::parse(&grant.kind, grant.id) else {
                continue;
            };
            let classes = table.entry((grant.workspace, subject)).or_default();
            for (names, standing) in [(grant.open, Standing::Open), (grant.off, Standing::Off)] {
                for class in names.iter().filter_map(|c| Class::parse(c)) {
                    classes.insert(class, standing);
                }
            }
        }
        Table(table)
    }

    /// A client in no workspace has no rows: Off.
    pub fn standing(&self, workspace: Option<&str>, subject: &Subject, class: Class) -> Standing {
        let Some(workspace) = workspace else {
            return Standing::Off;
        };
        self.0
            .get(&(workspace.to_string(), subject.clone()))
            .and_then(|classes| classes.get(&class).copied())
            .unwrap_or(Standing::Ask)
    }
}

/// The grants every global filter reads, kept current by the registry follower.
#[derive(Debug, Clone, Default)]
pub struct DisplayGrants(Arc<parking_lot::RwLock<Table>>);

impl DisplayGrants {
    /// Whether `client` may see a global of `class`: anything not sandboxed
    /// may; a registry-boxed client may unless its row is off, or, for a
    /// global with no request to hold (`askable` false), unless it is open;
    /// any other sandboxed client may not.
    pub fn may(&self, client: &Client, class: Class, askable: bool) -> bool {
        let Some(state) = client.get_data::<ClientState>() else {
            return false;
        };
        if state.not_sandboxed() {
            return true;
        }
        match self.standing(state, class) {
            Some(Standing::Open) => true,
            Some(Standing::Ask) => askable,
            Some(Standing::Off) | None => false,
        }
    }

    /// `None` for a client the registry did not box.
    pub fn standing(&self, state: &ClientState, class: Class) -> Option<Standing> {
        let subject = state.security_context.as_ref().and_then(Subject::of)?;
        Some(
            self.0
                .read()
                .standing(state.workspace.as_deref(), &subject, class),
        )
    }

    fn standing_of(&self, workspace: &str, subject: &Subject, class: Class) -> Standing {
        self.0.read().standing(Some(workspace), subject, class)
    }

    /// A filter for a global of `class` whose requests can be held.
    pub fn filter(&self, class: Class) -> impl Fn(&Client) -> bool + Send + Sync + Clone + 'static {
        let grants = self.clone();
        move |client: &Client| grants.may(client, class, true)
    }

    /// A filter for a global of `class` that only sends events.
    pub fn filter_open(
        &self,
        class: Class,
    ) -> impl Fn(&Client) -> bool + Send + Sync + Clone + 'static {
        let grants = self.clone();
        move |client: &Client| grants.may(client, class, false)
    }

    /// Take `table`, and disconnect every boxed client that holds an object
    /// of a class it can no longer use: one turned off, or an events-only one
    /// no longer open. Returns the table it replaced.
    pub fn replace(&self, table: Table, dh: &DisplayHandle) -> Table {
        let old = std::mem::replace(&mut *self.0.write(), table);
        let new = self.0.read().clone();
        let handle = dh.backend_handle();
        let mut clients = Vec::new();
        handle.with_all_clients(|id| clients.push(id));
        for id in clients {
            let Ok(data) = handle.get_client_data(id.clone()) else {
                continue;
            };
            let Some(state) = data.downcast_ref::<ClientState>() else {
                continue;
            };
            let Some(subject) = state.security_context.as_ref().and_then(Subject::of) else {
                continue;
            };
            let workspace = state.workspace.as_deref();
            let lost: Vec<&'static str> = Class::ALL
                .into_iter()
                .flat_map(|c| {
                    let (was, now) = (
                        old.standing(workspace, &subject, c),
                        new.standing(workspace, &subject, c),
                    );
                    let mut gone: Vec<&'static str> = Vec::new();
                    if now == Standing::Off && was != Standing::Off {
                        gone.extend(c.askable());
                    }
                    if now != Standing::Open && was == Standing::Open || now == Standing::Off {
                        gone.extend(c.open_only());
                    }
                    gone
                })
                .collect();
            if !lost.is_empty() && holds_any(dh, &id, &lost) {
                info!(
                    ?subject,
                    "a grant was taken back; disconnecting the client using it"
                );
                handle.kill_client(id, DisconnectReason::ConnectionClosed);
            }
        }
        old
    }
}

/// Whether `client` holds an object of any of `interfaces`.
fn holds_any(dh: &DisplayHandle, client: &ClientId, interfaces: &[&str]) -> bool {
    let handle = dh.backend_handle();
    let mut objects: Vec<ObjectId> = Vec::new();
    if handle
        .with_all_objects_for(client.clone(), |id| objects.push(id))
        .is_err()
    {
        return false;
    }
    objects
        .iter()
        .any(|id| interfaces.contains(&id.interface().name))
}

/// A question: a class, and whether the request changes anything.
pub type Ask = (Class, bool);

/// A request kept for a decision.
pub enum Held {
    Request(Pending<State>),
    Frame(SessionRef, Frame),
    Cursor(CursorSessionRef, Frame),
}

impl Held {
    fn setup(&self) -> bool {
        matches!(self, Held::Request(p) if p.effect() == Effect::Setup)
    }

    fn run(self, state: &mut State) {
        match self {
            Held::Request(pending) => pending.run(state),
            Held::Frame(session, frame) => {
                crate::wayland::handlers::image_copy_capture::capture_frame(state, &session, frame)
            }
            Held::Cursor(session, frame) => {
                crate::wayland::handlers::image_copy_capture::capture_cursor(state, &session, frame)
            }
        }
    }

    fn refuse(self, state: &mut State) {
        match self {
            Held::Request(pending) => pending.refuse(state),
            Held::Frame(_, frame) | Held::Cursor(_, frame) => {
                frame.fail(CaptureFailureReason::Unknown)
            }
        }
    }
}

/// One boxed client's questions and held requests.
struct Asker {
    workspace: String,
    subject: Subject,
    pid: Option<u32>,
    /// Disconnected: what it left is replayed or dropped, then it goes.
    gone: bool,
    allowed: HashMap<Ask, Instant>,
    asking: HashSet<Ask>,
    held: HashMap<Ask, VecDeque<Held>>,
}

/// A question for the registry.
pub struct Question {
    pub client: ClientId,
    pub ask: Ask,
    pub workspace: String,
    pub kind: &'static str,
    pub id: String,
    pub pidfd: OwnedFd,
}

/// The registry's answer to a [`Question`]; `None` when it could not be asked.
pub type Answer = (ClientId, Ask, Option<Verdict>);

/// Every boxed client's held requests and the answers they had.
#[derive(Default)]
pub struct DisplayAsks {
    clients: HashMap<ClientId, Asker>,
    /// Refused unasked until the row changes.
    denied: HashSet<(String, Subject, Class, bool)>,
    /// Where questions go: the registry follower.
    questions: Option<futures_channel::mpsc::UnboundedSender<Question>>,
}

impl std::fmt::Debug for DisplayAsks {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DisplayAsks")
            .field("clients", &self.clients.len())
            .field("denied", &self.denied)
            .finish()
    }
}

impl DisplayAsks {
    pub fn set_questions(&mut self, questions: futures_channel::mpsc::UnboundedSender<Question>) {
        self.questions = Some(questions);
    }
}

enum Decision {
    Pass,
    Refuse,
    Wait,
}

/// A guarded request from `client`: carried out, kept or refused by its
/// workspace's rows.
pub fn request(state: &mut State, client: &Client, ask: Ask, held: Held) {
    let Some(client_state) = client.get_data::<ClientState>() else {
        held.refuse(state);
        return;
    };
    if client_state.not_sandboxed() {
        held.run(state);
        return;
    }
    let (Some(subject), Some(workspace)) = (
        client_state.security_context.as_ref().and_then(Subject::of),
        client_state.workspace.clone(),
    ) else {
        held.refuse(state);
        return;
    };
    let id = client.id();
    let asker = state
        .common
        .display_asks
        .clients
        .entry(id.clone())
        .or_insert_with(|| Asker {
            workspace,
            subject,
            pid: client_state.pid,
            gone: false,
            allowed: HashMap::new(),
            asking: HashSet::new(),
            held: HashMap::new(),
        });
    asker.held.entry(ask).or_default().push_back(held);
    drain(state, &id, ask);
}

/// Carry out what `id` holds for `ask` as far as its rows let it.
fn drain(state: &mut State, id: &ClientId, ask: Ask) {
    loop {
        let decision = decide(state, id, ask);
        let asks = &mut state.common.display_asks;
        let Some(queue) = asks.clients.get_mut(id).and_then(|a| a.held.get_mut(&ask)) else {
            return;
        };
        let Some(front) = queue.front() else {
            break;
        };
        match decision {
            // A keymap at the front shows nothing yet.
            _ if front.setup() => queue.pop_front().unwrap().run(state),
            Decision::Pass => queue.pop_front().unwrap().run(state),
            Decision::Refuse => {
                let refused: Vec<Held> = queue.drain(..).collect();
                for held in refused {
                    held.refuse(state);
                }
                break;
            }
            Decision::Wait => return,
        }
    }
    let asks = &mut state.common.display_asks;
    if asks
        .clients
        .get(id)
        .is_some_and(|a| a.gone && a.held.values().all(VecDeque::is_empty))
    {
        asks.clients.remove(id);
    }
}

fn decide(state: &mut State, id: &ClientId, ask: Ask) -> Decision {
    let grants = state.common.display_grants.clone();
    let asks = &mut state.common.display_asks;
    let Some(asker) = asks.clients.get_mut(id) else {
        return Decision::Refuse;
    };
    let (class, change) = ask;
    let passes = match grants.standing_of(&asker.workspace, &asker.subject, class) {
        Standing::Off => return Decision::Refuse,
        Standing::Open => true,
        Standing::Ask => {
            let denied = (
                asker.workspace.clone(),
                asker.subject.clone(),
                class,
                change,
            );
            if asks.denied.contains(&denied) {
                return Decision::Refuse;
            }
            let fresh = |at: &Instant| match asker.subject {
                // The agent's "Allow once" is its runs': dropped when they change.
                Subject::Agent(_) => true,
                Subject::App(_) => at.elapsed() < APP_ONCE,
            };
            asker.allowed.get(&ask).is_some_and(fresh)
        }
    };
    if passes {
        let workspace = asker.workspace.clone();
        if class == Class::Input && !focus_in(state, &workspace) {
            return Decision::Wait;
        }
        return Decision::Pass;
    }
    if asker.asking.contains(&ask) {
        return Decision::Wait;
    }
    let pidfd = asker
        .pid
        .and_then(|pid| rustix::process::Pid::from_raw(pid as i32))
        .and_then(|pid| rustix::process::pidfd_open(pid, rustix::process::PidfdFlags::empty()).ok())
        .or_else(|| std::fs::File::open("/dev/null").ok().map(OwnedFd::from));
    let (Some(questions), Some(pidfd)) = (&asks.questions, pidfd) else {
        return Decision::Refuse;
    };
    let question = Question {
        client: id.clone(),
        ask,
        workspace: asker.workspace.clone(),
        kind: asker.subject.kind(),
        id: asker.subject.id().to_string(),
        pidfd,
    };
    if questions.unbounded_send(question).is_err() {
        return Decision::Refuse;
    }
    debug!(subject = ?asker.subject, ?ask, "holding a request for the person");
    asker.asking.insert(ask);
    Decision::Wait
}

/// Whether the keyboard is on a window of `workspace`: a Wayland surface of
/// one of its clients, or an X11 window while its desktop is on screen.
fn focus_in(state: &State, workspace: &str) -> bool {
    let shell = state.common.shell.read();
    let Some(focus) = shell
        .seats
        .last_active_checked()
        .and_then(|seat| seat.get_keyboard())
        .and_then(|keyboard| keyboard.current_focus())
    else {
        return false;
    };
    let owner = focus
        .wl_surface()
        .and_then(|surface| surface.client())
        .and_then(|client| {
            client
                .get_data::<ClientState>()
                .map(|s| s.workspace.clone())
        });
    match owner {
        Some(owner) => owner.as_deref() == Some(workspace),
        None => {
            matches!(
                focus,
                KeyboardFocusTarget::Element(_)
                    | KeyboardFocusTarget::Fullscreen(_)
                    | KeyboardFocusTarget::Group(_)
            ) && shell.active_realm() == workspace
        }
    }
}

/// The registry answered a question.
pub fn answered(state: &mut State, (id, ask, verdict): Answer) {
    let asks = &mut state.common.display_asks;
    let Some(asker) = asks.clients.get_mut(&id) else {
        return;
    };
    asker.asking.remove(&ask);
    info!(subject = ?asker.subject, ?ask, ?verdict, "the registry answered");
    match verdict {
        Some(Verdict::Allow) => {
            asker.allowed.insert(ask, Instant::now());
        }
        Some(Verdict::Deny) => {
            asks.denied
                .insert((asker.workspace.clone(), asker.subject.clone(), ask.0, ask.1));
        }
        // Off is the table's to say; nobody answering refuses this once.
        _ => {
            let refused: Vec<Held> = asker
                .held
                .get_mut(&ask)
                .map(|q| q.drain(..).collect())
                .unwrap_or_default();
            for held in refused {
                held.refuse(state);
            }
        }
    }
    drain(state, &id, ask);
}

/// The table changed: answers given under the old one are asked again, a
/// deny is forgotten once its row moves, and what is held is judged anew.
pub fn table_changed(state: &mut State, old: &Table) {
    let grants = state.common.display_grants.clone();
    let asks = &mut state.common.display_asks;
    asks.denied.retain(|(workspace, subject, class, _)| {
        old.standing(Some(workspace), subject, *class)
            == grants.standing_of(workspace, subject, *class)
    });
    for asker in asks.clients.values_mut() {
        asker.allowed.clear();
    }
    drain_all(state);
}

/// The keyboard moved: keys held for their workspace's window may go.
pub fn focus_changed(state: &mut State) {
    let held: Vec<ClientId> = state
        .common
        .display_asks
        .clients
        .iter()
        .filter(|(_, a)| {
            a.held
                .get(&(Class::Input, true))
                .is_some_and(|q| !q.is_empty())
        })
        .map(|(id, _)| id.clone())
        .collect();
    for id in held {
        drain(state, &id, (Class::Input, true));
    }
}

/// A workspace's runs changed: the agent's "Allow once" may have ended.
pub fn runs_changed(state: &mut State) {
    for asker in state.common.display_asks.clients.values_mut() {
        if matches!(asker.subject, Subject::Agent(_)) {
            asker.allowed.clear();
        }
    }
}

/// `id` disconnected. An agent's held keys still type once allowed, as
/// `wtype` exits before anyone answers; anything else it held is dropped.
pub fn client_gone(state: &mut State, id: &ClientId) {
    let asks = &mut state.common.display_asks;
    let Some(asker) = asks.clients.get_mut(id) else {
        return;
    };
    asker.gone = true;
    let agent = matches!(asker.subject, Subject::Agent(_));
    let dropped: Vec<Held> = asker
        .held
        .iter_mut()
        .filter(|(ask, _)| !(agent && ask.0 == Class::Input))
        .flat_map(|(_, q)| q.drain(..))
        .collect();
    for held in dropped {
        held.refuse(state);
    }
    let asks = &mut state.common.display_asks;
    if asks
        .clients
        .get(id)
        .is_some_and(|a| a.held.values().all(VecDeque::is_empty))
    {
        asks.clients.remove(id);
    }
}

/// A held frame the client gave up on.
pub fn frame_aborted(state: &mut State, frame: &FrameRef) {
    for asker in state.common.display_asks.clients.values_mut() {
        for queue in asker.held.values_mut() {
            queue.retain(
                |held| !matches!(held, Held::Frame(_, f) | Held::Cursor(_, f) if f == frame),
            );
        }
    }
}

fn drain_all(state: &mut State) {
    let held: Vec<(ClientId, Ask)> = state
        .common
        .display_asks
        .clients
        .iter()
        .flat_map(|(id, a)| {
            a.held
                .iter()
                .filter(|(_, q)| !q.is_empty())
                .map(move |(ask, _)| (id.clone(), *ask))
        })
        .collect();
    for (id, ask) in held {
        drain(state, &id, ask);
    }
}

/// The question a data-control or virtual keyboard request asks.
pub fn ask_of(class: Class, effect: Effect) -> Ask {
    (class, matches!(effect, Effect::Change | Effect::Input))
}

/// The interface name of a resource type, for checking [`Class`]'s lists.
#[cfg(test)]
fn name_of<R: Resource>() -> &'static str {
    R::interface().name
}

#[cfg(test)]
mod tests {
    use super::*;

    fn grant(workspace: &str, kind: &str, id: &str, open: &[&str], off: &[&str]) -> Grant {
        Grant {
            workspace: workspace.into(),
            kind: kind.into(),
            id: id.into(),
            open: open.iter().map(|c| c.to_string()).collect(),
            off: off.iter().map(|c| c.to_string()).collect(),
        }
    }

    #[test]
    fn only_the_registrys_engines_name_a_subject() {
        assert_eq!(
            Subject::tagged(Some(APP_ENGINE), Some("org.example.Mail")),
            Some(Subject::App("org.example.Mail".into()))
        );
        assert_eq!(
            Subject::tagged(Some(AGENT_ENGINE), Some("kora")),
            Some(Subject::Agent("kora".into()))
        );
        assert_eq!(
            Subject::tagged(Some("org.flatpak"), Some("org.example.Mail")),
            None
        );
        assert_eq!(Subject::tagged(Some(APP_ENGINE), Some("")), None);
    }

    #[test]
    fn a_class_not_listed_asks_and_a_client_in_no_workspace_is_off() {
        let table = Table::from_grants(vec![
            grant("work", "agent", "kora", &["input"], &["screen"]),
            grant(
                "work",
                "app",
                "org.example.Clips",
                &["clipboard", "telepathy"],
                &[],
            ),
            grant("work", "robot", "x", &["input"], &[]),
        ]);
        let kora = Subject::Agent("kora".into());
        let clips = Subject::App("org.example.Clips".into());
        assert_eq!(
            table.standing(Some("work"), &kora, Class::Input),
            Standing::Open
        );
        assert_eq!(
            table.standing(Some("work"), &kora, Class::Screen),
            Standing::Off
        );
        assert_eq!(
            table.standing(Some("work"), &kora, Class::Clipboard),
            Standing::Ask
        );
        assert_eq!(
            table.standing(Some("home"), &kora, Class::Input),
            Standing::Ask,
            "another workspace's agent"
        );
        assert_eq!(table.standing(None, &kora, Class::Input), Standing::Off);
        assert_eq!(
            table.standing(Some("work"), &Subject::App("kora".into()), Class::Input),
            Standing::Ask
        );
        assert_eq!(
            table.standing(Some("work"), &clips, Class::Clipboard),
            Standing::Open
        );
    }

    #[test]
    fn keys_and_clipboard_changes_ask_as_changes_and_reads_as_reads() {
        assert_eq!(ask_of(Class::Input, Effect::Input), (Class::Input, true));
        assert_eq!(
            ask_of(Class::Clipboard, Effect::Read),
            (Class::Clipboard, false)
        );
        assert_eq!(
            ask_of(Class::Clipboard, Effect::Change),
            (Class::Clipboard, true)
        );
    }

    #[test]
    fn the_interfaces_watched_are_the_ones_the_protocols_name() {
        use smithay::reexports::wayland_protocols::ext::data_control::v1::server::ext_data_control_manager_v1::ExtDataControlManagerV1;
        use smithay::reexports::wayland_protocols::ext::foreign_toplevel_list::v1::server::ext_foreign_toplevel_list_v1::ExtForeignToplevelListV1;
        use smithay::reexports::wayland_protocols::ext::image_copy_capture::v1::server::ext_image_copy_capture_manager_v1::ExtImageCopyCaptureManagerV1;
        use smithay::reexports::wayland_protocols_misc::zwp_virtual_keyboard_v1::server::zwp_virtual_keyboard_manager_v1::ZwpVirtualKeyboardManagerV1;
        use smithay::reexports::wayland_protocols_wlr::data_control::v1::server::zwlr_data_control_manager_v1::ZwlrDataControlManagerV1;
        assert!(
            Class::Input
                .askable()
                .contains(&name_of::<ZwpVirtualKeyboardManagerV1>())
        );
        assert!(
            Class::Clipboard
                .askable()
                .contains(&name_of::<ExtDataControlManagerV1>())
        );
        assert!(
            Class::Clipboard
                .askable()
                .contains(&name_of::<ZwlrDataControlManagerV1>())
        );
        assert!(
            Class::Screen
                .askable()
                .contains(&name_of::<ExtImageCopyCaptureManagerV1>())
        );
        assert!(
            Class::Screen
                .open_only()
                .contains(&name_of::<ExtForeignToplevelListV1>())
        );
    }
}
