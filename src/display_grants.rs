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
//! replay; on deny they fail as the protocol can. The registry keeps an app's
//! Deny as its row at Off; until that arrives, and for the agent, later ones
//! are refused unasked until the row changes. The window list and window management have
//! no request to hold, so they open only at the row's top level.
//!
//! Keys of a boxed client reach a window only while one of its own
//! workspace's windows has the keyboard: the access dialog holds the keyboard
//! while it is up, and the row's words are "windows in this workspace".
//! Held keys still type once allowed after their client hung up, as `wtype`
//! does before anyone can answer.
//!
//! A clipboard request from the process whose own window has the keyboard is
//! a copy or paste through focus, so it is not held.

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
use tracing::info;

use crate::shell::focus::target::KeyboardFocusTarget;
use crate::state::{ClientState, State};

/// The sandbox engine the registry tags an app's socket with.
pub const APP_ENGINE: &str = "inc.kora.Workspaces.App";
/// The sandbox engine the registry tags an agent's socket with.
pub const AGENT_ENGINE: &str = "inc.kora.Workspaces.Agent";

/// Where what the grants hold, ask and refuse is logged: on at info by default, since a held or
/// refused request otherwise leaves no trace in a release build.
pub const LOG_TARGET: &str = "display-grants";

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
pub struct DisplayGrants {
    table: Arc<parking_lot::RwLock<Table>>,
    /// The boxed clients logged as kept from a class that is off, so each is logged once.
    told_off: Arc<parking_lot::Mutex<HashSet<(ClientId, Class)>>>,
}

impl DisplayGrants {
    /// Whether `client` may see a global of `class`; see [`sees`].
    pub fn may(&self, client: &Client, class: Class, askable: bool) -> bool {
        let Some(state) = client.get_data::<ClientState>() else {
            return false;
        };
        let subject = state.security_context.as_ref().and_then(Subject::of);
        let table = self.table.read();
        let workspace = state.workspace.as_deref();
        let may = sees(
            &table,
            state.not_sandboxed(),
            subject.as_ref(),
            workspace,
            class,
            askable,
        );
        // A hidden global gives the client nothing to be refused, so the refusal is logged here.
        if let (false, true, Some(subject), Some(_)) = (may, askable, subject.as_ref(), workspace)
            && table.standing(workspace, subject, class) == Standing::Off
            && self.told_off.lock().insert((client.id(), class))
        {
            info!(
                target: LOG_TARGET,
                ?subject,
                ?workspace,
                class = class.name(),
                pid = state.pid,
                "refused a client the guarded protocols of a row that is off"
            );
        }
        may
    }

    fn standing_of(&self, workspace: &str, subject: &Subject, class: Class) -> Standing {
        self.table.read().standing(Some(workspace), subject, class)
    }

    /// Forget what was logged of a client that went.
    fn forget(&self, id: &ClientId) {
        self.told_off.lock().retain(|(client, _)| client != id);
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

    /// Take `table`, and disconnect every boxed client that holds an object it can no longer
    /// use (see [`lost`]). Returns the table it replaced.
    pub fn replace(&self, table: Table, dh: &DisplayHandle) -> Table {
        let old = std::mem::replace(&mut *self.table.write(), table);
        let new = self.table.read().clone();
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
            let lost = lost(&old, &new, state.workspace.as_deref(), &subject);
            if !lost.is_empty() && holds_any(dh, &id, &lost) {
                info!(
                    target: LOG_TARGET,
                    ?subject,
                    "a grant was taken back; disconnecting the client using it"
                );
                handle.kill_client(id, DisconnectReason::ConnectionClosed);
            }
        }
        old
    }
}

/// The session's clients see every guarded global and other sandboxes none,
/// as before grants; a registry-boxed client sees what its row leaves on, and
/// globals with no request to hold (`askable` false) only when it is open.
fn sees(
    table: &Table,
    not_sandboxed: bool,
    subject: Option<&Subject>,
    workspace: Option<&str>,
    class: Class,
    askable: bool,
) -> bool {
    if not_sandboxed {
        return true;
    }
    let Some(subject) = subject else {
        return false;
    };
    match table.standing(workspace, subject, class) {
        Standing::Open => true,
        Standing::Ask => askable,
        Standing::Off => false,
    }
}

/// Whether the registry made `client`'s socket. Shell-only globals are hidden
/// from such clients alone, so other sandboxes keep what they had.
pub fn boxed(client: &Client) -> bool {
    client
        .get_data::<ClientState>()
        .and_then(|state| state.security_context.as_ref())
        .and_then(Subject::of)
        .is_some()
}

/// What a boxed client may no longer hold once `old` becomes `new`: the events-only protocols of
/// a class no longer open. A class turned off refuses each later request of the others instead,
/// so a Deny the registry keeps as Off leaves the app running.
fn lost(old: &Table, new: &Table, workspace: Option<&str>, subject: &Subject) -> Vec<&'static str> {
    Class::ALL
        .into_iter()
        .filter(|&c| {
            let (was, now) = (
                old.standing(workspace, subject, c),
                new.standing(workspace, subject, c),
            );
            now != Standing::Open && was == Standing::Open || now == Standing::Off
        })
        .flat_map(|c| c.open_only().iter().copied())
        .collect()
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
    /// Refused, and why, for the log: nothing without workspaces, where nothing is ever asked.
    Refuse(Option<&'static str>),
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
            Decision::Refuse(why) => {
                let refused: Vec<Held> = queue.drain(..).collect();
                if let (Some(why), Some(asker)) = (why, asks.clients.get(id)) {
                    info!(
                        target: LOG_TARGET,
                        subject = ?asker.subject,
                        workspace = %asker.workspace,
                        ?ask,
                        requests = refused.len(),
                        why,
                        "refused held requests"
                    );
                }
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
    let owner = (ask.0 == Class::Clipboard)
        .then(|| keyboard_owner(state))
        .flatten();
    let asks = &mut state.common.display_asks;
    let Some(asker) = asks.clients.get_mut(id) else {
        return Decision::Refuse(Some("its client is gone"));
    };
    let (class, change) = ask;
    let passes = match grants.standing_of(&asker.workspace, &asker.subject, class) {
        Standing::Off => return Decision::Refuse(Some("its row is off")),
        Standing::Open => true,
        Standing::Ask if through_focus(ask, asker, owner.as_ref()) => true,
        Standing::Ask => {
            let denied = (
                asker.workspace.clone(),
                asker.subject.clone(),
                class,
                change,
            );
            if asks.denied.contains(&denied) {
                return Decision::Refuse(Some("denied until its row changes"));
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
    let Some(questions) = &asks.questions else {
        return Decision::Refuse(None);
    };
    let Some(pidfd) = pidfd else {
        return Decision::Refuse(Some("the registry cannot be asked"));
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
        return Decision::Refuse(Some("the registry cannot be asked"));
    }
    info!(
        target: LOG_TARGET,
        subject = ?asker.subject,
        workspace = %asker.workspace,
        ?ask,
        "holding a request while the registry asks the person"
    );
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

/// The boxed process whose surface has the keyboard.
#[derive(Debug, Clone, PartialEq, Eq)]
struct Owner {
    pid: u32,
    subject: Subject,
    workspace: String,
}

fn keyboard_owner(state: &State) -> Option<Owner> {
    let shell = state.common.shell.read();
    let focus = shell
        .seats
        .last_active_checked()
        .and_then(|seat| seat.get_keyboard())
        .and_then(|keyboard| keyboard.current_focus())?;
    let client = focus.wl_surface()?.client()?;
    let client_state = client.get_data::<ClientState>()?;
    Some(Owner {
        pid: client_state.pid?,
        subject: client_state
            .security_context
            .as_ref()
            .and_then(Subject::of)?,
        workspace: client_state.workspace.clone()?,
    })
}

/// Whether `ask` is a copy or paste through focus, which no row guards: a toolkit may use data
/// control for its text fields, on a connection other than its window's.
fn through_focus(ask: Ask, asker: &Asker, owner: Option<&Owner>) -> bool {
    ask.0 == Class::Clipboard
        && owner.is_some_and(|owner| {
            asker.pid == Some(owner.pid)
                && asker.subject == owner.subject
                && asker.workspace == owner.workspace
        })
}

/// Whether a held `ask` is still carried out once allowed after its client hung up: keys, as
/// `wtype` exits before anyone answers, and a paste, which waits on its pipe instead.
fn outlives_client(ask: Ask) -> bool {
    match ask {
        (Class::Input, _) => true,
        (Class::Clipboard, change) => !change,
        (Class::Screen, _) => false,
    }
}

/// The registry answered a question.
pub fn answered(state: &mut State, (id, ask, verdict): Answer) {
    let asks = &mut state.common.display_asks;
    let Some(asker) = asks.clients.get_mut(&id) else {
        return;
    };
    asker.asking.remove(&ask);
    info!(
        target: LOG_TARGET,
        subject = ?asker.subject,
        workspace = %asker.workspace,
        ?ask,
        ?verdict,
        "the registry answered"
    );
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
            info!(
                target: LOG_TARGET,
                subject = ?asker.subject,
                workspace = %asker.workspace,
                ?ask,
                requests = refused.len(),
                "refused held requests: nobody answered"
            );
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

/// `id` disconnected. What [`outlives_client`] still runs once allowed; the
/// rest it held is dropped.
pub fn client_gone(state: &mut State, id: &ClientId) {
    state.common.display_grants.forget(id);
    let asks = &mut state.common.display_asks;
    let Some(asker) = asks.clients.get_mut(id) else {
        return;
    };
    asker.gone = true;
    let dropped: Vec<Held> = asker
        .held
        .iter_mut()
        .filter(|(ask, _)| !outlives_client(**ask))
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
        assert_eq!(Subject::tagged(Some(APP_ENGINE), None), None);
        assert_eq!(Subject::tagged(None, Some("org.example.Mail")), None);
        assert_eq!(
            Subject::tagged(Some("com.system76.CosmicPanel"), Some("panel")),
            None
        );
        // An engine from a newer registry is another sandbox until this compositor knows it.
        assert_eq!(
            Subject::tagged(Some("inc.kora.Workspaces.Service"), Some("x")),
            None
        );
    }

    const EVERY: [(Class, bool); 6] = [
        (Class::Input, true),
        (Class::Input, false),
        (Class::Screen, true),
        (Class::Screen, false),
        (Class::Clipboard, true),
        (Class::Clipboard, false),
    ];

    /// Without the registry, or for anything it did not box, the filters are master's.
    #[test]
    fn the_session_and_other_sandboxes_see_what_they_saw_before_grants() {
        let granted = Table::from_grants(vec![grant(
            "work",
            "app",
            "org.example.Mail",
            &["input", "screen", "clipboard"],
            &[],
        )]);
        for table in [Table::default(), granted] {
            for (class, askable) in EVERY {
                for workspace in [None, Some("work")] {
                    assert!(sees(&table, true, None, workspace, class, askable));
                    // Flatpak's, or any socket the registry did not tag.
                    assert!(!sees(&table, false, None, workspace, class, askable));
                }
            }
        }
    }

    /// A boxed client fails closed: held (and refused with nobody to answer), never open.
    #[test]
    fn a_boxed_client_with_the_registry_away_is_held_not_opened() {
        let mail = Subject::App("org.example.Mail".into());
        let away = Table::default();
        for class in Class::ALL {
            assert_eq!(away.standing(Some("work"), &mail, class), Standing::Ask);
            assert!(sees(&away, false, Some(&mail), Some("work"), class, true));
            assert!(!sees(&away, false, Some(&mail), Some("work"), class, false));
            // Outside every workspace slice, as on a machine without the registry.
            assert!(!sees(&away, false, Some(&mail), None, class, true));
        }
        assert_eq!(Table::from_grants(Vec::new()), away);
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

    fn asker(pid: Option<u32>, subject: Subject, workspace: &str) -> Asker {
        Asker {
            workspace: workspace.into(),
            subject,
            pid,
            gone: false,
            allowed: HashMap::new(),
            asking: HashSet::new(),
            held: HashMap::new(),
        }
    }

    /// An app's text field pastes through data control on a connection of its own: with its own
    /// window on the keyboard that is a paste through focus, not a read outside one.
    #[test]
    fn the_process_whose_window_has_the_keyboard_copies_and_pastes_unasked() {
        let mail = Subject::App("org.example.Mail".into());
        let owner = Owner {
            pid: 40,
            subject: mail.clone(),
            workspace: "work".into(),
        };
        let paste = (Class::Clipboard, false);
        let copy = (Class::Clipboard, true);
        let app = asker(Some(40), mail.clone(), "work");
        assert!(through_focus(paste, &app, Some(&owner)));
        assert!(through_focus(copy, &app, Some(&owner)));

        // Another process, subject or workspace, or no window on the keyboard, still asks.
        let other = |pid, subject: &Subject, workspace| asker(pid, subject.clone(), workspace);
        assert!(!through_focus(
            paste,
            &other(Some(41), &mail, "work"),
            Some(&owner)
        ));
        assert!(!through_focus(
            paste,
            &other(None, &mail, "work"),
            Some(&owner)
        ));
        let clips = Subject::App("org.example.Clips".into());
        assert!(!through_focus(
            paste,
            &other(Some(40), &clips, "work"),
            Some(&owner)
        ));
        let agent = Subject::Agent("kora".into());
        assert!(!through_focus(
            paste,
            &other(Some(40), &agent, "work"),
            Some(&owner)
        ));
        assert!(!through_focus(
            paste,
            &other(Some(40), &mail, "home"),
            Some(&owner)
        ));
        assert!(!through_focus(paste, &app, None));

        // Typing and capture are never a paste.
        assert!(!through_focus((Class::Input, true), &app, Some(&owner)));
        assert!(!through_focus((Class::Screen, false), &app, Some(&owner)));
    }

    /// A paste on a connection of its own hangs up once it has asked and waits on its pipe, and
    /// `wtype` once it has sent its keys: dropping what they held made both fail at "Ask me first".
    #[test]
    fn a_held_paste_and_held_keys_outlive_their_client() {
        assert!(outlives_client((Class::Clipboard, false)));
        assert!(outlives_client((Class::Input, true)));
        assert!(!outlives_client((Class::Clipboard, true)));
        assert!(!outlives_client((Class::Screen, false)));
    }

    /// The registry keeps an app's Deny as its row at Off: that must not cost the app its
    /// connection, so only the events-only protocols of a class that closes are taken back.
    #[test]
    fn a_class_turned_off_takes_back_only_its_events_only_protocols() {
        let mail = Subject::App("org.example.Mail".into());
        let every = ["input", "screen", "clipboard"];
        let ask = Table::default();
        let off = Table::from_grants(vec![grant("work", "app", "org.example.Mail", &[], &every)]);
        let open = Table::from_grants(vec![grant("work", "app", "org.example.Mail", &every, &[])]);
        let events: Vec<&str> = Class::ALL
            .into_iter()
            .flat_map(|c| c.open_only().iter().copied())
            .collect();
        for (from, to) in [(&ask, &off), (&open, &off), (&open, &ask)] {
            let gone = lost(from, to, Some("work"), &mail);
            assert_eq!(gone, events);
        }
        assert!(lost(&ask, &open, Some("work"), &mail).is_empty());
        assert!(lost(&off, &ask, Some("work"), &mail).is_empty());
    }

    #[test]
    fn the_interfaces_watched_are_the_ones_the_protocols_name() {
        use smithay::reexports::wayland_protocols::ext::foreign_toplevel_list::v1::server::ext_foreign_toplevel_list_v1::ExtForeignToplevelListV1;
        assert!(
            Class::Screen
                .open_only()
                .contains(&name_of::<ExtForeignToplevelListV1>())
        );
    }
}
