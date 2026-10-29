use std::{
    collections::VecDeque,
    sync::{Arc, Mutex, mpsc},
    time::Duration,
};

use kora_workspaces_client::display::Grant;
use zbus::object_server::SignalEmitter;

use super::*;
use crate::dbus::private_bus::PrivateBus;
use crate::display_grants::Class;

const PATH: &str = "/one/playtron/Workspaces1";

type Row = (String, String, String, Vec<String>, Vec<String>);

fn row(id: &str, open: &[&str]) -> Row {
    (
        "work".into(),
        "app".into(),
        id.into(),
        open.iter().map(|c| (*c).to_owned()).collect(),
        Vec::new(),
    )
}

fn table(rows: &[Row]) -> Table {
    Table::from_grants(
        rows.iter()
            .cloned()
            .map(|(workspace, kind, id, open, off)| Grant {
                workspace,
                kind,
                id,
                open,
                off,
            })
            .collect(),
    )
}

/// The registry as it is now: rows that can change, and the person's answers in turn.
#[derive(Clone, Default)]
struct Registry {
    rows: Arc<Mutex<Vec<Row>>>,
    verdicts: Arc<Mutex<VecDeque<&'static str>>>,
}

#[zbus::interface(name = "one.playtron.Workspaces1")]
impl Registry {
    fn display_grants(&self) -> Vec<Row> {
        self.rows.lock().unwrap().clone()
    }

    #[zbus(signal)]
    async fn agent_policy_changed(emitter: &SignalEmitter<'_>, id: &str) -> zbus::Result<()>;
}

struct Access(Registry);

#[zbus::interface(name = "one.playtron.AgentAccess1")]
impl Access {
    fn judge_display(
        &self,
        _pidfd: zbus::zvariant::OwnedFd,
        _workspace: String,
        _kind: String,
        _id: String,
        _class: String,
        _change: bool,
    ) -> String {
        self.0
            .verdicts
            .lock()
            .unwrap()
            .pop_front()
            .unwrap_or("deny")
            .into()
    }
}

/// A released registry, from before display grants: it answers neither call.
struct Older;

#[zbus::interface(name = "one.playtron.Workspaces1")]
impl Older {
    fn active(&self) -> String {
        "work".into()
    }
}

/// A registry from after this compositor, whose rows carry one more field.
struct Newer;

#[zbus::interface(name = "one.playtron.Workspaces1")]
impl Newer {
    fn display_grants(&self) -> Vec<(String, String, String, Vec<String>, Vec<String>, u32)> {
        let (workspace, kind, id, open, off) = row("org.example.Clips", &["clipboard"]);
        vec![(workspace, kind, id, open, off, 7)]
    }
}

async fn own<I: zbus::object_server::Interface>(bus: &PrivateBus, at: I) -> zbus::Connection {
    zbus::connection::Builder::address(bus.address.as_str())
        .unwrap()
        .serve_at(PATH, at)
        .unwrap()
        .name(DEST)
        .unwrap()
        .build()
        .await
        .unwrap()
}

async fn serve(bus: &PrivateBus, registry: &Registry) -> zbus::Connection {
    zbus::connection::Builder::address(bus.address.as_str())
        .unwrap()
        .serve_at(PATH, registry.clone())
        .unwrap()
        .serve_at(PATH, Access(registry.clone()))
        .unwrap()
        .name(DEST)
        .unwrap()
        .build()
        .await
        .unwrap()
}

async fn leave(service: zbus::Connection) {
    service.release_name(DEST).await.unwrap();
}

/// A machine without the registry (plain COSMIC, or Kora's not started yet) must
/// still get an answer at once, and boxed clients nothing open, never an error.
#[test]
fn grants_follow_a_registry_that_is_absent_old_gone_and_back() {
    let bus = PrivateBus::start().expect("dbus-daemon on PATH");
    let (tx, rx) = mpsc::channel();
    let next = || rx.recv_timeout(Duration::from_secs(10)).expect("a table");
    let registry = Registry::default();
    *registry.rows.lock().unwrap() = vec![row("org.example.Mail", &["screen"])];
    zbus::block_on(async {
        let compositor = bus.connect().await;
        std::thread::spawn(move || {
            zbus::block_on(watch(&compositor, |table| tx.send(table).is_ok()))
        });
        assert_eq!(next(), Table::default(), "no registry: nothing open");

        let older = own(&bus, Older).await;
        assert_eq!(next(), Table::default(), "a registry without DisplayGrants");
        leave(older).await;
        assert_eq!(next(), Table::default());

        let service = serve(&bus, &registry).await;
        assert_eq!(next(), table(&[row("org.example.Mail", &["screen"])]));

        *registry.rows.lock().unwrap() = vec![row("org.example.Mail", &["screen", "input"])];
        let emitter = SignalEmitter::new(&service, PATH).unwrap();
        Registry::agent_policy_changed(&emitter, "work")
            .await
            .unwrap();
        assert_eq!(
            next(),
            table(&[row("org.example.Mail", &["screen", "input"])])
        );

        drop(emitter);
        leave(service).await;
        assert_eq!(
            next(),
            Table::default(),
            "a registry that left takes its grants along"
        );

        let _service = serve(&bus, &registry).await;
        assert_eq!(
            next(),
            table(&[row("org.example.Mail", &["screen", "input"])]),
            "picked up again without a restart"
        );
    });
}

#[test]
fn grants_from_a_newer_registry_keep_what_this_compositor_knows() {
    let bus = PrivateBus::start().expect("dbus-daemon on PATH");
    zbus::block_on(async {
        let _newer = own(&bus, Newer).await;
        let compositor = bus.connect().await;
        let read = read(&compositor).await;
        assert_eq!(read, table(&[row("org.example.Clips", &["clipboard"])]));
        let clips = crate::display_grants::Subject::App("org.example.Clips".into());
        assert_eq!(
            read.standing(Some("work"), &clips, Class::Clipboard),
            crate::display_grants::Standing::Open
        );
    });
}

/// Nobody to answer refuses the request; a newer verdict refuses it once.
#[test]
fn a_held_request_is_refused_when_the_registry_cannot_answer() {
    let bus = PrivateBus::start().expect("dbus-daemon on PATH");
    let null = std::fs::File::open("/dev/null").unwrap();
    let ask = (Class::Clipboard, true);
    zbus::block_on(async {
        let compositor = bus.connect().await;
        let judge = || {
            judge(
                &compositor,
                null.as_fd(),
                "work",
                "app",
                "org.example.Clips",
                ask,
            )
        };
        assert_eq!(judge().await, None, "no registry");

        let older = own(&bus, Older).await;
        assert_eq!(judge().await, None, "a registry without JudgeDisplay");
        leave(older).await;

        let registry = Registry::default();
        let _service = serve(&bus, &registry).await;
        *registry.verdicts.lock().unwrap() = ["pending", "allow"].into();
        assert_eq!(judge().await, Some(Verdict::Allow), "waits past a hold");
        *registry.verdicts.lock().unwrap() = ["something newer"].into();
        assert_eq!(judge().await, Some(Verdict::Unanswered));
    });
}
