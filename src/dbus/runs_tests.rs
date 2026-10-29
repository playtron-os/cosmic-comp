use std::{
    sync::{Arc, Mutex, mpsc},
    time::Duration,
};

use zbus::{object_server::SignalEmitter, zvariant::Str};

use super::*;
use crate::dbus::private_bus::PrivateBus;

fn fields(pairs: &[(&str, &str)]) -> Fields {
    pairs
        .iter()
        .map(|(name, value)| ((*name).to_owned(), Str::from(*value).into()))
        .collect()
}

fn row(id: &str, state: &str) -> Fields {
    let mut row = fields(&[
        ("id", id),
        ("workspace", ""),
        ("window", "abc123"),
        ("state", state),
        ("kind", "chat"),
        ("verb", "Search"),
    ]);
    row.insert("progress".into(), 0.5_f64.into());
    row.insert("created".into(), 7_u64.into());
    row.insert("ended".into(), 9_u64.into());
    row
}

#[test]
fn a_row_parses_into_what_the_halo_reads() {
    let run = parse(&row("run-1", "running")).unwrap();
    assert_eq!(
        run,
        Run {
            id: "run-1".into(),
            workspace: String::new(),
            window: Some("abc123".into()),
            state: RunState::Running,
            verb: Some("Search".into()),
            title: None,
            progress: Some(0.5),
            created: 7,
            ended: Some(9),
        }
    );
    assert_eq!(parse(&row("q", "queued")).unwrap().state, RunState::Queued);
    assert_eq!(parse(&row("d", "done")).unwrap().state, RunState::Done);
}

#[test]
fn the_halo_drops_states_it_has_no_chip_for_and_rows_that_are_not_inference() {
    for state in ["needs-you", "failed", "paused", ""] {
        assert_eq!(parse(&row("r", state)), None, "{state:?}");
    }
    let mut no_kind = row("r", "running");
    no_kind.remove("kind");
    assert_eq!(parse(&no_kind), None);
    let mut no_workspace = row("r", "running");
    no_workspace.remove("workspace");
    assert_eq!(
        parse(&no_workspace),
        None,
        "only the registry names a workspace"
    );
    let mut no_window = row("r", "running");
    no_window.insert("window".into(), Str::from("").into());
    assert_eq!(parse(&no_window).unwrap().window, None);
}

/// Plays the registry: a generation and the rows `List` answers with.
#[derive(Clone)]
struct StandIn(Arc<Mutex<(u64, Vec<(String, &'static str)>)>>);

impl StandIn {
    fn set(&self, generation: u64, rows: &[(&str, &'static str)]) {
        *self.0.lock().unwrap() = (
            generation,
            rows.iter()
                .map(|(id, state)| ((*id).to_owned(), *state))
                .collect(),
        );
    }
}

#[zbus::interface(name = "one.playtron.Runs1")]
impl StandIn {
    fn list(&self) -> (u64, Vec<Fields>) {
        let (generation, rows) = &*self.0.lock().unwrap();
        (
            *generation,
            rows.iter().map(|(id, state)| row(id, state)).collect(),
        )
    }

    #[zbus(signal)]
    async fn changed(emitter: &SignalEmitter<'_>, generation: u64) -> zbus::Result<()>;
}

async fn serve(bus: &PrivateBus, registry: &StandIn) -> zbus::Connection {
    zbus::connection::Builder::address(bus.address.as_str())
        .unwrap()
        .serve_at(PATH, registry.clone())
        .unwrap()
        .name(DEST)
        .unwrap()
        .build()
        .await
        .unwrap()
}

fn ids(runs: &[Run]) -> Vec<&str> {
    runs.iter().map(|run| run.id.as_str()).collect()
}

#[test]
fn the_watcher_follows_changes_and_a_restarted_registry() {
    let Some(bus) = PrivateBus::start() else {
        eprintln!("no dbus-daemon; skipping");
        return;
    };
    let registry = StandIn(Arc::new(Mutex::new((
        4,
        vec![("run-1".into(), "running"), ("gate".into(), "needs-you")],
    ))));
    let (tx, rx) = mpsc::channel();
    let next = || {
        rx.recv_timeout(Duration::from_secs(10))
            .expect("a snapshot")
    };
    zbus::block_on(async {
        let service = serve(&bus, &registry).await;
        let client = bus.connect().await;
        std::thread::spawn(move || {
            zbus::block_on(watch(&client, |runs: Vec<Run>| tx.send(runs).is_ok()))
        });
        assert_eq!(
            ids(&next()),
            ["run-1"],
            "needs-you is the panel's, not the Halo's"
        );

        registry.set(5, &[("run-1", "running"), ("run-2", "queued")]);
        let emitter = SignalEmitter::new(&service, PATH).unwrap();
        StandIn::changed(&emitter, 5).await.unwrap();
        assert_eq!(ids(&next()), ["run-1", "run-2"]);

        drop(emitter);
        service.release_name(DEST).await.unwrap();
        drop(service);
        assert!(
            next().is_empty(),
            "a registry that left takes its runs along"
        );

        // A new owner starts counting again; its first snapshot still counts.
        registry.set(1, &[("run-9", "done")]);
        let _service = serve(&bus, &registry).await;
        assert_eq!(ids(&next()), ["run-9"]);
    });
}
