use super::*;

const NOW: u64 = 1_000_000;

fn run(id: &str, window: &str, state: RunState) -> Run {
    Run {
        id: id.to_owned(),
        workspace: "w1".to_owned(),
        window: Some(window.to_owned()),
        state,
        verb: Some("Modify with AI — \"warmer dusk\"".to_owned()),
        title: Some("Warmer dusk".to_owned()),
        progress: Some(0.672),
        created: 10,
        ended: None,
    }
}

fn done(id: &str, window: &str, ended: u64) -> Run {
    Run {
        ended: Some(ended),
        ..run(id, window, RunState::Done)
    }
}

#[test]
fn the_most_urgent_run_speaks_for_the_window_and_counts_the_rest() {
    let runs = [
        done("a", "win", NOW - 1000),
        run("b", "win", RunState::Queued),
        run("c", "win", RunState::Running),
        run("d", "other", RunState::Running),
    ];
    let halo = halo_run_for(&runs, "win", "w1", NOW).unwrap();
    assert_eq!(halo.state, RunState::Running);
    assert_eq!(halo.extra, 2);
    assert_eq!(halo.chip_text(), "modify with ai · 67% +2");

    let halo = halo_run_for(&runs[..2], "win", "w1", NOW).unwrap();
    assert_eq!(halo.state, RunState::Queued, "queued outranks a done flash");
    assert_eq!(halo.chip_text(), "queued +1");
}

#[test]
fn equal_runs_keep_the_order_they_were_made_in() {
    let mut first = run("z", "win", RunState::Running);
    first.verb = Some("First".to_owned());
    let mut second = run("a", "win", RunState::Running);
    second.verb = Some("Second".to_owned());
    second.created = 20;
    let halo = halo_run_for(&[second, first], "win", "w1", NOW).unwrap();
    assert_eq!(halo.verb, "First");
}

#[test]
fn a_finished_run_flashes_for_four_seconds_from_when_it_ended() {
    let runs = [done("a", "win", NOW - DONE_FLASH_MS + 1)];
    assert_eq!(
        halo_run_for(&runs, "win", "w1", NOW).unwrap().chip_text(),
        "done ✓"
    );
    let runs = [done("a", "win", NOW - DONE_FLASH_MS)];
    assert_eq!(halo_run_for(&runs, "win", "w1", NOW), None);
    let mut never_ended = run("a", "win", RunState::Done);
    never_ended.ended = None;
    assert_eq!(halo_run_for(&[never_ended], "win", "w1", NOW), None);
}

#[test]
fn a_run_attaches_only_in_the_workspace_the_registry_stamped() {
    let runs = [run("a", "win", RunState::Running)];
    assert!(halo_run_for(&runs, "win", "w2", NOW).is_none());
    assert!(halo_run_for(&runs, "win", "", NOW).is_none());
    assert!(!has_live_work_in(&runs, "win", "w2"));

    let machine = Run {
        workspace: String::new(),
        ..run("a", "win", RunState::Running)
    };
    assert!(halo_run_for(std::slice::from_ref(&machine), "win", "", NOW).is_some());
    assert!(halo_run_for(&[machine], "win", "w1", NOW).is_none());

    let windowless = Run {
        window: None,
        ..run("a", "win", RunState::Running)
    };
    assert!(halo_run_for(&[windowless], "win", "w1", NOW).is_none());
}

#[test]
fn the_chip_reads_as_the_prototype_writes_it() {
    let mut halo = HaloRun {
        state: RunState::Running,
        verb: "Search the Web — 4 sources".to_owned(),
        progress: Some(0.999),
        extra: 0,
    };
    assert_eq!(halo.chip_text(), "search the web · 99%");
    assert_eq!(halo.tooltip(), "Search the Web — 4 sources — 99%");
    assert_eq!(halo.toast(), "Search the Web — 4 sources — 99%");

    halo.progress = None;
    assert_eq!(
        halo.chip_text(),
        "search the web",
        "no percentage it does not have"
    );
    assert_eq!(halo.tooltip(), "Search the Web — 4 sources");

    halo.state = RunState::Queued;
    halo.progress = Some(0.0);
    assert_eq!(halo.chip_text(), "queued", "a queued run says no 0%");
    assert_eq!(halo.tooltip(), "Search the Web — 4 sources");
    assert_eq!(halo.toast(), "Search the Web — 4 sources — queued");

    halo.state = RunState::Done;
    halo.extra = 3;
    assert_eq!(halo.chip_text(), "done ✓ +3");
    assert_eq!(halo.toast(), "Search the Web — 4 sources — 100%");
}

#[test]
fn a_run_with_no_verb_falls_back_to_its_title() {
    let mut untitled = run("a", "win", RunState::Running);
    untitled.verb = None;
    assert_eq!(
        halo_run_for(&[untitled.clone()], "win", "w1", NOW)
            .unwrap()
            .verb,
        "Warmer dusk"
    );
    untitled.title = Some("  ".to_owned());
    assert_eq!(
        halo_run_for(&[untitled], "win", "w1", NOW)
            .unwrap()
            .chip_text(),
        "working · 67%"
    );
}

#[test]
fn only_running_or_queued_work_keeps_a_window_open() {
    assert!(has_live_work_in(
        &[run("a", "win", RunState::Running)],
        "win",
        "w1"
    ));
    assert!(has_live_work_in(
        &[run("a", "win", RunState::Queued)],
        "win",
        "w1"
    ));
    assert!(!has_live_work_in(&[done("a", "win", NOW)], "win", "w1"));
    assert!(!has_live_work_in(&[], "win", "w1"));
}

#[test]
fn the_next_redraw_is_when_the_soonest_flash_ends() {
    let runs = [
        done("a", "win", NOW - 1000),
        done("b", "win", NOW - 3000),
        done("c", "win", NOW - 9000),
        Run {
            window: None,
            ..done("d", "win", NOW - 3500)
        },
        run("e", "win", RunState::Running),
    ];
    assert_eq!(next_expiry(&runs, NOW), Some(NOW + 1000));
    assert_eq!(next_expiry(&runs[2..], NOW), None);
}

#[test]
fn a_close_parks_what_owes_work_and_acts_on_each_window_once() {
    let mut acted = Vec::new();
    let closed = close_each(
        &[1, 2, 1, 3],
        |window| *window == 2,
        |window, outcome| acted.push((*window, outcome)),
    );
    assert_eq!(
        closed,
        Closed {
            closed: 2,
            parked: 1
        }
    );
    assert_eq!(
        acted,
        [
            (1, Outcome::Closed),
            (2, Outcome::Parked),
            (3, Outcome::Closed)
        ]
    );
}

#[test]
fn close_receipts_read_as_the_prototype_writes_them() {
    let receipt = |closed, parked| Closed { closed, parked }.receipt();
    assert_eq!(receipt(0, 0), None);
    assert_eq!(
        receipt(0, 1),
        Some(("1 parked — work continues".to_owned(), Tone::Ai))
    );
    assert_eq!(
        receipt(1, 0),
        Some(("Closed 1 window".to_owned(), Tone::Neutral))
    );
    assert_eq!(
        receipt(2, 1),
        Some((
            "Closed 2 windows · 1 parked — work continues".to_owned(),
            Tone::Ai
        ))
    );
}
