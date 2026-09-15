// SPDX-License-Identifier: GPL-3.0-only
//! Counters for the idle event-loop spin, off unless `COSMIC_COMP_SPIN=1` is
//! set or `~/.cosmic-comp-spin` exists.
//!
//! On an idle desktop the main thread and each `surface-<output>` thread
//! ping-pong over their calloop channels ~1600 times a second while submitting
//! almost nothing to KMS: the render timer is inserted and removed every round
//! and never fires. These counters say which hop of that loop runs and how
//! often, which of the five re-queue conditions is true, and — for
//! `schedule_render`, which anything in the tree may call — who calls it.
//!
//! Every hook is one relaxed load and a branch when the env var is unset.
//!
//! The summary goes through `tracing` at `warn!`, which the default filter
//! keeps in both debug and release: the compositor's own stdout and stderr are
//! swallowed by the session, so a `println!` here would go nowhere.

use std::panic::Location;
use std::sync::atomic::{AtomicBool, AtomicU64, AtomicUsize, Ordering::Relaxed};
use std::sync::{Once, OnceLock};
use std::time::Duration;

pub const SCHEDULE_RENDER: usize = 0;
pub const CMD_SCHEDULE_RENDER: usize = 1;
pub const CMD_VBLANK: usize = 2;
pub const CMD_ADOPT: usize = 3;
pub const QUEUE_REDRAW: usize = 4;
pub const QUEUE_REDRAW_FORCED: usize = 5;
pub const QUEUE_SKIP_WAITING_VBLANK: usize = 6;
pub const QUEUE_SKIP_ALREADY_QUEUED: usize = 7;
pub const QUEUE_INSERTED: usize = 8;
pub const QUEUE_REMOVED_OLD: usize = 9;
pub const RENDER_LATE_IMMEDIATE: usize = 10;
pub const RENDER_TIMER_FIRED: usize = 11;
pub const REDRAW_OK: usize = 12;
pub const REDRAW_ERR: usize = 13;
pub const ON_VBLANK: usize = 14;
pub const ON_ESTIMATED_VBLANK: usize = 15;
pub const EST_VBLANK_QUEUED: usize = 16;
pub const SEND_FRAMES_SURFACE: usize = 17;
pub const SEND_FRAMES_MAIN: usize = 18;
pub const NODE_ADDED: usize = 19;
pub const WHY_ICED: usize = 20;
pub const WHY_FORCE: usize = 21;
pub const WHY_STRESS: usize = 22;
pub const WHY_ANIMATIONS: usize = 23;
pub const WHY_ADOPT: usize = 24;
pub const WHY_NOTHING: usize = 25;
pub const N: usize = 26;

const NAMES: [&str; N] = [
    "schedule_render",
    "cmd.ScheduleRender",
    "cmd.VBlank",
    "cmd.AdoptFrozenFrame",
    "queue_redraw",
    "queue_redraw(force)",
    "queue.skip:waiting_vblank",
    "queue.skip:already_queued",
    "queue.timer_inserted",
    "queue.timer_removed_old",
    "queue.late_immediate",
    "render_timer_fired",
    "redraw.ok",
    "redraw.err",
    "on_vblank",
    "on_estimated_vblank",
    "estimated_vblank_queued",
    "send_frames(surface)",
    "send_frames(main)",
    "node_added",
    "why.iced_redraw_request",
    "why.force",
    "why.is_stressing",
    "why.animations_going",
    "why.adopt",
    "why.nothing(no requeue)",
];

static COUNTS: [AtomicU64; N] = [const { AtomicU64::new(0) }; N];
static ENABLED: AtomicBool = AtomicBool::new(false);

/// Where `schedule_render` is called from. Keyed on the address of the
/// caller's `Location`, which is a distinct static per call site.
const SLOTS: usize = 32;
struct Site {
    key: AtomicUsize,
    loc: OnceLock<&'static Location<'static>>,
    count: AtomicU64,
}
static SITES: [Site; SLOTS] = [const {
    Site {
        key: AtomicUsize::new(0),
        loc: OnceLock::new(),
        count: AtomicU64::new(0),
    }
}; SLOTS];
static SITES_OVERFLOWED: AtomicU64 = AtomicU64::new(0);

/// Reads the env var once and starts the reporter. Cheap on every later call.
#[inline]
pub fn enabled() -> bool {
    static ONCE: Once = Once::new();
    ONCE.call_once(|| {
        // The session's environment is the display manager's, which is awkward
        // to add to, so a file in the home directory turns this on as well.
        let on = std::env::var("COSMIC_COMP_SPIN").is_ok_and(|v| v != "0" && !v.is_empty())
            || std::env::var_os("HOME")
                .map(|home| std::path::Path::new(&home).join(".cosmic-comp-spin"))
                .is_some_and(|marker| marker.exists());
        ENABLED.store(on, Relaxed);
        if on {
            report_every(
                std::env::var("COSMIC_COMP_SPIN_SECS")
                    .ok()
                    .and_then(|v| v.parse().ok())
                    .unwrap_or(5),
            );
        }
    });
    ENABLED.load(Relaxed)
}

#[inline]
pub fn bump(counter: usize) {
    if enabled() {
        COUNTS[counter].fetch_add(1, Relaxed);
    }
}

/// Which of the five conditions made a vblank re-queue, counted separately
/// because the real `if` short-circuits and would hide the later ones.
pub fn why(iced: bool, force: bool, stress: bool, animations: bool, adopt: bool) {
    if !enabled() {
        return;
    }
    for (on, counter) in [
        (iced, WHY_ICED),
        (force, WHY_FORCE),
        (stress, WHY_STRESS),
        (animations, WHY_ANIMATIONS),
        (adopt, WHY_ADOPT),
    ] {
        if on {
            COUNTS[counter].fetch_add(1, Relaxed);
        }
    }
    if !(iced || force || stress || animations || adopt) {
        COUNTS[WHY_NOTHING].fetch_add(1, Relaxed);
    }
}

pub fn note_site(loc: &'static Location<'static>) {
    if !enabled() {
        return;
    }
    let key = std::ptr::from_ref(loc) as usize;
    for site in &SITES {
        let held = site.key.load(Relaxed);
        if held == key {
            site.count.fetch_add(1, Relaxed);
            return;
        }
        if held == 0 && site.key.compare_exchange(0, key, Relaxed, Relaxed).is_ok() {
            let _ = site.loc.set(loc);
            site.count.fetch_add(1, Relaxed);
            return;
        }
    }
    SITES_OVERFLOWED.fetch_add(1, Relaxed);
}

/// Report per-second rates every `secs`, into the journal and
/// `$XDG_RUNTIME_DIR/cosmic-comp.log` alongside the rest of the logging.
fn report_every(secs: u64) {
    let secs = secs.max(1);
    let _ = std::thread::Builder::new()
        .name("spin-report".into())
        .spawn(move || {
            let mut previous = [0u64; N];
            let mut previous_sites = [0u64; SLOTS];
            tracing::warn!(
                "[spin] counting; a summary follows every {secs}s, as events per second"
            );
            loop {
                std::thread::sleep(Duration::from_secs(secs));
                let mut line = String::new();
                for counter in 0..N {
                    let now = COUNTS[counter].load(Relaxed);
                    let delta = now - previous[counter];
                    previous[counter] = now;
                    if delta != 0 {
                        line.push_str(&format!(" {}={}", NAMES[counter], delta / secs));
                    }
                }
                if line.is_empty() {
                    tracing::warn!("[spin] quiet");
                } else {
                    tracing::warn!("[spin]{line}");
                }

                let mut sites = Vec::new();
                for (slot, site) in SITES.iter().enumerate() {
                    let now = site.count.load(Relaxed);
                    let delta = now - previous_sites[slot];
                    previous_sites[slot] = now;
                    if let (true, Some(loc)) = (delta != 0, site.loc.get()) {
                        sites.push((delta / secs, *loc));
                    }
                }
                sites.sort_by(|a, b| b.0.cmp(&a.0));
                for (rate, loc) in sites.iter().take(8) {
                    tracing::warn!("[spin]   caller {}:{} = {rate}/s", loc.file(), loc.line());
                }
                let lost = SITES_OVERFLOWED.load(Relaxed);
                if lost != 0 {
                    tracing::warn!("[spin]   (call sites beyond {SLOTS} not attributed: {lost})");
                }
            }
        });
}
