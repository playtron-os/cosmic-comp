// SPDX-License-Identifier: GPL-3.0-only

//! What each window is doing, from the machine's `one.playtron.Runs1` feed.
//!
//! One reduction decides it, after the prototype's `halo-run.ts`, so the
//! chip and the glyph dot can never disagree: running outranks queued,
//! which outranks a finished run's 4 s flash. A run belongs to a window only
//! when it names the window's stable identifier and the registry stamped it
//! with the window's own workspace; nothing is inferred from app ids or focus.

use std::{
    sync::{Arc, Mutex, RwLock},
    time::{Duration, SystemTime, UNIX_EPOCH},
};

use calloop::{
    RegistrationToken,
    timer::{TimeoutAction, Timer},
};
use smithay::wayland::seat::WaylandFocus;

use crate::{
    fl, shell::element::CosmicSurface, state::State,
    wayland::protocols::toplevel_info::mapped_toplevel_identifier,
};

/// How long a finished run keeps its green flash, from when it ended.
pub const DONE_FLASH_MS: u64 = 4000;

/// Most urgent first: the order is the reduction's ranking.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum RunState {
    Running,
    Queued,
    Done,
}

/// One run from the feed, as the Halo reads it. Needs-you and failed runs
/// are left out: the prototype's chip has no such states.
#[derive(Debug, Clone, PartialEq)]
pub struct Run {
    pub id: String,
    /// Stamped by the registry; empty for the machine plane.
    pub workspace: String,
    /// The ext-foreign-toplevel identifier of the window that asked.
    pub window: Option<String>,
    pub state: RunState,
    pub verb: Option<String>,
    pub title: Option<String>,
    /// 0 to 1; `None` while the producer cannot say.
    pub progress: Option<f64>,
    /// Milliseconds since the epoch, as the registry stamps them.
    pub created: u64,
    pub ended: Option<u64>,
}

impl Run {
    fn shown(&self, now: u64) -> bool {
        match self.state {
            RunState::Running | RunState::Queued => true,
            RunState::Done => self
                .ended
                .is_some_and(|ended| now.saturating_sub(ended) < DONE_FLASH_MS),
        }
    }

    fn attached(&self, window: &str, workspace: &str) -> bool {
        self.window.as_deref() == Some(window) && self.workspace == workspace
    }
}

/// What a window's Halo says: its most urgent run, plus how many it hides.
#[derive(Debug, Clone, PartialEq)]
pub struct HaloRun {
    pub state: RunState,
    /// The producer's verb, else the run's title, in `Verb — detail` form.
    pub verb: String,
    pub progress: Option<f64>,
    pub extra: usize,
}

impl HaloRun {
    fn percent(&self) -> Option<u32> {
        self.progress
            .map(|progress| (progress.clamp(0.0, 1.0) * 100.0).floor() as u32)
    }

    fn suffix(&self) -> String {
        if self.extra > 0 {
            format!(" +{}", self.extra)
        } else {
            String::new()
        }
    }

    /// `"modify with ai · 67%"`, `"queued"` or `"done ✓"`, plus `" +N"`.
    pub fn chip_text(&self) -> String {
        let text = match self.state {
            RunState::Done => fl!("halo-run-done"),
            RunState::Queued => fl!("halo-run-queued"),
            RunState::Running => {
                let short = self
                    .verb
                    .split(" — ")
                    .next()
                    .unwrap_or_default()
                    .to_lowercase();
                match self.percent() {
                    Some(percent) => format!("{short} · {percent}%"),
                    None => short,
                }
            }
        };
        text + &self.suffix()
    }

    pub fn tooltip(&self) -> String {
        match (self.state, self.percent()) {
            (RunState::Running, Some(percent)) => format!("{} — {percent}%", self.verb),
            _ => self.verb.clone(),
        }
    }

    /// The receipt a click on the chip leaves.
    pub fn toast(&self) -> String {
        match self.state {
            RunState::Queued => format!("{} — {}", self.verb, fl!("halo-run-queued")),
            RunState::Done => format!("{} — 100%", self.verb),
            RunState::Running => self.tooltip(),
        }
    }
}

/// A window's runs, most urgent first and then oldest first.
pub fn runs_for_window<'a>(
    runs: &'a [Run],
    window: &str,
    workspace: &str,
    now: u64,
) -> Vec<&'a Run> {
    let mut mine: Vec<_> = runs
        .iter()
        .filter(|run| run.attached(window, workspace) && run.shown(now))
        .collect();
    mine.sort_by(|a, b| (a.state, a.created, &a.id).cmp(&(b.state, b.created, &b.id)));
    mine
}

pub fn halo_run_for(runs: &[Run], window: &str, workspace: &str, now: u64) -> Option<HaloRun> {
    let mine = runs_for_window(runs, window, workspace, now);
    let top = mine.first()?;
    Some(HaloRun {
        state: top.state,
        verb: top
            .verb
            .clone()
            .or_else(|| top.title.clone())
            .filter(|verb| !verb.trim().is_empty())
            .unwrap_or_else(|| fl!("halo-run-untitled")),
        progress: top.progress,
        extra: mine.len() - 1,
    })
}

/// When the next done flash on a window runs out; nothing else announces it.
pub fn next_expiry(runs: &[Run], now: u64) -> Option<u64> {
    runs.iter()
        .filter(|run| run.state == RunState::Done && run.window.is_some())
        .filter_map(|run| run.ended)
        .map(|ended| ended.saturating_add(DONE_FLASH_MS))
        .filter(|at| *at > now)
        .min()
}

pub fn now_ms() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_or(0, |since| {
            u64::try_from(since.as_millis()).unwrap_or(u64::MAX)
        })
}

static FEED: RwLock<Option<Arc<[Run]>>> = RwLock::new(None);
static EXPIRY: Mutex<Option<RegistrationToken>> = Mutex::new(None);

fn feed() -> Arc<[Run]> {
    FEED.read()
        .unwrap()
        .clone()
        .unwrap_or_else(|| Arc::from(Vec::new()))
}

/// The workspace the registry would stamp on this window's client.
fn surface_workspace(surface: &CosmicSurface) -> String {
    if let Some(x11) = surface.x11_surface() {
        return crate::workspace_tag::of_x11(x11).unwrap_or_default();
    }
    surface
        .wl_surface()
        .and_then(|surface| {
            use smithay::reexports::wayland_server::Resource;
            surface.client()
        })
        .and_then(|client| crate::workspace_tag::of_client(&client))
        .unwrap_or_default()
}

pub fn halo_run(surface: &CosmicSurface) -> Option<HaloRun> {
    let window = mapped_toplevel_identifier(surface)?;
    halo_run_for(&feed(), &window, &surface_workspace(surface), now_ms())
}

/// Take a new snapshot from the feed and redraw every Halo with it.
pub fn apply(state: &mut State, runs: Vec<Run>) {
    *FEED.write().unwrap() = Some(Arc::from(runs));
    schedule_expiry(state);
    super::CosmicWindow::refresh_all_halos(&state.common.shell.read());
}

fn schedule_expiry(state: &mut State) {
    let handle = &state.common.event_loop_handle;
    if let Some(token) = EXPIRY.lock().unwrap().take() {
        handle.remove(token);
    }
    let now = now_ms();
    let Some(at) = next_expiry(&feed(), now) else {
        return;
    };
    // A frame late, so the flash has certainly ended when the Halo rebuilds.
    let timer = Timer::from_duration(Duration::from_millis(at - now + 16));
    let token = handle.insert_source(timer, |_, _, state| {
        EXPIRY.lock().unwrap().take();
        schedule_expiry(state);
        super::CosmicWindow::refresh_all_halos(&state.common.shell.read());
        TimeoutAction::Drop
    });
    *EXPIRY.lock().unwrap() = token.ok();
}

/// The prototype's receipt for a press on the chip.
pub fn chip_toast(state: &mut State, surface: &CosmicSurface) {
    if let Some(run) = halo_run(surface) {
        state.common.dbus_state.system_notification(run.toast());
    }
}

#[cfg(test)]
#[path = "runs_tests.rs"]
mod tests;
