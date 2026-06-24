// SPDX-License-Identifier: GPL-3.0-only

//! Cold-start (app-launch) benchmark harness.
//!
//! Triggered by **Ctrl+Alt+Super+Shift+F11** (separate from the F12 live report,
//! because this is destructive — it repeatedly launches and kills the target
//! app). For a configured target it measures **launch → first frame of content**,
//! broken into phases:
//!
//!  1. kill any existing instances of the target (clean slate);
//!  2. for N iterations: best-effort drop the page cache, spawn the target and
//!     stamp `t0` (monotonic), then capture, for the first client/window whose
//!     process name matches the target:
//!       * `t_connect`  — its wayland client connects;
//!       * `t_toplevel` — it creates its first xdg-toplevel;
//!       * `t1`         — it maps its first buffer (first frame of content);
//!     record the phase durations, then kill it;
//!  3. write `~/cosmic-coldstart-<unix>.{json,txt}` and reveal it.
//!
//! Phases reported:
//!  - `connect` = `t_connect − t0`  — process start + runtime init + wl connect
//!  - `build`   = `t_toplevel − t_connect` — app builds its UI / creates window
//!  - `frame`   = `t1 − t_toplevel` — app renders its first frame of content
//!  - `total`   = `t1 − t0`
//!
//! `t1` is the first-buffer commit; the on-screen scanout follows within one
//! refresh interval (that final tail would need surface-thread plumbing to
//! measure, so it is intentionally not broken out here).
//!
//! Configuration via environment (no rebuild needed):
//!  - `COSMIC_COLDSTART_APP`   — target command (default `agentos-mail-suite`)
//!  - `COSMIC_COLDSTART_ITERS` — iteration count (default 5)
//!
//! Caveat: a *true* cold start requires dropping the page cache, which needs
//! root. cosmic-comp usually isn't root, so the drop is best-effort; the report
//! records whether it was effective and highlights iteration 1 (the coldest).

use std::time::Duration;

use calloop::{
    RegistrationToken,
    timer::{TimeoutAction, Timer},
};
use smithay::{
    reexports::wayland_server::{Client, Resource, protocol::wl_surface::WlSurface},
    utils::{Monotonic, Time},
};

use crate::state::State;

use super::{Stats, json_escape, read_comm, read_trim};

/// Delay after the initial kill before the first launch (lets the kill settle).
const INITIAL_DELAY: Duration = Duration::from_millis(800);
/// Delay between iterations after killing the launched app.
const BETWEEN_DELAY: Duration = Duration::from_millis(1500);
/// Delay before writing the report after the last iteration.
const FINISH_DELAY: Duration = Duration::from_millis(600);
/// Give up on an iteration if the app hasn't shown a window in this long.
const ITERATION_TIMEOUT: Duration = Duration::from_secs(25);

fn target_app() -> String {
    std::env::var("COSMIC_COLDSTART_APP").unwrap_or_else(|_| "agentos-mail-suite".to_string())
}

fn iterations() -> usize {
    std::env::var("COSMIC_COLDSTART_ITERS")
        .ok()
        .and_then(|s| s.parse::<usize>().ok())
        .filter(|n| *n > 0)
        .unwrap_or(5)
}

/// Run a fire-and-forget shell command (kill / cache-drop), reaping it off-thread.
fn run_detached(command: String) {
    match std::process::Command::new("/bin/sh")
        .arg("-c")
        .arg(&command)
        .spawn()
    {
        Ok(mut child) => {
            std::thread::spawn(move || {
                let _ = child.wait();
            });
        }
        Err(err) => tracing::warn!(?err, "coldstart: failed to run '{}'", command),
    }
}

/// One launch's phase timings (milliseconds). `None` = phase not captured (e.g.
/// the iteration timed out, or a hook didn't fire for that app).
#[derive(Debug, Clone, Default)]
struct IterationResult {
    connect_ms: Option<f64>,
    build_ms: Option<f64>,
    frame_ms: Option<f64>,
    total_ms: Option<f64>,
}

/// In-progress benchmark state. Lives in `Common`.
#[derive(Debug, Default)]
pub struct ColdStart {
    run: Option<Run>,
}

#[derive(Debug)]
struct Run {
    /// Full command spawned each iteration.
    target_cmd: String,
    /// Target binary basename, matched against a mapped window's client `comm`.
    target_base: String,
    total: usize,
    done: usize,
    /// True while waiting for the launched app's first window to map.
    waiting: bool,
    /// Phase timestamps for the current iteration.
    t0: Option<Time<Monotonic>>,
    t_connect: Option<Time<Monotonic>>,
    t_toplevel: Option<Time<Monotonic>>,
    /// Per-iteration results.
    results: Vec<IterationResult>,
    /// Timeout timer for the current iteration, cancelled when the window maps.
    timeout_token: Option<RegistrationToken>,
}

impl Run {
    /// Compute phase durations given the iteration's end (first-buffer) time.
    fn finalize(&self, t1: Option<Time<Monotonic>>) -> IterationResult {
        let ms = |a: Option<Time<Monotonic>>, b: Option<Time<Monotonic>>| match (a, b) {
            (Some(a), Some(b)) => Some(Time::elapsed(&a, b).as_secs_f64() * 1000.0),
            _ => None,
        };
        IterationResult {
            connect_ms: ms(self.t0, self.t_connect),
            build_ms: ms(self.t_connect, self.t_toplevel),
            frame_ms: ms(self.t_toplevel, t1),
            total_ms: ms(self.t0, t1),
        }
    }
}

impl State {
    /// Start (or restart) the cold-start benchmark. Entry point from the hotkey.
    pub fn coldstart_bench(&mut self) {
        if self.common.coldstart.run.is_some() {
            tracing::info!("coldstart: benchmark already running, ignoring trigger");
            return;
        }

        let target_cmd = target_app();
        let bin = target_cmd
            .split_whitespace()
            .next()
            .unwrap_or(&target_cmd)
            .to_string();
        let target_base = bin.rsplit('/').next().unwrap_or(&bin).to_string();
        let total = iterations();

        tracing::info!(
            "coldstart: benchmarking '{}' for {} iterations",
            target_cmd,
            total
        );

        // Clear any existing instances so the first mapped window is ours.
        run_detached(format!("pkill -f '{target_base}' 2>/dev/null || true"));

        self.common.coldstart.run = Some(Run {
            target_cmd,
            target_base,
            total,
            done: 0,
            waiting: false,
            t0: None,
            t_connect: None,
            t_toplevel: None,
            results: Vec::new(),
            timeout_token: None,
        });

        self.coldstart_arm(INITIAL_DELAY, |state| state.coldstart_start_iteration());
    }

    /// Arm a one-shot timer that invokes `f` on the main loop.
    fn coldstart_arm(&mut self, delay: Duration, f: fn(&mut State)) -> Option<RegistrationToken> {
        self.common
            .event_loop_handle
            .insert_source(Timer::from_duration(delay), move |_, _, state| {
                f(state);
                TimeoutAction::Drop
            })
            .ok()
    }

    fn coldstart_start_iteration(&mut self) {
        let Some(run) = self.common.coldstart.run.as_mut() else {
            return;
        };
        // Best-effort page-cache drop for a true cold start (needs root).
        run_detached("sync; (echo 3 > /proc/sys/vm/drop_caches) 2>/dev/null || true".to_string());

        let cmd = run.target_cmd.clone();
        let (iter, total) = (run.done + 1, run.total);
        run.t0 = Some(self.common.clock.now());
        run.t_connect = None;
        run.t_toplevel = None;
        run.waiting = true;

        tracing::info!("coldstart: iter {iter}/{total} launching '{cmd}'");
        self.spawn_command(cmd);

        let token = self.coldstart_arm(ITERATION_TIMEOUT, |state| state.coldstart_timeout());
        if let Some(run) = self.common.coldstart.run.as_mut() {
            run.timeout_token = token;
        }
    }

    /// True if `pid`'s process name matches the benchmark target. `comm` is capped
    /// at 15 chars by the kernel, so it is a prefix of the (longer) basename.
    fn coldstart_pid_matches(&self, pid: i32) -> bool {
        let Some(run) = self.common.coldstart.run.as_ref() else {
            return false;
        };
        let Some(comm) = read_comm(pid) else {
            return false;
        };
        // `comm` is capped at 15 chars by the kernel, so match either direction
        // (target longer than comm, or a wrapper whose comm extends the target).
        comm.len() >= 3
            && (run.target_base.starts_with(&comm) || comm.starts_with(&run.target_base))
    }

    /// Phase 1 hook: a wayland client connected. Records `t_connect` for the
    /// target app. Called from the socket source for every client.
    pub fn coldstart_notify_client_connected(&mut self, client: &Client) {
        if self.coldstart_idle() {
            return;
        }
        let Some(pid) = client
            .get_credentials(&self.common.display_handle)
            .ok()
            .map(|creds| creds.pid)
        else {
            return;
        };
        if !self.coldstart_pid_matches(pid) {
            return;
        }
        let now = self.common.clock.now();
        if let Some(run) = self.common.coldstart.run.as_mut()
            && run.t_connect.is_none()
        {
            run.t_connect = Some(now);
        }
    }

    /// Phase 2 hook: the target created its first xdg-toplevel. Records
    /// `t_toplevel`. Called from `new_toplevel`.
    pub fn coldstart_notify_toplevel_created(&mut self, surface: &WlSurface) {
        if self.coldstart_idle() {
            return;
        }
        let Some(pid) = self.coldstart_surface_pid(surface) else {
            return;
        };
        if !self.coldstart_pid_matches(pid) {
            return;
        }
        let now = self.common.clock.now();
        if let Some(run) = self.common.coldstart.run.as_mut()
            && run.t_toplevel.is_none()
        {
            run.t_toplevel = Some(now);
        }
    }

    /// Phase 3 hook: the target mapped its first buffer (first frame of content).
    /// Records the iteration result and advances. Called from the window-map path.
    pub fn coldstart_notify_window_mapped(&mut self, surface: &WlSurface) {
        if self.coldstart_idle() {
            return;
        }
        let Some(pid) = self.coldstart_surface_pid(surface) else {
            return;
        };
        if !self.coldstart_pid_matches(pid) {
            // Surface a mapped-but-unmatched window so a wrong target name /
            // comm mismatch is debuggable from the log.
            let target = self
                .common
                .coldstart
                .run
                .as_ref()
                .map(|r| r.target_base.clone())
                .unwrap_or_default();
            tracing::debug!(
                "coldstart: ignoring mapped window pid {pid} (comm {:?}); waiting for '{target}'",
                read_comm(pid).unwrap_or_default(),
            );
            return;
        }

        let now = self.common.clock.now();
        let timeout_token;
        let (done, total);
        {
            let run = self.common.coldstart.run.as_mut().unwrap();
            let result = run.finalize(Some(now));
            tracing::info!(
                "coldstart: iter {}/{} = {} (connect {}, build {}, frame {})",
                run.done + 1,
                run.total,
                fmt_ms(result.total_ms),
                fmt_ms(result.connect_ms),
                fmt_ms(result.build_ms),
                fmt_ms(result.frame_ms),
            );
            run.results.push(result);
            run.waiting = false;
            run.done += 1;
            timeout_token = run.timeout_token.take();
            done = run.done;
            total = run.total;
        }
        if let Some(token) = timeout_token {
            self.common.event_loop_handle.remove(token);
        }

        let base = self
            .common
            .coldstart
            .run
            .as_ref()
            .unwrap()
            .target_base
            .clone();
        run_detached(format!(
            "kill {pid} 2>/dev/null; pkill -f '{base}' 2>/dev/null || true"
        ));

        self.coldstart_advance(done, total);
    }

    fn coldstart_timeout(&mut self) {
        let (done, total, base);
        {
            let Some(run) = self.common.coldstart.run.as_mut() else {
                return;
            };
            if !run.waiting {
                return;
            }
            // Record whatever phases were captured before the stall.
            let result = run.finalize(None);
            run.results.push(result);
            run.waiting = false;
            run.done += 1;
            run.timeout_token = None;
            done = run.done;
            total = run.total;
            base = run.target_base.clone();
            tracing::warn!("coldstart: iteration {}/{} timed out", done, total);
        }
        run_detached(format!("pkill -f '{base}' 2>/dev/null || true"));
        self.coldstart_advance(done, total);
    }

    fn coldstart_advance(&mut self, done: usize, total: usize) {
        if done < total {
            self.coldstart_arm(BETWEEN_DELAY, |state| state.coldstart_start_iteration());
        } else {
            self.coldstart_arm(FINISH_DELAY, |state| state.coldstart_finish());
        }
    }

    fn coldstart_finish(&mut self) {
        let Some(run) = self.common.coldstart.run.take() else {
            return;
        };
        match write_coldstart_report(&run) {
            Ok(path) => {
                tracing::info!("coldstart: report written to {}", path.display());
                self.reveal_in_file_manager(&path);
            }
            Err(err) => tracing::warn!(?err, "coldstart: failed to write report"),
        }
    }

    /// True when no benchmark iteration is waiting for a window (the common path).
    fn coldstart_idle(&self) -> bool {
        self.common
            .coldstart
            .run
            .as_ref()
            .map(|r| !r.waiting)
            .unwrap_or(true)
    }

    fn coldstart_surface_pid(&self, surface: &WlSurface) -> Option<i32> {
        self.common
            .display_handle
            .get_client(surface.id())
            .ok()
            .and_then(|client| {
                client
                    .get_credentials(&self.common.display_handle)
                    .ok()
                    .map(|creds| creds.pid)
            })
    }
}

fn fmt_ms(o: Option<f64>) -> String {
    o.map(|m| format!("{m:.0}ms")).unwrap_or_else(|| "-".into())
}

/// Collect the `Some` values of one phase across iterations into a `Stats`.
fn phase_stats(
    results: &[IterationResult],
    pick: impl Fn(&IterationResult) -> Option<f64>,
) -> Stats {
    Stats::of(results.iter().filter_map(pick).collect())
}

fn write_coldstart_report(run: &Run) -> std::io::Result<std::path::PathBuf> {
    let unix_secs = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0);
    let home = std::env::var("HOME").unwrap_or_else(|_| "/tmp".into());
    let base = format!("{home}/cosmic-coldstart-{unix_secs}");

    let hostname = read_trim("/proc/sys/kernel/hostname");
    let kernel = read_trim("/proc/sys/kernel/osrelease");
    let version = env!("CARGO_PKG_VERSION");
    let cache_drop_effective = std::fs::OpenOptions::new()
        .write(true)
        .open("/proc/sys/vm/drop_caches")
        .is_ok();

    let total_stats = phase_stats(&run.results, |r| r.total_ms);
    let connect_stats = phase_stats(&run.results, |r| r.connect_ms);
    let build_stats = phase_stats(&run.results, |r| r.build_ms);
    let frame_stats = phase_stats(&run.results, |r| r.frame_ms);
    let ok = run.results.iter().filter(|r| r.total_ms.is_some()).count();
    let first = run.results.first().and_then(|r| r.total_ms);

    // JSON
    let mut j = String::new();
    j.push('{');
    j.push_str("\"schema\":\"cosmic-comp.coldstart/2\",");
    j.push_str(&format!("\"unix_secs\":{unix_secs},"));
    j.push_str(&format!("\"hostname\":\"{}\",", json_escape(&hostname)));
    j.push_str(&format!("\"kernel\":\"{}\",", json_escape(&kernel)));
    j.push_str(&format!(
        "\"compositor_version\":\"{}\",",
        json_escape(version)
    ));
    j.push_str(&format!("\"target\":\"{}\",", json_escape(&run.target_cmd)));
    j.push_str(&format!("\"iterations\":{},", run.total));
    j.push_str(&format!("\"cache_drop_effective\":{cache_drop_effective},"));
    j.push_str("\"metric\":\"launch_to_first_frame_ms\",");
    j.push_str("\"phases\":\"connect|build|frame|total\",");
    j.push_str("\"samples\":[");
    for (i, r) in run.results.iter().enumerate() {
        if i > 0 {
            j.push(',');
        }
        j.push_str(&format!(
            "{{\"connect_ms\":{},\"build_ms\":{},\"frame_ms\":{},\"total_ms\":{}}}",
            json_num(r.connect_ms),
            json_num(r.build_ms),
            json_num(r.frame_ms),
            json_num(r.total_ms),
        ));
    }
    j.push_str("],");
    j.push_str(&format!("\"first_ms\":{},", json_num(first)));
    j.push_str(&format!("\"connect_ms\":{},", connect_stats.to_json()));
    j.push_str(&format!("\"build_ms\":{},", build_stats.to_json()));
    j.push_str(&format!("\"frame_ms\":{},", frame_stats.to_json()));
    j.push_str(&format!("\"total_ms\":{}", total_stats.to_json()));
    j.push('}');
    std::fs::write(format!("{base}.json"), j)?;

    // Human-readable
    let mut s = String::new();
    s.push_str("cosmic-comp cold-start (app-launch) report\n");
    s.push_str("==========================================\n");
    s.push_str(&format!("time (unix):  {unix_secs}\n"));
    s.push_str(&format!("hostname:     {hostname}\n"));
    s.push_str(&format!("kernel:       {kernel}\n"));
    s.push_str(&format!("compositor:   {version}\n"));
    s.push_str(&format!("target:       {}\n", run.target_cmd));
    s.push_str(&format!("iterations:   {}\n", run.total));
    s.push_str(&format!(
        "cache drop:   {}\n\n",
        if cache_drop_effective {
            "effective (true cold start)"
        } else {
            "NOT effective — warm start (page-cache drop needs root)"
        }
    ));

    s.push_str("Per-iteration phases (ms): launch(exec) → first frame of content\n");
    s.push_str("---------------------------------------------------------------\n");
    s.push_str(&format!(
        "  {:>4}  {:>9}  {:>9}  {:>9}  {:>9}\n",
        "iter", "connect", "build", "frame", "total"
    ));
    for (i, r) in run.results.iter().enumerate() {
        s.push_str(&format!(
            "  {:>4}  {:>9}  {:>9}  {:>9}  {:>9}\n",
            i + 1,
            col(r.connect_ms),
            col(r.build_ms),
            col(r.frame_ms),
            col(r.total_ms),
        ));
    }
    s.push('\n');
    s.push_str("  phase meaning: connect = exec→wl-connect (process + runtime init)\n");
    s.push_str("                 build   = connect→first toplevel (UI construction)\n");
    s.push_str("                 frame   = toplevel→first buffer (first content render)\n\n");

    if let Some(first) = first {
        s.push_str(&format!("  first (coldest) total: {first:.1} ms\n"));
    }
    if ok > 0 {
        s.push_str(&format!(
            "  total  ms: min {:.1}  p50 {:.1}  p95 {:.1}  max {:.1}  mean {:.1}  ({ok}/{} ok)\n",
            total_stats.min,
            total_stats.p50,
            total_stats.p95,
            total_stats.max,
            total_stats.mean,
            run.total,
        ));
        s.push_str(&format!(
            "  connect  : p50 {:.1}   build: p50 {:.1}   frame: p50 {:.1}\n",
            connect_stats.p50, build_stats.p50, frame_stats.p50,
        ));
    } else {
        s.push_str("  (no successful launches captured)\n");
    }
    s.push('\n');
    s.push_str("Notes\n-----\n");
    s.push_str("  * total = launch(exec) → first-buffer commit (first frame of\n");
    s.push_str("    content); on-screen scanout follows within one refresh interval.\n");
    s.push_str("  * Without an effective cache drop, only iteration 1 is near-cold;\n");
    s.push_str("    later iterations are warm. Run the compositor as root, or boot\n");
    s.push_str("    fresh, for a true cold start across all iterations.\n");
    s.push_str("  * The harness kills running instances of the target app.\n");

    let path = std::path::PathBuf::from(format!("{base}.txt"));
    std::fs::write(&path, s)?;
    Ok(path)
}

fn json_num(o: Option<f64>) -> String {
    o.map(|m| format!("{m:.3}"))
        .unwrap_or_else(|| "null".into())
}

fn col(o: Option<f64>) -> String {
    o.map(|m| format!("{m:.1}")).unwrap_or_else(|| "-".into())
}
