// SPDX-License-Identifier: GPL-3.0-only

//! UI performance measurement.
//!
//! This surfaces the *hardware-accurate* frame timing the compositor already
//! computes for frame pacing (see [`crate::backend::kms::surface::timings`]) plus
//! per-process memory, and writes a structured report to `$HOME` when the user
//! presses the global hotkey **Ctrl+Alt+Super+Shift+F12**.
//!
//! Accuracy notes (this is for due-diligence numbers, so be precise about what
//! each metric is):
//!  - **Frame rate / frametime / dropped frames** are ground truth: they come
//!    from the DRM vblank presentation timestamps and the vblank sequence
//!    numbers — the exact data the pacer schedules against. They are only valid
//!    on the native DRM/KMS backend; the nested winit/x11 backends use synthetic
//!    timing and the report flags them as non-authoritative.
//!  - **UI RAM** is the kernel's PSS (proportional set size, correct
//!    shared-memory attribution) plus best-effort per-process GPU memory from
//!    DRM fdinfo (deduplicated by `drm-client-id`).
//!  - **Input latency** (`input→scanout`) is the compositor-pipeline latency:
//!    a pointer event's libinput hardware timestamp → the DRM scanout that first
//!    displays it. Both ends are hardware timestamps, so it is exact; for
//!    end-to-end motion-to-photon, add a one-time hardware-tester offset for the
//!    USB/firmware-before and panel-response-after that software cannot see.
//!  - **Cold-start** is intentionally *not* part of this hotkey snapshot; it
//!    needs a launch harness — see `docs/PERF_MEASUREMENT.md`.

use std::{
    collections::{HashMap, VecDeque},
    sync::{
        Arc, Mutex,
        atomic::{AtomicBool, AtomicU64, Ordering},
    },
    time::Duration,
};

use calloop::timer::{TimeoutAction, Timer};
use smithay::utils::{Monotonic, Time};

use crate::state::{BackendData, State};

pub mod coldstart;

/// Global capture gate. When `false`, every per-frame / per-input collection hook
/// is a single relaxed atomic load away from a no-op — so the instrumentation has
/// no measurable cost unless a capture window is actively running. Set true only
/// between F12 (arm) and the end of the capture window.
static CAPTURING: AtomicBool = AtomicBool::new(false);

/// Length of an F12-triggered capture window.
const CAPTURE_WINDOW_SECS: u64 = 8;

/// Whether a performance capture is currently running. Read on the render and
/// input hot paths, so kept to a single relaxed load.
#[inline]
pub fn is_capturing() -> bool {
    CAPTURING.load(Ordering::Relaxed)
}

fn set_capturing(on: bool) {
    CAPTURING.store(on, Ordering::Relaxed);
}

/// How many presented frames to retain per output. Covers an 8 s capture window
/// up to ~256 Hz; older frames in a longer window roll off.
const SAMPLE_CAP: usize = 2048;

/// Cap on pointer inputs awaiting a frame (bounded so an idle/off output can't
/// grow the queue without presenting).
const INPUT_PENDING_CAP: usize = 512;

/// How many recent input→present latency samples to retain per output.
const LATENCY_CAP: usize = 2000;

/// Process-name prefixes considered "UI" for the memory section. Matches the
/// compositor itself plus the HumainOS / AgentOS / icetron application suite.
const UI_APP_PREFIXES: &[&str] = &[
    "cosmic-comp",
    "cosmic-",
    "agentos-",
    "humainos-",
    "icetron",
    "kora-sync",
    "nexus",
];

/// One presented frame, recorded from the same DRM vblank data the pacer uses.
#[derive(Clone, Copy, Debug)]
struct FrameSample {
    /// Hardware presentation timestamp (monotonic), nanoseconds.
    present_ns: u64,
    /// Total render cost (elements + draw), nanoseconds.
    render_ns: u64,
    /// Element-list build duration, nanoseconds.
    elements_ns: u64,
    /// GPU draw duration, nanoseconds.
    draw_ns: u64,
    /// Present latency: buffer submit → scanout, nanoseconds.
    present_latency_ns: u64,
    /// Whether this frame was GPU-composited (vs a zero-copy direct scanout).
    composited: bool,
}

/// Per-frame data handed to [`OutputPerf::record`] from the surface thread.
pub struct FrameRecord {
    pub present: Time<Monotonic>,
    pub elements: Duration,
    pub draw: Duration,
    pub present_latency: Duration,
    pub composited: bool,
    pub seq_delta: Option<i64>,
}

/// A pointer input awaiting the first frame that presents its effect.
#[derive(Clone, Copy, Debug)]
struct InputPending {
    /// Input hardware timestamp (CLOCK_MONOTONIC), nanoseconds — the latency start.
    hw_ns: u64,
    /// Monotonic time the compositor processed the event, matched against a
    /// frame's `render_start` to decide which frame first reflects it.
    sched_ns: u64,
}

/// Per-output performance accumulator shared between the output's surface thread
/// (the only writer) and the main thread (the only reader, on a report). The
/// lock is held only for the brief push/trim on each vblank.
#[derive(Debug, Default)]
pub struct OutputPerf {
    samples: Mutex<VecDeque<FrameSample>>,
    /// Total vblanks skipped (sum of `sequence_delta - 1` over all presents).
    dropped_vblanks: AtomicU64,
    /// Number of presents that skipped at least one vblank.
    drop_events: AtomicU64,
    /// Pointer inputs awaiting the first frame that presents their effect.
    input_pending: Mutex<VecDeque<InputPending>>,
    /// Recorded input→present latencies (nanoseconds).
    input_latencies: Mutex<VecDeque<u64>>,
}

impl OutputPerf {
    pub fn new() -> Arc<Self> {
        Arc::new(Self::default())
    }

    /// Record a presented frame. Called from the surface thread in `on_vblank`,
    /// right after [`crate::backend::kms::surface::Timings::presented`].
    /// `seq_delta` is the DRM sequence-number delta from the previous present
    /// (`Some(1)` == no drop, `> 1` == that many vblanks were skipped).
    pub fn record(&self, r: FrameRecord) {
        let ns = |d: Duration| d.as_nanos().min(u64::MAX as u128) as u64;
        let present_ns = ns(Duration::from(r.present));

        {
            let mut samples = self.samples.lock().unwrap();
            samples.push_back(FrameSample {
                present_ns,
                render_ns: ns(r.elements + r.draw),
                elements_ns: ns(r.elements),
                draw_ns: ns(r.draw),
                present_latency_ns: ns(r.present_latency),
                composited: r.composited,
            });
            while samples.len() > SAMPLE_CAP {
                samples.pop_front();
            }
        }

        if let Some(delta) = r.seq_delta
            && delta > 1
        {
            self.dropped_vblanks
                .fetch_add((delta - 1) as u64, Ordering::Relaxed);
            self.drop_events.fetch_add(1, Ordering::Relaxed);
        }
    }

    /// Enqueue a pointer input's timestamps (main thread). Matched to a presented
    /// frame later in [`OutputPerf::record_input_latencies`].
    pub fn push_input(&self, hw_ns: u64, sched_ns: u64) {
        let mut pending = self.input_pending.lock().unwrap();
        pending.push_back(InputPending { hw_ns, sched_ns });
        while pending.len() > INPUT_PENDING_CAP {
            pending.pop_front();
        }
    }

    /// On present (surface thread), consume every pending input processed at or
    /// before this frame's `render_start`, and record ONE latency sample for the
    /// frame: `present − hw_time` of the **most recent** such input. The cursor
    /// position this frame displays is the one set by that newest input; older
    /// batched inputs were superseded and never shown, so recording them would
    /// inflate the tail. This matches how a hardware latency tester measures
    /// displayed-state motion-to-photon. The pending queue is ordered ascending
    /// by `sched_ns`, so the last one popped is the newest.
    pub fn record_input_latencies(&self, render_start_ns: u64, present_ns: u64) {
        let mut newest_hw: Option<u64> = None;
        {
            let mut pending = self.input_pending.lock().unwrap();
            while let Some(front) = pending.front() {
                if front.sched_ns <= render_start_ns {
                    newest_hw = Some(pending.pop_front().unwrap().hw_ns);
                } else {
                    break;
                }
            }
        }
        if let Some(hw_ns) = newest_hw
            && present_ns > hw_ns
        {
            let mut lat = self.input_latencies.lock().unwrap();
            lat.push_back(present_ns - hw_ns);
            while lat.len() > LATENCY_CAP {
                lat.pop_front();
            }
        }
    }

    /// Clear all buffers/counters so a new capture window starts clean. Called on
    /// the main thread at arm time, while `CAPTURING` is still false (no writer).
    pub fn reset(&self) {
        self.samples.lock().unwrap().clear();
        self.input_pending.lock().unwrap().clear();
        self.input_latencies.lock().unwrap().clear();
        self.dropped_vblanks.store(0, Ordering::Relaxed);
        self.drop_events.store(0, Ordering::Relaxed);
    }

    fn snapshot(&self) -> (Vec<FrameSample>, u64, u64, Vec<u64>) {
        let samples = self.samples.lock().unwrap().iter().copied().collect();
        let latencies = self
            .input_latencies
            .lock()
            .unwrap()
            .iter()
            .copied()
            .collect();
        (
            samples,
            self.dropped_vblanks.load(Ordering::Relaxed),
            self.drop_events.load(Ordering::Relaxed),
            latencies,
        )
    }
}

/// Distribution summary (all values in milliseconds for the report).
#[derive(Debug, Clone, Default)]
pub(crate) struct Stats {
    pub min: f64,
    pub mean: f64,
    pub p50: f64,
    pub p95: f64,
    pub p99: f64,
    pub p999: f64,
    pub max: f64,
}

impl Stats {
    pub(crate) fn of(mut v: Vec<f64>) -> Stats {
        if v.is_empty() {
            return Stats::default();
        }
        v.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
        let sum: f64 = v.iter().sum();
        Stats {
            min: v[0],
            mean: sum / v.len() as f64,
            p50: percentile(&v, 0.50),
            p95: percentile(&v, 0.95),
            p99: percentile(&v, 0.99),
            p999: percentile(&v, 0.999),
            max: v[v.len() - 1],
        }
    }

    pub(crate) fn to_json(&self) -> String {
        format!(
            "{{\"min\":{:.3},\"mean\":{:.3},\"p50\":{:.3},\"p95\":{:.3},\"p99\":{:.3},\"p99_9\":{:.3},\"max\":{:.3}}}",
            self.min, self.mean, self.p50, self.p95, self.p99, self.p999, self.max
        )
    }
}

/// Nearest-rank percentile over an already-sorted slice. `q` in `[0, 1]`.
fn percentile(sorted: &[f64], q: f64) -> f64 {
    if sorted.is_empty() {
        return 0.0;
    }
    let rank = (q * (sorted.len() - 1) as f64).round() as usize;
    sorted[rank.min(sorted.len() - 1)]
}

/// Main-loop responsiveness tracking. Updated each event-loop iteration on the
/// main thread (so no synchronization needed) and read into the F12 report.
///
/// Two signals:
///  - **work**: duration of the per-iteration post-dispatch callback — pure
///    main-thread work, no idle wait, so no false positives.
///  - **hitch**: the iteration-to-iteration gap (which spans event dispatch too,
///    catching slow protocol handlers), counted only while the compositor was
///    actively rendering and capped at 5 s, so idle poll-waits aren't mistaken
///    for stalls.
#[derive(Debug, Default)]
pub struct LoopHealth {
    last_tick: Option<std::time::Instant>,
    prev_active: bool,
    iterations: u64,
    max_work_ms: f64,
    work_over_16ms: u64,
    max_hitch_ms: f64,
    hitch_count: u64,
}

impl LoopHealth {
    /// Call at the top of each main-loop iteration; returns the work-start time.
    pub fn begin(&mut self) -> std::time::Instant {
        let now = std::time::Instant::now();
        if let Some(last) = self.last_tick {
            let gap_ms = now.duration_since(last).as_secs_f64() * 1000.0;
            if self.prev_active && (50.0..=5000.0).contains(&gap_ms) {
                self.hitch_count += 1;
                self.max_hitch_ms = self.max_hitch_ms.max(gap_ms);
            }
        }
        self.last_tick = Some(now);
        self.iterations += 1;
        now
    }

    /// Call at the end of the iteration with whether it was actively rendering.
    pub fn end(&mut self, work_start: std::time::Instant, active: bool) {
        let work_ms = work_start.elapsed().as_secs_f64() * 1000.0;
        self.max_work_ms = self.max_work_ms.max(work_ms);
        if work_ms > 16.0 {
            self.work_over_16ms += 1;
        }
        self.prev_active = active;
    }

    /// Reset to start a fresh capture window.
    pub fn reset(&mut self) {
        *self = LoopHealth::default();
    }

    pub(crate) fn snapshot(&self) -> LoopHealthSnapshot {
        LoopHealthSnapshot {
            iterations: self.iterations,
            max_work_ms: self.max_work_ms,
            work_over_16ms: self.work_over_16ms,
            max_hitch_ms: self.max_hitch_ms,
            hitch_count: self.hitch_count,
        }
    }
}

/// A `Send` copy of [`LoopHealth`] for the report worker thread.
#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct LoopHealthSnapshot {
    iterations: u64,
    max_work_ms: f64,
    work_over_16ms: u64,
    max_hitch_ms: f64,
    hitch_count: u64,
}

/// Computed per-output frame statistics for the report.
#[derive(Debug, Clone)]
struct OutputFrameStats {
    name: String,
    frames: usize,
    window_secs: f64,
    fps: f64,
    frametime_ms: Stats,
    render_ms: Stats,
    elements_ms: Stats,
    draw_ms: Stats,
    /// Present latency: buffer submit → scanout.
    present_latency_ms: Stats,
    dropped_vblanks: u64,
    drop_events: u64,
    dropped_pct: f64,
    /// Fraction of frames that hit the zero-copy direct-scanout path (vs GPU
    /// composite). Higher is better for latency and power.
    direct_scanout_pct: f64,
    /// Fraction of frames whose frametime exceeded 1.5× the median (felt stutter).
    jank_pct: f64,
    /// 1%-low and 0.1%-low framerates (worst-case smoothness).
    low_1pct_fps: f64,
    low_01pct_fps: f64,
    /// Pointer input → on-screen (scanout) latency. Compositor-pipeline only;
    /// add the one-time hardware-tester offset for end-to-end motion-to-photon.
    input_latency_ms: Stats,
    input_samples: usize,
}

impl OutputFrameStats {
    fn compute(name: String, perf: &OutputPerf) -> OutputFrameStats {
        let (samples, dropped_vblanks, drop_events, latencies) = perf.snapshot();
        let frames = samples.len();

        let mut frametimes = Vec::with_capacity(frames.saturating_sub(1));
        for w in samples.windows(2) {
            let dt = w[1].present_ns.saturating_sub(w[0].present_ns) as f64 / 1_000_000.0;
            if dt > 0.0 {
                frametimes.push(dt);
            }
        }
        let render_ms: Vec<f64> = samples
            .iter()
            .map(|s| s.render_ns as f64 / 1_000_000.0)
            .collect();
        let elements_ms: Vec<f64> = samples
            .iter()
            .map(|s| s.elements_ns as f64 / 1_000_000.0)
            .collect();
        let draw_ms: Vec<f64> = samples
            .iter()
            .map(|s| s.draw_ns as f64 / 1_000_000.0)
            .collect();
        let present_latency_ms: Vec<f64> = samples
            .iter()
            .map(|s| s.present_latency_ns as f64 / 1_000_000.0)
            .collect();

        let composited = samples.iter().filter(|s| s.composited).count();
        let direct_scanout_pct = if frames > 0 {
            (frames - composited) as f64 / frames as f64 * 100.0
        } else {
            0.0
        };

        let frametime_stats = Stats::of(frametimes.clone());
        // Jank: frames slower than 1.5× the median frametime (felt stutter).
        let jank_threshold = frametime_stats.p50 * 1.5;
        let jank_pct = if !frametimes.is_empty() && jank_threshold > 0.0 {
            frametimes.iter().filter(|&&dt| dt > jank_threshold).count() as f64
                / frametimes.len() as f64
                * 100.0
        } else {
            0.0
        };
        let low_1pct_fps = if frametime_stats.p99 > 0.0 {
            1000.0 / frametime_stats.p99
        } else {
            0.0
        };
        let low_01pct_fps = if frametime_stats.p999 > 0.0 {
            1000.0 / frametime_stats.p999
        } else {
            0.0
        };

        let window_secs = if frames >= 2 {
            samples[frames - 1]
                .present_ns
                .saturating_sub(samples[0].present_ns) as f64
                / 1e9
        } else {
            0.0
        };
        let fps = if window_secs > 0.0 {
            (frames as f64 - 1.0) / window_secs
        } else {
            0.0
        };

        // Approximate the total vblanks in the window as presented frames plus
        // skipped vblanks, so the drop percentage is relative to refresh.
        let total_vblanks = frames as u64 + dropped_vblanks;
        let dropped_pct = if total_vblanks > 0 {
            dropped_vblanks as f64 / total_vblanks as f64 * 100.0
        } else {
            0.0
        };

        let input_samples = latencies.len();
        let input_latency_ms = Stats::of(
            latencies
                .iter()
                .map(|ns| *ns as f64 / 1_000_000.0)
                .collect(),
        );

        OutputFrameStats {
            name,
            frames,
            window_secs,
            fps,
            frametime_ms: frametime_stats,
            render_ms: Stats::of(render_ms),
            elements_ms: Stats::of(elements_ms),
            draw_ms: Stats::of(draw_ms),
            present_latency_ms: Stats::of(present_latency_ms),
            dropped_vblanks,
            drop_events,
            dropped_pct,
            direct_scanout_pct,
            jank_pct,
            low_1pct_fps,
            low_01pct_fps,
            input_latency_ms,
            input_samples,
        }
    }
}

/// A connected output's current mode, for the report header.
struct OutputModeInfo {
    name: String,
    mode: Option<OutputMode>,
}

struct OutputMode {
    width: i32,
    height: i32,
    refresh_hz: f64,
}

/// Per-process memory + CPU, attributed via the kernel.
struct ProcMem {
    pid: i32,
    name: String,
    pss_kb: u64,
    rss_kb: u64,
    gpu_kb: u64,
    cpu_pct: f64,
}

/// System-wide CPU / power / thermal metrics for the report.
#[derive(Default)]
struct SystemMetrics {
    compositor_cpu_pct: f64,
    compositor_threads: usize,
    package_power_w: Option<f64>,
    battery_power_w: Option<f64>,
    energy_per_frame_mj: Option<f64>,
    max_temp_c: Option<f64>,
    gpu_busy_pct: Option<f64>,
    cpu_freq_mhz: Option<f64>,
    cpu_max_freq_mhz: Option<f64>,
}

impl State {
    /// Record a pointer motion event's hardware timestamp for input-latency
    /// measurement. Called from `process_input_event` on the main thread; the
    /// matching present timestamp is paired in the surface thread on vblank.
    /// `hw_time_us` is the libinput event time (CLOCK_MONOTONIC microseconds).
    pub fn record_pointer_latency(&self, hw_time_us: u64) {
        if !is_capturing() {
            return; // no capture in progress — single relaxed load, then nothing
        }
        let hw_ns = hw_time_us.saturating_mul(1000);
        let sched_ns = Duration::from(self.common.clock.now())
            .as_nanos()
            .min(u64::MAX as u128) as u64;
        if let BackendData::Kms(kms) = &self.backend {
            kms.push_pointer_input(hw_ns, sched_ns);
        }
    }

    /// Open the default file manager focused on `path` (freedesktop
    /// `FileManager1.ShowItems` highlights the file; falls back to opening the
    /// containing folder for file managers that don't implement it).
    pub fn reveal_in_file_manager(&mut self, path: &std::path::Path) {
        let socket = self.common.socket.to_string_lossy().into_owned();
        reveal_path(path, &socket);
    }

    /// Arm a UI performance capture (Ctrl+Alt+Super+Shift+F12). Collection is off
    /// until this point — so the instrumentation has no steady-state cost. This
    /// resets the per-output buffers, flips the global `CAPTURING` gate on, shows
    /// the on-screen capture badge, and schedules [`State::finish_capture`] after
    /// the window. The report is built (and revealed) when the window ends.
    pub fn capture_perf_report(&mut self) {
        if is_capturing() {
            tracing::info!("perf: capture already running");
            return;
        }
        // Reset buffers while the gate is still off (no surface-thread writer).
        if let BackendData::Kms(kms) = &self.backend {
            for (_, perf) in kms.perf_snapshots() {
                perf.reset();
            }
        }
        self.common.loop_health.reset();
        set_capturing(true);
        tracing::info!("perf: capture started ({CAPTURE_WINDOW_SECS}s window)");

        // Redraw so the badge appears immediately, then end the window on a timer.
        self.perf_schedule_render_all();
        self.common
            .event_loop_handle
            .insert_source(
                Timer::from_duration(Duration::from_secs(CAPTURE_WINDOW_SECS)),
                |_, _, state| {
                    state.finish_capture();
                    TimeoutAction::Drop
                },
            )
            .ok();
    }

    /// End the capture window: stop collection, hide the badge, and write +
    /// reveal the report on a worker thread (CPU/power sampling needs a window, so
    /// the compositor never blocks while measuring itself).
    fn finish_capture(&mut self) {
        if !is_capturing() {
            return;
        }
        set_capturing(false);
        self.perf_schedule_render_all(); // hide the badge

        let (backend_label, frame_stats): (&'static str, Vec<OutputFrameStats>) =
            match &self.backend {
                BackendData::Kms(kms) => {
                    let stats = kms
                        .perf_snapshots()
                        .into_iter()
                        .map(|(name, perf)| OutputFrameStats::compute(name, &perf))
                        .collect();
                    ("drm/kms", stats)
                }
                BackendData::Winit(_) => (
                    "winit (nested — SYNTHETIC TIMING, NOT AUTHORITATIVE)",
                    Vec::new(),
                ),
                BackendData::X11(_) => (
                    "x11 (nested — SYNTHETIC TIMING, NOT AUTHORITATIVE)",
                    Vec::new(),
                ),
                BackendData::Unset => ("unset", Vec::new()),
            };

        let output_modes: Vec<OutputModeInfo> = {
            let shell = self.common.shell.read();
            shell
                .outputs()
                .map(|o| OutputModeInfo {
                    name: o.name(),
                    mode: o.current_mode().map(|m| OutputMode {
                        width: m.size.w,
                        height: m.size.h,
                        refresh_hz: m.refresh as f64 / 1000.0,
                    }),
                })
                .collect()
        };

        let loop_health = self.common.loop_health.snapshot();
        let socket = self.common.socket.to_string_lossy().into_owned();
        std::thread::Builder::new()
            .name("perf-report".into())
            .spawn(move || {
                match write_report(backend_label, output_modes, frame_stats, loop_health) {
                    Ok(path) => {
                        tracing::info!("UI perf report written to {}", path.display());
                        reveal_path(&path, &socket);
                    }
                    Err(err) => tracing::warn!(?err, "failed to write UI perf report"),
                }
            })
            .ok();
    }

    fn perf_schedule_render_all(&mut self) {
        let outputs: Vec<_> = self.common.shell.read().outputs().cloned().collect();
        for output in outputs {
            self.backend.schedule_render(&output);
        }
    }
}

/// Open the default file manager focused on `path`. Usable from any thread (the
/// F12 report worker and the cold-start path both call it). `FileManager1.ShowItems`
/// highlights the file; falls back to opening the folder.
fn reveal_path(path: &std::path::Path, wayland_socket: &str) {
    let uri = format!("file://{}", path.display());
    let dir = path
        .parent()
        .map(|p| p.display().to_string())
        .unwrap_or_else(|| "/".into());
    let cmd = format!(
        "gdbus call --session --dest org.freedesktop.FileManager1 \
         --object-path /org/freedesktop/FileManager1 \
         --method org.freedesktop.FileManager1.ShowItems \"['{uri}']\" \"\" \
         || xdg-open '{dir}'"
    );
    match std::process::Command::new("/bin/sh")
        .arg("-c")
        .arg(&cmd)
        .env("WAYLAND_DISPLAY", wayland_socket)
        .spawn()
    {
        Ok(mut child) => {
            std::thread::spawn(move || {
                let _ = child.wait();
            });
        }
        Err(err) => tracing::warn!(?err, "perf: failed to open file manager"),
    }
}

/// CPU/energy sampling window (ms). Runs on the worker thread, not the main loop.
const CPU_WINDOW_MS: u64 = 500;
/// Clock ticks per second for `/proc` CPU times. 100 on all common Linux kernels.
const USER_HZ: f64 = 100.0;

/// Scan `/proc`, sample CPU + power over a short window, and write the report.
/// Runs on a worker thread.
fn write_report(
    backend: &str,
    outputs: Vec<OutputModeInfo>,
    frame_stats: Vec<OutputFrameStats>,
    loop_health: LoopHealthSnapshot,
) -> std::io::Result<std::path::PathBuf> {
    // Sample CPU times + energy counters across a short window to get rates.
    let ui_pids = scan_ui_pids();
    let cpu0 = sample_cpu(&ui_pids);
    let energy0 = read_energy_uj();
    let clock = std::time::Instant::now();
    std::thread::sleep(Duration::from_millis(CPU_WINDOW_MS));
    let cpu1 = sample_cpu(&ui_pids);
    let energy1 = read_energy_uj();
    let dt = clock.elapsed().as_secs_f64().max(0.001);

    let mem = build_proc_mem(&ui_pids, &cpu0, &cpu1, dt);
    let system = build_system(&cpu0, &cpu1, dt, energy0, energy1, &frame_stats);

    let unix_secs = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0);
    let home = std::env::var("HOME").unwrap_or_else(|_| "/tmp".into());
    let base = format!("{home}/cosmic-perf-{unix_secs}");

    let hostname = read_trim("/proc/sys/kernel/hostname");
    let kernel = read_trim("/proc/sys/kernel/osrelease");
    let version = env!("CARGO_PKG_VERSION");

    let json = build_json(
        backend,
        &hostname,
        &kernel,
        version,
        unix_secs,
        &outputs,
        &frame_stats,
        &mem,
        &system,
        &loop_health,
    );
    std::fs::write(format!("{base}.json"), json)?;

    let txt = build_txt(
        backend,
        &hostname,
        &kernel,
        version,
        unix_secs,
        &outputs,
        &frame_stats,
        &mem,
        &system,
        &loop_health,
    );
    let path = std::path::PathBuf::from(format!("{base}.txt"));
    std::fs::write(&path, txt)?;

    Ok(path)
}

/// A CPU-time snapshot: total ticks (utime+stime) per UI pid, plus the compositor.
struct CpuSample {
    per_pid: HashMap<i32, u64>,
    compositor: Option<u64>,
}

fn sample_cpu(pids: &[(i32, String)]) -> CpuSample {
    let mut per_pid = HashMap::new();
    for (pid, _) in pids {
        if let Some(t) = read_proc_ticks(&pid.to_string()) {
            per_pid.insert(*pid, t);
        }
    }
    CpuSample {
        per_pid,
        compositor: read_proc_ticks("self"),
    }
}

/// utime+stime (clock ticks) from `/proc/<who>/stat`. Parsed after the last ')'
/// so a comm containing spaces/parens doesn't shift the fields.
fn read_proc_ticks(who: &str) -> Option<u64> {
    let stat = std::fs::read_to_string(format!("/proc/{who}/stat")).ok()?;
    let after = &stat[stat.rfind(')')? + 1..];
    let t: Vec<&str> = after.split_whitespace().collect();
    let utime: u64 = t.get(11)?.parse().ok()?;
    let stime: u64 = t.get(12)?.parse().ok()?;
    Some(utime + stime)
}

fn cpu_pct(t0: Option<u64>, t1: Option<u64>, dt: f64) -> f64 {
    match (t0, t1) {
        (Some(a), Some(b)) if b >= a => (b - a) as f64 / USER_HZ / dt * 100.0,
        _ => 0.0,
    }
}

fn build_proc_mem(
    pids: &[(i32, String)],
    cpu0: &CpuSample,
    cpu1: &CpuSample,
    dt: f64,
) -> Vec<ProcMem> {
    let mut out = Vec::new();
    for (pid, name) in pids {
        let (pss_kb, rss_kb) = read_pss_rss(*pid);
        if pss_kb == 0 && rss_kb == 0 {
            continue; // kernel thread or gone
        }
        out.push(ProcMem {
            pid: *pid,
            name: name.clone(),
            pss_kb,
            rss_kb,
            gpu_kb: read_gpu_kb(*pid),
            cpu_pct: cpu_pct(
                cpu0.per_pid.get(pid).copied(),
                cpu1.per_pid.get(pid).copied(),
                dt,
            ),
        });
    }
    out.sort_by(|a, b| b.pss_kb.cmp(&a.pss_kb));
    out
}

fn build_system(
    cpu0: &CpuSample,
    cpu1: &CpuSample,
    dt: f64,
    energy0: Option<u64>,
    energy1: Option<u64>,
    frame_stats: &[OutputFrameStats],
) -> SystemMetrics {
    let package_power_w = match (energy0, energy1) {
        (Some(a), Some(b)) if b >= a => Some((b - a) as f64 / 1e6 / dt),
        _ => None,
    };
    let max_fps = frame_stats.iter().map(|f| f.fps).fold(0.0_f64, f64::max);
    let energy_per_frame_mj = match package_power_w {
        Some(w) if max_fps > 1.0 => Some(w / max_fps * 1000.0),
        _ => None,
    };
    SystemMetrics {
        compositor_cpu_pct: cpu_pct(cpu0.compositor, cpu1.compositor, dt),
        compositor_threads: std::fs::read_dir("/proc/self/task")
            .map(|d| d.flatten().count())
            .unwrap_or(0),
        package_power_w,
        battery_power_w: read_battery_power_w(),
        energy_per_frame_mj,
        max_temp_c: read_max_temp_c(),
        gpu_busy_pct: read_gpu_busy_pct(),
        cpu_freq_mhz: read_cpu_freq_mhz("scaling_cur_freq"),
        cpu_max_freq_mhz: read_cpu_freq_mhz("cpuinfo_max_freq"),
    }
}

/// Sum of RAPL package energy counters (µJ). `None` if powercap is unavailable.
fn read_energy_uj() -> Option<u64> {
    let mut total = 0u64;
    let mut found = false;
    if let Ok(dir) = std::fs::read_dir("/sys/class/powercap") {
        for e in dir.flatten() {
            let name = e.file_name().to_string_lossy().into_owned();
            // Package zones look like "intel-rapl:0"; subzones "intel-rapl:0:0".
            if name.starts_with("intel-rapl:")
                && name.matches(':').count() == 1
                && let Ok(v) = std::fs::read_to_string(e.path().join("energy_uj"))
                && let Ok(uj) = v.trim().parse::<u64>()
            {
                total += uj;
                found = true;
            }
        }
    }
    found.then_some(total)
}

fn read_battery_power_w() -> Option<f64> {
    if let Ok(dir) = std::fs::read_dir("/sys/class/power_supply") {
        for e in dir.flatten() {
            if let Ok(v) = std::fs::read_to_string(e.path().join("power_now"))
                && let Ok(uw) = v.trim().parse::<i64>()
                && uw != 0
            {
                return Some(uw.unsigned_abs() as f64 / 1e6);
            }
        }
    }
    None
}

fn read_max_temp_c() -> Option<f64> {
    let mut max: Option<f64> = None;
    if let Ok(dir) = std::fs::read_dir("/sys/class/thermal") {
        for e in dir.flatten() {
            if !e.file_name().to_string_lossy().starts_with("thermal_zone") {
                continue;
            }
            if let Ok(v) = std::fs::read_to_string(e.path().join("temp"))
                && let Ok(milli) = v.trim().parse::<i64>()
            {
                let c = milli as f64 / 1000.0;
                if c > 0.0 && c < 200.0 {
                    max = Some(max.map_or(c, |m| m.max(c)));
                }
            }
        }
    }
    max
}

fn read_gpu_busy_pct() -> Option<f64> {
    if let Ok(dir) = std::fs::read_dir("/sys/class/drm") {
        for e in dir.flatten() {
            let name = e.file_name().to_string_lossy().into_owned();
            // cardN, not the cardN-CONNECTOR symlinks.
            if name.starts_with("card")
                && !name.contains('-')
                && let Ok(v) = std::fs::read_to_string(e.path().join("device/gpu_busy_percent"))
                && let Ok(p) = v.trim().parse::<f64>()
            {
                return Some(p);
            }
        }
    }
    None
}

/// Average per-core CPU frequency (MHz) from cpufreq.
fn read_cpu_freq_mhz(file: &str) -> Option<f64> {
    let mut sum = 0.0;
    let mut n = 0u32;
    if let Ok(dir) = std::fs::read_dir("/sys/devices/system/cpu") {
        for e in dir.flatten() {
            let name = e.file_name().to_string_lossy().into_owned();
            if !(name.len() > 3
                && name.starts_with("cpu")
                && name[3..].chars().all(|c| c.is_ascii_digit()))
            {
                continue;
            }
            if let Ok(v) = std::fs::read_to_string(e.path().join(format!("cpufreq/{file}")))
                && let Ok(khz) = v.trim().parse::<f64>()
            {
                sum += khz / 1000.0;
                n += 1;
            }
        }
    }
    (n > 0).then(|| sum / n as f64)
}

pub(crate) fn read_trim(path: &str) -> String {
    std::fs::read_to_string(path)
        .map(|s| s.trim().to_string())
        .unwrap_or_default()
}

pub(crate) fn read_comm(pid: i32) -> Option<String> {
    std::fs::read_to_string(format!("/proc/{pid}/comm"))
        .ok()
        .map(|s| s.trim().to_string())
}

/// PSS + RSS (KiB) from `smaps_rollup`. PSS is the correct way to attribute
/// shared pages across processes.
fn read_pss_rss(pid: i32) -> (u64, u64) {
    let mut pss = 0;
    let mut rss = 0;
    if let Ok(content) = std::fs::read_to_string(format!("/proc/{pid}/smaps_rollup")) {
        for line in content.lines() {
            if let Some(v) = line.strip_prefix("Pss:") {
                pss = parse_first_kb(v);
            } else if let Some(v) = line.strip_prefix("Rss:") {
                rss = parse_first_kb(v);
            }
        }
    }
    (pss, rss)
}

fn parse_first_kb(s: &str) -> u64 {
    s.split_whitespace()
        .next()
        .and_then(|n| n.parse().ok())
        .unwrap_or(0)
}

/// Best-effort resident GPU memory (KiB) from DRM fdinfo, deduplicated by
/// `drm-client-id` per the kernel fdinfo convention so the same GEM objects
/// aren't counted once per open fd. Driver support varies; 0 when unavailable.
fn read_gpu_kb(pid: i32) -> u64 {
    let mut per_client: HashMap<String, u64> = HashMap::new();
    if let Ok(dir) = std::fs::read_dir(format!("/proc/{pid}/fdinfo")) {
        for entry in dir.flatten() {
            let Ok(content) = std::fs::read_to_string(entry.path()) else {
                continue;
            };
            let mut client_id = None;
            let mut resident = 0u64;
            for line in content.lines() {
                if let Some(v) = line.strip_prefix("drm-client-id:") {
                    client_id = Some(v.trim().to_string());
                } else if let Some(v) = line.strip_prefix("drm-resident-memory:") {
                    resident = resident.max(parse_mem_bytes(v));
                } else if let Some(v) = line.strip_prefix("drm-memory-vram:") {
                    resident = resident.max(parse_mem_bytes(v));
                }
            }
            if let Some(id) = client_id {
                let slot = per_client.entry(id).or_insert(0);
                *slot = (*slot).max(resident);
            }
        }
    }
    per_client.values().sum::<u64>() / 1024
}

/// Parse a fdinfo memory value like `1234 KiB` into bytes.
fn parse_mem_bytes(s: &str) -> u64 {
    let mut it = s.split_whitespace();
    let num: u64 = it.next().and_then(|n| n.parse().ok()).unwrap_or(0);
    match it.next().unwrap_or("KiB") {
        "B" => num,
        "MiB" => num * 1024 * 1024,
        "GiB" => num * 1024 * 1024 * 1024,
        _ => num * 1024, // KiB
    }
}

fn scan_ui_pids() -> Vec<(i32, String)> {
    let mut out = Vec::new();
    if let Ok(dir) = std::fs::read_dir("/proc") {
        for entry in dir.flatten() {
            let Some(pid) = entry
                .file_name()
                .to_str()
                .and_then(|s| s.parse::<i32>().ok())
            else {
                continue;
            };
            let Some(comm) = read_comm(pid) else {
                continue;
            };
            if UI_APP_PREFIXES.iter().any(|p| comm.starts_with(p)) {
                out.push((pid, comm));
            }
        }
    }
    out
}

pub(crate) fn json_escape(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            c if (c as u32) < 0x20 => out.push_str(&format!("\\u{:04x}", c as u32)),
            c => out.push(c),
        }
    }
    out
}

fn opt_json(v: Option<f64>) -> String {
    v.map(|x| format!("{x:.3}"))
        .unwrap_or_else(|| "null".into())
}

#[allow(clippy::too_many_arguments)]
fn build_json(
    backend: &str,
    hostname: &str,
    kernel: &str,
    version: &str,
    unix_secs: u64,
    outputs: &[OutputModeInfo],
    frame_stats: &[OutputFrameStats],
    mem: &[ProcMem],
    system: &SystemMetrics,
    loop_health: &LoopHealthSnapshot,
) -> String {
    let mut s = String::new();
    s.push('{');
    s.push_str("\"schema\":\"cosmic-comp.perf/2\",");
    s.push_str(&format!("\"unix_secs\":{unix_secs},"));
    s.push_str(&format!("\"hostname\":\"{}\",", json_escape(hostname)));
    s.push_str(&format!("\"kernel\":\"{}\",", json_escape(kernel)));
    s.push_str(&format!(
        "\"compositor_version\":\"{}\",",
        json_escape(version)
    ));
    s.push_str(&format!("\"backend\":\"{}\",", json_escape(backend)));

    s.push_str("\"outputs\":[");
    for (i, o) in outputs.iter().enumerate() {
        if i > 0 {
            s.push(',');
        }
        match &o.mode {
            Some(m) => s.push_str(&format!(
                "{{\"name\":\"{}\",\"width\":{},\"height\":{},\"refresh_hz\":{:.3}}}",
                json_escape(&o.name),
                m.width,
                m.height,
                m.refresh_hz
            )),
            None => s.push_str(&format!(
                "{{\"name\":\"{}\",\"mode\":null}}",
                json_escape(&o.name)
            )),
        }
    }
    s.push_str("],");

    s.push_str("\"frame_stats\":[");
    for (i, f) in frame_stats.iter().enumerate() {
        if i > 0 {
            s.push(',');
        }
        s.push_str(&format!(
            "{{\"output\":\"{}\",\"frames\":{},\"window_secs\":{:.3},\"fps\":{:.3},\"low_1pct_fps\":{:.3},\"low_0_1pct_fps\":{:.3},\"jank_pct\":{:.4},\"direct_scanout_pct\":{:.3},\"dropped_vblanks\":{},\"drop_events\":{},\"dropped_pct\":{:.4},\"frametime_ms\":{},\"render_ms\":{},\"elements_ms\":{},\"draw_ms\":{},\"present_latency_ms\":{},\"input_samples\":{},\"input_latency_ms\":{}}}",
            json_escape(&f.name),
            f.frames,
            f.window_secs,
            f.fps,
            f.low_1pct_fps,
            f.low_01pct_fps,
            f.jank_pct,
            f.direct_scanout_pct,
            f.dropped_vblanks,
            f.drop_events,
            f.dropped_pct,
            f.frametime_ms.to_json(),
            f.render_ms.to_json(),
            f.elements_ms.to_json(),
            f.draw_ms.to_json(),
            f.present_latency_ms.to_json(),
            f.input_samples,
            f.input_latency_ms.to_json(),
        ));
    }
    s.push_str("],");

    s.push_str("\"memory\":[");
    for (i, p) in mem.iter().enumerate() {
        if i > 0 {
            s.push(',');
        }
        s.push_str(&format!(
            "{{\"pid\":{},\"name\":\"{}\",\"pss_kb\":{},\"rss_kb\":{},\"gpu_kb\":{},\"cpu_pct\":{:.2}}}",
            p.pid,
            json_escape(&p.name),
            p.pss_kb,
            p.rss_kb,
            p.gpu_kb,
            p.cpu_pct,
        ));
    }
    s.push_str("],");

    s.push_str(&format!(
        "\"system\":{{\"compositor_cpu_pct\":{:.2},\"compositor_threads\":{},\"package_power_w\":{},\"battery_power_w\":{},\"energy_per_frame_mj\":{},\"max_temp_c\":{},\"gpu_busy_pct\":{},\"cpu_freq_mhz\":{},\"cpu_max_freq_mhz\":{}}},",
        system.compositor_cpu_pct,
        system.compositor_threads,
        opt_json(system.package_power_w),
        opt_json(system.battery_power_w),
        opt_json(system.energy_per_frame_mj),
        opt_json(system.max_temp_c),
        opt_json(system.gpu_busy_pct),
        opt_json(system.cpu_freq_mhz),
        opt_json(system.cpu_max_freq_mhz),
    ));

    s.push_str(&format!(
        "\"main_loop\":{{\"iterations\":{},\"max_work_ms\":{:.3},\"work_over_16ms\":{},\"active_hitches\":{},\"max_hitch_ms\":{:.3}}}",
        loop_health.iterations,
        loop_health.max_work_ms,
        loop_health.work_over_16ms,
        loop_health.hitch_count,
        loop_health.max_hitch_ms,
    ));

    s.push('}');
    s
}

#[allow(clippy::too_many_arguments)]
fn build_txt(
    backend: &str,
    hostname: &str,
    kernel: &str,
    version: &str,
    unix_secs: u64,
    outputs: &[OutputModeInfo],
    frame_stats: &[OutputFrameStats],
    mem: &[ProcMem],
    system: &SystemMetrics,
    loop_health: &LoopHealthSnapshot,
) -> String {
    let mut s = String::new();
    s.push_str("cosmic-comp UI performance report\n");
    s.push_str("=================================\n");
    s.push_str(&format!("time (unix):  {unix_secs}\n"));
    s.push_str(&format!("hostname:     {hostname}\n"));
    s.push_str(&format!("kernel:       {kernel}\n"));
    s.push_str(&format!("compositor:   {version}\n"));
    s.push_str(&format!("backend:      {backend}\n\n"));

    s.push_str("Outputs\n-------\n");
    for o in outputs {
        match &o.mode {
            Some(m) => s.push_str(&format!(
                "  {}: {}x{} @ {:.3} Hz\n",
                o.name, m.width, m.height, m.refresh_hz
            )),
            None => s.push_str(&format!("  {}: (no mode)\n", o.name)),
        }
    }
    s.push('\n');

    s.push_str("Frame rate / frametime / dropped frames  [ground truth: DRM vblank timestamps + sequence numbers]\n");
    s.push_str("----------------------------------------\n");
    if frame_stats.is_empty() {
        s.push_str("  (no per-output frame data — non-DRM backend or no frames captured)\n");
    }
    for f in frame_stats {
        s.push_str(&format!(
            "  {} — {:.2} fps over {:.1}s ({} frames)\n",
            f.name, f.fps, f.window_secs, f.frames
        ));
        s.push_str(&format!(
            "    fps          : avg {:.1}  1%-low {:.1}  0.1%-low {:.1}  jank {:.2}%\n",
            f.fps, f.low_1pct_fps, f.low_01pct_fps, f.jank_pct,
        ));
        s.push_str(&format!(
            "    frametime ms : p50 {:.2}  p95 {:.2}  p99 {:.2}  p99.9 {:.2}  max {:.2}  (min {:.2}, mean {:.2})\n",
            f.frametime_ms.p50,
            f.frametime_ms.p95,
            f.frametime_ms.p99,
            f.frametime_ms.p999,
            f.frametime_ms.max,
            f.frametime_ms.min,
            f.frametime_ms.mean,
        ));
        s.push_str(&format!(
            "    render ms    : total p50 {:.2} (elements {:.2} + draw {:.2})  present-latency p50 {:.2}\n",
            f.render_ms.p50, f.elements_ms.p50, f.draw_ms.p50, f.present_latency_ms.p50,
        ));
        s.push_str(&format!(
            "    composition  : {:.1}% direct scanout (zero-copy), {:.1}% GPU composite\n",
            f.direct_scanout_pct,
            100.0 - f.direct_scanout_pct,
        ));
        s.push_str(&format!(
            "    dropped      : {} vblanks across {} stalls ({:.3}% of refresh)\n",
            f.dropped_vblanks, f.drop_events, f.dropped_pct,
        ));
        if f.input_samples > 0 {
            s.push_str(&format!(
                "    input→scanout: p50 {:.2}  p95 {:.2}  p99 {:.2}  max {:.2}  ({} pointer samples; + HW offset for end-to-end)\n",
                f.input_latency_ms.p50,
                f.input_latency_ms.p95,
                f.input_latency_ms.p99,
                f.input_latency_ms.max,
                f.input_samples,
            ));
        } else {
            s.push_str("    input→scanout: (no pointer motion captured this window)\n");
        }
    }
    s.push('\n');

    // System: CPU / power / thermal.
    let fmt_opt = |v: Option<f64>, unit: &str| {
        v.map(|x| format!("{x:.1}{unit}"))
            .unwrap_or_else(|| "n/a".into())
    };
    s.push_str("System  [CPU over a 0.5s window; power/thermal best-effort from sysfs]\n");
    s.push_str("------\n");
    s.push_str(&format!(
        "  compositor CPU : {:.1}%  ({} threads)\n",
        system.compositor_cpu_pct, system.compositor_threads,
    ));
    s.push_str(&format!(
        "  package power  : {}   battery draw: {}   energy/frame: {}\n",
        fmt_opt(system.package_power_w, " W"),
        fmt_opt(system.battery_power_w, " W"),
        fmt_opt(system.energy_per_frame_mj, " mJ"),
    ));
    s.push_str(&format!(
        "  thermal        : max {}   gpu busy: {}\n",
        fmt_opt(system.max_temp_c, " °C"),
        fmt_opt(system.gpu_busy_pct, "%"),
    ));
    s.push_str(&format!(
        "  cpu freq       : {} of {} (throttling if current ≪ max under load)\n",
        fmt_opt(system.cpu_freq_mhz, " MHz"),
        fmt_opt(system.cpu_max_freq_mhz, " MHz"),
    ));
    s.push('\n');

    // Main-loop responsiveness.
    s.push_str("Main-loop responsiveness  [freeze detection]\n");
    s.push_str("------------------------\n");
    s.push_str(&format!(
        "  iterations observed : {}\n",
        loop_health.iterations
    ));
    s.push_str(&format!(
        "  per-tick work       : max {:.1} ms  ({} ticks > 16 ms)\n",
        loop_health.max_work_ms, loop_health.work_over_16ms,
    ));
    s.push_str(&format!(
        "  active hitches >50ms: {}  (worst {:.1} ms)\n",
        loop_health.hitch_count, loop_health.max_hitch_ms,
    ));
    s.push('\n');

    let total_pss: u64 = mem.iter().map(|p| p.pss_kb).sum();
    let total_gpu: u64 = mem.iter().map(|p| p.gpu_kb).sum();
    let total_cpu: f64 = mem.iter().map(|p| p.cpu_pct).sum();
    s.push_str(
        "UI memory + CPU  [ground truth: kernel PSS/CPU; GPU best-effort from DRM fdinfo]\n",
    );
    s.push_str("---------------\n");
    s.push_str(&format!(
        "  {:<28} {:>5} {:>10} {:>10} {:>10} {:>8}\n",
        "process", "pid", "PSS MiB", "RSS MiB", "GPU MiB", "CPU %"
    ));
    for p in mem {
        s.push_str(&format!(
            "  {:<28} {:>5} {:>10.1} {:>10.1} {:>10.1} {:>8.1}\n",
            p.name,
            p.pid,
            p.pss_kb as f64 / 1024.0,
            p.rss_kb as f64 / 1024.0,
            p.gpu_kb as f64 / 1024.0,
            p.cpu_pct,
        ));
    }
    s.push_str(&format!(
        "  {:<28} {:>5} {:>10.1} {:>10} {:>10.1} {:>8.1}\n",
        "TOTAL",
        "",
        total_pss as f64 / 1024.0,
        "",
        total_gpu as f64 / 1024.0,
        total_cpu,
    ));
    s.push('\n');

    s.push_str("Notes\n-----\n");
    s.push_str("  * Frame and input metrics are only authoritative on the drm/kms backend.\n");
    s.push_str("  * input→scanout is the compositor-pipeline latency (libinput hardware\n");
    s.push_str("    timestamp → DRM scanout), driven by pointer motion. For end-to-end\n");
    s.push_str("    motion-to-photon, add the one-time hardware-tester offset (USB/firmware\n");
    s.push_str("    before libinput + panel pixel-response after scanout).\n");
    s.push_str("  * Cold-start is captured by a separate relaunch harness (next stage).\n");
    s.push_str("  * See docs/PERF_MEASUREMENT.md for methodology and cross-checks.\n");

    s
}
