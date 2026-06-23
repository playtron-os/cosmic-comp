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
        atomic::{AtomicU64, Ordering},
    },
    time::Duration,
};

use smithay::utils::{Monotonic, Time};

use crate::state::{BackendData, State};

pub mod coldstart;

/// How many recent presented frames to retain per output (~16 s at 60 Hz).
/// Written by the surface thread on every vblank, read by the main thread when a
/// report is requested.
const SAMPLE_CAP: usize = 1000;

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
    /// Wake→submit ("draw") duration for the frame, nanoseconds.
    render_ns: u64,
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
    pub fn record(&self, present: Time<Monotonic>, render: Duration, seq_delta: Option<i64>) {
        let present_ns = Duration::from(present).as_nanos().min(u64::MAX as u128) as u64;
        let render_ns = render.as_nanos().min(u64::MAX as u128) as u64;

        {
            let mut samples = self.samples.lock().unwrap();
            samples.push_back(FrameSample {
                present_ns,
                render_ns,
            });
            while samples.len() > SAMPLE_CAP {
                samples.pop_front();
            }
        }

        if let Some(delta) = seq_delta
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

/// Computed per-output frame statistics for the report.
#[derive(Debug, Clone)]
struct OutputFrameStats {
    name: String,
    frames: usize,
    window_secs: f64,
    fps: f64,
    frametime_ms: Stats,
    render_ms: Stats,
    dropped_vblanks: u64,
    drop_events: u64,
    dropped_pct: f64,
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
            frametime_ms: Stats::of(frametimes),
            render_ms: Stats::of(render_ms),
            dropped_vblanks,
            drop_events,
            dropped_pct,
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

/// Per-process memory, attributed via the kernel.
struct ProcMem {
    pid: i32,
    name: String,
    pss_kb: u64,
    rss_kb: u64,
    gpu_kb: u64,
}

impl State {
    /// Record a pointer motion event's hardware timestamp for input-latency
    /// measurement. Called from `process_input_event` on the main thread; the
    /// matching present timestamp is paired in the surface thread on vblank.
    /// `hw_time_us` is the libinput event time (CLOCK_MONOTONIC microseconds).
    pub fn record_pointer_latency(&self, hw_time_us: u64) {
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
        self.spawn_command(cmd);
    }

    /// Capture a UI performance report, write it to `$HOME`, and reveal it in the
    /// file manager. Invoked from the Ctrl+Alt+Super+Shift+F12 handler. Runs
    /// synchronously on the main thread — a brief one-off cost on the key press;
    /// the frame data it reports was already collected before this point, so that
    /// cost does not affect the reported numbers.
    pub fn capture_perf_report(&mut self) {
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

        match write_report(backend_label, output_modes, frame_stats) {
            Ok(path) => {
                tracing::info!("UI perf report written to {}", path.display());
                self.reveal_in_file_manager(&path);
            }
            Err(err) => tracing::warn!(?err, "failed to write UI perf report"),
        }
    }
}

/// Scan `/proc` for UI processes and write the JSON + human-readable report.
/// Runs on a worker thread.
fn write_report(
    backend: &str,
    outputs: Vec<OutputModeInfo>,
    frame_stats: Vec<OutputFrameStats>,
) -> std::io::Result<std::path::PathBuf> {
    let mem = scan_ui_processes();

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
    );
    let path = std::path::PathBuf::from(format!("{base}.txt"));
    std::fs::write(&path, txt)?;

    Ok(path)
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

fn scan_ui_processes() -> Vec<ProcMem> {
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
            if !UI_APP_PREFIXES.iter().any(|p| comm.starts_with(p)) {
                continue;
            }
            let (pss_kb, rss_kb) = read_pss_rss(pid);
            if pss_kb == 0 && rss_kb == 0 {
                continue; // kernel thread or gone
            }
            out.push(ProcMem {
                pid,
                name: comm,
                pss_kb,
                rss_kb,
                gpu_kb: read_gpu_kb(pid),
            });
        }
    }
    out.sort_by(|a, b| b.pss_kb.cmp(&a.pss_kb));
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
) -> String {
    let mut s = String::new();
    s.push('{');
    s.push_str("\"schema\":\"cosmic-comp.perf/1\",");
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
            "{{\"output\":\"{}\",\"frames\":{},\"window_secs\":{:.3},\"fps\":{:.3},\"dropped_vblanks\":{},\"drop_events\":{},\"dropped_pct\":{:.4},\"frametime_ms\":{},\"render_ms\":{},\"input_samples\":{},\"input_latency_ms\":{}}}",
            json_escape(&f.name),
            f.frames,
            f.window_secs,
            f.fps,
            f.dropped_vblanks,
            f.drop_events,
            f.dropped_pct,
            f.frametime_ms.to_json(),
            f.render_ms.to_json(),
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
            "{{\"pid\":{},\"name\":\"{}\",\"pss_kb\":{},\"rss_kb\":{},\"gpu_kb\":{}}}",
            p.pid,
            json_escape(&p.name),
            p.pss_kb,
            p.rss_kb,
            p.gpu_kb
        ));
    }
    s.push(']');

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
            "    render ms    : p50 {:.2}  p95 {:.2}  p99 {:.2}  max {:.2}\n",
            f.render_ms.p50, f.render_ms.p95, f.render_ms.p99, f.render_ms.max,
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

    let total_pss: u64 = mem.iter().map(|p| p.pss_kb).sum();
    let total_gpu: u64 = mem.iter().map(|p| p.gpu_kb).sum();
    s.push_str("UI memory  [ground truth: kernel PSS; GPU best-effort from DRM fdinfo]\n");
    s.push_str("---------\n");
    s.push_str(&format!(
        "  {:<28} {:>5} {:>10} {:>10} {:>10}\n",
        "process", "pid", "PSS MiB", "RSS MiB", "GPU MiB"
    ));
    for p in mem {
        s.push_str(&format!(
            "  {:<28} {:>5} {:>10.1} {:>10.1} {:>10.1}\n",
            p.name,
            p.pid,
            p.pss_kb as f64 / 1024.0,
            p.rss_kb as f64 / 1024.0,
            p.gpu_kb as f64 / 1024.0,
        ));
    }
    s.push_str(&format!(
        "  {:<28} {:>5} {:>10.1} {:>10} {:>10.1}\n",
        "TOTAL",
        "",
        total_pss as f64 / 1024.0,
        "",
        total_gpu as f64 / 1024.0,
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
