// SPDX-License-Identifier: GPL-3.0-only
//! A live account of where the compositor's time goes, for chasing lag on the
//! KMS path. Off unless `COSMIC_COMP_FRAMETRACE=1` is set or
//! `~/.cosmic-comp-frametrace` exists; every hook is then one relaxed load.
//!
//! Every `COSMIC_COMP_FRAMETRACE_SECS` (default 1) it logs `[frametrace]` lines
//! at `warn!`: input handling and how stale events were when the main loop got
//! to them, each output's frames and what asked for them, iced updates and lock
//! waits per program, timed-redraw bookings, and how many log lines went out.
//! Outliers also get a line as they happen, a few per second at most.
//!
//! Nothing here may log while holding [`TRACE`]: the log-counting layer takes
//! its own lock, and a log call under this one would be re-entered from it.

use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering::Relaxed};
use std::sync::{Mutex, Once};
use std::time::{Duration, Instant};

use smithay::output::Output;

static ENABLED: AtomicBool = AtomicBool::new(false);

/// Reads the switch once and starts the reporter. Cheap on every later call.
#[inline]
pub fn enabled() -> bool {
    static ONCE: Once = Once::new();
    ONCE.call_once(|| {
        let on = std::env::var("COSMIC_COMP_FRAMETRACE").is_ok_and(|v| v != "0" && !v.is_empty())
            || std::env::var_os("HOME")
                .map(|home| std::path::Path::new(&home).join(".cosmic-comp-frametrace"))
                .is_some_and(|marker| marker.exists());
        ENABLED.store(on, Relaxed);
        if on {
            report_every(
                std::env::var("COSMIC_COMP_FRAMETRACE_SECS")
                    .ok()
                    .and_then(|v| v.parse().ok())
                    .unwrap_or(1),
            );
        }
    });
    ENABLED.load(Relaxed)
}

/// Above these a single event gets its own line.
const SLOW_INPUT: Duration = Duration::from_millis(8);
const STALE_INPUT: Duration = Duration::from_millis(30);
const SLOW_LOCK: Duration = Duration::from_millis(4);
const SLOW_ICED: Duration = Duration::from_millis(8);
const SLOW_FRAME: Duration = Duration::from_millis(16);
const SLOW_LOOP: Duration = Duration::from_millis(8);
/// Outlier lines per category per report window, so a pathological second
/// cannot turn the trace into the load it is measuring.
const OUTLIER_BUDGET: u32 = 4;

#[derive(Default, Clone, Copy)]
struct Stat {
    n: u64,
    sum: Duration,
    max: Duration,
}

impl Stat {
    fn add(&mut self, d: Duration) {
        self.n += 1;
        self.sum += d;
        self.max = self.max.max(d);
    }

    fn fmt(&self) -> String {
        if self.n == 0 {
            return "-".into();
        }
        format!("{}/{}ms", ms(self.sum / self.n as u32), ms(self.max))
    }
}

fn ms(d: Duration) -> String {
    format!("{:.2}", d.as_secs_f64() * 1000.0)
}

#[derive(Default)]
struct OutputWindow {
    schedules: u64,
    requeue_iced: u64,
    requeue_redraw: u64,
    requeue_anim: u64,
    requeue_other: u64,
    loop_iced: u64,
    loop_anim: u64,
    render: Stat,
    elements: Stat,
    draw: Stat,
    submit: Stat,
    errors: u64,
    presented: u64,
    vblank_gap: Stat,
}

#[derive(Default)]
struct IcedWindow {
    /// Effective updates by source: input, anim, forced, refresh.
    by_source: [u64; 4],
    update: Stat,
    push: Stat,
    wait_main: Stat,
    wait_render: Stat,
    diff: Stat,
    draw: Stat,
    upload: Stat,
    damaged_px: u64,
    damaged_px_max: u64,
    buffer_px: u64,
    full: u64,
}

#[derive(Default)]
struct Window {
    input: Stat,
    input_age: Stat,
    loop_cb: Stat,
    outputs: HashMap<String, OutputWindow>,
    iced: HashMap<&'static str, IcedWindow>,
    booked: u64,
    armed: u64,
    fired: u64,
    due: u64,
    outliers: HashMap<&'static str, u32>,
}

#[derive(Default)]
struct Trace {
    window: Window,
    last_present: HashMap<String, Duration>,
}

static TRACE: Mutex<Option<Trace>> = Mutex::new(None);

fn with<R>(f: impl FnOnce(&mut Trace) -> R) -> R {
    let mut guard = TRACE.lock().unwrap_or_else(|err| err.into_inner());
    f(guard.get_or_insert_with(Trace::default))
}

/// Whether a `category` outlier may still be logged this window.
fn outlier(trace: &mut Trace, category: &'static str) -> bool {
    let spent = trace.window.outliers.entry(category).or_default();
    *spent += 1;
    *spent <= OUTLIER_BUDGET
}

/// A point in time worth finding in the log, such as the palette opening.
pub fn mark(what: impl std::fmt::Display) {
    if enabled() {
        tracing::warn!("[frametrace] mark: {what}");
    }
}

/// One input event, `handled` on the main thread `age` after the hardware saw it.
pub fn input(kind: &'static str, handled: Duration, age: Option<Duration>) {
    if !enabled() {
        return;
    }
    let report = with(|trace| {
        trace.window.input.add(handled);
        if let Some(age) = age {
            trace.window.input_age.add(age);
        }
        let slow = handled > SLOW_INPUT || age.is_some_and(|age| age > STALE_INPUT);
        slow && outlier(trace, "input")
    });
    if report {
        tracing::warn!(
            "[frametrace] slow input: {kind} handled in {}ms, {}ms after the hardware saw it",
            ms(handled),
            age.map_or("?".into(), ms)
        );
    }
}

/// One pass of the main loop's post-dispatch callback.
pub fn loop_callback(took: Duration) {
    if !enabled() {
        return;
    }
    let report = with(|trace| {
        trace.window.loop_cb.add(took);
        took > SLOW_LOOP && outlier(trace, "loop")
    });
    if report {
        tracing::warn!("[frametrace] slow main-loop callback: {}ms", ms(took));
    }
}

/// The main thread asked `output` for a frame, for any reason.
pub fn schedule(output: &Output) {
    if enabled() {
        let name = output.name();
        with(|trace| output_window(trace, &name).schedules += 1);
    }
}

/// The main loop scheduled `output` for an animation or an iced request.
pub fn loop_scheduled(output: &Output, iced: bool, anim: bool) {
    if enabled() && (iced || anim) {
        let name = output.name();
        with(|trace| {
            let window = output_window(trace, &name);
            window.loop_iced += u64::from(iced);
            window.loop_anim += u64::from(anim);
        });
    }
}

/// Why a vblank on `output` queued another frame.
pub fn requeue(output: &Output, iced: bool, redraw: bool, anim: bool, other: bool) {
    if !enabled() {
        return;
    }
    let name = output.name();
    with(|trace| {
        let window = output_window(trace, &name);
        window.requeue_iced += u64::from(iced);
        window.requeue_redraw += u64::from(redraw);
        window.requeue_anim += u64::from(anim);
        window.requeue_other += u64::from(other);
    });
}

/// A frame rendered on `output`, with its phases.
pub fn frame(
    output: &Output,
    total: Duration,
    elements: Duration,
    draw: Duration,
    submit: Duration,
    element_count: usize,
) {
    if !enabled() {
        return;
    }
    let output = output.name();
    let report = with(|trace| {
        let window = output_window(trace, &output);
        window.render.add(total);
        window.elements.add(elements);
        window.draw.add(draw);
        window.submit.add(submit);
        total > SLOW_FRAME && outlier(trace, "frame")
    });
    if report {
        tracing::warn!(
            "[frametrace] slow frame on {output}: {}ms (elements {}ms, draw {}ms, submit {}ms, {element_count} elements)",
            ms(total),
            ms(elements),
            ms(draw),
            ms(submit)
        );
    }
}

pub fn frame_error(output: &Output) {
    if enabled() {
        let name = output.name();
        with(|trace| output_window(trace, &name).errors += 1);
    }
}

/// A frame reached the screen on `output` at `at` (CLOCK_MONOTONIC).
pub fn presented(output: &Output, at: Option<Duration>) {
    if !enabled() {
        return;
    }
    let name = output.name();
    with(|trace| {
        let gap = at.and_then(|at| {
            let previous = trace.last_present.insert(name.clone(), at)?;
            at.checked_sub(previous)
        });
        let window = output_window(trace, &name);
        window.presented += 1;
        // A gap over a second is an idle stretch, not a slow frame.
        if let Some(gap) = gap.filter(|gap| *gap < Duration::from_secs(1)) {
            window.vblank_gap.add(gap);
        }
    });
}

fn output_window<'a>(trace: &'a mut Trace, output: &str) -> &'a mut OutputWindow {
    if !trace.window.outputs.contains_key(output) {
        trace
            .window
            .outputs
            .insert(output.to_owned(), OutputWindow::default());
    }
    trace.window.outputs.get_mut(output).unwrap()
}

/// An effective iced update of `program`, from `source` (its profiler label).
pub fn iced_update(program: &'static str, source: &'static str, took: Duration) {
    if !enabled() {
        return;
    }
    let report = with(|trace| {
        let window = trace.window.iced.entry(program).or_default();
        let slot = match source {
            "input" => 0,
            "anim" => 1,
            "forced" => 2,
            _ => 3,
        };
        window.by_source[slot] += 1;
        window.update.add(took);
        took > SLOW_ICED && outlier(trace, "iced")
    });
    if report {
        tracing::warn!(
            "[frametrace] slow iced update: {program} ({source}) {}ms",
            ms(took)
        );
    }
}

/// One rasterisation of an iced element's buffer, and the upload of what changed.
#[derive(Default)]
pub struct Raster {
    /// Diffing the new layers against the last frame's for damage.
    pub diff: Duration,
    /// tiny-skia drawing the damaged part.
    pub draw: Duration,
    /// Building the render element, which uploads the damage to a texture.
    pub upload: Duration,
    pub damaged_px: u64,
    pub buffer_px: u64,
}

pub fn iced_raster(program: &'static str, raster: Raster) {
    if !enabled() {
        return;
    }
    with(|trace| {
        let window = trace.window.iced.entry(program).or_default();
        window.diff.add(raster.diff);
        window.draw.add(raster.draw);
        window.upload.add(raster.upload);
        window.damaged_px += raster.damaged_px;
        window.damaged_px_max = window.damaged_px_max.max(raster.damaged_px);
        window.buffer_px = raster.buffer_px;
        // Near-whole-buffer damage means the diff found nothing to keep.
        window.full += u64::from(raster.damaged_px * 10 >= raster.buffer_px * 9);
    });
}

/// `program`'s render-element pass, which rasterises under the element lock.
pub fn iced_push(program: &'static str, took: Duration) {
    if !enabled() {
        return;
    }
    let report = with(|trace| {
        trace.window.iced.entry(program).or_default().push.add(took);
        took > SLOW_ICED && outlier(trace, "push")
    });
    if report {
        tracing::warn!(
            "[frametrace] slow iced render pass: {program} {}ms",
            ms(took)
        );
    }
}

/// Time spent waiting for `program`'s element lock.
pub fn lock_wait(program: &'static str, main_thread: bool, waited: Duration) {
    if !enabled() {
        return;
    }
    let report = with(|trace| {
        let window = trace.window.iced.entry(program).or_default();
        if main_thread {
            window.wait_main.add(waited);
        } else {
            window.wait_render.add(waited);
        }
        waited > SLOW_LOCK && outlier(trace, "lock")
    });
    if report {
        let thread = if main_thread { "main" } else { "render" };
        tracing::warn!(
            "[frametrace] {thread} thread waited {}ms for {program}'s lock",
            ms(waited)
        );
    }
}

/// Timed-redraw bookkeeping: a booking, a timer armed or fired, a due frame.
pub enum Booking {
    Booked,
    Armed,
    Fired,
    Due,
}

pub fn booking(event: Booking) {
    if enabled() {
        with(|trace| match event {
            Booking::Booked => trace.window.booked += 1,
            Booking::Armed => trace.window.armed += 1,
            Booking::Fired => trace.window.fired += 1,
            Booking::Due => trace.window.due += 1,
        });
    }
}

static LOG_LINES: AtomicU64 = AtomicU64::new(0);
static LOG_TARGETS: Mutex<Option<HashMap<&'static str, u64>>> = Mutex::new(None);

/// Counts the log lines that pass the filter, by target: under `run-debug`
/// every line is formatted three times and piped to a terminal, synchronously
/// on whichever thread logged it.
pub struct LogCounter;

impl<S: tracing::Subscriber> tracing_subscriber::Layer<S> for LogCounter {
    fn on_event(
        &self,
        event: &tracing::Event<'_>,
        _ctx: tracing_subscriber::layer::Context<'_, S>,
    ) {
        if !enabled() {
            return;
        }
        LOG_LINES.fetch_add(1, Relaxed);
        let mut targets = LOG_TARGETS.lock().unwrap_or_else(|err| err.into_inner());
        *targets
            .get_or_insert_with(HashMap::new)
            .entry(event.metadata().target())
            .or_default() += 1;
    }
}

fn report_every(secs: u64) {
    let secs = secs.max(1);
    let _ = std::thread::Builder::new()
        .name("frametrace-report".into())
        .spawn(move || {
            let start = Instant::now();
            tracing::warn!("[frametrace] tracing; a report follows every {secs}s");
            loop {
                std::thread::sleep(Duration::from_secs(secs));
                let window = with(|trace| std::mem::take(&mut trace.window));
                let lines = LOG_LINES.swap(0, Relaxed);
                let targets = LOG_TARGETS
                    .lock()
                    .unwrap_or_else(|err| err.into_inner())
                    .take()
                    .unwrap_or_default();
                report(start.elapsed(), secs, window, lines, targets);
            }
        });
}

fn report(
    elapsed: Duration,
    secs: u64,
    window: Window,
    lines: u64,
    targets: HashMap<&'static str, u64>,
) {
    let t = format!("t={:.0}s", elapsed.as_secs_f64());
    let mut quiet = true;

    if window.input.n > 0 || window.loop_cb.n > 0 {
        quiet = false;
        tracing::warn!(
            "[frametrace] {t} input n={} handle avg/max={} age avg/max={} | loop-callback n={} avg/max={}",
            window.input.n,
            window.input.fmt(),
            window.input_age.fmt(),
            window.loop_cb.n,
            window.loop_cb.fmt(),
        );
    }

    let mut outputs: Vec<_> = window.outputs.into_iter().collect();
    outputs.sort_by(|a, b| a.0.cmp(&b.0));
    for (name, o) in outputs {
        quiet = false;
        tracing::warn!(
            "[frametrace] {t} out={name} frames={} err={} presented={} vblank-gap avg/max={} render avg/max={} (elements {} draw {} submit {}) | schedules={} loop: iced={} anim={} | requeue: iced={} redraw={} anim={} other={}",
            o.render.n,
            o.errors,
            o.presented,
            o.vblank_gap.fmt(),
            o.render.fmt(),
            o.elements.fmt(),
            o.draw.fmt(),
            o.submit.fmt(),
            o.schedules,
            o.loop_iced,
            o.loop_anim,
            o.requeue_iced,
            o.requeue_redraw,
            o.requeue_anim,
            o.requeue_other,
        );
    }

    let mut iced: Vec<_> = window.iced.into_iter().collect();
    iced.sort_by(|a, b| b.1.update.sum.cmp(&a.1.update.sum));
    for (program, i) in iced {
        if i.update.n == 0 && i.push.n == 0 && i.wait_main.max < SLOW_LOCK / 4 {
            continue;
        }
        quiet = false;
        tracing::warn!(
            "[frametrace] {t} iced {program}: updates={} (input={} anim={} forced={} refresh={}) avg/max={} | render-pass n={} avg/max={} | lock-wait main={} render={}",
            i.update.n,
            i.by_source[0],
            i.by_source[1],
            i.by_source[2],
            i.by_source[3],
            i.update.fmt(),
            i.push.n,
            i.push.fmt(),
            i.wait_main.fmt(),
            i.wait_render.fmt(),
        );
        if i.draw.n > 0 {
            tracing::warn!(
                "[frametrace] {t} raster {program}: n={} full={} diff avg/max={} draw avg/max={} upload avg/max={} | damaged avg/max={}/{}kpx of {}kpx",
                i.draw.n,
                i.full,
                i.diff.fmt(),
                i.draw.fmt(),
                i.upload.fmt(),
                i.damaged_px / i.draw.n / 1000,
                i.damaged_px_max / 1000,
                i.buffer_px / 1000,
            );
        }
    }

    if window.booked + window.armed + window.fired + window.due > 0 {
        quiet = false;
        tracing::warn!(
            "[frametrace] {t} redraw-at booked={} armed={} fired={} due={}",
            window.booked,
            window.armed,
            window.fired,
            window.due
        );
    }

    // The reporter's own lines are part of the count; leave them out.
    let own = targets.get(module_path!()).copied().unwrap_or(0);
    let lines = lines.saturating_sub(own);
    if lines > 0 {
        let mut top: Vec<_> = targets
            .into_iter()
            .filter(|(target, _)| *target != module_path!())
            .collect();
        top.sort_by(|a, b| b.1.cmp(&a.1));
        let top: Vec<String> = top
            .iter()
            .take(5)
            .map(|(target, n)| format!("{target}={}", n / secs))
            .collect();
        tracing::warn!(
            "[frametrace] {t} log lines={}/s top: {}",
            lines / secs,
            top.join(" ")
        );
    } else if quiet {
        tracing::warn!("[frametrace] {t} idle");
    }
}
