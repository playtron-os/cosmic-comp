// SPDX-License-Identifier: GPL-3.0-only

//! Performance profiler for the iced UI element lifecycle.
//!
//! Provides rolling-window statistics on `view()`, `build()`, `draw()`, and
//! message processing for all `IcedElement` instances in the compositor.
//!
//! # Usage
//!
//! Set `COSMIC_ICED_PERF_LOG=1` to enable periodic iced performance reports.
//! Set `COSMIC_ICED_PERF_LOG_INTERVAL=N` to report every N seconds (default: 10).
//! Set `COSMIC_ICED_SLOW_THRESHOLD_MS=N` to warn on updates slower than N ms (default: 2).

use std::collections::HashMap;
use std::sync::{LazyLock, Mutex};
use std::time::{Duration, Instant};

use tracing::warn;

// =============================================================================
// Configuration
// =============================================================================

/// Whether iced perf logging is enabled (env: COSMIC_ICED_PERF_LOG).
pub fn iced_perf_logging_enabled() -> bool {
    static ENABLED: LazyLock<bool> = LazyLock::new(|| {
        std::env::var("COSMIC_ICED_PERF_LOG")
            .map(|v| v == "1" || v.eq_ignore_ascii_case("true"))
            .unwrap_or(false) // disabled by default, opt-in
    });
    *ENABLED
}

/// Per-update slow threshold in milliseconds (env: COSMIC_ICED_SLOW_THRESHOLD_MS).
fn slow_threshold_ms() -> f64 {
    static THRESHOLD: LazyLock<f64> = LazyLock::new(|| {
        std::env::var("COSMIC_ICED_SLOW_THRESHOLD_MS")
            .ok()
            .and_then(|v| v.parse().ok())
            .unwrap_or(2.0)
    });
    *THRESHOLD
}

/// Report interval in seconds (env: COSMIC_ICED_PERF_LOG_INTERVAL).
fn report_interval() -> Duration {
    static INTERVAL: LazyLock<Duration> = LazyLock::new(|| {
        let secs = std::env::var("COSMIC_ICED_PERF_LOG_INTERVAL")
            .ok()
            .and_then(|v| v.parse::<u64>().ok())
            .unwrap_or(10);
        Duration::from_secs(secs)
    });
    *INTERVAL
}

// =============================================================================
// Update Source
// =============================================================================

/// Identifies what triggered an iced element update.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UpdateSource {
    /// Normal refresh cycle (SpaceElement::refresh)
    Refresh,
    /// Pointer/keyboard/touch input handler
    Input,
    /// Animation-driven redraw from render_elements()
    AnimRedraw,
    /// Forced update (resize, set_theme, force_update, constructor)
    Forced,
}

impl UpdateSource {
    pub fn label(self) -> &'static str {
        match self {
            Self::Refresh => "refresh",
            Self::Input => "input",
            Self::AnimRedraw => "anim",
            Self::Forced => "forced",
        }
    }
}

// =============================================================================
// Per-Update Record
// =============================================================================

/// Timing breakdown for a single `IcedElementInternal::update()` call.
#[derive(Debug, Clone)]
pub struct UpdateRecord {
    pub program_name: &'static str,
    pub source: UpdateSource,
    pub view_duration: Duration,
    pub build_duration: Duration,
    pub ui_update_duration: Duration,
    pub draw_duration: Duration,
    /// Time spent in the message-loop second rebuild (0 if no messages produced).
    pub message_loop_duration: Duration,
    pub total_duration: Duration,
    /// Whether widgets produced messages (triggers second view+build+draw).
    pub had_messages: bool,
    /// Whether this update was skipped (empty event queue and not forced).
    pub skipped: bool,
}

// =============================================================================
// Animation Burst Tracking
// =============================================================================

/// Tracks a continuous burst of animation-driven redraws for one element.
#[derive(Debug, Clone)]
struct AnimBurst {
    start: Instant,
    frame_count: u32,
}

// =============================================================================
// Global Profiler
// =============================================================================

/// Global iced profiler instance.
pub static ICED_PROFILER: LazyLock<Mutex<IcedProfiler>> =
    LazyLock::new(|| Mutex::new(IcedProfiler::new()));

pub struct IcedProfiler {
    // Rolling window of update records
    records: Vec<UpdateRecord>,
    last_report: Instant,

    // Aggregate counters (reset each report)
    total_updates: u64,
    total_views: u64,
    skipped_updates: u64,
    updates_with_messages: u64,

    // Per-source counters
    source_counts: HashMap<UpdateSource, u64>,

    // Per-program-type aggregates
    program_totals: HashMap<&'static str, ProgramStats>,

    // Animation burst tracking (key: program name instance addr as string)
    active_bursts: HashMap<usize, AnimBurst>,
    completed_bursts: Vec<(Duration, u32)>, // (duration, frame_count)
}

#[derive(Debug, Clone, Default)]
struct ProgramStats {
    count: u64,
    total_duration: Duration,
    max_duration: Duration,
    view_duration: Duration,
}

impl IcedProfiler {
    fn new() -> Self {
        Self {
            records: Vec::with_capacity(1024),
            last_report: Instant::now(),
            total_updates: 0,
            total_views: 0,
            skipped_updates: 0,
            updates_with_messages: 0,
            source_counts: HashMap::new(),
            program_totals: HashMap::new(),
            active_bursts: HashMap::new(),
            completed_bursts: Vec::new(),
        }
    }

    /// Record a completed update cycle.
    pub fn record_update(&mut self, record: UpdateRecord) {
        let total_ms = record.total_duration.as_secs_f64() * 1000.0;

        self.total_updates += 1;
        if record.skipped {
            self.skipped_updates += 1;
        } else {
            // Count view calls: 1 for normal, 2 if messages triggered rebuild
            self.total_views += 1;
            if record.had_messages {
                self.total_views += 1;
                self.updates_with_messages += 1;
            }
        }

        *self.source_counts.entry(record.source).or_insert(0) += 1;

        // Per-program stats
        let stats = self.program_totals.entry(record.program_name).or_default();
        stats.count += 1;
        stats.total_duration += record.total_duration;
        stats.view_duration += record.view_duration;
        if record.total_duration > stats.max_duration {
            stats.max_duration = record.total_duration;
        }

        // Log slow individual updates
        if !record.skipped && total_ms > slow_threshold_ms() {
            let view_ms = record.view_duration.as_secs_f64() * 1000.0;
            let build_ms = record.build_duration.as_secs_f64() * 1000.0;
            let draw_ms = record.draw_duration.as_secs_f64() * 1000.0;
            let msg_ms = record.message_loop_duration.as_secs_f64() * 1000.0;
            warn!(
                program = record.program_name,
                source = record.source.label(),
                total_ms = format!("{total_ms:.2}"),
                view_ms = format!("{view_ms:.2}"),
                build_ms = format!("{build_ms:.2}"),
                draw_ms = format!("{draw_ms:.2}"),
                msg_loop_ms = format!("{msg_ms:.2}"),
                "SLOW iced update (>{:.1}ms)",
                slow_threshold_ms(),
            );
        }

        self.records.push(record);
    }

    /// Track animation burst start for an element.
    pub fn animation_burst_start(&mut self, element_id: usize) {
        self.active_bursts.entry(element_id).or_insert(AnimBurst {
            start: Instant::now(),
            frame_count: 0,
        });
    }

    /// Track animation burst frame for an element.
    pub fn animation_burst_frame(&mut self, element_id: usize) {
        if let Some(burst) = self.active_bursts.get_mut(&element_id) {
            burst.frame_count += 1;
        }
    }

    /// Track animation burst end for an element.
    pub fn animation_burst_end(&mut self, element_id: usize) {
        if let Some(burst) = self.active_bursts.remove(&element_id) {
            let duration = burst.start.elapsed();
            if burst.frame_count > 0 {
                self.completed_bursts.push((duration, burst.frame_count));
            }
        }
    }

    /// Emit periodic report if interval has elapsed.
    pub fn maybe_report(&mut self) {
        if !iced_perf_logging_enabled() {
            return;
        }

        let now = Instant::now();
        if now.duration_since(self.last_report) < report_interval() {
            return;
        }
        self.last_report = now;

        if self.total_updates == 0 {
            self.reset();
            return;
        }

        let effective_updates = self.total_updates - self.skipped_updates;

        // Phase breakdown from records (only non-skipped)
        let (avg_view, avg_build, avg_draw, avg_total, max_total, p95_total) =
            self.compute_timing_stats();

        // Source breakdown
        let refresh_count = self
            .source_counts
            .get(&UpdateSource::Refresh)
            .copied()
            .unwrap_or(0);
        let input_count = self
            .source_counts
            .get(&UpdateSource::Input)
            .copied()
            .unwrap_or(0);
        let anim_count = self
            .source_counts
            .get(&UpdateSource::AnimRedraw)
            .copied()
            .unwrap_or(0);
        let forced_count = self
            .source_counts
            .get(&UpdateSource::Forced)
            .copied()
            .unwrap_or(0);

        // Animation burst stats
        let burst_count = self.completed_bursts.len();
        let avg_burst_frames = if burst_count > 0 {
            self.completed_bursts
                .iter()
                .map(|(_, f)| *f as f64)
                .sum::<f64>()
                / burst_count as f64
        } else {
            0.0
        };
        let avg_burst_duration_ms = if burst_count > 0 {
            self.completed_bursts
                .iter()
                .map(|(d, _)| d.as_secs_f64() * 1000.0)
                .sum::<f64>()
                / burst_count as f64
        } else {
            0.0
        };

        warn!(
            "┌─── ICED PERF REPORT ({:.0}s window) ───────────────────────────────",
            report_interval().as_secs_f64()
        );
        warn!(
            "│ Updates:  total={:<5} effective={:<5} skipped={:<5} with_messages={}",
            self.total_updates, effective_updates, self.skipped_updates, self.updates_with_messages,
        );
        warn!(
            "│ Views:    total={:<5} avg={:.2}ms  max={:.2}ms  p95={:.2}ms",
            self.total_views,
            avg_view * 1000.0,
            max_total * 1000.0,
            p95_total * 1000.0,
        );
        warn!(
            "│ Phases:   view={:.2}ms  build={:.2}ms  draw={:.2}ms  total_avg={:.2}ms",
            avg_view * 1000.0,
            avg_build * 1000.0,
            avg_draw * 1000.0,
            avg_total * 1000.0,
        );
        warn!(
            "│ Sources:  refresh={:<4} input={:<4} anim_redraw={:<4} forced={}",
            refresh_count, input_count, anim_count, forced_count,
        );

        // Per-program breakdown (top 5 by count)
        let mut programs: Vec<_> = self.program_totals.iter().collect();
        programs.sort_by(|a, b| b.1.count.cmp(&a.1.count));
        for (name, stats) in programs.iter().take(5) {
            let avg_ms = if stats.count > 0 {
                (stats.total_duration.as_secs_f64() / stats.count as f64) * 1000.0
            } else {
                0.0
            };
            let max_ms = stats.max_duration.as_secs_f64() * 1000.0;
            // Shorten the type name for display
            let short_name = name.rsplit("::").next().unwrap_or(name);
            warn!(
                "│ Program:  {:<20} count={:<5} avg={:.2}ms  max={:.2}ms",
                short_name, stats.count, avg_ms, max_ms,
            );
        }

        if burst_count > 0 {
            warn!(
                "│ AnimBursts: count={:<4} avg_frames={:.1}  avg_duration={:.0}ms",
                burst_count, avg_burst_frames, avg_burst_duration_ms,
            );
        }

        warn!("└────────────────────────────────────────────────────────────────");

        self.reset();
    }

    fn compute_timing_stats(&self) -> (f64, f64, f64, f64, f64, f64) {
        let active: Vec<_> = self.records.iter().filter(|r| !r.skipped).collect();
        if active.is_empty() {
            return (0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
        }
        let n = active.len() as f64;

        let sum_view: f64 = active.iter().map(|r| r.view_duration.as_secs_f64()).sum();
        let sum_build: f64 = active.iter().map(|r| r.build_duration.as_secs_f64()).sum();
        let sum_draw: f64 = active.iter().map(|r| r.draw_duration.as_secs_f64()).sum();
        let sum_total: f64 = active.iter().map(|r| r.total_duration.as_secs_f64()).sum();

        let max_total = active
            .iter()
            .map(|r| r.total_duration.as_secs_f64())
            .fold(0.0_f64, f64::max);

        let p95_total = {
            let mut sorted: Vec<f64> = active
                .iter()
                .map(|r| r.total_duration.as_secs_f64())
                .collect();
            sorted.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
            sorted.get(sorted.len() * 95 / 100).copied().unwrap_or(0.0)
        };

        (
            sum_view / n,
            sum_build / n,
            sum_draw / n,
            sum_total / n,
            max_total,
            p95_total,
        )
    }

    fn reset(&mut self) {
        self.records.clear();
        self.total_updates = 0;
        self.total_views = 0;
        self.skipped_updates = 0;
        self.updates_with_messages = 0;
        self.source_counts.clear();
        self.program_totals.clear();
        self.completed_bursts.clear();
    }
}
