use std::{collections::VecDeque, num::NonZeroU64, time::Duration};

use smithay::{
    backend::drm::DrmNode,
    utils::{Clock, Monotonic, Time},
};
use tracing::{debug, error};

const BASE_SAFETY_MARGIN: Duration = Duration::from_millis(3);
const SAMPLE_TIME_WINDOW: usize = 5;

// Frame-pacing constants for the predictive late-wake vblank scheduler, whose
// late-wake design is the latency moat that makes a fullscreen game feel native.
//
// The scheduler wakes `rolling_draw_time + RED_ZONE` before the predicted
// vblank. `rolling_draw_time` is a slow EMA of measured wake→submit time that
// *snaps up instantly* on a spike (so one slow frame can't make us miss the
// next vblank) and decays back down at 2%/frame.

/// Red zone: fixed slack added on top of the draw-time estimate (1.65 ms).
const VBLANK_RED_ZONE_NS: u64 = 1_650_000;
/// EMA decay numerator — 98% of the old estimate is retained each frame.
const VBLANK_RATE_OF_DECAY: u64 = 980;
/// EMA decay denominator.
const VBLANK_RATE_OF_DECAY_MAX: u64 = 1000;
/// Seed/idle draw-time estimate (3 ms).
const VBLANK_STARTING_DRAW_TIME_NS: u64 = 3_000_000;
/// Minimum draw time charged while compositing (2.4 ms). Avoids a GPU-clock
/// feedback loop where scheduling tight against vblank lets DVFS down-clock,
/// making the next composite slower. Skipped on the pure direct-scanout path
/// (e.g. a fullscreen game in game mode) so that path gets the tightest pacing.
const VBLANK_COMPOSITING_FLOOR_NS: u64 = 2_400_000;
/// Under VRR we don't predict-ahead by draw time; we wake a flat 0.3 ms before
/// the nominal (max-rate) vblank and let the variable-refresh panel absorb the
/// rest.
const VRR_FLUSHING_TIME_NS: u64 = 300_000;

/// Sawtooth-EMA update of the rolling draw-time estimate, extracted as a pure
/// function so the exact math can be unit-tested.
fn roll_draw_time(
    current_ns: u64,
    draw_ns: u64,
    compositing: bool,
    refresh_interval_ns: Option<u64>,
) -> u64 {
    // Compositing floor is applied to the input before the average.
    let draw_ns = if compositing {
        draw_ns.max(VBLANK_COMPOSITING_FLOOR_NS)
    } else {
        draw_ns
    };

    let new = if draw_ns.saturating_sub(VBLANK_RED_ZONE_NS / 2) > current_ns {
        // The measured draw time overshot the estimate by more than half the
        // red zone — snap straight up to it (sawtooth), don't ease.
        draw_ns
    } else {
        // Otherwise ease: 98% old + 2% new.
        (VBLANK_RATE_OF_DECAY * current_ns
            + (VBLANK_RATE_OF_DECAY_MAX - VBLANK_RATE_OF_DECAY) * draw_ns)
            / VBLANK_RATE_OF_DECAY_MAX
    };

    // Never offset by more than (refresh interval − red zone).
    match refresh_interval_ns {
        Some(interval) => new.min(interval.saturating_sub(VBLANK_RED_ZONE_NS)),
        None => new,
    }
}

/// Game-mode fps cap: raise `to_next_ns` (the time from the last presentation to
/// the next one) so presentations are at least `1/fps_limit` apart. On a fixed
/// refresh the minimum is rounded up to a whole number of vblank intervals (so
/// it stays vblank-aligned); under VRR the exact interval is used. `fps_limit`
/// of 0 means uncapped. Pure function so the rounding can be unit-tested.
fn cap_to_fps(to_next_ns: u64, fps_limit: u32, refresh_interval_ns: u64, vrr: bool) -> u64 {
    if fps_limit == 0 || refresh_interval_ns == 0 {
        return to_next_ns;
    }
    let fps_limit = fps_limit as u64;
    let min_ns = if vrr {
        // VRR can present at any time, so honor the exact interval.
        1_000_000_000 / fps_limit
    } else {
        // Fixed refresh: present every Nth vblank, N = ceil(refresh_hz / fps), so
        // the rate never exceeds the requested cap. Computed from the integer
        // refresh *rate* (not the ns interval) to avoid truncation error at exact
        // submultiples — e.g. a truncated 16_666_666 ns would otherwise push a
        // 20 fps cap to 4 vblanks (15 fps) instead of 3 (20 fps).
        let refresh_hz = (1_000_000_000 + refresh_interval_ns / 2) / refresh_interval_ns;
        refresh_hz.div_ceil(fps_limit).max(1) * refresh_interval_ns
    };
    to_next_ns.max(min_ns)
}

pub struct Timings {
    refresh_interval_ns: Option<NonZeroU64>,
    min_refresh_interval_ns: Option<NonZeroU64>,
    vrr: bool,
    vendor: Option<u32>,

    /// The spike-robust EMA of wake→submit time that drives how early we wake
    /// before vblank. Seeded at 3 ms.
    rolling_draw_time_ns: u64,
    /// Whether the last frame was GPU-composited (vs a pure direct scanout).
    /// Gates the compositing draw-time floor.
    compositing: bool,
    /// Game-mode frame-rate cap (0 = uncapped). When set, the presentation rate
    /// is capped to this rate by scheduling the next presentation no sooner than
    /// `1/fps_limit` after the last — so the output keeps ticking at the target
    /// rate (unlike dropping frame callbacks, which stalls a damage-driven
    /// compositor).
    fps_limit: u32,

    pub pending_frame: Option<PendingFrame>,
    pub previous_frames: VecDeque<Frame>,
}

#[derive(Debug)]
pub struct PendingFrame {
    render_start: Time<Monotonic>,
    render_duration_elements: Option<Duration>,
    render_duration_draw: Option<Duration>,
    presentation_submitted: Option<Time<Monotonic>>,
}

#[derive(Debug)]
pub struct Frame {
    pub render_start: Time<Monotonic>,
    pub render_duration_elements: Duration,
    pub render_duration_draw: Duration,
    pub presentation_submitted: Time<Monotonic>,
    pub presentation_presented: Time<Monotonic>,
}

impl Frame {
    fn render_time(&self) -> Duration {
        self.render_duration_elements + self.render_duration_draw
    }

    fn submit_time(&self) -> Duration {
        Time::elapsed(&self.render_start, self.presentation_submitted)
    }

    fn frame_time(&self) -> Duration {
        Time::elapsed(&self.render_start, self.presentation_presented)
    }
}

impl Timings {
    const CLEANUP: usize = 360;

    pub fn new(
        refresh_interval: Option<Duration>,
        min_interval: Option<Duration>,
        vrr: bool,
        node: DrmNode,
    ) -> Self {
        let refresh_interval_ns = if let Some(interval) = &refresh_interval {
            assert_eq!(interval.as_secs(), 0);
            Some(NonZeroU64::new(interval.subsec_nanos().into()).unwrap())
        } else {
            None
        };

        let min_refresh_interval_ns = if let Some(interval) = &min_interval {
            assert_eq!(interval.as_secs(), 0);
            Some(NonZeroU64::new(interval.subsec_nanos().into()).unwrap())
        } else {
            None
        };

        let vendor = if let Ok(vendor) = std::fs::read_to_string(format!(
            "/sys/class/drm/renderD{}/device/vendor",
            node.minor()
        )) {
            u32::from_str_radix(&vendor.trim()[2..], 16).ok()
        } else {
            None
        };

        Self {
            refresh_interval_ns,
            min_refresh_interval_ns,
            vrr,
            vendor,

            rolling_draw_time_ns: VBLANK_STARTING_DRAW_TIME_NS,
            compositing: true,
            fps_limit: 0,

            pending_frame: None,
            previous_frames: VecDeque::new(),
        }
    }

    pub fn refresh_interval(&self) -> Duration {
        match self.refresh_interval_ns {
            Some(ns) => Duration::from_nanos(ns.get()),
            None => Duration::ZERO,
        }
    }

    pub fn set_refresh_interval(&mut self, interval: Option<Duration>) {
        self.refresh_interval_ns = interval
            .map(|duration| duration.subsec_nanos() as u64)
            .and_then(NonZeroU64::new);

        self.previous_frames.clear();
        self.rolling_draw_time_ns = VBLANK_STARTING_DRAW_TIME_NS;
    }

    /// Record whether the last frame was GPU-composited (vs direct scanout), so
    /// the pacer can skip the compositing draw-time floor on the scanout path.
    pub fn set_compositing(&mut self, compositing: bool) {
        self.compositing = compositing;
    }

    /// Whether the last submitted frame was GPU-composited (vs direct scanout).
    pub fn compositing(&self) -> bool {
        self.compositing
    }

    /// Set the game-mode frame-rate cap (0 = uncapped).
    pub fn set_fps_limit(&mut self, fps_limit: u32) {
        self.fps_limit = fps_limit;
    }

    pub fn fps_limit(&self) -> u32 {
        self.fps_limit
    }

    /// The pacer's current rolling draw-time estimate (≈ how early we wake before
    /// vblank). Exposed for the frame-pacing instrumentation log.
    pub fn rolling_draw_time(&self) -> Duration {
        Duration::from_nanos(self.rolling_draw_time_ns)
    }

    pub fn set_min_refresh_interval(&mut self, min_interval: Option<Duration>) {
        self.min_refresh_interval_ns = min_interval
            .map(|duration| duration.subsec_nanos() as u64)
            .and_then(NonZeroU64::new);
    }

    pub fn set_vrr(&mut self, vrr: bool) {
        self.vrr = vrr;
    }

    pub fn vrr(&self) -> bool {
        self.vrr
    }

    pub fn start_render(&mut self, clock: &Clock<Monotonic>) {
        self.pending_frame = Some(PendingFrame {
            render_start: clock.now(),
            render_duration_elements: None,
            render_duration_draw: None,
            presentation_submitted: None,
        });
    }

    pub fn elements_done(&mut self, clock: &Clock<Monotonic>) {
        if let Some(frame) = self.pending_frame.as_mut() {
            frame.render_duration_elements = Some(Time::elapsed(&frame.render_start, clock.now()));
        }
    }

    pub fn draw_done(&mut self, clock: &Clock<Monotonic>) {
        if let Some(frame) = self.pending_frame.as_mut() {
            frame.render_duration_draw = Some(
                Time::elapsed(&frame.render_start, clock.now())
                    - frame.render_duration_elements.unwrap_or(Duration::ZERO),
            );
        }
    }

    pub fn submitted_for_presentation(&mut self, clock: &Clock<Monotonic>) {
        if let Some(frame) = self.pending_frame.as_mut() {
            frame.presentation_submitted = Some(clock.now());
        }
    }

    pub fn presented(&mut self, value: Time<Monotonic>) {
        if let Some(frame) = self.pending_frame.take() {
            let new_frame = Frame {
                render_start: frame.render_start,
                render_duration_elements: frame.render_duration_elements.unwrap_or_default(),
                render_duration_draw: frame.render_duration_draw.unwrap_or_default(),
                presentation_submitted: frame.presentation_submitted.unwrap(),
                presentation_presented: value,
            };
            if new_frame.render_start > new_frame.presentation_submitted {
                debug!(
                    "frame time overflowed: {}",
                    new_frame.frame_time().as_millis()
                );
            }

            // Feed the rolling draw-time estimator with this frame's wake→submit
            // time (our measured "draw time").
            let draw_ns = new_frame.submit_time().as_nanos().min(u64::MAX as u128) as u64;
            self.rolling_draw_time_ns = roll_draw_time(
                self.rolling_draw_time_ns,
                draw_ns,
                self.compositing,
                self.refresh_interval_ns.map(NonZeroU64::get),
            );

            self.previous_frames.push_back(new_frame);

            if let Some(overflow) = self.previous_frames.len().checked_sub(Self::CLEANUP * 2) {
                self.previous_frames = self.previous_frames.split_off(overflow + Self::CLEANUP);
            }
        }
    }

    pub fn discard_current_frame(&mut self) {
        let _ = self.pending_frame.take();
    }

    pub fn max_rendertime(&self) -> Duration {
        self.previous_frames
            .iter()
            .map(|f| f.render_time())
            .max()
            .unwrap_or(Duration::ZERO)
    }

    pub fn min_rendertime(&self) -> Duration {
        self.previous_frames
            .iter()
            .map(|f| f.render_time())
            .min()
            .unwrap_or(Duration::ZERO)
    }

    pub fn max_frametime(&self, window: usize) -> Duration {
        self.previous_frames
            .iter()
            .rev()
            .take(window)
            .map(|f| f.frame_time())
            .max()
            .unwrap_or(Duration::ZERO)
    }

    pub fn min_frametime(&self, window: usize) -> Duration {
        self.previous_frames
            .iter()
            .rev()
            .take(window)
            .map(|f| f.frame_time())
            .min()
            .unwrap_or(Duration::ZERO)
    }

    pub fn avg_rendertime(&self) -> Duration {
        let Some(sum_rendertime) = self
            .previous_frames
            .iter()
            .map(|f| f.render_time())
            .try_fold(Duration::ZERO, |acc, x| acc.checked_add(x))
        else {
            return Duration::ZERO;
        };

        sum_rendertime
            .checked_div(self.previous_frames.len() as u32)
            .unwrap_or(Duration::ZERO)
    }

    pub fn avg_submittime(&self, window: usize) -> Option<Duration> {
        if self.previous_frames.len() < window || window == 0 {
            return None;
        }

        Some(
            self.previous_frames
                .iter()
                .rev()
                .take(window)
                .map(|f| f.submit_time())
                .try_fold(Duration::ZERO, |acc, x| acc.checked_add(x))?
                / (window.min(self.previous_frames.len()) as u32),
        )
    }

    pub fn avg_frametime(&self, window: usize) -> Option<Duration> {
        if self.previous_frames.len() < window || window == 0 {
            return None;
        }

        Some(
            self.previous_frames
                .iter()
                .rev()
                .take(window)
                .map(|f| f.frame_time())
                .try_fold(Duration::ZERO, |acc, x| acc.checked_add(x))?
                / (window.min(self.previous_frames.len()) as u32),
        )
    }

    pub fn avg_fps(&self) -> f64 {
        if self.previous_frames.is_empty() {
            return 0.0;
        }
        let secs = match (self.previous_frames.front(), self.previous_frames.back()) {
            (Some(Frame { render_start, .. }), Some(end_frame)) => {
                Time::elapsed(render_start, end_frame.render_start) + end_frame.frame_time()
            }
            _ => {
                return 0.0;
            }
        }
        .as_secs_f64();

        1.0 / (secs / self.previous_frames.len() as f64)
    }

    /// Achieved fps over the last `window` presented frames, from their
    /// presentation timestamps. Unlike [`Timings::avg_fps`] (which averages over
    /// the whole history and lags), this tracks the current rate quickly — used
    /// for the frame-pacing instrumentation log.
    pub fn recent_fps(&self, window: usize) -> f64 {
        let n = self.previous_frames.len().min(window);
        if n < 2 {
            return 0.0;
        }
        let newest = self.previous_frames.back().unwrap().presentation_presented;
        let oldest = self.previous_frames[self.previous_frames.len() - n].presentation_presented;
        let secs = Time::elapsed(&oldest, newest).as_secs_f64();
        if secs <= 0.0 {
            return 0.0;
        }
        (n as f64 - 1.0) / secs
    }

    /// Recent average frame time (ns) over the last `window` presented frames —
    /// the inverse of [`Timings::recent_fps`]. Fed to the game-mode D-Bus
    /// `AppFrametimeNs` reader for Auto-TDP. 0 if not enough frames yet.
    pub fn recent_frametime_ns(&self, window: usize) -> u64 {
        let fps = self.recent_fps(window);
        if fps > 0.0 {
            (1_000_000_000.0 / fps) as u64
        } else {
            0
        }
    }

    /// Smallest gap between consecutive presented frames over the last `window`
    /// frames (ms). Compared against the fps-cap floor: ~equal to the mean ⇒ a
    /// uniform cap-floor miss; far below the mean ⇒ bursty double-presents.
    pub fn recent_min_interval_ms(&self, window: usize) -> f64 {
        let len = self.previous_frames.len();
        let n = len.min(window);
        if n < 2 {
            return 0.0;
        }
        let mut min = f64::INFINITY;
        for i in (len - n + 1)..len {
            let prev = self.previous_frames[i - 1].presentation_presented;
            let cur = self.previous_frames[i].presentation_presented;
            let gap = Time::elapsed(&prev, cur).as_secs_f64() * 1000.0;
            if gap < min {
                min = gap;
            }
        }
        if min.is_finite() { min } else { 0.0 }
    }

    pub fn next_presentation_time(&self, clock: &Clock<Monotonic>) -> Duration {
        let mut now = clock.now().into();
        // The real clock, before the early-vblank correction below may advance
        // `now`. Used by the fps-cap branch so the cap is measured from the true
        // clock, not the vblank-prediction-adjusted one.
        let real_now = now;

        let Some(refresh_interval_ns) = self.refresh_interval_ns else {
            return Duration::ZERO;
        };
        let Some(last_presentation_time): Option<Duration> = self
            .previous_frames
            .back()
            .map(|frame| frame.presentation_presented.into())
        else {
            return Duration::ZERO;
        };
        let refresh_interval_ns = refresh_interval_ns.get();

        if now <= last_presentation_time {
            // Got an early VBlank.
            let orig_now = now;
            now += Duration::from_nanos(refresh_interval_ns);

            if now < last_presentation_time {
                // Not sure when this can happen.
                error!(
                    now = ?orig_now,
                    ?last_presentation_time,
                    "got a 2+ early VBlank, {:?} until presentation",
                    last_presentation_time - now,
                );
                now = last_presentation_time + Duration::from_nanos(refresh_interval_ns);
            }
        }

        let since_last = now - last_presentation_time;
        let since_last_ns =
            since_last.as_secs() * 1_000_000_000 + u64::from(since_last.subsec_nanos());
        let to_next_ns = (since_last_ns / refresh_interval_ns + 1) * refresh_interval_ns;

        // Game-mode fps cap: schedule the next presentation no sooner than
        // 1/fps_limit after the last. This caps the presentation rate — and thus
        // the client's frame rate, since it gets one frame callback per present —
        // while keeping the output ticking, unlike dropping frame callbacks
        // (which stalls a damage-driven compositor into a freeze).
        let to_next_ns = cap_to_fps(to_next_ns, self.fps_limit, refresh_interval_ns, self.vrr);

        // If VRR is enabled and more than one frame passed since last presentation, assume that we
        // can present immediately — unless an fps cap is active, in which case we honor it.
        if self.vrr && self.fps_limit == 0 && to_next_ns > refresh_interval_ns {
            Duration::ZERO
        } else if self.fps_limit != 0 {
            // Schedule the capped present at `last + to_next` relative to the REAL
            // clock. The early-vblank correction above advances `now` by a refresh
            // interval for vblank prediction; returning the delay against that
            // advanced `now` shortens the capped interval by one vblank, so
            // consecutive presents land 4 vblanks apart instead of 5 (cap 33 →
            // ~37 fps under fast scanout). `real_now` keeps them a full `to_next`
            // apart.
            (last_presentation_time + Duration::from_nanos(to_next_ns)).saturating_sub(real_now)
        } else {
            last_presentation_time + Duration::from_nanos(to_next_ns) - now
        }
    }

    pub fn past_min_render_time(&self, clock: &Clock<Monotonic>) -> bool {
        let now: Duration = clock.now().into();
        let Some(min_refresh_interval_ns) = self.min_refresh_interval_ns else {
            return true;
        };
        let Some(last_presentation_time): Option<Duration> = self
            .previous_frames
            .back()
            .map(|frame| frame.presentation_presented.into())
        else {
            return true;
        };

        let min_refresh_interval_ns = min_refresh_interval_ns.get();
        if now <= last_presentation_time {
            return false;
        }

        const MIN_MARGIN: Duration = Duration::from_millis(3);
        let baseline = if let Some(refresh_interval_ns) = self.refresh_interval_ns {
            MIN_MARGIN.max(Duration::from_nanos(refresh_interval_ns.get() / 2))
        } else {
            MIN_MARGIN
        };

        let next_presentation_time =
            last_presentation_time + Duration::from_nanos(min_refresh_interval_ns);
        let deadline = next_presentation_time.saturating_sub(
            if let Some(avg_submittime) = self.avg_submittime(SAMPLE_TIME_WINDOW) {
                avg_submittime
            } else {
                baseline
            } + BASE_SAFETY_MARGIN,
        );

        now >= deadline
    }

    pub fn next_render_time(&self, clock: &Clock<Monotonic>) -> Duration {
        if self.refresh_interval_ns.is_none() {
            return Duration::ZERO; // we don't know what to expect, so render immediately.
        }

        let estimated_presentation_time = self.next_presentation_time(clock);
        if estimated_presentation_time.is_zero() {
            return Duration::ZERO;
        }

        // HACK: Nvidia returns `page_flip`/`commit` early, so we have no information to optimize latency on submission.
        // Qualcomm Adreno drivers can also exhibit similar early-return behavior on some SoCs.
        if self.vendor == Some(0x10de) || self.vendor == Some(0x5143) {
            if self.vendor == Some(0x5143) {
                debug!(
                    "Adreno GPU: skipping render time optimization (driver returns commit early)"
                );
            }
            return Duration::ZERO;
        }

        // Wake-ahead offset: wake `rolling_draw_time + red_zone` before the
        // predicted vblank; under VRR skip prediction and wake a flat flush time
        // (plus the compositing floor only when compositing).
        let offset_ns = if self.vrr {
            VRR_FLUSHING_TIME_NS
                + if self.compositing {
                    VBLANK_COMPOSITING_FLOOR_NS
                } else {
                    0
                }
        } else {
            self.rolling_draw_time_ns + VBLANK_RED_ZONE_NS
        };

        estimated_presentation_time.saturating_sub(Duration::from_nanos(offset_ns))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Verifies the pacer's rolling-draw-time math: the sawtooth EMA, the
    // instant spike snap, the red-zone clamp, and the compositing floor.
    #[test]
    fn rolling_draw_time_sawtooth_ema() {
        let refresh = Some(16_666_666u64); // ~60 Hz

        // Steady state at the seed: a draw equal to the estimate keeps it put.
        assert_eq!(
            roll_draw_time(3_000_000, 3_000_000, false, refresh),
            3_000_000
        );

        // A large spike snaps the estimate straight up (sawtooth), it does not ease.
        assert_eq!(
            roll_draw_time(3_000_000, 10_000_000, false, refresh),
            10_000_000
        );

        // After a spike, a fast frame eases down only ~2%/frame (98% retained):
        // (980*10_000_000 + 20*1_000_000)/1000 = 9_820_000.
        assert_eq!(
            roll_draw_time(10_000_000, 1_000_000, false, refresh),
            9_820_000
        );

        // The compositing floor (2.4 ms) is applied to the input before easing:
        // floored draw 2_400_000, (980*3_000_000 + 20*2_400_000)/1000 = 2_988_000.
        assert_eq!(
            roll_draw_time(3_000_000, 1_000_000, true, refresh),
            2_988_000
        );

        // Without the compositing floor the same fast frame eases lower:
        // (980*3_000_000 + 20*1_000_000)/1000 = 2_960_000.
        assert_eq!(
            roll_draw_time(3_000_000, 1_000_000, false, refresh),
            2_960_000
        );

        // The estimate is clamped to (refresh interval − red zone) so we never
        // schedule past the previous vblank: cap = 16_666_666 − 1_650_000.
        assert_eq!(
            roll_draw_time(3_000_000, 50_000_000, false, refresh),
            16_666_666 - VBLANK_RED_ZONE_NS
        );

        // A sub-half-red-zone draw never triggers the spike path (saturating_sub
        // floors at 0, which is never > a positive estimate).
        assert_eq!(
            roll_draw_time(3_000_000, 500_000, false, refresh),
            (980 * 3_000_000 + 20 * 500_000) / 1000
        );
    }

    // The fps cap raises the next-presentation delta to ≥ 1/fps (vblank-rounded
    // on fixed refresh) — this is what makes the limiter hold a steady rate
    // instead of spiraling like the old frame-callback-dropping approach.
    #[test]
    fn fps_cap_schedules_at_target_rate() {
        let r60 = 16_666_666u64; // 60 Hz vblank interval
        let vblank = r60; // a normal next-vblank delta

        // Uncapped: unchanged.
        assert_eq!(cap_to_fps(vblank, 0, r60, false), vblank);

        // 20 fps on 60 Hz → 3 vblanks → 50 ms.
        assert_eq!(cap_to_fps(vblank, 20, r60, false), 3 * r60);
        // 30 fps on 60 Hz → 2 vblanks → 33.3 ms.
        assert_eq!(cap_to_fps(vblank, 30, r60, false), 2 * r60);
        // 50 fps on 60 Hz isn't an even divisor → rounds up to 2 vblanks (30 fps).
        assert_eq!(cap_to_fps(vblank, 50, r60, false), 2 * r60);

        // If the next vblank is already further out than the cap, it's untouched.
        assert_eq!(cap_to_fps(4 * r60, 20, r60, false), 4 * r60);

        // Under VRR the exact interval is used (no vblank rounding).
        assert_eq!(cap_to_fps(vblank, 20, r60, true), 1_000_000_000 / 20);
    }
}
