// SPDX-License-Identifier: GPL-3.0-only

//! GPU profiling and diagnostics module for performance analysis.
//!
//! Provides:
//! - GPU vendor/driver detection (Adreno, Mali, Intel, AMD, Nvidia)
//! - Per-frame phase timing (elements, draw, submit, present)
//! - Periodic performance summary logging
//! - Adreno-specific optimization hints and adaptive quality settings
//!
//! # Usage
//!
//! Set `COSMIC_PERF_LOG=1` to enable periodic frame statistics logging.
//! Set `COSMIC_PERF_LOG_INTERVAL=N` to log every N seconds (default: 5).
//! Set `COSMIC_GPU_DIAG=1` to log full GPU diagnostics at startup.

use std::collections::VecDeque;
use std::sync::{LazyLock, RwLock};
use std::time::{Duration, Instant};

use tracing::{info, warn};

// =============================================================================
// GPU Vendor Constants
// =============================================================================

/// PCI vendor IDs for GPU identification.
pub const VENDOR_QUALCOMM: u32 = 0x5143;
pub const VENDOR_INTEL: u32 = 0x8086;
pub const VENDOR_AMD: u32 = 0x1002;
pub const VENDOR_NVIDIA: u32 = 0x10de;
pub const VENDOR_ARM: u32 = 0x13b5; // ARM Mali

// =============================================================================
// GPU Info
// =============================================================================

/// Detected GPU information, gathered once at startup.
#[derive(Debug, Clone)]
pub struct GpuInfo {
    pub vendor_id: Option<u32>,
    pub device_id: Option<u32>,
    pub vendor_name: String,
    pub driver_name: String,
    pub gl_renderer: String,
    pub gl_version: String,
    pub gl_vendor: String,
    pub is_adreno: bool,
    pub is_mali: bool,
    pub is_software: bool,
    /// Whether this is a tile-based renderer (Adreno, Mali, PowerVR).
    pub is_tiler: bool,
}

impl Default for GpuInfo {
    fn default() -> Self {
        Self {
            vendor_id: None,
            device_id: None,
            vendor_name: "Unknown".into(),
            driver_name: "Unknown".into(),
            gl_renderer: String::new(),
            gl_version: String::new(),
            gl_vendor: String::new(),
            is_adreno: false,
            is_mali: false,
            is_software: false,
            is_tiler: false,
        }
    }
}

impl GpuInfo {
    /// Detect GPU info from DRM node sysfs and GL strings.
    pub fn detect(drm_node_minor: u32, gl_renderer: &str, gl_version: &str, gl_vendor: &str) -> Self {
        let sysfs_base = format!("/sys/class/drm/renderD{}/device", drm_node_minor);

        let vendor_id = read_sysfs_hex(&format!("{}/vendor", sysfs_base));
        let device_id = read_sysfs_hex(&format!("{}/device", sysfs_base));

        let vendor_name = match vendor_id {
            Some(VENDOR_QUALCOMM) => "Qualcomm (Adreno)".into(),
            Some(VENDOR_INTEL) => "Intel".into(),
            Some(VENDOR_AMD) => "AMD".into(),
            Some(VENDOR_NVIDIA) => "NVIDIA".into(),
            Some(VENDOR_ARM) => "ARM (Mali)".into(),
            Some(id) => format!("Unknown (0x{:04x})", id),
            None => "Unknown".into(),
        };

        let driver_name = std::fs::read_to_string(format!("{}/driver/module/description", sysfs_base))
            .or_else(|_| {
                // Try reading the driver symlink name
                std::fs::read_link(format!("{}/driver", sysfs_base))
                    .map(|p| p.file_name().map(|n| n.to_string_lossy().into_owned()).unwrap_or_default())
            })
            .unwrap_or_else(|_| "unknown".into())
            .trim()
            .to_string();

        let gl_renderer_lower = gl_renderer.to_lowercase();
        let gl_vendor_lower = gl_vendor.to_lowercase();
        let driver_lower = driver_name.to_lowercase();

        // Detect GPU family from multiple signals:
        // 1. PCI vendor ID (works for discrete GPUs)
        // 2. GL renderer/vendor strings (works when GL context is available)
        // 3. DRM driver name (works for SoC/platform devices like Qualcomm)
        let is_adreno = gl_renderer_lower.contains("adreno")
            || gl_vendor_lower.contains("qualcomm")
            || vendor_id == Some(VENDOR_QUALCOMM)
            || driver_lower.starts_with("msm");
        let is_mali = gl_renderer_lower.contains("mali")
            || vendor_id == Some(VENDOR_ARM)
            || driver_lower == "panfrost"
            || driver_lower == "lima";
        let is_software = gl_renderer_lower.contains("llvmpipe")
            || gl_renderer_lower.contains("softpipe")
            || gl_renderer_lower.contains("swrast");
        let is_tiler = is_adreno || is_mali || gl_renderer_lower.contains("powervr");

        // Infer vendor name from driver if PCI vendor ID is unavailable (SoC devices)
        let vendor_name = if vendor_name == "Unknown" {
            if is_adreno {
                "Qualcomm (Adreno) [from driver]".into()
            } else if is_mali {
                "ARM (Mali) [from driver]".into()
            } else {
                vendor_name
            }
        } else {
            vendor_name
        };

        Self {
            vendor_id,
            device_id,
            vendor_name,
            driver_name,
            gl_renderer: gl_renderer.to_string(),
            gl_version: gl_version.to_string(),
            gl_vendor: gl_vendor.to_string(),
            is_adreno,
            is_mali,
            is_software,
            is_tiler,
        }
    }

    /// Log a comprehensive GPU diagnostics report.
    pub fn log_diagnostics(&self) {
        info!("╔══════════════════════════════════════════════════════════════╗");
        info!("║              GPU DIAGNOSTICS REPORT                        ║");
        info!("╠══════════════════════════════════════════════════════════════╣");
        info!("║ Vendor:      {:>46} ║", self.vendor_name);
        info!("║ Vendor ID:   {:>46} ║",
            self.vendor_id.map(|v| format!("0x{:04x}", v)).unwrap_or_else(|| "N/A".into()));
        info!("║ Device ID:   {:>46} ║",
            self.device_id.map(|d| format!("0x{:04x}", d)).unwrap_or_else(|| "N/A".into()));
        info!("║ Driver:      {:>46} ║", self.driver_name);
        info!("║ GL Renderer: {:>46} ║", truncate_str(&self.gl_renderer, 46));
        info!("║ GL Version:  {:>46} ║", truncate_str(&self.gl_version, 46));
        info!("║ GL Vendor:   {:>46} ║", truncate_str(&self.gl_vendor, 46));
        info!("║ Tile-based:  {:>46} ║", if self.is_tiler { "YES" } else { "no" });
        info!("║ Adreno:      {:>46} ║", if self.is_adreno { "YES" } else { "no" });
        info!("║ Software:    {:>46} ║", if self.is_software { "YES" } else { "no" });
        info!("╚══════════════════════════════════════════════════════════════╝");

        if self.is_adreno {
            info!("Adreno GPU detected — applying tile-based renderer optimizations:");
            info!("  • Minimizing render target switches (FBO ping-pong)");
            info!("  • Reducing blur iterations for tile-based architecture");
            info!("  • Using larger downsample factor for blur");
            info!("  • Preferring direct scanout when possible");

            // Check for known problematic Adreno GPUs
            if self.gl_renderer.contains("Adreno (TM) 6") {
                info!("  • Adreno 6xx series: mid-range, moderate shader complexity recommended");
            } else if self.gl_renderer.contains("Adreno (TM) 7") {
                info!("  • Adreno 7xx series: capable GPU, standard optimizations apply");
            }
        }

        if self.is_tiler && !self.is_adreno {
            info!("Tile-based GPU detected — rendering optimizations will be applied");
        }
    }
}

// =============================================================================
// Frame Phase Profiler
// =============================================================================

/// Tracks timing of individual phases within a single frame render.
#[derive(Debug, Clone)]
pub struct FrameProfile {
    pub frame_start: Instant,
    pub elements_duration: Duration,
    pub blur_duration: Duration,
    pub draw_duration: Duration,
    pub submit_duration: Duration,
    pub total_duration: Duration,
    pub element_count: usize,
    pub blur_window_count: usize,
    pub damage_rects: usize,
    pub skipped: bool,
}

impl Default for FrameProfile {
    fn default() -> Self {
        Self {
            frame_start: Instant::now(),
            elements_duration: Duration::ZERO,
            blur_duration: Duration::ZERO,
            draw_duration: Duration::ZERO,
            submit_duration: Duration::ZERO,
            total_duration: Duration::ZERO,
            element_count: 0,
            blur_window_count: 0,
            damage_rects: 0,
            skipped: false,
        }
    }
}

/// Rolling window of frame profiles for performance analysis.
pub struct FrameProfiler {
    profiles: VecDeque<FrameProfile>,
    max_profiles: usize,
    last_report: Instant,
    report_interval: Duration,
    total_frames: u64,
    total_slow_frames: u64,
    /// Threshold above which a frame is considered "slow" (default: 16.67ms for 60fps).
    slow_threshold: Duration,
    output_name: String,
}

impl FrameProfiler {
    pub fn new(output_name: &str) -> Self {
        let interval_secs = std::env::var("COSMIC_PERF_LOG_INTERVAL")
            .ok()
            .and_then(|v| v.parse::<u64>().ok())
            .unwrap_or(5);

        Self {
            profiles: VecDeque::with_capacity(600),
            max_profiles: 600, // ~10 seconds at 60fps
            last_report: Instant::now(),
            report_interval: Duration::from_secs(interval_secs),
            total_frames: 0,
            total_slow_frames: 0,
            slow_threshold: Duration::from_micros(16_667), // 60fps
            output_name: output_name.to_string(),
        }
    }

    /// Set the target framerate to determine what counts as "slow".
    pub fn set_target_fps(&mut self, fps: f64) {
        if fps > 0.0 {
            self.slow_threshold = Duration::from_secs_f64(1.0 / fps);
        }
    }

    /// Record a completed frame profile.
    pub fn record(&mut self, profile: FrameProfile) {
        self.total_frames += 1;
        if profile.total_duration > self.slow_threshold {
            self.total_slow_frames += 1;
        }

        // Log individual slow frames at debug level
        if profile.total_duration > self.slow_threshold * 2 {
            warn!(
                output = %self.output_name,
                total_ms = format!("{:.2}", profile.total_duration.as_secs_f64() * 1000.0),
                elements_ms = format!("{:.2}", profile.elements_duration.as_secs_f64() * 1000.0),
                blur_ms = format!("{:.2}", profile.blur_duration.as_secs_f64() * 1000.0),
                draw_ms = format!("{:.2}", profile.draw_duration.as_secs_f64() * 1000.0),
                submit_ms = format!("{:.2}", profile.submit_duration.as_secs_f64() * 1000.0),
                elements = profile.element_count,
                blur_windows = profile.blur_window_count,
                damage = profile.damage_rects,
                "SLOW FRAME (>{:.1}ms): {:.2}ms total",
                self.slow_threshold.as_secs_f64() * 2000.0,
                profile.total_duration.as_secs_f64() * 1000.0,
            );
        }

        if self.profiles.len() >= self.max_profiles {
            self.profiles.pop_front();
        }
        self.profiles.push_back(profile);
    }

    /// Check if it's time to log a periodic report, and do so.
    pub fn maybe_report(&mut self) {
        if !perf_logging_enabled() {
            return;
        }

        let now = Instant::now();
        if now.duration_since(self.last_report) < self.report_interval {
            return;
        }
        self.last_report = now;

        if self.profiles.is_empty() {
            return;
        }

        let window_size = self.profiles.len();
        let (avg_total, _avg_elements, avg_blur, avg_draw, avg_submit) = {
            let mut sum_total = Duration::ZERO;
            let mut sum_elements = Duration::ZERO;
            let mut sum_blur = Duration::ZERO;
            let mut sum_draw = Duration::ZERO;
            let mut sum_submit = Duration::ZERO;
            for p in &self.profiles {
                sum_total += p.total_duration;
                sum_elements += p.elements_duration;
                sum_blur += p.blur_duration;
                sum_draw += p.draw_duration;
                sum_submit += p.submit_duration;
            }
            let n = window_size as u32;
            (
                sum_total / n,
                sum_elements / n,
                sum_blur / n,
                sum_draw / n,
                sum_submit / n,
            )
        };

        let max_total = self.profiles.iter().map(|p| p.total_duration).max().unwrap_or_default();
        let min_total = self.profiles.iter().map(|p| p.total_duration).min().unwrap_or_default();

        let p99_total = {
            let mut sorted: Vec<_> = self.profiles.iter().map(|p| p.total_duration).collect();
            sorted.sort();
            sorted.get(sorted.len() * 99 / 100).copied().unwrap_or_default()
        };

        let p95_total = {
            let mut sorted: Vec<_> = self.profiles.iter().map(|p| p.total_duration).collect();
            sorted.sort();
            sorted.get(sorted.len() * 95 / 100).copied().unwrap_or_default()
        };

        let avg_fps = if avg_total.as_secs_f64() > 0.0 {
            1.0 / avg_total.as_secs_f64()
        } else {
            0.0
        };

        let slow_pct = if self.total_frames > 0 {
            (self.total_slow_frames as f64 / self.total_frames as f64) * 100.0
        } else {
            0.0
        };

        let avg_elements: f64 = self.profiles.iter().map(|p| p.element_count as f64).sum::<f64>() / window_size as f64;
        let avg_blur_wins: f64 = self.profiles.iter().map(|p| p.blur_window_count as f64).sum::<f64>() / window_size as f64;
        let avg_damage: f64 = self.profiles.iter().map(|p| p.damage_rects as f64).sum::<f64>() / window_size as f64;

        info!(
            "┌─── PERF REPORT: {} ({} frames) ──────────────────────────────",
            self.output_name, window_size
        );
        info!(
            "│ FPS:     avg={:.1}  target={:.1}  slow_frames={:.1}%",
            avg_fps,
            1.0 / self.slow_threshold.as_secs_f64(),
            slow_pct,
        );
        info!(
            "│ Frame:   avg={:.2}ms  min={:.2}ms  max={:.2}ms  p95={:.2}ms  p99={:.2}ms",
            avg_total.as_secs_f64() * 1000.0,
            min_total.as_secs_f64() * 1000.0,
            max_total.as_secs_f64() * 1000.0,
            p95_total.as_secs_f64() * 1000.0,
            p99_total.as_secs_f64() * 1000.0,
        );
        info!(
            "│ Phases:  elements={:.2}ms  blur={:.2}ms  draw={:.2}ms  submit={:.2}ms",
            avg_elements_dur_ms(avg_elements_ms(&self.profiles)),
            avg_blur.as_secs_f64() * 1000.0,
            avg_draw.as_secs_f64() * 1000.0,
            avg_submit.as_secs_f64() * 1000.0,
        );
        info!(
            "│ Load:    elements={:.0}  blur_windows={:.1}  damage_rects={:.0}",
            avg_elements, avg_blur_wins, avg_damage,
        );
        info!(
            "└──────────────────────────────────────────────────────────────"
        );
    }
}

fn avg_elements_ms(profiles: &VecDeque<FrameProfile>) -> f64 {
    if profiles.is_empty() {
        return 0.0;
    }
    let sum: f64 = profiles.iter().map(|p| p.elements_duration.as_secs_f64() * 1000.0).sum();
    sum / profiles.len() as f64
}

fn avg_elements_dur_ms(v: f64) -> f64 {
    v
}

// =============================================================================
// Adreno Optimization Settings
// =============================================================================

/// Adaptive quality settings for tile-based GPUs (Adreno/Mali).
///
/// These can be checked at render time to reduce GPU load.
#[derive(Debug, Clone)]
pub struct TilerOptimizations {
    /// Reduce blur iterations (default 12 → 6 on Adreno).
    pub blur_iterations: u32,
    /// Increase blur downsample factor (default 8 → 12 on Adreno).
    pub blur_downsample_factor: i32,
    /// Whether to skip shadow rendering when frame budget is tight.
    pub adaptive_shadows: bool,
    /// Maximum number of blur windows to process per frame.
    /// Beyond this, remaining blur windows use fallback solid color.
    pub max_blur_windows_per_frame: usize,
    /// Whether to prefer direct scanout for fullscreen surfaces.
    pub prefer_direct_scanout: bool,
    /// Blur throttle interval - how long to wait between re-blurs when content changes.
    /// Longer intervals save GPU on slower tiler GPUs.
    pub blur_throttle_interval: std::time::Duration,
}

impl Default for TilerOptimizations {
    fn default() -> Self {
        Self {
            blur_iterations: super::BLUR_ITERATIONS,
            blur_downsample_factor: super::BLUR_DOWNSAMPLE_FACTOR,
            adaptive_shadows: false,
            max_blur_windows_per_frame: usize::MAX,
            prefer_direct_scanout: true,
            blur_throttle_interval: std::time::Duration::from_millis(100),
        }
    }
}

impl TilerOptimizations {
    /// Create optimizations tuned for Adreno GPUs.
    pub fn for_adreno() -> Self {
        Self {
            // Adreno tile-based architecture is penalized by many FBO switches
            // from ping-pong blur passes. Reduce iterations significantly.
            blur_iterations: 6,
            // Larger downsample = less pixels to blur = massive FBO savings.
            blur_downsample_factor: 12,
            // Enable adaptive shadow quality when running behind.
            adaptive_shadows: true,
            // Limit concurrent blur windows to avoid FBO thrashing.
            max_blur_windows_per_frame: 3,
            // Adreno benefits greatly from direct scanout (bypasses composition).
            prefer_direct_scanout: true,
            // 250ms throttle = max 4 blur updates/sec. Background changes like
            // wallpaper clock are imperceptible at this rate through blurred glass.
            blur_throttle_interval: std::time::Duration::from_millis(250),
        }
    }

    /// Create optimizations for other tile-based GPUs (Mali, etc.).
    pub fn for_tiler() -> Self {
        Self {
            blur_iterations: 8,
            blur_downsample_factor: 10,
            adaptive_shadows: true,
            max_blur_windows_per_frame: 4,
            prefer_direct_scanout: true,
            blur_throttle_interval: std::time::Duration::from_millis(150),
        }
    }
}

// =============================================================================
// Global State
// =============================================================================

/// Global GPU info, set once during initialization.
static GPU_INFO: LazyLock<RwLock<Option<GpuInfo>>> = LazyLock::new(|| RwLock::new(None));

/// Global tiler optimizations, set based on detected GPU.
static TILER_OPTS: LazyLock<RwLock<TilerOptimizations>> =
    LazyLock::new(|| RwLock::new(TilerOptimizations::default()));

/// Set the global GPU info (called once during device initialization).
pub fn set_gpu_info(info: GpuInfo) {
    // Determine and set optimizations based on GPU type
    let opts = if info.is_adreno {
        TilerOptimizations::for_adreno()
    } else if info.is_tiler {
        TilerOptimizations::for_tiler()
    } else {
        TilerOptimizations::default()
    };

    info!(
        gpu_vendor = %info.vendor_name,
        is_tiler = info.is_tiler,
        blur_iterations = opts.blur_iterations,
        blur_downsample = opts.blur_downsample_factor,
        max_blur_windows = opts.max_blur_windows_per_frame,
        "GPU optimization profile applied"
    );

    if let Ok(mut guard) = TILER_OPTS.write() {
        *guard = opts;
    }

    // Log full diagnostics if requested
    if gpu_diag_enabled() {
        info.log_diagnostics();
    }

    if let Ok(mut guard) = GPU_INFO.write() {
        *guard = Some(info);
    }
}

/// Get the current GPU info, if detected.
pub fn get_gpu_info() -> Option<GpuInfo> {
    GPU_INFO.read().ok().and_then(|g| g.clone())
}

/// Get the active tiler optimizations.
pub fn get_tiler_opts() -> TilerOptimizations {
    TILER_OPTS
        .read()
        .ok()
        .map(|g| g.clone())
        .unwrap_or_default()
}

/// Check if the detected GPU is a tile-based renderer.
pub fn is_tiler_gpu() -> bool {
    GPU_INFO
        .read()
        .ok()
        .and_then(|g| g.as_ref().map(|i| i.is_tiler))
        .unwrap_or(false)
}

/// Check if the detected GPU is Adreno.
pub fn is_adreno_gpu() -> bool {
    GPU_INFO
        .read()
        .ok()
        .and_then(|g| g.as_ref().map(|i| i.is_adreno))
        .unwrap_or(false)
}

/// Get the effective blur iteration count (may be reduced for tile-based GPUs).
pub fn effective_blur_iterations() -> u32 {
    // Allow env override
    if let Some(val) = std::env::var("COSMIC_BLUR_ITERATIONS")
        .ok()
        .and_then(|v| v.parse::<u32>().ok())
    {
        return val;
    }
    get_tiler_opts().blur_iterations
}

/// Get the effective blur downsample factor.
pub fn effective_blur_downsample_factor() -> i32 {
    // Allow env override
    if let Some(val) = std::env::var("COSMIC_BLUR_DOWNSAMPLE_FACTOR")
        .ok()
        .and_then(|v| v.parse::<i32>().ok())
    {
        return val;
    }
    get_tiler_opts().blur_downsample_factor
}

/// Check if blur processing should be skipped for this window index.
pub fn should_skip_blur_window(window_index: usize) -> bool {
    window_index >= get_tiler_opts().max_blur_windows_per_frame
}

// =============================================================================
// Environment Helpers
// =============================================================================

/// Check if performance logging is enabled via COSMIC_PERF_LOG.
/// Enabled by default. Set COSMIC_PERF_LOG=0 to disable.
pub fn perf_logging_enabled() -> bool {
    crate::utils::env::bool_var("COSMIC_PERF_LOG").unwrap_or(true)
}

/// Check if GPU diagnostics logging is enabled via COSMIC_GPU_DIAG.
/// Enabled by default. Set COSMIC_GPU_DIAG=0 to disable.
fn gpu_diag_enabled() -> bool {
    crate::utils::env::bool_var("COSMIC_GPU_DIAG").unwrap_or(true)
}

// =============================================================================
// Utility Functions
// =============================================================================

fn read_sysfs_hex(path: &str) -> Option<u32> {
    let contents = std::fs::read_to_string(path).ok()?;
    let trimmed = contents.trim();
    let hex_str = trimmed.strip_prefix("0x").unwrap_or(trimmed);
    u32::from_str_radix(hex_str, 16).ok()
}

fn truncate_str(s: &str, max_len: usize) -> String {
    if s.len() <= max_len {
        s.to_string()
    } else {
        format!("{}...", &s[..max_len - 3])
    }
}

/// Macro for timing a block and storing it in a duration variable.
///
/// Usage:
/// ```ignore
/// let mut dur = Duration::ZERO;
/// profile_phase!(dur, {
///     // ... code to time ...
/// });
/// ```
#[macro_export]
macro_rules! profile_phase {
    ($dur:expr, $block:block) => {{
        let _phase_start = std::time::Instant::now();
        let _result = $block;
        $dur = _phase_start.elapsed();
        _result
    }};
}
