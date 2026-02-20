// SPDX-License-Identifier: GPL-3.0-only

//! Real-time background blur implementation using Kawase blur algorithm.
//!
//! This module provides blur rendering for windows that request the KDE blur protocol.
//! The algorithm works by:
//! 1. Rendering background elements to an offscreen texture (excluding windows above the target)
//! 2. Optionally downsampling for performance (when `COSMIC_BLUR_DOWNSAMPLE=1`)
//! 3. Applying multiple Kawase blur iterations (ping-pong rendering)
//! 4. Caching blurred textures per-window for reuse when content hasn't changed
//! 5. Compositing the blurred result behind the target window
//!
//! # Architecture
//!
//! - `BlurRenderState`: Per-output state managing textures for blur passes
//! - `BlurredTextureInfo`: Cached blur result with metadata for invalidation
//! - `BlurCaptureContext`: Context passed through render pipeline for window exclusion
//! - Global caches: `BLUR_TEXTURE_CACHE` (per-window), `BLUR_GROUP_CONTENT_STATE` (invalidation)
//!
//! # Performance Optimizations
//!
//! - **Cache invalidation**: Tracks content hash via commit counters + geometry
//! - **Grouped captures**: Consecutive blur windows share a single capture
//! - **Optional downsampling**: Reduces blur texture size for less GPU work
//! - **Window exclusion**: Grabbed windows and windows above target are excluded via `BlurCaptureContext`

use smithay::{
    backend::{
        allocator::{Fourcc, dmabuf::Dmabuf},
        renderer::{
            Bind, Offscreen, Renderer,
            damage::OutputDamageTracker,
            element::{
                Element, Kind,
                texture::{TextureRenderBuffer, TextureRenderElement},
            },
            gles::{
                GlesError, GlesTexProgram, GlesTexture, Uniform, element::TextureShaderElement,
            },
        },
    },
    utils::{Logical, Physical, Point, Rectangle, Scale, Size, Transform},
};
use std::collections::HashMap;
use std::sync::{LazyLock, RwLock};
use std::time::{Duration, Instant};
use wayland_backend::server::ObjectId;

use super::element::AsGlowRenderer;
use crate::shell::element::{CosmicMapped, CosmicMappedKey};

// =============================================================================
// Constants
// =============================================================================

/// Default blur radius in pixels
pub const DEFAULT_BLUR_RADIUS: f32 = 60.0;

/// Number of blur iterations.
pub const BLUR_ITERATIONS: u32 = 12;

/// Downsample factor for blur textures.
/// This massively increases perceived blur and is very GPU-efficient.
pub const BLUR_DOWNSAMPLE_FACTOR: i32 = 8;

// Blur backdrop styling constants
pub const BLUR_TINT_COLOR: [f32; 3] = [1.0, 1.0, 1.0];
/// Strength of the tint overlay (0.30 = 30% opacity)
pub const BLUR_TINT_STRENGTH: f32 = 0.15;
/// Alpha for fallback solid color when blur texture not available
pub const BLUR_FALLBACK_ALPHA: f32 = 0.95;
/// Fallback color when blur texture not available (light gray-blue)
pub const BLUR_FALLBACK_COLOR: [f32; 3] = [0.9, 0.9, 0.95];

/// Minimum interval between blur updates when only content changes (e.g., cursor blink).
/// This throttles blur re-computation to reduce GPU load from high-frequency updates.
/// 100ms = 10 blur updates per second max when background is animating.
pub const BLUR_THROTTLE_INTERVAL: Duration = Duration::from_millis(100);

// =============================================================================
// Environment Configuration
// =============================================================================

/// Check if blur texture downsampling is enabled via env var.
/// Downsampling is enabled by default for strong CSS-like blur effect.
/// Set `COSMIC_BLUR_DOWNSAMPLE=0` to disable (weaker blur, more GPU work).
pub fn blur_downsample_enabled() -> bool {
    crate::utils::env::bool_var("COSMIC_BLUR_DOWNSAMPLE").unwrap_or(true)
}

// =============================================================================
// Blur Render State
// =============================================================================

/// State for managing blur effect rendering per-output.
///
/// Blur requires rendering background content to an offscreen texture,
/// applying multiple Kawase blur passes (ping-pong), then compositing
/// the result behind the target surface.
///
/// Optimization: Blur passes can run at reduced resolution (1/BLUR_DOWNSAMPLE_FACTOR)
/// since blur removes high-frequency detail anyway.
///
/// Note: Window blur and layer blur have SEPARATE ping/pong textures to avoid
/// cache pollution. Window blur uses texture_a/texture_b, layer blur uses
/// layer_texture_a/layer_texture_b. This is necessary because cached blur
/// textures are references to these buffers, not copies.
#[derive(Debug)]
pub struct BlurRenderState {
    /// Texture containing the previous frame's content at full resolution (blur source)
    pub background_texture: Option<TextureRenderBuffer<GlesTexture>>,
    /// Downsampled texture for blur input (from background_texture)
    pub downsampled_texture: Option<TextureRenderBuffer<GlesTexture>>,
    /// Offscreen texture for window blur passes (ping) - at reduced resolution
    pub texture_a: Option<TextureRenderBuffer<GlesTexture>>,
    /// Offscreen texture for window blur passes (pong) - at reduced resolution
    pub texture_b: Option<TextureRenderBuffer<GlesTexture>>,
    /// Offscreen texture for layer blur passes (ping) - at reduced resolution
    /// Separate from texture_a to avoid cache pollution between window and layer blur
    pub layer_texture_a: Option<TextureRenderBuffer<GlesTexture>>,
    /// Offscreen texture for layer blur passes (pong) - at reduced resolution
    /// Separate from texture_b to avoid cache pollution between window and layer blur
    pub layer_texture_b: Option<TextureRenderBuffer<GlesTexture>>,
    /// Damage tracker for the blur textures
    pub damage_tracker: Option<OutputDamageTracker>,
    /// Size of the full-resolution background texture (screen size)
    pub screen_size: Size<i32, Physical>,
    /// Size of the downsampled blur textures
    pub texture_size: Size<i32, Physical>,
    /// Scale factor
    pub scale: Scale<f64>,
    /// Whether blur has been applied this frame
    pub blur_applied: bool,
    /// Whether texture allocation has permanently failed (don't retry)
    pub allocation_failed: bool,
}

impl Default for BlurRenderState {
    fn default() -> Self {
        Self {
            background_texture: None,
            downsampled_texture: None,
            texture_a: None,
            texture_b: None,
            layer_texture_a: None,
            layer_texture_b: None,
            damage_tracker: None,
            screen_size: Size::from((0, 0)),
            texture_size: Size::from((0, 0)),
            scale: Scale::from(1.0),
            blur_applied: false,
            allocation_failed: false,
        }
    }
}

impl BlurRenderState {
    /// Internal helper to create all blur textures with a specific format.
    /// Returns Ok(()) on success, or Err with the error on failure.
    fn create_textures_with_format<R: AsGlowRenderer + Offscreen<GlesTexture>>(
        &mut self,
        renderer: &mut R,
        format: Fourcc,
        size: Size<i32, Physical>,
        scale: Scale<f64>,
        downsample_enabled: bool,
        blur_size: Size<i32, Physical>,
    ) -> Result<(), R::Error> {
        // Full-size background texture (stores previous frame for blur source)
        let full_buffer_size = size.to_logical(1).to_buffer(1, Transform::Normal);
        let bg_tex = Offscreen::<GlesTexture>::create_buffer(renderer, format, full_buffer_size)?;
        self.background_texture = Some(TextureRenderBuffer::from_texture(
            renderer.glow_renderer(),
            bg_tex,
            1,
            Transform::Normal,
            None,
        ));

        // Downsampled buffer size for blur passes
        let blur_buffer_size = blur_size.to_logical(1).to_buffer(1, Transform::Normal);

        // Downsampled texture (intermediate for downsample) - only when downsampling enabled
        if downsample_enabled {
            let ds_tex =
                Offscreen::<GlesTexture>::create_buffer(renderer, format, blur_buffer_size)?;
            self.downsampled_texture = Some(TextureRenderBuffer::from_texture(
                renderer.glow_renderer(),
                ds_tex,
                1,
                Transform::Normal,
                None,
            ));
        } else {
            self.downsampled_texture = None;
        }

        // Create texture A (ping) - at blur resolution (reduced or full)
        let tex_a = Offscreen::<GlesTexture>::create_buffer(renderer, format, blur_buffer_size)?;
        self.texture_a = Some(TextureRenderBuffer::from_texture(
            renderer.glow_renderer(),
            tex_a,
            1,
            Transform::Normal,
            None,
        ));

        // Create texture B (pong) - at reduced resolution
        let tex_b = Offscreen::<GlesTexture>::create_buffer(renderer, format, blur_buffer_size)?;
        self.texture_b = Some(TextureRenderBuffer::from_texture(
            renderer.glow_renderer(),
            tex_b,
            1,
            Transform::Normal,
            None,
        ));

        // Create layer texture A (ping) - separate from window blur to avoid cache pollution
        let layer_tex_a =
            Offscreen::<GlesTexture>::create_buffer(renderer, format, blur_buffer_size)?;
        self.layer_texture_a = Some(TextureRenderBuffer::from_texture(
            renderer.glow_renderer(),
            layer_tex_a,
            1,
            Transform::Normal,
            None,
        ));

        // Create layer texture B (pong) - separate from window blur to avoid cache pollution
        let layer_tex_b =
            Offscreen::<GlesTexture>::create_buffer(renderer, format, blur_buffer_size)?;
        self.layer_texture_b = Some(TextureRenderBuffer::from_texture(
            renderer.glow_renderer(),
            layer_tex_b,
            1,
            Transform::Normal,
            None,
        ));

        // Create damage tracker
        self.damage_tracker = Some(OutputDamageTracker::new(size, scale, Transform::Normal));

        self.screen_size = size;
        self.texture_size = blur_size;
        self.scale = scale;
        self.blur_applied = false;
        Ok(())
    }

    /// Create or resize blur textures if needed.
    /// Creates full-size background texture and optionally downsampled blur textures.
    ///
    /// If the requested format is not supported (e.g., 10-bit formats on software renderers),
    /// this will automatically fall back to Argb8888.
    ///
    /// Returns `Ok(true)` if textures are ready, `Ok(false)` if allocation was previously
    /// marked as failed (skipped), or `Err` on new failure.
    pub fn ensure_textures<R: AsGlowRenderer + Offscreen<GlesTexture>>(
        &mut self,
        renderer: &mut R,
        format: Fourcc,
        size: Size<i32, Physical>,
        scale: Scale<f64>,
    ) -> Result<bool, R::Error> {
        // If allocation has previously failed permanently, skip without error
        if self.allocation_failed {
            return Ok(false);
        }

        let downsample_enabled = blur_downsample_enabled();

        // Calculate blur size (downsampled if enabled, full size otherwise)
        let blur_size: Size<i32, Physical> = if downsample_enabled {
            Size::from((
                (size.w / BLUR_DOWNSAMPLE_FACTOR).max(1),
                (size.h / BLUR_DOWNSAMPLE_FACTOR).max(1),
            ))
        } else {
            size
        };

        // Only recreate if size changed
        let textures_valid = if downsample_enabled {
            self.screen_size == size
                && self.texture_a.is_some()
                && self.texture_b.is_some()
                && self.layer_texture_a.is_some()
                && self.layer_texture_b.is_some()
                && self.background_texture.is_some()
                && self.downsampled_texture.is_some()
        } else {
            self.screen_size == size
                && self.texture_a.is_some()
                && self.texture_b.is_some()
                && self.layer_texture_a.is_some()
                && self.layer_texture_b.is_some()
                && self.background_texture.is_some()
        };

        if textures_valid {
            return Ok(true);
        }

        tracing::debug!(
            screen_w = size.w,
            screen_h = size.h,
            blur_w = blur_size.w,
            blur_h = blur_size.h,
            downsample_enabled = downsample_enabled,
            "Creating blur textures"
        );

        // Try to create textures with the requested format first
        match self.create_textures_with_format(
            renderer,
            format,
            size,
            scale,
            downsample_enabled,
            blur_size,
        ) {
            Ok(()) => return Ok(true),
            Err(err) => {
                // Check if this is an unsupported pixel format error
                let err_str = format!("{:?}", err);
                if err_str.contains("UnsupportedPixelFormat") && format != Fourcc::Argb8888 {
                    // Clear any partially created textures before retry
                    self.background_texture = None;
                    self.downsampled_texture = None;
                    self.texture_a = None;
                    self.texture_b = None;
                    self.layer_texture_a = None;
                    self.layer_texture_b = None;
                    self.damage_tracker = None;

                    tracing::info!(
                        ?format,
                        "Blur texture format not supported, falling back to Argb8888"
                    );

                    // Try again with Argb8888 as fallback (universally supported)
                    self.create_textures_with_format(
                        renderer,
                        Fourcc::Argb8888,
                        size,
                        scale,
                        downsample_enabled,
                        blur_size,
                    )?;
                    return Ok(true);
                }
                // Other errors are returned as-is
                return Err(err);
            }
        }
    }

    /// Get background texture (previous frame content at full resolution)
    pub fn background_texture(&self) -> Option<&TextureRenderBuffer<GlesTexture>> {
        self.background_texture.as_ref()
    }

    /// Get downsampled texture
    pub fn downsampled_texture(&self) -> Option<&TextureRenderBuffer<GlesTexture>> {
        self.downsampled_texture.as_ref()
    }

    /// Get texture A for rendering (at reduced resolution)
    pub fn texture_a(&self) -> Option<&TextureRenderBuffer<GlesTexture>> {
        self.texture_a.as_ref()
    }

    /// Get texture B for rendering (at reduced resolution)
    pub fn texture_b(&self) -> Option<&TextureRenderBuffer<GlesTexture>> {
        self.texture_b.as_ref()
    }

    /// Check if blur state is ready for rendering
    pub fn is_ready(&self) -> bool {
        let base_ready = self.background_texture.is_some()
            && self.texture_a.is_some()
            && self.texture_b.is_some()
            && self.layer_texture_a.is_some()
            && self.layer_texture_b.is_some();

        if blur_downsample_enabled() {
            base_ready && self.downsampled_texture.is_some()
        } else {
            base_ready
        }
    }
}

// =============================================================================
// Blur Texture Cache
// =============================================================================

/// Blurred texture data for a window
#[derive(Debug, Clone)]
pub struct BlurredTextureInfo {
    /// The blurred texture buffer
    pub texture: TextureRenderBuffer<GlesTexture>,
    /// Size of the blurred texture (may be downsampled)
    pub size: Size<i32, Physical>,
    /// Original screen size (for coordinate mapping when downsampled)
    pub screen_size: Size<i32, Physical>,
    /// Scale factor
    pub scale: Scale<f64>,
    /// Hash of background state for cache invalidation.
    /// Changes when windows below move/resize or z-order changes.
    pub background_state_hash: u64,
}

/// Cache key combining output name and window key hash
type BlurCacheKey = (String, u64);

/// Global cache of blurred textures per window per output.
/// For iterative rendering: each blur window has its own cached texture
/// capturing everything behind it (including other blurred windows below).
pub static BLUR_TEXTURE_CACHE: LazyLock<RwLock<HashMap<BlurCacheKey, BlurredTextureInfo>>> =
    LazyLock::new(|| RwLock::new(HashMap::new()));

/// Cache key for blur group content state: (output_name, capture_z_threshold)
type BlurGroupContentKey = (String, usize);

/// Tracks the last known content state for blur groups.
/// Used to detect when elements have changed (via their commit counter).
pub static BLUR_GROUP_CONTENT_STATE: LazyLock<RwLock<HashMap<BlurGroupContentKey, u64>>> =
    LazyLock::new(|| RwLock::new(HashMap::new()));

/// Tracks the last time blur was computed for each group.
/// Used for throttling blur updates when content changes frequently.
pub static BLUR_GROUP_LAST_UPDATE: LazyLock<RwLock<HashMap<BlurGroupContentKey, Instant>>> =
    LazyLock::new(|| RwLock::new(HashMap::new()));

/// Compute a hash for a window key (for cache lookup)
fn window_key_hash(key: &CosmicMappedKey) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    key.hash(&mut hasher);
    hasher.finish()
}

/// Store a blurred texture for a specific window on an output.
/// This is used for iterative multi-pass blur where each blur window
/// gets its own texture capturing everything behind it.
pub fn cache_blur_texture_for_window(
    output_name: &str,
    window_key: &CosmicMappedKey,
    info: BlurredTextureInfo,
) {
    let key = (output_name.to_string(), window_key_hash(window_key));
    if let Ok(mut cache) = BLUR_TEXTURE_CACHE.write() {
        cache.insert(key, info);
    }
}

/// Get the cached blurred texture for a specific window on an output.
pub fn get_cached_blur_texture_for_window(
    output_name: &str,
    window_key: &CosmicMappedKey,
) -> Option<BlurredTextureInfo> {
    let key = (output_name.to_string(), window_key_hash(window_key));
    if let Ok(cache) = BLUR_TEXTURE_CACHE.read() {
        cache.get(&key).cloned()
    } else {
        None
    }
}

/// Clear all blur textures for an output (e.g., on window close)
pub fn clear_blur_textures_for_output(output_name: &str) {
    if let Ok(mut cache) = BLUR_TEXTURE_CACHE.write() {
        cache.retain(|(out, _), _| out != output_name);
    }
}

// =============================================================================
// Layer Surface Blur Cache
// =============================================================================

/// Cache key for layer surface blur: (output_name, surface_id)
type LayerBlurCacheKey = (String, ObjectId);

/// Global cache of blurred textures for layer surfaces per output.
pub static LAYER_BLUR_TEXTURE_CACHE: LazyLock<
    RwLock<HashMap<LayerBlurCacheKey, BlurredTextureInfo>>,
> = LazyLock::new(|| RwLock::new(HashMap::new()));

/// Store a blurred texture for a specific layer surface on an output.
pub fn cache_blur_texture_for_layer(
    output_name: &str,
    surface_id: ObjectId,
    info: BlurredTextureInfo,
) {
    let key = (output_name.to_string(), surface_id);
    if let Ok(mut cache) = LAYER_BLUR_TEXTURE_CACHE.write() {
        cache.insert(key, info);
    }
}

/// Get the cached blurred texture for a specific layer surface on an output.
pub fn get_cached_blur_texture_for_layer(
    output_name: &str,
    surface_id: &ObjectId,
) -> Option<BlurredTextureInfo> {
    let key = (output_name.to_string(), surface_id.clone());
    if let Ok(cache) = LAYER_BLUR_TEXTURE_CACHE.read() {
        cache.get(&key).cloned()
    } else {
        None
    }
}

/// Clear all layer blur textures for an output
pub fn clear_layer_blur_textures_for_output(output_name: &str) {
    if let Ok(mut cache) = LAYER_BLUR_TEXTURE_CACHE.try_write() {
        cache.retain(|(out, _), _| out != output_name);
    }
}

/// Information about a layer surface with blur, cached from the main thread.
#[derive(Debug, Clone)]
pub struct LayerBlurSurfaceInfo {
    /// Surface ObjectId for cache key
    pub surface_id: ObjectId,
    /// Geometry of the layer surface
    pub geometry: Rectangle<i32, Logical>,
    /// Layer type (Bottom, Top, Overlay, Background)
    pub layer: smithay::wayland::shell::wlr_layer::Layer,
}

/// Cached layer surface info for rendering (includes the actual LayerSurface).
/// Used by render thread to avoid calling layer_map_for_output.
#[derive(Clone)]
pub struct CachedLayerSurface {
    /// The layer surface (cloned for thread safety)
    pub surface: smithay::desktop::LayerSurface,
    /// Location relative to output
    pub location: Point<i32, Logical>,
}

/// Convert Layer enum to a hashable u8 key
fn layer_to_key(layer: smithay::wayland::shell::wlr_layer::Layer) -> u8 {
    use smithay::wayland::shell::wlr_layer::Layer;
    match layer {
        Layer::Background => 0,
        Layer::Bottom => 1,
        Layer::Top => 2,
        Layer::Overlay => 3,
    }
}

/// Cache tracking ALL layer surfaces per output, organized by layer type.
/// Updated from the main thread when layer surfaces change, read by render thread.
/// Key: (output_name, layer_key), Value: list of cached layer surfaces
pub static OUTPUT_LAYER_SURFACES: LazyLock<RwLock<HashMap<(String, u8), Vec<CachedLayerSurface>>>> =
    LazyLock::new(|| RwLock::new(HashMap::new()));

/// Get cached layer surfaces for a specific layer on an output (non-blocking).
/// Returns empty vec if lock can't be acquired.
pub fn get_cached_layer_surfaces(
    output_name: &str,
    layer: smithay::wayland::shell::wlr_layer::Layer,
) -> Vec<CachedLayerSurface> {
    if let Ok(cache) = OUTPUT_LAYER_SURFACES.try_read() {
        cache
            .get(&(output_name.to_string(), layer_to_key(layer)))
            .cloned()
            .unwrap_or_default()
    } else {
        Vec::new()
    }
}

/// Update the cached layer surfaces for a specific layer on an output.
/// Called from the main thread when layer surfaces change.
pub fn set_cached_layer_surfaces(
    output_name: &str,
    layer: smithay::wayland::shell::wlr_layer::Layer,
    surfaces: Vec<CachedLayerSurface>,
) {
    if let Ok(mut cache) = OUTPUT_LAYER_SURFACES.try_write() {
        let key = (output_name.to_string(), layer_to_key(layer));
        if surfaces.is_empty() {
            cache.remove(&key);
        } else {
            cache.insert(key, surfaces);
        }
    }
}

/// Clear all cached layer surfaces for an output.
pub fn clear_cached_layer_surfaces(output_name: &str) {
    if let Ok(mut cache) = OUTPUT_LAYER_SURFACES.try_write() {
        cache.retain(|(out, _), _| out != output_name);
    }
}

/// Cache tracking layer surfaces with blur for each output.
/// Updated from the main thread when layer surfaces change, read by render thread.
/// Key: output_name, Value: list of layer blur surface info
pub static OUTPUT_LAYER_BLUR_SURFACES: LazyLock<
    RwLock<HashMap<String, Vec<LayerBlurSurfaceInfo>>>,
> = LazyLock::new(|| RwLock::new(HashMap::new()));

/// Check if an output has any layer surfaces with blur (non-blocking).
/// Returns false if the lock can't be acquired (safe default for render thread).
pub fn output_has_layer_blur(output_name: &str) -> bool {
    if let Ok(cache) = OUTPUT_LAYER_BLUR_SURFACES.try_read() {
        cache
            .get(output_name)
            .map(|v| !v.is_empty())
            .unwrap_or(false)
    } else {
        false
    }
}

/// Get cached layer blur surface info for an output (non-blocking).
/// Returns empty vec if lock can't be acquired.
pub fn get_layer_blur_surfaces(output_name: &str) -> Vec<LayerBlurSurfaceInfo> {
    if let Ok(cache) = OUTPUT_LAYER_BLUR_SURFACES.try_read() {
        cache.get(output_name).cloned().unwrap_or_default()
    } else {
        Vec::new()
    }
}

/// Update the layer blur surfaces for an output.
/// Called from the main thread when layer surfaces are added/removed/changed.
pub fn set_layer_blur_surfaces(output_name: &str, surfaces: Vec<LayerBlurSurfaceInfo>) {
    if let Ok(mut cache) = OUTPUT_LAYER_BLUR_SURFACES.try_write() {
        if surfaces.is_empty() {
            cache.remove(output_name);
        } else {
            cache.insert(output_name.to_string(), surfaces);
        }
    }
}

/// Cache for layer blur content state: output_name -> content_hash
/// Used to detect when background content has changed and blur needs re-rendering
pub static LAYER_BLUR_CONTENT_STATE: LazyLock<RwLock<HashMap<String, u64>>> =
    LazyLock::new(|| RwLock::new(HashMap::new()));

/// Get the stored content hash for layer blur on an output
pub fn get_layer_blur_content_hash(output_name: &str) -> Option<u64> {
    if let Ok(cache) = LAYER_BLUR_CONTENT_STATE.read() {
        cache.get(output_name).copied()
    } else {
        None
    }
}

/// Store the content hash for layer blur after rendering
pub fn store_layer_blur_content_hash(output_name: &str, hash: u64) {
    if let Ok(mut cache) = LAYER_BLUR_CONTENT_STATE.write() {
        cache.insert(output_name.to_string(), hash);
    }
}

// =============================================================================
// Content Hash for Cache Invalidation
// =============================================================================

/// Compute a hash of element content state (commit counters + geometry).
/// Commit counters change when surfaces commit new content (terminal updates, etc.)
///
/// NOTE: We intentionally DO NOT include element IDs in the hash because some
/// render elements (like shadows, decorations) get recreated each frame with
/// new ExternalIds. We rely on commit counters + geometry for change detection.
pub fn compute_element_content_hash<E: Element>(
    z_threshold: usize,
    elements: &[E],
    scale: Scale<f64>,
) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut hasher = std::collections::hash_map::DefaultHasher::new();

    // Include z-threshold
    z_threshold.hash(&mut hasher);

    // Include element count
    elements.len().hash(&mut hasher);

    // Include each element's commit counter (changes on content update) and geometry
    for elem in elements {
        // Commit counter - THIS changes when surface content updates
        // CommitCounter doesn't implement Hash, so we use Debug format
        let commit = elem.current_commit();
        let commit_str = format!("{:?}", commit);
        commit_str.hash(&mut hasher);

        // Also include geometry
        let geo = elem.geometry(scale);
        geo.loc.x.hash(&mut hasher);
        geo.loc.y.hash(&mut hasher);
        geo.size.w.hash(&mut hasher);
        geo.size.h.hash(&mut hasher);
    }

    hasher.finish()
}

/// Get the stored content hash for a blur group
pub fn get_blur_group_content_hash(output_name: &str, capture_z_threshold: usize) -> Option<u64> {
    let key = (output_name.to_string(), capture_z_threshold);
    if let Ok(cache) = BLUR_GROUP_CONTENT_STATE.read() {
        cache.get(&key).copied()
    } else {
        None
    }
}

/// Store the content hash for a blur group after rendering
pub fn store_blur_group_content_hash(output_name: &str, capture_z_threshold: usize, hash: u64) {
    let key = (output_name.to_string(), capture_z_threshold);
    if let Ok(mut cache) = BLUR_GROUP_CONTENT_STATE.write() {
        cache.insert(key, hash);
    }
}

/// Get the last blur update time for a group
pub fn get_blur_group_last_update(
    output_name: &str,
    capture_z_threshold: usize,
) -> Option<Instant> {
    let key = (output_name.to_string(), capture_z_threshold);
    if let Ok(cache) = BLUR_GROUP_LAST_UPDATE.read() {
        cache.get(&key).copied()
    } else {
        None
    }
}

/// Store the last blur update time for a group
pub fn store_blur_group_last_update(output_name: &str, capture_z_threshold: usize) {
    let key = (output_name.to_string(), capture_z_threshold);
    if let Ok(mut cache) = BLUR_GROUP_LAST_UPDATE.write() {
        cache.insert(key, Instant::now());
    }
}

/// Check if blur should be throttled (content changed but update was too recent).
/// Returns true if we should skip re-blurring this frame.
pub fn should_throttle_blur(output_name: &str, capture_z_threshold: usize) -> bool {
    if let Some(last_update) = get_blur_group_last_update(output_name, capture_z_threshold) {
        last_update.elapsed() < BLUR_THROTTLE_INTERVAL
    } else {
        false
    }
}

/// Copy a blur texture to a new texture for caching.
///
/// This is necessary because multiple blur groups share the same ping/pong textures.
/// After blurring group 0, we must copy the result to a dedicated texture before
/// processing group 1, otherwise group 1's blur will overwrite group 0's cached texture.
///
/// Returns a new `TextureRenderBuffer` containing a copy of the source texture.
pub fn copy_blur_texture_for_cache<R>(
    renderer: &mut R,
    source: &TextureRenderBuffer<GlesTexture>,
    size: Size<i32, Physical>,
) -> Result<TextureRenderBuffer<GlesTexture>, GlesError>
where
    R: Renderer + Bind<Dmabuf> + Offscreen<GlesTexture> + AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
{
    use smithay::backend::renderer::{Color32F, Frame as FrameTrait};
    use smithay::utils::Buffer as BufferCoords;

    // Create a new texture buffer for the copy
    let new_texture = Offscreen::<GlesTexture>::create_buffer(
        renderer,
        Fourcc::Abgr8888,
        Size::from((size.w, size.h)),
    )
    .map_err(|_| GlesError::UnknownSize)?;

    let mut new_buffer = TextureRenderBuffer::from_texture(
        renderer.glow_renderer(),
        new_texture,
        1,
        Transform::Normal,
        None,
    );

    // Create a texture element from the source at position (0, 0)
    let src_elem = TextureRenderElement::from_texture_render_buffer(
        Point::<f64, Physical>::from((0.0, 0.0)),
        source,
        Some(1.0),
        None,                                               // No src_rect - use full texture
        Some(Size::<i32, Logical>::from((size.w, size.h))), // Same size
        Kind::Unspecified,
    );

    new_buffer.render().draw::<_, GlesError>(|tex| {
        let glow = renderer.glow_renderer_mut();
        let mut target = glow.bind(tex)?;

        use smithay::backend::renderer::Renderer as RendererTrait;
        let mut frame = glow.render(&mut target, size, Transform::Normal)?;

        // Clear and render the source texture
        use smithay::backend::renderer::element::RenderElement;
        use smithay::backend::renderer::glow::GlowRenderer;

        let clear_damage = [Rectangle::from_size(size)];
        frame.clear(Color32F::from([0.0, 0.0, 0.0, 0.0]), &clear_damage)?;

        // Source: full texture in buffer coordinates
        let src: Rectangle<f64, BufferCoords> =
            Rectangle::from_size((size.w as f64, size.h as f64).into());
        // Destination: full output in physical coordinates
        let dst: Rectangle<i32, Physical> = Rectangle::from_size(size);
        let damage = [dst];

        <TextureRenderElement<GlesTexture> as RenderElement<GlowRenderer>>::draw(
            &src_elem,
            &mut frame,
            src,
            dst,
            &damage,
            &[],
        )?;

        drop(frame);

        // Return damage in buffer coordinates
        let buffer_size: Size<i32, Logical> = size.to_logical(1);
        let damage_rect = Rectangle::from_size(size);
        Ok(vec![damage_rect.to_logical(1).to_buffer(
            1,
            Transform::Normal,
            &buffer_size,
        )])
    })?;

    Ok(new_buffer)
}

/// Compute a hash of the background state for blur cache invalidation.
/// This hash changes when:
/// - The z-threshold changes (different windows visible)
/// - Any element's geometry changes (position/size)
/// - Element count changes
pub fn compute_background_state_hash<E: Element>(
    z_threshold: usize,
    elements: &[E],
    scale: Scale<f64>,
) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut hasher = std::collections::hash_map::DefaultHasher::new();

    // Include z-threshold
    z_threshold.hash(&mut hasher);

    // Include element count
    elements.len().hash(&mut hasher);

    // Include each element's geometry (position + size)
    for elem in elements {
        let geo = elem.geometry(scale);
        geo.loc.x.hash(&mut hasher);
        geo.loc.y.hash(&mut hasher);
        geo.size.w.hash(&mut hasher);
        geo.size.h.hash(&mut hasher);
    }

    hasher.finish()
}

// =============================================================================
// Blur Capture Context
// =============================================================================

/// Context for blur capture rendering.
/// This replaces the thread-local globals and is passed through the render pipeline.
#[derive(Clone, PartialEq)]
pub struct BlurCaptureContext {
    /// Z-index threshold: windows at or above this index will be excluded from capture
    pub exclude_z_threshold: usize,
    /// Window key being grabbed/moved - should be excluded from blur capture
    pub grabbed_window_key: Option<CosmicMappedKey>,
}

impl BlurCaptureContext {
    /// Create a new blur capture context
    pub fn new(exclude_z_threshold: usize, grabbed_window_key: Option<CosmicMappedKey>) -> Self {
        Self {
            exclude_z_threshold,
            grabbed_window_key,
        }
    }

    /// Check if a window at the given z-index should be excluded
    pub fn is_z_index_excluded(&self, z_idx: usize) -> bool {
        z_idx >= self.exclude_z_threshold
    }

    /// Check if a window is the grabbed window (by key)
    pub fn is_window_grabbed(&self, window: &CosmicMapped) -> bool {
        if let Some(ref grabbed_key) = self.grabbed_window_key {
            window.key() == *grabbed_key
        } else {
            false
        }
    }
}

// =============================================================================
// Blur Processing Functions
// =============================================================================

/// Downsample a texture by rendering it to a smaller target.
/// This uses GPU bilinear filtering for high-quality downscaling.
pub fn downsample_texture<R>(
    renderer: &mut R,
    src_texture: &TextureRenderBuffer<GlesTexture>,
    dst_texture: &mut TextureRenderBuffer<GlesTexture>,
    src_size: Size<i32, Physical>,
    dst_size: Size<i32, Physical>,
) -> Result<(), GlesError>
where
    R: Renderer + Bind<Dmabuf> + Offscreen<GlesTexture> + AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
{
    let _span = tracing::debug_span!(
        "downsample_texture",
        src_w = src_size.w,
        src_h = src_size.h,
        dst_w = dst_size.w,
        dst_h = dst_size.h,
    )
    .entered();

    // Create a texture element from the source at position (0, 0)
    // The element will be rendered to fill the destination texture
    let src_elem = TextureRenderElement::from_texture_render_buffer(
        Point::<f64, Physical>::from((0.0, 0.0)),
        src_texture,
        Some(1.0),
        None, // No src_rect - use full texture
        Some(Size::<i32, Logical>::from((dst_size.w, dst_size.h))), // Scale to dst size
        Kind::Unspecified,
    );

    dst_texture.render().draw::<_, GlesError>(|tex| {
        let glow = renderer.glow_renderer_mut();
        let mut target = glow.bind(tex)?;

        use smithay::backend::renderer::Renderer as RendererTrait;
        let mut frame = glow.render(&mut target, dst_size, Transform::Normal)?;

        // Clear and render the downsampled texture
        use smithay::backend::renderer::element::RenderElement;
        use smithay::backend::renderer::glow::GlowRenderer;
        use smithay::backend::renderer::{Color32F, Frame as FrameTrait};
        use smithay::utils::Buffer as BufferCoords;

        let clear_damage = [Rectangle::from_size(dst_size)];
        frame.clear(Color32F::from([0.0, 0.0, 0.0, 0.0]), &clear_damage)?;

        // Source: full texture in buffer coordinates (original size)
        let src: Rectangle<f64, BufferCoords> =
            Rectangle::from_size((src_size.w as f64, src_size.h as f64).into());
        // Destination: full output in physical coordinates (downsampled size)
        let dst: Rectangle<i32, Physical> = Rectangle::from_size(dst_size);
        let damage = [dst];

        <TextureRenderElement<GlesTexture> as RenderElement<GlowRenderer>>::draw(
            &src_elem,
            &mut frame,
            src,
            dst,
            &damage,
            &[],
        )?;

        drop(frame);

        // Return damage in buffer coordinates
        let buffer_size: Size<i32, Logical> = dst_size.to_logical(1);
        let damage_rect = Rectangle::from_size(dst_size);
        Ok(vec![damage_rect.to_logical(1).to_buffer(
            1,
            Transform::Normal,
            &buffer_size,
        )])
    })?;

    Ok(())
}

/// Apply Kawase blur passes using ping-pong rendering.
///
/// - `renderer`: The renderer to use
/// - `src_texture`: Source texture to blur (will not be modified)
/// - `ping_texture`: First intermediate texture
/// - `pong_texture`: Second intermediate texture (will contain final result)
/// - `tex_size`: Size of all textures
/// - `scale`: Scale factor for damage tracking
/// - `iterations`: Number of blur iterations
///
/// After this function, `pong_texture` contains the blurred result.
pub fn apply_blur_passes<R>(
    renderer: &mut R,
    src_texture: &TextureRenderBuffer<GlesTexture>,
    ping_texture: &mut TextureRenderBuffer<GlesTexture>,
    pong_texture: &mut TextureRenderBuffer<GlesTexture>,
    tex_size: Size<i32, Physical>,
    scale: Scale<f64>,
    iterations: u32,
) -> Result<(), GlesError>
where
    R: Renderer + Bind<Dmabuf> + Offscreen<GlesTexture> + AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
{
    let _span = tracing::info_span!(
        "blur_passes",
        iterations = iterations,
        tex_w = tex_size.w,
        tex_h = tex_size.h,
    )
    .entered();

    let blur_shader = super::BlurShader::get(renderer);
    let blur_radius = DEFAULT_BLUR_RADIUS;

    // First pass: src -> ping (horizontal blur)
    apply_single_blur_pass(
        renderer,
        src_texture,
        ping_texture,
        &blur_shader,
        blur_radius,
        tex_size,
        scale,
        true,
    )?;

    // Second pass: ping -> pong (vertical blur)
    apply_single_blur_pass(
        renderer,
        ping_texture,
        pong_texture,
        &blur_shader,
        blur_radius,
        tex_size,
        scale,
        false,
    )?;

    // Additional iterations for smoother blur
    for _ in 1..iterations {
        // Horizontal: pong -> ping
        apply_single_blur_pass(
            renderer,
            pong_texture,
            ping_texture,
            &blur_shader,
            blur_radius,
            tex_size,
            scale,
            true,
        )?;

        // Vertical: ping -> pong
        apply_single_blur_pass(
            renderer,
            ping_texture,
            pong_texture,
            &blur_shader,
            blur_radius,
            tex_size,
            scale,
            false,
        )?;
    }

    tracing::debug!(
        iterations = iterations,
        tex_w = tex_size.w,
        tex_h = tex_size.h,
        blur_radius = blur_radius,
        "Blur passes complete"
    );

    Ok(())
}

/// Apply a single blur pass from src to dst
fn apply_single_blur_pass<R>(
    renderer: &mut R,
    src_texture: &TextureRenderBuffer<GlesTexture>,
    dst_texture: &mut TextureRenderBuffer<GlesTexture>,
    blur_shader: &GlesTexProgram,
    blur_radius: f32,
    tex_size: Size<i32, Physical>,
    _scale: Scale<f64>,
    horizontal: bool,
) -> Result<(), GlesError>
where
    R: Renderer + Bind<Dmabuf> + Offscreen<GlesTexture> + AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
{
    tracing::trace!(
        horizontal = horizontal,
        blur_radius = blur_radius,
        tex_size_w = tex_size.w,
        tex_size_h = tex_size.h,
        "Applying single blur pass"
    );

    let src_elem = TextureRenderElement::from_texture_render_buffer(
        Point::<f64, Physical>::from((0.0, 0.0)),
        src_texture,
        Some(1.0),
        None,
        None,
        Kind::Unspecified,
    );

    let blur_elem = TextureShaderElement::new(
        src_elem,
        blur_shader.clone(),
        vec![
            Uniform::new("tex_size", [tex_size.w as f32, tex_size.h as f32]),
            Uniform::new("blur_radius", blur_radius),
            Uniform::new("direction", if horizontal { 0.0 } else { 1.0 }),
        ],
    );

    dst_texture.render().draw::<_, GlesError>(|tex| {
        let glow = renderer.glow_renderer_mut();
        let mut target = glow.bind(tex)?;

        // Get a frame to render with
        use smithay::backend::renderer::Renderer as RendererTrait;
        let mut frame = glow.render(&mut target, tex_size, Transform::Normal)?;

        // Clear the framebuffer first
        use smithay::backend::renderer::Color32F;
        use smithay::backend::renderer::Frame as FrameTrait;
        let clear_damage = [Rectangle::from_size(tex_size)];
        frame.clear(Color32F::from([0.0, 0.0, 0.0, 0.0]), &clear_damage)?;

        // Render the blur element using the RenderElement trait
        use smithay::backend::renderer::element::RenderElement;
        use smithay::backend::renderer::glow::GlowRenderer;
        use smithay::utils::Buffer as BufferCoords;

        // Source: full texture in buffer coordinates
        let src: Rectangle<f64, BufferCoords> =
            Rectangle::from_size((tex_size.w as f64, tex_size.h as f64).into());
        // Destination: full output in physical coordinates
        let dst: Rectangle<i32, Physical> = Rectangle::from_size(tex_size);
        let damage = [dst];

        <TextureShaderElement as RenderElement<GlowRenderer>>::draw(
            &blur_elem,
            &mut frame,
            src,
            dst,
            &damage,
            &[],
        )?;

        // Finish the frame
        drop(frame);

        tracing::trace!(horizontal = horizontal, "Blur pass render completed");

        // Return damage in buffer coordinates
        let buffer_size: Size<i32, Logical> = tex_size.to_logical(1);
        let damage_rect = Rectangle::from_size(tex_size);
        Ok(vec![damage_rect.to_logical(1).to_buffer(
            1,
            Transform::Normal,
            &buffer_size,
        )])
    })?;

    Ok(())
}

// =============================================================================
// HasBlur Trait
// =============================================================================

/// Trait for elements that may have blur enabled
pub trait HasBlur {
    /// Check if this element has blur enabled
    fn has_blur(&self) -> bool;
}
