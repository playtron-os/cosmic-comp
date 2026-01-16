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

use super::element::AsGlowRenderer;
use crate::shell::element::{CosmicMapped, CosmicMappedKey};

// =============================================================================
// Constants
// =============================================================================

/// Default blur radius in pixels (design spec: Figma blur 100)
/// Figma blur 100 ≈ Gaussian σ ≈ 25–30px, effective diameter ~100–120px.
/// This value is passed to the shader and scaled by the offset divisor.
pub const DEFAULT_BLUR_RADIUS: f32 = 100.0;

/// Number of blur iterations for Figma-quality blur.
/// Kawase blur needs many passes for large radii. For Figma blur 100:
/// - Minimum: 16 passes
/// - Good: 20-24 passes
/// 24 iterations at 1/8 resolution is still very performant.
pub const BLUR_ITERATIONS: u32 = 24;

/// Downsample factor for blur textures.
/// For Figma blur 100, we need heavy downsampling:
/// - Factor of 8 = 1/64 pixel count (1/8 in each dimension)
/// This massively increases perceived blur and is very GPU-efficient.
pub const BLUR_DOWNSAMPLE_FACTOR: i32 = 8;

// Blur backdrop styling constants
pub const BLUR_TINT_COLOR: [f32; 3] = [1.0, 1.0, 1.0];
/// Strength of the tint overlay (0.10 = 10% opacity)
pub const BLUR_TINT_STRENGTH: f32 = 0.10;
/// Alpha for fallback solid color when blur texture not available
pub const BLUR_FALLBACK_ALPHA: f32 = 0.25;
/// Fallback color when blur texture not available (light gray-blue)
pub const BLUR_FALLBACK_COLOR: [f32; 3] = [0.9, 0.9, 0.95];

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
#[derive(Debug)]
pub struct BlurRenderState {
    /// Texture containing the previous frame's content at full resolution (blur source)
    pub background_texture: Option<TextureRenderBuffer<GlesTexture>>,
    /// Downsampled texture for blur input (from background_texture)
    pub downsampled_texture: Option<TextureRenderBuffer<GlesTexture>>,
    /// Offscreen texture for blur passes (ping) - at reduced resolution
    pub texture_a: Option<TextureRenderBuffer<GlesTexture>>,
    /// Offscreen texture for blur passes (pong) - at reduced resolution
    pub texture_b: Option<TextureRenderBuffer<GlesTexture>>,
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
}

impl Default for BlurRenderState {
    fn default() -> Self {
        Self {
            background_texture: None,
            downsampled_texture: None,
            texture_a: None,
            texture_b: None,
            damage_tracker: None,
            screen_size: Size::from((0, 0)),
            texture_size: Size::from((0, 0)),
            scale: Scale::from(1.0),
            blur_applied: false,
        }
    }
}

impl BlurRenderState {
    /// Create or resize blur textures if needed.
    /// Creates full-size background texture and optionally downsampled blur textures.
    pub fn ensure_textures<R: AsGlowRenderer + Offscreen<GlesTexture>>(
        &mut self,
        renderer: &mut R,
        format: Fourcc,
        size: Size<i32, Physical>,
        scale: Scale<f64>,
    ) -> Result<(), R::Error> {
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
                && self.background_texture.is_some()
                && self.downsampled_texture.is_some()
        } else {
            self.screen_size == size
                && self.texture_a.is_some()
                && self.texture_b.is_some()
                && self.background_texture.is_some()
        };

        if textures_valid {
            return Ok(());
        }

        tracing::debug!(
            screen_w = size.w,
            screen_h = size.h,
            blur_w = blur_size.w,
            blur_h = blur_size.h,
            downsample_enabled = downsample_enabled,
            "Creating blur textures"
        );

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

        // Create damage tracker
        self.damage_tracker = Some(OutputDamageTracker::new(size, scale, Transform::Normal));

        self.screen_size = size;
        self.texture_size = blur_size;
        self.scale = scale;
        self.blur_applied = false;
        Ok(())
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
            && self.texture_b.is_some();

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
type LayerBlurCacheKey = (String, u32);

/// Global cache of blurred textures for layer surfaces per output.
pub static LAYER_BLUR_TEXTURE_CACHE: LazyLock<
    RwLock<HashMap<LayerBlurCacheKey, BlurredTextureInfo>>,
> = LazyLock::new(|| RwLock::new(HashMap::new()));

/// Store a blurred texture for a specific layer surface on an output.
pub fn cache_blur_texture_for_layer(output_name: &str, surface_id: u32, info: BlurredTextureInfo) {
    let key = (output_name.to_string(), surface_id);
    if let Ok(mut cache) = LAYER_BLUR_TEXTURE_CACHE.write() {
        cache.insert(key, info);
    }
}

/// Get the cached blurred texture for a specific layer surface on an output.
pub fn get_cached_blur_texture_for_layer(
    output_name: &str,
    surface_id: u32,
) -> Option<BlurredTextureInfo> {
    let key = (output_name.to_string(), surface_id);
    if let Ok(cache) = LAYER_BLUR_TEXTURE_CACHE.read() {
        cache.get(&key).cloned()
    } else {
        None
    }
}

/// Clear all layer blur textures for an output
pub fn clear_layer_blur_textures_for_output(output_name: &str) {
    if let Ok(mut cache) = LAYER_BLUR_TEXTURE_CACHE.write() {
        cache.retain(|(out, _), _| out != output_name);
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
        // Element ID for identity
        let id = elem.id();
        id.hash(&mut hasher);

        // Commit counter - THIS changes when surface content updates
        // CommitCounter doesn't implement Hash, so we use Debug format
        let commit = elem.current_commit();
        format!("{:?}", commit).hash(&mut hasher);

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
