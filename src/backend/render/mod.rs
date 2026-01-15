// SPDX-License-Identifier: GPL-3.0-only

use std::{
    borrow::Borrow,
    cell::RefCell,
    collections::HashMap,
    ops::ControlFlow,
    sync::{Arc, Weak},
    time::Instant,
};

#[cfg(feature = "debug")]
use crate::debug::fps_ui;
use crate::{
    backend::{
        kms::render::gles::GbmGlowBackend,
        render::{
            clipped_surface::{CLIPPING_SHADER, ClippingShader},
            element::DamageElement,
            shadow::{SHADOW_SHADER, ShadowShader},
        },
    },
    config::ScreenFilter,
    shell::{
        CosmicMapped, CosmicMappedRenderElement, OverviewMode, SeatExt, Trigger, WorkspaceDelta,
        WorkspaceRenderElement,
        element::CosmicMappedKey,
        focus::{FocusTarget, Stage, render_input_order, target::WindowGroup},
        grabs::{SeatMenuGrabState, SeatMoveGrabState},
        layout::tiling::ANIMATION_DURATION,
        zoom::ZoomState,
    },
    utils::{prelude::*, quirks::workspace_overview_is_open},
    wayland::{
        handlers::{
            compositor::FRAME_TIME_FILTER,
            data_device::get_dnd_icon,
            screencopy::{FrameHolder, SessionData, render_session},
        },
        protocols::workspace::WorkspaceHandle,
    },
};

use cosmic::Theme;
use element::FromGlesError;
use smithay::{
    backend::{
        allocator::{Fourcc, dmabuf::Dmabuf},
        drm::{DrmDeviceFd, DrmNode},
        renderer::{
            Bind, Blit, Color32F, ExportMem, ImportAll, ImportMem, Offscreen, Renderer, Texture,
            TextureFilter,
            damage::{Error as RenderError, OutputDamageTracker, RenderOutputResult},
            element::{
                Element, Id, Kind, RenderElement, WeakId,
                surface::{WaylandSurfaceRenderElement, render_elements_from_surface_tree},
                texture::{TextureRenderBuffer, TextureRenderElement},
                utils::{
                    ConstrainAlign, ConstrainScaleBehavior, CropRenderElement, Relocate,
                    RelocateRenderElement, RescaleRenderElement, constrain_render_elements,
                },
            },
            gles::{
                GlesError, GlesPixelProgram, GlesRenderer, GlesTexProgram, GlesTexture, Uniform,
                UniformName, UniformType,
                element::{PixelShaderElement, TextureShaderElement},
            },
            glow::GlowRenderer,
            multigpu::{Error as MultiError, MultiFrame, MultiRenderer},
            sync::SyncPoint,
        },
    },
    input::Seat,
    output::{Output, OutputModeSource, OutputNoMode},
    reexports::wayland_server::Resource,
    utils::{
        IsAlive, Logical, Monotonic, Physical, Point, Rectangle, Scale, Size, Time, Transform,
    },
    wayland::{dmabuf::get_dmabuf, seat::WaylandFocus, session_lock::LockSurface},
};

#[cfg(feature = "debug")]
use smithay_egui::EguiState;

pub mod animations;
pub mod blur;
pub mod clipped_surface;

pub mod cursor;
pub mod element;
pub mod shadow;
use self::element::{AsGlowRenderer, CosmicElement};

use super::kms::Timings;

pub type GlMultiRenderer<'a> =
    MultiRenderer<'a, 'a, GbmGlowBackend<DrmDeviceFd>, GbmGlowBackend<DrmDeviceFd>>;
pub type GlMultiFrame<'a, 'frame, 'buffer> =
    MultiFrame<'a, 'a, 'frame, 'buffer, GbmGlowBackend<DrmDeviceFd>, GbmGlowBackend<DrmDeviceFd>>;
pub type GlMultiError = MultiError<GbmGlowBackend<DrmDeviceFd>, GbmGlowBackend<DrmDeviceFd>>;

pub enum RendererRef<'a> {
    Glow(&'a mut GlowRenderer),
    GlMulti(GlMultiRenderer<'a>),
}

impl AsRef<GlowRenderer> for RendererRef<'_> {
    fn as_ref(&self) -> &GlowRenderer {
        match self {
            Self::Glow(renderer) => renderer,
            Self::GlMulti(renderer) => renderer.as_ref(),
        }
    }
}

impl AsMut<GlowRenderer> for RendererRef<'_> {
    fn as_mut(&mut self) -> &mut GlowRenderer {
        match self {
            Self::Glow(renderer) => renderer,
            Self::GlMulti(renderer) => renderer.as_mut(),
        }
    }
}

pub static CLEAR_COLOR: Color32F = Color32F::new(0.153, 0.161, 0.165, 1.0);
pub static OUTLINE_SHADER: &str = include_str!("./shaders/rounded_outline.frag");
pub static RECTANGLE_SHADER: &str = include_str!("./shaders/rounded_rectangle.frag");
pub static POSTPROCESS_SHADER: &str = include_str!("./shaders/offscreen.frag");
pub static BLUR_SHADER: &str = include_str!("./shaders/blur.frag");
pub static BLURRED_BACKDROP_SHADER: &str = include_str!("./shaders/blurred_backdrop.frag");
pub static GROUP_COLOR: [f32; 3] = [0.788, 0.788, 0.788];
pub static ACTIVE_GROUP_COLOR: [f32; 3] = [0.58, 0.922, 0.922];

// Blur module re-exports
pub use blur::{
    BLUR_DOWNSAMPLE_FACTOR, BLUR_FALLBACK_ALPHA, BLUR_FALLBACK_COLOR, BLUR_ITERATIONS,
    BLUR_TINT_COLOR, BLUR_TINT_STRENGTH, BlurCaptureContext, BlurRenderState, BlurredTextureInfo,
    DEFAULT_BLUR_RADIUS, HasBlur, apply_blur_passes, blur_downsample_enabled,
    cache_blur_texture_for_window, clear_blur_textures_for_output, compute_element_content_hash,
    downsample_texture, get_blur_group_content_hash, get_cached_blur_texture_for_window,
    store_blur_group_content_hash,
};

/// Shader for applying blur effects to surfaces
pub struct BlurShader(pub GlesTexProgram);

/// Shader for rendering blurred backdrop (samples from pre-blurred texture)
pub struct BlurredBackdropShader(pub GlesTexProgram);

pub struct IndicatorShader(pub GlesPixelProgram);

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Usage {
    OverviewBackdrop,
    Overlay,
    MoveGrabIndicator,
    FocusIndicator,
    PotentialGroupIndicator,
    SnappingIndicator,
    Border,
}

#[derive(Clone)]
pub enum Key {
    Static(WeakId),
    Group(Weak<()>),
    Window(Usage, CosmicMappedKey),
}
impl std::hash::Hash for Key {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Key::Static(id) => id.hash(state),
            Key::Group(arc) => (arc.as_ptr() as usize).hash(state),
            Key::Window(usage, window) => {
                usage.hash(state);
                window.hash(state);
            }
        }
    }
}
impl PartialEq for Key {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Key::Static(s1), Key::Static(s2)) => s1 == s2,
            (Key::Group(g1), Key::Group(g2)) => Weak::ptr_eq(g1, g2),
            (Key::Window(u1, w1), Key::Window(u2, w2)) => u1 == u2 && w1 == w2,
            _ => false,
        }
    }
}
impl Eq for Key {}
impl From<WindowGroup> for Key {
    fn from(group: WindowGroup) -> Self {
        Key::Group(group.alive.clone())
    }
}
impl From<Id> for Key {
    fn from(id: Id) -> Self {
        Key::Static(id.downgrade())
    }
}

#[derive(PartialEq)]
struct IndicatorSettings {
    thickness: u8,
    outer_radius: [u8; 4],
    alpha: f32,
    color: [f32; 3],
    scale: f64,
}
type IndicatorCache = RefCell<HashMap<Key, (IndicatorSettings, PixelShaderElement)>>;

impl IndicatorShader {
    pub fn get<R: AsGlowRenderer>(renderer: &R) -> GlesPixelProgram {
        Borrow::<GlesRenderer>::borrow(renderer.glow_renderer())
            .egl_context()
            .user_data()
            .get::<IndicatorShader>()
            .expect("Custom Shaders not initialized")
            .0
            .clone()
    }

    pub fn focus_element<R: AsGlowRenderer>(
        renderer: &R,
        key: impl Into<Key>,
        mut element_geo: Rectangle<i32, Local>,
        thickness: u8,
        inner_radius: [u8; 4],
        alpha: f32,
        scale: f64,
        active_window_hint: [f32; 3],
    ) -> PixelShaderElement {
        let t = thickness as i32;
        element_geo.loc -= (t, t).into();
        element_geo.size += (t * 2, t * 2).into();
        let outer_radius = inner_radius.map(|r| r + thickness);

        IndicatorShader::element(
            renderer,
            key,
            element_geo,
            thickness,
            outer_radius,
            alpha,
            scale,
            active_window_hint,
        )
    }

    pub fn element<R: AsGlowRenderer>(
        renderer: &R,
        key: impl Into<Key>,
        geo: Rectangle<i32, Local>,
        thickness: u8,
        outer_radius: [u8; 4],
        alpha: f32,
        scale: f64,
        color: [f32; 3],
    ) -> PixelShaderElement {
        let settings = IndicatorSettings {
            thickness,
            outer_radius,
            alpha,
            scale,
            color,
        };

        let user_data = Borrow::<GlesRenderer>::borrow(renderer.glow_renderer())
            .egl_context()
            .user_data();

        user_data.insert_if_missing(|| IndicatorCache::new(HashMap::new()));
        let mut cache = user_data.get::<IndicatorCache>().unwrap().borrow_mut();
        cache.retain(|k, _| match k {
            Key::Static(w) => w.upgrade().is_some(),
            Key::Group(w) => w.upgrade().is_some(),
            Key::Window(_, w) => w.alive(),
        });

        let key = key.into();
        if cache
            .get(&key)
            .filter(|(old_settings, _)| &settings == old_settings)
            .is_none()
        {
            let thickness: f32 = ((thickness as f64 * scale).ceil() / scale) as f32;
            let shader = Self::get(renderer);

            let elem = PixelShaderElement::new(
                shader,
                geo.as_logical(),
                None, //TODO
                alpha,
                vec![
                    Uniform::new(
                        "color",
                        [color[0] * alpha, color[1] * alpha, color[2] * alpha],
                    ),
                    Uniform::new("thickness", thickness),
                    Uniform::new(
                        "radius",
                        [
                            outer_radius[3] as f32,
                            outer_radius[1] as f32,
                            outer_radius[0] as f32,
                            outer_radius[2] as f32,
                        ],
                    ),
                    Uniform::new("scale", scale as f32),
                ],
                Kind::Unspecified,
            );
            cache.insert(key.clone(), (settings, elem));
        }

        let elem = &mut cache.get_mut(&key).unwrap().1;
        if elem.geometry(1.0.into()).to_logical(1) != geo.as_logical() {
            elem.resize(geo.as_logical(), None);
        }
        elem.clone()
    }
}

pub struct BackdropShader(pub GlesPixelProgram);

#[derive(PartialEq)]
struct BackdropSettings {
    corner_radius: [f32; 4],
    alpha: f32,
    color: [f32; 3],
}
type BackdropCache = RefCell<HashMap<Key, (BackdropSettings, PixelShaderElement)>>;

impl BackdropShader {
    pub fn get<R: AsGlowRenderer>(renderer: &R) -> GlesPixelProgram {
        Borrow::<GlesRenderer>::borrow(renderer.glow_renderer())
            .egl_context()
            .user_data()
            .get::<BackdropShader>()
            .expect("Custom Shaders not initialized")
            .0
            .clone()
    }

    pub fn element<R: AsGlowRenderer>(
        renderer: &R,
        key: impl Into<Key>,
        geo: Rectangle<i32, Local>,
        corner_radius: [f32; 4],
        alpha: f32,
        color: [f32; 3],
    ) -> PixelShaderElement {
        let settings = BackdropSettings {
            corner_radius,
            alpha,
            color,
        };

        let user_data = Borrow::<GlesRenderer>::borrow(renderer.glow_renderer())
            .egl_context()
            .user_data();

        user_data.insert_if_missing(|| BackdropCache::new(HashMap::new()));
        let mut cache = user_data.get::<BackdropCache>().unwrap().borrow_mut();
        cache.retain(|k, _| match k {
            Key::Static(w) => w.upgrade().is_some(),
            Key::Group(a) => a.upgrade().is_some(),
            Key::Window(_, w) => w.alive(),
        });

        let key = key.into();
        if cache
            .get(&key)
            .filter(|(old_settings, _)| &settings == old_settings)
            .is_none()
        {
            let shader = Self::get(renderer);

            let elem = PixelShaderElement::new(
                shader,
                geo.as_logical(),
                None, // TODO
                alpha,
                vec![
                    Uniform::new(
                        "color",
                        [color[0] * alpha, color[1] * alpha, color[2] * alpha],
                    ),
                    Uniform::new("corner_radius_tl", corner_radius[0]),
                    Uniform::new("corner_radius_tr", corner_radius[1]),
                    Uniform::new("corner_radius_br", corner_radius[2]),
                    Uniform::new("corner_radius_bl", corner_radius[3]),
                ],
                Kind::Unspecified,
            );
            cache.insert(key.clone(), (settings, elem));
        }

        let elem = &mut cache.get_mut(&key).unwrap().1;
        if elem.geometry(1.0.into()).to_logical(1) != geo.as_logical() {
            elem.resize(geo.as_logical(), None);
        }
        elem.clone()
    }
}

pub struct PostprocessShader(pub GlesTexProgram);

impl BlurShader {
    /// Get the blur shader program
    pub fn get<R: AsGlowRenderer>(renderer: &R) -> GlesTexProgram {
        Borrow::<GlesRenderer>::borrow(renderer.glow_renderer())
            .egl_context()
            .user_data()
            .get::<BlurShader>()
            .expect("Custom Shaders not initialized")
            .0
            .clone()
    }

    /// Create a horizontal blur pass element from a texture
    ///
    /// The blur shader requires two passes (horizontal + vertical) for proper gaussian blur.
    /// This creates the horizontal pass. The output should be rendered to an intermediate
    /// texture, then `element_vertical` should be called on that result.
    pub fn element_horizontal<R: AsGlowRenderer>(
        renderer: &R,
        texture_elem: TextureRenderElement<GlesTexture>,
        blur_radius: f32,
    ) -> TextureShaderElement {
        let shader = Self::get(renderer);
        let geo = texture_elem.geometry(1.0.into());

        TextureShaderElement::new(
            texture_elem,
            shader,
            vec![
                Uniform::new("tex_size", [geo.size.w as f32, geo.size.h as f32]),
                Uniform::new("blur_radius", blur_radius),
                Uniform::new("direction", 0.0), // 0.0 = horizontal
            ],
        )
    }

    /// Create a vertical blur pass element from a texture
    ///
    /// This should be called on the output of `element_horizontal` for proper
    /// two-pass gaussian blur.
    pub fn element_vertical<R: AsGlowRenderer>(
        renderer: &R,
        texture_elem: TextureRenderElement<GlesTexture>,
        blur_radius: f32,
    ) -> TextureShaderElement {
        let shader = Self::get(renderer);
        let geo = texture_elem.geometry(1.0.into());

        TextureShaderElement::new(
            texture_elem,
            shader,
            vec![
                Uniform::new("tex_size", [geo.size.w as f32, geo.size.h as f32]),
                Uniform::new("blur_radius", blur_radius),
                Uniform::new("direction", 1.0), // 1.0 = vertical
            ],
        )
    }

    /// Create a single-pass blur element (simplified blur for performance)
    ///
    /// For a proper gaussian blur, two passes (horizontal + vertical) are needed.
    /// This creates a single-pass blur which is faster but lower quality.
    /// Use `element_horizontal` followed by `element_vertical` for better quality.
    pub fn element_single_pass<R: AsGlowRenderer>(
        renderer: &R,
        texture_elem: TextureRenderElement<GlesTexture>,
        blur_radius: f32,
        horizontal: bool,
    ) -> TextureShaderElement {
        let shader = Self::get(renderer);
        let geo = texture_elem.geometry(1.0.into());

        TextureShaderElement::new(
            texture_elem,
            shader,
            vec![
                Uniform::new("tex_size", [geo.size.w as f32, geo.size.h as f32]),
                Uniform::new("blur_radius", blur_radius),
                Uniform::new("direction", if horizontal { 0.0 } else { 1.0 }),
            ],
        )
    }
}

impl BlurredBackdropShader {
    /// Get the blurred backdrop shader program
    pub fn get<R: AsGlowRenderer>(renderer: &R) -> GlesTexProgram {
        Borrow::<GlesRenderer>::borrow(renderer.glow_renderer())
            .egl_context()
            .user_data()
            .get::<BlurredBackdropShader>()
            .expect("BlurredBackdropShader not initialized")
            .0
            .clone()
    }

    /// Create a blurred backdrop element from a pre-blurred texture
    ///
    /// This crops the correct region from the blurred background texture,
    /// applies a tint overlay, and masks with rounded corners.
    ///
    /// Supports downsampled blur textures - coordinates are scaled from
    /// screen_size to texture_size automatically.
    ///
    /// # Arguments
    /// * `renderer` - The renderer to use
    /// * `blurred_texture` - The pre-blurred background texture (may be downsampled)
    /// * `element_geo` - The geometry of the element (where the backdrop appears)
    /// * `texture_size` - The actual size of the blur texture (may be smaller than screen)
    /// * `screen_size` - The full screen size for coordinate mapping
    /// * `scale` - Output scale factor for coordinate conversion
    /// * `transform` - Output transform for coordinate conversion
    /// * `corner_radius` - Corner radii [top-left, top-right, bottom-right, bottom-left]
    /// * `alpha` - Overall opacity
    /// * `tint_color` - Tint overlay color
    /// * `tint_strength` - How much tint to apply (0.0 = none, 1.0 = full)
    pub fn element<R: AsGlowRenderer>(
        renderer: &R,
        blurred_texture: &TextureRenderBuffer<GlesTexture>,
        element_geo: Rectangle<i32, Local>,
        texture_size: Size<i32, Physical>,
        screen_size: Size<i32, Physical>,
        scale: f64,
        transform: Transform,
        corner_radius: [f32; 4],
        alpha: f32,
        tint_color: [f32; 3],
        tint_strength: f32,
    ) -> TextureShaderElement {
        use crate::utils::geometry::RectLocalExt;

        let shader = Self::get(renderer);

        // Convert from Local to Logical, then to Physical coordinates
        // The blur texture is captured in physical pixels, so we need physical coordinates
        // for the src_rect to crop the correct region
        let phys_geo: Rectangle<i32, Physical> =
            element_geo.as_logical().to_physical_precise_round(scale);

        // Position the element at the physical location (scaled from logical)
        // This is critical for HiDPI: element_geo is in logical coords, but rendering
        // happens in physical coords
        let location: Point<f64, Physical> = (phys_geo.loc.x as f64, phys_geo.loc.y as f64).into();

        // If transform is not Normal, we need to transform the coordinates
        // Transform affects how coordinates map to the buffer
        let transformed_phys_geo = if transform != Transform::Normal {
            // Transform the location within the screen bounds
            let transformed_loc = transform.transform_point_in(phys_geo.loc, &screen_size);
            let transformed_size = transform.transform_size(phys_geo.size);
            Rectangle::new(transformed_loc, transformed_size)
        } else {
            phys_geo
        };

        // Calculate scale factor for downsampled texture
        // If texture is smaller than screen, we need to scale coordinates
        let scale_x = texture_size.w as f64 / screen_size.w as f64;
        let scale_y = texture_size.h as f64 / screen_size.h as f64;

        // Calculate src_rect in texture coordinates (scaled from screen coordinates)
        // The blur texture may be downsampled, so we scale the crop region
        let src_rect = Rectangle::<f64, Logical>::new(
            (
                transformed_phys_geo.loc.x as f64 * scale_x,
                transformed_phys_geo.loc.y as f64 * scale_y,
            )
                .into(),
            (
                transformed_phys_geo.size.w as f64 * scale_x,
                transformed_phys_geo.size.h as f64 * scale_y,
            )
                .into(),
        );

        // Output size should be in logical coordinates - this is the size the element
        // appears on screen. The src_rect crops from physical pixels, but output_size
        // specifies the logical output dimensions.
        let output_size = Size::<i32, Logical>::from((element_geo.size.w, element_geo.size.h));

        let source_elem = TextureRenderElement::from_texture_render_buffer(
            location,
            blurred_texture,
            Some(alpha),
            Some(src_rect),
            Some(output_size),
            Kind::Unspecified,
        );

        // Scale corner radii to physical pixels
        let scaled_corner_radius = [
            corner_radius[0] * scale as f32,
            corner_radius[1] * scale as f32,
            corner_radius[2] * scale as f32,
            corner_radius[3] * scale as f32,
        ];

        TextureShaderElement::new(
            source_elem,
            shader,
            vec![
                Uniform::new("alpha", alpha),
                // Use physical size for shader calculations (corner radius, etc.)
                // The shader operates in physical pixel space
                Uniform::new("size", [phys_geo.size.w as f32, phys_geo.size.h as f32]),
                Uniform::new("screen_size", [screen_size.w as f32, screen_size.h as f32]),
                // Use physical position for shader calculations
                Uniform::new(
                    "element_pos",
                    [phys_geo.loc.x as f32, phys_geo.loc.y as f32],
                ),
                // Order: [top-left, top-right, bottom-right, bottom-left]
                Uniform::new("corner_radius_tl", scaled_corner_radius[0]),
                Uniform::new("corner_radius_tr", scaled_corner_radius[1]),
                Uniform::new("corner_radius_br", scaled_corner_radius[2]),
                Uniform::new("corner_radius_bl", scaled_corner_radius[3]),
                Uniform::new("tint_color", tint_color),
                Uniform::new("tint_strength", tint_strength),
            ],
        )
    }
}

pub fn init_shaders(renderer: &mut GlesRenderer) -> Result<(), GlesError> {
    {
        let egl_context = renderer.egl_context();
        if egl_context.user_data().get::<IndicatorShader>().is_some()
            && egl_context.user_data().get::<BackdropShader>().is_some()
            && egl_context.user_data().get::<PostprocessShader>().is_some()
            && egl_context.user_data().get::<BlurShader>().is_some()
            && egl_context
                .user_data()
                .get::<BlurredBackdropShader>()
                .is_some()
        {
            return Ok(());
        }
    }

    let outline_shader = renderer.compile_custom_pixel_shader(
        OUTLINE_SHADER,
        &[
            UniformName::new("color", UniformType::_3f),
            UniformName::new("thickness", UniformType::_1f),
            UniformName::new("scale", UniformType::_1f),
            UniformName::new("radius", UniformType::_4f),
        ],
    )?;
    let rectangle_shader = renderer.compile_custom_pixel_shader(
        RECTANGLE_SHADER,
        &[
            UniformName::new("color", UniformType::_3f),
            UniformName::new("corner_radius_tl", UniformType::_1f),
            UniformName::new("corner_radius_tr", UniformType::_1f),
            UniformName::new("corner_radius_br", UniformType::_1f),
            UniformName::new("corner_radius_bl", UniformType::_1f),
        ],
    )?;
    let postprocess_shader = renderer.compile_custom_texture_shader(
        POSTPROCESS_SHADER,
        &[
            UniformName::new("invert", UniformType::_1f),
            UniformName::new("color_mode", UniformType::_1f),
        ],
    )?;
    let clipping_shader = renderer.compile_custom_texture_shader(
        CLIPPING_SHADER,
        &[
            UniformName::new("geo_size", UniformType::_2f),
            UniformName::new("corner_radius", UniformType::_4f),
            UniformName::new("input_to_geo", UniformType::Matrix3x3),
        ],
    )?;
    let shadow_shader = renderer.compile_custom_pixel_shader(
        SHADOW_SHADER,
        &[
            // Primary shadow uniforms
            UniformName::new("shadow_color", UniformType::_4f),
            UniformName::new("sigma", UniformType::_1f),
            UniformName::new("input_to_geo", UniformType::Matrix3x3),
            UniformName::new("geo_size", UniformType::_2f),
            UniformName::new("corner_radius", UniformType::_4f),
            // Secondary shadow uniforms
            UniformName::new("shadow_color_2", UniformType::_4f),
            UniformName::new("sigma_2", UniformType::_1f),
            UniformName::new("input_to_geo_2", UniformType::Matrix3x3),
            UniformName::new("geo_size_2", UniformType::_2f),
            UniformName::new("corner_radius_2", UniformType::_4f),
            // Window cutout uniforms
            UniformName::new("window_input_to_geo", UniformType::Matrix3x3),
            UniformName::new("window_geo_size", UniformType::_2f),
            UniformName::new("window_corner_radius", UniformType::_4f),
        ],
    )?;
    let blur_shader = renderer.compile_custom_texture_shader(
        BLUR_SHADER,
        &[
            UniformName::new("tex_size", UniformType::_2f),
            UniformName::new("blur_radius", UniformType::_1f),
            UniformName::new("direction", UniformType::_1f),
        ],
    )?;
    let blurred_backdrop_shader = renderer.compile_custom_texture_shader(
        BLURRED_BACKDROP_SHADER,
        &[
            UniformName::new("alpha", UniformType::_1f),
            UniformName::new("size", UniformType::_2f),
            UniformName::new("screen_size", UniformType::_2f),
            UniformName::new("element_pos", UniformType::_2f),
            UniformName::new("corner_radius_tl", UniformType::_1f),
            UniformName::new("corner_radius_tr", UniformType::_1f),
            UniformName::new("corner_radius_br", UniformType::_1f),
            UniformName::new("corner_radius_bl", UniformType::_1f),
            UniformName::new("tint_color", UniformType::_3f),
            UniformName::new("tint_strength", UniformType::_1f),
        ],
    )?;

    let egl_context = renderer.egl_context();
    egl_context
        .user_data()
        .insert_if_missing(|| IndicatorShader(outline_shader));
    egl_context
        .user_data()
        .insert_if_missing(|| BackdropShader(rectangle_shader));
    egl_context
        .user_data()
        .insert_if_missing(|| PostprocessShader(postprocess_shader));
    egl_context
        .user_data()
        .insert_if_missing(|| ClippingShader(clipping_shader));
    egl_context
        .user_data()
        .insert_if_missing(|| ShadowShader(shadow_shader));
    egl_context
        .user_data()
        .insert_if_missing(|| BlurShader(blur_shader));
    egl_context
        .user_data()
        .insert_if_missing(|| BlurredBackdropShader(blurred_backdrop_shader));

    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CursorMode {
    None,
    NotDefault,
    All,
}

#[profiling::function]
pub fn cursor_elements<'a, 'frame, R>(
    renderer: &mut R,
    seats: impl Iterator<Item = &'a Seat<State>>,
    zoom_state: Option<&ZoomState>,
    theme: &Theme,
    now: Time<Monotonic>,
    output: &Output,
    mode: CursorMode,
    exclude_dnd_icon: bool,
    skip_move_grab: bool,
    embedded_children_for_grabbed: &[(
        CosmicMapped,
        crate::wayland::handlers::surface_embed::EmbedRenderInfo,
    )],
) -> Vec<CosmicElement<R>>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    let scale = output.current_scale().fractional_scale();
    let (focal_point, zoom_scale) = zoom_state
        .map(|state| {
            (
                state.animating_focal_point(Some(output)).to_local(output),
                state.animating_level(output),
            )
        })
        .unwrap_or_else(|| ((0., 0.).into(), 1.));
    let mut elements = Vec::new();

    for seat in seats {
        let pointer = match seat.get_pointer() {
            Some(ptr) => ptr,
            None => continue,
        };
        let location = pointer.current_location() - output.current_location().to_f64();

        if mode != CursorMode::None {
            elements.extend(
                cursor::draw_cursor(
                    renderer,
                    seat,
                    location,
                    scale.into(),
                    zoom_scale,
                    now,
                    mode != CursorMode::NotDefault,
                )
                .into_iter()
                .map(|(elem, hotspot)| {
                    CosmicElement::Cursor(RescaleRenderElement::from_element(
                        RelocateRenderElement::from_element(
                            elem,
                            Point::from((-hotspot.x, -hotspot.y)),
                            Relocate::Relative,
                        ),
                        focal_point
                            .as_logical()
                            .to_physical(output.current_scale().fractional_scale())
                            .to_i32_round(),
                        zoom_scale,
                    ))
                }),
            );
        }

        if !exclude_dnd_icon {
            if let Some(dnd_icon) = get_dnd_icon(seat) {
                elements.extend(
                    cursor::draw_dnd_icon(
                        renderer,
                        &dnd_icon.surface,
                        (location + dnd_icon.offset.to_f64()).to_i32_round(),
                        scale,
                    )
                    .into_iter()
                    .map(CosmicElement::Dnd),
                );
            }
        }

        let theme = theme.cosmic();
        // Skip move grab render when capturing for blur - the grabbed window
        // is always on top and shouldn't be included in the blur source
        if !skip_move_grab {
            if let Some(grab_elements) = seat
                .user_data()
                .get::<SeatMoveGrabState>()
                .unwrap()
                .lock()
                .unwrap()
                .as_ref()
                .map(|state| {
                    state.render::<CosmicMappedRenderElement<R>, R>(
                        renderer,
                        output,
                        theme,
                        embedded_children_for_grabbed,
                    )
                })
            {
                elements.extend(grab_elements.into_iter().map(|elem| {
                    CosmicElement::MoveGrab(RescaleRenderElement::from_element(
                        elem,
                        focal_point
                            .as_logical()
                            .to_physical(output.current_scale().fractional_scale())
                            .to_i32_round(),
                        zoom_scale,
                    ))
                }));
            }
        }

        if let Some((grab_elements, should_scale)) = seat
            .user_data()
            .get::<SeatMenuGrabState>()
            .unwrap()
            .lock()
            .unwrap()
            .as_ref()
            .map(|state| {
                (
                    state.render::<CosmicMappedRenderElement<R>, R>(renderer, output),
                    !state.is_in_screen_space(),
                )
            })
        {
            elements.extend(grab_elements.into_iter().map(|elem| {
                CosmicElement::MoveGrab(RescaleRenderElement::from_element(
                    elem,
                    if should_scale {
                        focal_point
                            .as_logical()
                            .to_physical(output.current_scale().fractional_scale())
                            .to_i32_round()
                    } else {
                        Point::from((0, 0))
                    },
                    if should_scale { zoom_scale } else { 1.0 },
                ))
            }));
        }
    }

    elements
}

#[cfg(not(feature = "debug"))]
pub type EguiState = ();

#[derive(Clone, PartialEq)]
pub enum ElementFilter {
    All,
    ExcludeWorkspaceOverview,
    LayerShellOnly,
    /// Exclude windows with blur effect (for capturing background to blur)
    ExcludeBlurWindows,
    /// Blur capture mode with context (replaces thread-local globals)
    BlurCapture(BlurCaptureContext),
}

pub fn output_elements<R>(
    _gpu: Option<&DrmNode>,
    renderer: &mut R,
    shell: &Arc<parking_lot::RwLock<Shell>>,
    now: Time<Monotonic>,
    output: &Output,
    cursor_mode: CursorMode,
    _fps: Option<(&EguiState, &Timings)>,
) -> Result<Vec<CosmicElement<R>>, RenderError<R::Error>>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
    R::Error: FromGlesError,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    WorkspaceRenderElement<R>: RenderElement<R>,
{
    #[cfg(feature = "debug")]
    let mut debug_elements = {
        let output_geo = output.geometry();
        let shell_guard = shell.read();
        let seats = shell_guard.seats.iter().cloned().collect::<Vec<_>>();
        let debug_active = shell_guard.debug_active;
        std::mem::drop(shell_guard);
        let scale = output.current_scale().fractional_scale();

        if let Some((state, timings)) = _fps {
            vec![
                fps_ui(
                    _gpu,
                    debug_active,
                    &seats,
                    renderer.glow_renderer_mut(),
                    state,
                    timings,
                    Rectangle::from_size(
                        (output_geo.size.w.min(400), output_geo.size.h.min(800)).into(),
                    ),
                    scale,
                )
                .map_err(FromGlesError::from_gles_error)
                .map_err(RenderError::Rendering)?
                .into(),
            ]
        } else {
            Vec::new()
        }
    };

    let shell_guard = shell.read();
    let Some((previous_workspace, workspace)) = shell_guard.workspaces.active(output) else {
        #[cfg(not(feature = "debug"))]
        return Ok(Vec::new());
        #[cfg(feature = "debug")]
        return Ok(debug_elements);
    };

    let (previous_idx, idx) = shell_guard.workspaces.active_num(output);
    let previous_workspace = previous_workspace
        .zip(previous_idx)
        .map(|((w, start), idx)| (w.handle, idx, start));
    let workspace = (workspace.handle, idx);

    std::mem::drop(shell_guard);

    let element_filter = if workspace_overview_is_open(output) {
        ElementFilter::LayerShellOnly
    } else {
        ElementFilter::All
    };
    let zoom_state = shell.read().zoom_state().cloned();

    #[allow(unused_mut)]
    let workspace_elements = workspace_elements(
        _gpu,
        renderer,
        shell,
        zoom_state.as_ref(),
        now,
        output,
        previous_workspace,
        workspace,
        cursor_mode,
        &element_filter,
    )?;

    #[cfg(feature = "debug")]
    {
        debug_elements.extend(workspace_elements);
        Ok(debug_elements)
    }
    #[cfg(not(feature = "debug"))]
    Ok(workspace_elements)
}

#[profiling::function]
pub fn workspace_elements<R>(
    _gpu: Option<&DrmNode>,
    renderer: &mut R,
    shell: &Arc<parking_lot::RwLock<Shell>>,
    zoom_level: Option<&ZoomState>,
    now: Time<Monotonic>,
    output: &Output,
    previous: Option<(WorkspaceHandle, usize, WorkspaceDelta)>,
    current: (WorkspaceHandle, usize),
    cursor_mode: CursorMode,
    element_filter: &ElementFilter,
) -> Result<Vec<CosmicElement<R>>, RenderError<R::Error>>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
    R::Error: FromGlesError,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    WorkspaceRenderElement<R>: RenderElement<R>,
{
    let mut elements = Vec::new();

    let shell_ref = shell.read();
    let seats = shell_ref.seats.iter().cloned().collect::<Vec<_>>();
    if seats.is_empty() {
        return Ok(Vec::new());
    }
    let theme = shell_ref.theme().clone();
    let scale = output.current_scale().fractional_scale();

    // Gather embedded children for any move-grabbed window before dropping shell lock
    // This allows us to render embedded windows following the dragged parent
    let embedded_children_for_grabbed: Vec<(
        CosmicMapped,
        crate::wayland::handlers::surface_embed::EmbedRenderInfo,
    )> = {
        // Check if any seat has an active move grab and get the parent's surface_id
        seats
            .iter()
            .find_map(|seat| {
                seat.user_data()
                    .get::<SeatMoveGrabState>()
                    .and_then(|state| state.lock().ok())
                    .and_then(|state| {
                        state.as_ref().and_then(|s| {
                            s.element()
                                .active_window()
                                .wl_surface()
                                .map(|surf| surf.id().to_string())
                        })
                    })
            })
            .map(|parent_surface_id| {
                // Get all embedded children for this parent (by surface_id)
                let children =
                    crate::wayland::handlers::surface_embed::get_children_for_parent_by_surface_id(
                        &parent_surface_id,
                    );
                children
                    .into_iter()
                    .filter_map(|(child_surface_id, embed_info)| {
                        // Find the CosmicMapped for this embedded child (by surface_id)
                        shell_ref
                            .workspaces
                            .spaces()
                            .flat_map(|s| s.mapped())
                            .find(|mapped| {
                                mapped
                                    .active_window()
                                    .wl_surface()
                                    .map(|s| s.id().to_string() == child_surface_id)
                                    .unwrap_or(false)
                            })
                            .map(|mapped| (mapped.clone(), embed_info))
                    })
                    .collect()
            })
            .unwrap_or_default()
    };

    // we don't want to hold a shell lock across `cursor_elements`,
    // that is prone to deadlock with the main-thread on some grabs.
    std::mem::drop(shell_ref);

    elements.extend(cursor_elements(
        renderer,
        seats.iter(),
        zoom_level,
        &theme,
        now,
        output,
        cursor_mode,
        *element_filter == ElementFilter::ExcludeWorkspaceOverview,
        matches!(element_filter, ElementFilter::BlurCapture(_)),
        &embedded_children_for_grabbed,
    ));

    let shell = shell.read();
    let overview = shell.overview_mode();
    let (resize_mode, resize_indicator) = shell.resize_mode();
    let resize_indicator = resize_indicator.map(|indicator| (resize_mode, indicator));
    let swap_tree = if let Some(Trigger::KeyboardSwap(_, desc)) = overview.0.active_trigger() {
        if current.0 != desc.handle {
            shell
                .workspaces
                .space_for_handle(&desc.handle)
                .map(|w| w.tiling_layer.tree())
        } else {
            None
        }
    } else {
        None
    };
    let overview = (
        overview.0,
        overview.1.map(|indicator| (indicator, swap_tree)),
    );
    let last_active_seat = shell.seats.last_active();
    let move_active = last_active_seat
        .user_data()
        .get::<SeatMoveGrabState>()
        .unwrap()
        .lock()
        .unwrap()
        .is_some();
    let focused_output = last_active_seat.focused_or_active_output();
    let set = shell.workspaces.sets.get(output).ok_or(OutputNoMode)?;
    let workspace = set
        .workspaces
        .iter()
        .find(|w| w.handle == current.0)
        .ok_or(OutputNoMode)?;
    let is_active_space = workspace.output == focused_output;
    let active_hint = if shell.active_hint {
        theme.cosmic().active_hint as u8
    } else {
        0
    };

    let output_size = output
        .geometry()
        .size
        .as_logical()
        .to_physical_precise_round(scale);
    let (focal_point, zoom_scale) = zoom_level
        .map(|state| {
            (
                state.animating_focal_point(Some(output)).to_local(output),
                state.animating_level(output),
            )
        })
        .unwrap_or_else(|| ((0., 0.).into(), 1.));

    let crop_to_output = |element: WorkspaceRenderElement<R>| {
        CropRenderElement::from_element(
            RescaleRenderElement::from_element(
                element,
                focal_point
                    .as_logical()
                    .to_physical(output.current_scale().fractional_scale())
                    .to_i32_round(),
                zoom_scale,
            ),
            scale,
            Rectangle::from_size(output_size),
        )
    };

    render_input_order::<()>(
        &shell,
        output,
        previous,
        current,
        &element_filter,
        |stage| {
            match stage {
                Stage::ZoomUI => {
                    elements.extend(ZoomState::render(renderer, output));
                }
                Stage::SessionLock(lock_surface) => {
                    elements.extend(
                        session_lock_elements(renderer, output, lock_surface)
                            .into_iter()
                            .map(Into::into)
                            .flat_map(crop_to_output)
                            .map(Into::into),
                    );
                }
                Stage::LayerPopup {
                    popup, location, ..
                } => {
                    elements.extend(
                        render_elements_from_surface_tree::<_, WorkspaceRenderElement<_>>(
                            renderer,
                            popup.wl_surface(),
                            location
                                .to_local(output)
                                .as_logical()
                                .to_physical_precise_round(scale),
                            Scale::from(scale),
                            1.0,
                            FRAME_TIME_FILTER,
                        )
                        .into_iter()
                        .flat_map(crop_to_output)
                        .map(Into::into),
                    );
                }
                Stage::LayerSurface { layer, location } => {
                    elements.extend(
                        render_elements_from_surface_tree::<_, WorkspaceRenderElement<_>>(
                            renderer,
                            layer.wl_surface(),
                            location
                                .to_local(output)
                                .as_logical()
                                .to_physical_precise_round(scale),
                            Scale::from(scale),
                            1.0,
                            FRAME_TIME_FILTER,
                        )
                        .into_iter()
                        .flat_map(crop_to_output)
                        .map(Into::into),
                    );
                }
                Stage::OverrideRedirect { surface, location } => {
                    elements.extend(surface.wl_surface().into_iter().flat_map(|surface| {
                        render_elements_from_surface_tree::<_, WorkspaceRenderElement<_>>(
                            renderer,
                            &surface,
                            location
                                .to_local(output)
                                .as_logical()
                                .to_physical_precise_round(scale),
                            Scale::from(scale),
                            1.0,
                            FRAME_TIME_FILTER,
                        )
                        .into_iter()
                        .flat_map(crop_to_output)
                        .map(Into::into)
                    }));
                }
                Stage::StickyPopups(layout) => {
                    let alpha = match &overview.0 {
                        OverviewMode::Started(_, started) => {
                            (1.0 - (Instant::now().duration_since(*started).as_millis()
                                / ANIMATION_DURATION.as_millis())
                                as f32)
                                .max(0.0)
                                * 0.4
                                + 0.6
                        }
                        OverviewMode::Ended(_, ended) => {
                            ((Instant::now().duration_since(*ended).as_millis()
                                / ANIMATION_DURATION.as_millis())
                                as f32)
                                * 0.4
                                + 0.6
                        }
                        OverviewMode::Active(_) => 0.6,
                        OverviewMode::None => 1.0,
                    };

                    elements.extend(
                        layout
                            .render_popups(renderer, alpha)
                            .into_iter()
                            .map(Into::into)
                            .flat_map(crop_to_output)
                            .map(Into::into),
                    );
                }
                Stage::Sticky(layout) => {
                    let alpha = match &overview.0 {
                        OverviewMode::Started(_, started) => {
                            (1.0 - (Instant::now().duration_since(*started).as_millis()
                                / ANIMATION_DURATION.as_millis())
                                as f32)
                                .max(0.0)
                                * 0.4
                                + 0.6
                        }
                        OverviewMode::Ended(_, ended) => {
                            ((Instant::now().duration_since(*ended).as_millis()
                                / ANIMATION_DURATION.as_millis())
                                as f32)
                                * 0.4
                                + 0.6
                        }
                        OverviewMode::Active(_) => 0.6,
                        OverviewMode::None => 1.0,
                    };

                    let current_focus = (!move_active && is_active_space)
                        .then_some(last_active_seat)
                        .map(|seat| workspace.focus_stack.get(seat));

                    elements.extend(
                        layout
                            .render(
                                renderer,
                                current_focus.as_ref().and_then(|stack| {
                                    stack.last().and_then(|t| match t {
                                        FocusTarget::Window(w) => Some(w),
                                        _ => None,
                                    })
                                }),
                                resize_indicator.clone(),
                                active_hint,
                                alpha,
                                theme.cosmic(),
                                element_filter.clone(),
                            )
                            .into_iter()
                            .map(Into::into)
                            .flat_map(crop_to_output)
                            .map(Into::into),
                    )
                }
                Stage::WorkspacePopups { workspace, offset } => {
                    elements.extend(
                        match workspace.render_popups(
                            renderer,
                            last_active_seat,
                            !move_active && is_active_space,
                            overview.clone(),
                            theme.cosmic(),
                        ) {
                            Ok(elements) => {
                                elements
                                    .into_iter()
                                    .flat_map(crop_to_output)
                                    .map(|element| {
                                        CosmicElement::Workspace(
                                            RelocateRenderElement::from_element(
                                                element,
                                                offset.to_physical_precise_round(scale),
                                                Relocate::Relative,
                                            ),
                                        )
                                    })
                            }
                            Err(_) => {
                                return ControlFlow::Break(Err(OutputNoMode));
                            }
                        },
                    );
                }
                Stage::Workspace { workspace, offset } => {
                    elements.extend(
                        match workspace.render(
                            renderer,
                            last_active_seat,
                            !move_active && is_active_space,
                            overview.clone(),
                            resize_indicator.clone(),
                            active_hint,
                            theme.cosmic(),
                            element_filter.clone(),
                        ) {
                            Ok(elements) => {
                                elements
                                    .into_iter()
                                    .flat_map(crop_to_output)
                                    .map(|element| {
                                        CosmicElement::Workspace(
                                            RelocateRenderElement::from_element(
                                                element,
                                                offset.to_physical_precise_round(scale),
                                                Relocate::Relative,
                                            ),
                                        )
                                    })
                            }
                            Err(_) => {
                                return ControlFlow::Break(Err(OutputNoMode));
                            }
                        },
                    );
                }
            };

            ControlFlow::Continue(())
        },
    )?;

    Ok(elements)
}

fn session_lock_elements<R>(
    renderer: &mut R,
    output: &Output,
    lock_surface: Option<&LockSurface>,
) -> Vec<WaylandSurfaceRenderElement<R>>
where
    R: Renderer + ImportAll,
    R::TextureId: Clone + 'static,
{
    if let Some(surface) = lock_surface {
        let scale = Scale::from(output.current_scale().fractional_scale());
        render_elements_from_surface_tree(
            renderer,
            surface.wl_surface(),
            (0, 0),
            scale,
            1.0,
            FRAME_TIME_FILTER,
        )
    } else {
        Vec::new()
    }
}

// Used for mirroring and postprocessing
#[derive(Debug)]
pub struct PostprocessState {
    pub texture: TextureRenderBuffer<GlesTexture>,
    pub damage_tracker: OutputDamageTracker,
    pub cursor_texture: Option<TextureRenderBuffer<GlesTexture>>,
    pub cursor_damage_tracker: Option<OutputDamageTracker>,
    pub output_config: PostprocessOutputConfig,
}

impl PostprocessState {
    pub fn new_with_renderer<R: AsGlowRenderer + Offscreen<GlesTexture>>(
        renderer: &mut R,
        format: Fourcc,
        output_config: PostprocessOutputConfig,
    ) -> Result<Self, R::Error> {
        let size = output_config.size;
        let buffer_size = size.to_logical(1).to_buffer(1, Transform::Normal);
        let opaque_regions = vec![Rectangle::from_size(buffer_size)];

        let texture = Offscreen::<GlesTexture>::create_buffer(renderer, format, buffer_size)?;
        let texture_buffer = TextureRenderBuffer::from_texture(
            renderer.glow_renderer(),
            texture,
            1,
            Transform::Normal,
            Some(opaque_regions),
        );

        // Don't use `from_output` to avoid applying output transform
        let damage_tracker =
            OutputDamageTracker::new(size, output_config.fractional_scale, Transform::Normal);

        Ok(PostprocessState {
            texture: texture_buffer,
            damage_tracker,
            cursor_texture: None,
            cursor_damage_tracker: None,
            output_config,
        })
    }

    pub fn track_cursor<R: AsGlowRenderer + Offscreen<GlesTexture>>(
        &mut self,
        renderer: &mut R,
        format: Fourcc,
        size: Size<i32, Physical>,
        scale: Scale<f64>,
    ) -> Result<(), R::Error> {
        let buffer_size = size.to_logical(1).to_buffer(1, Transform::Normal);

        if let (Some(tex), Some(tracker)) = (
            self.cursor_texture.as_ref(),
            self.cursor_damage_tracker.as_ref(),
        ) {
            if tex.format().is_some_and(|f| f == format)
                && tracker.mode()
                    == &(OutputModeSource::Static {
                        size,
                        scale,
                        transform: Transform::Normal,
                    })
            {
                return Ok(());
            }
        }

        let texture = Offscreen::<GlesTexture>::create_buffer(renderer, format, buffer_size)?;

        let texture_buffer = TextureRenderBuffer::from_texture(
            renderer.glow_renderer(),
            texture,
            1,
            Transform::Normal,
            None,
        );

        let damage_tracker = OutputDamageTracker::new(size, scale, Transform::Normal);

        self.cursor_texture = Some(texture_buffer);
        self.cursor_damage_tracker = Some(damage_tracker);

        Ok(())
    }

    pub fn remove_cursor(&mut self) {
        self.cursor_texture.take();
        self.cursor_damage_tracker.take();
    }
}

#[derive(Debug, PartialEq)]
pub struct PostprocessOutputConfig {
    pub size: Size<i32, Physical>,
    pub fractional_scale: f64,
}

impl PostprocessOutputConfig {
    pub fn for_output_untransformed(output: &Output) -> Self {
        Self {
            // Apply inverse of output transform to mode size to get correct size
            // for an untransformed render.
            size: output.current_transform().invert().transform_size(
                output
                    .current_mode()
                    .map(|mode| mode.size)
                    .unwrap_or_default(),
            ),
            fractional_scale: output.current_scale().fractional_scale(),
        }
    }

    pub fn for_output(output: &Output) -> Self {
        Self {
            size: output
                .current_mode()
                .map(|mode| mode.size)
                .unwrap_or_default(),
            fractional_scale: output.current_scale().fractional_scale(),
        }
    }
}

#[derive(Debug, Default)]
pub struct ScreenFilterStorage {
    pub filter: ScreenFilter,
    pub state: Option<PostprocessState>,
}

/// Render output with blur support
#[profiling::function]
pub fn render_output<'d, R>(
    gpu: Option<&DrmNode>,
    renderer: &mut R,
    target: &mut R::Framebuffer<'_>,
    damage_tracker: &'d mut OutputDamageTracker,
    age: usize,
    shell: &Arc<parking_lot::RwLock<Shell>>,
    now: Time<Monotonic>,
    output: &Output,
    cursor_mode: CursorMode,
    screen_filter: &'d mut ScreenFilterStorage,
    loop_handle: &calloop::LoopHandle<'static, State>,
    blur_state: &mut BlurRenderState,
) -> Result<RenderOutputResult<'d>, RenderError<R::Error>>
where
    R: Renderer
        + ImportAll
        + ImportMem
        + ExportMem
        + Bind<Dmabuf>
        + Offscreen<GlesTexture>
        + Blit
        + AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
    R::Error: FromGlesError,
    CosmicElement<R>: RenderElement<R>,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    WorkspaceRenderElement<R>: RenderElement<R>,
{
    let shell_ref = shell.read();
    let (previous_workspace, workspace_ref) = shell_ref
        .workspaces
        .active(output)
        .ok_or(RenderError::OutputNoMode(OutputNoMode))?;
    let (previous_idx, idx) = shell_ref.workspaces.active_num(output);
    let previous_workspace = previous_workspace
        .zip(previous_idx)
        .map(|((w, start), idx)| (w.handle, idx, start));
    let workspace = (workspace_ref.handle, idx);
    let zoom_state = shell_ref.zoom_state().cloned();

    // Check if there are blur windows and get their geometries
    let has_blur = workspace_ref.has_blur_windows();
    let blur_geometries = if has_blur {
        workspace_ref.blur_window_geometries(1.0)
    } else {
        Vec::new()
    };

    // Debug logging for blur detection
    tracing::debug!(
        has_blur = has_blur,
        blur_geometry_count = blur_geometries.len(),
        screen_filter_noop = screen_filter.filter.is_noop(),
        "Blur detection in render_output_with_blur"
    );

    std::mem::drop(shell_ref);

    let element_filter = if workspace_overview_is_open(output) {
        ElementFilter::LayerShellOnly
    } else {
        ElementFilter::All
    };

    let mut postprocess_texture = None;
    let result = if !screen_filter.filter.is_noop() {
        if screen_filter.state.as_ref().is_none_or(|state| {
            state.output_config != PostprocessOutputConfig::for_output_untransformed(output)
        }) {
            screen_filter.state = Some(
                PostprocessState::new_with_renderer(
                    renderer,
                    target.format().unwrap_or(Fourcc::Abgr8888),
                    PostprocessOutputConfig::for_output_untransformed(output),
                )
                .map_err(RenderError::Rendering)?,
            );
        }

        let state = screen_filter.state.as_mut().unwrap();
        let mut result = Err(RenderError::OutputNoMode(OutputNoMode));
        state
            .texture
            .render()
            .draw::<_, RenderError<R::Error>>(|tex| {
                let mut target = renderer.bind(tex).map_err(RenderError::Rendering)?;
                result = render_workspace(
                    gpu,
                    renderer,
                    &mut target,
                    &mut state.damage_tracker,
                    1,
                    None,
                    shell,
                    zoom_state.as_ref(),
                    now,
                    output,
                    previous_workspace,
                    workspace,
                    cursor_mode,
                    element_filter,
                );
                std::mem::drop(target);
                postprocess_texture = Some(tex.clone());

                Ok(if let Ok((res, _)) = result.as_ref() {
                    renderer.wait(&res.sync).map_err(RenderError::Rendering)?;
                    let transform = output.current_transform();
                    let area = tex.size().to_logical(1, transform);

                    res.damage
                        .cloned()
                        .map(|v| {
                            v.into_iter()
                                .map(|r| r.to_logical(1).to_buffer(1, transform, &area))
                                .collect::<Vec<_>>()
                        })
                        .unwrap_or_default()
                } else {
                    Vec::new()
                })
            })?;

        if result.is_ok() {
            let texture_elem = TextureRenderElement::from_texture_render_buffer(
                (0., 0.),
                &state.texture,
                Some(1.0),
                None,
                None,
                Kind::Unspecified,
            );

            let postprocess_texture_shader = renderer
                .glow_renderer_mut()
                .egl_context()
                .user_data()
                .get::<PostprocessShader>()
                .expect("OffscreenShader should be available through `init_shaders`");
            let texture_geometry =
                texture_elem.geometry(output.current_scale().fractional_scale().into());
            let elements = {
                let texture_elem = TextureShaderElement::new(
                    texture_elem,
                    postprocess_texture_shader.0.clone(),
                    vec![
                        Uniform::new(
                            "invert",
                            if screen_filter.filter.inverted {
                                1.
                            } else {
                                0.
                            },
                        ),
                        Uniform::new(
                            "color_mode",
                            screen_filter
                                .filter
                                .color_filter
                                .map(|val| val as u8 as f32)
                                .unwrap_or(0.),
                        ),
                    ],
                );
                constrain_render_elements(
                    std::iter::once(texture_elem),
                    (0, 0),
                    Rectangle::from_size(
                        output
                            .geometry()
                            .size
                            .as_logical()
                            .to_f64()
                            .to_physical(output.current_scale().fractional_scale())
                            .to_i32_round(),
                    ),
                    texture_geometry,
                    ConstrainScaleBehavior::Fit,
                    ConstrainAlign::CENTER,
                    1.0,
                )
                .map(CosmicElement::Postprocess)
                .collect::<Vec<_>>()
            };

            damage_tracker.render_output(renderer, target, age, &elements, CLEAR_COLOR)?;
        }

        result
    } else if has_blur && !blur_geometries.is_empty() {
        // Iterative multi-pass blur (macOS-style):
        // For each blur window (bottom to top in Z-order):
        //   1. Capture scene up to (but excluding) that window
        //   2. Apply blur and cache per-window texture
        // Then render final scene where each blur window uses its cached texture

        let output_name = output.name();
        tracing::debug!(
            blur_geometry_count = blur_geometries.len(),
            output = %output_name,
            "Entering iterative multi-pass blur path"
        );

        let output_size = output
            .current_mode()
            .ok_or(RenderError::OutputNoMode(OutputNoMode))?
            .size
            .to_logical(1)
            .to_physical_precise_round(output.current_scale().fractional_scale());
        let scale = Scale::from(output.current_scale().fractional_scale());
        let format = target.format().unwrap_or(Fourcc::Abgr8888);

        // Ensure blur textures are allocated
        blur_state
            .ensure_textures(renderer, format, output_size, scale)
            .map_err(RenderError::Rendering)?;

        // Get blur windows GROUPED by shared capture requirements
        // Consecutive blur windows (no non-blur windows between them) share a single capture
        let blur_groups = {
            let shell_ref = shell.read();
            let (_, workspace_ref) = shell_ref
                .workspaces
                .active(output)
                .ok_or(RenderError::OutputNoMode(OutputNoMode))?;
            workspace_ref.blur_windows_grouped(1.0)
        };

        let total_windows: usize = blur_groups.iter().map(|g| g.windows.len()).sum();
        tracing::debug!(
            blur_group_count = blur_groups.len(),
            total_blur_windows = total_windows,
            "Processing blur windows in {} groups (optimized from {} captures to {})",
            blur_groups.len(),
            total_windows,
            blur_groups.len()
        );

        // Get the grabbed window key (if any) to exclude from blur capture
        let grabbed_window_key = {
            let shell_ref = shell.read();
            let last_active_seat = shell_ref.seats.last_active();
            let key = last_active_seat
                .user_data()
                .get::<SeatMoveGrabState>()
                .unwrap()
                .lock()
                .unwrap()
                .as_ref()
                .map(|s| s.element().key());
            if key.is_some() {
                tracing::debug!("Found grabbed window to exclude from blur");
            }
            key
        };

        // Optimized iterative blur: one capture per GROUP, then blur for each window in group
        // OPTIMIZATION: Skip re-blurring if background hasn't changed (no damage + same geometry)
        let mut any_blur_applied = false;
        for group in &blur_groups {
            // Create blur capture context for this group
            let blur_ctx =
                BlurCaptureContext::new(group.capture_z_threshold, grabbed_window_key.clone());
            let capture_filter = ElementFilter::BlurCapture(blur_ctx);

            // Capture scene elements ONCE for the entire group
            let capture_elements: Vec<CosmicElement<R>> = workspace_elements(
                gpu,
                renderer,
                shell,
                zoom_state.as_ref(),
                now,
                output,
                previous_workspace,
                workspace,
                cursor_mode,
                &capture_filter,
            )?;

            // Compute content hash for cache invalidation
            // This includes element IDs (which change on content updates) and geometry
            let content_hash =
                compute_element_content_hash(group.capture_z_threshold, &capture_elements, scale);

            // Check if content has changed since last blur
            let stored_hash = get_blur_group_content_hash(&output_name, group.capture_z_threshold);
            let content_changed = stored_hash.is_none() || stored_hash != Some(content_hash);

            // Also verify all windows have cached textures
            let all_cached = group.windows.iter().all(|(window_key, _, _, _)| {
                get_cached_blur_texture_for_window(&output_name, window_key).is_some()
            });

            let can_reuse_cache = !content_changed && all_cached;

            if can_reuse_cache {
                tracing::debug!(
                    capture_z_threshold = group.capture_z_threshold,
                    windows_in_group = group.windows.len(),
                    "Skipping blur for group - cache valid (content unchanged)"
                );
                any_blur_applied = true; // Cached textures are still valid
                continue;
            }

            tracing::debug!(
                capture_z_threshold = group.capture_z_threshold,
                windows_in_group = group.windows.len(),
                content_changed = content_changed,
                all_cached = all_cached,
                "Re-blurring group"
            );

            // Store the new content hash after we commit to re-blurring
            store_blur_group_content_hash(&output_name, group.capture_z_threshold, content_hash);

            // Render captured elements to background texture (once per group)
            let bg_render_ok = if let Some(bg_texture) = blur_state.background_texture.as_mut() {
                let mut blur_dt = OutputDamageTracker::new(output_size, scale, Transform::Normal);

                let render_result = (|| {
                    let mut gles_frame = bg_texture.render();

                    let render_res = gles_frame.draw::<_, RenderError<R::Error>>(|tex| {
                        let bound = renderer.bind(tex).map_err(RenderError::Rendering)?;
                        let mut bound_target = bound;
                        let res = blur_dt.render_output(
                            renderer,
                            &mut bound_target,
                            0, // Full redraw
                            &capture_elements,
                            CLEAR_COLOR,
                        );
                        match res {
                            Ok(_) => Ok(Vec::new()),
                            Err(e) => Err(e.into()),
                        }
                    });

                    render_res.map_err(|_| ())
                })();

                render_result.is_ok()
            } else {
                false
            };

            // Downsample background to smaller texture for blur passes (if enabled)
            let downsample_enabled = blur_downsample_enabled();
            let downsample_ok = if downsample_enabled && bg_render_ok {
                if let (Some(bg), Some(ds)) = (
                    blur_state.background_texture.as_ref().cloned(),
                    blur_state.downsampled_texture.as_mut(),
                ) {
                    let blur_size = blur_state.texture_size;
                    downsample_texture(renderer, &bg, ds, output_size, blur_size).is_ok()
                } else {
                    false
                }
            } else {
                !downsample_enabled && bg_render_ok // Skip downsample step when disabled
            };

            // Apply blur passes (on downsampled or full-size textures)
            // Since they share the same background, they share the same blurred result
            if downsample_ok && blur_state.is_ready() {
                let blur_size = blur_state.texture_size;
                // Source texture: downsampled if enabled, background if disabled
                let blur_source = if downsample_enabled {
                    blur_state.downsampled_texture.as_ref().cloned()
                } else {
                    blur_state.background_texture.as_ref().cloned()
                };

                if let (Some(src), Some(ping), Some(pong)) = (
                    blur_source,
                    blur_state.texture_a.as_mut(),
                    blur_state.texture_b.as_mut(),
                ) {
                    let blur_result = apply_blur_passes(
                        renderer,
                        &src,
                        ping,
                        pong,
                        blur_size,
                        scale,
                        BLUR_ITERATIONS,
                    );

                    if blur_result.is_ok() {
                        // Cache the SAME blurred texture for ALL windows in this group
                        // Store both blur texture size and original screen size for coordinate mapping
                        for (window_key, _geometry, _alpha, z_idx) in &group.windows {
                            cache_blur_texture_for_window(
                                &output_name,
                                window_key,
                                BlurredTextureInfo {
                                    texture: pong.clone(),
                                    size: blur_size,
                                    screen_size: output_size,
                                    scale,
                                    background_state_hash: content_hash,
                                },
                            );
                            tracing::debug!(
                                global_z_idx = z_idx,
                                blur_w = blur_size.w,
                                blur_h = blur_size.h,
                                downsampled = downsample_enabled,
                                "Cached blur texture for window in group"
                            );
                        }
                        any_blur_applied = true;
                    } else {
                        tracing::warn!(
                            capture_z_threshold = group.capture_z_threshold,
                            "Blur passes failed for group"
                        );
                    }
                }
            }
        }
        blur_state.blur_applied = any_blur_applied;

        // Final render: collect ALL elements (blur backdrops will find their per-window cached textures)
        let final_elements: Vec<CosmicElement<R>> = workspace_elements(
            gpu,
            renderer,
            shell,
            zoom_state.as_ref(),
            now,
            output,
            previous_workspace,
            workspace,
            cursor_mode,
            &element_filter,
        )?;

        // Render final scene with blur backdrops to actual target
        let result = damage_tracker.render_output(
            renderer,
            target,
            if any_blur_applied { 0 } else { age }, // Force full redraw if blur active
            &final_elements,
            CLEAR_COLOR,
        )?;

        Ok((result, final_elements))
    } else {
        render_workspace(
            gpu,
            renderer,
            target,
            damage_tracker,
            age,
            None,
            shell,
            zoom_state.as_ref(),
            now,
            output,
            previous_workspace,
            workspace,
            cursor_mode,
            element_filter,
        )
    };

    match result {
        Ok((res, mut elements)) => {
            for (session, frame) in output.take_pending_frames() {
                if let Some(pending_image_copy_data) = render_session::<_, _, GlesTexture>(
                    renderer,
                    session.user_data().get::<SessionData>().unwrap(),
                    frame,
                    output.current_transform(),
                    |buffer, renderer, offscreen, dt, age, additional_damage| {
                        let old_len = if !additional_damage.is_empty() {
                            let area = output
                                .current_mode()
                                .ok_or(RenderError::OutputNoMode(OutputNoMode))
                                .map(
                                    |mode| {
                                        mode.size
                                            .to_logical(1)
                                            .to_buffer(1, Transform::Normal)
                                            .to_f64()
                                    }, /* TODO: Mode is Buffer..., why is this Physical in the first place */
                                )?;

                            let old_len = elements.len();
                            elements.extend(
                                additional_damage
                                    .into_iter()
                                    .map(|rect| {
                                        rect.to_f64()
                                            .to_logical(
                                                output.current_scale().fractional_scale(),
                                                output.current_transform(),
                                                &area,
                                            )
                                            .to_i32_round()
                                    })
                                    .map(DamageElement::new)
                                    .map(Into::into),
                            );

                            Some(old_len)
                        } else {
                            None
                        };

                        let res = dt.damage_output(age, &elements)?;

                        if let Some(old_len) = old_len {
                            elements.truncate(old_len);
                        }

                        let mut sync = SyncPoint::default();

                        if let (Some(damage), _) = &res {
                            // TODO: On Vulkan, may need to combine sync points instead of just using latest?
                            let blit_to_buffer =
                                |renderer: &mut R, blit_from: &mut R::Framebuffer<'_>| {
                                    if let Ok(dmabuf) = get_dmabuf(buffer) {
                                        let mut dmabuf_clone = dmabuf.clone();
                                        let mut fb = renderer.bind(&mut dmabuf_clone)?;
                                        for rect in damage.iter() {
                                            sync = renderer.blit(
                                                blit_from,
                                                &mut fb,
                                                *rect,
                                                *rect,
                                                TextureFilter::Nearest,
                                            )?;
                                        }
                                    } else {
                                        let fb = offscreen
                                            .expect("shm buffers should have offscreen target");
                                        for rect in damage.iter() {
                                            sync = renderer.blit(
                                                blit_from,
                                                fb,
                                                *rect,
                                                *rect,
                                                TextureFilter::Nearest,
                                            )?;
                                        }
                                    }

                                    Result::<_, R::Error>::Ok(())
                                };

                            // we would want to just assign a different framebuffer to a variable, depending on the code-path,
                            // but then rustc tries to equate the lifetime of target with the lifetime of our temporary fb...
                            // So instead of duplicating all the code, we use a closure..
                            if let Some(tex) = postprocess_texture.as_mut() {
                                let mut fb = renderer.bind(tex).map_err(RenderError::Rendering)?;
                                blit_to_buffer(renderer, &mut fb)
                                    .map_err(RenderError::Rendering)?;
                            } else {
                                blit_to_buffer(renderer, target).map_err(RenderError::Rendering)?;
                            }
                        }

                        Ok(RenderOutputResult {
                            damage: res.0,
                            sync,
                            states: res.1,
                        })
                    },
                )? {
                    pending_image_copy_data.send_success_when_ready(
                        output.current_transform(),
                        loop_handle,
                        now,
                    );
                }
            }

            Ok(res)
        }
        Err(err) => Err(err),
    }
}

#[profiling::function]
pub fn render_workspace<'d, R>(
    gpu: Option<&DrmNode>,
    renderer: &mut R,
    target: &mut R::Framebuffer<'_>,
    damage_tracker: &'d mut OutputDamageTracker,
    age: usize,
    additional_damage: Option<Vec<Rectangle<i32, Logical>>>,
    shell: &Arc<parking_lot::RwLock<Shell>>,
    zoom_level: Option<&ZoomState>,
    now: Time<Monotonic>,
    output: &Output,
    previous: Option<(WorkspaceHandle, usize, WorkspaceDelta)>,
    current: (WorkspaceHandle, usize),
    cursor_mode: CursorMode,
    element_filter: ElementFilter,
) -> Result<(RenderOutputResult<'d>, Vec<CosmicElement<R>>), RenderError<R::Error>>
where
    R: Renderer + ImportAll + ImportMem + ExportMem + Bind<Dmabuf> + AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
    R::Error: FromGlesError,
    CosmicElement<R>: RenderElement<R>,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    WorkspaceRenderElement<R>: RenderElement<R>,
{
    let mut elements: Vec<CosmicElement<R>> = workspace_elements(
        gpu,
        renderer,
        shell,
        zoom_level,
        now,
        output,
        previous,
        current,
        cursor_mode,
        &element_filter,
    )?;

    if let Some(additional_damage) = additional_damage {
        let output_geo = output.geometry().to_local(output).as_logical();
        elements.extend(
            additional_damage
                .into_iter()
                .filter_map(|rect| rect.intersection(output_geo))
                .map(DamageElement::new)
                .map(Into::<CosmicElement<R>>::into),
        );
    }

    let res = damage_tracker.render_output(
        renderer,
        target,
        age,
        &elements,
        CLEAR_COLOR, // TODO use a theme neutral color
    );

    res.map(|res| (res, elements))
}
