use std::{borrow::Borrow, cell::RefCell, collections::HashMap};

use cgmath::{Matrix3, Vector2};
use smithay::{
    backend::renderer::{
        element::Kind,
        gles::{
            GlesPixelProgram, GlesRenderer, Uniform, UniformValue, element::PixelShaderElement,
        },
    },
    utils::{Coordinate, IsAlive, Point, Rectangle, Size},
};
use wayland_backend::server::ObjectId;

use crate::{
    backend::render::element::AsGlowRenderer,
    shell::element::CosmicMappedKey,
    utils::prelude::{Local, RectLocalExt},
};

pub static SHADOW_SHADER: &str = include_str!("./shaders/shadow.frag");
pub struct ShadowShader(pub GlesPixelProgram);

#[derive(Debug, PartialEq)]
pub struct ShadowParameters {
    geo: Rectangle<i32, Local>,
    scale: f64,
    alpha: f32,
    radius: [u8; 4],
    dark_mode: bool,
}
type ShadowCache = RefCell<HashMap<CosmicMappedKey, (ShadowParameters, PixelShaderElement)>>;
/// Cache for layer surface shadows (keyed by WlSurface ObjectId)
type LayerShadowCache = RefCell<HashMap<ObjectId, (ShadowParameters, PixelShaderElement)>>;

impl ShadowShader {
    pub fn get<R: AsGlowRenderer>(renderer: &R) -> GlesPixelProgram {
        Borrow::<GlesRenderer>::borrow(renderer.glow_renderer())
            .egl_context()
            .user_data()
            .get::<ShadowShader>()
            .expect("Custom Shaders not initialized")
            .0
            .clone()
    }

    pub fn element<R: AsGlowRenderer>(
        renderer: &R,
        key: CosmicMappedKey,
        geo: Rectangle<i32, Local>,
        radius: [u8; 4],
        alpha: f32,
        scale: f64,
        dark_mode: bool,
    ) -> PixelShaderElement {
        let params = ShadowParameters {
            geo,
            scale,
            alpha,
            radius,
            dark_mode,
        };
        let ceil = |logical: f64| (logical * scale).ceil() / scale;

        let mut geo = geo.to_f64();
        let fractional_pixel = scale.ceil() / scale;
        geo.loc.x += fractional_pixel;
        geo.loc.y += fractional_pixel;
        geo.size.w -= fractional_pixel * 2.;
        geo.size.h -= fractional_pixel * 2.;

        let user_data = Borrow::<GlesRenderer>::borrow(renderer.glow_renderer())
            .egl_context()
            .user_data();

        user_data.insert_if_missing(|| ShadowCache::new(HashMap::new()));
        let mut cache = user_data.get::<ShadowCache>().unwrap().borrow_mut();
        cache.retain(|k, _| k.alive());

        if cache
            .get(&key)
            .filter(|(old_params, _)| &params == old_params)
            .is_none()
        {
            let shader = Self::get(renderer);

            // Primary shadow: rgba(0, 0, 0, 0.1) 0px 24px 80px
            let softness = 80.;
            let spread = 0.;
            let offset = [0., 24.];
            let color = [0., 0., 0., 0.1];

            let radius = radius.map(|r| ceil(r as f64));
            let radius = [
                radius[3], // top_left
                radius[1], // top_right
                radius[0], // bottom_right
                radius[2], // bottom_left
            ];

            // Primary shadow geometry
            let width = softness;
            let sigma = width / 2.;
            let width = ceil(sigma * 4.);

            let offset: Point<f64, Local> = Point::new(ceil(offset[0]), ceil(offset[1]));
            let spread = ceil(spread.abs()).copysign(spread);
            let offset = offset - Point::new(spread, spread);

            let box_size = if spread >= 0. {
                geo.size + Size::new(spread, spread).upscale(2.)
            } else {
                geo.size - Size::new(-spread, -spread).upscale(2.)
            };

            let win_radius = radius;
            let radius = radius.map(|r| if r > 0. { r.saturating_add(spread) } else { 0. });

            let shader_size = geo.size
                + Size::from((width + offset.x.abs(), width + offset.y.abs())).upscale(2.)
                + Size::new(spread, spread).upscale(2.);
            let mut shader_geo = Rectangle::new(
                Point::from((-width - offset.x.abs(), -width - offset.y.abs())),
                shader_size,
            );

            // Primary shadow transforms
            let window_geo = Rectangle::new(Point::new(0., 0.) - shader_geo.loc, geo.size);
            let area_size = Vector2::new(shader_geo.size.w, shader_geo.size.h);
            let geo_loc = Vector2::new(-shader_geo.loc.x + offset.x, -shader_geo.loc.y + offset.y);

            let input_to_geo = (Matrix3::from_nonuniform_scale(area_size.x, area_size.y)
                * Matrix3::from_translation(Vector2::new(
                    -geo_loc.x / area_size.x,
                    -geo_loc.y / area_size.y,
                )))
            .cast::<f32>()
            .unwrap();

            // Window cutout transforms
            let window_geo_loc = Vector2::new(window_geo.loc.x, window_geo.loc.y);
            let window_input_to_geo = (Matrix3::from_nonuniform_scale(area_size.x, area_size.y)
                * Matrix3::from_translation(Vector2::new(
                    -window_geo_loc.x / area_size.x,
                    -window_geo_loc.y / area_size.y,
                )))
            .cast::<f32>()
            .unwrap();

            shader_geo.loc += geo.loc;

            let element = PixelShaderElement::new(
                shader,
                shader_geo.to_i32_up().as_logical(),
                None,
                alpha,
                vec![
                    // Primary shadow uniforms
                    Uniform::new("shadow_color", color),
                    Uniform::new("sigma", sigma as f32),
                    Uniform::new(
                        "input_to_geo",
                        UniformValue::Matrix3x3 {
                            matrices: vec![*AsRef::<[f32; 9]>::as_ref(&input_to_geo)],
                            transpose: false,
                        },
                    ),
                    Uniform::new("geo_size", [box_size.w as f32, box_size.h as f32]),
                    Uniform::new(
                        "corner_radius",
                        [
                            radius[0] as f32,
                            radius[1] as f32,
                            radius[2] as f32,
                            radius[3] as f32,
                        ],
                    ),
                    // Window cutout uniforms
                    Uniform::new(
                        "window_input_to_geo",
                        UniformValue::Matrix3x3 {
                            matrices: vec![*AsRef::<[f32; 9]>::as_ref(&window_input_to_geo)],
                            transpose: false,
                        },
                    ),
                    Uniform::new(
                        "window_geo_size",
                        [window_geo.size.w as f32, window_geo.size.h as f32],
                    ),
                    Uniform::new(
                        "window_corner_radius",
                        [
                            win_radius[0] as f32,
                            win_radius[1] as f32,
                            win_radius[2] as f32,
                            win_radius[3] as f32,
                        ],
                    ),
                ],
                Kind::Unspecified,
            );

            cache.insert(key.clone(), (params, element));
        }

        cache.get(&key).unwrap().1.clone()
    }

    /// Create a shadow element for layer surfaces (uses surface ID for caching)
    pub fn layer_element<R: AsGlowRenderer>(
        renderer: &R,
        surface_id: &ObjectId,
        geo: Rectangle<i32, Local>,
        radius: [u8; 4],
        alpha: f32,
        scale: f64,
        dark_mode: bool,
    ) -> PixelShaderElement {
        let params = ShadowParameters {
            geo,
            scale,
            alpha,
            radius,
            dark_mode,
        };
        let ceil = |logical: f64| (logical * scale).ceil() / scale;

        let mut geo = geo.to_f64();
        let fractional_pixel = scale.ceil() / scale;
        geo.loc.x += fractional_pixel;
        geo.loc.y += fractional_pixel;
        geo.size.w -= fractional_pixel * 2.;
        geo.size.h -= fractional_pixel * 2.;

        let user_data = Borrow::<GlesRenderer>::borrow(renderer.glow_renderer())
            .egl_context()
            .user_data();

        user_data.insert_if_missing(|| LayerShadowCache::new(HashMap::new()));
        let mut cache = user_data.get::<LayerShadowCache>().unwrap().borrow_mut();

        if cache
            .get(surface_id)
            .filter(|(old_params, _)| &params == old_params)
            .is_none()
        {
            let shader = Self::get(renderer);

            // Primary shadow: rgba(0, 0, 0, 0.12) 0px 8px 32px
            let softness = 32.;
            let spread = 0.;
            let offset = [0., 8.];
            let color = [0., 0., 0., 0.12];

            let radius = radius.map(|r| ceil(r as f64));
            let radius = [
                radius[3], // top_left
                radius[1], // top_right
                radius[0], // bottom_right
                radius[2], // bottom_left
            ];

            // Primary shadow geometry
            let width = softness;
            let sigma = width / 2.;
            let width = ceil(sigma * 4.);

            let offset: Point<f64, Local> = Point::new(ceil(offset[0]), ceil(offset[1]));
            let spread = ceil(spread.abs()).copysign(spread);
            let offset = offset - Point::new(spread, spread);

            let box_size = if spread >= 0. {
                geo.size + Size::new(spread, spread).upscale(2.)
            } else {
                geo.size - Size::new(-spread, -spread).upscale(2.)
            };

            let win_radius = radius;
            let radius = radius.map(|r| if r > 0. { r.saturating_add(spread) } else { 0. });

            let shader_size = geo.size
                + Size::from((width + offset.x.abs(), width + offset.y.abs())).upscale(2.)
                + Size::new(spread, spread).upscale(2.);
            let mut shader_geo = Rectangle::new(
                Point::from((-width - offset.x.abs(), -width - offset.y.abs())),
                shader_size,
            );

            // Primary shadow transforms
            let window_geo = Rectangle::new(Point::new(0., 0.) - shader_geo.loc, geo.size);
            let area_size = Vector2::new(shader_geo.size.w, shader_geo.size.h);
            let geo_loc = Vector2::new(-shader_geo.loc.x + offset.x, -shader_geo.loc.y + offset.y);

            let input_to_geo = (Matrix3::from_nonuniform_scale(area_size.x, area_size.y)
                * Matrix3::from_translation(Vector2::new(
                    -geo_loc.x / area_size.x,
                    -geo_loc.y / area_size.y,
                )))
            .cast::<f32>()
            .unwrap();

            // Window cutout transforms
            let window_geo_loc = Vector2::new(window_geo.loc.x, window_geo.loc.y);
            let window_input_to_geo = (Matrix3::from_nonuniform_scale(area_size.x, area_size.y)
                * Matrix3::from_translation(Vector2::new(
                    -window_geo_loc.x / area_size.x,
                    -window_geo_loc.y / area_size.y,
                )))
            .cast::<f32>()
            .unwrap();

            shader_geo.loc += geo.loc;

            let element = PixelShaderElement::new(
                shader,
                shader_geo.to_i32_up().as_logical(),
                None,
                alpha,
                vec![
                    // Primary shadow uniforms
                    Uniform::new("shadow_color", color),
                    Uniform::new("sigma", sigma as f32),
                    Uniform::new(
                        "input_to_geo",
                        UniformValue::Matrix3x3 {
                            matrices: vec![*AsRef::<[f32; 9]>::as_ref(&input_to_geo)],
                            transpose: false,
                        },
                    ),
                    Uniform::new("geo_size", [box_size.w as f32, box_size.h as f32]),
                    Uniform::new(
                        "corner_radius",
                        [
                            radius[0] as f32,
                            radius[1] as f32,
                            radius[2] as f32,
                            radius[3] as f32,
                        ],
                    ),
                    // Window cutout uniforms
                    Uniform::new(
                        "window_input_to_geo",
                        UniformValue::Matrix3x3 {
                            matrices: vec![*AsRef::<[f32; 9]>::as_ref(&window_input_to_geo)],
                            transpose: false,
                        },
                    ),
                    Uniform::new(
                        "window_geo_size",
                        [window_geo.size.w as f32, window_geo.size.h as f32],
                    ),
                    Uniform::new(
                        "window_corner_radius",
                        [
                            win_radius[0] as f32,
                            win_radius[1] as f32,
                            win_radius[2] as f32,
                            win_radius[3] as f32,
                        ],
                    ),
                ],
                Kind::Unspecified,
            );

            cache.insert(surface_id.clone(), (params, element));
        }

        cache.get(surface_id).unwrap().1.clone()
    }
}
