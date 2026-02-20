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

            // Primary shadow: blur 14, offset (0, 3), alpha 0.12
            let softness = 14.;
            let spread = 0.;
            let offset = [0., 3.];
            let color = [0., 0., 0., 0.12];

            // Secondary shadow: blur 5, offset (0, 5), alpha 0.02
            let softness_2 = 5.;
            let spread_2 = 0.;
            let offset_2 = [0., 5.];
            let color_2 = [0., 0., 0., 0.02];

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

            // Secondary shadow geometry
            let width_2 = softness_2;
            let sigma_2 = width_2 / 2.;
            let width_2 = ceil(sigma_2 * 4.);

            let offset_2_point: Point<f64, Local> =
                Point::new(ceil(offset_2[0]), ceil(offset_2[1]));
            let spread_2 = ceil(spread_2.abs()).copysign(spread_2);
            let offset_2 = offset_2_point - Point::new(spread_2, spread_2);

            let box_size_2 = if spread_2 >= 0. {
                geo.size + Size::new(spread_2, spread_2).upscale(2.)
            } else {
                geo.size - Size::new(-spread_2, -spread_2).upscale(2.)
            };

            let radius_2 = win_radius.map(|r| {
                if r > 0. {
                    r.saturating_add(spread_2)
                } else {
                    0.
                }
            });

            // Combine shader sizes (use the larger of the two)
            let shader_width = width.max(width_2);
            // Account for max offset in each direction
            let max_offset_x = offset.x.abs().max(offset_2.x.abs());
            let max_offset_y = offset.y.abs().max(offset_2.y.abs());
            let shader_size = geo.size
                + Size::from((shader_width + max_offset_x, shader_width + max_offset_y))
                    .upscale(2.)
                + Size::new(spread.max(spread_2), spread.max(spread_2)).upscale(2.);
            let mut shader_geo = Rectangle::new(
                Point::from((-shader_width - max_offset_x, -shader_width - max_offset_y)),
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

            // Secondary shadow transforms
            let geo_loc_2 = Vector2::new(
                -shader_geo.loc.x + offset_2.x,
                -shader_geo.loc.y + offset_2.y,
            );

            let input_to_geo_2 = (Matrix3::from_nonuniform_scale(area_size.x, area_size.y)
                * Matrix3::from_translation(Vector2::new(
                    -geo_loc_2.x / area_size.x,
                    -geo_loc_2.y / area_size.y,
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
                    // Secondary shadow uniforms
                    Uniform::new("shadow_color_2", color_2),
                    Uniform::new("sigma_2", sigma_2 as f32),
                    Uniform::new(
                        "input_to_geo_2",
                        UniformValue::Matrix3x3 {
                            matrices: vec![*AsRef::<[f32; 9]>::as_ref(&input_to_geo_2)],
                            transpose: false,
                        },
                    ),
                    Uniform::new("geo_size_2", [box_size_2.w as f32, box_size_2.h as f32]),
                    Uniform::new(
                        "corner_radius_2",
                        [
                            radius_2[0] as f32,
                            radius_2[1] as f32,
                            radius_2[2] as f32,
                            radius_2[3] as f32,
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
            .get(&surface_id)
            .filter(|(old_params, _)| &params == old_params)
            .is_none()
        {
            let shader = Self::get(renderer);

            // Primary shadow: blur 14, offset (0, 3), alpha 0.12
            let softness = 14.;
            let spread = 0.;
            let offset = [0., 3.];
            let color = [0., 0., 0., 0.12];

            // Secondary shadow: blur 5, offset (0, 5), alpha 0.02
            let softness_2 = 5.;
            let spread_2 = 0.;
            let offset_2 = [0., 5.];
            let color_2 = [0., 0., 0., 0.02];

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

            // Secondary shadow geometry
            let width_2 = softness_2;
            let sigma_2 = width_2 / 2.;
            let width_2 = ceil(sigma_2 * 4.);

            let offset_2_point: Point<f64, Local> =
                Point::new(ceil(offset_2[0]), ceil(offset_2[1]));
            let spread_2 = ceil(spread_2.abs()).copysign(spread_2);
            let offset_2 = offset_2_point - Point::new(spread_2, spread_2);

            let box_size_2 = if spread_2 >= 0. {
                geo.size + Size::new(spread_2, spread_2).upscale(2.)
            } else {
                geo.size - Size::new(-spread_2, -spread_2).upscale(2.)
            };

            let radius_2 = win_radius.map(|r| {
                if r > 0. {
                    r.saturating_add(spread_2)
                } else {
                    0.
                }
            });

            // Combine shader sizes (use the larger of the two)
            let shader_width = width.max(width_2);
            // Account for max offset in each direction
            let max_offset_x = offset.x.abs().max(offset_2.x.abs());
            let max_offset_y = offset.y.abs().max(offset_2.y.abs());
            let shader_size = geo.size
                + Size::from((shader_width + max_offset_x, shader_width + max_offset_y))
                    .upscale(2.)
                + Size::new(spread.max(spread_2), spread.max(spread_2)).upscale(2.);
            let mut shader_geo = Rectangle::new(
                Point::from((-shader_width - max_offset_x, -shader_width - max_offset_y)),
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

            // Secondary shadow transforms
            let geo_loc_2 = Vector2::new(
                -shader_geo.loc.x + offset_2.x,
                -shader_geo.loc.y + offset_2.y,
            );

            let input_to_geo_2 = (Matrix3::from_nonuniform_scale(area_size.x, area_size.y)
                * Matrix3::from_translation(Vector2::new(
                    -geo_loc_2.x / area_size.x,
                    -geo_loc_2.y / area_size.y,
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
                    // Secondary shadow uniforms
                    Uniform::new("shadow_color_2", color_2),
                    Uniform::new("sigma_2", sigma_2 as f32),
                    Uniform::new(
                        "input_to_geo_2",
                        UniformValue::Matrix3x3 {
                            matrices: vec![*AsRef::<[f32; 9]>::as_ref(&input_to_geo_2)],
                            transpose: false,
                        },
                    ),
                    Uniform::new("geo_size_2", [box_size_2.w as f32, box_size_2.h as f32]),
                    Uniform::new(
                        "corner_radius_2",
                        [
                            radius_2[0] as f32,
                            radius_2[1] as f32,
                            radius_2[2] as f32,
                            radius_2[3] as f32,
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
