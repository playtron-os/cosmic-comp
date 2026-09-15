use super::*;

#[test]
fn canvas_maps_pixel_centres_to_the_unsnapped_shape() {
    for scale in [1.0, 1.25, 1.5, 1.75, 2.0] {
        for phase in [0.0, 0.25, 0.5, 0.75] {
            for origin in [-101.0, 0.0, 439.0] {
                let shape = Rectangle::new(
                    (origin + phase, 37.0 + phase).into(),
                    (303.5, 199.25).into(),
                );
                let geometry = Geometry::new(shape, 1.0, scale);
                let pixels: Rectangle<i32, Physical> = geometry
                    .canvas
                    .as_logical()
                    .to_physical_precise_round(scale);
                for x in [0.5, f64::from(pixels.size.w) - 0.5] {
                    let shader_x = (x / f64::from(pixels.size.w))
                        * f64::from(geometry.draw_size[0])
                        - f64::from(geometry.shape_origin[0]);
                    let expected = (f64::from(pixels.loc.x) + x) / scale - shape.loc.x;
                    assert!((shader_x - expected).abs() < 0.0001);
                }
                assert!(f64::from(pixels.loc.x) <= (shape.loc.x - 1.0) * scale - 0.5);
                assert!(
                    f64::from(pixels.loc.x + pixels.size.w)
                        >= (shape.loc.x + shape.size.w + 1.0) * scale + 0.5
                );
            }
        }
    }
}

fn fill(distance: f64, scale: f64) -> f64 {
    (0.5 - distance * scale).clamp(0.0, 1.0)
}

#[test]
fn straight_strokes_keep_their_weight_at_fractional_positions() {
    for scale in [1.0, 1.25, 1.5, 1.75, 2.0] {
        for step in 0..32 {
            let edge = 10.0 + f64::from(step) / 32.0;
            let mut border = 0.0;
            let mut ring = 0.0;
            for px in 0..40 {
                let x = (f64::from(px) + 0.5) / scale;
                let body = fill(edge - x, scale);
                let inner = fill(edge + 1.0 - x, scale);
                let outer = fill(edge - 1.0 - x, scale);
                border += body - inner;
                ring += outer - body;
                assert!(((body - inner) + (outer - body) - (outer - inner)).abs() < 0.00001);
            }
            assert!((border - scale).abs() < 0.00001);
            assert!((ring - scale).abs() < 0.00001);
        }
    }
}

/// Run with `LIBGL_ALWAYS_SOFTWARE=1 cargo test gles_outline -- --ignored --nocapture`.
#[test]
#[ignore = "requires surfaceless EGL (Mesa llvmpipe or a GPU driver)"]
fn gles_outline_has_consistent_edges_and_a_single_shared_blend() -> anyhow::Result<()> {
    use crate::backend::render::IndicatorShader;
    use iced_core::Color;
    use smithay::{
        backend::{
            allocator::Fourcc,
            egl::{EGLContext, EGLDisplay, native::EGLSurfacelessDisplay},
            renderer::{
                Bind, ExportMem, Offscreen, TextureMapping,
                damage::OutputDamageTracker,
                element::{Element, Id},
                gles::GlesRenderbuffer,
                glow::GlowRenderer,
            },
        },
        utils::{Size, Transform},
    };
    use std::borrow::BorrowMut;

    // SAFETY: the surfaceless display owns no borrowed native handles and this
    // freshly created context is used only on this test thread.
    let display = unsafe { EGLDisplay::new(EGLSurfacelessDisplay)? };
    let context = EGLContext::new(&display)?;
    let mut renderer = unsafe { GlowRenderer::new(context)? };
    let program = IndicatorShader::compile(renderer.borrow_mut())?;
    renderer
        .egl_context()
        .user_data()
        .insert_if_missing(|| IndicatorShader(program));
    let key = Id::new();
    let output_size: Size<i32, Physical> = (256, 192).into();
    for (radii, bottom_border) in [([9.0; 4], true), ([0.0, 9.0, 0.0, 9.0], false)] {
        for parent_alpha in [1.0, 0.5] {
            let border_alpha = if parent_alpha == 1.0 { 1.0 } else { 0.5 };
            let ring_alpha = if parent_alpha == 1.0 { 1.0 } else { 0.25 };
            let mut previous_id = None;
            for scale in [1.0, 1.25, 1.5, 1.75, 2.0] {
                for phase in [0.0, 0.25, 0.5, 0.75] {
                    let shape =
                        Rectangle::new((12.0 + phase, 13.0 + phase).into(), (100.0, 60.0).into());
                    let element = IndicatorShader::animated_outline_with_bottom_border(
                        &renderer,
                        key.clone(),
                        shape,
                        1.0,
                        radii,
                        parent_alpha,
                        scale,
                        Color::from_rgba(1.0, 0.0, 0.0, border_alpha),
                        1.0,
                        Color::from_rgba(0.0, 1.0, 0.0, ring_alpha),
                        None,
                        bottom_border,
                    );
                    if let Some(previous) = &previous_id {
                        assert_eq!(
                            previous,
                            element.id(),
                            "moving/resizing must retain the render element ID"
                        );
                    }
                    previous_id = Some(element.id().clone());
                    let mut buffer = <GlowRenderer as Offscreen<GlesRenderbuffer>>::create_buffer(
                        &mut renderer,
                        Fourcc::Abgr8888,
                        (256, 192).into(),
                    )?;
                    let mut fb = renderer.bind(&mut buffer)?;
                    let mut tracker =
                        OutputDamageTracker::new(output_size, scale, Transform::Normal);
                    tracker.render_output(&mut renderer, &mut fb, 0, &[element], [0.0; 4])?;
                    let mapping = renderer.copy_framebuffer(
                        &fb,
                        Rectangle::from_size((256, 192).into()),
                        Fourcc::Abgr8888,
                    )?;
                    let flipped = mapping.flipped();
                    let bytes = renderer.map_texture(&mapping)?;
                    let pixel = |x: usize, y: usize| {
                        // flipped() is relative to GL's bottom-left origin.
                        let y = if flipped { y } else { 191 - y };
                        let i = (y * 256 + x) * 4;
                        [bytes[i], bytes[i + 1], bytes[i + 2], bytes[i + 3]]
                            .map(|v| f64::from(v) / 255.0)
                    };
                    let cx = ((shape.loc.x + shape.size.w / 2.0) * scale).floor() as usize;
                    let cy = ((shape.loc.y + shape.size.h / 2.0) * scale).floor() as usize;
                    // Red is the inner border; green is the adjacent outer ring.
                    // Integrated alpha must equal logical width * scale * style alpha * parent alpha.
                    let left: [f64; 2] =
                        std::array::from_fn(|channel| (0..cx).map(|x| pixel(x, cy)[channel]).sum());
                    let right: [f64; 2] = std::array::from_fn(|channel| {
                        (cx..256).map(|x| pixel(x, cy)[channel]).sum()
                    });
                    let top: [f64; 2] =
                        std::array::from_fn(|channel| (0..cy).map(|y| pixel(cx, y)[channel]).sum());
                    let bottom: [f64; 2] = std::array::from_fn(|channel| {
                        (cy..192).map(|y| pixel(cx, y)[channel]).sum()
                    });
                    for (index, side) in [left, right, top, bottom].into_iter().enumerate() {
                        let width = if index == 3 && !bottom_border {
                            0.0
                        } else {
                            scale
                        };
                        assert!(
                            (side[0] - width * f64::from(border_alpha * parent_alpha)).abs()
                                < 0.016,
                            "border: scale={scale}, phase={phase}, sides={left:?}/{right:?}/{top:?}/{bottom:?}"
                        );
                        assert!(
                            (side[1] - width * f64::from(ring_alpha * parent_alpha)).abs() < 0.016,
                            "ring: scale={scale}, phase={phase}, sides={left:?}/{right:?}/{top:?}/{bottom:?}"
                        );
                    }
                    for y in 0..192 {
                        for x in 0..256 {
                            let [red, green, blue, alpha] = pixel(x, y);
                            assert!(
                                (alpha - red - green).abs() < 0.012,
                                "shared edge must be a single premultiplied sum"
                            );
                            assert_eq!(blue, 0.0);
                        }
                    }
                    assert_eq!(
                        pixel(cx, cy),
                        [0.0; 4],
                        "the client interior stays transparent"
                    );
                    if !bottom_border && scale == 1.0 && phase == 0.0 {
                        let x = shape.loc.x as usize;
                        let y = shape.loc.y as usize;
                        assert_eq!(pixel(x, y), [0.0; 4], "top-left must be rounded");
                        assert_eq!(pixel(x + 99, y), [0.0; 4], "top-right must be rounded");
                        assert!(pixel(x, y + 59)[0] > 0.2 * f64::from(parent_alpha * border_alpha));
                        assert!(
                            pixel(x + 99, y + 59)[0] > 0.2 * f64::from(parent_alpha * border_alpha),
                            "bottom-right must be square; vertical strokes still reach the join"
                        );
                    }
                }
            }
        }
        eprintln!("GLES outline verified at five scales and four pixel phases");
    }
    Ok(())
}

#[test]
#[ignore = "requires surfaceless EGL (Mesa llvmpipe or a GPU driver)"]
fn gles_outline_focus_reveals_bidirectionally_and_lands_on_the_static_pixels() -> anyhow::Result<()>
{
    use crate::backend::render::{IndicatorShader, OutlineFocus};
    use iced_core::Color;
    use smithay::backend::{
        allocator::Fourcc,
        egl::{EGLContext, EGLDisplay, native::EGLSurfacelessDisplay},
        renderer::{
            Bind, ExportMem, Offscreen, TextureMapping,
            damage::OutputDamageTracker,
            element::{Element, Id},
            gles::GlesRenderbuffer,
            glow::GlowRenderer,
        },
    };
    use smithay::utils::Transform;
    use std::borrow::BorrowMut;

    // SAFETY: this surfaceless context is owned and used on this test thread.
    let display = unsafe { EGLDisplay::new(EGLSurfacelessDisplay)? };
    let context = EGLContext::new(&display)?;
    let mut renderer = unsafe { GlowRenderer::new(context)? };
    let shader = IndicatorShader::compile(renderer.borrow_mut())?;
    renderer
        .egl_context()
        .user_data()
        .insert_if_missing(|| IndicatorShader(shader));
    let key = Id::new();
    let neutral = Color::from_rgba(0.0, 0.0, 1.0, 0.5);
    let accent = Color::from_rgba(1.0, 0.0, 0.0, 0.5);
    let ring = Color::from_rgba(0.0, 1.0, 0.0, 0.22);

    for halo in [false, true] {
        for scale in [1.0, 1.25, 1.5, 2.0] {
            for phase in [0.0, 0.25, 0.5, 0.75] {
                let shape = Rectangle::new(
                    (12.0 + phase, 13.0 + phase).into(),
                    (100.0, if halo { 40.0 } else { 60.0 }).into(),
                );
                let mut previous_id = None;
                let mut draw = |progress: Option<f32>,
                                focused: bool|
                 -> anyhow::Result<Vec<[u8; 4]>> {
                    let element = IndicatorShader::animated_outline(
                        &renderer,
                        key.clone(),
                        shape,
                        if halo { 0.5 } else { 1.0 },
                        [if halo { 20.0 } else { 9.0 }; 4],
                        0.75,
                        scale,
                        if focused { accent } else { neutral },
                        1.0,
                        if focused { ring } else { Color::TRANSPARENT },
                        progress.map(|progress| OutlineFocus {
                            progress,
                            tip: 30.0,
                            halo,
                            neutral,
                        }),
                    );
                    if let Some(previous) = &previous_id {
                        assert_eq!(
                            previous,
                            element.id(),
                            "animation must retain damage identity"
                        );
                    }
                    previous_id = Some(element.id().clone());
                    let mut buffer = <GlowRenderer as Offscreen<GlesRenderbuffer>>::create_buffer(
                        &mut renderer,
                        Fourcc::Abgr8888,
                        (256, 192).into(),
                    )?;
                    let mut fb = renderer.bind(&mut buffer)?;
                    let mut tracker =
                        OutputDamageTracker::new((256, 192), scale, Transform::Normal);
                    tracker.render_output(&mut renderer, &mut fb, 0, &[element], [0.0; 4])?;
                    let mapping = renderer.copy_framebuffer(
                        &fb,
                        Rectangle::from_size((256, 192).into()),
                        Fourcc::Abgr8888,
                    )?;
                    let flipped = mapping.flipped();
                    let bytes = renderer.map_texture(&mapping)?;
                    Ok((0..192)
                        .flat_map(|y| (0..256).map(move |x| (x, y)))
                        .map(|(x, y)| {
                            let y = if flipped { y } else { 191 - y };
                            let i = (y * 256 + x) * 4;
                            [bytes[i], bytes[i + 1], bytes[i + 2], bytes[i + 3]]
                        })
                        .collect())
                };
                let start = draw(Some(0.0), true)?;
                assert_eq!(start, draw(None, false)?, "start must be exactly neutral");
                if !halo {
                    let first = draw(Some(0.01), true)?;
                    let settled = draw(None, true)?;
                    let red_sum = |pixels: &[[u8; 4]]| {
                        pixels.iter().map(|p| u64::from(p[0])).sum::<u64>() as f64
                    };
                    assert!(
                        red_sum(&first) < red_sum(&settled) * 0.04,
                        "the beginning of the sweep must reveal only a small part of the window outline"
                    );
                }
                let mut previous = start;
                for progress in [0.005, 0.01, 0.1, 0.25, 0.5, 0.75, 1.0] {
                    let pixels = draw(Some(progress), true)?;
                    for (before, after) in previous.iter().zip(&pixels) {
                        assert!(
                            after[0] >= before[0] && after[1] >= before[1] && after[2] <= before[2],
                            "every pixel reveals monotonically, halo={halo}, progress={progress}"
                        );
                    }
                    if scale == 1.0 && phase == 0.0 {
                        for y in 10..80 {
                            for x in 10..62 {
                                let a = pixels[y * 256 + x];
                                let b = pixels[y * 256 + (123 - x)];
                                assert!(
                                    a.iter().zip(b).all(|(a, b)| a.abs_diff(b) <= 1),
                                    "left/right symmetry"
                                );
                            }
                        }
                        if progress == 0.5 {
                            // Both start at the Halo ends. The window has reached
                            // its sides, not bottom center; the Halo's center is last.
                            let at = |x: usize, y: usize| pixels[(13 + y) * 256 + 12 + x];
                            assert!(at(23, 0)[0] > 0);
                            if halo {
                                assert_eq!(at(50, 0)[0], 0);
                                assert!(at(50, 0)[2] > 0);
                            } else {
                                assert!(at(0, 30)[0] > 0);
                                assert_eq!(at(50, 59)[0], 0);
                                assert!(at(50, 59)[2] > 0);
                            }
                        }
                    }
                    previous = pixels;
                }
                assert_eq!(
                    previous,
                    draw(None, true)?,
                    "settling must not double-blend or flash"
                );
            }
        }
    }
    Ok(())
}
