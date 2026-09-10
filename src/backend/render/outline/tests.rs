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
                let geometry = Geometry::new(shape, 1, scale);
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
    for parent_alpha in [1.0, 0.5] {
        let border_alpha = if parent_alpha == 1.0 { 1.0 } else { 0.5 };
        let ring_alpha = if parent_alpha == 1.0 { 1.0 } else { 0.25 };
        let mut previous_id = None;
        for scale in [1.0, 1.25, 1.5, 1.75, 2.0] {
            for phase in [0.0, 0.25, 0.5, 0.75] {
                let shape =
                    Rectangle::new((12.0 + phase, 13.0 + phase).into(), (100.0, 60.0).into());
                let element = IndicatorShader::window_outline(
                    &renderer,
                    key.clone(),
                    shape,
                    1,
                    [9; 4],
                    parent_alpha,
                    scale,
                    Color::from_rgba(1.0, 0.0, 0.0, border_alpha),
                    Some(Color::from_rgba(0.0, 1.0, 0.0, ring_alpha)),
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
                let mut tracker = OutputDamageTracker::new(output_size, scale, Transform::Normal);
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
                let right: [f64; 2] =
                    std::array::from_fn(|channel| (cx..256).map(|x| pixel(x, cy)[channel]).sum());
                let top: [f64; 2] =
                    std::array::from_fn(|channel| (0..cy).map(|y| pixel(cx, y)[channel]).sum());
                let bottom: [f64; 2] =
                    std::array::from_fn(|channel| (cy..192).map(|y| pixel(cx, y)[channel]).sum());
                for side in [left, right, top, bottom] {
                    assert!(
                        (side[0] - scale * f64::from(border_alpha * parent_alpha)).abs() < 0.016,
                        "border: scale={scale}, phase={phase}, sides={left:?}/{right:?}/{top:?}/{bottom:?}"
                    );
                    assert!(
                        (side[1] - scale * f64::from(ring_alpha * parent_alpha)).abs() < 0.016,
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
            }
        }
    }
    eprintln!("GLES outline verified at five scales and four pixel phases");
    Ok(())
}
