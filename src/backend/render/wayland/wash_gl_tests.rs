//! The workspace washes drawn by the real shaders, read back from the GPU.
//! Run with `LIBGL_ALWAYS_SOFTWARE=1 cargo test gles_ -- --ignored`.

use iced_core::Color;
use smithay::{
    backend::{
        allocator::Fourcc,
        egl::{EGLContext, EGLDisplay, native::EGLSurfacelessDisplay},
        renderer::{
            Bind, ExportMem, ImportMem, Offscreen, Renderer, TextureMapping,
            damage::OutputDamageTracker,
            element::{Id, Kind, texture::TextureRenderElement},
            gles::{
                GlesRenderbuffer, GlesRenderer, UniformName, UniformType,
                element::{PixelShaderElement, TextureShaderElement},
            },
        },
    },
    utils::{Physical, Rectangle, Size, Transform},
};

use super::washed_surface::{DESKTOP_WASH_SHADER, DesktopWashShader, washed};
use crate::backend::render::SWITCH_WASH_SHADER;

const W: i32 = 64;
const H: i32 = 64;

fn renderer() -> anyhow::Result<GlesRenderer> {
    // SAFETY: the surfaceless display owns no borrowed native handles and this
    // freshly created context is used only on this test thread.
    let display = unsafe { EGLDisplay::new(EGLSurfacelessDisplay)? };
    let context = EGLContext::new(&display)?;
    Ok(unsafe { GlesRenderer::new(context)? })
}

/// Draw `elements` into a fresh RGBA buffer and return its rows top to bottom.
fn draw<E>(renderer: &mut GlesRenderer, elements: &[E]) -> anyhow::Result<Vec<[f32; 4]>>
where
    E: smithay::backend::renderer::element::RenderElement<GlesRenderer>,
{
    let size: Size<i32, Physical> = (W, H).into();
    let mut buffer = <GlesRenderer as Offscreen<GlesRenderbuffer>>::create_buffer(
        renderer,
        Fourcc::Abgr8888,
        (W, H).into(),
    )?;
    let mut fb = renderer.bind(&mut buffer)?;
    let mut tracker = OutputDamageTracker::new(size, 1.0, Transform::Normal);
    tracker.render_output(renderer, &mut fb, 0, elements, [0.0; 4])?;
    let mapping =
        renderer.copy_framebuffer(&fb, Rectangle::from_size((W, H).into()), Fourcc::Abgr8888)?;
    let flipped = mapping.flipped();
    let bytes = renderer.map_texture(&mapping)?;
    Ok((0..H as usize)
        .flat_map(|y| {
            let y = if flipped { y } else { H as usize - 1 - y };
            (0..W as usize).map(move |x| (y * W as usize + x) * 4)
        })
        .map(|i| std::array::from_fn(|c| f32::from(bytes[i + c]) / 255.0))
        .collect())
}

#[test]
#[ignore = "requires surfaceless EGL (Mesa llvmpipe or a GPU driver)"]
fn gles_the_desktop_wash_soft_lights_every_wallpaper_tone() -> anyhow::Result<()> {
    let mut renderer = renderer()?;
    let program = renderer.compile_custom_texture_shader(
        DESKTOP_WASH_SHADER,
        &[
            UniformName::new("wash_color", UniformType::_3f),
            UniformName::new("wash_amount", UniformType::_1f),
        ],
    )?;
    // A grey ramp across, so every column is one backdrop tone through both formula branches.
    let ramp: Vec<u8> = (0..H)
        .flat_map(|_| (0..W).flat_map(|x| [(x * 255 / (W - 1)) as u8; 3].into_iter().chain([255])))
        .collect();
    let texture = renderer.import_memory(&ramp, Fourcc::Abgr8888, (W, H).into(), false)?;
    for color in [
        Color::from_rgb8(0xe9, 0x7c, 0x86),
        Color::from_rgb8(0x4f, 0xb8, 0x8c),
        Color::from_rgb8(0x7c, 0x9f, 0xe0),
    ] {
        let element = TextureShaderElement::new(
            TextureRenderElement::from_static_texture(
                Id::new(),
                renderer.context_id(),
                (0.0, 0.0),
                texture.clone(),
                1,
                Transform::Normal,
                None,
                None,
                None,
                None,
                Kind::Unspecified,
            ),
            program.clone(),
            DesktopWashShader::uniforms(color),
        );
        let pixels = draw(&mut renderer, &[element])?;
        for x in 0..W as usize {
            let base = (x as f32 * 255.0 / (W - 1) as f32).floor() / 255.0;
            let got = pixels[(H as usize / 2) * W as usize + x];
            for (channel, source) in [color.r, color.g, color.b].into_iter().enumerate() {
                let want = washed(base, source);
                assert!(
                    (got[channel] - want).abs() <= 1.5 / 255.0,
                    "column {x} channel {channel}: drew {} for {want}",
                    got[channel]
                );
            }
            assert_eq!(got[3], 1.0, "the wallpaper stays opaque");
        }
    }
    Ok(())
}

#[test]
#[ignore = "requires surfaceless EGL (Mesa llvmpipe or a GPU driver)"]
fn gles_the_switch_wash_rises_from_the_edge_it_came_from() -> anyhow::Result<()> {
    let mut renderer = renderer()?;
    let program = renderer.compile_custom_pixel_shader(
        SWITCH_WASH_SHADER,
        &[
            UniformName::new("color", UniformType::_3f),
            UniformName::new("from_bottom", UniformType::_1f),
            UniformName::new("reach", UniformType::_1f),
        ],
    )?;
    let color = [0.9, 0.5, 0.2];
    let alpha = 0.14;
    for from_bottom in [true, false] {
        let element = PixelShaderElement::new(
            program.clone(),
            Rectangle::from_size((W, H).into()),
            None,
            alpha,
            vec![
                smithay::backend::renderer::gles::Uniform::new("color", color),
                smithay::backend::renderer::gles::Uniform::new(
                    "from_bottom",
                    if from_bottom { 1.0 } else { 0.0 },
                ),
                smithay::backend::renderer::gles::Uniform::new("reach", 0.55),
            ],
            Kind::Unspecified,
        );
        let pixels = draw(&mut renderer, &[element])?;
        let at = |row: usize| pixels[row * W as usize + W as usize / 2];
        let (edge, far) = if from_bottom {
            (at(H as usize - 1), at(0))
        } else {
            (at(0), at(H as usize - 1))
        };
        assert!((edge[3] - alpha).abs() < 0.02, "edge alpha {edge:?}");
        for c in 0..3 {
            assert!(
                (edge[c] - color[c] * edge[3]).abs() < 0.02,
                "premultiplied {edge:?}"
            );
        }
        assert_eq!(far, [0.0; 4], "nothing past the reach");
        // Gone by 55% of the height from its edge.
        let past = (0.6 * H as f32) as usize;
        let row = if from_bottom {
            H as usize - 1 - past
        } else {
            past
        };
        assert_eq!(at(row)[3], 0.0);
    }
    Ok(())
}
