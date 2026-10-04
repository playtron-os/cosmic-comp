// SPDX-License-Identifier: GPL-3.0-only

//! Whether a window has drawn anything yet.
//!
//! A game maps its window seconds before it draws into it. Fading from the
//! launcher's loading screen into that window fades into black, so game mode
//! asks this before it leaves the launcher. The window's surface tree is
//! rendered into a tiny offscreen and read back, as `adaptive_foreground`
//! samples the wallpaper.

use smithay::{
    backend::{
        allocator::Fourcc,
        renderer::{
            Color32F, ExportMem, ImportAll, Offscreen, Renderer,
            damage::OutputDamageTracker,
            element::{Kind, surface::WaylandSurfaceRenderElement},
            gles::GlesRenderbuffer,
        },
    },
    reexports::wayland_server::protocol::wl_surface::WlSurface,
    utils::{Logical, Physical, Point, Rectangle, Scale, Size, Transform},
};

use crate::{
    backend::render::{RendererRef, element::AsGlowRenderer},
    state::{State, advertised_node_for_surface},
};

/// Downsample target. Any texel of real content survives a 16x16 reduction.
const SAMPLE_EDGE: i32 = 16;

/// A channel this far above black counts as drawn: a fade in from black
/// crosses it within a few frames, and a black clear never does.
const DRAWN: u8 = 10;

/// Whether `surface`, `size` across, has drawn anything but black. `None` when
/// it could not be sampled.
pub fn has_drawn(state: &mut State, surface: &WlSurface, size: Size<i32, Logical>) -> Option<bool> {
    if size.w <= 0 || size.h <= 0 {
        return None;
    }
    let dh = state.common.display_handle.clone();
    let renderer = state
        .backend
        .offscreen_renderer(|kms| {
            advertised_node_for_surface(surface, &dh).or(*kms.primary_node.read().unwrap())
        })
        .ok()?;
    let sampled = match renderer {
        RendererRef::Glow(r) => sample(r, surface, size),
        RendererRef::GlMulti(mut r) => sample(&mut r, surface, size),
    };
    sampled
        .map_err(|err| tracing::debug!(?err, "first_frame: sample failed"))
        .ok()
}

fn sample<R>(renderer: &mut R, surface: &WlSurface, size: Size<i32, Logical>) -> anyhow::Result<bool>
where
    R: Renderer + ImportAll + Offscreen<GlesRenderbuffer> + ExportMem + AsGlowRenderer,
    R::TextureId: Clone + 'static,
    R::Error: Send + Sync + 'static,
{
    let edge: Size<i32, Physical> = Size::from((SAMPLE_EDGE, SAMPLE_EDGE));
    // Rendering at this scale is itself the downsample.
    let scale = Scale {
        x: f64::from(SAMPLE_EDGE) / f64::from(size.w),
        y: f64::from(SAMPLE_EDGE) / f64::from(size.h),
    };
    let elements = smithay::backend::renderer::element::surface::render_elements_from_surface_tree::<
        R,
        WaylandSurfaceRenderElement<R>,
    >(renderer, surface, Point::from((0, 0)), scale, 1.0, Kind::Unspecified);
    // No buffer yet is nothing drawn yet.
    if elements.is_empty() {
        return Ok(false);
    }

    let format = Fourcc::Abgr8888;
    let buffer_size = edge.to_logical(1).to_buffer(1, Transform::Normal);
    let mut buffer = Offscreen::<GlesRenderbuffer>::create_buffer(renderer, format, buffer_size)?;
    let mut fb = renderer.bind(&mut buffer)?;
    let mut damage = OutputDamageTracker::new(edge, 1.0, Transform::Normal);
    damage
        .render_output(renderer, &mut fb, 0, &elements, Color32F::BLACK)
        .map_err(|err| match err {
            smithay::backend::renderer::damage::Error::Rendering(err) => anyhow::anyhow!("{err:?}"),
            smithay::backend::renderer::damage::Error::OutputNoMode(_) => {
                anyhow::anyhow!("output has no mode")
            }
        })?;
    let mapping = renderer.copy_framebuffer(&fb, Rectangle::from_size(buffer_size), format)?;
    let pixels = renderer.map_texture(&mapping)?;
    Ok(drawn(pixels))
}

/// Whether any texel of an RGBA8 buffer is above black.
fn drawn(rgba: &[u8]) -> bool {
    rgba.chunks_exact(4).any(|px| px[..3].iter().any(|&c| c > DRAWN))
}

#[cfg(test)]
mod tests {
    use super::drawn;

    #[test]
    fn black_is_not_drawn_and_any_light_is() {
        assert!(!drawn(&[0, 0, 0, 255].repeat(256)));
        assert!(!drawn(&[4, 6, 8, 255].repeat(256)), "near-black noise");
        let mut one_lit = [0, 0, 0, 255].repeat(256);
        one_lit[4 * 37 + 1] = 40;
        assert!(drawn(&one_lit));
    }
}
