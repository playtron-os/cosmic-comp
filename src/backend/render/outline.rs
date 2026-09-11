//! Subpixel outline geometry inside Smithay's integer-logical shader canvas.

use crate::utils::prelude::*;
use smithay::utils::{Physical, Rectangle};
use smithay::{
    backend::renderer::{
        element::{Element, Id, Kind, RenderElement},
        gles::element::PixelShaderElement,
        glow::GlowRenderer,
        utils::{CommitCounter, DamageSet, OpaqueRegions},
    },
    utils::{Buffer, Scale, Transform, user_data::UserDataMap},
};

/// Adapt a GLES outline to either the single-GPU or multi-GPU renderer.
#[derive(Debug)]
pub struct OutlineElement(pub PixelShaderElement);

impl Element for OutlineElement {
    fn id(&self) -> &Id {
        self.0.id()
    }
    fn current_commit(&self) -> CommitCounter {
        self.0.current_commit()
    }
    fn src(&self) -> Rectangle<f64, Buffer> {
        self.0.src()
    }
    fn geometry(&self, scale: Scale<f64>) -> Rectangle<i32, Physical> {
        self.0.geometry(scale)
    }
    fn transform(&self) -> Transform {
        self.0.transform()
    }
    fn damage_since(
        &self,
        scale: Scale<f64>,
        commit: Option<CommitCounter>,
    ) -> DamageSet<i32, Physical> {
        self.0.damage_since(scale, commit)
    }
    fn opaque_regions(&self, scale: Scale<f64>) -> OpaqueRegions<i32, Physical> {
        self.0.opaque_regions(scale)
    }
    fn alpha(&self) -> f32 {
        self.0.alpha()
    }
    fn kind(&self) -> Kind {
        self.0.kind()
    }
}

impl<R: super::element::AsGlowRenderer> RenderElement<R> for OutlineElement {
    fn draw(
        &self,
        frame: &mut R::Frame<'_, '_>,
        src: Rectangle<f64, Buffer>,
        dst: Rectangle<i32, Physical>,
        damage: &[Rectangle<i32, Physical>],
        opaque: &[Rectangle<i32, Physical>],
        cache: Option<&UserDataMap>,
    ) -> Result<(), R::Error> {
        RenderElement::<GlowRenderer>::draw(
            &self.0,
            R::glow_frame_mut(frame),
            src,
            dst,
            damage,
            opaque,
            cache,
        )
        .map_err(R::from_gles_error)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(super) struct Geometry {
    pub canvas: Rectangle<i32, Local>,
    pub draw_size: [f32; 2],
    pub shape_origin: [f32; 2],
    pub shape_size: [f32; 2],
}

impl Geometry {
    pub fn new(shape: Rectangle<f64, Local>, ring_width: f32, scale: f64) -> Self {
        // Keep a physical pixel outside the ring for coverage antialiasing.
        let padding = f64::from(ring_width) + 1.0 / scale;
        let left = (shape.loc.x - padding).floor() as i32;
        let top = (shape.loc.y - padding).floor() as i32;
        let right = (shape.loc.x + shape.size.w + padding).ceil() as i32;
        let bottom = (shape.loc.y + shape.size.h + padding).ceil() as i32;
        let canvas: Rectangle<i32, Local> =
            Rectangle::new((left, top).into(), (right - left, bottom - top).into());
        // PixelShaderElement rounds its destination. Map UVs back from that
        // actual rectangle, not from canvas.size * scale (which may differ).
        let physical: Rectangle<i32, Physical> =
            canvas.as_logical().to_physical_precise_round(scale);
        Self {
            canvas,
            draw_size: [
                (f64::from(physical.size.w) / scale) as f32,
                (f64::from(physical.size.h) / scale) as f32,
            ],
            shape_origin: [
                (shape.loc.x - f64::from(physical.loc.x) / scale) as f32,
                (shape.loc.y - f64::from(physical.loc.y) / scale) as f32,
            ],
            shape_size: [shape.size.w as f32, shape.size.h as f32],
        }
    }
}

#[cfg(test)]
mod tests;
