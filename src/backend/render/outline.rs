//! Subpixel outline geometry inside Smithay's integer-logical shader canvas.

use crate::utils::prelude::*;
use smithay::utils::{Physical, Rectangle};

#[derive(Clone, Copy, Debug, PartialEq)]
pub(super) struct Geometry {
    pub canvas: Rectangle<i32, Local>,
    pub draw_size: [f32; 2],
    pub shape_origin: [f32; 2],
    pub shape_size: [f32; 2],
}

impl Geometry {
    pub fn new(shape: Rectangle<f64, Local>, ring_width: u8, scale: f64) -> Self {
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
