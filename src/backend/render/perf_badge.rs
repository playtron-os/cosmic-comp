// SPDX-License-Identifier: GPL-3.0-only

//! On-screen "capturing performance" badge.
//!
//! Shown in the top-left of each output while an F12 capture window is running
//! (`crate::perf::is_capturing()`). A small dark pill with a soft drop shadow and
//! a red record dot. Built from the existing filled-rounded-rect shader
//! ([`super::BackdropShader`]) — no text, so no font/iced machinery — and only
//! while capturing, so it costs nothing otherwise.

use smithay::{
    backend::renderer::{
        ImportAll, ImportMem, Renderer,
        element::{Kind, RenderElement},
        gles::{Uniform, element::PixelShaderElement},
    },
    utils::{Logical, Point, Rectangle, Size},
};

use crate::shell::CosmicMappedRenderElement;

use super::{
    BackdropShader,
    element::{AsGlowRenderer, CosmicElement},
};

/// Build the badge's render elements for one output, in front-to-back order
/// (element 0 is topmost). `output_geo` is the output geometry in logical coords.
pub fn elements<R>(renderer: &R, output_geo: Rectangle<i32, Logical>) -> Vec<CosmicElement<R>>
where
    R: AsGlowRenderer + Renderer + ImportAll + ImportMem,
    R::TextureId: 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    let shader = BackdropShader::get(renderer);

    // Top-left, with a margin that clears a typical top panel.
    let loc = output_geo.loc + Point::<i32, Logical>::from((16, 52));
    let pill_size = Size::<i32, Logical>::from((176, 34));
    let pill = Rectangle::<i32, Logical>::new(loc, pill_size);
    let pill_radius = (pill_size.h / 2) as f32;

    let fill = |geo: Rectangle<i32, Logical>, radius: f32, color: [f32; 3], alpha: f32| {
        PixelShaderElement::new(
            shader.clone(),
            geo,
            None,
            alpha,
            vec![
                Uniform::new(
                    "color",
                    [color[0] * alpha, color[1] * alpha, color[2] * alpha],
                ),
                Uniform::new("corner_radius_tl", radius),
                Uniform::new("corner_radius_tr", radius),
                Uniform::new("corner_radius_br", radius),
                Uniform::new("corner_radius_bl", radius),
            ],
            Kind::Unspecified,
        )
    };

    // Record dot: vertically centred near the left edge.
    let dot_d = 12;
    let dot = Rectangle::<i32, Logical>::new(
        loc + Point::from((14, (pill_size.h - dot_d) / 2)),
        Size::from((dot_d, dot_d)),
    );

    // Front (index 0) to back: dot, pill, then two faux-shadow layers (a larger,
    // more transparent black rounded rect offset downward approximates a soft
    // box-shadow without the window-keyed shadow shader).
    vec![
        CosmicElement::PerfBadge(fill(dot, (dot_d / 2) as f32, [0.93, 0.22, 0.22], 1.0)),
        CosmicElement::PerfBadge(fill(pill, pill_radius, [0.10, 0.11, 0.13], 0.92)),
        CosmicElement::PerfBadge(fill(
            grow(pill, 3, (0, 2)),
            pill_radius + 3.0,
            [0.0, 0.0, 0.0],
            0.16,
        )),
        CosmicElement::PerfBadge(fill(
            grow(pill, 7, (0, 5)),
            pill_radius + 7.0,
            [0.0, 0.0, 0.0],
            0.10,
        )),
    ]
}

/// Grow a rect by `by` on every side and offset it by `(dx, dy)`.
fn grow(r: Rectangle<i32, Logical>, by: i32, offset: (i32, i32)) -> Rectangle<i32, Logical> {
    Rectangle::new(
        r.loc + Point::from((offset.0 - by, offset.1 - by)),
        r.size + Size::from((by * 2, by * 2)),
    )
}
