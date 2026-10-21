//! The desktop wash: the active workspace's colour soft-lit over the wallpaper
//! (the design's `Desktop` wash, mirrored on the switcher's desktop previews).

use std::borrow::{Borrow, BorrowMut};

use iced_core::Color;
use smithay::backend::renderer::{
    ImportAll, Renderer,
    element::{Element, Id, Kind, RenderElement, UnderlyingStorage},
    gles::{GlesFrame, GlesRenderer, GlesTexProgram, Uniform},
    utils::{CommitCounter, DamageSet, OpaqueRegions},
};
use smithay::utils::{Buffer, Physical, Rectangle, Scale, Transform, user_data::UserDataMap};

use crate::backend::render::{element::AsGlowRenderer, thread_user_data};

pub static DESKTOP_WASH_SHADER: &str = include_str!("../shaders/desktop_wash.frag");

/// How much of the colour is blended in: `color-mix(…, transparent 90%)`.
pub const DESKTOP_WASH_AMOUNT: f32 = 0.1;

pub struct DesktopWashShader(pub GlesTexProgram);

impl DesktopWashShader {
    pub fn get<R: AsGlowRenderer>(renderer: &R) -> GlesTexProgram {
        thread_user_data(Borrow::<GlesRenderer>::borrow(renderer.glow_renderer()))
            .get::<DesktopWashShader>()
            .expect("Custom Shaders not initialized")
            .0
            .clone()
    }

    pub fn uniforms(color: Color) -> Vec<Uniform<'static>> {
        vec![
            Uniform::new("wash_color", [color.r, color.g, color.b]),
            Uniform::new("wash_amount", DESKTOP_WASH_AMOUNT),
        ]
    }
}

/// CSS `soft-light` for one channel: the shader's formula, for tests to check it against.
pub fn soft_light(backdrop: f32, source: f32) -> f32 {
    if source <= 0.5 {
        backdrop - (1.0 - 2.0 * source) * backdrop * (1.0 - backdrop)
    } else {
        let d = if backdrop <= 0.25 {
            ((16.0 * backdrop - 12.0) * backdrop + 4.0) * backdrop
        } else {
            backdrop.sqrt()
        };
        backdrop + (2.0 * source - 1.0) * (d - backdrop)
    }
}

/// One wallpaper channel under the wash.
pub fn washed(backdrop: f32, source: f32) -> f32 {
    backdrop + (soft_light(backdrop, source) - backdrop) * DESKTOP_WASH_AMOUNT
}

/// The colour rides in the commit's top bits, so recolouring a wallpaper that did
/// not commit still repaints it, on screen and in every preview's own tracker.
const KEY_BITS: u32 = 16;
const KEY_SHIFT: u32 = usize::BITS - KEY_BITS;
const COUNT_MASK: usize = (1 << KEY_SHIFT) - 1;

/// RGB565: colours one key apart differ by under a 32nd, invisible at a tenth's wash.
fn color_key(color: Color) -> usize {
    let [r, g, b, _] = color.into_rgba8();
    (usize::from(r >> 3) << 11) | (usize::from(g >> 2) << 5) | usize::from(b >> 3)
}

fn commit_value(commit: CommitCounter) -> usize {
    commit
        .distance(Some(CommitCounter::from(0)))
        .unwrap_or_default()
}

fn keyed_commit(inner: CommitCounter, key: usize) -> CommitCounter {
    CommitCounter::from((key << KEY_SHIFT) | (commit_value(inner) & COUNT_MASK))
}

/// The surface's own commit under `commit`, or `None` when it was drawn in another colour.
fn inner_commit(commit: CommitCounter, key: usize) -> Option<CommitCounter> {
    let value = commit_value(commit);
    (value >> KEY_SHIFT == key).then(|| CommitCounter::from(value & COUNT_MASK))
}

#[derive(Debug)]
pub struct WashedSurfaceRenderElement<E> {
    inner: E,
    program: GlesTexProgram,
    uniforms: Vec<Uniform<'static>>,
    key: usize,
}

impl<E: Element> WashedSurfaceRenderElement<E> {
    pub fn new(inner: E, program: GlesTexProgram, color: Color) -> Self {
        Self {
            inner,
            program,
            uniforms: DesktopWashShader::uniforms(color),
            key: color_key(color),
        }
    }
}

impl<E: Element> Element for WashedSurfaceRenderElement<E> {
    fn id(&self) -> &Id {
        self.inner.id()
    }

    fn current_commit(&self) -> CommitCounter {
        keyed_commit(self.inner.current_commit(), self.key)
    }

    fn geometry(&self, scale: Scale<f64>) -> Rectangle<i32, Physical> {
        self.inner.geometry(scale)
    }

    fn src(&self) -> Rectangle<f64, Buffer> {
        self.inner.src()
    }

    fn transform(&self) -> Transform {
        self.inner.transform()
    }

    fn damage_since(
        &self,
        scale: Scale<f64>,
        commit: Option<CommitCounter>,
    ) -> DamageSet<i32, Physical> {
        match commit.map(|commit| inner_commit(commit, self.key)) {
            Some(None) => DamageSet::from_slice(&[Rectangle::from_size(self.geometry(scale).size)]),
            Some(inner) => self.inner.damage_since(scale, inner),
            None => self.inner.damage_since(scale, None),
        }
    }

    fn opaque_regions(&self, scale: Scale<f64>) -> OpaqueRegions<i32, Physical> {
        self.inner.opaque_regions(scale)
    }

    fn alpha(&self) -> f32 {
        self.inner.alpha()
    }

    fn kind(&self) -> Kind {
        self.inner.kind()
    }
}

impl<R, E> RenderElement<R> for WashedSurfaceRenderElement<E>
where
    R: AsGlowRenderer + Renderer + ImportAll,
    R::TextureId: 'static,
    E: RenderElement<R>,
{
    fn draw(
        &self,
        frame: &mut R::Frame<'_, '_>,
        src: Rectangle<f64, Buffer>,
        dst: Rectangle<i32, Physical>,
        damage: &[Rectangle<i32, Physical>],
        opaque_regions: &[Rectangle<i32, Physical>],
        cache: Option<&UserDataMap>,
    ) -> Result<(), R::Error> {
        BorrowMut::<GlesFrame>::borrow_mut(<R as AsGlowRenderer>::glow_frame_mut(frame))
            .override_default_tex_program(self.program.clone(), self.uniforms.clone());
        let drawn = self
            .inner
            .draw(frame, src, dst, damage, opaque_regions, cache);
        BorrowMut::<GlesFrame>::borrow_mut(<R as AsGlowRenderer>::glow_frame_mut(frame))
            .clear_tex_program_override();
        drawn
    }

    // Composited, never scanned out: a plane would show the wallpaper unwashed.
    fn underlying_storage(&self, _renderer: &mut R) -> Option<UnderlyingStorage<'_>> {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn soft_light_keeps_black_and_white_and_leans_mid_tones_to_the_colour() {
        for source in [0.0, 0.3, 0.5, 0.8, 1.0] {
            assert_eq!(soft_light(0.0, source), 0.0);
            assert!((soft_light(1.0, source) - 1.0).abs() < 1e-6);
        }
        assert!((soft_light(0.5, 0.5) - 0.5).abs() < 1e-6);
        assert!(soft_light(0.5, 1.0) > 0.5);
        assert!(soft_light(0.5, 0.0) < 0.5);
        // The CSS formula's two branches meet at a quarter.
        let below = ((16.0 * 0.25 - 12.0) * 0.25 + 4.0) * 0.25;
        assert!((below - 0.25f32.sqrt()).abs() < 1e-6);
    }

    #[test]
    fn the_wash_moves_a_wallpaper_a_tenth_of_the_way_to_its_soft_light() {
        let full = soft_light(0.4, 0.9);
        assert!((washed(0.4, 0.9) - (0.4 + (full - 0.4) * 0.1)).abs() < 1e-6);
        assert_eq!(washed(0.0, 0.9), 0.0);
    }

    #[test]
    fn a_new_colour_is_a_new_commit_and_damages_the_whole_wallpaper() {
        let crimson = Color::from_rgb8(0xe9, 0x7c, 0x86);
        let jade = Color::from_rgb8(0x4f, 0xb8, 0x8c);
        let (a, b) = (color_key(crimson), color_key(jade));
        assert_ne!(a, b);
        let surface = CommitCounter::from(41);
        let drawn = keyed_commit(surface, a);
        assert_eq!(inner_commit(drawn, a), Some(surface));
        assert_eq!(inner_commit(drawn, b), None);
        assert_ne!(keyed_commit(surface, b), drawn);
        // A wallpaper commit under one colour still reads as that surface's next commit.
        let next = keyed_commit(CommitCounter::from(42), a);
        assert_eq!(next.distance(Some(drawn)), Some(1));
    }

    #[test]
    fn every_workspace_colour_has_its_own_key() {
        // The design's seven workspace hues (`--color-ws-*`).
        let hues = [
            0xe97c86, 0xe3874d, 0x92af47, 0x45bb82, 0x00b9b4, 0x00b4d7, 0xd87ebd,
        ];
        let mut keys: Vec<usize> = hues
            .iter()
            .map(|hex: &u32| {
                let [_, r, g, b] = hex.to_be_bytes();
                color_key(Color::from_rgb8(r, g, b))
            })
            .collect();
        keys.sort_unstable();
        keys.dedup();
        assert_eq!(keys.len(), hues.len());
    }
}
