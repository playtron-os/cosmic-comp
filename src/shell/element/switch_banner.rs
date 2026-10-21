//! What a workspace switch shows besides its own transition: the destination's
//! mark and name in a band at the top, and its colour rising from the edge the
//! switch came from (the design's `WorkspaceSwitchBanner` and `WorkspaceSwitchWash`).
//! Both ride the switch's duration, so neither can outlive it.

use std::time::{Duration, Instant};

use calloop::LoopHandle;
use iced_core::{Alignment, Color, Length};
use iced_widget::{container, row};
use icetron_p::icetron_themes::{Icon, TextRole};
use icetron_p::prelude::{EntityMark, EntityMarkSize, styled_text, workspace_icon};
use smithay::{
    backend::renderer::ImportMem,
    desktop::space::SpaceElement,
    output::Output,
    utils::{Logical, Physical, Point, Rectangle, Scale, Size},
};

use crate::{
    backend::render::{animations::motion::Motion, element::AsGlowRenderer},
    comp_theme::CompTheme,
    utils::{
        apply::Apply,
        iced::{CompElement, IcedElement, IcedRenderElement, Program},
    },
};

/// The band mirrors the taskbar pill off the opposite edge: the pill's height
/// (the design's `TASKBAR_HEIGHT`; there is no token), at its margin.
const BAND_HEIGHT: f32 = 40.0;

/// How far the wash reaches from its edge: `transparent 55%`.
pub const WASH_REACH: f32 = 0.55;

/// Opacity keyframes as (time, value), each segment eased on its own.
const BANNER_KEYS: [(f32, f32); 4] = [(0.0, 0.0), (0.25, 1.0), (0.7, 1.0), (1.0, 0.0)];
/// Peaks at 0.14: the banner announces, the wash only colours the arrival.
const WASH_KEYS: [(f32, f32); 3] = [(0.0, 0.0), (0.35, 0.14), (1.0, 0.0)];

fn keyframes(t: f32, keys: &[(f32, f32)], ease: impl Fn(f32) -> f32) -> f32 {
    let t = if t.is_finite() {
        t.clamp(0.0, 1.0)
    } else {
        1.0
    };
    for pair in keys.windows(2) {
        let ((start, from), (end, to)) = (pair[0], pair[1]);
        if t <= end {
            let span = end - start;
            let local = if span > 0.0 { (t - start) / span } else { 1.0 };
            return from + (to - from) * ease(local);
        }
    }
    keys.last().map_or(0.0, |(_, value)| *value)
}

pub fn banner_opacity(t: f32, motion: &Motion) -> f32 {
    keyframes(t, &BANNER_KEYS, |t| motion.ease_out_expo(t))
}

pub fn wash_opacity(t: f32, motion: &Motion) -> f32 {
    keyframes(t, &WASH_KEYS, |t| motion.ease_out_expo(t))
}

/// A workspace switch's banner and wash, for as long as the switch plays.
#[derive(Debug, Clone)]
pub struct SwitchAnnouncement {
    started: Instant,
    duration: Duration,
    /// Into a later workspace, so the colour rises from the bottom edge.
    pub forward: bool,
    /// The colour of the workspace arrived in, which the chrome now wears.
    pub wash: Color,
    banner: IcedElement<SwitchBanner>,
    size: Size<i32, Logical>,
}

impl SwitchAnnouncement {
    /// For a switch that began at `started` and plays for `duration`.
    pub fn new(
        banner: IcedElement<SwitchBanner>,
        wash: Color,
        forward: bool,
        started: Instant,
        duration: Duration,
    ) -> Self {
        let size = banner.minimum_size();
        banner.resize(size);
        Self {
            started,
            duration,
            forward,
            wash,
            banner,
            size,
        }
    }

    /// How far through the switch, or `None` once it is over.
    pub fn progress(&self) -> Option<f32> {
        let elapsed = self.started.elapsed();
        (elapsed < self.duration).then(|| elapsed.as_secs_f32() / self.duration.as_secs_f32())
    }

    pub fn output_enter(&self, output: &Output) {
        self.banner.output_enter(output, Rectangle::default());
    }

    pub fn set_theme(&mut self, theme: CompTheme) {
        self.banner.set_theme(theme);
        self.size = self.banner.minimum_size();
        self.banner.resize(self.size);
    }

    /// The band centred across `output_size`, below its top edge by the pill's margin.
    pub fn banner_location(
        &self,
        output_size: (i32, i32),
        theme: &CompTheme,
    ) -> Point<i32, Logical> {
        let band = BAND_HEIGHT.round() as i32;
        Point::from((
            (output_size.0 - self.size.w) / 2,
            theme.spacing_3().round() as i32 + (band - self.size.h) / 2,
        ))
    }

    pub fn push_banner_elements<R>(
        &self,
        renderer: &mut R,
        location: Point<i32, Physical>,
        scale: Scale<f64>,
        alpha: f32,
        push: &mut dyn FnMut(IcedRenderElement<R>),
    ) where
        R: AsGlowRenderer + ImportMem,
        R::TextureId: Send + Clone + 'static,
    {
        self.banner
            .push_render_elements(renderer, location, scale, alpha, [0; 4], push, None);
    }
}

/// The destination's mark, at the size where its glyph can be learnt, captioned by its name.
#[derive(Debug, Clone)]
pub struct SwitchBanner {
    name: String,
    color: Option<Color>,
    icon: Icon,
}

impl SwitchBanner {
    /// `color` is the workspace's own, as its mark is drawn everywhere else.
    pub fn new(id: &str, name: String, color: Option<Color>, icon: Option<&str>) -> Self {
        Self {
            name,
            color,
            icon: workspace_icon(id, icon).icon,
        }
    }

    /// Drawn on every output, each at its own scale.
    pub fn into_element(
        self,
        outputs: impl Iterator<Item = Output>,
        evlh: LoopHandle<'static, crate::state::State>,
        theme: CompTheme,
    ) -> IcedElement<Self> {
        let element = IcedElement::new(self, (1, 1), evlh, theme);
        for output in outputs {
            element.output_enter(&output, Rectangle::default());
        }
        element
    }
}

impl Program for SwitchBanner {
    type Message = ();

    fn program_name() -> &'static str {
        "SwitchBanner"
    }

    fn view<'a>(&'a self, theme: &'a CompTheme) -> CompElement<'a, Self::Message> {
        row![
            EntityMark::workspace(self.color, Some(self.icon))
                .size(EntityMarkSize::S32)
                .view(&**theme),
            styled_text(
                &self.name,
                theme.text_styles().role(TextRole::Label),
                theme.text_primary()
            ),
        ]
        .spacing(theme.spacing_2())
        .align_y(Alignment::Center)
        .apply(container)
        .height(Length::Fixed(BAND_HEIGHT))
        .align_y(Alignment::Center)
        .into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use icetron_p::icetron_themes::dynamic::DEFAULT_THEME_PAIR;

    fn motion() -> Motion {
        Motion::from_theme(&DEFAULT_THEME_PAIR.load(true))
    }

    #[test]
    fn the_banner_arrives_holds_and_leaves_inside_the_switch() {
        let motion = motion();
        assert_eq!(banner_opacity(0.0, &motion), 0.0);
        assert!((banner_opacity(0.25, &motion) - 1.0).abs() < 1e-4);
        assert!((banner_opacity(0.5, &motion) - 1.0).abs() < 1e-4);
        assert!((banner_opacity(0.7, &motion) - 1.0).abs() < 1e-4);
        assert!(banner_opacity(1.0, &motion).abs() < 1e-4);
        // Ease-out: most of the arrival happens early in its segment.
        assert!(banner_opacity(0.125, &motion) > 0.75);
        assert!(banner_opacity(f32::NAN, &motion).abs() < 1e-4);
    }

    #[test]
    fn the_wash_blooms_to_a_seventh_and_recedes_by_the_end() {
        let motion = motion();
        assert_eq!(wash_opacity(0.0, &motion), 0.0);
        assert!((wash_opacity(0.35, &motion) - 0.14).abs() < 1e-4);
        assert!(wash_opacity(1.0, &motion).abs() < 1e-4);
        let peak = (0..=100)
            .map(|i| wash_opacity(i as f32 / 100.0, &motion))
            .fold(0.0, f32::max);
        assert!(peak <= 0.14 + 1e-4);
    }

    /// The banner drawn by tiny-skia at `scale`, BGRA, with its logical size.
    fn raster(banner: &SwitchBanner, dark: bool, scale: f32) -> (tiny_skia::Pixmap, (f32, f32)) {
        use iced_core::{Font, Pixels, Size, mouse, renderer::Style};
        use iced_graphics::Viewport;
        use iced_runtime::{UserInterface, user_interface};
        use std::sync::Arc;
        static FONTS: std::sync::Once = std::sync::Once::new();
        FONTS.call_once(|| {
            let mut fonts = iced_graphics::text::font_system().write().unwrap();
            for data in icetron_p::icetron_themes::fonts::ALL {
                fonts.load_font(std::borrow::Cow::Borrowed(*data));
            }
        });
        let theme = CompTheme::new(Arc::new(DEFAULT_THEME_PAIR.load(dark)), dark);
        let mut renderer = iced_tiny_skia::Renderer::new(Font::DEFAULT, Pixels(16.0));
        let size = {
            let mut element = banner.view(&theme);
            let tree = &mut iced_core::widget::Tree::new(element.as_widget());
            let node = element.as_widget_mut().layout(
                tree,
                &renderer,
                &iced_core::layout::Limits::new(Size::ZERO, Size::INFINITE)
                    .width(Length::Shrink)
                    .height(Length::Shrink),
            );
            node.size()
        };
        let mut ui = UserInterface::build(
            banner.view(&theme),
            size,
            user_interface::Cache::default(),
            &mut renderer,
        );
        ui.draw(
            &mut renderer,
            &iced_core::Theme::Dark,
            &Style::default(),
            mouse::Cursor::Unavailable,
        );
        let viewport = Viewport::with_physical_size(
            Size::new(
                (size.width * scale).round() as u32,
                (size.height * scale).round() as u32,
            ),
            scale,
        );
        let mut pixels =
            tiny_skia::Pixmap::new(viewport.physical_width(), viewport.physical_height()).unwrap();
        let mut mask = tiny_skia::Mask::new(pixels.width(), pixels.height()).unwrap();
        renderer.draw(
            &mut pixels.as_mut(),
            &mut mask,
            &viewport,
            &[iced_core::Rectangle::with_size(size)],
            Color::TRANSPARENT,
        );
        if let Some(dir) = std::env::var_os("SWITCH_BANNER_SNAPSHOT_DIR") {
            let mut png = pixels.clone();
            for pixel in png.data_mut().chunks_exact_mut(4) {
                pixel.swap(0, 2);
            }
            let dir = std::path::PathBuf::from(dir);
            std::fs::create_dir_all(&dir).unwrap();
            let mode = if dark { "dark" } else { "light" };
            png.save_png(dir.join(format!("banner-{mode}-{scale}x.png")))
                .unwrap();
        }
        (pixels, (size.width, size.height))
    }

    #[test]
    fn the_band_holds_the_workspace_mark_then_its_name_in_the_chrome_text_colour() {
        let crimson = Color::from_rgb8(0xe9, 0x7c, 0x86);
        let banner = SwitchBanner::new("ws-a", "Driftlands".into(), Some(crimson), Some("swords"));
        for dark in [true, false] {
            for scale in [1.0, 1.5] {
                let (pixels, (width, height)) = raster(&banner, dark, scale);
                assert_eq!(height, BAND_HEIGHT, "the band is the pill's height");
                assert!(width > 32.0 + 8.0, "the caption follows the mark");
                let at = |x: f32, y: f32| {
                    let p = pixels
                        .pixel((x * scale) as u32, (y * scale) as u32)
                        .unwrap()
                        .demultiply();
                    // BGRA, as the compositor uploads it.
                    [p.blue(), p.green(), p.red(), p.alpha()]
                };
                // The mark's ring is the workspace colour at 28% over nothing.
                let ring = at(16.0, (BAND_HEIGHT - 32.0) / 2.0 + 0.6);
                let [r, g, b, _] = crimson.into_rgba8();
                assert!(
                    ring[3] > 20
                        && ring[0].abs_diff(r) < 12
                        && ring[1].abs_diff(g) < 12
                        && ring[2].abs_diff(b) < 12,
                    "ring {ring:?} in {} mode at {scale}x",
                    if dark { "dark" } else { "light" }
                );
                // Something is written after the mark and the gap.
                let inked = (0..(height * scale) as u32).any(|y| {
                    ((48.0 * scale) as u32..(width * scale) as u32)
                        .any(|x| pixels.pixel(x, y).unwrap().alpha() > 128)
                });
                assert!(inked, "the name is drawn");
            }
        }
    }
}
