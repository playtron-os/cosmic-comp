//! Separate tooltip surfaces: no clipping to SSD bounds, and a real client backdrop.

use super::*;
use iced_core::{Padding, Rectangle as IcedRectangle, widget::Operation};
use icetron_p::{
    components::shadow::with_elevation_shadow,
    prelude::{TooltipReport, glass_tooltip},
};

#[derive(Default)]
struct Collector(Option<TooltipReport>);

impl Operation for Collector {
    fn traverse(&mut self, operate: &mut dyn FnMut(&mut dyn Operation)) {
        operate(self);
    }
    fn custom(&mut self, _: Option<&iced_core::widget::Id>, _: IcedRectangle, state: &mut dyn Any) {
        if let Some(report) = state.downcast_ref::<TooltipReport>() {
            self.0 = Some(report.clone());
        }
    }
}

pub(super) fn collect<Message>(
    interface: &mut UserInterface<'_, Message, iced_core::Theme, iced_tiny_skia::Renderer>,
    renderer: &iced_tiny_skia::Renderer,
) -> Option<TooltipReport> {
    let mut collector = Collector::default();
    interface.operate(renderer, &mut collector);
    collector.0
}

struct Raster {
    buffer: MemoryRenderBuffer,
    padding: Padding,
    logical_size: IcedSize,
    physical_size: Size<i32, BufferCoords>,
}

#[derive(Default)]
pub(super) struct Surface {
    /// Current widget request, not the lifetime/opacity of the painted chip.
    pub report: Option<TooltipReport>,
    active: Option<Tip>,
    closing: Vec<Tip>,
}

struct Tip {
    report: TooltipReport,
    settings: Visibility,
    animation: VisibilityAnimation,
    frame: VisibilityFrame,
    content: Option<(String, IcedSize)>,
    rasters: HashMap<OrderedFloat<f64>, Raster>,
    blur: BlurState,
}

fn shadow_padding(theme: &CompTheme) -> Padding {
    let mut padding = Padding::ZERO;
    for shadow in theme
        .shadow_popover_deep()
        .iter()
        .filter(|s| !s.inset && s.color.a > 0.0)
    {
        let reach = shadow.blur_radius.max(0.0) + shadow.spread_radius.max(0.0);
        padding.top = padding.top.max((reach - shadow.offset.y).ceil());
        padding.bottom = padding.bottom.max((reach + shadow.offset.y).ceil());
        padding.left = padding.left.max((reach - shadow.offset.x).ceil());
        padding.right = padding.right.max((reach + shadow.offset.x).ceil());
    }
    padding
}

fn rasterize(label: &str, body: IcedSize, theme: &CompTheme, scale: f64) -> Option<Raster> {
    let padding = shadow_padding(theme);
    let logical_size = IcedSize::new(
        (body.width + padding.left + padding.right).ceil(),
        (body.height + padding.top + padding.bottom).ceil(),
    );
    let physical_size: Size<i32, BufferCoords> =
        Size::<f64, Logical>::from((logical_size.width as f64, logical_size.height as f64))
            .to_buffer(scale, Transform::Normal)
            .to_i32_round();
    if physical_size.w <= 0 || physical_size.h <= 0 {
        return None;
    }
    let chip = glass_tooltip(label, &**theme)
        .width(body.width)
        .height(body.height);
    let content: CompElement<'_, ()> = iced_widget::container(with_elevation_shadow(
        chip,
        &theme.shadow_popover_deep(),
        Some(theme.radii_sm()),
    ))
    .padding(padding)
    .into();
    let mut renderer = iced_tiny_skia::Renderer::new(
        Font::DEFAULT,
        Pixels(theme.text_styles().caption().font_size),
    );
    let mut interface = UserInterface::build(
        content,
        logical_size,
        user_interface::Cache::default(),
        &mut renderer,
    );
    interface.draw(
        &mut renderer,
        &theme.to_iced_theme(),
        &RendererStyle {
            text_color: theme.text_primary(),
        },
        Cursor::Unavailable,
    );
    let viewport = Viewport::with_physical_size(
        IcedSize::new(physical_size.w as u32, physical_size.h as u32),
        scale as f32,
    );
    let mut mask = tiny_skia::Mask::new(physical_size.w as u32, physical_size.h as u32)?;
    let mut buffer =
        MemoryRenderBuffer::new(Fourcc::Argb8888, physical_size, 1, Transform::Normal, None);
    buffer
        .render()
        .draw(|bytes| {
            let mut pixels = tiny_skia::PixmapMut::from_bytes(
                bytes,
                physical_size.w as u32,
                physical_size.h as u32,
            )
            .ok_or(())?;
            renderer.draw(
                &mut pixels,
                &mut mask,
                &viewport,
                &[IcedRectangle::with_size(logical_size)],
                Color::TRANSPARENT,
            );
            Ok::<_, ()>(vec![Rectangle::from_size(physical_size)])
        })
        .ok()?;
    Some(Raster {
        buffer,
        padding,
        logical_size,
        physical_size,
    })
}

impl Surface {
    pub fn invalidate(&mut self) {
        for tip in self.active.iter_mut().chain(&mut self.closing) {
            tip.invalidate();
        }
    }

    /// Keep outgoing labels/rasters separate from a replacement tooltip, just
    /// like agentos-panel's closing tooltip surfaces. No widget fade is used.
    pub fn sync(&mut self, theme: &CompTheme, now: IcedInstant) {
        if self.active.as_ref().is_some_and(|tip| {
            self.report
                .as_ref()
                .is_none_or(|report| report.label != tip.report.label)
        }) {
            let mut tip = self.active.take().unwrap();
            tip.animation.update(
                Visibility {
                    visible: false,
                    ..tip.settings
                },
                now,
            );
            if !tip.animation.is_fully_hidden(now) {
                self.closing.push(tip);
            }
        }
        if let Some(report) = &self.report {
            if let Some(tip) = &mut self.active {
                tip.report = report.clone();
            } else {
                let settings = Visibility::fade(theme.motion);
                let animation = VisibilityAnimation::new(settings);
                let frame = animation.frame(now);
                self.active = Some(Tip {
                    report: report.clone(),
                    settings,
                    animation,
                    frame,
                    content: None,
                    rasters: HashMap::new(),
                    blur: BlurState::default(),
                });
            }
        }
        self.closing
            .retain(|tip| !tip.animation.is_fully_hidden(now));
    }

    /// Sample at compositor draw time, so a slow initial layout cannot consume
    /// the fade offscreen. Tooltip-only frames need no header widget rebuild.
    pub fn advance(&mut self, now: IcedInstant) {
        if let Some(tip) = &mut self.active {
            tip.animation.start_on_draw(now);
        }
        for tip in self.active.iter_mut().chain(&mut self.closing) {
            tip.frame = tip.animation.frame(now);
        }
        self.closing
            .retain(|tip| !tip.animation.is_fully_hidden(now));
    }

    pub fn is_animating(&self, now: IcedInstant) -> bool {
        self.active
            .iter()
            .chain(&self.closing)
            .any(|tip| tip.animation.is_animating(now))
    }

    #[cfg(test)]
    pub(super) fn snapshots(&self) -> Vec<TooltipReport> {
        self.active
            .iter()
            .chain(self.closing.iter().rev())
            .map(|tip| TooltipReport {
                opacity: tip.frame.opacity,
                ..tip.report.clone()
            })
            .collect()
    }

    pub fn push<R>(
        &mut self,
        renderer: &mut R,
        theme: &CompTheme,
        origin: Point<f64, Physical>,
        scale: Scale<f64>,
        additional_scale: f64,
        parent_alpha: f32,
        push: &mut dyn FnMut(IcedRenderElement<R>),
    ) where
        R: AsGlowRenderer + ImportMem,
        R::TextureId: Send + Clone + 'static,
    {
        for tip in self.active.iter_mut().chain(self.closing.iter_mut().rev()) {
            tip.push(
                renderer,
                theme,
                origin,
                scale,
                additional_scale,
                parent_alpha,
                push,
            );
        }
    }
}

impl Tip {
    fn invalidate(&mut self) {
        self.content = None;
        self.rasters.clear();
    }

    pub fn push<R>(
        &mut self,
        renderer: &mut R,
        theme: &CompTheme,
        origin: Point<f64, Physical>,
        scale: Scale<f64>,
        additional_scale: f64,
        parent_alpha: f32,
        push: &mut dyn FnMut(IcedRenderElement<R>),
    ) where
        R: AsGlowRenderer + ImportMem,
        R::TextureId: Send + Clone + 'static,
    {
        let alpha = parent_alpha * self.frame.opacity;
        if alpha <= 0.0 {
            return;
        }
        let report = &self.report;
        let body = report.bounds;
        if self
            .content
            .as_ref()
            .is_none_or(|(label, size)| label != &report.label || *size != body.size())
        {
            self.content = Some((report.label.clone(), body.size()));
            self.rasters.clear();
        }
        let raster = match self.rasters.entry(OrderedFloat(scale.x)) {
            std::collections::hash_map::Entry::Occupied(entry) => entry.into_mut(),
            std::collections::hash_map::Entry::Vacant(entry) => {
                let Some(raster) = rasterize(&report.label, body.size(), theme, scale.x) else {
                    return;
                };
                entry.insert(raster)
            }
        };
        let location = origin
            + Point::<f64, Logical>::from((
                (body.x - raster.padding.left) as f64,
                (body.y - raster.padding.top) as f64,
            ))
            .to_physical(scale);
        let size = Size::<f64, Logical>::from((
            raster.logical_size.width as f64,
            raster.logical_size.height as f64,
        ))
        .upscale(additional_scale)
        .to_i32_round();
        match MemoryRenderBufferRenderElement::from_buffer(
            renderer,
            location,
            &raster.buffer,
            Some(alpha),
            Some(VisibilityFrame::texture_source(
                location,
                raster.physical_size,
            )),
            Some(size),
            Kind::Unspecified,
        ) {
            Ok(element) => push(element.into()),
            Err(error) => {
                tracing::warn!(?error, "Tooltip texture import failed");
                return;
            }
        }
        let local = Rectangle::<f64, Logical>::new(
            (body.x as f64, body.y as f64).into(),
            (body.width as f64, body.height as f64).into(),
        )
        .upscale(additional_scale);
        let element_origin = origin.to_logical(scale).upscale(additional_scale);
        let radius = (theme.radii_sm() as f64 * additional_scale).round() as u8;
        match BlurElement::from_state_with_appearance(
            renderer,
            &mut self.blur,
            Rectangle::new(element_origin + local.loc, local.size.to_i32_round()),
            scale.x,
            [radius; 4],
            configured_blur_strength(true),
            alpha,
            [theme.backdrop_saturate_popover(), 0.0, 0.0],
        ) {
            Ok(Some(element)) => push(element.into()),
            Ok(None) => {}
            Err(error) => tracing::warn!(?error, "Tooltip backdrop failed"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn report(label: &str, x: f32) -> TooltipReport {
        TooltipReport {
            label: label.into(),
            bounds: IcedRectangle {
                x,
                y: 60.0,
                width: 120.0,
                height: 24.0,
            },
            opacity: 1.0,
        }
    }

    #[test]
    fn tooltip_fades_from_first_draw_and_retains_its_label_on_exit() {
        let theme = CompTheme::default();
        let duration = theme.motion.layer_open;
        let now = IcedInstant::now();
        let mut surface = Surface {
            report: Some(report("Close", 20.0)),
            ..Surface::default()
        };
        surface.sync(&theme, now);
        let first_draw = now + std::time::Duration::from_secs(2);
        surface.advance(first_draw);
        assert_eq!(surface.snapshots()[0].opacity, 0.0);
        surface.advance(first_draw + duration / 2);
        let middle = surface.snapshots()[0].clone();
        assert!((middle.opacity - 0.5).abs() < 0.00001);
        assert_eq!(
            middle.bounds,
            report("Close", 20.0).bounds,
            "fade only, like panel tooltips"
        );
        surface.report = None;
        surface.sync(&theme, first_draw + duration / 2);
        surface.advance(first_draw + duration / 2);
        assert!(surface.active.is_none());
        assert_eq!(surface.snapshots()[0].label, "Close");
        assert!((surface.snapshots()[0].opacity - middle.opacity).abs() < 0.00001);
        surface.advance(first_draw + duration + std::time::Duration::from_millis(1));
        assert!(surface.snapshots().is_empty());
        assert!(!surface.is_animating(first_draw + duration * 2));
    }

    #[test]
    fn replacing_tooltips_keeps_old_and_new_labels_separate() {
        let theme = CompTheme::default();
        let duration = theme.motion.layer_open;
        let now = IcedInstant::now();
        let mut surface = Surface {
            report: Some(report("Close", 20.0)),
            ..Surface::default()
        };
        surface.sync(&theme, now);
        surface.advance(now);
        surface.advance(now + duration);
        surface.report = Some(report("Minimize", 80.0));
        surface.sync(&theme, now + duration);
        surface.advance(now + duration);
        let frames = surface.snapshots();
        assert_eq!(
            (frames[0].label.as_str(), frames[0].opacity),
            ("Minimize", 0.0)
        );
        assert_eq!(
            (frames[1].label.as_str(), frames[1].opacity),
            ("Close", 1.0)
        );
        assert_eq!(frames[1].bounds.x, 20.0);
        surface.advance(now + duration * 2);
        assert_eq!(surface.snapshots(), vec![report("Minimize", 80.0)]);
    }

    #[test]
    fn unseen_and_zero_duration_tooltips_do_not_flash_or_linger() {
        let mut theme = CompTheme::default();
        let now = IcedInstant::now();
        let mut surface = Surface {
            report: Some(report("Close", 20.0)),
            ..Surface::default()
        };
        surface.sync(&theme, now);
        surface.report = None;
        surface.sync(&theme, now);
        assert!(surface.snapshots().is_empty());
        theme.motion.layer_open = std::time::Duration::ZERO;
        surface.report = Some(report("Close", 20.0));
        surface.sync(&theme, now);
        surface.advance(now);
        assert_eq!(surface.snapshots()[0].opacity, 1.0);
        assert!(!surface.is_animating(now));
        surface.report = None;
        surface.sync(&theme, now);
        assert!(surface.snapshots().is_empty());
    }

    #[test]
    #[ignore = "requires surfaceless EGL (Mesa llvmpipe or a GPU driver)"]
    fn gles_tooltip_fade_keeps_chip_shadow_and_blur_together() -> anyhow::Result<()> {
        use smithay::backend::{
            egl::{EGLContext, EGLDisplay, native::EGLSurfacelessDisplay},
            renderer::{
                Bind, Offscreen, damage::OutputDamageTracker, element::Element as _,
                gles::GlesRenderbuffer, glow::GlowRenderer,
            },
        };
        use std::borrow::BorrowMut;
        // SAFETY: a fresh surfaceless context used exclusively on this thread.
        let display = unsafe { EGLDisplay::new(EGLSurfacelessDisplay)? };
        let context = EGLContext::new(&display)?;
        let mut renderer = unsafe { GlowRenderer::new(context)? };
        crate::backend::render::init_shaders(renderer.borrow_mut())?;
        let theme = CompTheme::default();
        let duration = theme.motion.layer_open;
        for scale in [1.0, 1.5, 2.0] {
            let now = IcedInstant::now();
            let mut surface = Surface {
                report: Some(report("Close", 20.0)),
                ..Surface::default()
            };
            surface.sync(&theme, now);
            let mut previous = None;
            for (closing, elapsed) in [
                (false, std::time::Duration::ZERO),
                (false, duration / 2),
                (false, duration),
                (true, std::time::Duration::ZERO),
                (true, duration / 2),
                (true, duration + std::time::Duration::from_millis(1)),
            ] {
                let at = now
                    + elapsed
                    + if closing {
                        duration
                    } else {
                        std::time::Duration::ZERO
                    };
                if closing && elapsed.is_zero() {
                    surface.report = None;
                    surface.sync(&theme, at);
                }
                surface.advance(at);
                let mut elements = Vec::new();
                surface.push(
                    &mut renderer,
                    &theme,
                    (80.0, 80.0).into(),
                    scale.into(),
                    1.0,
                    0.8,
                    &mut |e| elements.push(e),
                );
                let opacity = surface.snapshots().first().map_or(0.0, |r| r.opacity);
                if opacity == 0.0 {
                    assert!(
                        elements.is_empty(),
                        "no bare blur or shadow before/after the fade"
                    );
                    continue;
                }
                assert_eq!(
                    elements.len(),
                    2,
                    "one complete chip/shadow texture and one backdrop"
                );
                let state: Vec<_> = elements
                    .iter()
                    .map(|e| (e.id().clone(), e.geometry(scale.into())))
                    .collect();
                if let Some(previous) = &previous {
                    assert_eq!(
                        &state, previous,
                        "fade without rerasterizing or moving either element"
                    );
                }
                previous = Some(state);
                for element in &elements {
                    assert!((element.alpha() - opacity * 0.8).abs() < 0.0001);
                }
                let mut target = <GlowRenderer as Offscreen<GlesRenderbuffer>>::create_buffer(
                    &mut renderer,
                    Fourcc::Abgr8888,
                    (512, 512).into(),
                )?;
                let mut fb = renderer.bind(&mut target)?;
                let mut damage = OutputDamageTracker::new((512, 512), scale, Transform::Normal);
                damage.render_output(&mut renderer, &mut fb, 0, &elements, [0.2, 0.4, 0.7, 1.0])?;
            }
        }

        // The chip has its own exit lifetime, even if its header disappears at
        // once. This exercises IcedElement's actual alpha-zero early-return path.
        struct Owner {
            visible: bool,
        }
        impl Program for Owner {
            type Message = ();
            fn view<'a>(&'a self, _: &'a CompTheme) -> CompElement<'a, ()> {
                iced_widget::Space::new().into()
            }
            fn visibility(&self, _: &CompTheme) -> Option<Visibility> {
                Some(Visibility {
                    visible: self.visible,
                    duration: std::time::Duration::ZERO,
                    animate_initial: false,
                    ..Visibility::fade(CompTheme::default().motion)
                })
            }
        }
        let event_loop = calloop::EventLoop::<crate::state::State>::try_new().unwrap();
        let owner = IcedElement::new(
            Owner { visible: true },
            (240, 80),
            event_loop.handle(),
            theme.clone(),
        );
        {
            let mut internal = owner.0.lock().unwrap();
            let now = IcedInstant::now();
            internal.tooltip.report = Some(report("Close", 20.0));
            internal.tooltip.sync(&theme, now - duration * 2);
            internal.tooltip.advance(now - duration * 2);
            internal.tooltip.advance(now);
            internal.program.visible = false;
            internal.update(UpdateSource::Forced);
            assert_eq!(internal.visibility_frame.opacity, 0.0);
            assert!(internal.tooltip.report.is_none());
            assert_eq!(internal.tooltip.closing.len(), 1);
        }
        let mut elements = Vec::new();
        owner.push_render_elements(
            &mut renderer,
            (80, 80).into(),
            1.0.into(),
            1.0,
            [0; 4],
            &mut |e| elements.push(e),
            None,
        );
        assert_eq!(
            elements.len(),
            2,
            "the outgoing tooltip still paints over a hidden header"
        );
        Ok(())
    }
    #[test]
    fn raster_reserves_shadow_space_separately_from_body() {
        let theme = CompTheme::new(
            Arc::new(icetron_themes::dynamic::DEFAULT_THEME_PAIR.load(true)),
            true,
        );
        let body = IcedSize::new(120.0, 24.0);
        for scale in [1.0, 1.25, 2.0] {
            let raster = rasterize("Close", body, &theme, scale).unwrap();
            assert!(raster.padding.bottom > 0.0);
            assert!(raster.padding.left > 0.0);
            assert_eq!(
                raster.logical_size.width,
                body.width + raster.padding.left + raster.padding.right
            );
            assert_eq!(
                raster.physical_size.w,
                (raster.logical_size.width as f64 * scale).round() as i32
            );
        }
    }
}
