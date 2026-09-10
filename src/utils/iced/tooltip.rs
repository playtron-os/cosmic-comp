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
    pub report: Option<TooltipReport>,
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
        let Some(report) = self.report.as_ref().filter(|r| r.opacity > 0.0) else {
            return;
        };
        let alpha = parent_alpha * report.opacity;
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
