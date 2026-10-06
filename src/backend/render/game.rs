//! Cached game presentation in physical pixels.

use smithay::{desktop::space::SpaceElement, wayland::seat::WaylandFocus};
use std::borrow::{Borrow, BorrowMut};

use smithay::{
    backend::renderer::{
        Bind, Color32F, ContextId, Frame, Renderer, Texture,
        element::{
            Element, Id, Kind, RenderElement, RenderElementStates,
            surface::{WaylandSurfaceRenderElement, render_elements_from_surface_tree},
            texture::TextureRenderElement,
        },
        gles::{
            GlesError, GlesRenderer, GlesTexProgram, GlesTexture, Uniform, UniformName, UniformType,
        },
        glow::{GlowFrame, GlowRenderer},
        utils::{
            CommitCounter, DamageBag, DamageSet, OpaqueRegions, import_surface_tree,
            with_renderer_surface_state,
        },
    },
    utils::{
        Buffer, Logical, Physical, Point, Rectangle, Scale, Size, Transform, user_data::UserDataMap,
    },
};

use super::{element::AsGlowRenderer, fsr, nis, thread_user_data};
use crate::{dbus::game_mode::ScalingMode, shell::CosmicSurface};

#[derive(Debug, PartialEq)]
struct SourceKey {
    id: Id,
    commit: CommitCounter,
    src: Rectangle<f64, Buffer>,
    dst: Rectangle<i32, Physical>,
    transform: Transform,
    alpha: f32,
}

#[derive(Debug)]
pub struct GameFrame {
    source: GlesTexture,
    target: GlesTexture,
    intermediate: Option<GlesTexture>,
    context: ContextId<GlesTexture>,
    keys: Vec<SourceKey>,
    mode: ScalingMode,
    sharpness: f32,
    damage: DamageBag<i32, Buffer>,
    pub applied: &'static str,
    pub fallback: &'static str,
}

impl GameFrame {
    pub fn extend_render_states(&self, root: Id, states: &mut RenderElementStates) {
        let Some(state) = states.element_render_state(root) else {
            return;
        };
        for key in &self.keys {
            states.states.entry(key.id.clone()).or_insert(state);
        }
    }
}

/// Pixels per surface coordinate, including viewport crop and buffer scale.
pub fn source_scale(surface: &CosmicSurface) -> Scale<f64> {
    surface
        .wl_surface()
        .and_then(|wl| {
            with_renderer_surface_state(&wl, |state| {
                let view = state.view()?;
                (view.dst.w > 0 && view.dst.h > 0).then_some(Scale {
                    x: view.src.size.w * state.buffer_scale() as f64 / view.dst.w as f64,
                    y: view.src.size.h * state.buffer_scale() as f64 / view.dst.h as f64,
                })
            })
            .flatten()
        })
        .unwrap_or(Scale::from(1.0))
}

pub fn integer_rect(
    surface: &CosmicSurface,
    output: Size<i32, Physical>,
) -> Rectangle<i32, Physical> {
    let src: Size<i32, Physical> = surface
        .bbox()
        .size
        .to_physical_precise_round(source_scale(surface));
    if src.w <= 0 || src.h <= 0 {
        return Rectangle::from_size(output);
    }
    let factor = (output.w / src.w).min(output.h / src.h).max(1);
    let size = Size::from((src.w * factor, src.h * factor));
    Rectangle::new(
        ((output.w - size.w) / 2, (output.h - size.h) / 2).into(),
        size,
    )
}

struct NearestShader(GlesTexProgram);

fn nearest_program(renderer: &mut GlowRenderer) -> Result<GlesTexProgram, GlesError> {
    let data = thread_user_data(Borrow::<GlesRenderer>::borrow(renderer));
    if let Some(shader) = data.get::<NearestShader>() {
        return Ok(shader.0.clone());
    }
    let gles: &mut GlesRenderer = renderer.borrow_mut();
    let shader = gles.compile_custom_texture_shader(
        include_str!("shaders/nearest.frag"),
        &[UniformName::new("src_size", UniformType::_2f)],
    )?;
    data.insert_if_missing(|| NearestShader(shader.clone()));
    Ok(shader)
}

fn copy_scaled(
    renderer: &mut GlowRenderer,
    source: &GlesTexture,
    target: &mut GlesTexture,
    nearest: bool,
) -> Result<(), GlesError> {
    let program = nearest.then(|| nearest_program(renderer)).transpose()?;
    let renderer: &mut GlesRenderer = renderer.borrow_mut();
    let src_size = source.size();
    let size: Size<i32, Physical> = (target.width() as i32, target.height() as i32).into();
    let full = Rectangle::from_size(size);
    let mut fb = renderer.bind(target)?;
    let sync = {
        let mut frame = renderer.render(&mut fb, size, Transform::Normal)?;
        let uniforms = [Uniform::new(
            "src_size",
            (src_size.w as f32, src_size.h as f32),
        )];
        frame.render_texture_from_to(
            source,
            Rectangle::from_size(src_size.to_f64()),
            full,
            &[full],
            &[],
            Transform::Normal,
            1.0,
            program.as_ref(),
            if nearest { &uniforms } else { &[] },
        )?;
        frame.finish()?
    };
    drop(fb);
    renderer.wait(&sync)
}

/// Resolve the same surface tree and viewport as normal presentation before filtering.
pub fn prepare<R>(
    slot: &mut Option<GameFrame>,
    renderer: &mut R,
    surface: &CosmicSurface,
    destination: Rectangle<i32, Physical>,
    output_scale: f64,
    mode: ScalingMode,
    sharpness: f32,
    alpha: f32,
) -> Option<GameFrameElement>
where
    R: AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
{
    let wl = surface.wl_surface()?;
    import_surface_tree(renderer, &wl).ok()?;
    let scale = source_scale(surface);
    let bbox = surface.bbox();
    let src_size: Size<i32, Physical> = bbox.size.to_physical_precise_round(scale);
    if src_size.w <= 0 || src_size.h <= 0 || destination.is_empty() {
        return None;
    }
    let origin = (Point::<i32, Logical>::from((0, 0)) - bbox.loc).to_physical_precise_round(scale);
    let elements: Vec<WaylandSurfaceRenderElement<R>> =
        render_elements_from_surface_tree(renderer, &wl, origin, scale, 1.0, Kind::Unspecified);
    if elements.is_empty() {
        return None;
    }
    let keys = elements
        .iter()
        .map(|e| SourceKey {
            id: e.id().clone(),
            commit: e.current_commit(),
            src: e.src(),
            dst: e.geometry(scale),
            transform: e.transform(),
            alpha: e.alpha(),
        })
        .collect::<Vec<_>>();
    let context = renderer.glow_renderer().context_id();
    let changed_target = slot.as_ref().is_none_or(|s| {
        s.context != context
            || s.mode != mode
            || s.target.size()
                != destination
                    .size
                    .to_logical(1)
                    .to_buffer(1, Transform::Normal)
            || s.source.size() != src_size.to_logical(1).to_buffer(1, Transform::Normal)
    });
    if changed_target {
        let source = fsr::create_target(renderer, src_size).ok()?;
        let target = if mode == ScalingMode::Nis {
            nis::create_target(renderer, destination.size)
                .ok()
                .or_else(|| fsr::create_target(renderer, destination.size).ok())?
        } else {
            fsr::create_target(renderer, destination.size).ok()?
        };
        let intermediate = if mode == ScalingMode::Fsr {
            Some(fsr::create_target(renderer, destination.size).ok()?)
        } else {
            None
        };
        *slot = Some(GameFrame {
            source,
            target,
            intermediate,
            context: context.clone(),
            keys: Vec::new(),
            mode,
            sharpness,
            // The surface keeps its identity when a target is rebuilt.
            damage: slot
                .as_mut()
                .map(|old| std::mem::take(&mut old.damage))
                .unwrap_or_default(),
            applied: "pending",
            fallback: "",
        });
    }
    let cache = slot.as_mut()?;
    if cache.keys != keys || cache.sharpness != sharpness {
        tracing::trace!(target: crate::logger::GAMING_TARGET, ?src_size, destination = ?destination.size, "game filter refreshed");
        let mut fb = renderer.bind(&mut cache.source).ok()?;
        let sync = {
            let mut frame = renderer.render(&mut fb, src_size, Transform::Normal).ok()?;
            let full = Rectangle::from_size(src_size);
            frame.clear(Color32F::BLACK, &[full]).ok()?;
            for element in elements.iter().rev() {
                let dst = element.geometry(scale);
                element
                    .draw(
                        &mut frame,
                        element.src(),
                        dst,
                        &[Rectangle::from_size(dst.size)],
                        &[],
                        None,
                    )
                    .ok()?;
            }
            frame.finish().ok()?
        };
        drop(fb);
        renderer.wait(&sync).ok()?;
        let result = match mode {
            ScalingMode::Fsr => fsr::upscale(
                renderer,
                &cache.source,
                cache.intermediate.as_mut()?,
                &mut cache.target,
                destination.size,
                sharpness,
            )
            .map_err(|e| match e {
                fsr::Unavailable::Disabled => "disabled",
                fsr::Unavailable::NoUpscale => "not-an-upscale",
                fsr::Unavailable::DegenerateSize => "invalid-size",
                fsr::Unavailable::Failed => "filter-failed",
            }),
            ScalingMode::Nis => nis::upscale(
                renderer,
                &cache.source,
                cache.source.is_y_inverted(),
                &cache.target,
                nis::NisConfig::new(sharpness),
            )
            .map_err(|e| match e {
                nis::Unavailable::Disabled => "disabled",
                nis::Unavailable::NoUpscale => "not-an-upscale",
                nis::Unavailable::OutOfRange => "unsupported-ratio",
                nis::Unavailable::NoProgram => "unsupported-renderer",
                nis::Unavailable::DegenerateSize => "invalid-size",
                nis::Unavailable::Failed => "filter-failed",
            }),
            _ => copy_scaled(
                renderer.glow_renderer_mut(),
                &cache.source,
                &mut cache.target,
                mode == ScalingMode::Integer,
            )
            .map_err(|_| "copy-failed"),
        };
        let (applied, fallback) = match result {
            Ok(()) => (
                if mode == ScalingMode::Integer {
                    "nearest"
                } else if mode.is_filtered() {
                    mode.as_str()
                } else {
                    "linear"
                },
                "",
            ),
            Err(reason) => {
                copy_scaled(
                    renderer.glow_renderer_mut(),
                    &cache.source,
                    &mut cache.target,
                    false,
                )
                .ok()?;
                ("linear", reason)
            }
        };
        if (cache.applied, cache.fallback) != (applied, fallback) {
            tracing::info!(target: crate::logger::GAMING_TARGET, requested = mode.as_str(), applied, fallback, ?src_size, destination = ?destination.size, "game filter applied");
        }
        cache.applied = applied;
        cache.fallback = fallback;
        cache.keys = keys;
        cache.sharpness = sharpness;
        cache
            .damage
            .add([Rectangle::from_size(cache.target.size())]);
    }
    let texture = TextureRenderElement::from_texture_with_damage(
        Id::from_wayland_resource(wl.as_ref()),
        context,
        destination.loc.to_f64(),
        cache.target.clone(),
        1,
        Transform::Normal,
        Some(alpha),
        None,
        None,
        (alpha >= 1.0).then(|| vec![Rectangle::from_size(cache.target.size())]),
        cache.damage.snapshot(),
        Kind::Unspecified,
    );
    Some(GameFrameElement {
        texture,
        destination,
        output_scale,
        opaque: true,
        _buffer: None,
    })
}

pub struct GameFrameElement {
    texture: TextureRenderElement<GlesTexture>,
    destination: Rectangle<i32, Physical>,
    output_scale: f64,
    opaque: bool,
    _buffer: Option<smithay::backend::renderer::utils::Buffer>,
}

impl Element for GameFrameElement {
    fn id(&self) -> &Id {
        self.texture.id()
    }
    fn current_commit(&self) -> CommitCounter {
        self.texture.current_commit()
    }
    fn src(&self) -> Rectangle<f64, Buffer> {
        self.texture.src()
    }
    fn geometry(&self, scale: Scale<f64>) -> Rectangle<i32, Physical> {
        self.destination
            .to_f64()
            .to_logical(self.output_scale)
            .to_physical_precise_round(scale)
    }
    fn transform(&self) -> Transform {
        Transform::Normal
    }
    fn damage_since(
        &self,
        scale: Scale<f64>,
        commit: Option<CommitCounter>,
    ) -> DamageSet<i32, Physical> {
        if commit == Some(self.current_commit()) {
            DamageSet::default()
        } else {
            DamageSet::from_slice(&[Rectangle::from_size(self.geometry(scale).size)])
        }
    }
    fn opaque_regions(&self, scale: Scale<f64>) -> OpaqueRegions<i32, Physical> {
        if self.opaque && self.alpha() >= 1.0 {
            OpaqueRegions::from_slice(&[Rectangle::from_size(self.geometry(scale).size)])
        } else {
            OpaqueRegions::default()
        }
    }
    fn alpha(&self) -> f32 {
        self.texture.alpha()
    }
    fn kind(&self) -> Kind {
        Kind::Unspecified
    }
}

impl RenderElement<GlowRenderer> for GameFrameElement {
    fn draw(
        &self,
        frame: &mut GlowFrame<'_, '_>,
        src: Rectangle<f64, Buffer>,
        dst: Rectangle<i32, Physical>,
        damage: &[Rectangle<i32, Physical>],
        opaque: &[Rectangle<i32, Physical>],
        cache: Option<&UserDataMap>,
    ) -> Result<(), GlesError> {
        RenderElement::<GlowRenderer>::draw(&self.texture, frame, src, dst, damage, opaque, cache)
    }
}

#[derive(Debug, Clone)]
struct SnapshotPart {
    id: Id,
    alpha: f32,
    texture: GlesTexture,
    buffer: Option<smithay::backend::renderer::utils::Buffer>,
    src: Rectangle<f64, Logical>,
    scale: i32,
    transform: Transform,
    destination: Rectangle<i32, Physical>,
    opaque: bool,
}

#[derive(Debug, Clone)]
pub struct GameSnapshot {
    context: ContextId<GlesTexture>,
    parts: Vec<SnapshotPart>,
}

impl GameSnapshot {
    pub fn from_filtered(frame: &GameFrame, destination: Rectangle<i32, Physical>) -> Self {
        Self {
            context: frame.context.clone(),
            parts: vec![SnapshotPart {
                id: Id::new(),
                alpha: 1.0,
                texture: frame.target.clone(),
                buffer: None,
                src: Rectangle::from_size(
                    frame
                        .target
                        .size()
                        .to_logical(1, Transform::Normal)
                        .to_f64(),
                ),
                scale: 1,
                transform: Transform::Normal,
                destination,
                opaque: true,
            }],
        }
    }

    pub fn capture<R>(
        renderer: &mut R,
        surface: &CosmicSurface,
        destination: Rectangle<i32, Physical>,
    ) -> Option<Self>
    where
        R: AsGlowRenderer,
        R::TextureId: Send + Clone + 'static,
    {
        use smithay::backend::renderer::element::surface::WaylandSurfaceTexture;
        let wl = surface.wl_surface()?;
        import_surface_tree(renderer, &wl).ok()?;
        let bbox = surface.bbox();
        if bbox.size.w <= 0 || bbox.size.h <= 0 {
            return None;
        }
        let origin =
            (Point::<i32, Logical>::from((0, 0)) - bbox.loc).to_physical_precise_round(1.0);
        let elements: Vec<WaylandSurfaceRenderElement<R>> =
            render_elements_from_surface_tree(renderer, &wl, origin, 1.0, 1.0, Kind::Unspecified);
        if elements.is_empty() {
            return None;
        }
        let context = renderer.glow_renderer().context_id();
        let fit = Scale {
            x: destination.size.w as f64 / bbox.size.w as f64,
            y: destination.size.h as f64 / bbox.size.h as f64,
        };
        let mut parts = Vec::new();
        for element in elements {
            let (texture, src, scale, transform) = match element.texture() {
                WaylandSurfaceTexture::Texture(texture) => {
                    let texture = R::tex_to_gl(&context, texture)?;
                    let transformed = element.transform().transform_size(texture.size());
                    if element.buffer_size().w <= 0 {
                        return None;
                    }
                    let scale = transformed.w / element.buffer_size().w;
                    (texture, element.view().src, scale, element.transform())
                }
                WaylandSurfaceTexture::SolidColor(color) => {
                    use smithay::backend::{allocator::Fourcc, renderer::ImportMem};
                    let pixels = color
                        .components()
                        .map(|v| (v.clamp(0.0, 1.0) * 255.0).round() as u8);
                    let texture = renderer
                        .glow_renderer_mut()
                        .import_memory(&pixels, Fourcc::Abgr8888, (1, 1).into(), false)
                        .ok()?;
                    (
                        texture,
                        Rectangle::from_size((1.0, 1.0).into()),
                        1,
                        Transform::Normal,
                    )
                }
            };
            let mut dst = element
                .geometry(1.0.into())
                .to_f64()
                .upscale(fit)
                .to_i32_round();
            dst.loc += destination.loc;
            parts.push(SnapshotPart {
                id: Id::new(),
                alpha: element.alpha(),
                texture,
                buffer: Some(element.buffer().clone()),
                src,
                scale,
                transform,
                destination: dst,
                opaque: false,
            });
        }
        Some(Self { context, parts })
    }

    pub fn elements<R>(&self, renderer: &R, alpha: f32, output_scale: f64) -> Vec<GameFrameElement>
    where
        R: AsGlowRenderer,
        R::TextureId: Send + 'static,
    {
        if self.context != renderer.glow_renderer().context_id() {
            return Vec::new();
        }
        self.parts
            .iter()
            .map(|part| GameFrameElement {
                texture: TextureRenderElement::from_static_texture(
                    part.id.clone(),
                    self.context.clone(),
                    part.destination.loc.to_f64(),
                    part.texture.clone(),
                    part.scale,
                    part.transform,
                    Some(alpha * part.alpha),
                    Some(part.src),
                    None,
                    None,
                    Kind::Unspecified,
                ),
                destination: part.destination,
                output_scale,
                opaque: part.opaque,
                _buffer: part.buffer.clone(),
            })
            .collect()
    }
}

#[derive(Debug)]
struct Handoff {
    outgoing: Option<GameSnapshot>,
    requested_at: std::time::Instant,
    started_at: Option<std::time::Instant>,
}

#[derive(Debug, Default)]
pub struct GamePresentation {
    last: std::sync::Mutex<Option<GameSnapshot>>,
    handoff: std::sync::Mutex<Option<Handoff>>,
}

impl GamePresentation {
    pub fn begin(&self, duration: std::time::Duration) {
        let mut handoff = self.handoff.lock().unwrap();
        if let Some(previous) = handoff.as_mut()
            && previous.started_at.is_none_or(|t| t.elapsed() < duration)
        {
            if previous.outgoing.is_none() {
                previous.outgoing = self.last.lock().unwrap().clone();
            }
            previous.started_at = None;
            return;
        }
        *handoff = Some(Handoff {
            outgoing: self.last.lock().unwrap().clone(),
            requested_at: std::time::Instant::now(),
            started_at: None,
        });
    }

    pub fn alpha(&self, duration: std::time::Duration) -> f32 {
        match self.handoff.lock().unwrap().as_ref() {
            Some(handoff) => handoff.started_at.map_or(0.0, |start| {
                let t = (start.elapsed().as_secs_f32() / duration.as_secs_f32().max(f32::EPSILON))
                    .clamp(0.0, 1.0);
                keyframe::ease(keyframe::functions::EaseInOutCubic, 0.0, 1.0, t)
            }),
            None => 1.0,
        }
    }

    pub fn ready(&self, snapshot: GameSnapshot) {
        *self.last.lock().unwrap() = Some(snapshot);
        if let Some(handoff) = self.handoff.lock().unwrap().as_mut() {
            handoff.started_at.get_or_insert_with(|| {
                tracing::debug!(target: crate::logger::GAMING_TARGET, "game presentation ready; starting crossfade");
                std::time::Instant::now()
            });
        }
    }

    pub fn has_backdrop(&self) -> bool {
        self.handoff
            .lock()
            .unwrap()
            .as_ref()
            .is_some_and(|h| h.outgoing.is_some())
    }

    pub fn waiting(&self) -> bool {
        self.handoff
            .lock()
            .unwrap()
            .as_ref()
            .is_some_and(|h| h.started_at.is_none())
    }

    pub fn timed_out(&self) -> bool {
        self.handoff.lock().unwrap().as_ref().is_some_and(|h| {
            h.started_at.is_none() && h.requested_at.elapsed() >= std::time::Duration::from_secs(5)
        })
    }

    pub fn animating(&self, duration: std::time::Duration) -> bool {
        self.handoff.lock().unwrap().as_ref().is_some_and(|h| {
            h.started_at.map_or_else(
                || h.requested_at.elapsed() < std::time::Duration::from_secs(5),
                |t| t.elapsed() < duration,
            )
        })
    }

    pub fn backdrop<R>(
        &self,
        renderer: &R,
        duration: std::time::Duration,
        missing: bool,
        output_scale: f64,
    ) -> Vec<GameFrameElement>
    where
        R: AsGlowRenderer,
        R::TextureId: Send + 'static,
    {
        let mut handoff = self.handoff.lock().unwrap();
        if handoff
            .as_ref()
            .is_some_and(|h| h.started_at.is_some_and(|t| t.elapsed() >= duration))
        {
            *handoff = None;
        }
        if let Some(outgoing) = handoff
            .as_ref()
            .and_then(|transition| transition.outgoing.as_ref())
        {
            return outgoing.elements(renderer, 1.0, output_scale);
        }
        if missing {
            self.last
                .lock()
                .unwrap()
                .as_ref()
                .map_or_else(Vec::new, |s| s.elements(renderer, 1.0, output_scale))
        } else {
            Vec::new()
        }
    }
}
