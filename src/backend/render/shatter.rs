use std::{
    borrow::{Borrow, BorrowMut},
    sync::{
        Arc, Mutex,
        atomic::{AtomicBool, Ordering},
    },
};

use smithay::{
    backend::{
        allocator::Fourcc,
        renderer::{
            Frame, FrameContext, Offscreen, Renderer,
            element::{Element, Id, Kind, RenderElement},
            gles::{GlesFrame, GlesTexProgram, Uniform},
            utils::{CommitCounter, DamageSet},
        },
    },
    utils::{Buffer, Physical, Rectangle, Scale, Size, Transform, user_data::UserDataMap},
};

use super::{element::AsGlowRenderer, wayland::blur_effect::blit_from_active_fb};
use crate::utils::geometry::{Local, RectLocalExt, SizeExt};

pub static SHADER: &str = include_str!("./shaders/workspace_shatter.frag");

pub struct ShatterShader(pub GlesTexProgram);

impl ShatterShader {
    pub fn get<R: AsGlowRenderer>(renderer: &R) -> GlesTexProgram {
        Borrow::<smithay::backend::renderer::gles::GlesRenderer>::borrow(renderer.glow_renderer())
            .egl_context()
            .user_data()
            .get::<ShatterShader>()
            .expect("Custom Shaders not initialized")
            .0
            .clone()
    }
}

struct ShatterTexture<T>(Mutex<Option<T>>);

impl<T> Default for ShatterTexture<T> {
    fn default() -> Self {
        Self(Mutex::new(None))
    }
}

/// A frozen copy of the outgoing framebuffer, fractured over the live new realm.
pub struct ShatterElement {
    id: Id,
    geometry: Rectangle<i32, Local>,
    src: Size<f64, Buffer>,
    progress: f32,
    forward: bool,
    seed: f32,
    shader: GlesTexProgram,
    captured: Arc<AtomicBool>,
}

impl ShatterElement {
    pub fn new<R: AsGlowRenderer>(
        renderer: &R,
        id: Id,
        geometry: Rectangle<i32, Local>,
        output_scale: f64,
        progress: f32,
        forward: bool,
        seed: f32,
        captured: Arc<AtomicBool>,
    ) -> Self {
        Self {
            id,
            geometry,
            src: geometry
                .size
                .as_logical()
                .to_f64()
                .to_buffer(output_scale, Transform::Normal),
            progress,
            forward,
            seed,
            shader: ShatterShader::get(renderer),
            captured,
        }
    }

    fn uniforms(&self) -> Vec<Uniform<'static>> {
        let aspect = self.geometry.size.w as f32 / self.geometry.size.h.max(1) as f32;
        vec![
            Uniform::new("progress", self.progress),
            Uniform::new("aspect", aspect),
            Uniform::new("direction", if self.forward { 1.0f32 } else { -1.0f32 }),
            Uniform::new("seed", self.seed),
        ]
    }
}

impl Element for ShatterElement {
    fn id(&self) -> &Id {
        &self.id
    }

    fn current_commit(&self) -> CommitCounter {
        CommitCounter::default()
    }

    fn src(&self) -> Rectangle<f64, Buffer> {
        Rectangle::from_size(self.src)
    }

    fn geometry(&self, scale: Scale<f64>) -> Rectangle<i32, Physical> {
        self.geometry.as_logical().to_physical_precise_round(scale)
    }

    fn damage_since(
        &self,
        scale: Scale<f64>,
        _commit: Option<CommitCounter>,
    ) -> DamageSet<i32, Physical> {
        DamageSet::from_slice(&[self.geometry(scale)])
    }

    fn alpha(&self) -> f32 {
        1.0
    }

    fn kind(&self) -> Kind {
        Kind::Unspecified
    }

    fn is_framebuffer_effect(&self) -> bool {
        true
    }
}

impl<R: Renderer + AsGlowRenderer> RenderElement<R> for ShatterElement
where
    R::TextureId: Send + 'static,
{
    fn capture_framebuffer(
        &self,
        frame: &mut R::Frame<'_, '_>,
        src: Rectangle<f64, Buffer>,
        dst: Rectangle<i32, Physical>,
        cache: &UserDataMap,
    ) -> Result<(), R::Error> {
        let texture = cache.get_or_insert_threadsafe(ShatterTexture::<R::TextureId>::default);
        let mut texture = texture.0.lock().unwrap();
        if texture.is_some() {
            self.captured.store(true, Ordering::Release);
            return Ok(());
        }

        let transform = frame.transformation();
        let glow_frame = R::glow_frame_mut(frame);
        let gles_frame = BorrowMut::<GlesFrame<'_, '_>>::borrow_mut(glow_frame);
        let mut renderer = gles_frame.renderer();
        let mut gl_texture = renderer
            .as_mut()
            .create_buffer(Fourcc::Abgr8888, self.src().size.to_i32_round())
            .map_err(R::from_gles_error)?;
        std::mem::drop(renderer);

        let sync = blit_from_active_fb(gles_frame, src, dst, transform, &mut gl_texture)
            .map_err(R::from_gles_error)?;
        gles_frame.wait(&sync).map_err(R::from_gles_error)?;

        let context = gles_frame.renderer().as_ref().context_id();
        *texture = Some(R::tex_from_gl(&context, gl_texture));
        self.captured.store(true, Ordering::Release);
        Ok(())
    }

    fn draw(
        &self,
        frame: &mut R::Frame<'_, '_>,
        src: Rectangle<f64, Buffer>,
        dst: Rectangle<i32, Physical>,
        damage: &[Rectangle<i32, Physical>],
        opaque_regions: &[Rectangle<i32, Physical>],
        cache: Option<&UserDataMap>,
    ) -> Result<(), R::Error> {
        let cache = cache.expect("Framebuffer element without cache");
        let Some(texture) = cache.get::<ShatterTexture<R::TextureId>>() else {
            return Ok(());
        };
        let texture = texture.0.lock().unwrap();
        let Some(texture) = texture.as_ref() else {
            return Ok(());
        };

        BorrowMut::<GlesFrame>::borrow_mut(R::glow_frame_mut(frame))
            .override_default_tex_program(self.shader.clone(), self.uniforms());
        let result = frame.render_texture_from_to(
            texture,
            src,
            dst,
            damage,
            opaque_regions,
            Transform::Normal,
            1.0,
        );
        BorrowMut::<GlesFrame>::borrow_mut(R::glow_frame_mut(frame)).clear_tex_program_override();
        result
    }
}
