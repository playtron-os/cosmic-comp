//! Window screenshots, from the halo header or the window menu. The window is
//! rendered offscreen on the compositor thread and the pixels handed to a
//! worker to encode and save; the result comes back to copy the PNG to the
//! clipboard and announce it, the way `cosmic-screenshot` does for output
//! captures.
//!
//! A capture is saved into the home of the workspace the window belongs to —
//! `~/Workspaces/<id>` on the machine plane, which the sandbox binds onto
//! `$HOME` — so it shows up in that workspace's own files. Outside a workspace
//! it is the user's home. [`DIR_ENV`] moves it.

use std::{
    cell::Cell,
    io::Write,
    path::{Path, PathBuf},
    rc::Rc,
};

use anyhow::Context;
use calloop::channel::{self, Event};
use smithay::{
    backend::{
        allocator::Fourcc,
        renderer::{
            ExportMem, ImportAll, Offscreen, Renderer, damage::OutputDamageTracker,
            gles::GlesRenderbuffer,
        },
    },
    desktop::utils::bbox_from_surface_tree,
    input::Seat,
    utils::{Buffer, Physical, Rectangle, Scale, Size, Transform},
    wayland::seat::WaylandFocus,
};
use tracing::warn;

use crate::{
    backend::render::{RendererRef, element::AsGlowRenderer},
    dbus::notifications::Tone,
    fl,
    shell::element::CosmicSurface,
    state::{State, advertised_node_for_surface},
    utils::captures::{self, CaptureKind},
};

const PNG_MIME: &str = "image/png";
/// Captures of one window within the same second before giving up on a name.
const NAME_ATTEMPTS: u32 = 100;

/// Raw RGBA pixels of a captured window.
struct Capture {
    size: Size<i32, Buffer>,
    pixels: Vec<u8>,
    title: String,
}

/// What the worker produced: the PNG, and where it was written if anywhere.
struct Encoded {
    png: Vec<u8>,
    saved: Option<PathBuf>,
}

/// Render `surface` alone, at `scale`, into pixels.
fn capture(state: &mut State, surface: &CosmicSurface, scale: f64) -> anyhow::Result<Capture> {
    let wl_surface = surface.wl_surface().context("the window has no surface")?;
    state
        .backend
        .offscreen_renderer(|kms| {
            advertised_node_for_surface(&wl_surface, &state.common.display_handle)
                .or(*kms.primary_node.read().unwrap())
        })
        .with_context(|| "Failed to get renderer for screenshot")
        .and_then(|renderer| match renderer {
            RendererRef::Glow(renderer) => capture_window(renderer, surface, scale),
            RendererRef::GlMulti(mut renderer) => capture_window(&mut renderer, surface, scale),
        })
}

/// Write `surface` alone, nothing drawn over it, into `out` as a PNG at `scale`,
/// for a client that announces it itself: no clipboard, no toast, no flash.
/// `done` runs on the encoding thread.
pub fn write_window(
    state: &mut State,
    surface: &CosmicSurface,
    scale: f64,
    mut out: std::fs::File,
    done: impl FnOnce(Result<(), String>) + Send + 'static,
) {
    let capture = match capture(state, surface, scale) {
        Ok(capture) => capture,
        Err(err) => return done(Err(format!("{err:#}"))),
    };
    let spawned = std::thread::Builder::new()
        .name("screenshot-encode".into())
        .spawn(move || {
            let written = encode(&capture).and_then(|png| Ok(out.write_all(&png)?));
            done(written.map_err(|err| format!("{err:#}")));
        });
    if let Err(err) = spawned {
        warn!(?err, "Failed to spawn screenshot encoder");
    }
}

pub fn screenshot_window(state: &mut State, surface: &CosmicSurface) {
    let capture = capture(state, surface, 1.0);
    let mut capture = match capture {
        Ok(capture) => capture,
        Err(err) => {
            warn!(?err, "Failed to take screenshot");
            return;
        }
    };
    if capture.title.trim().is_empty() {
        capture.title = fl!("screenshot-app-name");
    }

    // The capture is done, so the flash never ends up in it.
    surface.start_screenshot_flash();
    state.schedule_all_outputs();

    let seat = state.common.shell.read().seats.last_active().clone();
    let (tx, rx) = channel::channel::<Encoded>();
    // The worker sends once and hangs up; the source removes itself on hangup.
    let token = Rc::new(Cell::new(None));
    let source_token = token.clone();
    let registered = state
        .common
        .event_loop_handle
        .insert_source(rx, move |event, _, state| match event {
            Event::Msg(encoded) => deliver(state, &seat, encoded),
            Event::Closed => {
                if let Some(token) = source_token.take() {
                    state.common.event_loop_handle.remove(token);
                }
            }
        });
    match registered {
        Ok(registration) => token.set(Some(registration)),
        Err(err) => {
            warn!(?err, "Failed to register screenshot result source");
            return;
        }
    }

    let directory = captures::directory_for(state, surface, CaptureKind::Screenshot);
    if directory.is_none() {
        warn!("HOME is not set; the screenshot is only copied");
    }
    let spawned = std::thread::Builder::new()
        .name("screenshot-encode".into())
        .spawn(move || match encode(&capture) {
            Ok(png) => {
                let saved = directory.and_then(|directory| save(&directory, &capture.title, &png));
                let _ = tx.send(Encoded { png, saved });
            }
            Err(err) => warn!(?err, "Failed to encode screenshot"),
        });
    if let Err(err) = spawned {
        warn!(?err, "Failed to spawn screenshot encoder");
    }
}

/// Back on the compositor thread: offer the PNG on the clipboard and toast.
fn deliver(state: &mut State, seat: &Seat<State>, encoded: Encoded) {
    crate::clipboard::set_compositor_clipboard(state, seat, PNG_MIME.to_owned(), encoded.png);
    let message = match &encoded.saved {
        Some(_) => fl!("screenshot-saved"),
        None => fl!("screenshot-saved-to-clipboard"),
    };
    state.common.dbus_state.system_toast(message, Tone::Neutral);
}

fn capture_window<R>(
    renderer: &mut R,
    window: &CosmicSurface,
    scale: f64,
) -> anyhow::Result<Capture>
where
    R: Renderer + ImportAll + Offscreen<GlesRenderbuffer> + ExportMem + AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
    R::Error: Send + Sync + 'static,
{
    // Down, not round: a fullscreen window at a fractional scale is a part of
    // a logical pixel wider than the output, and rounding adds a column.
    let bbox: Rectangle<i32, Physical> =
        bbox_from_surface_tree(&window.wl_surface().unwrap(), (0, 0))
            .to_physical_precise_down(scale);
    let mut elements = Vec::new();
    window.push_render_elements(
        renderer,
        (-bbox.loc.x, -bbox.loc.y).into(),
        Scale::from(scale),
        1.0,
        None,
        None,
        false,
        [0; 4],
        0,
        &mut |elem| elements.push(elem),
        None,
    );

    // TODO: 10-bit
    let format = Fourcc::Abgr8888;
    let size = bbox.size.to_logical(1).to_buffer(1, Transform::Normal);
    let mut render_buffer = Offscreen::<GlesRenderbuffer>::create_buffer(renderer, format, size)?;
    let mut fb = renderer.bind(&mut render_buffer)?;
    let mut output_damage_tracker = OutputDamageTracker::new(bbox.size, scale, Transform::Normal);
    output_damage_tracker
        .render_output(renderer, &mut fb, 0, &elements, [0.0, 0.0, 0.0, 0.0])
        .map_err(|err| match err {
            smithay::backend::renderer::damage::Error::Rendering(err) => err,
            smithay::backend::renderer::damage::Error::OutputNoMode(_) => unreachable!(),
        })?;
    let mapping = renderer.copy_framebuffer(&fb, Rectangle::from_size(size), format)?;
    let pixels = renderer.map_texture(&mapping)?.to_vec();
    Ok(Capture {
        size,
        pixels,
        title: window.title(),
    })
}

fn encode(capture: &Capture) -> anyhow::Result<Vec<u8>> {
    let mut png = Vec::new();
    let mut encoder = png::Encoder::new(&mut png, capture.size.w as u32, capture.size.h as u32);
    encoder.set_color(png::ColorType::Rgba);
    encoder.set_depth(png::BitDepth::Eight);
    encoder.set_source_gamma(png::ScaledFloat::new(1.0 / 2.2)); // 1.0 / 2.2, unscaled, but rounded
    let source_chromaticities = png::SourceChromaticities::new(
        // Using unscaled instantiation here
        (0.31270, 0.32900),
        (0.64000, 0.33000),
        (0.30000, 0.60000),
        (0.15000, 0.06000),
    );
    encoder.set_source_chromaticities(source_chromaticities);
    let mut writer = encoder.write_header()?;
    writer.write_image_data(&capture.pixels)?;
    writer.finish()?;
    Ok(png)
}

/// Write `png` into `directory`, named from the window title and the time. A
/// name that is taken gets a counter rather than replacing the earlier capture.
fn save(directory: &Path, title: &str, png: &[u8]) -> Option<PathBuf> {
    if let Err(err) = std::fs::create_dir_all(directory) {
        warn!(
            ?err,
            ?directory,
            "Failed to create the screenshot directory"
        );
        return None;
    }
    let stem = captures::file_stem(title, &jiff::Zoned::now());
    let created = (0..NAME_ATTEMPTS).find_map(|attempt| {
        let path = directory.join(file_name(&stem, attempt));
        match std::fs::File::create_new(&path) {
            Err(err) if err.kind() == std::io::ErrorKind::AlreadyExists => None,
            result => Some(result.map(|file| (file, path))),
        }
    });
    let (mut file, path) = match created {
        Some(Ok(created)) => created,
        Some(Err(err)) => {
            warn!(?err, ?directory, "Failed to create screenshot file");
            return None;
        }
        None => {
            warn!(%stem, "Too many screenshots share this name");
            return None;
        }
    };
    match file.write_all(png) {
        Ok(()) => Some(path),
        Err(err) => {
            warn!(?err, ?path, "Failed to write screenshot");
            None
        }
    }
}

fn file_name(stem: &str, attempt: u32) -> String {
    if attempt == 0 {
        format!("{stem}.png")
    } else {
        format!("{stem}_{attempt}.png")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn file_names_count_up_after_the_first() {
        assert_eq!(file_name("a", 0), "a.png");
        assert_eq!(file_name("a", 3), "a_3.png");
    }

    #[test]
    fn encode_round_trips_pixels() {
        let capture = Capture {
            size: Size::from((2, 1)),
            pixels: vec![1, 2, 3, 255, 4, 5, 6, 128],
            title: String::new(),
        };
        let png = encode(&capture).unwrap();
        let mut reader = png::Decoder::new(std::io::Cursor::new(png))
            .read_info()
            .unwrap();
        let mut decoded = vec![0; reader.output_buffer_size().unwrap()];
        let info = reader.next_frame(&mut decoded).unwrap();
        assert_eq!((info.width, info.height), (2, 1));
        assert_eq!(info.color_type, png::ColorType::Rgba);
        assert_eq!(&decoded[..info.buffer_size()], &capture.pixels[..]);
    }

    #[test]
    fn save_creates_the_directory_and_never_replaces_an_earlier_capture() {
        let directory = std::env::temp_dir()
            .join(format!(
                "cosmic-comp-screenshot-test-{}-{:?}",
                std::process::id(),
                std::thread::current().id()
            ))
            .join("Captures/Screenshots");
        let first = save(&directory, "win", b"one").unwrap();
        let second = save(&directory, "win", b"two").unwrap();
        assert_ne!(first, second);
        assert!(
            first
                .file_name()
                .unwrap()
                .to_str()
                .unwrap()
                .ends_with(".png")
        );
        assert_eq!(std::fs::read(&first).unwrap(), b"one");
        assert_eq!(std::fs::read(&second).unwrap(), b"two");
        std::fs::remove_dir_all(directory.parent().unwrap().parent().unwrap()).unwrap();
    }
}
