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
    ffi::OsStr,
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
    utils::{Buffer, Scale, Size, Transform},
    wayland::seat::WaylandFocus,
};
use tracing::warn;

use crate::{
    backend::render::{RendererRef, element::AsGlowRenderer},
    dbus::notifications::Notification,
    fl,
    shell::element::CosmicSurface,
    state::{State, advertised_node_for_surface},
};

/// Where captures go. Relative, it is taken under the window's home (its
/// workspace's root, or `$HOME` outside one); absolute, every capture lands
/// in that one place.
pub const DIR_ENV: &str = "COSMIC_SCREENSHOT_DIR";
const DEFAULT_DIR: &str = "Captures/Screenshots";
/// Same variable and values as kora-workspaces' `DefaultRoot`: `home`/`HOME`
/// roots the default workspace at `$HOME` itself; anything else, unset
/// included, at `~/Workspaces/default`.
const DEFAULT_ROOT_ENV: &str = "KORA_DEFAULT_WORKSPACE_ROOT";
const WORKSPACES_DIR: &str = "Workspaces";
const DEFAULT_WORKSPACE: &str = "default";
/// Icon name shared with `cosmic-screenshot`, so both toasts look the same.
const NOTIFICATION_ICON: &str = "com.system76.CosmicScreenshot";
/// Toast lifetime in milliseconds, as `cosmic-screenshot` sends it.
const NOTIFICATION_TIMEOUT_MS: i32 = 5000;
const PNG_MIME: &str = "image/png";
/// Longest file stem: the 255-byte name limit less `_NN.png`.
const MAX_STEM_BYTES: usize = 247;
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

pub fn screenshot_window(state: &mut State, surface: &CosmicSurface) {
    let Some(wl_surface) = surface.wl_surface() else {
        return;
    };
    let capture = state
        .backend
        .offscreen_renderer(|kms| {
            advertised_node_for_surface(&wl_surface, &state.common.display_handle)
                .or(*kms.primary_node.read().unwrap())
        })
        .with_context(|| "Failed to get renderer for screenshot")
        .and_then(|renderer| match renderer {
            RendererRef::Glow(renderer) => capture_window(renderer, surface),
            RendererRef::GlMulti(mut renderer) => capture_window(&mut renderer, surface),
        });
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

    // A machine-plane window has no workspace of its own; its capture goes
    // where the user is standing, so it is reachable from there.
    let workspace = {
        let shell = state.common.shell.read();
        shell
            .client_workspace(surface)
            .or_else(|| shell.active_workspace().map(ToString::to_string))
    };
    let directory = std::env::var_os("HOME").map(|home| {
        capture_directory(
            Path::new(&home),
            workspace.as_deref(),
            default_root_is_home(std::env::var(DEFAULT_ROOT_ENV).ok().as_deref()),
            std::env::var_os(DIR_ENV).as_deref(),
        )
    });
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
    let (summary, body) = match &encoded.saved {
        Some(path) => (fl!("screenshot-saved-to"), path.display().to_string()),
        None => (fl!("screenshot-saved-to-clipboard"), String::new()),
    };
    state.common.dbus_state.notify(Notification {
        app_name: fl!("screenshot-app-name"),
        app_icon: NOTIFICATION_ICON.to_owned(),
        summary,
        body,
        expire_timeout: NOTIFICATION_TIMEOUT_MS,
        transient: true,
    });
}

fn capture_window<R>(renderer: &mut R, window: &CosmicSurface) -> anyhow::Result<Capture>
where
    R: Renderer + ImportAll + Offscreen<GlesRenderbuffer> + ExportMem + AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
    R::Error: Send + Sync + 'static,
{
    let bbox = bbox_from_surface_tree(&window.wl_surface().unwrap(), (0, 0));
    let mut elements = Vec::new();
    window.push_render_elements(
        renderer,
        (-bbox.loc.x, -bbox.loc.y).into(),
        Scale::from(1.0),
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
    let size = bbox.size.to_buffer(1, Transform::Normal);
    let mut render_buffer = Offscreen::<GlesRenderbuffer>::create_buffer(renderer, format, size)?;
    let mut fb = renderer.bind(&mut render_buffer)?;
    let mut output_damage_tracker =
        OutputDamageTracker::new(bbox.size.to_physical(1), 1.0, Transform::Normal);
    output_damage_tracker
        .render_output(renderer, &mut fb, 0, &elements, [0.0, 0.0, 0.0, 0.0])
        .map_err(|err| match err {
            smithay::backend::renderer::damage::Error::Rendering(err) => err,
            smithay::backend::renderer::damage::Error::OutputNoMode(_) => unreachable!(),
        })?;
    let mapping = renderer.copy_framebuffer(
        &fb,
        bbox.to_buffer(1, Transform::Normal, &bbox.size),
        format,
    )?;
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

fn default_root_is_home(value: Option<&str>) -> bool {
    matches!(value, Some("home" | "HOME"))
}

/// The home a window's files live in, seen from the machine plane: its
/// workspace's root, or the user's home outside a workspace.
fn workspace_home(home: &Path, workspace: Option<&str>, default_root_is_home: bool) -> PathBuf {
    match workspace {
        None => home.to_path_buf(),
        Some(DEFAULT_WORKSPACE) if default_root_is_home => home.to_path_buf(),
        Some(id) => home.join(WORKSPACES_DIR).join(id),
    }
}

/// Where a capture of a window in `workspace` is saved, with `configured`
/// being [`DIR_ENV`].
fn capture_directory(
    home: &Path,
    workspace: Option<&str>,
    default_root_is_home: bool,
    configured: Option<&OsStr>,
) -> PathBuf {
    let dir = configured
        .filter(|dir| !dir.is_empty())
        .map_or_else(|| PathBuf::from(DEFAULT_DIR), PathBuf::from);
    if dir.is_absolute() {
        dir
    } else {
        workspace_home(home, workspace, default_root_is_home).join(dir)
    }
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
    let stem = file_stem(title, &jiff::Zoned::now());
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

/// `<title>_<date>_<time>`, made safe for the filesystem and short enough to
/// leave room for a counter and the extension.
fn file_stem(title: &str, time: &jiff::Zoned) -> String {
    let mut stem =
        sanitize_filename::sanitize(format!("{}_{}", title, time.strftime("%Y-%m-%d_%H-%M-%S")));
    let mut end = MAX_STEM_BYTES.min(stem.len());
    while !stem.is_char_boundary(end) {
        end -= 1;
    }
    stem.truncate(end);
    stem
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

    fn at(hour: i8, minute: i8, second: i8) -> jiff::Zoned {
        jiff::civil::date(2026, 9, 10)
            .at(hour, minute, second, 0)
            .in_tz("UTC")
            .unwrap()
    }

    #[test]
    fn file_stem_is_title_and_time_without_path_hostile_characters() {
        assert_eq!(
            file_stem("Notes: draft/final", &at(14, 25, 30)),
            "Notes draftfinal_2026-09-10_14-25-30"
        );
        assert_eq!(file_name("a", 0), "a.png");
        assert_eq!(file_name("a", 3), "a_3.png");
    }

    #[test]
    fn file_stem_is_cut_on_a_character_boundary() {
        let stem = file_stem(&"é".repeat(300), &at(0, 0, 0));
        assert!(stem.len() <= MAX_STEM_BYTES);
        assert!(stem.len() > MAX_STEM_BYTES - 2);
        assert!(stem.chars().all(|c| c == 'é'));
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
    fn captures_land_in_the_window_workspace_home() {
        let home = Path::new("/home/u");
        let dir = |workspace, root_is_home, env: Option<&str>| {
            capture_directory(home, workspace, root_is_home, env.map(OsStr::new))
        };
        assert_eq!(
            dir(None, false, None),
            Path::new("/home/u/Captures/Screenshots")
        );
        assert_eq!(
            dir(Some("meridian"), false, None),
            Path::new("/home/u/Workspaces/meridian/Captures/Screenshots")
        );
        assert_eq!(
            dir(Some("default"), false, None),
            Path::new("/home/u/Workspaces/default/Captures/Screenshots")
        );
        assert_eq!(
            dir(Some("default"), true, None),
            Path::new("/home/u/Captures/Screenshots")
        );
        assert!(default_root_is_home(Some("home")));
        assert!(default_root_is_home(Some("HOME")));
        assert!(!default_root_is_home(Some("workspaces")));
        assert!(!default_root_is_home(None));
    }

    #[test]
    fn the_directory_variable_moves_captures() {
        let home = Path::new("/home/u");
        let dir = |workspace, env| capture_directory(home, workspace, false, Some(OsStr::new(env)));
        assert_eq!(
            dir(Some("meridian"), "Shots"),
            Path::new("/home/u/Workspaces/meridian/Shots")
        );
        assert_eq!(dir(Some("meridian"), "/srv/shots"), Path::new("/srv/shots"));
        assert_eq!(dir(None, "/srv/shots"), Path::new("/srv/shots"));
        assert_eq!(dir(None, ""), Path::new("/home/u/Captures/Screenshots"));
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
