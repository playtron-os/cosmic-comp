// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    shell::focus::target::KeyboardFocusTarget, shell::grabs::SeatMoveGrabState, state::ClientState,
    utils::prelude::*,
};
use calloop::Interest;
use calloop::timer::{TimeoutAction, Timer};
use smithay::{
    backend::renderer::{
        element::{Kind, surface::KindEvaluation},
        utils::{on_commit_buffer_handler, with_renderer_surface_state},
    },
    desktop::{LayerSurface, PopupKind, WindowSurfaceType, layer_map_for_output},
    reexports::{
        wayland_protocols::xdg::shell::server::xdg_toplevel::State as ToplevelState,
        wayland_server::{Client, Resource, protocol::wl_surface::WlSurface},
    },
    utils::{Clock, Logical, Monotonic, SERIAL_COUNTER, Size, Time},
    wayland::{
        compositor::{
            BufferAssignment, CompositorClientState, CompositorHandler, CompositorState,
            SurfaceAttributes, SurfaceData, TraversalAction, add_blocker, add_post_commit_hook,
            add_pre_commit_hook, with_states, with_surface_tree_downward,
        },
        dmabuf::get_dmabuf,
        drm_syncobj::DrmSyncobjCachedState,
        seat::WaylandFocus,
        shell::{
            wlr_layer::{
                KeyboardInteractivity, Layer, LayerSurfaceAttributes, LayerSurfaceCachedState,
            },
            xdg::{
                ToplevelSurface, XdgPopupSurfaceRoleAttributes, XdgToplevelSurfaceRoleAttributes,
            },
        },
        xwayland_shell::PhantomXWaylandSurface,
    },
    xwayland::XWaylandClientData,
};
use std::{collections::VecDeque, sync::Mutex, time::Duration};

fn toplevel_ensure_initial_configure(
    toplevel: &ToplevelSurface,
    size: Option<Size<i32, Logical>>,
) -> bool {
    // send the initial configure if relevant
    let initial_configure_sent = with_states(toplevel.wl_surface(), |states| {
        states
            .data_map
            .get::<Mutex<XdgToplevelSurfaceRoleAttributes>>()
            .unwrap()
            .lock()
            .unwrap()
            .initial_configure_sent
    });
    if !initial_configure_sent {
        toplevel.with_pending_state(|states| {
            states.size = size;
        });
        toplevel.send_configure();
    }

    initial_configure_sent
}

fn xdg_popup_ensure_initial_configure(popup: &PopupKind) {
    if let PopupKind::Xdg(popup) = popup {
        let initial_configure_sent = with_states(popup.wl_surface(), |states| {
            states
                .data_map
                .get::<Mutex<XdgPopupSurfaceRoleAttributes>>()
                .unwrap()
                .lock()
                .unwrap()
                .initial_configure_sent
        });
        if !initial_configure_sent {
            // NOTE: This should never fail as the initial configure is always
            // allowed.
            popup.send_configure().expect("initial configure failed");
        }
    }
}

fn layer_surface_check_inital_configure(surface: &LayerSurface) -> bool {
    // send the initial configure if relevant
    with_states(surface.wl_surface(), |states| {
        states
            .data_map
            .get::<Mutex<LayerSurfaceAttributes>>()
            .unwrap()
            .lock()
            .unwrap()
            .initial_configure_sent
    })
}

pub fn client_compositor_state(client: &Client) -> &CompositorClientState {
    if let Some(state) = client.get_data::<XWaylandClientData>() {
        return &state.compositor_state;
    }
    if let Some(state) = client.get_data::<ClientState>() {
        return &state.compositor_client_state;
    }
    panic!("Unknown client data type")
}

#[derive(Debug)]
struct FrametimeData {
    last_commit: Option<Time<Monotonic>>,
    last_diffs: VecDeque<Duration>,
    estimation: Duration,
}

impl Default for FrametimeData {
    fn default() -> Self {
        FrametimeData {
            last_commit: None,
            last_diffs: VecDeque::with_capacity(100),
            estimation: Duration::MAX,
        }
    }
}

pub fn frame_time_estimation(clock: &Clock<Monotonic>, states: &SurfaceData) -> Option<Duration> {
    let data = states
        .data_map
        .get::<Mutex<FrametimeData>>()?
        .lock()
        .unwrap();
    if let Some(ref last) = data.last_commit {
        // if the time since the last commit is already higher than our estimation,
        // there is no reason to not use that as a better "guess"
        let diff = Time::elapsed(last, clock.now());
        Some(diff.max(data.estimation))
    } else {
        Some(data.estimation)
    }
}

pub fn recursive_frame_time_estimation(
    clock: &Clock<Monotonic>,
    surface: &WlSurface,
) -> Option<Duration> {
    let mut overall_estimate = None;
    with_surface_tree_downward(
        surface,
        (),
        |_, _, _| TraversalAction::DoChildren(()),
        |_, data, _| {
            let surface_estimate = frame_time_estimation(clock, data);
            overall_estimate = match (overall_estimate, surface_estimate) {
                (x, None) => x,
                (None, Some(estimate)) => Some(estimate),
                (Some(a), Some(b)) => Some(a.min(b)),
            };
        },
        |_, _, _| true,
    );
    overall_estimate
}

/// True when this surface requests client blur (`org_kde_kwin_blur`).
fn surface_is_blur_backed(states: &SurfaceData) -> bool {
    use crate::wayland::protocols::blur::CacheableBlurState;
    states.cached_state.has::<CacheableBlurState>()
        && states
            .cached_state
            .get::<CacheableBlurState>()
            .current()
            .enabled
}

pub fn frame_time_filter_fn(states: &SurfaceData) -> Kind {
    // Keep blur-backed surfaces off overlay planes: their backdrop is composited into
    // the primary FB, so promoting the surface mid-stream leaves it one frame stale
    // (the #113 idle->scroll blink). Other high-FPS surfaces stay overlay-eligible.
    if surface_is_blur_backed(states) {
        return Kind::Unspecified;
    }

    let clock = Clock::<Monotonic>::new();
    const _20_FPS: Duration = Duration::from_nanos(1_000_000_000 / 20);

    if frame_time_estimation(&clock, states).is_some_and(|dur| dur <= _20_FPS) {
        Kind::ScanoutCandidate
    } else {
        Kind::Unspecified
    }
}

pub const FRAME_TIME_FILTER: KindEvaluation = KindEvaluation::Dynamic(frame_time_filter_fn);

impl CompositorHandler for State {
    fn compositor_state(&mut self) -> &mut CompositorState {
        &mut self.common.compositor_state
    }

    fn client_compositor_state<'a>(&self, client: &'a Client) -> &'a CompositorClientState {
        client_compositor_state(client)
    }

    fn new_surface(&mut self, surface: &WlSurface) {
        add_pre_commit_hook::<Self, _>(surface, move |state, _dh, surface| {
            let mut acquire_point = None;
            let maybe_dmabuf = with_states(surface, |surface_data| {
                acquire_point = surface_data
                    .cached_state
                    .get::<DrmSyncobjCachedState>()
                    .pending()
                    .acquire_point
                    .clone();
                surface_data
                    .cached_state
                    .get::<SurfaceAttributes>()
                    .pending()
                    .buffer
                    .as_ref()
                    .and_then(|assignment| match assignment {
                        BufferAssignment::NewBuffer(buffer) => get_dmabuf(buffer).ok().cloned(),
                        _ => None,
                    })
            });
            if let Some(dmabuf) = maybe_dmabuf {
                if let Some(acquire_point) = acquire_point
                    && let Ok((blocker, source)) = acquire_point.generate_blocker()
                {
                    let client = surface.client().unwrap();
                    let res =
                        state
                            .common
                            .event_loop_handle
                            .insert_source(source, move |_, _, state| {
                                let dh = state.common.display_handle.clone();
                                state
                                    .client_compositor_state(&client)
                                    .blocker_cleared(state, &dh);
                                Ok(())
                            });
                    if res.is_ok() {
                        add_blocker(surface, blocker);
                        return;
                    }
                }
                if let Ok((blocker, source)) = dmabuf.generate_blocker(Interest::READ) {
                    let client = surface.client().unwrap();
                    let res =
                        state
                            .common
                            .event_loop_handle
                            .insert_source(source, move |_, _, state| {
                                let dh = state.common.display_handle.clone();
                                state
                                    .client_compositor_state(&client)
                                    .blocker_cleared(state, &dh);
                                Ok(())
                            });
                    if res.is_ok() {
                        add_blocker(surface, blocker);
                    }
                }
            }
        });

        add_post_commit_hook::<Self, _>(surface, |state, _dh, surface| {
            let now = state.common.clock.now();
            with_states(surface, |states| {
                let mut data = states
                    .data_map
                    .get_or_insert_threadsafe::<Mutex<FrametimeData>, _>(Default::default)
                    .lock()
                    .unwrap();
                if let Some(ref last) = data.last_commit {
                    let diff = Time::elapsed(last, now);
                    data.last_diffs.push_back(diff);
                    if data.last_diffs.len() > 100 {
                        data.last_diffs.pop_front();
                    }
                    data.estimation = data
                        .last_diffs
                        .iter()
                        .fold(Duration::ZERO, |acc, new| acc.saturating_add(*new))
                        / (data.last_diffs.len() as u32);
                }
                data.last_commit = Some(now);
            });
        });
    }

    fn commit(&mut self, surface: &WlSurface) {
        if with_states(surface, |s| {
            s.data_map.get::<PhantomXWaylandSurface>().is_some()
        }) {
            on_commit_buffer_handler::<Self>(surface);
            return;
        }

        // first load the buffer for various smithay helper functions (which also initializes the RendererSurfaceState)
        on_commit_buffer_handler::<Self>(surface);

        // and refresh smithays internal state
        self.common.on_commit(surface);

        // handle initial configure events and map windows if necessary
        let mapped = self.send_initial_configure_and_map(surface);

        let mut shell = self.common.shell.write();

        // schedule a new render
        if let Some(output) = shell.visible_output_for_surface(surface) {
            self.backend.schedule_render(output);
        }

        // Compositor-owned LOCK fade-IN: the moment the ext-session-lock cover surface
        // first presents a buffer, begin fading it in over the still-live desktop
        // (`focus/order.rs` suppresses its lock short-circuit until the fade completes).
        // Latched per-lock so a later redraw commit won't restart the ramp. Kick a
        // render on every output so the fade actually advances (the lock surface may
        // not resolve to a single visible output above).
        if shell.note_lock_surface_first_buffer(surface) {
            let outputs = shell.outputs().cloned().collect::<Vec<_>>();
            for output in &outputs {
                self.backend.schedule_render(output);
            }
        }

        if mapped {
            return;
        }

        if let Some(popup) = self.common.popups.find_popup(surface) {
            xdg_popup_ensure_initial_configure(&popup);
            // The IME popup need to be repositioned when the size changed
            if let PopupKind::InputMethod(_) = popup {
                shell.unconstrain_popup(&popup);
            }
            return;
        }

        if with_renderer_surface_state(surface, |state| state.buffer().is_none()).unwrap_or(false) {
            // handle null-commits causing weird conflicts:

            // session-lock disallows null commits

            // if it was a move-grab?
            let seat = shell.seats.last_active().clone();
            let moved_window = seat
                .user_data()
                .get::<SeatMoveGrabState>()
                .unwrap()
                .lock()
                .unwrap()
                .as_ref()
                .and_then(|state| {
                    state
                        .element()
                        .windows()
                        .any(|(s, _)| {
                            s.wl_surface()
                                .as_deref()
                                .map(|s| s == surface)
                                .unwrap_or(false)
                        })
                        .then(|| state.element())
                });
            if let Some(window) = moved_window {
                if window.is_stack() {
                    let stack = window.stack_ref().unwrap();
                    if let Some(i) = stack.surfaces().position(|s| {
                        s.wl_surface()
                            .as_deref()
                            .map(|s| s == surface)
                            .unwrap_or(false)
                    }) {
                        stack.remove_idx(i);
                    }
                } else {
                    std::mem::drop(shell);
                    seat.get_pointer()
                        .unwrap()
                        .unset_grab(self, SERIAL_COUNTER.next_serial(), 0);
                    return;
                }
            }

            // if it was a layer-shell surface?
            // ignore, that will affect recompute normally

            // if it was a sticky / floating / tiled window
            // we could unmap, I guess?

            // if it was an x11 surface => do nothing, we will get a separate UnmapNotify anyway
        } else {
            // handle some special cases, like grabs and changing layer surfaces

            // If we would re-position the window inside the grab we would get a weird jittery animation.
            // We only want to resize once the client has acknoledged & commited the new size,
            // so we need to carefully track the state through different handlers.
            if let Some(element) = shell.element_for_surface(surface).cloned() {
                crate::shell::layout::floating::ResizeSurfaceGrab::apply_resize_to_location(
                    element.clone(),
                    &mut shell,
                );
            }
        }

        // re-arrange layer-surfaces (commits may change size and positioning)
        let layer_output = shell
            .outputs()
            .find(|o| {
                let map = layer_map_for_output(o);
                map.layer_for_surface(surface, WindowSurfaceType::ALL)
                    .is_some()
            })
            .cloned();

        // Deferred: set when the desktop wallpaper paints its first frame behind a
        // covering login greeter.
        let mut should_dismiss_greeter = false;
        // Deferred (logout handoff): set when the fresh greeter paints over the held
        // desktop frame, so we can resume rendering after the shell guard is dropped.
        let mut release_logout_hold = false;
        // Deferred (login handoff): set when the desktop wallpaper paints over a held
        // greeter frame (`login_hold`), so we can resume rendering after the guard drop.
        let mut release_login_hold = false;

        if let Some(ref output) = layer_output {
            // Record the exclusive zone this client just committed (cached
            // state is fresh from the commit here). The slide animation
            // overrides cached state, so this map is the only reliable record
            // of the client's intent.
            let is_layer_toplevel = layer_map_for_output(output)
                .layer_for_surface(surface, WindowSurfaceType::TOPLEVEL)
                .is_some();
            if is_layer_toplevel {
                let client_zone = with_states(surface, |states| {
                    states
                        .cached_state
                        .get::<LayerSurfaceCachedState>()
                        .current()
                        .exclusive_zone
                });
                shell.record_client_exclusive_zone(surface.id(), client_zone);
            }

            // Persistent-compositor login handoff
            let is_wallpaper_first_frame = self.common.greeter_present
                && layer_map_for_output(output)
                    .layer_for_surface(surface, WindowSurfaceType::TOPLEVEL)
                    .map(|l| matches!(l.layer(), Layer::Background))
                    .unwrap_or(false)
                && with_renderer_surface_state(surface, |s| s.buffer().is_some()).unwrap_or(false);
            if is_wallpaper_first_frame && every_greeter_output_has_wallpaper(&shell) {
                should_dismiss_greeter = true;
            }

            let committing_is_greeter = layer_map_for_output(output)
                .layer_for_surface(surface, WindowSurfaceType::TOPLEVEL)
                .map(|l| l.namespace() == crate::utils::quirks::GREETER_NAMESPACE)
                .unwrap_or(false);

            // Logout handoff RELEASE. Drop the hold and resume rendering when the held
            // frame has real content to give way to. Two release paths:
            //  (a) the fresh greeter painted its first frame over the held desktop frame
            //      (the real logout case) — gated on every greeter-covered output, like
            //      the login handoff;
            //  (b) a fresh WALLPAPER (Background) surface painted — this self-heals a
            //      FALSE arm from a wallpaper CHANGE, where cosmic-bg destroys+recreates
            //      its Background surface (which arms the hold in `layer_destroyed`) and
            //      no greeter ever comes. On a real logout cosmic-bg has exited, so no
            //      wallpaper repaints and only (a) fires — flash-free logout preserved.
            // Not latency-critical: the painting surface already covers the held frame
            // the instant it paints, so releasing a frame late only resumes compositing.
            if shell.logout_hold {
                let surface_has_buffer =
                    with_renderer_surface_state(surface, |s| s.buffer().is_some()).unwrap_or(false);
                let committing_is_wallpaper = layer_map_for_output(output)
                    .layer_for_surface(surface, WindowSurfaceType::TOPLEVEL)
                    .map(|l| matches!(l.layer(), Layer::Background))
                    .unwrap_or(false);
                let greeter_painted = surface_has_buffer
                    && committing_is_greeter
                    && every_greeter_output_has_painted(&shell);
                let wallpaper_repainted = surface_has_buffer && committing_is_wallpaper;
                if greeter_painted && shell.greeter_logout_return() && shell.any_logout_snapshot() {
                    // LOGOUT crossfade
                    let mut greeter_ids = Vec::new();
                    let outputs = shell.outputs().cloned().collect::<Vec<_>>();
                    for o in &outputs {
                        for l in layer_map_for_output(o).layers() {
                            if l.namespace() == crate::utils::quirks::GREETER_NAMESPACE {
                                greeter_ids.push(l.wl_surface().id());
                            }
                        }
                    }
                    for id in &greeter_ids {
                        shell.activate_pending_fade_in(id);
                    }
                    shell.start_greeter_fade_in();
                    shell.logout_hold = false;
                    release_logout_hold = true;
                } else if greeter_painted || wallpaper_repainted {
                    // No captured snapshot (or the self-heal wallpaper-repaint path):
                    // instant reveal, exactly as before.
                    shell.logout_hold = false;
                    release_logout_hold = true;
                    shell.set_greeter_logout_return(false);
                }
            }

            // Login handoff RELEASE: the greeter left before the wallpaper painted, so
            // `login_hold` latched its last frame. Resume rendering the instant a desktop
            // wallpaper (Background) surface paints its first frame.
            if shell.login_hold.contains(output) {
                let committing_is_wallpaper = layer_map_for_output(output)
                    .layer_for_surface(surface, WindowSurfaceType::TOPLEVEL)
                    .map(|l| matches!(l.layer(), Layer::Background))
                    .unwrap_or(false);
                let surface_has_buffer =
                    with_renderer_surface_state(surface, |s| s.buffer().is_some()).unwrap_or(false);
                // Per-output release: drop THIS output's hold the instant ITS own wallpaper
                // paints a real frame, independent of every other output — so no output is
                // ever revealed black while waiting on another to catch up.
                if committing_is_wallpaper && surface_has_buffer {
                    shell.login_hold.remove(output);
                    release_login_hold = true;
                }
            }

            // Override exclusive zones for sliding/hidden surfaces BEFORE
            // arrange() runs, to prevent one-frame jumps when the client
            // commits a new exclusive_zone value.
            shell.override_slide_exclusive_zones(output);
            // Likewise force the actively-resized side panel's size/zone, so the
            // client's own commits can't fight a compositor-driven resize.
            shell.override_active_layer_resize(output);
            // Persistently cap the chat panel's exclusive zone so the desktop never
            // shrinks below the min viewport (the panel surface may still overlap it).
            shell.cap_chat_panel_exclusive_zone(output);

            let changed = layer_map_for_output(output).arrange();
            if changed {
                shell.workspaces.recalculate();
            }
            // Drop the post-grab resize "settle" once the client's buffer has caught up,
            // so the size override stops holding the final width past convergence.
            shell.clear_layer_resize_settle_if_caught_up(output);

            // If this surface was just un-hidden and had a deferred fade-in,
            // start the blur animation now that the client has committed
            // a fresh buffer with actual content.
            let surface_id = surface.id();
            // ...but NOT the greeter during a logout crossfade: it's held at alpha 0 until
            // EVERY greeter output has painted.
            if !(committing_is_greeter && shell.greeter_logout_return()) {
                shell.activate_pending_fade_in(&surface_id);
            }

            // Update layer blur cache when layer surfaces are committed
            // (blur protocol state may have changed)
            crate::wayland::handlers::layer_shell::update_layer_blur_state(
                output,
                shell.hidden_surfaces(),
            );
        }

        // Re-evaluate keyboard focus for layer surfaces whose
        // keyboard_interactivity transitioned to Exclusive in this commit.
        // The visibility handler only checks at show time and the initial
        // map handler only checks at map time, so a transition from None
        // to Exclusive on an already-visible surface would otherwise be
        // missed.  We track which surfaces already had focus granted to
        // avoid stealing focus on every subsequent frame commit.
        let layer_focus_target = layer_output.as_ref().and_then(|output| {
            let map = layer_map_for_output(output);
            let layer = map.layer_for_surface(surface, WindowSurfaceType::TOPLEVEL)?;
            let surface_id = layer.wl_surface().id();
            if shell.is_surface_hidden(&surface_id) {
                return None;
            }
            let wants_exclusive = with_states(layer.wl_surface(), |states| {
                let mut cached = states.cached_state.get::<LayerSurfaceCachedState>();
                let current = cached.current();
                matches!(current.layer, Layer::Top | Layer::Overlay)
                    && current.keyboard_interactivity == KeyboardInteractivity::Exclusive
            });
            if wants_exclusive {
                // Only grant focus once per Exclusive transition
                if shell.exclusive_focus_granted.insert(surface_id) {
                    let target: KeyboardFocusTarget = layer.clone().into();
                    let seat = shell.seats.last_active().clone();
                    return Some((target, seat));
                }
            } else {
                // Interactivity dropped below Exclusive — clear tracking
                shell.exclusive_focus_granted.remove(&surface_id);
            }
            None
        });
        std::mem::drop(shell);

        if let Some((target, seat)) = layer_focus_target {
            Shell::set_focus(self, Some(&target), &seat, None, false);
        }

        if should_dismiss_greeter {
            self.dismiss_greeter();
        }

        if release_logout_hold {
            tracing::info!("logout handoff: greeter painted, resuming rendering");
            let shell = self.common.shell.read();
            for output in shell.outputs() {
                self.backend.schedule_render(output);
            }
        }

        if release_login_hold {
            let shell = self.common.shell.read();
            for output in shell.outputs() {
                self.backend.schedule_render(output);
            }
        }
    }
}

/// True when every output that currently shows a login greeter also has a wallpaper
fn every_greeter_output_has_wallpaper(shell: &Shell) -> bool {
    shell.outputs().all(|output| {
        let map = layer_map_for_output(output);
        let has_greeter = map
            .layers()
            .any(|l| l.namespace() == crate::utils::quirks::GREETER_NAMESPACE);
        if !has_greeter {
            return true; // outputs without a greeter don't gate the handoff
        }
        map.layers().any(|l| {
            matches!(l.layer(), Layer::Background)
                && with_renderer_surface_state(l.wl_surface(), |s| s.buffer().is_some())
                    .unwrap_or(false)
        })
    })
}

/// True if `output` still shows any desktop-session content
fn output_has_desktop_content(shell: &Shell, output: &smithay::output::Output) -> bool {
    let has_layer = layer_map_for_output(output)
        .layers()
        .any(|l| l.namespace() != crate::utils::quirks::GREETER_NAMESPACE);
    let has_window = shell
        .active_space(output)
        .map(|w| w.mapped().next().is_some())
        .unwrap_or(false);
    has_layer || has_window
}

/// True when every output that currently shows a greeter has painted its first greeter
/// frame
fn every_greeter_output_has_painted(shell: &Shell) -> bool {
    shell.outputs().all(|output| {
        let map = layer_map_for_output(output);
        let has_greeter = map
            .layers()
            .any(|l| l.namespace() == crate::utils::quirks::GREETER_NAMESPACE);
        if !has_greeter {
            return true; // outputs without a greeter don't gate the release
        }
        map.layers()
            .filter(|l| l.namespace() == crate::utils::quirks::GREETER_NAMESPACE)
            .any(|l| {
                with_renderer_surface_state(l.wl_surface(), |s| s.buffer().is_some())
                    .unwrap_or(false)
            })
    })
}

/// True if `output` has a Background (wallpaper) layer surface that has painted a buffer.
/// Distinguishes a MAPPED-but-unpainted (black) wallpaper from one with real content — only
/// the latter means the login gap is over and the frame hold can release.
fn output_has_painted_wallpaper(output: &smithay::output::Output) -> bool {
    layer_map_for_output(output).layers().any(|l| {
        matches!(l.layer(), Layer::Background)
            && with_renderer_surface_state(l.wl_surface(), |s| s.buffer().is_some())
                .unwrap_or(false)
    })
}

/// True if `output` still has any greeter layer surface mapped.
fn output_has_greeter(output: &smithay::output::Output) -> bool {
    layer_map_for_output(output)
        .layers()
        .any(|l| l.namespace() == crate::utils::quirks::GREETER_NAMESPACE)
}

impl State {
    /// Close the login greeter's layer surface(s) so it crossfades out.
    fn dismiss_greeter(&mut self) {
        self.common.greeter_present = false;
        {
            let mut shell = self.common.shell.write();
            shell.clear_greeter_fade_in();
            // Begin the compositor-owned LOGIN crossfade: hold the captured greeter frame
            // fully opaque (masking cosmic-bg's first not-yet-real frame / the CLEAR_COLOR
            // reveal gap), then fade it out over the wallpaper. The greeter hard-exits on the
            // `send_close` below, so this snapshot is what carries the transition. No-op (and
            // no regression — instant reveal) if no greeter cover was captured.
            shell.start_login_fade();
        }
        tracing::info!(
            "login handoff: desktop wallpaper painted, closing greeter layer surface (crossfade)"
        );
        let outputs = {
            let shell = self.common.shell.read();
            for output in shell.outputs() {
                for surface in layer_map_for_output(output).layers() {
                    if surface.namespace() == crate::utils::quirks::GREETER_NAMESPACE {
                        surface.layer_surface().send_close();
                    }
                }
            }
            shell.outputs().cloned().collect::<Vec<_>>()
        };
        // Drive the crossfade (the render loop parks otherwise).
        for output in &outputs {
            self.backend.schedule_render(output);
        }
    }

    /// Persistent-compositor LOGOUT handoff — arm `logout_hold` for `output` so its
    /// surface render thread stops presenting new (black) frames: the last composited
    /// desktop frame stays latched on the CRTC until the fresh greeter paints (released
    /// in `commit`).
    /// fallback in `note_possible_logout`.
    pub(crate) fn arm_logout_hold(&mut self, output: &smithay::output::Output) {
        if self.common.greeter_present || self.common.should_stop {
            return;
        }
        if !self.common.shell.read().outputs().any(|o| o == output) {
            return;
        }
        {
            let mut shell = self.common.shell.write();
            if shell.logout_hold {
                return;
            }
            shell.logout_hold = true;
            shell.set_greeter_logout_return(true);
        }
        tracing::info!("logout handoff: holding last desktop frame until greeter paints");
        self.backend.schedule_render(output);
        // Safety net: force-release if the greeter never paints, so the screen can't
        // freeze on the stale desktop frame. The greeter normally paints within ~1-2s.
        let _ = self.common.event_loop_handle.insert_source(
            Timer::from_duration(Duration::from_secs(5)),
            |_now, _, state: &mut State| {
                if state.common.shell.read().logout_hold {
                    tracing::warn!(
                        "logout handoff: greeter did not paint within timeout, releasing hold"
                    );
                    let outputs = state
                        .common
                        .shell
                        .read()
                        .outputs()
                        .cloned()
                        .collect::<Vec<_>>();
                    // Collect greeter surface ids (each per-output layer_map guard dropped
                    // per iteration) so we can force them to full opacity below.
                    let greeter_ids = outputs
                        .iter()
                        .flat_map(|o| {
                            layer_map_for_output(o)
                                .layers()
                                .filter(|l| {
                                    l.namespace() == crate::utils::quirks::GREETER_NAMESPACE
                                })
                                .map(|l| l.wl_surface().id())
                                .collect::<Vec<_>>()
                        })
                        .collect::<Vec<_>>();
                    {
                        let mut shell = state.common.shell.write();
                        shell.logout_hold = false;
                        // Fall back to an INSTANT reveal if the timeout fires mid-fade
                        for id in &greeter_ids {
                            shell.remove_layer_fade_in(id);
                        }
                        shell.clear_greeter_fade_in();
                    }
                    for output in &outputs {
                        state.backend.schedule_render(output);
                    }
                }
                TimeoutAction::Drop
            },
        );
    }

    /// Fallback logout trigger: arm the frame-hold once `output` becomes fully empty of
    /// desktop content. The PRIMARY trigger (wallpaper destroyed) fires earlier and is
    /// what actually holds a non-black frame; this only covers teardown orders / desktop
    /// shapes where the wallpaper is not present (e.g. a toplevel-only session).
    pub(crate) fn note_possible_logout(&mut self, output: &smithay::output::Output) {
        if self.common.greeter_present || self.common.should_stop {
            return;
        }
        let empty = {
            let shell = self.common.shell.read();
            if !shell.outputs().any(|o| o == output) {
                return;
            }
            !output_has_desktop_content(&shell, output)
        };
        if empty {
            self.arm_logout_hold(output);
        }
    }

    /// Persistent-compositor LOGIN handoff — arm `login_hold` for `output` so its surface
    /// render thread stops presenting new (black) frames: the greeter's last frame stays
    /// latched on the CRTC until the desktop wallpaper paints (released in `commit`). The
    /// mirror of `arm_logout_hold`.
    pub(crate) fn arm_login_hold(&mut self, output: &smithay::output::Output) {
        if self.common.should_stop {
            return;
        }
        if !self.common.shell.read().outputs().any(|o| o == output) {
            return;
        }
        {
            let mut shell = self.common.shell.write();
            // insert() returns false if this output was already held — its safety timer is
            // already pending, so don't arm a second one.
            if !shell.login_hold.insert(output.clone()) {
                return;
            }
            // Abnormal greeter departure (crash / timeout / greetd overlap alarm): login_hold
            // carries this output's transition, NOT a dismiss_greeter crossfade — so
            // cleanup_login_fade (fade-in-flight only) will never free the cover we rolled into
            // login_snapshot. Drop it now so a captured greeter texture can't linger the session.
            shell.drop_login_snapshot(output);
        }
        tracing::info!("login handoff: greeter left before wallpaper, holding its last frame");
        self.backend.schedule_render(output);
        // Safety net: force-release THIS output if its wallpaper never paints, so it can't
        // freeze on the stale greeter frame. The wallpaper normally paints within ~1-2s.
        let hold_output = output.clone();
        let _ = self.common.event_loop_handle.insert_source(
            Timer::from_duration(Duration::from_secs(5)),
            move |_now, _, state: &mut State| {
                if state.common.shell.read().login_hold.contains(&hold_output) {
                    tracing::warn!(
                        "login handoff: wallpaper did not paint within timeout, releasing hold"
                    );
                    state.common.shell.write().login_hold.remove(&hold_output);
                    state.backend.schedule_render(&hold_output);
                }
                TimeoutAction::Drop
            },
        );
    }

    /// Persistent-compositor LOGIN handoff fallback
    pub(crate) fn note_possible_login_gap(&mut self, output: &smithay::output::Output) {
        if self.common.should_stop {
            return;
        }
        let should_arm = {
            let shell = self.common.shell.read();
            if !shell.outputs().any(|o| o == output) {
                return;
            }
            // Arm only when the greeter has FULLY left this output (no greeter layer surface
            // remains — so a transient greeter surface tearing down while the greeter is
            // still up cannot false-arm and freeze a live output) AND its wallpaper has not
            // painted yet (which excludes the normal compositor-driven dismiss).
            !output_has_greeter(output) && !output_has_painted_wallpaper(output)
        };
        if should_arm {
            self.arm_login_hold(output);
        }
    }

    fn send_initial_configure_and_map(&mut self, surface: &WlSurface) -> bool {
        // Check for pending windows first
        {
            let shell = self.common.shell.write();

            if let Some(pending) = shell
                .pending_windows
                .iter()
                .find(|pending| pending.surface.wl_surface().as_deref() == Some(surface))
                && let Some(toplevel) = pending.surface.0.toplevel()
            {
                let initial_size = if let Some(output) = pending.fullscreen.as_ref() {
                    Some(output.geometry().size.as_logical())
                } else if pending.maximized {
                    let active_output = shell.seats.last_active().active_output();
                    let zone = layer_map_for_output(&active_output).non_exclusive_zone();
                    // Set maximized + tiled states so the client knows to use
                    // the configured size on its very first commit.
                    toplevel.with_pending_state(|state| {
                        state.states.set(ToplevelState::Maximized);
                        state.states.set(ToplevelState::TiledLeft);
                        state.states.set(ToplevelState::TiledRight);
                        state.states.set(ToplevelState::TiledTop);
                        state.states.set(ToplevelState::TiledBottom);
                    });
                    Some(zone.size)
                } else {
                    // For floating windows, set bounds so the client picks a
                    // size that won't be further reduced by map_internal().
                    // map_internal() caps windows without a max_size to 2/3 of
                    // the non-exclusive zone. Subtract SSD height from the
                    // height so that content + SSD header fits within the cap.
                    let active_output = shell.seats.last_active().active_output();
                    let zone = layer_map_for_output(&active_output).non_exclusive_zone();
                    let has_ssd = !pending.surface.is_decorated(true);
                    let ssd_h = if has_ssd {
                        icetron_p::prelude::header_height(&**shell.theme()) as i32
                    } else {
                        0
                    };
                    let bounds = Size::from((zone.size.w / 3 * 2, zone.size.h / 3 * 2 - ssd_h));
                    toplevel.with_pending_state(|state| {
                        state.bounds = Some(bounds);
                    });
                    None
                };
                let has_buffer =
                    with_renderer_surface_state(surface, |state| state.buffer().is_some())
                        .unwrap_or(false);
                if toplevel_ensure_initial_configure(toplevel, initial_size) && has_buffer {
                    let window = pending.surface.clone();
                    window.on_commit();

                    // Check if this window matches any pending PID-based embed requests
                    // BEFORE mapping, so the surface is marked as embedded before rendering.
                    // This prevents the window from appearing as a separate window for a few frames.
                    // Drop shell lock first as check_pending_pid_embeds_for_window needs &mut self
                    std::mem::drop(shell);
                    self.check_pending_pid_embeds_for_window(&window);

                    // Re-acquire shell lock for map_window
                    let mut shell = self.common.shell.write();
                    let res = shell.map_window(
                        &window,
                        &mut self.common.toplevel_info_state,
                        &mut self.common.workspace_state,
                        &self.common.event_loop_handle,
                    );
                    std::mem::drop(shell);

                    // After mapping, if this window is an embedded child, ensure it's
                    // on the same output as its parent (it may have been placed on the
                    // wrong output based on cursor position).
                    self.ensure_embedded_on_parent_output(&window);

                    if let Some(target) = res {
                        let shell = self.common.shell.read();
                        let seat = shell.seats.last_active().clone();
                        std::mem::drop(shell);
                        Shell::set_focus(self, Some(&target), &seat, None, true);
                    }

                    // This window just mapped its first buffer (first frame of
                    // content) — let the cold-start benchmark record it if active.
                    self.coldstart_notify_window_mapped(surface);
                    return true;
                }
            }
        }

        // X11 toplevel path: no XDG configure/ack cycle. We wait for the first
        // non-empty buffer commit after MapNotify (frame_notified=true) so the
        // window never appears as a black rectangle.
        {
            let shell = self.common.shell.write();
            let maybe_x11_window = shell
                .pending_windows
                .iter()
                .find(|p| {
                    p.frame_notified
                        && p.surface.wl_surface().as_deref() == Some(surface)
                        && p.surface.x11_surface().is_some()
                })
                .map(|p| p.surface.clone());

            if let Some(window) = maybe_x11_window {
                let has_buffer =
                    with_renderer_surface_state(surface, |state| state.buffer().is_some())
                        .unwrap_or(false);
                if has_buffer {
                    window.on_commit();
                    std::mem::drop(shell);

                    self.check_pending_pid_embeds_for_window(&window);

                    let mut shell = self.common.shell.write();
                    let res = shell.map_window(
                        &window,
                        &mut self.common.toplevel_info_state,
                        &mut self.common.workspace_state,
                        &self.common.event_loop_handle,
                    );
                    std::mem::drop(shell);

                    self.ensure_embedded_on_parent_output(&window);

                    if let Some(target) = res {
                        let shell = self.common.shell.read();
                        let seat = shell.seats.last_active().clone();
                        std::mem::drop(shell);
                        Shell::set_focus(self, Some(&target), &seat, None, false);
                    }

                    if let Some(x11) = window.x11_surface().cloned() {
                        self.update_game_mode_input_grab(&x11);
                    }

                    self.coldstart_notify_window_mapped(surface);
                    return true;
                }
                std::mem::drop(shell);
            }
        }

        // Check for pending layers
        let mut shell = self.common.shell.write();
        if let Some(layer_surface) = shell
            .pending_layers
            .iter()
            .find(|pending| pending.surface.wl_surface() == surface)
            .map(|pending| pending.surface.clone())
            && !layer_surface_check_inital_configure(&layer_surface)
        {
            // compute initial dimensions by mapping
            let target = shell.map_layer(&layer_surface);

            let map_output = shell
                .outputs()
                .find(|o| {
                    let map = layer_map_for_output(o);
                    map.layer_for_surface(layer_surface.wl_surface(), WindowSurfaceType::ALL)
                        .is_some()
                })
                .cloned();
            if let Some(output) = map_output {
                crate::wayland::handlers::layer_shell::update_layer_blur_state(
                    &output,
                    shell.hidden_surfaces(),
                );
            }

            if let Some(target) = target {
                let seat = shell.seats.last_active().clone();
                std::mem::drop(shell);
                Shell::set_focus(self, Some(&target), &seat, None, false);
            }
            layer_surface.layer_surface().send_configure();
            return true;
        };

        false
    }
}
