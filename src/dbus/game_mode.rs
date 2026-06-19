// Native `one.playtron.GameMode` D-Bus interface for cosmic-comp.
//
// An app-centric gaming-control interface. Because cosmic-comp is a Wayland
// compositor that already tracks apps by app_id, the interface is compact: no
// X11 window enumeration, no XWayland topology, no per-window atom plumbing. It
// exposes the controls a game-mode client (the session/launcher manager) needs
// to drive game mode, with typed state instead of magic u32s + atoms.
//
// Architecture:
//   * A shared snapshot (`GameModeShared`) holds the readable state; the
//     interface getters serve straight from it.
//   * Control calls become `GameModeCommand`s pushed into the calloop event
//     loop, applied by `State::handle_game_mode_command`.
//   * The compositor pushes state changes + signals back out through
//     `GameModeBridge`.

use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex, OnceLock};

use calloop::{LoopHandle, channel::Sender};
use futures_executor::ThreadPool;
use smithay::input::Seat;
use smithay::output::Output;
use tracing::{debug, info, warn};
use zbus::{interface, object_server::SignalEmitter};

use crate::logger::GAMING_TARGET;
use crate::shell::{CosmicSurface, GameMode, Shell};
use crate::state::{Common, State};
use crate::utils::prelude::{OutputExt, SeatExt};

/// Well-known bus name cosmic-comp owns for gaming control.
const BUS_NAME: &str = "one.playtron.GameMode";
/// The single object path; one object, one interface (no ObjectManager needed).
const OBJECT_PATH: &str = "/one/playtron/GameMode";

/// App id the launcher shell presents as. `FocusedAppId == LAUNCHER_APP_ID`
/// means the launcher is in focus (i.e. not in a game).
pub const LAUNCHER_APP_ID: u32 = 769;

/// Window title/class cosmic-comp treats as the launcher shell.
const LAUNCHER_WINDOW_NAME: &str = "grid";

/// Variable-refresh-rate policy.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum VrrMode {
    /// Enable VRR automatically for a fullscreen game (the default).
    #[default]
    Auto,
    /// Always on.
    On,
    /// Always off.
    Off,
}

impl VrrMode {
    pub fn as_str(self) -> &'static str {
        match self {
            VrrMode::Auto => "auto",
            VrrMode::On => "on",
            VrrMode::Off => "off",
        }
    }

    pub fn parse(s: &str) -> Option<Self> {
        match s {
            "auto" => Some(VrrMode::Auto),
            "on" => Some(VrrMode::On),
            "off" => Some(VrrMode::Off),
            _ => None,
        }
    }
}

/// How the game's render resolution is scaled to the physical output.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum ScalingMode {
    /// Present at native resolution, no scaling.
    #[default]
    Native,
    /// Integer (pixel-perfect) scaling.
    Integer,
    /// FSR spatial upscaling.
    Fsr,
    /// Aspect-preserving fit (letterbox).
    Fit,
    /// Aspect-preserving fill (pillarbox / crop).
    Fill,
    /// Non-uniform stretch to fill.
    Stretch,
}

impl ScalingMode {
    pub fn as_str(self) -> &'static str {
        match self {
            ScalingMode::Native => "native",
            ScalingMode::Integer => "integer",
            ScalingMode::Fsr => "fsr",
            ScalingMode::Fit => "fit",
            ScalingMode::Fill => "fill",
            ScalingMode::Stretch => "stretch",
        }
    }

    pub fn parse(s: &str) -> Option<Self> {
        match s {
            "native" => Some(ScalingMode::Native),
            "integer" => Some(ScalingMode::Integer),
            "fsr" => Some(ScalingMode::Fsr),
            "fit" => Some(ScalingMode::Fit),
            "fill" => Some(ScalingMode::Fill),
            "stretch" => Some(ScalingMode::Stretch),
            _ => None,
        }
    }
}

/// The readable snapshot of game-mode state, served to clients. Minimal — no
/// window registry, no atoms.
#[derive(Debug, Default)]
pub struct GameModeShared {
    // State
    pub active: bool,
    /// The app currently in exclusive game mode (0 if none).
    pub active_app_id: u32,
    /// The app currently focused (the game, the launcher shell, or 0).
    pub focused_app_id: u32,
    pub overlay_visible: bool,

    // Tunables
    pub fps_limit: u32,
    pub tearing: bool,
    pub vrr_mode: VrrMode,
    pub hdr_enabled: bool,
    pub scale_width: u32,
    pub scale_height: u32,
    pub scale_mode: ScalingMode,

    // Capabilities / display
    pub vrr_supported: bool,
    pub hdr_supported: bool,
    pub tearing_supported: bool,
    pub refresh_rates: Vec<u32>,
    /// Current refresh rate of the game's output, in Hz.
    pub display_refresh_rate: u32,
    /// Whether the game's output is an external display (vs the internal panel).
    pub display_external: bool,
}

/// Shared I/O the interface uses: read state + send control commands.
#[derive(Debug)]
pub struct GameModeIo {
    pub shared: Mutex<GameModeShared>,
    cmd: Mutex<Sender<GameModeCommand>>,
    /// Live recent frame time (ns) of the output showing the game, written by the
    /// KMS surface thread (via [`Shell`]) and read by `AppFrametimeNs` for Auto-TDP.
    /// 0 when no game is fullscreen.
    frametime_ns: Arc<AtomicU64>,
}

impl GameModeIo {
    fn send(&self, cmd: GameModeCommand) {
        if let Ok(tx) = self.cmd.lock()
            && let Err(err) = tx.send(cmd)
        {
            warn!(?err, "Failed to forward game-mode command into event loop");
        }
    }

    fn with_shared<R>(&self, f: impl FnOnce(&GameModeShared) -> R) -> R {
        f(&self.shared.lock().unwrap())
    }
}

/// Control requests, applied on the compositor event loop.
#[derive(Debug, Clone)]
pub enum GameModeCommand {
    Enter(u32),
    Exit,
    SetFpsLimit(u32),
    SetScaling {
        width: u32,
        height: u32,
        mode: ScalingMode,
    },
    SetTearing(bool),
    SetVrr(VrrMode),
    SetHdr(bool),
    SetOverlay {
        visible: bool,
        blocking: bool,
    },
}

/// Compositor-side handle: mutate the snapshot and emit signals. Stored on
/// [`Common`]; the connection is wired in asynchronously once `serve` connects.
#[derive(Clone)]
pub struct GameModeBridge {
    io: Arc<GameModeIo>,
    conn: Arc<OnceLock<zbus::Connection>>,
    executor: ThreadPool,
}

impl std::fmt::Debug for GameModeBridge {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("GameModeBridge")
            .field("connected", &self.conn.get().is_some())
            .finish()
    }
}

impl GameModeBridge {
    pub fn shared(&self) -> &Mutex<GameModeShared> {
        &self.io.shared
    }

    /// Shared handle the KMS surface thread writes the game's live frame time into
    /// (ns). Wired into [`Shell`] at startup so `AppFrametimeNs` reads live values.
    pub fn frametime_handle(&self) -> Arc<AtomicU64> {
        self.io.frametime_ns.clone()
    }

    fn spawn(&self, fut: impl std::future::Future<Output = ()> + Send + 'static) {
        self.executor.spawn_ok(fut);
    }

    fn with<R>(&self, f: impl FnOnce(&GameModeShared) -> R) -> R {
        f(&self.io.shared.lock().unwrap())
    }

    /// `StateChanged` + the Active/ActiveAppId property changes.
    pub fn notify_state_changed(&self) {
        let Some(conn) = self.conn.get().cloned() else {
            return;
        };
        let (active, app_id) = self.with(|s| (s.active, s.active_app_id));
        self.spawn(async move {
            if let Ok(emitter) = SignalEmitter::new(&conn, OBJECT_PATH) {
                let _ = GameModeInterface::state_changed(&emitter, active, app_id).await;
            }
            if let Ok(iref) = conn
                .object_server()
                .interface::<_, GameModeInterface>(OBJECT_PATH)
                .await
            {
                let e = iref.signal_emitter().clone();
                let iface = iref.get().await;
                let _ = iface.active_changed(&e).await;
                let _ = iface.active_app_id_changed(&e).await;
            }
        });
    }

    /// `FocusChanged` + the FocusedAppId/OverlayVisible property changes.
    pub fn notify_focus_changed(&self) {
        let Some(conn) = self.conn.get().cloned() else {
            return;
        };
        let (app_id, overlay) = self.with(|s| (s.focused_app_id, s.overlay_visible));
        self.spawn(async move {
            if let Ok(emitter) = SignalEmitter::new(&conn, OBJECT_PATH) {
                let _ = GameModeInterface::focus_changed(&emitter, app_id, overlay).await;
            }
            if let Ok(iref) = conn
                .object_server()
                .interface::<_, GameModeInterface>(OBJECT_PATH)
                .await
            {
                let e = iref.signal_emitter().clone();
                let iface = iref.get().await;
                let _ = iface.focused_app_id_changed(&e).await;
                let _ = iface.overlay_visible_changed(&e).await;
            }
        });
    }

    /// `CapabilitiesChanged` + the capability property changes.
    pub fn notify_capabilities_changed(&self) {
        let Some(conn) = self.conn.get().cloned() else {
            return;
        };
        self.spawn(async move {
            if let Ok(emitter) = SignalEmitter::new(&conn, OBJECT_PATH) {
                let _ = GameModeInterface::capabilities_changed(&emitter).await;
            }
            if let Ok(iref) = conn
                .object_server()
                .interface::<_, GameModeInterface>(OBJECT_PATH)
                .await
            {
                let e = iref.signal_emitter().clone();
                let iface = iref.get().await;
                let _ = iface.vrr_supported_changed(&e).await;
                let _ = iface.hdr_supported_changed(&e).await;
                let _ = iface.tearing_supported_changed(&e).await;
                let _ = iface.refresh_rates_changed(&e).await;
                let _ = iface.display_refresh_rate_changed(&e).await;
                let _ = iface.display_external_changed(&e).await;
            }
        });
    }

    /// Property changes for the tunables (fps limit / tearing / vrr / scaling).
    pub fn notify_tunables_changed(&self) {
        let Some(conn) = self.conn.get().cloned() else {
            return;
        };
        self.spawn(async move {
            if let Ok(iref) = conn
                .object_server()
                .interface::<_, GameModeInterface>(OBJECT_PATH)
                .await
            {
                let e = iref.signal_emitter().clone();
                let iface = iref.get().await;
                let _ = iface.fps_limit_changed(&e).await;
                let _ = iface.tearing_changed(&e).await;
                let _ = iface.vrr_changed(&e).await;
                let _ = iface.hdr_enabled_changed(&e).await;
                let _ = iface.scaling_changed(&e).await;
            }
        });
    }
}

// ───────────────────────────── the interface ───────────────────────────────

/// `one.playtron.GameMode` — cosmic-comp's gaming-control interface.
pub struct GameModeInterface {
    io: Arc<GameModeIo>,
}

#[interface(name = "one.playtron.GameMode")]
impl GameModeInterface {
    /// Enter exclusive game mode for `app_id`: make that app the fullscreen game
    /// (engaging the direct-scanout + VRR + tearing fast paths) and clear the
    /// rest with an animated transition.
    async fn enter_game_mode(&self, app_id: u32) {
        self.io.send(GameModeCommand::Enter(app_id));
    }

    /// Leave game mode and return to the launcher.
    async fn exit_game_mode(&self) {
        self.io.send(GameModeCommand::Exit);
    }

    /// Cap the in-game frame rate (0 = uncapped).
    async fn set_fps_limit(&self, fps: u32) {
        self.io.send(GameModeCommand::SetFpsLimit(fps));
    }

    /// Set the game's render resolution + how it scales to the output.
    /// `mode` is one of `native|integer|fsr|fit|fill|stretch`.
    async fn set_scaling(&self, width: u32, height: u32, mode: &str) -> zbus::fdo::Result<()> {
        let mode = ScalingMode::parse(mode).ok_or_else(|| {
            zbus::fdo::Error::InvalidArgs(format!("unknown scaling mode {mode:?}"))
        })?;
        self.io.send(GameModeCommand::SetScaling {
            width,
            height,
            mode,
        });
        Ok(())
    }

    /// Allow tearing (immediate / non-vblank-latched flips) for the game.
    async fn set_tearing(&self, enabled: bool) {
        self.io.send(GameModeCommand::SetTearing(enabled));
    }

    /// Set the VRR policy: `off|on|auto`.
    async fn set_vrr(&self, mode: &str) -> zbus::fdo::Result<()> {
        let mode = VrrMode::parse(mode)
            .ok_or_else(|| zbus::fdo::Error::InvalidArgs(format!("unknown vrr mode {mode:?}")))?;
        self.io.send(GameModeCommand::SetVrr(mode));
        Ok(())
    }

    /// Enable/disable HDR output. No-op while `HdrSupported` is false (cosmic-comp
    /// has no HDR pipeline yet) — present for interface completeness.
    async fn set_hdr(&self, enabled: bool) {
        self.io.send(GameModeCommand::SetHdr(enabled));
    }

    /// Show/hide the launcher overlay over the game. `blocking` routes pointer
    /// input to the overlay while the game keeps rendering (the QAM behaviour).
    async fn set_overlay(&self, visible: bool, blocking: bool) {
        self.io
            .send(GameModeCommand::SetOverlay { visible, blocking });
    }

    // ── state ──
    #[zbus(property)]
    async fn active(&self) -> bool {
        self.io.with_shared(|s| s.active)
    }

    #[zbus(property)]
    async fn active_app_id(&self) -> u32 {
        self.io.with_shared(|s| s.active_app_id)
    }

    #[zbus(property)]
    async fn focused_app_id(&self) -> u32 {
        self.io.with_shared(|s| s.focused_app_id)
    }

    #[zbus(property)]
    async fn overlay_visible(&self) -> bool {
        self.io.with_shared(|s| s.overlay_visible)
    }

    // ── tunables (read; set via the methods above) ──
    #[zbus(property)]
    async fn fps_limit(&self) -> u32 {
        self.io.with_shared(|s| s.fps_limit)
    }

    #[zbus(property)]
    async fn tearing(&self) -> bool {
        self.io.with_shared(|s| s.tearing)
    }

    #[zbus(property)]
    async fn vrr(&self) -> String {
        self.io.with_shared(|s| s.vrr_mode.as_str().to_string())
    }

    #[zbus(property)]
    async fn hdr_enabled(&self) -> bool {
        self.io.with_shared(|s| s.hdr_enabled)
    }

    /// `(width, height, mode)` of the current game-mode scaling.
    #[zbus(property)]
    async fn scaling(&self) -> (u32, u32, String) {
        self.io.with_shared(|s| {
            (
                s.scale_width,
                s.scale_height,
                s.scale_mode.as_str().to_string(),
            )
        })
    }

    // ── capabilities ──
    #[zbus(property)]
    async fn vrr_supported(&self) -> bool {
        self.io.with_shared(|s| s.vrr_supported)
    }

    #[zbus(property)]
    async fn hdr_supported(&self) -> bool {
        self.io.with_shared(|s| s.hdr_supported)
    }

    #[zbus(property)]
    async fn tearing_supported(&self) -> bool {
        self.io.with_shared(|s| s.tearing_supported)
    }

    #[zbus(property)]
    async fn refresh_rates(&self) -> Vec<u32> {
        self.io.with_shared(|s| s.refresh_rates.clone())
    }

    /// Current refresh rate of the game's output, in Hz.
    #[zbus(property)]
    async fn display_refresh_rate(&self) -> u32 {
        self.io.with_shared(|s| s.display_refresh_rate)
    }

    /// Whether the game's output is an external display (vs the internal panel).
    #[zbus(property)]
    async fn display_external(&self) -> bool {
        self.io.with_shared(|s| s.display_external)
    }

    // ── metrics ──
    /// Recent frame time (ns) of `app_id` if it's the active fullscreen game, else
    /// 0. The per-app frame-timing source for Auto-TDP.
    async fn app_frametime_ns(&self, app_id: u32) -> u64 {
        if self
            .io
            .with_shared(|s| s.active && s.active_app_id == app_id)
        {
            self.io.frametime_ns.load(Ordering::Relaxed)
        } else {
            0
        }
    }

    // ── signals ──
    #[zbus(signal)]
    async fn state_changed(
        emitter: &SignalEmitter<'_>,
        active: bool,
        app_id: u32,
    ) -> zbus::Result<()>;

    #[zbus(signal)]
    async fn focus_changed(
        emitter: &SignalEmitter<'_>,
        app_id: u32,
        overlay_visible: bool,
    ) -> zbus::Result<()>;

    #[zbus(signal)]
    async fn capabilities_changed(emitter: &SignalEmitter<'_>) -> zbus::Result<()>;
}

// ──────────────────────────── lifecycle / wiring ───────────────────────────

/// Set up the game-mode control bridge: shared snapshot, a command channel into
/// the event loop, and the D-Bus server on the executor.
pub fn init(handle: &LoopHandle<'static, State>, executor: &ThreadPool) -> GameModeBridge {
    let (cmd_tx, cmd_rx) = calloop::channel::channel::<GameModeCommand>();

    let io = Arc::new(GameModeIo {
        shared: Mutex::new(GameModeShared::default()),
        cmd: Mutex::new(cmd_tx),
        frametime_ns: Arc::new(AtomicU64::new(0)),
    });

    if let Err(err) = handle.insert_source(cmd_rx, |event, _, state| {
        if let calloop::channel::Event::Msg(cmd) = event {
            state.handle_game_mode_command(cmd);
        }
    }) {
        warn!(?err, "Failed to register game-mode command channel");
    }

    let conn = Arc::new(OnceLock::new());

    let serve_io = io.clone();
    let serve_conn = conn.clone();
    executor.spawn_ok(async move {
        match serve(serve_io).await {
            Ok(connection) => {
                let _ = serve_conn.set(connection);
                debug!("Serving {BUS_NAME}");
            }
            Err(err) => {
                warn!(
                    ?err,
                    "Failed to serve {BUS_NAME}; gaming control unavailable"
                );
            }
        }
    });

    GameModeBridge {
        io,
        conn,
        executor: executor.clone(),
    }
}

async fn serve(io: Arc<GameModeIo>) -> zbus::Result<zbus::Connection> {
    let conn = zbus::Connection::session().await?;
    conn.object_server()
        .at(OBJECT_PATH, GameModeInterface { io })
        .await?;
    conn.request_name(BUS_NAME).await?;
    Ok(conn)
}

// ─────────────────────────── compositor side ───────────────────────────────

impl State {
    /// Apply a control request that arrived over the game-mode D-Bus interface.
    pub fn handle_game_mode_command(&mut self, cmd: GameModeCommand) {
        let bridge = self.common.game_mode_bridge.clone();
        match cmd {
            GameModeCommand::Enter(app_id) => {
                debug!(app_id, "game-mode: enter");
                self.enter_game_mode(app_id);
                self.refresh_game_mode_state();
            }
            GameModeCommand::Exit => {
                debug!("game-mode: exit");
                self.exit_game_mode();
                self.refresh_game_mode_state();
            }
            GameModeCommand::SetFpsLimit(fps) => {
                bridge.shared().lock().unwrap().fps_limit = fps;
                // The KMS surface thread reads this and caps the presentation
                // rate for the output showing the fullscreen game.
                self.common.shell.write().game_mode_fps_limit = fps;
                debug!(fps, "game-mode: set fps limit");
                bridge.notify_tunables_changed();
            }
            GameModeCommand::SetTearing(enabled) => {
                bridge.shared().lock().unwrap().tearing = enabled;
                self.common.shell.write().tearing_allowed = enabled;
                debug!(enabled, "game-mode: set tearing");
                bridge.notify_tunables_changed();
            }
            GameModeCommand::SetVrr(mode) => {
                bridge.shared().lock().unwrap().vrr_mode = mode;
                // The KMS surface thread reads this and overrides the output's
                // adaptive-sync policy while a game is fullscreen.
                self.common.shell.write().game_mode_vrr = mode;
                debug!(mode = mode.as_str(), "game-mode: set vrr");
                bridge.notify_tunables_changed();
            }
            GameModeCommand::SetHdr(enabled) => {
                bridge.shared().lock().unwrap().hdr_enabled = enabled;
                // No-op until cosmic-comp grows an HDR output pipeline; the state
                // is tracked so clients read back what they set.
                debug!(enabled, "game-mode: set hdr (no-op, HDR unsupported)");
                bridge.notify_tunables_changed();
            }
            GameModeCommand::SetScaling {
                width,
                height,
                mode,
            } => {
                {
                    let mut s = bridge.shared().lock().unwrap();
                    s.scale_width = width;
                    s.scale_height = height;
                    s.scale_mode = mode;
                }
                // TODO(gaming, PLAN.md Phase 3): resolution spoof + upscaler.
                debug!(
                    width,
                    height,
                    mode = mode.as_str(),
                    "game-mode: set scaling"
                );
                bridge.notify_tunables_changed();
            }
            GameModeCommand::SetOverlay { visible, blocking } => {
                bridge.shared().lock().unwrap().overlay_visible = visible;
                // TODO(gaming, PLAN.md Phase 2): render-focus vs input-focus split.
                debug!(visible, blocking, "game-mode: set overlay");
                bridge.notify_focus_changed();
            }
        }
    }

    /// Rebuild the game-mode snapshot (focus, display caps) from current
    /// compositor state and emit change notifications.
    pub fn refresh_game_mode_state(&mut self) {
        let bridge = self.common.game_mode_bridge.clone();
        let shell = self.common.shell.read();

        let active = shell.game_mode.active;
        let active_app_id = shell.game_mode.app_id.unwrap_or(0);

        // Focused app id, via the focused window (heuristic until a real app-id
        // signal lands — PLAN.md §4.3).
        let focused_app_id = shell
            .seats
            .last_active()
            .get_keyboard()
            .and_then(|kbd| kbd.current_focus())
            .and_then(|target| shell.focused_element(&target))
            .map(|mapped| app_id_of(&mapped.active_window()))
            .unwrap_or(0);

        let output = shell.outputs().next().cloned();
        drop(shell);

        let display_refresh_rate = output
            .as_ref()
            .and_then(|o| o.current_mode())
            .map(|mode| (mode.refresh as f64 / 1000.0).round() as u32)
            .unwrap_or(0);
        let refresh_rates = if display_refresh_rate > 0 {
            vec![display_refresh_rate]
        } else {
            Vec::new()
        };
        let display_external = output.as_ref().map(|o| !o.is_internal()).unwrap_or(false);

        let (state_changed, focus_changed, caps_changed) = {
            let mut s = bridge.shared().lock().unwrap();
            let state_changed = s.active != active || s.active_app_id != active_app_id;
            let focus_changed = s.focused_app_id != focused_app_id;
            let caps_changed = s.refresh_rates != refresh_rates
                || s.display_refresh_rate != display_refresh_rate
                || s.display_external != display_external;

            s.active = active;
            s.active_app_id = active_app_id;
            s.focused_app_id = focused_app_id;
            s.refresh_rates = refresh_rates;
            s.display_refresh_rate = display_refresh_rate;
            s.display_external = display_external;

            (state_changed, focus_changed, caps_changed)
        };

        if state_changed {
            bridge.notify_state_changed();
        }
        if focus_changed {
            bridge.notify_focus_changed();
        }
        if caps_changed {
            bridge.notify_capabilities_changed();
        }
    }

    /// Enter exclusive gaming mode for `app_id`: make the game an exclusive
    /// fullscreen surface (which engages the VRR/direct-scanout/tearing fast
    /// path) and minimize everything else. If no game window is focused/mapped
    /// yet, the request is deferred and retried from `refresh_game_mode_state`.
    pub fn enter_game_mode(&mut self, app_id: u32) {
        let loop_handle = self.common.event_loop_handle.clone();
        let mut shell = self.common.shell.write();

        if shell.game_mode.active {
            // Already in game mode — just retarget the appid.
            shell.game_mode.app_id = Some(app_id);
            shell.game_mode.pending_app_id = None;
            return;
        }

        let seat = shell.seats.last_active().clone();
        let output = seat.active_output();

        let Some(game) = game_target_surface(&shell, &seat, &output) else {
            shell.game_mode.pending_app_id = Some(app_id);
            debug!(app_id, "game mode deferred: no game window yet");
            return;
        };

        // Everything else mapped on the active workspace gets minimized.
        let others: Vec<CosmicSurface> = shell
            .active_space(&output)
            .map(|ws| {
                ws.mapped()
                    .map(|mapped| mapped.active_window())
                    .filter(|surface| surface != &game)
                    .collect()
            })
            .unwrap_or_default();

        info!(target: GAMING_TARGET, app_id, minimized = others.len(), "entering game mode");

        // Fullscreen the game first — this is what turns on the gaming fast path.
        let focus = shell.fullscreen_request(&game, output.clone(), &loop_handle);
        for surface in &others {
            shell.minimize_request(surface);
        }

        shell.game_mode = GameMode {
            active: true,
            app_id: Some(app_id),
            game_surface: Some(game),
            minimized: others,
            pending_app_id: None,
        };
        drop(shell);

        // Give the game keyboard focus so it receives input immediately.
        if let Some(target) = focus {
            Shell::set_focus(self, Some(&target), &seat, None, true);
        }
    }

    /// Exit gaming mode: un-fullscreen the game and restore minimized windows.
    pub fn exit_game_mode(&mut self) {
        let loop_handle = self.common.event_loop_handle.clone();
        let mut shell = self.common.shell.write();
        if !shell.game_mode.active {
            shell.game_mode.pending_app_id = None;
            return;
        }

        let game_mode = std::mem::take(&mut shell.game_mode);
        let seat = shell.seats.last_active().clone();

        if let Some(game) = &game_mode.game_surface {
            shell.unfullscreen_request(game, &loop_handle);
        }
        for surface in &game_mode.minimized {
            shell.unminimize_request(surface, &seat, &loop_handle);
        }
        info!(target: GAMING_TARGET, "exited game mode");
    }
}

/// Whether a surface is the launcher shell (heuristic, pending a real
/// app-id signal — see PLAN.md §4.3).
fn is_launcher_surface(surface: &CosmicSurface) -> bool {
    surface
        .title()
        .to_lowercase()
        .contains(LAUNCHER_WINDOW_NAME)
        || surface
            .app_id()
            .to_lowercase()
            .contains(LAUNCHER_WINDOW_NAME)
}

/// Best-effort numeric app id for a surface (launcher → 769, else a numeric class).
fn app_id_of(surface: &CosmicSurface) -> u32 {
    if is_launcher_surface(surface) {
        LAUNCHER_APP_ID
    } else {
        surface.app_id().parse::<u32>().unwrap_or(0)
    }
}

/// The window game mode should fullscreen: the focused non-shell toplevel, else
/// the topmost non-shell window on the active workspace.
fn game_target_surface(
    shell: &Shell,
    seat: &Seat<State>,
    output: &Output,
) -> Option<CosmicSurface> {
    if let Some(focused) = seat
        .get_keyboard()
        .and_then(|kbd| kbd.current_focus())
        .and_then(|target| shell.focused_element(&target))
        .map(|mapped| mapped.active_window())
        && !is_launcher_surface(&focused)
    {
        return Some(focused);
    }

    shell
        .active_space(output)?
        .mapped()
        .map(|mapped| mapped.active_window())
        .find(|surface| !is_launcher_surface(surface))
}

/// Update display-capability fields that depend on a specific output (called
/// from the KMS surface thread as outputs/caps change).
pub fn update_display_caps(common: &Common, vrr_supported: bool, tearing_supported: bool) {
    let bridge = &common.game_mode_bridge;
    let changed = {
        let mut s = bridge.shared().lock().unwrap();
        let changed = s.vrr_supported != vrr_supported || s.tearing_supported != tearing_supported;
        s.vrr_supported = vrr_supported;
        s.tearing_supported = tearing_supported;
        changed
    };
    if changed {
        bridge.notify_capabilities_changed();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// End-to-end over a live session bus: own the well-known name, then from a
    /// separate client connection introspect the member surface, read a
    /// property, and drive a control method. Uses only client→server calls (a
    /// self-introspect deadlocks under `zbus::block_on`). Skips if no bus.
    #[test]
    fn interface_over_bus() {
        zbus::block_on(async {
            let (tx, rx) = calloop::channel::channel::<GameModeCommand>();
            std::mem::forget(rx);
            let io = Arc::new(GameModeIo {
                shared: Mutex::new(GameModeShared {
                    active: true,
                    active_app_id: 1234,
                    focused_app_id: LAUNCHER_APP_ID,
                    ..Default::default()
                }),
                cmd: Mutex::new(tx),
                frametime_ns: Arc::new(AtomicU64::new(0)),
            });
            let conn = match zbus::Connection::session().await {
                Ok(c) => c,
                Err(_) => {
                    eprintln!("no session bus; skipping");
                    return;
                }
            };
            if conn
                .object_server()
                .at(OBJECT_PATH, GameModeInterface { io })
                .await
                .is_err()
                || conn.request_name(BUS_NAME).await.is_err()
            {
                eprintln!("skipping: could not own {BUS_NAME}");
                return;
            }

            let client = zbus::Connection::session().await.unwrap();

            // Introspect the member surface.
            let xml: String = client
                .call_method(
                    Some(BUS_NAME),
                    OBJECT_PATH,
                    Some("org.freedesktop.DBus.Introspectable"),
                    "Introspect",
                    &(),
                )
                .await
                .unwrap()
                .body()
                .deserialize()
                .unwrap();
            for member in [
                "one.playtron.GameMode",
                "EnterGameMode",
                "ExitGameMode",
                "SetFpsLimit",
                "SetScaling",
                "SetTearing",
                "SetVrr",
                "SetOverlay",
                "Active",
                "ActiveAppId",
                "FocusedAppId",
                "FpsLimit",
                "Tearing",
                "Vrr",
                "Scaling",
                "VrrSupported",
                "TearingSupported",
                "RefreshRates",
                "StateChanged",
                "FocusChanged",
                "CapabilitiesChanged",
            ] {
                assert!(xml.contains(member), "interface missing `{member}`:\n{xml}");
            }

            // Read a property.
            let reply = client
                .call_method(
                    Some(BUS_NAME),
                    OBJECT_PATH,
                    Some("org.freedesktop.DBus.Properties"),
                    "Get",
                    &("one.playtron.GameMode", "ActiveAppId"),
                )
                .await
                .unwrap();
            let value: zbus::zvariant::OwnedValue = reply.body().deserialize().unwrap();
            assert_eq!(u32::try_from(value).unwrap(), 1234);

            // Drive a control method.
            client
                .call_method(
                    Some(BUS_NAME),
                    OBJECT_PATH,
                    Some("one.playtron.GameMode"),
                    "SetFpsLimit",
                    &60u32,
                )
                .await
                .expect("SetFpsLimit over the bus");
        });
    }
}
