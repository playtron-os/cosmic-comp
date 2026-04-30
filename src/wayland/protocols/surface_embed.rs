// SPDX-License-Identifier: GPL-3.0-only

//! Implementation of the COSMIC surface embedding protocol (zcosmic_surface_embed_manager_v1)
//!
//! This protocol allows clients to embed foreign toplevel windows within their own surfaces.
//! The embedded surface can be interactive (receiving input) or display-only.

pub use generated::{zcosmic_embedded_surface_v1, zcosmic_surface_embed_manager_v1};

#[allow(non_snake_case, non_upper_case_globals, non_camel_case_types)]
mod generated {
    use smithay::reexports::wayland_server::{self, protocol::*};

    pub mod __interfaces {
        use smithay::reexports::wayland_server::protocol::__interfaces::*;
        use wayland_backend;
        wayland_scanner::generate_interfaces!("resources/protocols/surface_embed.xml");
    }
    use self::__interfaces::*;

    wayland_scanner::generate_server_code!("resources/protocols/surface_embed.xml");
}

use smithay::{
    reexports::wayland_server::{
        Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource, Weak,
        backend::GlobalId, protocol::wl_surface::WlSurface,
    },
    utils::{IsAlive, Logical, Rectangle},
    wayland::compositor::{Cacheable, with_states},
};
use std::sync::Mutex;
use tracing::{debug, info, warn};

/// Check if `child_pid` is a direct or indirect child of `parent_pid`
/// by walking up the process tree via /proc/{pid}/stat
pub fn is_child_of_pid(parent_pid: u32, child_pid: u32) -> bool {
    if parent_pid == child_pid {
        return true; // Same process
    }

    let mut current_pid = child_pid;
    let mut visited = std::collections::HashSet::new();

    // Walk up the process tree
    while current_pid != 0 && current_pid != 1 {
        if !visited.insert(current_pid) {
            // Cycle detection - shouldn't happen but be safe
            break;
        }

        // Read /proc/{pid}/stat to get parent PID
        let stat_path = format!("/proc/{}/stat", current_pid);
        let stat_content = match std::fs::read_to_string(&stat_path) {
            Ok(content) => content,
            Err(_) => break, // Process may have exited
        };

        // Parse the stat file - format: pid (comm) state ppid ...
        // The comm field can contain spaces and parentheses, so find the last ')'
        let ppid = if let Some(last_paren) = stat_content.rfind(')') {
            let after_comm = &stat_content[last_paren + 1..];
            let fields: Vec<&str> = after_comm.split_whitespace().collect();
            // fields[0] is state, fields[1] is ppid
            fields.get(1).and_then(|s| s.parse::<u32>().ok())
        } else {
            None
        };

        match ppid {
            Some(ppid) if ppid == parent_pid => return true,
            Some(ppid) => current_pid = ppid,
            None => break,
        }
    }

    false
}

/// Render mode for embedded surfaces
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum EmbedRenderMode {
    /// Live rendering - compositor renders the surface directly
    #[default]
    Live,
    /// Screencopy mode - frames are captured and sent to client
    Screencopy,
}

bitflags::bitflags! {
    /// Anchor edges for positioning embedded surfaces
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
    pub struct EmbedAnchor: u32 {
        /// No anchoring, use absolute position
        const NONE = 0;
        /// Anchor to top edge
        const TOP = 1;
        /// Anchor to bottom edge
        const BOTTOM = 2;
        /// Anchor to left edge
        const LEFT = 4;
        /// Anchor to right edge
        const RIGHT = 8;
    }
}

/// Anchor-based positioning for embedded surfaces
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EmbedAnchorConfig {
    /// Anchor edges
    pub anchor: EmbedAnchor,
    /// Margin from top edge
    pub margin_top: i32,
    /// Margin from right edge
    pub margin_right: i32,
    /// Margin from bottom edge
    pub margin_bottom: i32,
    /// Margin from left edge
    pub margin_left: i32,
    /// Width (0 to stretch between anchored edges)
    pub width: i32,
    /// Height (0 to stretch between anchored edges)
    pub height: i32,
}

impl EmbedAnchorConfig {
    /// Calculate the actual geometry given the parent size
    pub fn calculate_geometry(
        &self,
        parent_width: i32,
        parent_height: i32,
    ) -> Rectangle<i32, Logical> {
        let x = if self.anchor.contains(EmbedAnchor::LEFT) {
            self.margin_left
        } else if self.anchor.contains(EmbedAnchor::RIGHT) {
            // If only right-anchored, position from right edge
            let w = if self.width > 0 {
                self.width
            } else {
                parent_width - self.margin_left - self.margin_right
            };
            parent_width - self.margin_right - w
        } else {
            // Centered or absolute - use margin_left as x
            self.margin_left
        };

        let y = if self.anchor.contains(EmbedAnchor::TOP) {
            self.margin_top
        } else if self.anchor.contains(EmbedAnchor::BOTTOM) {
            // If only bottom-anchored, position from bottom edge
            let h = if self.height > 0 {
                self.height
            } else {
                parent_height - self.margin_top - self.margin_bottom
            };
            parent_height - self.margin_bottom - h
        } else {
            // Centered or absolute - use margin_top as y
            self.margin_top
        };

        let width = if self.width > 0 {
            self.width
        } else if self.anchor.contains(EmbedAnchor::LEFT)
            && self.anchor.contains(EmbedAnchor::RIGHT)
        {
            // Stretch between left and right
            parent_width - self.margin_left - self.margin_right
        } else {
            // Default to parent width minus margins
            parent_width - self.margin_left - self.margin_right
        };

        let height = if self.height > 0 {
            self.height
        } else if self.anchor.contains(EmbedAnchor::TOP)
            && self.anchor.contains(EmbedAnchor::BOTTOM)
        {
            // Stretch between top and bottom
            parent_height - self.margin_top - self.margin_bottom
        } else {
            // Default to parent height minus margins
            parent_height - self.margin_top - self.margin_bottom
        };

        Rectangle::new((x, y).into(), (width.max(1), height.max(1)).into())
    }
}

/// State for a single embedded surface
#[derive(Debug)]
pub struct EmbeddedSurfaceData<W: Clone> {
    /// The parent surface this is embedded into
    pub parent: Weak<WlSurface>,
    /// The embedded toplevel window
    pub toplevel: Option<W>,
    /// The toplevel identifier string
    pub toplevel_id: String,
    /// Current geometry within parent surface
    pub geometry: Rectangle<i32, Logical>,
    /// Whether input should be routed to the embedded surface
    pub interactive: bool,
    /// Render mode
    pub render_mode: EmbedRenderMode,
    /// Corner radius [top_left, top_right, bottom_right, bottom_left]
    pub corner_radius: [u8; 4],
    /// Anchor configuration (if set, overrides geometry calculation)
    pub anchor_config: Option<EmbedAnchorConfig>,
    /// Pending geometry (before commit)
    pending_geometry: Option<Rectangle<i32, Logical>>,
    /// Pending interactive state
    pending_interactive: Option<bool>,
    /// Pending render mode
    pending_render_mode: Option<EmbedRenderMode>,
    /// Pending corner radius
    pending_corner_radius: Option<[u8; 4]>,
    /// Pending anchor configuration
    pending_anchor_config: Option<Option<EmbedAnchorConfig>>,
    /// Whether the embedded toplevel is still valid
    pub valid: bool,
    /// If waiting for a PID to connect, this holds the target PID
    pub waiting_for_pid: Option<u32>,
    /// Expected app_id for PID-based embeds (for verification)
    pub expected_app_id: Option<String>,
}

impl<W: Clone> EmbeddedSurfaceData<W> {
    /// Create base struct with default values for pending fields
    fn with_defaults(parent: Weak<WlSurface>, toplevel_id: String) -> Self {
        Self {
            parent,
            toplevel: None,
            toplevel_id,
            geometry: Rectangle::new((0, 0).into(), (100, 100).into()),
            interactive: false,
            render_mode: EmbedRenderMode::Live,
            corner_radius: [0, 0, 0, 0],
            anchor_config: None,
            pending_geometry: None,
            pending_interactive: None,
            pending_render_mode: None,
            pending_corner_radius: None,
            pending_anchor_config: None,
            valid: true,
            waiting_for_pid: None,
            expected_app_id: None,
        }
    }

    fn new(parent: Weak<WlSurface>, toplevel: W, toplevel_id: String) -> Self {
        Self {
            toplevel: Some(toplevel),
            ..Self::with_defaults(parent, toplevel_id)
        }
    }

    fn new_inert(parent: Weak<WlSurface>, toplevel_id: String) -> Self {
        Self {
            geometry: Rectangle::default(),
            valid: false,
            ..Self::with_defaults(parent, toplevel_id)
        }
    }

    /// Create a new embed waiting for a PID to connect
    fn new_waiting_for_pid(
        parent: Weak<WlSurface>,
        pid: u32,
        expected_app_id: Option<String>,
    ) -> Self {
        Self {
            toplevel_id: format!("pid:{}", pid),
            waiting_for_pid: Some(pid),
            expected_app_id,
            ..Self::with_defaults(parent, String::new())
        }
    }

    /// Attach a toplevel to this embed (called when PID matches)
    pub fn attach_toplevel(&mut self, toplevel: W, toplevel_id: String) {
        self.toplevel = Some(toplevel);
        self.toplevel_id = toplevel_id;
        self.waiting_for_pid = None;
    }

    fn commit(&mut self) {
        if let Some(geometry) = self.pending_geometry.take() {
            self.geometry = geometry;
        }
        if let Some(interactive) = self.pending_interactive.take() {
            self.interactive = interactive;
        }
        if let Some(render_mode) = self.pending_render_mode.take() {
            self.render_mode = render_mode;
        }
        if let Some(corner_radius) = self.pending_corner_radius.take() {
            self.corner_radius = corner_radius;
        }
        if let Some(anchor_config) = self.pending_anchor_config.take() {
            self.anchor_config = anchor_config;
        }
    }

    /// Calculate geometry given parent size, using anchor config if set
    pub fn calculate_geometry(
        &self,
        parent_width: i32,
        parent_height: i32,
    ) -> Rectangle<i32, Logical> {
        if let Some(ref config) = self.anchor_config {
            config.calculate_geometry(parent_width, parent_height)
        } else {
            self.geometry
        }
    }
}

/// Global state for embedded surfaces
pub type EmbeddedSurfaceState<W> = Mutex<EmbeddedSurfaceData<W>>;

/// Data stored per-surface tracking all embeds on that surface
#[derive(Debug, Default)]
pub struct SurfaceEmbedData {
    /// List of embedded surfaces on this parent surface
    pub embeds:
        std::cell::RefCell<Vec<Weak<zcosmic_embedded_surface_v1::ZcosmicEmbeddedSurfaceV1>>>,
}

impl Cacheable for SurfaceEmbedData {
    fn commit(&mut self, _dh: &DisplayHandle) -> Self {
        // Clean up dead references
        self.embeds.borrow_mut().retain(|e| e.upgrade().is_ok());
        self.clone()
    }

    fn merge_into(self, into: &mut Self, _dh: &DisplayHandle) {
        *into.embeds.borrow_mut() = self.embeds.into_inner();
    }
}

impl Clone for SurfaceEmbedData {
    fn clone(&self) -> Self {
        Self {
            embeds: std::cell::RefCell::new(self.embeds.borrow().clone()),
        }
    }
}

/// Manager state for the surface embed protocol
#[derive(Debug)]
pub struct SurfaceEmbedManagerState {
    global: GlobalId,
}

impl SurfaceEmbedManagerState {
    /// Create a new surface embed manager global
    pub fn new<D, W>(dh: &DisplayHandle) -> SurfaceEmbedManagerState
    where
        D: GlobalDispatch<
                zcosmic_surface_embed_manager_v1::ZcosmicSurfaceEmbedManagerV1,
                SurfaceEmbedManagerGlobalData,
            > + Dispatch<zcosmic_surface_embed_manager_v1::ZcosmicSurfaceEmbedManagerV1, ()>
            + Dispatch<zcosmic_embedded_surface_v1::ZcosmicEmbeddedSurfaceV1, EmbeddedSurfaceState<W>>
            + SurfaceEmbedHandler<Window = W>
            + 'static,
        W: Clone + Send + 'static,
    {
        let global = dh
            .create_global::<D, zcosmic_surface_embed_manager_v1::ZcosmicSurfaceEmbedManagerV1, _>(
                1,
                SurfaceEmbedManagerGlobalData {
                    filter: Box::new(|_| true),
                },
            );
        SurfaceEmbedManagerState { global }
    }

    /// Create a new surface embed manager global with a client filter
    pub fn new_with_filter<D, W, F>(dh: &DisplayHandle, filter: F) -> SurfaceEmbedManagerState
    where
        D: GlobalDispatch<
                zcosmic_surface_embed_manager_v1::ZcosmicSurfaceEmbedManagerV1,
                SurfaceEmbedManagerGlobalData,
            > + Dispatch<zcosmic_surface_embed_manager_v1::ZcosmicSurfaceEmbedManagerV1, ()>
            + Dispatch<zcosmic_embedded_surface_v1::ZcosmicEmbeddedSurfaceV1, EmbeddedSurfaceState<W>>
            + SurfaceEmbedHandler<Window = W>
            + 'static,
        W: Clone + Send + 'static,
        F: for<'a> Fn(&'a Client) -> bool + Send + Sync + 'static,
    {
        let global = dh
            .create_global::<D, zcosmic_surface_embed_manager_v1::ZcosmicSurfaceEmbedManagerV1, _>(
                1,
                SurfaceEmbedManagerGlobalData {
                    filter: Box::new(filter),
                },
            );
        SurfaceEmbedManagerState { global }
    }

    /// Get the global ID
    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }
}

/// Global data for the surface embed manager
pub struct SurfaceEmbedManagerGlobalData {
    filter: Box<dyn for<'a> Fn(&'a Client) -> bool + Send + Sync>,
}

impl std::fmt::Debug for SurfaceEmbedManagerGlobalData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SurfaceEmbedManagerGlobalData")
            .field("filter", &"<fn>")
            .finish()
    }
}

/// Handler trait for surface embedding
pub trait SurfaceEmbedHandler: Sized {
    /// The window type used by the compositor
    type Window: Clone + Send + IsAlive + 'static;

    /// Get the surface embed manager state
    fn surface_embed_state(&mut self) -> &mut SurfaceEmbedManagerState;

    /// Resolve a toplevel identifier string to a window
    fn window_from_toplevel_id(&self, toplevel_id: &str) -> Option<Self::Window>;

    /// Find a window by its client's PID
    /// Returns (window, toplevel_id) if found
    fn window_from_pid(
        &self,
        pid: u32,
        expected_app_id: Option<&str>,
    ) -> Option<(Self::Window, String)> {
        let _ = (pid, expected_app_id);
        None
    }

    /// Register a pending PID-based embed request
    /// The compositor should call `check_pending_pid_embeds` when new clients connect
    fn register_pending_pid_embed(
        &mut self,
        pid: u32,
        expected_app_id: Option<&str>,
        embed: zcosmic_embedded_surface_v1::ZcosmicEmbeddedSurfaceV1,
    ) {
        let _ = (pid, expected_app_id, embed);
    }

    /// Called when an embed is created
    fn embed_created(
        &mut self,
        parent: &WlSurface,
        toplevel: &Self::Window,
        embed: &zcosmic_embedded_surface_v1::ZcosmicEmbeddedSurfaceV1,
    ) {
        let _ = (parent, toplevel, embed);
    }

    /// Called when an embed geometry or corner radius changes
    fn embed_geometry_changed(
        &mut self,
        parent: &WlSurface,
        toplevel: &Self::Window,
        geometry: Rectangle<i32, Logical>,
        corner_radius: [u8; 4],
        anchor_config: Option<EmbedAnchorConfig>,
        embed: &zcosmic_embedded_surface_v1::ZcosmicEmbeddedSurfaceV1,
    ) {
        let _ = (
            parent,
            toplevel,
            geometry,
            corner_radius,
            anchor_config,
            embed,
        );
    }

    /// Called when an embed is destroyed
    fn embed_destroyed(&mut self, parent: &WlSurface, toplevel: &Self::Window) {
        let _ = (parent, toplevel);
    }
}

impl<D, W>
    GlobalDispatch<
        zcosmic_surface_embed_manager_v1::ZcosmicSurfaceEmbedManagerV1,
        SurfaceEmbedManagerGlobalData,
        D,
    > for SurfaceEmbedManagerState
where
    D: GlobalDispatch<
            zcosmic_surface_embed_manager_v1::ZcosmicSurfaceEmbedManagerV1,
            SurfaceEmbedManagerGlobalData,
        > + Dispatch<zcosmic_surface_embed_manager_v1::ZcosmicSurfaceEmbedManagerV1, ()>
        + Dispatch<zcosmic_embedded_surface_v1::ZcosmicEmbeddedSurfaceV1, EmbeddedSurfaceState<W>>
        + SurfaceEmbedHandler<Window = W>
        + 'static,
    W: Clone + Send + 'static,
{
    fn bind(
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<zcosmic_surface_embed_manager_v1::ZcosmicSurfaceEmbedManagerV1>,
        _global_data: &SurfaceEmbedManagerGlobalData,
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }

    fn can_view(client: Client, global_data: &SurfaceEmbedManagerGlobalData) -> bool {
        (global_data.filter)(&client)
    }
}

impl<D, W> Dispatch<zcosmic_surface_embed_manager_v1::ZcosmicSurfaceEmbedManagerV1, (), D>
    for SurfaceEmbedManagerState
where
    D: GlobalDispatch<
            zcosmic_surface_embed_manager_v1::ZcosmicSurfaceEmbedManagerV1,
            SurfaceEmbedManagerGlobalData,
        > + Dispatch<zcosmic_surface_embed_manager_v1::ZcosmicSurfaceEmbedManagerV1, ()>
        + Dispatch<zcosmic_embedded_surface_v1::ZcosmicEmbeddedSurfaceV1, EmbeddedSurfaceState<W>>
        + SurfaceEmbedHandler<Window = W>
        + 'static,
    W: Clone + Send + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        _resource: &zcosmic_surface_embed_manager_v1::ZcosmicSurfaceEmbedManagerV1,
        request: zcosmic_surface_embed_manager_v1::Request,
        _data: &(),
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_surface_embed_manager_v1::Request::Destroy => {
                // Nothing to do, the object is destroyed
            }
            zcosmic_surface_embed_manager_v1::Request::EmbedToplevel {
                id,
                parent,
                toplevel_id,
            } => {
                // Resolve the toplevel ID to a window
                let window = match state.window_from_toplevel_id(&toplevel_id) {
                    Some(w) => w,
                    None => {
                        warn!("embed_toplevel: invalid toplevel id '{}'", toplevel_id);
                        // Create an inert object
                        let embedded_data =
                            EmbeddedSurfaceData::new_inert(parent.downgrade(), toplevel_id);
                        data_init.init(id, Mutex::new(embedded_data));
                        return;
                    }
                };

                debug!(
                    "Creating embedded surface for toplevel '{}' in parent {:?}",
                    toplevel_id,
                    parent.id()
                );

                let embedded_data =
                    EmbeddedSurfaceData::new(parent.downgrade(), window.clone(), toplevel_id);
                let embed = data_init.init(id, Mutex::new(embedded_data));

                // Register the embed on the parent surface
                with_states(&parent, |states| {
                    let data = states.data_map.get_or_insert(SurfaceEmbedData::default);
                    data.embeds.borrow_mut().push(embed.downgrade());
                });

                // Notify handler
                state.embed_created(&parent, &window, &embed);
            }
            zcosmic_surface_embed_manager_v1::Request::EmbedToplevelByPid {
                id,
                parent,
                pid,
                app_id,
            } => {
                info!(
                    "embed_toplevel_by_pid: request to embed PID {} (app_id hint: '{}') in parent {:?}",
                    pid,
                    app_id,
                    parent.id()
                );

                // Security check: verify the target PID is a child of the embedder client's PID
                let embedder_pid = _client
                    .get_credentials(_dhandle)
                    .ok()
                    .map(|creds| creds.pid as u32);

                if let Some(embedder_pid) = embedder_pid {
                    if !is_child_of_pid(embedder_pid, pid) {
                        warn!(
                            "embed_toplevel_by_pid: DENIED - PID {} is not a child of embedder PID {}",
                            pid, embedder_pid
                        );
                        // Create an inert object - not allowed to embed non-child processes
                        let embedded_data = EmbeddedSurfaceData::<W>::new_inert(
                            parent.downgrade(),
                            format!("denied:pid:{}", pid),
                        );
                        data_init.init(id, Mutex::new(embedded_data));
                        return;
                    }
                    info!(
                        "embed_toplevel_by_pid: ALLOWED - PID {} is a child of embedder PID {}",
                        pid, embedder_pid
                    );
                } else {
                    warn!("embed_toplevel_by_pid: Could not get embedder client credentials");
                    // Deny if we can't verify
                    let embedded_data = EmbeddedSurfaceData::<W>::new_inert(
                        parent.downgrade(),
                        format!("denied:no-creds:{}", pid),
                    );
                    data_init.init(id, Mutex::new(embedded_data));
                    return;
                }

                let expected_app_id = if app_id.is_empty() {
                    None
                } else {
                    Some(app_id.as_str())
                };

                // First, check if a window from this PID already exists
                if let Some((window, toplevel_id)) = state.window_from_pid(pid, expected_app_id) {
                    info!("Found existing window for PID {}: '{}'", pid, toplevel_id);

                    let embedded_data =
                        EmbeddedSurfaceData::new(parent.downgrade(), window.clone(), toplevel_id);
                    let embed = data_init.init(id, Mutex::new(embedded_data));

                    // Register the embed on the parent surface
                    with_states(&parent, |states| {
                        let data = states.data_map.get_or_insert(SurfaceEmbedData::default);
                        data.embeds.borrow_mut().push(embed.downgrade());
                    });

                    // Notify handler
                    state.embed_created(&parent, &window, &embed);
                } else {
                    info!("No window yet for PID {}, creating pending embed", pid);

                    // No window yet, create a pending embed that will be fulfilled later
                    let expected_app_id_owned = if app_id.is_empty() {
                        None
                    } else {
                        Some(app_id.clone())
                    };

                    let embedded_data = EmbeddedSurfaceData::<W>::new_waiting_for_pid(
                        parent.downgrade(),
                        pid,
                        expected_app_id_owned,
                    );
                    let embed = data_init.init(id, Mutex::new(embedded_data));

                    // Register the embed on the parent surface
                    with_states(&parent, |states| {
                        let data = states.data_map.get_or_insert(SurfaceEmbedData::default);
                        data.embeds.borrow_mut().push(embed.downgrade());
                    });

                    // Register for later fulfillment
                    let expected_app_id_ref = if app_id.is_empty() {
                        None
                    } else {
                        Some(app_id.as_str())
                    };
                    state.register_pending_pid_embed(pid, expected_app_id_ref, embed);
                }
            }
        }
    }
}

impl<D, W>
    Dispatch<zcosmic_embedded_surface_v1::ZcosmicEmbeddedSurfaceV1, EmbeddedSurfaceState<W>, D>
    for SurfaceEmbedManagerState
where
    D: GlobalDispatch<
            zcosmic_surface_embed_manager_v1::ZcosmicSurfaceEmbedManagerV1,
            SurfaceEmbedManagerGlobalData,
        > + Dispatch<zcosmic_surface_embed_manager_v1::ZcosmicSurfaceEmbedManagerV1, ()>
        + Dispatch<zcosmic_embedded_surface_v1::ZcosmicEmbeddedSurfaceV1, EmbeddedSurfaceState<W>>
        + SurfaceEmbedHandler<Window = W>
        + 'static,
    W: Clone + Send + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        resource: &zcosmic_embedded_surface_v1::ZcosmicEmbeddedSurfaceV1,
        request: zcosmic_embedded_surface_v1::Request,
        data: &EmbeddedSurfaceState<W>,
        _dhandle: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        let mut embed_data = data.lock().unwrap();

        if !embed_data.valid {
            return;
        }

        match request {
            zcosmic_embedded_surface_v1::Request::Destroy => {
                // Notify handler if we have valid data
                if let (Ok(parent), Some(_toplevel)) =
                    (embed_data.parent.upgrade(), embed_data.toplevel.as_ref())
                {
                    // Clean up the embed from the parent surface
                    with_states(&parent, |states| {
                        if let Some(data) = states.data_map.get::<SurfaceEmbedData>() {
                            data.embeds
                                .borrow_mut()
                                .retain(|e| e.upgrade().map(|e| &e != resource).unwrap_or(false));
                        }
                    });

                    drop(embed_data);
                    let embed_data = data.lock().unwrap();
                    if let Some(toplevel) = embed_data.toplevel.as_ref() {
                        state.embed_destroyed(&parent, toplevel);
                    }
                }
            }
            zcosmic_embedded_surface_v1::Request::SetGeometry {
                x,
                y,
                width,
                height,
            } => {
                if width <= 0 || height <= 0 {
                    resource.post_error(
                        zcosmic_embedded_surface_v1::Error::InvalidGeometry,
                        "width and height must be positive",
                    );
                    return;
                }
                embed_data.pending_geometry =
                    Some(Rectangle::new((x, y).into(), (width, height).into()));
            }
            zcosmic_embedded_surface_v1::Request::SetInteractive { interactive } => {
                embed_data.pending_interactive = Some(interactive != 0);
            }
            zcosmic_embedded_surface_v1::Request::SetRenderMode { mode } => {
                embed_data.pending_render_mode = Some(match mode {
                    0 => EmbedRenderMode::Live,
                    1 => EmbedRenderMode::Screencopy,
                    _ => EmbedRenderMode::Live,
                });
            }
            zcosmic_embedded_surface_v1::Request::SetCornerRadius {
                top_left,
                top_right,
                bottom_right,
                bottom_left,
            } => {
                // Clamp to u8 range
                let tl = top_left.min(255) as u8;
                let tr = top_right.min(255) as u8;
                let br = bottom_right.min(255) as u8;
                let bl = bottom_left.min(255) as u8;
                embed_data.pending_corner_radius = Some([tl, tr, br, bl]);
            }
            zcosmic_embedded_surface_v1::Request::SetAnchor {
                anchor,
                margin_top,
                margin_right,
                margin_bottom,
                margin_left,
                width,
                height,
            } => {
                // Convert WEnum<Anchor> to raw u32 value
                let anchor_value: u32 = anchor.into();
                let anchor_flags = EmbedAnchor::from_bits_truncate(anchor_value);
                embed_data.pending_anchor_config = Some(Some(EmbedAnchorConfig {
                    anchor: anchor_flags,
                    margin_top,
                    margin_right,
                    margin_bottom,
                    margin_left,
                    width,
                    height,
                }));
            }
            zcosmic_embedded_surface_v1::Request::Commit => {
                let old_geometry = embed_data.geometry;
                let old_corner_radius = embed_data.corner_radius;
                let old_anchor_config = embed_data.anchor_config;
                embed_data.commit();

                // Notify handler if geometry, corner_radius, or anchor changed
                if (old_geometry != embed_data.geometry
                    || old_corner_radius != embed_data.corner_radius
                    || old_anchor_config != embed_data.anchor_config)
                    && let (Ok(parent), Some(toplevel)) =
                        (embed_data.parent.upgrade(), embed_data.toplevel.clone())
                {
                    let geometry = embed_data.geometry;
                    let corner_radius = embed_data.corner_radius;
                    let anchor_config = embed_data.anchor_config;
                    drop(embed_data);
                    state.embed_geometry_changed(
                        &parent,
                        &toplevel,
                        geometry,
                        corner_radius,
                        anchor_config,
                        resource,
                    );
                }
            }
        }
    }

    fn destroyed(
        state: &mut D,
        _client: wayland_backend::server::ClientId,
        resource: &zcosmic_embedded_surface_v1::ZcosmicEmbeddedSurfaceV1,
        data: &EmbeddedSurfaceState<W>,
    ) {
        let embed_data = data.lock().unwrap();
        if let (Ok(parent), Some(toplevel)) =
            (embed_data.parent.upgrade(), embed_data.toplevel.as_ref())
        {
            // Clean up the embed from the parent surface
            with_states(&parent, |states| {
                if let Some(data) = states.data_map.get::<SurfaceEmbedData>() {
                    data.embeds
                        .borrow_mut()
                        .retain(|e| e.upgrade().map(|e| &e != resource).unwrap_or(false));
                }
            });

            state.embed_destroyed(&parent, toplevel);
        }
    }
}

/// Helper to get embedded surfaces for a parent surface
pub fn embedded_surfaces_for(
    surface: &WlSurface,
) -> Vec<zcosmic_embedded_surface_v1::ZcosmicEmbeddedSurfaceV1> {
    with_states(surface, |states| {
        states
            .data_map
            .get::<SurfaceEmbedData>()
            .map(|data| {
                data.embeds
                    .borrow()
                    .iter()
                    .filter_map(|e| e.upgrade().ok())
                    .collect()
            })
            .unwrap_or_default()
    })
}

/// Macro to delegate the surface embed protocol
#[macro_export]
macro_rules! delegate_surface_embed {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty, $window: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::surface_embed::zcosmic_surface_embed_manager_v1::ZcosmicSurfaceEmbedManagerV1: $crate::wayland::protocols::surface_embed::SurfaceEmbedManagerGlobalData
        ] => $crate::wayland::protocols::surface_embed::SurfaceEmbedManagerState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::surface_embed::zcosmic_surface_embed_manager_v1::ZcosmicSurfaceEmbedManagerV1: ()
        ] => $crate::wayland::protocols::surface_embed::SurfaceEmbedManagerState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::surface_embed::zcosmic_embedded_surface_v1::ZcosmicEmbeddedSurfaceV1: $crate::wayland::protocols::surface_embed::EmbeddedSurfaceState<$window>
        ] => $crate::wayland::protocols::surface_embed::SurfaceEmbedManagerState);
    };
}

pub(crate) use delegate_surface_embed;
