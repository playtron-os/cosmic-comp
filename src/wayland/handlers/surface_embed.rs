// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    shell::CosmicSurface,
    state::State,
    wayland::protocols::surface_embed::{
        EmbedAnchorConfig, SurfaceEmbedHandler, SurfaceEmbedManagerState, delegate_surface_embed,
        zcosmic_embedded_surface_v1,
    },
};
use smithay::{
    desktop::space::SpaceElement,
    reexports::wayland_server::{Resource, protocol::wl_surface::WlSurface},
    utils::{Logical, Rectangle, Size},
    wayland::seat::WaylandFocus,
};
use std::collections::HashMap;
use std::sync::{LazyLock, RwLock};
use tracing::{debug, info};

/// Embed info for rendering: parent app_id and geometry offset within parent
#[derive(Clone, Debug)]
pub struct EmbedRenderInfo {
    /// The app_id of the parent window
    pub parent_app_id: String,
    /// The unique surface ID of the parent (ObjectId as string)
    pub parent_surface_id: String,
    /// The app_id of the embedded window (for logging/debugging)
    pub embedded_app_id: String,
    /// The geometry within the parent where this window should be rendered
    pub geometry: Rectangle<i32, Logical>,
    /// Corner radius [top_left, top_right, bottom_right, bottom_left]
    pub corner_radius: [u8; 4],
    /// Anchor configuration (if set, geometry is calculated from parent size)
    pub anchor_config: Option<EmbedAnchorConfig>,
}

/// State for tracking embedded window animation frame buffering.
/// This enables frame-ahead buffering: we configure the embedded window ahead of
/// the current animation frame based on measured commit latency, ensuring the
/// embedded buffer is ready when we need to render it.
#[derive(Clone, Debug)]
pub struct EmbedAnimationSync {
    /// The size we last configured the embedded window to
    pub configured_size: Size<i32, Logical>,
    /// When we sent the last configure
    pub configure_time: std::time::Instant,
    /// The size of the last committed buffer from embedded
    pub committed_size: Option<Size<i32, Logical>>,
    /// When the last commit was received
    pub commit_time: Option<std::time::Instant>,
    /// Estimated latency from configure to commit (rolling average)
    pub estimated_latency: std::time::Duration,
    /// Number of samples in the latency estimate
    pub latency_samples: u32,
}

impl EmbedAnimationSync {
    pub fn new(initial_size: Size<i32, Logical>) -> Self {
        Self {
            configured_size: initial_size,
            configure_time: std::time::Instant::now(),
            committed_size: None,
            commit_time: None,
            // Start with a reasonable default latency (~16ms = 1 frame at 60fps)
            estimated_latency: std::time::Duration::from_millis(16),
            latency_samples: 0,
        }
    }

    /// Update the latency estimate when a commit is received
    pub fn record_commit(&mut self, committed_size: Size<i32, Logical>) {
        let now = std::time::Instant::now();
        let latency = now.duration_since(self.configure_time);

        // Update rolling average (exponential moving average)
        if self.latency_samples == 0 {
            self.estimated_latency = latency;
        } else {
            // Weight new samples more heavily initially, then stabilize
            let alpha = if self.latency_samples < 10 { 0.5 } else { 0.2 };
            let new_nanos = (latency.as_nanos() as f64 * alpha
                + self.estimated_latency.as_nanos() as f64 * (1.0 - alpha))
                as u64;
            self.estimated_latency = std::time::Duration::from_nanos(new_nanos);
        }
        self.latency_samples = self.latency_samples.saturating_add(1);

        self.committed_size = Some(committed_size);
        self.commit_time = Some(now);
    }

    /// Record that we sent a new configure
    pub fn record_configure(&mut self, size: Size<i32, Logical>) {
        self.configured_size = size;
        self.configure_time = std::time::Instant::now();
    }
}

/// Global map of embedded surface IDs -> pending animation sync state
/// Key is the WlSurface ObjectId of the EMBEDDED window (as string)
pub static EMBED_ANIMATION_SYNC: LazyLock<RwLock<HashMap<String, EmbedAnimationSync>>> =
    LazyLock::new(|| RwLock::new(HashMap::new()));

/// Global map of embedded surface IDs -> render info (for checking during render without state access)
/// Key is the WlSurface ObjectId of the EMBEDDED window (as string), not the app_id
pub static EMBEDDED_APP_IDS: LazyLock<RwLock<HashMap<String, EmbedRenderInfo>>> =
    LazyLock::new(|| RwLock::new(HashMap::new()));

/// Set of PIDs that have a pending embed request.
/// Windows from these PIDs should be hidden until the embed is fulfilled.
/// This prevents the "flash" of the window appearing before embedding.
pub static PIDS_PENDING_EMBED: LazyLock<RwLock<std::collections::HashSet<u32>>> =
    LazyLock::new(|| RwLock::new(std::collections::HashSet::new()));

/// Set of surface IDs (WlSurface ObjectId as string) that are pending embed.
/// These specific surfaces should be hidden until the embed is fulfilled.
/// This is more precise than app_id - each surface ID is unique.
pub static SURFACE_IDS_PENDING_EMBED: LazyLock<RwLock<std::collections::HashSet<String>>> =
    LazyLock::new(|| RwLock::new(std::collections::HashSet::new()));

/// Set of parent surface IDs that are currently being grabbed (move/resize).
/// These should NOT be considered "orphaned" in cleanup_orphaned_embeds even if
/// they're not in space.elements() (because they're in the grab state).
pub static GRABBED_PARENT_SURFACE_IDS: LazyLock<RwLock<std::collections::HashSet<String>>> =
    LazyLock::new(|| RwLock::new(std::collections::HashSet::new()));

/// Mark a parent surface as being grabbed (protected from orphan cleanup)
pub fn mark_parent_grabbed(parent_surface_id: &str) {
    if let Ok(mut set) = GRABBED_PARENT_SURFACE_IDS.write() {
        set.insert(parent_surface_id.to_string());
        tracing::debug!(
            parent_surface_id = parent_surface_id,
            "Marked parent as grabbed (protected from orphan cleanup)"
        );
    }
}

/// Unmark a parent surface as being grabbed
pub fn unmark_parent_grabbed(parent_surface_id: &str) {
    if let Ok(mut set) = GRABBED_PARENT_SURFACE_IDS.write() {
        set.remove(parent_surface_id);
        tracing::debug!(
            parent_surface_id = parent_surface_id,
            "Unmarked parent as grabbed"
        );
    }
}

/// Check if a parent surface is currently being grabbed
pub fn is_parent_grabbed(parent_surface_id: &str) -> bool {
    GRABBED_PARENT_SURFACE_IDS
        .read()
        .map(|set| set.contains(parent_surface_id))
        .unwrap_or(false)
}

/// Mark a PID as having a pending embed (window should be hidden until embedded)
pub fn mark_pid_pending_embed(pid: u32) {
    if let Ok(mut set) = PIDS_PENDING_EMBED.write() {
        set.insert(pid);
        tracing::info!(
            pid = pid,
            "Marked PID as pending embed (will hide until embedded)"
        );
    }
}

/// Remove a PID from the pending embed set (window can now be shown)
pub fn unmark_pid_pending_embed(pid: u32) {
    if let Ok(mut set) = PIDS_PENDING_EMBED.write() {
        if set.remove(&pid) {
            tracing::info!(pid = pid, "Unmarked PID from pending embed (now visible)");
        }
    }
}

/// Check if a PID has a pending embed (should be hidden)
pub fn is_pid_pending_embed(pid: u32) -> bool {
    PIDS_PENDING_EMBED
        .read()
        .map(|set| set.contains(&pid))
        .unwrap_or(false)
}

/// Mark a surface ID as having a pending embed (window should be hidden until embedded)
/// This is called when we find a window matching a pending PID embed, before it renders.
pub fn mark_surface_id_pending_embed(surface_id: &str) {
    if surface_id.is_empty() {
        return;
    }
    if let Ok(mut set) = SURFACE_IDS_PENDING_EMBED.write() {
        set.insert(surface_id.to_string());
        tracing::info!(
            surface_id = surface_id,
            "Marked surface as pending embed (will hide until embedded)"
        );
    }
}

/// Remove a surface ID from the pending embed set (window can now be shown)
pub fn unmark_surface_id_pending_embed(surface_id: &str) {
    if let Ok(mut set) = SURFACE_IDS_PENDING_EMBED.write() {
        if set.remove(surface_id) {
            tracing::info!(
                surface_id = surface_id,
                "Unmarked surface from pending embed (now visible)"
            );
        }
    }
}

/// Check if a surface ID has a pending embed (should be hidden)
pub fn is_surface_id_pending_embed(surface_id: &str) -> bool {
    SURFACE_IDS_PENDING_EMBED
        .read()
        .map(|set| set.contains(surface_id))
        .unwrap_or(false)
}

/// Check if a parent surface ID is a valid embed parent (has registered children in global registry)
/// This is used to avoid orphaning embeds when the parent is on a different output
pub fn is_valid_embed_parent(parent_surface_id: &str) -> bool {
    EMBEDDED_APP_IDS
        .read()
        .map(|map| {
            map.values()
                .any(|info| info.parent_surface_id == parent_surface_id)
        })
        .unwrap_or(false)
}

/// State stored per-toplevel for embed rectangles (stored on WlSurface data_map)
#[derive(Default, Debug)]
pub struct EmbedToplevelState {
    pub rectangles: Vec<(
        smithay::reexports::wayland_server::Weak<WlSurface>,
        Rectangle<i32, Logical>,
    )>,
}

impl EmbedToplevelState {
    /// Returns true if this toplevel is currently embedded in at least one parent
    pub fn is_embedded(&self) -> bool {
        self.rectangles
            .iter()
            .any(|(weak, _)| weak.upgrade().is_ok())
    }
}

/// Check if a CosmicSurface is embedded (should be hidden from normal workspace rendering)
/// This uses the global EMBEDDED_APP_IDS map to avoid with_states deadlocks
pub fn is_surface_embedded(surface: &CosmicSurface) -> bool {
    surface
        .wl_surface()
        .and_then(|wl| {
            EMBEDDED_APP_IDS
                .read()
                .ok()
                .map(|map| map.contains_key(&wl.id().to_string()))
        })
        .unwrap_or(false)
}

/// Get the embed render info for a surface (if embedded)
pub fn get_embed_render_info(surface: &CosmicSurface) -> Option<EmbedRenderInfo> {
    let surface_id = surface.wl_surface()?.id().to_string();
    get_embed_render_info_by_id(&surface_id)
}

/// Get the embed render info by surface ID string
pub fn get_embed_render_info_by_id(surface_id: &str) -> Option<EmbedRenderInfo> {
    EMBEDDED_APP_IDS
        .read()
        .ok()
        .and_then(|map| map.get(surface_id).cloned())
}

/// Get the parent surface ID for an embedded surface (if embedded)
/// Returns the WlSurface ObjectId of the parent as a string
pub fn get_parent_surface_id(surface: &CosmicSurface) -> Option<String> {
    get_embed_render_info(surface).map(|info| info.parent_surface_id)
}

/// Check if a WlSurface belongs to an embedded window
/// Returns true if the surface_id is in the EMBEDDED_APP_IDS map
pub fn is_wl_surface_embedded(surface: &WlSurface) -> bool {
    let surface_id = surface.id().to_string();
    EMBEDDED_APP_IDS
        .read()
        .ok()
        .map(|map| map.contains_key(&surface_id))
        .unwrap_or(false)
}

/// Mark a surface as embedded with render info
/// Uses the WlSurface ObjectId as the unique key (not app_id)
///
/// Note: corner_radius from the protocol is in format [top_left, top_right, bottom_right, bottom_left]
/// We convert it to cosmic-comp internal format [bottom_right, top_right, bottom_left, top_left]
pub fn mark_surface_embedded(
    surface_id: &str,
    app_id: &str,
    parent_app_id: &str,
    parent_surface_id: &str,
    geometry: Rectangle<i32, Logical>,
    corner_radius: [u8; 4],
    anchor_config: Option<EmbedAnchorConfig>,
) {
    // Convert from protocol format [tl, tr, br, bl] to cosmic-comp format [br, tr, bl, tl]
    let converted_corner_radius = [
        corner_radius[2], // br
        corner_radius[1], // tr
        corner_radius[3], // bl
        corner_radius[0], // tl
    ];

    // Surface is now fully embedded - unmark from pending embed set
    // This allows the surface to be rendered (as an embedded child of parent)
    unmark_surface_id_pending_embed(surface_id);

    if let Ok(mut map) = EMBEDDED_APP_IDS.write() {
        map.insert(
            surface_id.to_string(),
            EmbedRenderInfo {
                parent_app_id: parent_app_id.to_string(),
                parent_surface_id: parent_surface_id.to_string(),
                embedded_app_id: app_id.to_string(),
                geometry,
                corner_radius: converted_corner_radius,
                anchor_config,
            },
        );
        info!(
            "Marked surface_id='{}' (app_id='{}') as embedded in parent='{}' (surface='{}') at {:?} with corner_radius={:?} (converted from {:?}), anchor_config={:?}",
            surface_id,
            app_id,
            parent_app_id,
            parent_surface_id,
            geometry,
            converted_corner_radius,
            corner_radius,
            anchor_config
        );
    }
}

/// Get all embedded windows that have the given parent surface ID
/// Returns Vec<(embedded_surface_id, EmbedRenderInfo)>
pub fn get_children_for_parent_by_surface_id(
    parent_surface_id: &str,
) -> Vec<(String, EmbedRenderInfo)> {
    EMBEDDED_APP_IDS
        .read()
        .ok()
        .map(|map| {
            map.iter()
                .filter(|(_, info)| info.parent_surface_id == parent_surface_id)
                .map(|(surface_id, info)| (surface_id.clone(), info.clone()))
                .collect()
        })
        .unwrap_or_default()
}

/// Update embedded render info for a parent (by surface_id) with new size
/// Returns the updated embedded surface_ids and their new geometries
pub fn update_embedded_geometry_for_parent_by_surface_id(
    parent_surface_id: &str,
    parent_width: i32,
    parent_height: i32,
) -> Vec<(String, Rectangle<i32, Logical>)> {
    let mut updated = Vec::new();
    if let Ok(mut map) = EMBEDDED_APP_IDS.write() {
        for (surface_id, info) in map.iter_mut() {
            if info.parent_surface_id == parent_surface_id {
                if let Some(ref anchor_config) = info.anchor_config {
                    let new_geometry =
                        anchor_config.calculate_geometry(parent_width, parent_height);
                    if new_geometry != info.geometry {
                        info!(
                            "Updating embedded '{}' (surface='{}') geometry: {:?} -> {:?} (parent_surface {} size {}x{})",
                            info.embedded_app_id,
                            surface_id,
                            info.geometry,
                            new_geometry,
                            parent_surface_id,
                            parent_width,
                            parent_height
                        );
                        info.geometry = new_geometry;
                        updated.push((surface_id.clone(), new_geometry));
                    }
                }
            }
        }
    }
    updated
}

/// Unmark a surface as embedded (by surface ID)
pub fn unmark_surface_embedded(surface_id: &str) {
    if let Ok(mut map) = EMBEDDED_APP_IDS.write() {
        if let Some(removed) = map.remove(surface_id) {
            info!(
                "Unmarked surface_id='{}' (app_id='{}') from global map",
                surface_id, removed.embedded_app_id
            );
        }
    }
}

/// Clear all embedded surfaces that belong to a parent surface ID (when parent closes)
pub fn clear_embeds_for_parent_surface(parent_surface_id: &str) -> Vec<String> {
    let mut cleared = Vec::new();
    if let Ok(mut map) = EMBEDDED_APP_IDS.write() {
        let to_remove: Vec<_> = map
            .iter()
            .filter(|(_, info)| info.parent_surface_id == parent_surface_id)
            .map(|(id, info)| (id.clone(), info.embedded_app_id.clone()))
            .collect();

        for (surface_id, app_id) in to_remove {
            map.remove(&surface_id);
            info!(
                "Cleared embedded surface_id='{}' (app_id='{}') because parent surface '{}' closed",
                surface_id, app_id, parent_surface_id
            );
            cleared.push(surface_id);
        }
    }
    cleared
}

// ============================================================================
// Animation Sync Functions
// ============================================================================

/// Start tracking animation sync for an embedded surface.
/// Call this when the parent starts an animation that affects the embedded window.
pub fn start_embed_animation_sync(surface_id: &str, initial_size: Size<i32, Logical>) {
    if let Ok(mut map) = EMBED_ANIMATION_SYNC.write() {
        let sync = EmbedAnimationSync::new(initial_size);
        info!(
            "Started animation sync for embedded surface '{}', initial_size={:?}",
            surface_id, initial_size
        );
        map.insert(surface_id.to_string(), sync);
    }
}

/// Stop tracking animation sync for an embedded surface.
/// Call this when the animation completes.
pub fn stop_embed_animation_sync(surface_id: &str) {
    if let Ok(mut map) = EMBED_ANIMATION_SYNC.write() {
        if map.remove(surface_id).is_some() {
            info!(
                "Stopped animation sync for embedded surface '{}'",
                surface_id
            );
        }
    }
}

/// Record that we configured an embedded surface to a new size during animation.
/// Returns the estimated latency so the caller knows how far ahead to configure.
pub fn record_embed_configure(
    surface_id: &str,
    size: Size<i32, Logical>,
) -> Option<std::time::Duration> {
    if let Ok(mut map) = EMBED_ANIMATION_SYNC.write() {
        if let Some(sync) = map.get_mut(surface_id) {
            sync.record_configure(size);
            return Some(sync.estimated_latency);
        }
    }
    None
}

/// Record that an embedded surface committed a buffer.
/// Call this from the compositor's commit handler when an embedded surface commits.
/// Returns the updated latency estimate.
pub fn record_embed_commit(
    surface_id: &str,
    committed_size: Size<i32, Logical>,
) -> Option<std::time::Duration> {
    if let Ok(mut map) = EMBED_ANIMATION_SYNC.write() {
        if let Some(sync) = map.get_mut(surface_id) {
            sync.record_commit(committed_size);
            debug!(
                "Embedded '{}' committed size {:?}, latency estimate: {:?}",
                surface_id, committed_size, sync.estimated_latency
            );
            return Some(sync.estimated_latency);
        }
    }
    None
}

/// Get the animation sync state for an embedded surface.
pub fn get_embed_animation_sync(surface_id: &str) -> Option<EmbedAnimationSync> {
    EMBED_ANIMATION_SYNC
        .read()
        .ok()
        .and_then(|map| map.get(surface_id).cloned())
}

/// Check if an embedded surface has animation sync active.
pub fn has_embed_animation_sync(surface_id: &str) -> bool {
    EMBED_ANIMATION_SYNC
        .read()
        .ok()
        .map(|map| map.contains_key(surface_id))
        .unwrap_or(false)
}

/// Get all embedded surfaces currently being tracked for animation sync.
pub fn get_all_synced_embeds() -> Vec<String> {
    EMBED_ANIMATION_SYNC
        .read()
        .ok()
        .map(|map| map.keys().cloned().collect())
        .unwrap_or_default()
}

#[derive(Default)]
pub struct PendingPidEmbeds {
    /// Map from PID to pending embed objects
    pub pending: HashMap<u32, Vec<zcosmic_embedded_surface_v1::ZcosmicEmbeddedSurfaceV1>>,
}

impl SurfaceEmbedHandler for State {
    type Window = CosmicSurface;

    fn surface_embed_state(&mut self) -> &mut SurfaceEmbedManagerState {
        &mut self.common.surface_embed_state
    }

    fn window_from_toplevel_id(&self, toplevel_id: &str) -> Option<Self::Window> {
        // Look through all spaces and windows to find one matching the ID
        // The ID format is the app_id (for now - could be UUID in future)
        let shell = self.common.shell.read();
        for mapped in shell.workspaces.spaces().flat_map(|s| s.mapped()) {
            for (surface, _point) in mapped.windows() {
                if surface.app_id() == toplevel_id || surface.title() == toplevel_id {
                    return Some(surface.clone());
                }
            }
        }
        None
    }

    fn window_from_pid(
        &self,
        pid: u32,
        expected_app_id: Option<&str>,
    ) -> Option<(Self::Window, String)> {
        let shell = self.common.shell.read();

        for mapped in shell.workspaces.spaces().flat_map(|s| s.mapped()) {
            for (surface, _point) in mapped.windows() {
                // Get the WlSurface to check the client's PID
                if let Some(wl_surface) = surface.wl_surface() {
                    // Get the client from the surface
                    if let Ok(client) = self.common.display_handle.get_client(wl_surface.id()) {
                        // Get client credentials (PID, UID, GID)
                        if let Ok(creds) = client.get_credentials(&self.common.display_handle) {
                            if creds.pid as u32 == pid {
                                let app_id = surface.app_id();

                                // If expected_app_id is provided, verify it matches
                                if let Some(expected) = expected_app_id {
                                    if !expected.is_empty() && app_id != expected {
                                        debug!(
                                            "PID {} matches but app_id mismatch: expected '{}', got '{}'",
                                            pid, expected, app_id
                                        );
                                        continue;
                                    }
                                }

                                info!(
                                    "Found window for PID {}: app_id='{}', title='{}'",
                                    pid,
                                    app_id,
                                    surface.title()
                                );
                                return Some((surface.clone(), app_id));
                            }
                        }
                    }
                }
            }
        }

        debug!("No window found for PID {}", pid);
        None
    }

    fn register_pending_pid_embed(
        &mut self,
        pid: u32,
        expected_app_id: Option<&str>,
        embed: zcosmic_embedded_surface_v1::ZcosmicEmbeddedSurfaceV1,
    ) {
        info!(
            "Registering pending PID embed for PID {}, app_id hint: {:?}",
            pid, expected_app_id
        );
        // Mark this PID as pending embed - we'll mark the specific surface ID when window maps
        mark_pid_pending_embed(pid);
        // Note: We don't mark by app_id because multiple windows could have same app_id.
        // Instead, when check_pending_pid_embeds_for_window finds a matching window,
        // we mark that specific surface ID as pending.
        self.common
            .pending_pid_embeds
            .entry(pid)
            .or_default()
            .push(embed);
    }

    fn embed_created(
        &mut self,
        parent: &WlSurface,
        toplevel: &Self::Window,
        embed: &zcosmic_embedded_surface_v1::ZcosmicEmbeddedSurfaceV1,
    ) {
        debug!(
            "Embedded surface created: {:?} in parent {:?}",
            toplevel.title(),
            parent.id()
        );

        // Send initial configure with toplevel's preferred size
        let geometry = toplevel.geometry();
        embed.configure(geometry.size.w, geometry.size.h);

        // If the embedded window is already mapped on a different output than the parent,
        // we need to move it to the parent's output.
        self.move_embedded_to_parent_output(parent, toplevel);
    }

    fn embed_geometry_changed(
        &mut self,
        parent: &WlSurface,
        toplevel: &Self::Window,
        geometry: Rectangle<i32, Logical>,
        corner_radius: [u8; 4],
        anchor_config: Option<EmbedAnchorConfig>,
        embed: &zcosmic_embedded_surface_v1::ZcosmicEmbeddedSurfaceV1,
    ) {
        // Calculate actual geometry - use anchor config if present, otherwise use provided geometry
        let actual_geometry = if let Some(ref config) = anchor_config {
            // Get parent window size to calculate anchor-based geometry
            if let Some(parent_geo) = self.find_window_geometry_for_surface(parent) {
                let calculated = config.calculate_geometry(parent_geo.size.w, parent_geo.size.h);
                info!(
                    "Calculated anchor-based geometry: parent_size={}x{}, margins=[{},{},{},{}] -> {:?}",
                    parent_geo.size.w,
                    parent_geo.size.h,
                    config.margin_top,
                    config.margin_right,
                    config.margin_bottom,
                    config.margin_left,
                    calculated
                );
                calculated
            } else {
                info!("Could not find parent window geometry, falling back to provided geometry");
                geometry
            }
        } else {
            geometry
        };

        info!(
            "Embedded surface geometry changed: app_id='{}' title='{}' -> {:?}, corner_radius={:?}, anchor_config={:?}, configuring and storing",
            toplevel.app_id(),
            toplevel.title(),
            actual_geometry,
            corner_radius,
            anchor_config
        );

        // Resize the toplevel to match the embed geometry size
        // This actually tells the toplevel (e.g., cosmic-term) to resize
        let global_geo = smithay::utils::Rectangle::new(
            (actual_geometry.loc.x, actual_geometry.loc.y).into(),
            (actual_geometry.size.w, actual_geometry.size.h).into(),
        );
        toplevel.set_geometry(global_geo, 0); // Pass 0 for SSD height - we skip SSD rendering for embedded

        // Force send configure to the embedded toplevel so it actually resizes
        // We use force_configure() instead of send_configure() because send_pending_configure()
        // may not send if smithay doesn't detect a difference in pending state.
        // For initial embed sizing, we need to guarantee the configure is sent.
        toplevel.force_configure();

        // Send configure event to the parent client (chat-ui-rs)
        // This tells it the preferred size changed
        embed.configure(actual_geometry.size.w, actual_geometry.size.h);

        // Get unique IDs for both surfaces
        let parent_app_id = self.find_app_id_for_surface(parent).unwrap_or_default();
        let parent_surface_id = parent.id().to_string();
        let app_id = toplevel.app_id();
        let surface_id = toplevel
            .wl_surface()
            .map(|s| s.id().to_string())
            .unwrap_or_default();

        // Store in our global registry keyed by surface_id (unique per window instance)
        self.common.embedded_surfaces.insert(
            surface_id.clone(),
            (embed.clone(), parent.downgrade(), actual_geometry),
        );

        // Also mark in the static map for fast render-time checks
        mark_surface_embedded(
            &surface_id,
            &app_id,
            &parent_app_id,
            &parent_surface_id,
            actual_geometry,
            corner_radius,
            anchor_config,
        );
    }

    fn embed_destroyed(&mut self, parent: &WlSurface, toplevel: &Self::Window) {
        debug!(
            "Embedded surface destroyed: {:?} from parent {:?}",
            toplevel.title(),
            parent.id()
        );

        // Get the unique surface ID
        let surface_id = toplevel
            .wl_surface()
            .map(|s| s.id().to_string())
            .unwrap_or_default();

        // Clean up from global registry
        self.common.embedded_surfaces.remove(&surface_id);

        // Also unmark from static set
        unmark_surface_embedded(&surface_id);
    }
}

impl State {
    /// Move an embedded window to the same output as its parent.
    /// This is called when an embed relationship is created for an already-mapped window
    /// that may be on a different output than its parent.
    fn move_embedded_to_parent_output(
        &mut self,
        parent_surface: &WlSurface,
        embedded: &CosmicSurface,
    ) {
        let parent_surface_id = parent_surface.id().to_string();

        info!(
            "move_embedded_to_parent_output: checking if embedded '{}' needs to move to parent's output",
            embedded.app_id()
        );

        // Get the workspace handles for both parent and embedded
        let (parent_handle, embedded_handle, embedded_mapped) = {
            let shell = self.common.shell.read();

            // Find parent's workspace
            let parent_mapped = shell.element_for_surface_id(&parent_surface_id);
            let parent_workspace = parent_mapped.and_then(|m| shell.space_for(m));
            let parent_handle = parent_workspace.map(|w| w.handle.clone());

            // Find embedded's workspace
            let embedded_mapped = shell.element_for_surface(embedded).cloned();
            let embedded_workspace = embedded_mapped.as_ref().and_then(|m| shell.space_for(m));
            let embedded_handle = embedded_workspace.map(|w| w.handle.clone());

            if let (Some(p), Some(e)) = (&parent_handle, &embedded_handle) {
                info!(
                    "move_embedded_to_parent_output: parent workspace={:?}, embedded workspace={:?}",
                    p, e
                );
            } else {
                info!(
                    "move_embedded_to_parent_output: could not find workspaces - parent={:?}, embedded={:?}",
                    parent_handle.is_some(),
                    embedded_handle.is_some()
                );
            }

            (parent_handle, embedded_handle, embedded_mapped)
        };

        // If they're in different workspaces, move the embedded window
        if let (Some(parent_handle), Some(embedded_handle), Some(embedded_mapped)) =
            (parent_handle, embedded_handle, embedded_mapped)
        {
            if parent_handle != embedded_handle {
                info!(
                    "move_embedded_to_parent_output: moving embedded '{}' from {:?} to {:?}",
                    embedded.app_id(),
                    embedded_handle,
                    parent_handle
                );

                // Get workspace state for the move
                let mut workspace_state = self.common.workspace_state.update();
                let mut shell = self.common.shell.write();

                // Move the element
                let _ = shell.move_element(
                    None, // No seat - we don't want to follow with focus
                    &embedded_mapped,
                    &embedded_handle,
                    &parent_handle,
                    false, // Don't follow (don't change active workspace)
                    None,  // No direction preference
                    &mut workspace_state,
                );

                info!(
                    "move_embedded_to_parent_output: moved embedded '{}' to parent's workspace",
                    embedded.app_id()
                );
            } else {
                info!(
                    "move_embedded_to_parent_output: embedded '{}' already on same workspace as parent",
                    embedded.app_id()
                );
            }
        }
    }

    /// Find the CosmicSurface for a WlSurface by searching through all mapped windows
    fn find_window_for_surface(&self, surface: &WlSurface) -> Option<CosmicSurface> {
        let shell = self.common.shell.read();
        let target_id = surface.id();
        shell
            .workspaces
            .spaces()
            .flat_map(|s| s.mapped())
            .flat_map(|m| m.windows().map(|(w, _)| w))
            .find(|w| w.wl_surface().map(|s| s.id() == target_id).unwrap_or(false))
            .clone()
    }

    /// Find the app_id for a WlSurface by searching through all mapped windows
    fn find_app_id_for_surface(&self, surface: &WlSurface) -> Option<String> {
        self.find_window_for_surface(surface).map(|w| w.app_id())
    }

    /// Find the geometry of a window for a WlSurface
    fn find_window_geometry_for_surface(
        &self,
        surface: &WlSurface,
    ) -> Option<Rectangle<i32, Logical>> {
        self.find_window_for_surface(surface).map(|w| w.geometry())
    }

    /// Check if a newly mapped window matches any pending PID-based embed requests
    /// and fulfill them if so.
    pub fn check_pending_pid_embeds_for_window(&mut self, window: &CosmicSurface) {
        // Get the PID of the window's client
        let wl_surface = match window.wl_surface() {
            Some(s) => s,
            None => return,
        };

        let surface_id = wl_surface.id().to_string();

        let client = match self.common.display_handle.get_client(wl_surface.id()) {
            Ok(c) => c,
            Err(_) => return,
        };

        let creds = match client.get_credentials(&self.common.display_handle) {
            Ok(c) => c,
            Err(_) => return,
        };

        let pid = creds.pid as u32;

        // Check if there are any pending embeds for this PID
        if let Some(pending_embeds) = self.common.pending_pid_embeds.remove(&pid) {
            // IMMEDIATELY mark this specific surface as pending embed to hide it
            // This prevents any render frames showing the window before embed is fulfilled
            mark_surface_id_pending_embed(&surface_id);

            // Unmark PID from pending (we found the window)
            unmark_pid_pending_embed(pid);
            let app_id = window.app_id();
            info!(
                "Fulfilling {} pending embed(s) for PID {} (app_id: {}, surface: {})",
                pending_embeds.len(),
                pid,
                app_id,
                surface_id
            );

            for embed in pending_embeds {
                // Get the embed data to update it with the window
                if let Some(data) = embed
                    .data::<crate::wayland::protocols::surface_embed::EmbeddedSurfaceState<
                    CosmicSurface,
                >>() {
                    let mut embed_data = data.lock().unwrap();

                    // Verify app_id if one was expected
                    if let Some(ref expected) = embed_data.expected_app_id {
                        if !expected.is_empty() && &app_id != expected {
                            info!(
                                "Skipping embed - app_id mismatch: expected '{}', got '{}'",
                                expected, app_id
                            );
                            // Put back the embed for this PID - maybe another window will match
                            drop(embed_data);
                            self.common
                                .pending_pid_embeds
                                .entry(pid)
                                .or_default()
                                .push(embed);
                            continue;
                        }
                    }

                    // Update the embed data with the actual window
                    embed_data.toplevel = Some(window.clone());
                    embed_data.toplevel_id = app_id.clone();
                    embed_data.waiting_for_pid = None;
                    embed_data.valid = true;

                    // Get the parent surface, geometry, and corner_radius for callbacks
                    let parent = embed_data.parent.upgrade();
                    let geometry = embed_data.geometry;
                    let corner_radius = embed_data.corner_radius;
                    let anchor_config = embed_data.anchor_config;

                    drop(embed_data);

                    // Notify handler
                    if let Ok(ref parent) = parent {
                        <Self as SurfaceEmbedHandler>::embed_created(self, parent, window, &embed);
                        // Also notify about geometry so the embed is properly registered
                        // (geometry may have been set before the window appeared)
                        <Self as SurfaceEmbedHandler>::embed_geometry_changed(
                            self,
                            parent,
                            window,
                            geometry,
                            corner_radius,
                            anchor_config,
                            &embed,
                        );
                        info!(
                            "Embed fulfilled for PID {} -> app_id '{}', geometry: {:?}, corner_radius: {:?}, anchor_config: {:?}",
                            pid, app_id, geometry, corner_radius, anchor_config
                        );
                    }
                }
            }
        }
    }
}

delegate_surface_embed!(State, CosmicSurface);
