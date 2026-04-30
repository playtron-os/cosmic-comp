// SPDX-License-Identifier: GPL-3.0-only

//! Implementation of the compositor-driven tooltip protocol (zcosmic_tooltip_manager_v1)
//!
//! This protocol allows clients to create tooltip surfaces that the compositor
//! positions relative to the pointer cursor automatically, eliminating client
//! round-trip latency for tooltip repositioning.

pub use generated::{zcosmic_tooltip_manager_v1, zcosmic_tooltip_v1};

#[allow(non_snake_case, non_upper_case_globals, non_camel_case_types)]
mod generated {
    use smithay::reexports::wayland_server::{self, protocol::*};

    pub mod __interfaces {
        use smithay::reexports::wayland_server::protocol::__interfaces::*;
        use wayland_backend;
        wayland_scanner::generate_interfaces!("resources/protocols/tooltip.xml");
    }
    use self::__interfaces::*;

    wayland_scanner::generate_server_code!("resources/protocols/tooltip.xml");
}

use smithay::{
    reexports::wayland_server::{
        Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource, Weak,
        backend::GlobalId, protocol::wl_surface::WlSurface,
    },
    utils::Point,
    wayland::compositor::with_states,
};
use std::sync::Mutex;

use crate::utils::prelude::Global;
use smithay::utils::Logical;
use wayland_backend::server::ObjectId;

/// Cached pointer state so that newly-registered tooltips can be
/// immediately positioned without waiting for the next motion event.
#[derive(Debug, Clone)]
pub struct CachedCursorState {
    /// Global cursor position (logical coordinates).
    pub position: Point<f64, Global>,
    /// The `ObjectId` of the `WlSurface` currently under the cursor.
    pub surface_id: ObjectId,
    /// Cursor position relative to that surface's origin.
    pub cursor_in_parent: Point<i32, Logical>,
}

/// Direction the tooltip appears relative to the cursor.
///
/// - `TopLeft`     → tooltip above-left  of cursor
/// - `TopRight`    → tooltip above-right of cursor
/// - `BottomLeft`  → tooltip below-left  of cursor
/// - `BottomRight` → tooltip below-right of cursor (default)
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
#[repr(u32)]
pub enum TooltipAnchor {
    TopLeft = 0,
    TopRight = 1,
    BottomLeft = 2,
    #[default]
    BottomRight = 3,
}

impl TooltipAnchor {
    /// Adjust a position so the correct corner of the popup aligns to the
    /// anchor point.
    ///
    /// The anchor describes where the tooltip appears relative to the cursor:
    /// - `BottomRight` → top-left corner at position (no adjustment)
    /// - `BottomLeft`  → top-right corner at position (shift x by −width)
    /// - `TopRight`    → bottom-left corner at position (shift y by −height)
    /// - `TopLeft`     → bottom-right corner at position (shift both)
    pub fn adjust_position(&self, x: &mut i32, y: &mut i32, popup_w: i32, popup_h: i32) {
        match self {
            TooltipAnchor::BottomRight => {}
            TooltipAnchor::BottomLeft => *x -= popup_w,
            TooltipAnchor::TopRight => *y -= popup_h,
            TooltipAnchor::TopLeft => {
                *x -= popup_w;
                *y -= popup_h;
            }
        }
    }
}

impl TryFrom<u32> for TooltipAnchor {
    type Error = ();
    fn try_from(value: u32) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::TopLeft),
            1 => Ok(Self::TopRight),
            2 => Ok(Self::BottomLeft),
            3 => Ok(Self::BottomRight),
            _ => Err(()),
        }
    }
}

/// Data stored per-surface for tooltip position override.
#[derive(Debug, Clone, Copy)]
pub struct TooltipOverrideData {
    /// Global position (used by layer-shell popup rendering path).
    pub position: Point<i32, Global>,
    /// Position relative to the parent surface origin (used by XDG window popup rendering path).
    pub parent_relative: Point<i32, Logical>,
    pub anchor: TooltipAnchor,
    /// When `Some(t)` and `now < t`, the render path hides this popup (show-delay).
    /// When `None` or `now >= t`, the popup renders normally.
    pub show_at: Option<std::time::Instant>,
}

/// Per-surface tooltip position override.
/// When set, the rendering pipeline uses this global position instead of
/// the standard xdg_popup-computed position.
pub struct TooltipPositionOverride(pub Mutex<Option<TooltipOverrideData>>);

/// Get the tooltip position override for a surface, if any.
pub fn get_tooltip_position(surface: &WlSurface) -> Option<TooltipOverrideData> {
    let result = with_states(surface, |states| {
        states
            .data_map
            .get::<TooltipPositionOverride>()
            .and_then(|p| *p.0.lock().unwrap())
    });
    tracing::debug!(
        "[tooltip] get_tooltip_position surface={:?} => {:?}",
        surface.id(),
        result,
    );
    result
}

/// Set the tooltip position override for a surface.
pub fn set_tooltip_position(
    surface: &WlSurface,
    position: Point<i32, Global>,
    parent_relative: Point<i32, Logical>,
    anchor: TooltipAnchor,
    show_at: Option<std::time::Instant>,
) {
    tracing::debug!(
        "[tooltip] set_tooltip_position surface={:?} pos=({},{}) parent_rel=({},{}) anchor={:?} show_at={:?}",
        surface.id(),
        position.x,
        position.y,
        parent_relative.x,
        parent_relative.y,
        anchor,
        show_at,
    );
    with_states(surface, |states| {
        let data = states
            .data_map
            .get_or_insert_threadsafe(|| TooltipPositionOverride(Mutex::new(None)));
        *data.0.lock().unwrap() = Some(TooltipOverrideData {
            position,
            parent_relative,
            anchor,
            show_at,
        });
    });
}

/// Clear the tooltip position override for a surface.
pub fn clear_tooltip_position(surface: &WlSurface) {
    tracing::debug!(
        "[tooltip] clear_tooltip_position surface={:?}",
        surface.id(),
    );
    with_states(surface, |states| {
        if let Some(data) = states.data_map.get::<TooltipPositionOverride>() {
            *data.0.lock().unwrap() = None;
        }
    });
}

/// A registered tooltip: associates a tooltip popup surface with a parent surface and offset.
#[derive(Debug)]
pub struct TooltipRegistration {
    pub tooltip_surface: WlSurface,
    pub parent_surface: WlSurface,
    pub offset_x: i32,
    pub offset_y: i32,
    pub anchor: TooltipAnchor,
    pub show_delay_ms: u32,
    /// Set when `show_delay_ms > 0` and the cursor first enters the parent.
    /// The tooltip position is only committed once this instant is reached.
    pub show_at: Option<std::time::Instant>,
    /// True until the first full positioning pass (after set_offset/set_anchor
    /// have been applied). GetTooltip sets this flag; the motion handler
    /// consumes it once all protocol requests from the same batch have been
    /// dispatched.
    pub pending_initial_position: bool,
    /// True once a delayed tooltip has been shown at a fixed position.
    /// While set, motion events no longer update the tooltip position.
    pub position_committed: bool,
}

/// Data associated with a tooltip protocol object
pub struct TooltipData(pub Mutex<TooltipInternal>);

pub struct TooltipInternal {
    pub tooltip_surface: Weak<WlSurface>,
    pub parent_surface: Weak<WlSurface>,
    pub offset_x: i32,
    pub offset_y: i32,
    pub anchor: TooltipAnchor,
    pub show_delay_ms: u32,
}

/// State for the tooltip manager protocol
#[derive(Debug)]
pub struct TooltipManagerState {
    global: GlobalId,
    /// Active tooltip registrations
    pub registrations: Vec<TooltipRegistration>,
    /// Last known cursor state, updated on every pointer motion event.
    pub cached_cursor: Option<CachedCursorState>,
}

impl TooltipManagerState {
    pub fn new<D>(dh: &DisplayHandle) -> TooltipManagerState
    where
        D: GlobalDispatch<zcosmic_tooltip_manager_v1::ZcosmicTooltipManagerV1, ()>
            + Dispatch<zcosmic_tooltip_manager_v1::ZcosmicTooltipManagerV1, ()>
            + Dispatch<zcosmic_tooltip_v1::ZcosmicTooltipV1, TooltipData>
            + TooltipHandler
            + 'static,
    {
        let global =
            dh.create_global::<D, zcosmic_tooltip_manager_v1::ZcosmicTooltipManagerV1, _>(1, ());
        TooltipManagerState {
            global,
            registrations: Vec::new(),
            cached_cursor: None,
        }
    }

    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }

    /// Update all tooltip positions based on the current cursor state.
    ///
    /// Called from both relative and absolute pointer motion handlers to avoid
    /// duplicating the tooltip positioning logic.
    pub fn update_positions(
        &mut self,
        cursor_surface: Option<&WlSurface>,
        position: Point<f64, Global>,
        cursor_in_parent: Option<Point<i32, Logical>>,
    ) {
        // Prune dead registrations before processing
        self.registrations.retain(|r| r.tooltip_surface.is_alive());

        // Cache cursor state so GetTooltip can immediately position new tooltips
        if let Some(surface) = cursor_surface {
            self.cached_cursor = Some(CachedCursorState {
                position,
                surface_id: surface.id(),
                cursor_in_parent: cursor_in_parent.unwrap_or_else(|| Point::from((0, 0))),
            });
        } else {
            self.cached_cursor = None;
        }

        for reg in &mut self.registrations {
            // Handle deferred initial position: GetTooltip sets this flag;
            // by the time the motion handler runs, set_offset / set_anchor /
            // set_show_delay from the same Wayland request batch have already
            // been applied.
            if reg.pending_initial_position {
                reg.pending_initial_position = false;
                if reg.show_delay_ms > 0 {
                    reg.show_at = Some(
                        std::time::Instant::now()
                            + std::time::Duration::from_millis(reg.show_delay_ms as u64),
                    );
                }
            }

            let pointer_over_parent =
                cursor_surface.is_some_and(|s| s.id() == reg.parent_surface.id());

            if pointer_over_parent {
                // Apply offset in the direction the tooltip expands,
                // so it always pushes AWAY from the cursor.
                let (eff_ox, eff_oy) = match reg.anchor {
                    TooltipAnchor::BottomRight => (reg.offset_x, reg.offset_y),
                    TooltipAnchor::BottomLeft => (-reg.offset_x, reg.offset_y),
                    TooltipAnchor::TopRight => (reg.offset_x, -reg.offset_y),
                    TooltipAnchor::TopLeft => (-reg.offset_x, -reg.offset_y),
                };
                let pos = Point::from((
                    position.x.round() as i32 + eff_ox,
                    position.y.round() as i32 + eff_oy,
                ));
                let parent_rel = cursor_in_parent
                    .map(|cip| Point::from((cip.x + eff_ox, cip.y + eff_oy)))
                    .unwrap_or_else(|| Point::from((0, 0)));

                if reg.show_delay_ms > 0 {
                    // Delayed tooltip: show at fixed position after delay
                    if reg.position_committed {
                        continue;
                    }
                    if let Some(show_at) = reg.show_at {
                        if std::time::Instant::now() < show_at {
                            set_tooltip_position(
                                &reg.tooltip_surface,
                                pos,
                                parent_rel,
                                reg.anchor,
                                Some(show_at),
                            );
                            continue;
                        }
                        // Delay elapsed — lock position
                        reg.show_at = None;
                        reg.position_committed = true;
                        set_tooltip_position(
                            &reg.tooltip_surface,
                            pos,
                            parent_rel,
                            reg.anchor,
                            None,
                        );
                    }
                } else {
                    // Immediate tooltip — follow cursor
                    set_tooltip_position(&reg.tooltip_surface, pos, parent_rel, reg.anchor, None);
                }
            } else {
                // Cursor left parent — reset delay timer and position lock
                reg.position_committed = false;
                if reg.show_delay_ms > 0 {
                    reg.show_at = Some(
                        std::time::Instant::now()
                            + std::time::Duration::from_millis(reg.show_delay_ms as u64),
                    );
                }
                clear_tooltip_position(&reg.tooltip_surface);
            }
        }
    }
}

/// Handler trait for tooltip events
pub trait TooltipHandler {
    fn tooltip_state(&mut self) -> &mut TooltipManagerState;
}

impl<D> GlobalDispatch<zcosmic_tooltip_manager_v1::ZcosmicTooltipManagerV1, (), D>
    for TooltipManagerState
where
    D: GlobalDispatch<zcosmic_tooltip_manager_v1::ZcosmicTooltipManagerV1, ()>
        + Dispatch<zcosmic_tooltip_manager_v1::ZcosmicTooltipManagerV1, ()>
        + Dispatch<zcosmic_tooltip_v1::ZcosmicTooltipV1, TooltipData>
        + TooltipHandler
        + 'static,
{
    fn bind(
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<zcosmic_tooltip_manager_v1::ZcosmicTooltipManagerV1>,
        _global_data: &(),
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }
}

impl<D> Dispatch<zcosmic_tooltip_manager_v1::ZcosmicTooltipManagerV1, (), D> for TooltipManagerState
where
    D: GlobalDispatch<zcosmic_tooltip_manager_v1::ZcosmicTooltipManagerV1, ()>
        + Dispatch<zcosmic_tooltip_manager_v1::ZcosmicTooltipManagerV1, ()>
        + Dispatch<zcosmic_tooltip_v1::ZcosmicTooltipV1, TooltipData>
        + TooltipHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        resource: &zcosmic_tooltip_manager_v1::ZcosmicTooltipManagerV1,
        request: zcosmic_tooltip_manager_v1::Request,
        _data: &(),
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_tooltip_manager_v1::Request::Destroy => {}
            zcosmic_tooltip_manager_v1::Request::GetTooltip {
                id,
                tooltip_surface,
                parent_surface,
            } => {
                // Check if tooltip_surface already has a tooltip object
                let exists = with_states(&tooltip_surface, |surface_data| {
                    surface_data
                        .data_map
                        .get::<TooltipPositionOverride>()
                        .and_then(|p| p.0.lock().unwrap().map(|_| true))
                });
                // Check registrations too
                let tooltip_state = state.tooltip_state();
                let already_registered = tooltip_state
                    .registrations
                    .iter()
                    .any(|r| r.tooltip_surface == tooltip_surface);

                if exists.unwrap_or(false) || already_registered {
                    resource.post_error(
                        zcosmic_tooltip_manager_v1::Error::AlreadyExists as u32,
                        format!(
                            "{resource:?} ZcosmicTooltipV1 object already exists for this surface"
                        ),
                    );
                    return;
                }

                let data = TooltipData(Mutex::new(TooltipInternal {
                    tooltip_surface: tooltip_surface.downgrade(),
                    parent_surface: parent_surface.downgrade(),
                    offset_x: 0,
                    offset_y: 0,
                    anchor: TooltipAnchor::default(),
                    show_delay_ms: 0,
                }));
                data_init.init(id, data);

                // Register
                tracing::debug!(
                    "[tooltip] GetTooltip: registering tooltip_surface={:?} parent_surface={:?}",
                    tooltip_surface.id(),
                    parent_surface.id(),
                );

                let tooltip_state = state.tooltip_state();
                let reg = TooltipRegistration {
                    tooltip_surface,
                    parent_surface,
                    offset_x: 0,
                    offset_y: 0,
                    anchor: TooltipAnchor::default(),
                    show_delay_ms: 0,
                    show_at: None,
                    pending_initial_position: true,
                    position_committed: false,
                };

                // Don't immediately position — set_offset / set_anchor /
                // set_show_delay requests from the same batch haven't been
                // dispatched yet.  The motion handler will pick up
                // `pending_initial_position` and use the cached cursor to
                // position the tooltip with the correct values.
                tracing::debug!(
                    "[tooltip] GetTooltip: deferred initial positioning (pending_initial_position=true)",
                );

                tooltip_state.registrations.push(reg);
            }
        }
    }
}

impl<D> Dispatch<zcosmic_tooltip_v1::ZcosmicTooltipV1, TooltipData, D> for TooltipManagerState
where
    D: GlobalDispatch<zcosmic_tooltip_manager_v1::ZcosmicTooltipManagerV1, ()>
        + Dispatch<zcosmic_tooltip_manager_v1::ZcosmicTooltipManagerV1, ()>
        + Dispatch<zcosmic_tooltip_v1::ZcosmicTooltipV1, TooltipData>
        + TooltipHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        _resource: &zcosmic_tooltip_v1::ZcosmicTooltipV1,
        request: zcosmic_tooltip_v1::Request,
        data: &TooltipData,
        _dhandle: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        let internal = data.0.lock().unwrap();

        match request {
            zcosmic_tooltip_v1::Request::Destroy => {
                // Clear position override and remove registration.
                // We must always remove the registration, even if the surface
                // was already destroyed (weak ref upgrade fails).
                let tooltip_surface_weak = internal.tooltip_surface.clone();
                if let Ok(surface) = tooltip_surface_weak.upgrade() {
                    tracing::debug!(
                        "[tooltip] Destroy: surface {:?} still alive, clearing position",
                        surface.id()
                    );
                    clear_tooltip_position(&surface);
                    let surface_id = surface.id();
                    drop(internal);
                    let tooltip_state = state.tooltip_state();
                    tooltip_state
                        .registrations
                        .retain(|r| r.tooltip_surface.id() != surface_id);
                } else {
                    drop(internal);
                    let tooltip_state = state.tooltip_state();
                    // Surface is dead — remove any registration whose tooltip_surface can't be upgraded
                    tooltip_state
                        .registrations
                        .retain(|r| r.tooltip_surface.is_alive());
                }
            }
            zcosmic_tooltip_v1::Request::SetOffset { x, y } => {
                drop(internal);
                let tooltip_data = data.0.lock().unwrap();
                let tooltip_surface = tooltip_data.tooltip_surface.upgrade();
                drop(tooltip_data);

                if let Ok(surface) = tooltip_surface {
                    let surface_id = surface.id();
                    let tooltip_state = state.tooltip_state();
                    if let Some(reg) = tooltip_state
                        .registrations
                        .iter_mut()
                        .find(|r| r.tooltip_surface.id() == surface_id)
                    {
                        reg.offset_x = x;
                        reg.offset_y = y;
                    } else {
                        tracing::warn!(
                            "[tooltip] SetOffset: no registration found for surface {:?}",
                            surface_id
                        );
                    }
                } else {
                    tracing::warn!("[tooltip] SetOffset: tooltip_surface upgrade failed");
                }
            }
            zcosmic_tooltip_v1::Request::SetAnchor { anchor } => {
                let anchor_val = match anchor.into_result() {
                    Ok(a) => TooltipAnchor::try_from(a as u32).unwrap_or_default(),
                    Err(_) => TooltipAnchor::default(),
                };
                tracing::debug!("[tooltip] SetAnchor {:?}", anchor_val);
                drop(internal);
                let tooltip_data = data.0.lock().unwrap();
                let tooltip_surface = tooltip_data.tooltip_surface.upgrade();
                drop(tooltip_data);

                if let Ok(surface) = tooltip_surface {
                    let surface_id = surface.id();
                    let tooltip_state = state.tooltip_state();
                    if let Some(reg) = tooltip_state
                        .registrations
                        .iter_mut()
                        .find(|r| r.tooltip_surface.id() == surface_id)
                    {
                        reg.anchor = anchor_val;
                    }
                }
            }
            zcosmic_tooltip_v1::Request::SetShowDelay { milliseconds } => {
                drop(internal);
                let tooltip_data = data.0.lock().unwrap();
                let tooltip_surface = tooltip_data.tooltip_surface.upgrade();
                drop(tooltip_data);

                if let Ok(surface) = tooltip_surface {
                    let surface_id = surface.id();
                    let tooltip_state = state.tooltip_state();
                    if let Some(reg) = tooltip_state
                        .registrations
                        .iter_mut()
                        .find(|r| r.tooltip_surface.id() == surface_id)
                    {
                        reg.show_delay_ms = milliseconds;
                    } else {
                        tracing::warn!(
                            "[tooltip] SetShowDelay: no registration found for surface {:?}",
                            surface_id
                        );
                    }
                } else {
                    tracing::warn!("[tooltip] SetShowDelay: tooltip_surface upgrade failed");
                }
            }
        }
    }
}

/// Delegate macro for tooltip protocol
#[macro_export]
macro_rules! delegate_tooltip {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::tooltip::zcosmic_tooltip_manager_v1::ZcosmicTooltipManagerV1: ()
        ] => $crate::wayland::protocols::tooltip::TooltipManagerState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::tooltip::zcosmic_tooltip_manager_v1::ZcosmicTooltipManagerV1: ()
        ] => $crate::wayland::protocols::tooltip::TooltipManagerState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::tooltip::zcosmic_tooltip_v1::ZcosmicTooltipV1: $crate::wayland::protocols::tooltip::TooltipData
        ] => $crate::wayland::protocols::tooltip::TooltipManagerState);
    };
}
