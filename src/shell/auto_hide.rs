// SPDX-License-Identifier: GPL-3.0-only

//! Compositor-driven auto-hide for layer surfaces.
//!
//! When a layer surface registers for auto-hide via the render-offset protocol,
//! the compositor manages its visibility animation internally. This provides
//! smooth 60fps animation synced with the compositor's render loop, eliminating
//! the protocol round-trip latency of client-driven animation.
//!
//! The state machine:
//! - **Visible**: Surface is fully shown (offset = 0).
//! - **HidePending**: Waiting before starting hide animation.
//! - **SlidingOut**: Animate surface off the specified edge.
//! - **Hidden**: Surface is fully off-screen.
//! - **HoverPending**: Cursor hit the edge zone; waiting before showing.
//! - **SlidingIn**: Animate surface back on-screen.

use keyframe::{ease, functions::EaseInOutCubic};
use smithay::reexports::wayland_server::Resource;
use smithay::reexports::wayland_server::Weak;
use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;
use std::time::{Duration, Instant};

// ---------------------------------------------------------------------------
// Constants – matching the tuned values from the dock's previous client-side
// animation, now driven at compositor frame rate.
// ---------------------------------------------------------------------------

/// Duration of the hide animation (slide off screen).
pub const HIDE_DURATION: Duration = Duration::from_millis(200);
/// Duration of the show animation (slide on screen).
pub const SHOW_DURATION: Duration = Duration::from_millis(300);
/// Delay before starting the hide animation after cursor leaves.
pub const HIDE_DELAY: Duration = Duration::from_millis(500);
/// Delay before starting the show animation after cursor enters edge zone.
pub const SHOW_DELAY: Duration = Duration::from_millis(400);
/// Extra pixels beyond the surface height for the hide offset (shadow/blur).
pub const SHADOW_BUFFER: i32 = 60;

/// Fullscreen reveal is confined to the outermost physical pixel; elsewhere
/// retain the client's configured logical edge zone (as used by the dock).
pub fn edge_zone_height(configured: u32, fullscreen: bool, scale: f64) -> f64 {
    if configured == 0 {
        0.0
    } else if fullscreen {
        1.0 / scale
    } else {
        f64::from(configured)
    }
}

// ---------------------------------------------------------------------------
// Edge enum – matches the protocol's auto_hide_edge enum
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AutoHideEdge {
    Bottom,
}

impl AutoHideEdge {
    pub fn from_protocol(value: u32) -> Option<Self> {
        match value {
            0 => Some(AutoHideEdge::Bottom),
            _ => None,
        }
    }
}

// ---------------------------------------------------------------------------
// Mode enum – controls when auto-hide triggers
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AutoHideMode {
    /// Always hide when cursor leaves the surface.
    Always,
    /// Only hide when maximized/fullscreen windows exist on the same output.
    OnMaximize,
    /// Stay visible on the desktop, hiding only while this output has fullscreen content.
    OnFullscreen,
}

impl AutoHideMode {
    pub fn should_hide(self, has_windows: bool, has_maximized: bool, has_fullscreen: bool) -> bool {
        match self {
            Self::Always => has_windows,
            Self::OnMaximize => has_maximized,
            Self::OnFullscreen => has_fullscreen,
        }
    }

    pub fn from_protocol(value: u32) -> Self {
        match value {
            1 => AutoHideMode::OnMaximize,
            2 => AutoHideMode::OnFullscreen,
            _ => AutoHideMode::Always, // 0 or unknown defaults to Always
        }
    }
}

/// What holds keyboard focus, as far as one output's fullscreen window cares.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FullscreenFocus {
    /// This output's fullscreen window holds it.
    Held,
    /// Something else does — another window, a layer surface, a popup.
    Elsewhere,
    /// Nothing holds it; the focus stack decides.
    Unfocused,
}

/// What one output looks like when deciding whether a fullscreen window still
/// owns its screen — the same inputs `has_focused_fullscreen` in
/// `focus::order` uses to gate the Top layer. The two must agree: whatever is
/// drawn over the panel is what may hide it.
#[derive(Debug, Clone, Copy)]
pub struct ScreenOwnership {
    /// Game mode holds this output exclusively for a settled fullscreen game.
    pub game_mode_exclusive: bool,
    /// What holds keyboard focus.
    pub focus: FullscreenFocus,
    /// Whether this output's workspace is the one holding the keyboard.
    pub on_focused_workspace: bool,
    /// Whether the workspace's focus stack is topped by its fullscreen window.
    pub focus_stack_top_is_fullscreen: bool,
    /// Whether the workspace overview is open over this output.
    pub overview_is_open: bool,
}

impl ScreenOwnership {
    /// Whether a fullscreen window still owns the screen, and so still hides
    /// an `OnFullscreen` panel.
    ///
    /// Presence alone never answers this. A fullscreen window that has lost
    /// focus to another window on its output is no longer covering the shell —
    /// the render path puts the Top layer back at that point, so the panel has
    /// to come back with it.
    ///
    /// Live focus only speaks for the workspace that holds it. On any other
    /// output the keyboard is elsewhere by definition, and saying so would pop
    /// that screen's panel over its own fullscreen window; there the focus
    /// stack decides. The overview draws over everything and the render path
    /// puts the Top layer back for it, so it releases the screen too — but a
    /// game holding the output exclusively outranks all of it.
    pub fn fullscreen_holds_screen(self) -> bool {
        if self.game_mode_exclusive {
            return true;
        }
        if !self.on_focused_workspace {
            return self.focus_stack_top_is_fullscreen && !self.overview_is_open;
        }
        match self.focus {
            FullscreenFocus::Held => true,
            FullscreenFocus::Elsewhere => false,
            FullscreenFocus::Unfocused => {
                self.focus_stack_top_is_fullscreen && !self.overview_is_open
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Visibility state machine
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub enum AutoHideVisibility {
    /// Fully visible (factor = 0.0).
    Visible,
    /// Waiting before starting hide animation.
    HidePending { start: Instant },
    /// Actively hiding (sliding off-screen).
    SlidingOut { start: Instant, from_factor: f32 },
    /// Fully hidden (factor = 1.0).
    Hidden,
    /// Cursor entered edge zone; waiting before showing.
    HoverPending { start: Instant },
    /// Actively showing (sliding on-screen).
    SlidingIn { start: Instant, from_factor: f32 },
}

impl AutoHideVisibility {
    /// Returns the current visibility factor: 0.0 = fully visible, 1.0 = fully hidden.
    pub fn factor(&self) -> f32 {
        match self {
            Self::Visible | Self::HidePending { .. } => 0.0,
            Self::SlidingOut {
                start, from_factor, ..
            } => {
                let t = progress_clamped(*start, HIDE_DURATION);
                let eased = ease(EaseInOutCubic, 0.0_f32, 1.0_f32, t);
                from_factor + (1.0 - from_factor) * eased
            }
            Self::Hidden | Self::HoverPending { .. } => 1.0,
            Self::SlidingIn {
                start, from_factor, ..
            } => {
                let t = progress_clamped(*start, SHOW_DURATION);
                let eased = ease_out_back(t);
                from_factor * (1.0 - eased)
            }
        }
    }

    /// True while any animation or pending delay is active.
    pub fn is_animating(&self) -> bool {
        !matches!(self, Self::Visible | Self::Hidden)
    }

    /// Advance the state machine. Returns `true` if a state transition occurred
    /// (callers should send `visibility_changed` events for terminal transitions).
    pub fn update(&mut self) -> Option<bool> {
        match self {
            Self::HidePending { start } => {
                if start.elapsed() >= HIDE_DELAY {
                    *self = Self::SlidingOut {
                        start: Instant::now(),
                        from_factor: 0.0,
                    };
                }
                None
            }
            Self::SlidingOut { start, .. } => {
                if start.elapsed() >= HIDE_DURATION {
                    *self = Self::Hidden;
                    Some(false) // now hidden → visibility_changed(0)
                } else {
                    None
                }
            }
            Self::HoverPending { start } => {
                if start.elapsed() >= SHOW_DELAY {
                    *self = Self::SlidingIn {
                        start: Instant::now(),
                        from_factor: 1.0,
                    };
                }
                None
            }
            Self::SlidingIn { start, .. } => {
                if start.elapsed() >= SHOW_DURATION {
                    *self = Self::Visible;
                    Some(true) // now visible → visibility_changed(1)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    // -- State transition helpers --

    /// Begin hiding (with optional delay). Call when maximize detected or
    /// cursor leaves the surface while maximized windows exist.
    pub fn start_hide(&mut self, with_delay: bool) {
        match self {
            Self::Visible => {
                if with_delay {
                    *self = Self::HidePending {
                        start: Instant::now(),
                    };
                } else {
                    *self = Self::SlidingOut {
                        start: Instant::now(),
                        from_factor: 0.0,
                    };
                }
            }
            Self::SlidingIn { .. } => {
                // Reverse from current position.
                let current = self.factor();
                *self = Self::SlidingOut {
                    start: Instant::now(),
                    from_factor: current,
                };
            }
            Self::HoverPending { .. } => {
                // Cancel pending show; stay hidden.
                *self = Self::Hidden;
            }
            _ => {} // Already hiding or hidden
        }
    }

    /// Begin showing. Call when cursor enters edge zone while hidden, or
    /// when all maximized/fullscreen windows are gone.
    pub fn start_show(&mut self, with_delay: bool) {
        match self {
            Self::Hidden => {
                if with_delay {
                    *self = Self::HoverPending {
                        start: Instant::now(),
                    };
                } else {
                    *self = Self::SlidingIn {
                        start: Instant::now(),
                        from_factor: 1.0,
                    };
                }
            }
            Self::SlidingOut { .. } => {
                // Reverse from current position.
                let current = self.factor();
                *self = Self::SlidingIn {
                    start: Instant::now(),
                    from_factor: current,
                };
            }
            Self::HidePending { .. } => {
                // Cancel pending hide; stay visible.
                *self = Self::Visible;
            }
            _ => {} // Already showing or visible
        }
    }

    /// Force immediate show (no delay). Used when un-maximize occurs.
    pub fn force_show(&mut self) {
        match self {
            Self::Hidden | Self::HoverPending { .. } => {
                *self = Self::SlidingIn {
                    start: Instant::now(),
                    from_factor: 1.0,
                };
            }
            Self::SlidingOut { .. } => {
                let current = self.factor();
                *self = Self::SlidingIn {
                    start: Instant::now(),
                    from_factor: current,
                };
            }
            Self::HidePending { .. } => {
                *self = Self::Visible;
            }
            _ => {} // Already visible or showing
        }
    }
}

// ---------------------------------------------------------------------------
// Per-surface auto-hide tracking
// ---------------------------------------------------------------------------

#[derive(Debug)]
pub struct AutoHideSurface {
    /// Weak reference to the underlying wl_surface.
    pub surface: Weak<WlSurface>,
    /// Protocol ID of the wl_surface (for logging).
    pub surface_id: u32,
    /// Which edge the surface hides toward.
    pub edge: AutoHideEdge,
    /// When to trigger auto-hide.
    pub mode: AutoHideMode,
    /// Current animation state.
    pub visibility: AutoHideVisibility,
    /// Whether the cursor is currently over this surface or its edge zone.
    pub cursor_over: bool,
}

impl AutoHideSurface {
    pub fn new(surface: &WlSurface, edge: AutoHideEdge, mode: AutoHideMode) -> Self {
        Self {
            surface: surface.downgrade(),
            surface_id: surface.id().protocol_id(),
            edge,
            mode,
            visibility: AutoHideVisibility::Visible,
            cursor_over: false,
        }
    }

    /// Compute the render offset for the current animation factor.
    /// `surface_height` is the layer surface's height from the layer map.
    /// Factor may go slightly negative during the show animation overshoot
    /// (bounce effect), producing a small offset in the opposite direction.
    pub fn render_offset(&self, surface_height: i32) -> (i32, i32) {
        let factor = self.visibility.factor();
        if factor == 0.0 {
            return (0, 0);
        }
        let max_offset = surface_height + SHADOW_BUFFER;
        match self.edge {
            AutoHideEdge::Bottom => (0, (max_offset as f32 * factor).round() as i32),
        }
    }
}

// ---------------------------------------------------------------------------
// Easing functions
// ---------------------------------------------------------------------------

/// Ease-out-back: overshoot past the target then settle back for a
/// springy bounce feel.  c1 = 1.2 gives a subtle ~7 % overshoot.
fn ease_out_back(t: f32) -> f32 {
    let c1: f32 = 1.2;
    let c3 = c1 + 1.0;
    1.0 + c3 * (t - 1.0).powi(3) + c1 * (t - 1.0).powi(2)
}

/// Clamped progress ratio for an animation started at `start` with `duration`.
fn progress_clamped(start: Instant, duration: Duration) -> f32 {
    (start.elapsed().as_secs_f32() / duration.as_secs_f32()).min(1.0)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fullscreen_mode_leaves_normal_and_maximized_desktops_visible() {
        for (windows, maximized, fullscreen) in [
            (false, false, false),
            (true, false, false),
            (true, true, false),
            (true, true, true),
        ] {
            assert_eq!(
                AutoHideMode::OnFullscreen.should_hide(windows, maximized, fullscreen),
                fullscreen
            );
            assert_eq!(
                AutoHideMode::OnMaximize.should_hide(windows, maximized, fullscreen),
                maximized
            );
            assert_eq!(
                AutoHideMode::Always.should_hide(windows, maximized, fullscreen),
                windows
            );
        }
        assert_eq!(AutoHideMode::from_protocol(2), AutoHideMode::OnFullscreen);
        assert_eq!(
            crate::wayland::protocols::layer_auto_hide::layer_auto_hide_v1::Mode::OnFullscreen
                as u32,
            2
        );
    }

    fn owns(
        focus: FullscreenFocus,
        on_focused_workspace: bool,
        stack_top: bool,
        overview: bool,
        game: bool,
    ) -> bool {
        ScreenOwnership {
            game_mode_exclusive: game,
            focus,
            on_focused_workspace,
            focus_stack_top_is_fullscreen: stack_top,
            overview_is_open: overview,
        }
        .fullscreen_holds_screen()
    }

    const EVERY_FOCUS: [FullscreenFocus; 3] = [
        FullscreenFocus::Held,
        FullscreenFocus::Elsewhere,
        FullscreenFocus::Unfocused,
    ];

    /// Focusing another window on the output the fullscreen window is on gives
    /// the screen back to the shell, so an `OnFullscreen` panel must reappear.
    #[test]
    fn a_fullscreen_window_that_lost_focus_no_longer_hides_the_panel() {
        // Whatever the focus stack says, live focus wins while there is some.
        for stack_top in [false, true] {
            assert!(
                owns(FullscreenFocus::Held, true, stack_top, false, false),
                "the focused fullscreen window still owns the screen"
            );
            assert!(
                !owns(FullscreenFocus::Elsewhere, true, stack_top, false, false),
                "another window has focus, so the panel must come back"
            );
        }
        // With nothing focused the stack decides, matching the render path.
        assert!(owns(FullscreenFocus::Unfocused, true, true, false, false));
        assert!(!owns(FullscreenFocus::Unfocused, true, false, false, false));

        // And that decision is what reaches the panel: only OnFullscreen mode
        // reads it. Maximized is a different preference and is unaffected.
        for (focus, expected) in [
            (FullscreenFocus::Held, true),
            (FullscreenFocus::Elsewhere, false),
        ] {
            let holds = owns(focus, true, true, false, false);
            assert_eq!(holds, expected);
            assert_eq!(
                AutoHideMode::OnFullscreen.should_hide(true, true, holds),
                expected
            );
            assert!(
                AutoHideMode::OnMaximize.should_hide(true, true, holds),
                "a maximized window still hides an OnMaximize panel ({focus:?})"
            );
        }
    }

    /// The keyboard is on one screen at a time. Another output's fullscreen
    /// window is still covering its own screen, so its panel must stay hidden
    /// however far away focus has gone.
    #[test]
    fn another_outputs_fullscreen_keeps_its_own_panel_hidden() {
        for focus in EVERY_FOCUS {
            assert!(
                owns(focus, false, true, false, false),
                "{focus:?}: an unfocused output's fullscreen still owns its screen"
            );
            assert!(
                !owns(focus, false, false, false, false),
                "{focus:?}: nothing fullscreen on top there, so show the panel"
            );
        }
        // Only the screen holding the keyboard reads live focus at all.
        assert!(!owns(FullscreenFocus::Elsewhere, true, true, false, false));
        assert!(owns(FullscreenFocus::Elsewhere, false, true, false, false));
    }

    /// A settled fullscreen game holds its output whatever else happens, and
    /// the overview releases the screen the same way the render path does.
    #[test]
    fn game_mode_outranks_focus_and_the_overview_releases_the_screen() {
        for focus in EVERY_FOCUS {
            for on_focused in [false, true] {
                for stack_top in [false, true] {
                    for overview in [false, true] {
                        assert!(
                            owns(focus, on_focused, stack_top, overview, true),
                            "game mode must keep the panel off the game ({focus:?})"
                        );
                    }
                }
            }
        }
        // The overview draws over everything, so it gives the screen back —
        // except to a fullscreen window that still holds live focus.
        assert!(!owns(FullscreenFocus::Unfocused, true, true, true, false));
        assert!(!owns(FullscreenFocus::Elsewhere, false, true, true, false));
        assert!(owns(FullscreenFocus::Held, true, true, true, false));
    }

    /// The bug was auto-hide and the renderer disagreeing about who owns the
    /// screen. Pin them together: this is `has_focused_fullscreen` from
    /// `focus::order`, and the two must answer identically.
    #[test]
    fn the_rule_matches_what_the_renderer_gates_the_top_layer_on() {
        fn renderer(
            game_mode_exclusive: bool,
            is_active_workspace: bool,
            focus_is_fullscreen: bool,
            focus_is_none: bool,
            focus_stack_is_valid_fullscreen: bool,
            overview_is_open: bool,
        ) -> bool {
            game_mode_exclusive
                || if is_active_workspace {
                    focus_is_fullscreen
                        || (focus_is_none && focus_stack_is_valid_fullscreen && !overview_is_open)
                } else {
                    focus_stack_is_valid_fullscreen && !overview_is_open
                }
        }
        let mut checked = 0;
        for focus in EVERY_FOCUS {
            for on_focused in [false, true] {
                for stack_top in [false, true] {
                    for overview in [false, true] {
                        for game in [false, true] {
                            // A focused fullscreen surface belongs to the
                            // workspace holding the keyboard, so `Held` off
                            // the focused workspace is unreachable.
                            if matches!(focus, FullscreenFocus::Held) && !on_focused {
                                continue;
                            }
                            assert_eq!(
                                owns(focus, on_focused, stack_top, overview, game),
                                renderer(
                                    game,
                                    on_focused,
                                    matches!(focus, FullscreenFocus::Held),
                                    matches!(focus, FullscreenFocus::Unfocused),
                                    stack_top,
                                    overview,
                                ),
                                "auto-hide and the renderer disagree: {focus:?} \
                                 on_focused={on_focused} stack_top={stack_top} \
                                 overview={overview} game={game}"
                            );
                            checked += 1;
                        }
                    }
                }
            }
        }
        assert_eq!(checked, 40, "the whole reachable table");
    }

    #[test]
    fn fullscreen_bottom_trigger_is_one_physical_pixel_at_every_scale() {
        use smithay::utils::{Logical, Point, Rectangle};
        for scale in [1.0, 1.25, 1.5, 2.0] {
            let height = edge_zone_height(4, true, scale);
            let edge = Rectangle::<f64, Logical>::new(
                (1920.0, 1080.0 - height).into(),
                (1920.0, height).into(),
            );
            assert!(edge.contains(Point::from((2100.0, 1080.0 - 0.5 / scale))));
            assert!(!edge.contains(Point::from((2100.0, 1080.0 - 1.1 / scale))));
            assert!(!edge.contains(Point::from((1800.0, 1080.0 - 0.5 / scale))));
            assert!(!edge.contains(Point::from((2100.0, 1080.0))));
            assert_eq!(edge_zone_height(4, false, scale), 4.0);
            assert_eq!(edge_zone_height(0, true, scale), 0.0);
        }
    }

    #[test]
    fn leaving_and_reentering_revealed_panel_respects_hide_delay() {
        let mut visibility = AutoHideVisibility::Visible;
        visibility.start_hide(true);
        assert!(matches!(visibility, AutoHideVisibility::HidePending { .. }));
        visibility.start_show(true);
        assert!(matches!(visibility, AutoHideVisibility::Visible));
        visibility.start_hide(false);
        assert!(matches!(visibility, AutoHideVisibility::SlidingOut { .. }));
        visibility.force_show();
        assert!(matches!(visibility, AutoHideVisibility::SlidingIn { .. }));
    }
}
