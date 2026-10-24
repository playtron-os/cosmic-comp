// SPDX-License-Identifier: GPL-3.0-only

//! The edge reveals over a fullscreen window: the Halo at the top and an
//! auto-hiding panel at the bottom. They hold back over a game, which may
//! scroll when the pointer reaches an edge, and are reached by touch and at an
//! edge shared with another output, which the pointer would cross instead.

use crate::dbus::game_mode::LAUNCHER_APP_ID;

/// Game mode shows a game, not its launcher, which is a fullscreen app like any other.
pub fn game_mode_shows_game(active: bool, app_id: Option<u32>) -> bool {
    active && app_id.is_some_and(|id| id != LAUNCHER_APP_ID)
}

/// Whether both edge reveals hold back over a fullscreen window.
pub fn reveals_held_back(game_mode_game: bool, pointer_constrained: bool) -> bool {
    game_mode_game || pointer_constrained
}

/// How far the pointer pushes past a shared edge before crossing, so a reveal
/// waiting there can be reached.
pub const EDGE_BARRIER: f64 = 100.0;
/// A touch starting this close to an edge may swipe a reveal in.
pub const TOUCH_EDGE: f64 = 16.0;
/// How far inward that touch travels before it counts as a swipe.
pub const TOUCH_SWIPE: f64 = 24.0;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Edge {
    Top,
    Bottom,
}

impl Edge {
    /// How far `from` → `to` goes past this edge of the span `[top, bottom)`;
    /// zero or less when it stays on this side.
    pub fn past(self, top: f64, bottom: f64, from: f64, to: f64) -> f64 {
        match self {
            Edge::Bottom if from < bottom => to - bottom,
            Edge::Top if from >= top => top - to,
            _ => 0.0,
        }
    }

    /// Where the pointer rests against this edge: inside the outermost physical pixel.
    pub fn rest(self, top: f64, bottom: f64, scale: f64) -> f64 {
        match self {
            Edge::Top => top,
            Edge::Bottom => bottom - 0.5 / scale,
        }
    }

    /// Distance from this edge to `y`, inward.
    pub fn inward(self, top: f64, bottom: f64, y: f64) -> f64 {
        match self {
            Edge::Top => y - top,
            Edge::Bottom => bottom - y,
        }
    }
}

/// Holds the pointer at an armed shared edge until it has pushed [`EDGE_BARRIER`] past it.
#[derive(Debug, Default)]
pub struct EdgeBarrier {
    pushed: f64,
}

impl EdgeBarrier {
    /// `past` is how far this motion goes beyond the armed edge. True holds the pointer.
    pub fn hold(&mut self, past: f64) -> bool {
        if past <= 0.0 {
            self.pushed = 0.0;
            return false;
        }
        self.pushed += past;
        if self.pushed < EDGE_BARRIER {
            true
        } else {
            self.pushed = 0.0;
            false
        }
    }
}

/// A touch that began at an armed edge, until it swipes inward or lifts.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct EdgeSwipe {
    pub slot: i32,
    pub edge: Edge,
    pub start: f64,
}

impl EdgeSwipe {
    /// Whether a touch at `y` began close enough to `edge`.
    pub fn starts(edge: Edge, top: f64, bottom: f64, y: f64) -> bool {
        (0.0..TOUCH_EDGE).contains(&edge.inward(top, bottom, y))
    }

    /// Whether the touch, now at `y`, has swiped inward far enough.
    pub fn swiped(&self, y: f64) -> bool {
        match self.edge {
            Edge::Top => y - self.start >= TOUCH_SWIPE,
            Edge::Bottom => self.start - y >= TOUCH_SWIPE,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_game_in_game_mode_or_a_held_pointer_holds_both_reveals_back() {
        let game = game_mode_shows_game(true, Some(1237320));
        assert!(game);
        assert!(reveals_held_back(game, false), "a game game mode shows");
        assert!(
            reveals_held_back(false, true),
            "a locked or confined pointer"
        );
        assert!(reveals_held_back(game, true));
    }

    /// The launcher is game mode's active app 769 between games: both reveals stay.
    #[test]
    fn the_launcher_keeps_both_reveals() {
        let launcher = game_mode_shows_game(true, Some(LAUNCHER_APP_ID));
        assert_eq!(LAUNCHER_APP_ID, 769);
        assert!(!launcher);
        assert!(!reveals_held_back(launcher, false));
        assert!(
            reveals_held_back(launcher, true),
            "unless it holds the pointer"
        );
    }

    /// Any other fullscreen window, a game started outside game mode included,
    /// keeps both reveals unless it holds the pointer.
    #[test]
    fn outside_game_mode_only_a_held_pointer_counts() {
        for app_id in [None, Some(LAUNCHER_APP_ID), Some(1237320)] {
            let game = game_mode_shows_game(false, app_id);
            assert!(!game, "{app_id:?}");
            assert!(!reveals_held_back(game, false));
        }
    }
}
#[cfg(test)]
mod reach_tests {
    use super::*;

    #[test]
    fn a_shared_edge_holds_the_pointer_until_it_pushes_through() {
        let mut barrier = EdgeBarrier::default();
        assert!(barrier.hold(30.0), "the first push stops at the edge");
        assert!(barrier.hold(60.0));
        assert!(!barrier.hold(20.0), "100px past it, the pointer crosses");
        assert!(barrier.hold(5.0), "the next arrival is held again");
        assert!(!barrier.hold(0.0), "moving back along or away releases it");
        assert!(barrier.hold(99.0), "and the push starts over");
    }

    #[test]
    fn crossing_is_measured_only_through_the_edge_it_left() {
        let (top, bottom) = (0.0, 1080.0);
        assert_eq!(Edge::Bottom.past(top, bottom, 1079.5, 1090.0), 10.0);
        assert!(Edge::Bottom.past(top, bottom, 1000.0, 1050.0) <= 0.0);
        assert!(
            Edge::Bottom.past(top, bottom, 1080.0, 1090.0) <= 0.0,
            "already beyond"
        );
        assert_eq!(Edge::Top.past(top, bottom, 0.0, -12.0), 12.0);
        assert!(Edge::Top.past(top, bottom, 20.0, 10.0) <= 0.0);
        for scale in [1.0, 1.5, 2.0] {
            let rest = Edge::Bottom.rest(top, bottom, scale);
            assert!(
                rest < bottom && bottom - rest < 1.0 / scale,
                "inside the last pixel"
            );
            assert_eq!(Edge::Top.rest(top, bottom, scale), top);
        }
    }

    #[test]
    fn a_touch_from_the_edge_swipes_inward() {
        let (top, bottom) = (0.0, 1080.0);
        assert!(EdgeSwipe::starts(Edge::Bottom, top, bottom, 1070.0));
        assert!(!EdgeSwipe::starts(Edge::Bottom, top, bottom, 1060.0));
        assert!(EdgeSwipe::starts(Edge::Top, top, bottom, 4.0));
        assert!(!EdgeSwipe::starts(Edge::Top, top, bottom, 1070.0));
        let up = EdgeSwipe {
            slot: 0,
            edge: Edge::Bottom,
            start: 1075.0,
        };
        assert!(!up.swiped(1060.0));
        assert!(up.swiped(1051.0));
        assert!(!up.swiped(1080.0), "sliding off the edge is no swipe");
        let down = EdgeSwipe {
            slot: 1,
            edge: Edge::Top,
            start: 2.0,
        };
        assert!(down.swiped(26.0));
        assert!(!down.swiped(20.0));
    }
}
