//! Where game mode draws the windows of a game it scales. Xwayland hands the pointer to the
//! window X11 has under it, so every window is drawn where X11 has it, seen through the
//! game's scale: menus, dropdowns and tooltips scale with the game. Helper windows keep
//! their own size where the game asked, and X11 has them beside the game instead.

use std::sync::Mutex;

use smithay::{
    output::Output,
    utils::{Logical, Physical, Point, Rectangle, Scale, Size},
    xwayland::X11Surface,
};

use crate::utils::geometry::{Global, PointGlobalExt, PointLocalExt, SizeExt};

/// The game window's offset and scale, from where X11 has it to where it is drawn.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct GroupTransform {
    /// The game's window, where X11 has it.
    pub game: Rectangle<f64, Global>,
    /// Where its corner is drawn.
    pub destination: Point<f64, Global>,
    pub scale: Scale<f64>,
    /// The output the game is drawn on; its windows stay inside it.
    pub output: Rectangle<f64, Global>,
}

/// Where one of the game's windows is drawn, and where X11 has it.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Placement {
    pub drawn: Rectangle<f64, Global>,
    pub x11: Rectangle<f64, Global>,
    pub scale: Scale<f64>,
}

impl GroupTransform {
    /// The transform that draws a window of `size` at `origin` into `presented` on `output`.
    pub fn new(
        origin: Point<f64, Global>,
        size: Size<i32, Logical>,
        presented: Rectangle<f64, Global>,
        output: Rectangle<f64, Global>,
    ) -> Option<Self> {
        (size.w > 0 && size.h > 0 && !presented.is_empty()).then(|| Self {
            game: Rectangle::new(origin, size.to_f64().as_global()),
            destination: presented.loc,
            scale: Scale {
                x: presented.size.w / f64::from(size.w),
                y: presented.size.h / f64::from(size.h),
            },
            output,
        })
    }

    /// Whether drawing through this transform changes nothing.
    pub fn is_identity(&self) -> bool {
        const EPSILON: f64 = 1e-6;
        (self.scale.x - 1.0).abs() < EPSILON
            && (self.scale.y - 1.0).abs() < EPSILON
            && (self.destination.x - self.game.loc.x).abs() < 0.5
            && (self.destination.y - self.game.loc.y).abs() < 0.5
    }

    /// Where a point of the game's coordinate space is drawn.
    pub fn to_screen(&self, point: Point<f64, Global>) -> Point<f64, Global> {
        let offset = point - self.game.loc;
        self.destination + Point::from((offset.x * self.scale.x, offset.y * self.scale.y))
    }

    /// The point of the game's coordinate space drawn at `point`.
    pub fn to_game(&self, point: Point<f64, Global>) -> Point<f64, Global> {
        let offset = point - self.destination;
        self.game.loc + Point::from((offset.x / self.scale.x, offset.y / self.scale.y))
    }

    /// The game's own window.
    pub fn base(&self) -> Placement {
        Placement {
            drawn: Rectangle::new(
                self.destination,
                Size::from((
                    self.game.size.w * self.scale.x,
                    self.game.size.h * self.scale.y,
                )),
            ),
            x11: self.game,
            scale: self.scale,
        }
    }

    /// A helper window at its own size, drawn where the game asked (`asked`) while X11 has
    /// it at `x11` (see [`Self::helper_spot`]). Input maps 1:1 from the drawn corner.
    pub fn one_to_one(&self, asked: Point<i32, Global>, x11: Rectangle<i32, Global>) -> Placement {
        let size = x11.size.to_f64();
        Placement {
            drawn: Rectangle::new(
                inside(self.output, self.to_screen(asked.to_f64()), size),
                size,
            ),
            x11: x11.to_f64(),
            scale: Scale::from(1.0),
        }
    }

    /// Whether a pointer at `point` on the game is under none of `windows` as drawn, but
    /// under one where X11 has it: Xwayland would hand the click to that window, so it goes
    /// nowhere instead. Only a window drawn away from where X11 has it does this.
    pub fn shadowed(&self, windows: &[Placement], point: Point<f64, Global>) -> bool {
        let at = self.to_game(point);
        let base = self.base();
        let mut others = windows.iter().filter(|w| **w != base);
        self.game.contains(at)
            && !others.clone().any(|w| w.drawn.contains(point))
            && others.any(|w| w.x11.contains(at))
    }

    /// Where X11 has a helper of `size` drawn at `drawn`, which asked for `asked` and is at
    /// `current`, given the game's `others` on the `area` the pointer reaches. Over the game
    /// it would take clicks the screen shows on the game, so it goes where it covers the
    /// least of the game outside its drawn copy (none, when it fits beside the game), then
    /// the least of the others. On a tie it stays where it asked, then where it is, then
    /// takes the top-left-most spot.
    pub fn helper_spot(
        &self,
        area: Rectangle<i32, Global>,
        drawn: Rectangle<f64, Global>,
        size: Size<i32, Global>,
        asked: Point<i32, Global>,
        current: Point<i32, Global>,
        others: &[Rectangle<f64, Global>],
    ) -> Point<i32, Global> {
        let area_f = area.to_f64();
        let game = self.game;
        // The game under the drawn copy: a click there lands on the helper either way.
        let under = Rectangle::new(
            self.to_game(drawn.loc),
            Size::from((drawn.size.w / self.scale.x, drawn.size.h / self.scale.y)),
        );
        let under_game = intersection(game, under);
        let cost = |loc: Point<i32, Global>| {
            let rect = Rectangle::new(loc, size).to_f64();
            let lost = overlap(rect, game) - under_game.map_or(0.0, |u| overlap(rect, u))
                + (rect.size.w * rect.size.h - overlap(rect, area_f));
            let clash = others.iter().map(|o| overlap(rect, *o)).sum::<f64>();
            (lost, clash)
        };

        // The best spot touches an edge of the area or of an obstacle on each axis.
        let obstacles = [game]
            .into_iter()
            .chain(under_game)
            .chain(others.iter().copied())
            .collect::<Vec<_>>();
        let xs = candidates(
            (area.loc.x, area.size.w),
            size.w,
            obstacles.iter().map(|o| (o.loc.x, o.loc.x + o.size.w)),
        );
        let ys = candidates(
            (area.loc.y, area.size.h),
            size.h,
            obstacles.iter().map(|o| (o.loc.y, o.loc.y + o.size.h)),
        );
        let grid = ys
            .iter()
            .flat_map(|y| xs.iter().map(move |x| Point::from((*x, *y))));

        const EPSILON: f64 = 1e-6;
        let mut best = (asked, cost(asked));
        for loc in std::iter::once(current).chain(grid) {
            let (lost, clash) = cost(loc);
            let (best_lost, best_clash) = best.1;
            if lost < best_lost - EPSILON
                || (lost < best_lost + EPSILON && clash < best_clash - EPSILON)
            {
                best = (loc, (lost, clash));
            }
        }
        best.0
    }
}

impl Placement {
    /// A pointer at `point` on screen, in the window's own coordinates.
    pub fn window_point(&self, point: Point<f64, Global>) -> Point<f64, Logical> {
        let offset = point - self.drawn.loc;
        Point::from((offset.x / self.scale.x, offset.y / self.scale.y))
    }

    /// Where a point in the window's own coordinates is drawn.
    pub fn to_screen(&self, point: Point<f64, Logical>) -> Point<f64, Global> {
        self.drawn.loc + Point::from((point.x * self.scale.x, point.y * self.scale.y))
    }

    pub fn is_scaled(&self) -> bool {
        const EPSILON: f64 = 1e-6;
        (self.scale.x - 1.0).abs() > EPSILON || (self.scale.y - 1.0).abs() > EPSILON
    }

    /// Where the drawn window's corner is, in whole logical units for the render stages.
    pub fn location(&self) -> Point<i32, Global> {
        self.drawn.loc.to_i32_round()
    }

    /// The physical position on `output` to draw the window at.
    pub fn render_origin(&self, output: &Output, output_scale: f64) -> Point<i32, Physical> {
        self.drawn
            .loc
            .to_local(output)
            .as_logical()
            .to_physical_precise_round(output_scale)
    }

    /// A menu or tooltip that opened from this window at `rect` (its X11 geometry): drawn
    /// with the window's offset and scale, pushed back inside `output` if it would run past
    /// an edge. Only the drawn position moves; input maps through it.
    pub fn popup(&self, rect: Rectangle<i32, Global>, output: Rectangle<f64, Global>) -> Placement {
        let rect = rect.to_f64();
        let offset = rect.loc - self.x11.loc;
        let at = self.drawn.loc + Point::from((offset.x * self.scale.x, offset.y * self.scale.y));
        let size = Size::from((rect.size.w * self.scale.x, rect.size.h * self.scale.y));
        Placement {
            drawn: Rectangle::new(inside(output, at, size), size),
            x11: rect,
            scale: self.scale,
        }
    }
}

/// The window, of `windows` topmost first, that a menu or tooltip at `rect` opened from:
/// the first whose X11 rectangle holds or touches its corner, else the nearest.
pub fn owner<'a>(
    windows: impl IntoIterator<Item = &'a Placement>,
    rect: Rectangle<i32, Global>,
) -> Option<&'a Placement> {
    let corner = rect.loc.to_f64();
    let distance = |window: &Placement| {
        let r = window.x11;
        let dx = (r.loc.x - corner.x)
            .max(corner.x - (r.loc.x + r.size.w))
            .max(0.0);
        let dy = (r.loc.y - corner.y)
            .max(corner.y - (r.loc.y + r.size.h))
            .max(0.0);
        dx * dx + dy * dy
    };
    let mut best: Option<(&Placement, f64)> = None;
    for window in windows {
        let d = distance(window);
        if best.is_none_or(|(_, best)| d < best) {
            best = Some((window, d));
        }
    }
    best.map(|(window, _)| window)
}

/// Where a helper window of a scaled game asked to be, while X11 has it elsewhere.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Helper {
    pub asked: Point<i32, Global>,
    /// How many times in a row it asked for that spot again after it was moved.
    insists: u8,
}

#[derive(Debug, Default)]
struct HelperState(Mutex<Option<Helper>>);

impl Helper {
    /// After this many asks in a row X11 leaves it where it asks, so the two never fight.
    const INSIST_LIMIT: u8 = 3;

    pub fn at(asked: Point<i32, Global>) -> Self {
        Self { asked, insists: 0 }
    }

    pub fn of(surface: &X11Surface) -> Option<Self> {
        *surface.user_data().get::<HelperState>()?.0.lock().unwrap()
    }

    pub fn set(surface: &X11Surface, helper: Option<Self>) {
        *surface
            .user_data()
            .get_or_insert_threadsafe(HelperState::default)
            .0
            .lock()
            .unwrap() = helper;
    }

    /// Whether it keeps asking for its own spot, so X11 keeps it there.
    pub fn insists(&self) -> bool {
        self.insists >= Self::INSIST_LIMIT
    }

    /// Where it asks to be after it requests `requested` while X11 has it at `current`. The
    /// app sees itself where X11 has it, so a nudge that still overlaps that spot moves it by
    /// as much from where it asked; anything else is a new spot.
    pub fn request(self, current: Rectangle<i32, Global>, requested: Point<i32, Global>) -> Self {
        if requested == current.loc {
            return self;
        }
        let moved = current.loc != self.asked;
        if requested == self.asked {
            return Self {
                insists: self.insists.saturating_add(u8::from(moved)),
                ..self
            };
        }
        if moved && Rectangle::new(requested, current.size).overlaps(current) {
            return Self::at(self.asked + (requested - current.loc));
        }
        Self::at(requested)
    }
}

/// Where a window of `size` drawn at `at` stays inside `output`.
fn inside(
    output: Rectangle<f64, Global>,
    at: Point<f64, Global>,
    size: Size<f64, Global>,
) -> Point<f64, Global> {
    let max = output.loc + Point::from((output.size.w, output.size.h));
    Point::from((
        at.x.min(max.x - size.w).max(output.loc.x),
        at.y.min(max.y - size.h).max(output.loc.y),
    ))
}

/// Where a window `len` long may start on an axis of `area`: at either end, or against
/// either side of each obstacle, in order.
fn candidates(
    (start, area_len): (i32, i32),
    len: i32,
    obstacles: impl Iterator<Item = (f64, f64)>,
) -> Vec<i32> {
    let last = (start + area_len - len).max(start);
    let mut at = vec![start, last];
    for (lo, hi) in obstacles {
        at.extend([lo.floor() as i32 - len, hi.ceil() as i32]);
    }
    let mut at = at
        .into_iter()
        .map(|a| a.clamp(start, last))
        .collect::<Vec<_>>();
    at.sort_unstable();
    at.dedup();
    at
}

fn intersection(
    a: Rectangle<f64, Global>,
    b: Rectangle<f64, Global>,
) -> Option<Rectangle<f64, Global>> {
    let lo: Point<f64, Global> = Point::from((a.loc.x.max(b.loc.x), a.loc.y.max(b.loc.y)));
    let hi: Point<f64, Global> = Point::from((
        (a.loc.x + a.size.w).min(b.loc.x + b.size.w),
        (a.loc.y + a.size.h).min(b.loc.y + b.size.h),
    ));
    (hi.x > lo.x && hi.y > lo.y).then(|| Rectangle::new(lo, Size::from((hi.x - lo.x, hi.y - lo.y))))
}

fn overlap(a: Rectangle<f64, Global>, b: Rectangle<f64, Global>) -> f64 {
    intersection(a, b).map_or(0.0, |r| r.size.w * r.size.h)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn rect(x: f64, y: f64, w: f64, h: f64) -> Rectangle<f64, Global> {
        Rectangle::new(Point::from((x, y)), Size::from((w, h)))
    }

    fn window(x: i32, y: i32, w: i32, h: i32) -> Rectangle<i32, Global> {
        Rectangle::new(Point::from((x, y)), Size::from((w, h)))
    }

    const OUTPUT: (f64, f64) = (1707.0, 960.0);

    fn area() -> Rectangle<i32, Global> {
        window(0, 0, 1707, 960)
    }

    /// The 640x400 launcher at 150% (427x267 logical) centred on a 1707x960 output,
    /// fitted to its height.
    fn launcher() -> GroupTransform {
        let height = OUTPUT.1;
        let width = 427.0 * height / 267.0;
        GroupTransform::new(
            Point::from((640.0, 347.0)),
            Size::from((427, 267)),
            rect((OUTPUT.0 - width) / 2.0, 0.0, width, height),
            rect(0.0, 0.0, OUTPUT.0, OUTPUT.1),
        )
        .unwrap()
    }

    fn close(a: f64, b: f64) -> bool {
        (a - b).abs() < 1e-9
    }

    /// Where game mode puts a helper the game asked for at `asked`: drawn and in X11.
    fn helper(
        t: &GroupTransform,
        asked: Rectangle<i32, Global>,
        others: &[Rectangle<f64, Global>],
    ) -> Placement {
        let drawn = t.one_to_one(asked.loc, asked).drawn;
        let spot = t.helper_spot(area(), drawn, asked.size, asked.loc, asked.loc, others);
        t.one_to_one(asked.loc, Rectangle::new(spot, asked.size))
    }

    #[test]
    fn the_game_fits_the_output_keeping_its_aspect_ratio() {
        let t = launcher();
        assert!(close(t.scale.x, t.scale.y), "uniform");
        let top_left = t.to_screen(t.game.loc);
        let bottom_right = t.to_screen(Point::from((640.0 + 427.0, 347.0 + 267.0)));
        assert!(
            close(top_left.y, 0.0) && close(bottom_right.y, OUTPUT.1),
            "fills the height"
        );
        let (left, right) = (top_left.x, OUTPUT.0 - bottom_right.x);
        assert!(
            close(left, right) && left > 0.0,
            "centred between equal bars"
        );
        let back = t.to_game(t.to_screen(Point::from((700.0, 400.0))));
        assert!(close(back.x, 700.0) && close(back.y, 400.0));
    }

    #[test]
    fn a_menu_scales_with_the_game_under_its_button() {
        let t = launcher();
        // A dropdown opened 100,50 from the launcher's corner.
        let menu = t.base().popup(window(740, 397, 80, 120), t.output);
        let corner = t.to_screen(t.game.loc);
        assert!(close(menu.drawn.loc.x - corner.x, 100.0 * t.scale.x));
        assert!(close(menu.drawn.loc.y - corner.y, 50.0 * t.scale.y));
        assert!(
            close(menu.drawn.size.w, 80.0 * t.scale.x),
            "at the game's scale"
        );
        // Its item 10,20 from its corner is drawn 10,20 game pixels from it.
        let item = menu.to_screen(Point::from((10.0, 20.0)));
        let local = menu.window_point(item);
        assert!(close(local.x, 10.0) && close(local.y, 20.0));
        assert!(close(item.x - menu.drawn.loc.x, 10.0 * t.scale.x));
    }

    /// A menu opened at the launcher's bottom-right corner would run off the screen.
    #[test]
    fn a_scaled_menu_past_an_edge_is_drawn_inside_the_output() {
        let t = launcher();
        let menu = t.base().popup(window(1066, 600, 200, 80), t.output);
        assert!(
            close(menu.drawn.loc.x + menu.drawn.size.w, OUTPUT.0),
            "flush right"
        );
        assert!(
            close(menu.drawn.loc.y + menu.drawn.size.h, OUTPUT.1),
            "flush bottom"
        );
        // Its last item, drawn at the bottom edge, still maps to the menu's own pixels.
        let item = Point::<f64, Logical>::from((36.0, 74.0));
        let back = menu.window_point(menu.to_screen(item));
        assert!(close(back.x, item.x) && close(back.y, item.y));
        // Clamped, it still covers where X11 has it over the game: nothing is shadowed.
        let over_the_game = Point::from((1619.0, 930.0));
        let at = t.to_game(over_the_game);
        assert!(t.game.contains(at) && menu.x11.contains(at));
        assert!(!t.shadowed(&[menu], over_the_game));
        // Above or left of the output it is pushed the other way.
        let high = t.base().popup(window(600, 300, 50, 50), t.output);
        assert!(close(high.drawn.loc.x, 0.0) && close(high.drawn.loc.y, 0.0));
    }

    #[test]
    fn a_helper_keeps_its_size_where_the_game_asked() {
        let t = launcher();
        let asked = window(740, 397, 160, 93);
        let placed = helper(&t, asked, &[]);
        let corner = t.to_screen(t.game.loc);
        assert!(close(placed.drawn.loc.x - corner.x, 100.0 * t.scale.x));
        assert!(close(placed.drawn.loc.y - corner.y, 50.0 * t.scale.y));
        assert_eq!(placed.drawn.size, Size::from((160.0, 93.0)), "its own size");
        // Input maps 1:1 from the drawn corner to where X11 has it.
        let local = placed.window_point(placed.drawn.loc + Point::from((10.0, 20.0)));
        assert!(close(local.x, 10.0) && close(local.y, 20.0));
    }

    #[test]
    fn x11_has_a_helper_beside_the_game_so_nothing_is_shadowed() {
        let t = launcher();
        let asked = window(740, 397, 160, 93);
        let placed = helper(&t, asked, &[]);
        assert!(
            intersection(placed.x11, t.game).is_none(),
            "{:?} is off the game",
            placed.x11
        );
        assert!(area().to_f64().contains_rect(placed.x11), "reachable");
        // Every point of the game beside and below the drawn helper takes its own clicks.
        let d = placed.drawn;
        for (dx, dy) in [
            (d.size.w + 20.0, 10.0),
            (10.0, d.size.h + 20.0),
            (-20.0, -20.0),
        ] {
            assert!(!t.shadowed(&[placed], d.loc + Point::from((dx, dy))));
        }
        // Where X11 had it before, the game was shadowed.
        let unmoved = t.one_to_one(asked.loc, asked);
        assert!(t.shadowed(&[unmoved], d.loc + Point::from((d.size.w + 20.0, 10.0))));
    }

    #[test]
    fn helpers_share_the_free_space_without_overlapping() {
        let t = launcher();
        let asked = window(740, 397, 300, 200);
        let first = helper(&t, asked, &[]);
        let second = helper(&t, asked, &[first.x11]);
        let third = helper(&t, window(700, 360, 300, 200), &[first.x11, second.x11]);
        for placed in [first, second, third] {
            assert!(intersection(placed.x11, t.game).is_none());
        }
        assert!(intersection(first.x11, second.x11).is_none());
        assert!(intersection(third.x11, first.x11).is_none());
        assert!(intersection(third.x11, second.x11).is_none());
    }

    #[test]
    fn a_helper_already_off_the_game_stays_where_it_is() {
        let t = launcher();
        // Asked beside the game, it covers none of it.
        let asked = window(100, 100, 300, 200);
        assert_eq!(helper(&t, asked, &[]).x11, asked.to_f64());
        // Moved once, it stays there while that spot is still free.
        let drawn = t.one_to_one(Point::from((740, 397)), asked).drawn;
        let current = Point::from((5, 600));
        let spot = t.helper_spot(
            area(),
            drawn,
            asked.size,
            Point::from((740, 397)),
            current,
            &[],
        );
        assert_eq!(spot, current);
    }

    #[test]
    fn a_helper_bigger_than_the_free_space_covers_the_game_least() {
        let t = launcher();
        // Wider than either side and taller than above or below the game.
        let asked = window(400, 200, 900, 500);
        let placed = helper(&t, asked, &[]);
        let covered = overlap(placed.x11, t.game);
        assert!(covered > 0.0 && covered < overlap(asked.to_f64(), t.game) / 2.0);
        assert!(area().to_f64().contains_rect(placed.x11));
        // The game left under X11's copy, off the drawn one, still drops its clicks.
        let covered = intersection(placed.x11, t.game).unwrap();
        let corner = covered.loc + Point::from((covered.size.w, covered.size.h));
        let probe = t.to_screen(corner) - Point::from((1.0, 1.0));
        assert!(!placed.drawn.contains(probe));
        assert!(t.shadowed(&[placed, t.base()], probe));
    }

    #[test]
    fn a_helper_on_a_smaller_screen_than_its_scale_stays_where_it_asked() {
        // Shrunk to fit, the game's helper covers no more of it than its own drawn copy.
        let t = GroupTransform::new(
            Point::from((0.0, 0.0)),
            Size::from((2000, 1200)),
            rect(0.0, 30.0, 1600.0, 960.0),
            rect(0.0, 0.0, 1600.0, 1020.0),
        )
        .unwrap();
        let asked = window(500, 400, 300, 200);
        let drawn = t.one_to_one(asked.loc, asked).drawn;
        let spot = t.helper_spot(
            window(0, 0, 1600, 1020),
            drawn,
            asked.size,
            asked.loc,
            asked.loc,
            &[],
        );
        assert_eq!(spot, asked.loc);
    }

    #[test]
    fn a_menu_opens_with_the_window_it_belongs_to() {
        let t = launcher();
        let placed = helper(&t, window(740, 397, 160, 93), &[]);
        let base = t.base();
        let windows = [placed, base];
        // A dropdown under a button of the helper, where X11 has the helper.
        let x11 = placed.x11.to_i32_round::<i32>();
        let menu = window(x11.loc.x + 20, x11.loc.y + 60, 100, 80);
        let owner = owner(&windows, menu).unwrap();
        assert_eq!(*owner, placed);
        let drawn = owner.popup(menu, t.output);
        assert!(!drawn.is_scaled(), "at the helper's size");
        assert!(close(drawn.drawn.loc.x - placed.drawn.loc.x, 20.0));
        assert!(close(drawn.drawn.loc.y - placed.drawn.loc.y, 60.0));
        // One the game opens on itself scales with it.
        let game_menu = window(700, 400, 100, 80);
        assert_eq!(*super::owner(&windows, game_menu).unwrap(), base);
        // A tooltip just past the game's edge still belongs to the game.
        assert_eq!(
            *super::owner(&windows, window(1070, 620, 50, 20)).unwrap(),
            base
        );
    }

    #[test]
    fn a_helper_that_moves_itself_is_drawn_where_it_moves() {
        let current = window(10, 10, 160, 93);
        let helper = Helper::at(Point::from((740, 397)));
        // A nudge from where it sees itself moves the drawn copy by as much.
        let nudged = helper.request(current, Point::from((25, 4)));
        assert_eq!(nudged.asked, Point::from((755, 391)));
        // A new spot of its own is taken as it is.
        let moved = helper.request(current, Point::from((600, 300)));
        assert_eq!(moved, Helper::at(Point::from((600, 300))));
        // A resize keeps the spot.
        assert_eq!(helper.request(current, current.loc), helper);
    }

    #[test]
    fn a_helper_that_insists_on_its_spot_is_left_there() {
        let current = window(10, 10, 160, 93);
        let mut helper = Helper::at(Point::from((740, 397)));
        for _ in 0..Helper::INSIST_LIMIT - 1 {
            helper = helper.request(current, helper.asked);
            assert!(!helper.insists());
        }
        helper = helper.request(current, helper.asked);
        assert!(helper.insists());
        // Asking somewhere new starts over.
        assert!(!helper.request(current, Point::from((700, 380))).insists());
    }

    #[test]
    fn a_window_drawn_where_it_is_is_the_identity() {
        let at = rect(0.0, 0.0, OUTPUT.0, OUTPUT.1);
        let native = GroupTransform::new(at.loc, Size::from((1707, 960)), at, at).unwrap();
        assert!(native.is_identity());
        assert!(!launcher().is_identity());
        assert!(
            GroupTransform::new(at.loc, Size::from((0, 960)), at, at).is_none(),
            "an empty window"
        );
    }
}
