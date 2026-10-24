//! Where game mode draws the windows of a game it scales. The game's window is scaled; its
//! menus, dropdowns, tooltips and helpers stay at their own size, each with its corner where
//! the game put it on the scaled window, so a dropdown still opens under its scaled button.

use smithay::{
    output::Output,
    utils::{Logical, Physical, Point, Rectangle, Scale, Size},
};

use crate::utils::geometry::{Global, PointExt, PointGlobalExt, PointLocalExt};

/// The game window's offset and scale, from where X11 has it to where it is drawn.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct GroupTransform {
    /// The game window's corner, where X11 has placed it.
    pub origin: Point<f64, Global>,
    /// Where that corner is drawn.
    pub destination: Point<f64, Global>,
    pub scale: Scale<f64>,
    /// The output the game is drawn on; its popups stay inside it.
    pub output: Rectangle<f64, Global>,
}

/// Where one of the game's popups is drawn, and where X11 sees it from the scaled game.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Placement {
    /// At its own size.
    pub drawn: Rectangle<f64, Global>,
    /// The popup's X11 rectangle seen through the game's scale. Xwayland picks the window
    /// under the pointer in X11 coordinates, so a pointer on the game's window anywhere in
    /// here reaches the popup, not the game.
    pub footprint: Rectangle<f64, Global>,
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
            origin,
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
            && (self.destination.x - self.origin.x).abs() < 0.5
            && (self.destination.y - self.origin.y).abs() < 0.5
    }

    /// Where a point of the game's coordinate space is drawn.
    pub fn to_screen(&self, point: Point<f64, Global>) -> Point<f64, Global> {
        let offset = point - self.origin;
        self.destination + Point::from((offset.x * self.scale.x, offset.y * self.scale.y))
    }

    /// Where a popup the game placed at `rect` is drawn: at its own size, its corner moved
    /// with the game, then pushed back inside the output if it would run past an edge. Only
    /// the drawn position moves, never the X11 geometry; input follows the drawn popup.
    pub fn place(&self, rect: Rectangle<i32, Global>) -> Placement {
        let rect = rect.to_f64();
        let at = self.to_screen(rect.loc);
        let max = self.output.loc + Point::from((self.output.size.w, self.output.size.h));
        let loc = Point::from((
            at.x.min(max.x - rect.size.w).max(self.output.loc.x),
            at.y.min(max.y - rect.size.h).max(self.output.loc.y),
        ));
        Placement {
            drawn: Rectangle::new(loc, rect.size),
            footprint: Rectangle::new(
                at,
                Size::from((rect.size.w * self.scale.x, rect.size.h * self.scale.y)),
            ),
        }
    }
}

impl Placement {
    /// A pointer at `point` on screen, in the popup's own coordinates.
    pub fn window_point(&self, point: Point<f64, Global>) -> Point<f64, Logical> {
        (point - self.drawn.loc).as_logical()
    }

    /// Where a point in the popup's own coordinates is drawn.
    pub fn to_screen(&self, point: Point<f64, Logical>) -> Point<f64, Global> {
        self.drawn.loc + point.as_global()
    }

    /// Where the drawn popup's corner is, in whole logical units for the render stages.
    pub fn location(&self) -> Point<i32, Global> {
        self.drawn.loc.to_i32_round()
    }

    /// The physical position on `output` to draw the popup at.
    pub fn render_origin(&self, output: &Output, output_scale: f64) -> Point<i32, Physical> {
        self.drawn
            .loc
            .to_local(output)
            .as_logical()
            .to_physical_precise_round(output_scale)
    }
}

/// Whether `point` is shadowed by the game's popups: under one's footprint but on none of
/// them. A click there would reach a popup in X11 while the screen shows the game, so it
/// goes nowhere instead.
pub fn shadowed<'a>(
    placements: impl IntoIterator<Item = &'a Placement>,
    point: Point<f64, Global>,
) -> bool {
    let mut shadowed = false;
    for placement in placements {
        if placement.drawn.contains(point) {
            return false;
        }
        shadowed |= placement.footprint.contains(point);
    }
    shadowed
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

    #[test]
    fn the_game_fits_the_output_keeping_its_aspect_ratio() {
        let t = launcher();
        assert!(close(t.scale.x, t.scale.y), "uniform");
        let top_left = t.to_screen(t.origin);
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
    }

    #[test]
    fn a_popup_keeps_its_size_with_its_corner_where_the_game_put_it() {
        let t = launcher();
        // A dropdown opened 100,50 from the launcher's corner.
        let popup = t.place(window(740, 397, 80, 120));
        let corner = t.to_screen(t.origin);
        assert!(close(popup.drawn.loc.x - corner.x, 100.0 * t.scale.x));
        assert!(close(popup.drawn.loc.y - corner.y, 50.0 * t.scale.y));
        assert_eq!(popup.drawn.size, Size::from((80.0, 120.0)), "its own size");
        assert_eq!(popup.footprint.loc, popup.drawn.loc);
        assert!(close(popup.footprint.size.w, 80.0 * t.scale.x));
    }

    /// A menu opened at the launcher's bottom-right corner would run off the screen.
    #[test]
    fn a_popup_past_an_edge_is_drawn_inside_the_output() {
        let t = launcher();
        let menu = t.place(window(1066, 600, 200, 80));
        assert!(menu.footprint.loc.x + 200.0 > OUTPUT.0 && menu.footprint.loc.y + 80.0 > OUTPUT.1);
        assert!(close(menu.drawn.loc.x + 200.0, OUTPUT.0), "flush right");
        assert!(close(menu.drawn.loc.y + 80.0, OUTPUT.1), "flush bottom");
        // Its last item, drawn at the bottom edge, still maps to the menu's own pixels.
        let item = Point::<f64, Logical>::from((36.0, 74.0));
        let back = menu.window_point(menu.to_screen(item));
        assert!(close(back.x, item.x) && close(back.y, item.y));
        // Above or left of the output it is pushed the other way.
        let high = t.place(window(600, 300, 50, 50));
        assert!(close(high.drawn.loc.x, 0.0) && close(high.drawn.loc.y, 0.0));
    }

    #[test]
    fn input_in_a_popup_maps_one_to_one() {
        let popup = launcher().place(window(740, 397, 80, 120));
        let corner = popup.drawn.loc;
        let local = popup.window_point(corner + Point::from((10.0, 20.0)));
        assert!(close(local.x, 10.0) && close(local.y, 20.0));
        assert_eq!(popup.location(), corner.to_i32_round());
    }

    #[test]
    fn the_game_under_a_popups_footprint_is_shadowed() {
        let t = launcher();
        let popup = t.place(window(740, 397, 80, 120));
        let on_popup = popup.drawn.loc + Point::from((10.0, 10.0));
        let beside = popup.drawn.loc + Point::from((100.0, 10.0));
        let away = popup.drawn.loc + Point::from((10.0, 500.0));
        assert!(
            !shadowed([&popup], on_popup),
            "the popup itself takes the click"
        );
        assert!(
            shadowed([&popup], beside),
            "X11 would hand this to the popup"
        );
        assert!(!shadowed([&popup], away), "the game takes the click");
        // Another popup drawn inside the first one's footprint still takes its clicks.
        let other = Placement {
            drawn: rect(beside.x - 5.0, beside.y - 5.0, 20.0, 20.0),
            footprint: rect(beside.x - 5.0, beside.y - 5.0, 20.0, 20.0),
        };
        assert!(!shadowed([&popup, &other], beside));
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
