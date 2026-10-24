//! One transform for a game's windows in game mode. The game's window and every window
//! of the same game are drawn at the game's own coordinates, moved and scaled by the same
//! amounts about the game window's corner, so a menu the game opens at (x, y) from its
//! window lands at scale × (x, y) from the scaled window. Input maps back the same way.

use smithay::{
    output::Output,
    utils::{Coordinate, Logical, Physical, Point, Rectangle, Scale, Size},
};

use crate::utils::geometry::{Global, PointGlobalExt, PointLocalExt};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct GroupTransform {
    /// The game window's corner, where X11 has placed it.
    pub origin: Point<f64, Global>,
    /// Where that corner is drawn.
    pub destination: Point<f64, Global>,
    pub scale: Scale<f64>,
}

impl GroupTransform {
    /// The transform that draws a window of `size` at `origin` into `presented`.
    pub fn new(
        origin: Point<f64, Global>,
        size: Size<i32, Logical>,
        presented: Rectangle<f64, Global>,
    ) -> Option<Self> {
        (size.w > 0 && size.h > 0 && !presented.is_empty()).then(|| Self {
            origin,
            destination: presented.loc,
            scale: Scale {
                x: presented.size.w / f64::from(size.w),
                y: presented.size.h / f64::from(size.h),
            },
        })
    }

    /// Whether drawing through this transform changes nothing, so windows need no wrapping.
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

    /// The point of the game's coordinate space drawn at `point`: the inverse of [`Self::to_screen`].
    pub fn to_game(&self, point: Point<f64, Global>) -> Point<f64, Global> {
        let offset = point - self.destination;
        self.origin + Point::from((offset.x / self.scale.x, offset.y / self.scale.y))
    }

    /// Where a window the game placed at `rect` is drawn.
    pub fn window_rect<C: Coordinate>(&self, rect: Rectangle<C, Global>) -> Rectangle<f64, Global> {
        let rect = rect.to_f64();
        Rectangle::new(
            self.to_screen(rect.loc),
            Size::from((rect.size.w * self.scale.x, rect.size.h * self.scale.y)),
        )
    }

    /// Whether a window of `size` still fits `output` once scaled. One that would not
    /// (a login window larger than the game) is shown at its own size instead of cropped.
    pub fn fits(&self, size: Size<i32, Logical>, output: Size<i32, Global>) -> bool {
        let output = output.to_f64();
        f64::from(size.w) * self.scale.x <= output.w + 0.5
            && f64::from(size.h) * self.scale.y <= output.h + 0.5
    }

    /// A pointer at `point` on screen, in the coordinates of the game's window at `loc`.
    pub fn window_point(
        &self,
        point: Point<f64, Global>,
        loc: Point<i32, Global>,
    ) -> Point<f64, Logical> {
        (self.to_game(point) - loc.to_f64()).as_logical()
    }

    /// The physical position on `output` to draw a window placed at `loc`.
    pub fn render_origin(
        &self,
        loc: Point<i32, Global>,
        output: &Output,
        output_scale: f64,
    ) -> Point<i32, Physical> {
        self.to_screen(loc.to_f64())
            .to_local(output)
            .as_logical()
            .to_physical_precise_round(output_scale)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn rect(x: f64, y: f64, w: f64, h: f64) -> Rectangle<f64, Global> {
        Rectangle::new(Point::from((x, y)), Size::from((w, h)))
    }

    /// The 640x400 launcher at 150% (427x267 logical) centred on a 1707x960 output,
    /// fitted to its height.
    fn launcher() -> GroupTransform {
        let origin = Point::from((640.0, 347.0));
        let size = Size::from((427, 267));
        let height = 960.0;
        let width = 427.0 * height / 267.0;
        GroupTransform::new(
            origin,
            size,
            rect((1707.0 - width) / 2.0, 0.0, width, height),
        )
        .unwrap()
    }

    #[test]
    fn fits_the_output_keeping_the_aspect_ratio() {
        let t = launcher();
        assert!((t.scale.x - t.scale.y).abs() < 1e-9, "uniform");
        let drawn = t.window_rect(Rectangle::<i32, Global>::new(
            Point::from((640, 347)),
            Size::from((427, 267)),
        ));
        assert!((drawn.size.h - 960.0).abs() < 1e-9, "fills the height");
        assert!(drawn.loc.y.abs() < 1e-9);
        let left = drawn.loc.x;
        let right = 1707.0 - (drawn.loc.x + drawn.size.w);
        assert!((left - right).abs() < 1e-9, "centred between equal bars");
        assert!(left > 0.0, "letterboxed at the sides");
    }

    #[test]
    fn a_popup_keeps_its_place_on_the_game_window() {
        let t = launcher();
        // A menu opened 100,50 from the launcher's corner.
        let menu = Rectangle::<i32, Global>::new(Point::from((740, 397)), Size::from((80, 120)));
        let drawn = t.window_rect(menu);
        let corner = t.to_screen(t.origin);
        assert_eq!(corner, t.destination);
        assert!((drawn.loc.x - corner.x - 100.0 * t.scale.x).abs() < 1e-9);
        assert!((drawn.loc.y - corner.y - 50.0 * t.scale.y).abs() < 1e-9);
        assert!((drawn.size.w - 80.0 * t.scale.x).abs() < 1e-9);
        assert!((drawn.size.h - 120.0 * t.scale.y).abs() < 1e-9);
    }

    #[test]
    fn input_maps_back_through_the_inverse() {
        let t = launcher();
        for point in [(0.0, 0.0), (853.5, 480.0), (1706.0, 959.0), (-10.0, 2000.0)] {
            let point = Point::<f64, Global>::from(point);
            let back = t.to_screen(t.to_game(point));
            assert!((back.x - point.x).abs() < 1e-9 && (back.y - point.y).abs() < 1e-9);
        }
        // A click drawn 10,20 into a menu at 740,397 lands 10,20 scaled-back into it.
        let menu = Point::<i32, Global>::from((740, 397));
        let click = t.to_screen(menu.to_f64()) + Point::from((10.0 * t.scale.x, 20.0 * t.scale.y));
        let local = t.window_point(click, menu);
        assert!((local.x - 10.0).abs() < 1e-9 && (local.y - 20.0).abs() < 1e-9);
    }

    #[test]
    fn a_window_drawn_where_it_is_is_the_identity() {
        let at = rect(0.0, 0.0, 1707.0, 960.0);
        let native = GroupTransform::new(at.loc, Size::from((1707, 960)), at).unwrap();
        assert!(native.is_identity());
        let launcher = launcher();
        assert!(!launcher.is_identity());
        assert!(
            GroupTransform::new(at.loc, Size::from((0, 960)), at).is_none(),
            "an empty window"
        );
    }

    #[test]
    fn only_windows_that_fit_once_scaled_follow() {
        let t = launcher();
        let output = Size::from((1707, 960));
        assert!(
            t.fits(Size::from((427, 267)), output),
            "the game window itself"
        );
        assert!(t.fits(Size::from((80, 120)), output), "a menu");
        assert!(!t.fits(Size::from((800, 600)), output), "a login window");
    }

    #[test]
    fn stretch_scales_each_axis_on_its_own() {
        let t = GroupTransform::new(
            Point::from((0.0, 0.0)),
            Size::from((640, 400)),
            rect(0.0, 0.0, 1920.0, 1080.0),
        )
        .unwrap();
        let drawn = t.window_rect(Rectangle::<i32, Global>::new(
            Point::from((320, 200)),
            Size::from((10, 10)),
        ));
        assert_eq!(drawn, rect(960.0, 540.0, 30.0, 27.0));
    }
}
