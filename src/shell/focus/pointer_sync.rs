// SPDX-License-Identifier: GPL-3.0-only

use crate::utils::prelude::*;
use smithay::utils::{Logical, Point, Rectangle};
use std::sync::Mutex;

use super::target::PointerFocusTarget;

/// What the pointer was over at the last focus refresh, with that surface's origin.
#[derive(Default)]
pub(super) struct LastPointerUnder(pub Mutex<Option<(PointerFocusTarget, Point<f64, Logical>)>>);

/// Whether the surface under the pointer is the one there last time, but moved, so the
/// surface-local position its client holds is stale.
pub(super) fn moved_under<T: PartialEq>(
    last: Option<&(T, Point<f64, Logical>)>,
    under: Option<&(T, Point<f64, Logical>)>,
) -> bool {
    matches!((last, under), (Some((was, from)), Some((target, to))) if was == target && from != to)
}

/// For a pointer outside every output, the nearest point on one of them and its index.
pub(super) fn clamp_into(
    position: Point<f64, Global>,
    outputs: &[Rectangle<f64, Global>],
) -> Option<(usize, Point<f64, Global>)> {
    if outputs.iter().any(|geometry| geometry.contains(position)) {
        return None;
    }
    outputs
        .iter()
        .enumerate()
        .filter(|(_, geometry)| geometry.size.w > 0. && geometry.size.h > 0.)
        .map(|(index, geometry)| {
            let max = geometry.loc + geometry.size.to_point();
            let point = Point::<f64, Global>::from((
                position.x.clamp(geometry.loc.x, max.x.next_down()),
                position.y.clamp(geometry.loc.y, max.y.next_down()),
            ));
            let distance = (point.x - position.x).powi(2) + (point.y - position.y).powi(2);
            (index, point, distance)
        })
        .min_by(|(_, _, a), (_, _, b)| a.total_cmp(b))
        .map(|(index, point, _)| (index, point))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn rect(x: f64, y: f64, w: f64, h: f64) -> Rectangle<f64, Global> {
        Rectangle::new((x, y).into(), (w, h).into())
    }

    fn at(x: f64, y: f64) -> Point<f64, Logical> {
        (x, y).into()
    }

    /// The scale-change bug: the popover re-laid out under a still pointer, and a click
    /// landed on the old row because nothing told the client.
    #[test]
    fn a_surface_that_moves_under_a_still_pointer_gets_one_motion() {
        let popover = 7;
        let mut last = Some((popover, at(1200., 40.)));
        let under = Some((popover, at(1440., 40.)));

        assert!(moved_under(last.as_ref(), under.as_ref()));
        last = under;
        assert!(!moved_under(last.as_ref(), under.as_ref()));
    }

    #[test]
    fn a_still_surface_under_a_still_pointer_gets_none() {
        let under = Some((1, at(100., 100.)));
        assert!(!moved_under(under.as_ref(), under.as_ref()));
        assert!(!moved_under::<i32>(None, None));
    }

    /// Input sent fresh coordinates when it moved the pointer onto this surface or off one.
    #[test]
    fn another_surface_is_not_a_move() {
        let (old, new) = (1, 2);
        let under = Some((new, at(300., 0.)));
        assert!(!moved_under(Some(&(old, at(0., 0.))), under.as_ref()));
        assert!(!moved_under(None, under.as_ref()));
        assert!(!moved_under(under.as_ref(), None));
    }

    #[test]
    fn a_pointer_on_an_output_stays() {
        let outputs = [rect(0., 0., 1280., 800.), rect(1280., 0., 1920., 1080.)];
        assert_eq!(clamp_into((2000., 900.).into(), &outputs), None);
    }

    /// 1920x1200 going from 125 % to 150 % shrinks the desktop from 1536x960 to 1280x800.
    #[test]
    fn a_shrunk_output_pulls_the_pointer_to_its_nearest_edge() {
        let outputs = [rect(0., 0., 1280., 800.)];
        let (index, point) = clamp_into((1500., 400.).into(), &outputs).unwrap();
        assert_eq!(index, 0);
        assert_eq!(point.y, 400.);
        assert!(point.x < 1280. && point.x > 1279.9);
        assert!(outputs[0].contains(point));
    }

    #[test]
    fn the_nearest_of_several_outputs_takes_the_pointer() {
        let outputs = [rect(0., 0., 1280., 800.), rect(1280., 0., 1600., 900.)];
        let (index, point) = clamp_into((1400., 950.).into(), &outputs).unwrap();
        assert_eq!(index, 1);
        assert_eq!(point.x, 1400.);
        assert!(outputs[1].contains(point));
    }

    #[test]
    fn without_outputs_the_pointer_stays() {
        assert_eq!(clamp_into((10., 10.).into(), &[]), None);
        assert_eq!(clamp_into((10., 10.).into(), &[rect(0., 0., 0., 0.)]), None);
    }
}
