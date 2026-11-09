// SPDX-License-Identifier: GPL-3.0-only

//! Keeping a mode's shape on a panel of another aspect ratio: the panel stays in its
//! native timing and the compositor scales the picture, so a monitor's scaler (which
//! stretches to fill) never sees a timing of the wrong shape.

use smithay::utils::{Logical, Physical, Point, Rectangle, Size, Transform};

/// 3440x1440 and 2560x1080 are both "21:9", and 1366x768 is 16:9 in practice.
const SAME_ASPECT_TOLERANCE: f64 = 0.015;

/// A timing a connector offers, in the terms mode selection needs.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Timing {
    pub size: Size<i32, Physical>,
    /// In mHz, like `drm_helpers::calculate_refresh_rate`.
    pub refresh: u32,
    pub preferred: bool,
}

/// Where a mode's picture sits on a panel it does not match in shape.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Letterbox {
    panel: Size<i32, Physical>,
    picture: Rectangle<i32, Physical>,
}

impl Letterbox {
    /// `None` when the monitor can show `mode` as it is: same shape, or a mode bigger
    /// than the panel, which only the monitor can do anything with.
    pub fn new(mode: Size<i32, Physical>, panel: Size<i32, Physical>) -> Option<Self> {
        if mode.w <= 0 || mode.h <= 0 || mode.w > panel.w || mode.h > panel.h {
            return None;
        }
        if same_aspect(mode, panel) {
            return None;
        }

        let scale = f64::min(
            panel.w as f64 / mode.w as f64,
            panel.h as f64 / mode.h as f64,
        );
        let size = Size::<i32, Physical>::from((
            (mode.w as f64 * scale).round() as i32,
            (mode.h as f64 * scale).round() as i32,
        ));
        let loc = Point::from(((panel.w - size.w) / 2, (panel.h - size.h) / 2));

        Some(Self {
            panel,
            picture: Rectangle::new(loc, size),
        })
    }

    pub fn panel(&self) -> Size<i32, Physical> {
        self.panel
    }

    /// The panel as the rendered output sees it.
    pub fn panel_transformed(&self, transform: Transform) -> Size<i32, Physical> {
        transform.transform_size(self.panel)
    }

    /// The picture on the panel, in panel pixels before the output's transform.
    pub fn picture(&self) -> Rectangle<i32, Physical> {
        self.picture
    }

    /// Maps a position on the whole panel, in a `size`-sized space, into the picture.
    /// A touch or pen on the bars lands on the picture's nearest edge.
    pub fn panel_to_picture(
        &self,
        point: Point<f64, Logical>,
        size: Size<f64, Logical>,
    ) -> Point<f64, Logical> {
        let axis = |pos: f64, extent: f64, panel: i32, start: i32, length: i32| {
            let on_panel = pos / extent * panel as f64;
            ((on_panel - start as f64) / length as f64).clamp(0.0, 1.0) * extent
        };
        Point::from((
            axis(
                point.x,
                size.w,
                self.panel.w,
                self.picture.loc.x,
                self.picture.size.w,
            ),
            axis(
                point.y,
                size.h,
                self.panel.h,
                self.picture.loc.y,
                self.picture.size.h,
            ),
        ))
    }
}

fn same_aspect(a: Size<i32, Physical>, b: Size<i32, Physical>) -> bool {
    let a = a.w as f64 / a.h as f64;
    let b = b.w as f64 / b.h as f64;
    (a - b).abs() / b <= SAME_ASPECT_TOLERANCE
}

/// The timing to drive the panel with when `requested` must be letterboxed, as an
/// index into `timings`; `None` to modeset to `requested` itself. The refresh is the
/// closest the panel offers at its native size, which can be below a low resolution's.
pub fn panel_timing(timings: &[Timing], requested: Timing) -> Option<usize> {
    let native = timings
        .iter()
        .find(|timing| timing.preferred)
        .or_else(|| timings.first())?
        .size;
    Letterbox::new(requested.size, native)?;

    timings
        .iter()
        .enumerate()
        .filter(|(_, timing)| timing.size == native)
        .min_by_key(|(_, timing)| {
            (
                timing.refresh.abs_diff(requested.refresh),
                std::cmp::Reverse(timing.refresh),
            )
        })
        .map(|(index, _)| index)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::prelude::OutputExt;
    use smithay::{
        backend::renderer::{
            Color32F,
            element::{
                Element, Id, Kind,
                solid::SolidColorRenderElement,
                utils::{ConstrainAlign, ConstrainScaleBehavior, constrain_render_elements},
            },
            utils::CommitCounter,
        },
        output::{Mode, Output, OutputModeSource, PhysicalProperties, Subpixel},
    };

    fn timing(w: i32, h: i32, hz: u32, preferred: bool) -> Timing {
        Timing {
            size: (w, h).into(),
            refresh: hz * 1000,
            preferred,
        }
    }

    fn letterbox(mode: (i32, i32), panel: (i32, i32)) -> Option<Letterbox> {
        Letterbox::new(mode.into(), panel.into())
    }

    #[test]
    fn a_16_9_mode_on_a_21_9_panel_gets_bars_at_the_sides() {
        let boxed = letterbox((1920, 1080), (3440, 1440)).unwrap();

        assert_eq!(
            boxed.picture(),
            Rectangle::new((440, 0).into(), (2560, 1440).into())
        );
        // The bars are equal, and the picture keeps 16:9.
        let picture = boxed.picture();
        assert_eq!(picture.loc.x, 3440 - picture.loc.x - picture.size.w);
        assert_eq!(picture.size.w * 9, picture.size.h * 16);
    }

    #[test]
    fn a_21_9_mode_on_a_16_9_panel_gets_bars_above_and_below() {
        let boxed = letterbox((2560, 1080), (3840, 2160)).unwrap();

        assert_eq!(
            boxed.picture(),
            Rectangle::new((0, 270).into(), (3840, 1620).into())
        );
    }

    #[test]
    fn a_5_4_mode_on_a_16_9_panel_gets_bars_at_the_sides() {
        let boxed = letterbox((1280, 1024), (1920, 1080)).unwrap();

        assert_eq!(
            boxed.picture(),
            Rectangle::new((285, 0).into(), (1350, 1080).into())
        );
    }

    #[test]
    fn native_and_same_shape_modes_are_left_to_the_monitor() {
        assert_eq!(letterbox((3440, 1440), (3440, 1440)), None);
        assert_eq!(letterbox((1920, 1080), (3840, 2160)), None);
        assert_eq!(letterbox((1366, 768), (1920, 1080)), None);
        // "21:9" modes differ by under a percent.
        assert_eq!(letterbox((2560, 1080), (3440, 1440)), None);
    }

    #[test]
    fn a_mode_larger_than_the_panel_is_left_to_the_monitor() {
        assert_eq!(letterbox((4096, 2160), (1920, 1080)), None);
    }

    #[test]
    fn the_panel_transforms_with_the_output() {
        let boxed = letterbox((1920, 1080), (3440, 1440)).unwrap();

        assert_eq!(
            boxed.panel_transformed(Transform::Normal),
            (3440, 1440).into()
        );
        assert_eq!(boxed.panel_transformed(Transform::_90), (1440, 3440).into());
    }

    #[test]
    fn a_touch_maps_into_the_picture_and_not_the_bars() {
        let boxed = letterbox((1920, 1080), (3440, 1440)).unwrap();
        let size = Size::<f64, Logical>::from((1920.0, 1080.0));

        // The picture's centre stays the centre of the panel.
        let center = boxed.panel_to_picture((960.0, 540.0).into(), size);
        assert!((center.x - 960.0).abs() < 1e-6 && (center.y - 540.0).abs() < 1e-6);

        // The picture's left edge is 440 of 3440 panel pixels in.
        let left = boxed.panel_to_picture(((440.0 / 3440.0) * 1920.0, 540.0).into(), size);
        assert!(left.x.abs() < 1e-6);

        // The bars clamp to the nearest edge of the picture.
        let bar = boxed.panel_to_picture((10.0, 540.0).into(), size);
        assert_eq!(bar.x, 0.0);
        let bar = boxed.panel_to_picture((1915.0, 540.0).into(), size);
        assert_eq!(bar.x, size.w);
    }

    fn drawn_picture(
        mode: (i32, i32),
        panel: (i32, i32),
        transform: Transform,
    ) -> Rectangle<i32, Physical> {
        let letterbox = letterbox(mode, panel).unwrap();
        let reference = Rectangle::from_size(transform.transform_size(Size::from(mode)));
        let element = SolidColorRenderElement::new(
            Id::new(),
            reference,
            CommitCounter::default(),
            Color32F::BLACK,
            Kind::Unspecified,
        );
        constrain_render_elements(
            [element],
            (0, 0),
            Rectangle::from_size(letterbox.panel_transformed(transform)),
            reference,
            ConstrainScaleBehavior::Fit,
            ConstrainAlign::CENTER,
            1.0,
        )
        .next()
        .unwrap()
        .geometry(1.0.into())
    }

    /// Input maps onto `picture`, so what the render pass draws has to be that rectangle.
    #[test]
    fn the_drawn_picture_is_where_input_lands() {
        for (mode, panel) in [
            ((1920, 1080), (3440, 1440)),
            ((2560, 1080), (3840, 2160)),
            ((1280, 1024), (1920, 1080)),
        ] {
            let picture = letterbox(mode, panel).unwrap().picture();
            assert_eq!(drawn_picture(mode, panel, Transform::Normal), picture);
        }
    }

    #[test]
    fn a_rotated_output_keeps_its_bars_on_the_rotated_sides() {
        assert_eq!(
            drawn_picture((1920, 1080), (3440, 1440), Transform::_90),
            Rectangle::new((0, 440).into(), (1440, 2560).into())
        );
    }

    fn output(mode: (i32, i32)) -> Output {
        let output = Output::new(
            "DP-1".into(),
            PhysicalProperties {
                size: (800, 340).into(),
                subpixel: Subpixel::Unknown,
                make: String::new(),
                model: String::new(),
                serial_number: String::new(),
            },
        );
        let mode = Mode {
            size: mode.into(),
            refresh: 60_000,
        };
        output.add_mode(mode);
        output.change_current_state(Some(mode), Some(Transform::_90), None, None);
        output
    }

    #[test]
    fn a_letterboxed_output_draws_into_the_panel() {
        let output = output((1920, 1080));
        output.set_scanout_panel(Some((3440, 1440).into()));

        assert_eq!(
            output.drm_mode_source(),
            OutputModeSource::Static {
                size: (3440, 1440).into(),
                scale: 1.0.into(),
                transform: Transform::_90,
            }
        );
    }

    #[test]
    fn an_output_without_a_panel_follows_its_mode() {
        let output = output((1920, 1080));
        assert_eq!(output.letterbox(), None);
        assert_eq!(output.drm_mode_source(), OutputModeSource::from(&output));

        // Leaving a letterboxed mode has to hand the buffer back to the output's mode.
        output.set_scanout_panel(Some((3440, 1440).into()));
        output.set_scanout_panel(None);
        assert_eq!(output.drm_mode_source(), OutputModeSource::from(&output));
    }

    #[test]
    fn a_panel_matching_the_mode_is_not_a_letterbox() {
        let output = output((3440, 1440));
        output.set_scanout_panel(Some((3440, 1440).into()));

        assert_eq!(output.letterbox(), None);
        assert_eq!(output.drm_mode_source(), OutputModeSource::from(&output));
    }

    #[test]
    fn panel_timing_picks_the_native_size_at_the_closest_refresh() {
        let timings = [
            timing(3440, 1440, 100, true),
            timing(3440, 1440, 60, false),
            timing(1920, 1080, 120, false),
            timing(1920, 1080, 60, false),
        ];

        // Settings picks a resolution's highest refresh, which the panel may not reach.
        assert_eq!(
            panel_timing(&timings, timing(1920, 1080, 120, false)),
            Some(0)
        );
        assert_eq!(
            panel_timing(&timings, timing(1920, 1080, 60, false)),
            Some(1)
        );
    }

    #[test]
    fn panel_timing_prefers_the_higher_refresh_on_a_tie() {
        let timings = [
            timing(3440, 1440, 60, true),
            timing(3440, 1440, 100, false),
            timing(1920, 1080, 80, false),
        ];

        assert_eq!(
            panel_timing(&timings, timing(1920, 1080, 80, false)),
            Some(1)
        );
    }

    #[test]
    fn panel_timing_leaves_native_and_same_shape_modes_alone() {
        let timings = [
            timing(3840, 2160, 60, true),
            timing(1920, 1080, 120, false),
            timing(3840, 2160, 30, false),
        ];

        assert_eq!(panel_timing(&timings, timing(3840, 2160, 60, true)), None);
        assert_eq!(panel_timing(&timings, timing(1920, 1080, 120, false)), None);
    }

    #[test]
    fn panel_timing_without_a_preferred_mode_takes_the_first() {
        let timings = [timing(3440, 1440, 60, false), timing(1920, 1080, 60, false)];

        assert_eq!(
            panel_timing(&timings, timing(1920, 1080, 60, false)),
            Some(0)
        );
    }
}
