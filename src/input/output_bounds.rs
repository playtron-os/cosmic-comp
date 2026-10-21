// SPDX-License-Identifier: GPL-3.0-only

use crate::{shell::SeatExt, state::State, utils::prelude::*};
use smithay::{
    input::pointer::MotionEvent,
    utils::{Point, Rectangle, SERIAL_COUNTER},
};

impl State {
    /// A pointer left where an output used to be is invisible and points at nothing until the
    /// next motion; bring it back to the nearest output now.
    pub fn keep_pointers_on_outputs(&mut self) {
        let shell = self.common.shell.read();
        let outputs = shell.outputs().cloned().collect::<Vec<_>>();
        let seats = shell.seats.iter().cloned().collect::<Vec<_>>();
        drop(shell);

        let geometries = outputs
            .iter()
            .map(|output| output.geometry().to_f64())
            .collect::<Vec<_>>();
        for seat in seats {
            let Some(pointer) = seat.get_pointer() else {
                continue;
            };
            let Some((index, location)) =
                pointer_home(pointer.current_location().as_global(), &geometries)
            else {
                continue;
            };
            let output = &outputs[index];
            seat.set_active_output(output);
            let under = State::surface_under(location, output, &self.common.shell.read())
                .map(|(target, pos)| (target, pos.as_logical()));
            pointer.motion(
                self,
                under,
                &MotionEvent {
                    location: location.as_logical(),
                    serial: SERIAL_COUNTER.next_serial(),
                    time: self.common.clock.now().as_millis(),
                },
            );
            pointer.frame(self);
            self.common
                .shell
                .write()
                .update_pointer_position(location.to_local(output), output);
        }
    }
}

/// For a pointer outside every output, the nearest output and its centre. None while it is on one.
fn pointer_home(
    position: Point<f64, Global>,
    outputs: &[Rectangle<f64, Global>],
) -> Option<(usize, Point<f64, Global>)> {
    if outputs.iter().any(|geometry| geometry.contains(position)) {
        return None;
    }
    let distance = |geometry: &Rectangle<f64, Global>| {
        let dx = (geometry.loc.x - position.x)
            .max(position.x - (geometry.loc.x + geometry.size.w))
            .max(0.);
        let dy = (geometry.loc.y - position.y)
            .max(position.y - (geometry.loc.y + geometry.size.h))
            .max(0.);
        dx * dx + dy * dy
    };
    outputs
        .iter()
        .enumerate()
        .min_by(|(_, a), (_, b)| distance(a).total_cmp(&distance(b)))
        .map(|(index, geometry)| {
            let centre = geometry.loc + geometry.size.downscale(2.).to_point();
            (index, centre)
        })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn rect(x: f64, y: f64, w: f64, h: f64) -> Rectangle<f64, Global> {
        Rectangle::new((x, y).into(), (w, h).into())
    }

    #[test]
    fn a_pointer_on_an_output_stays_where_it_is() {
        let outputs = [rect(0., 0., 1920., 1080.), rect(1920., 0., 2560., 1440.)];
        assert_eq!(pointer_home((2000., 1200.).into(), &outputs), None);
    }

    /// The monitor right of the built-in one went away while the pointer was on it.
    #[test]
    fn a_pointer_left_on_an_unplugged_output_goes_to_the_nearest_one() {
        let outputs = [rect(0., 0., 1920., 1080.)];
        assert_eq!(
            pointer_home((3000., 500.).into(), &outputs),
            Some((0, (960., 540.).into()))
        );
    }

    #[test]
    fn the_nearest_of_several_outputs_takes_the_pointer() {
        let outputs = [rect(0., 0., 1920., 1080.), rect(0., 1080., 1280., 800.)];
        assert_eq!(
            pointer_home((100., 2500.).into(), &outputs),
            Some((1, (640., 1480.).into()))
        );
    }

    #[test]
    fn without_outputs_the_pointer_stays() {
        assert_eq!(pointer_home((10., 10.).into(), &[]), None);
    }
}
