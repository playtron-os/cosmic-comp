use super::*;
use smithay::output::{Mode, PhysicalProperties, Subpixel};

fn display(name: &str, x: i32) -> Output {
    let output = Output::new(
        name.into(),
        PhysicalProperties {
            size: (300, 200).into(),
            subpixel: Subpixel::Unknown,
            make: "test".into(),
            model: "test".into(),
            serial_number: String::new(),
        },
    );
    let mode = Mode {
        size: (1920, 1080).into(),
        refresh: 60_000,
    };
    output.change_current_state(Some(mode), None, None, Some((x, 0).into()));
    output
}

/// Keyboard focus took the active output to the second display while the pointer stayed on the first.
#[test]
fn a_surface_that_follows_the_pointer_shows_under_it() {
    let (first, second) = (display("A", 0), display("B", 1920));
    let outputs = [first.clone(), second.clone()];
    let pointer = Some((400., 300.).into());
    assert_eq!(
        agnostic_layer_output(true, pointer, outputs.iter(), second.clone()),
        first
    );
    let pointer = Some((2500., 300.).into());
    assert_eq!(
        agnostic_layer_output(true, pointer, outputs.iter(), first.clone()),
        second
    );
}

/// The launcher, dialogs and every surface that did not ask stay where keyboard focus put them.
#[test]
fn any_other_surface_shows_on_the_active_output() {
    let (first, second) = (display("A", 0), display("B", 1920));
    let outputs = [first.clone(), second.clone()];
    let pointer = Some((400., 300.).into());
    assert_eq!(
        agnostic_layer_output(false, pointer, outputs.iter(), second.clone()),
        second
    );
}

#[test]
fn without_a_pointer_on_an_output_the_active_output_stands() {
    let (first, second) = (display("A", 0), display("B", 1920));
    let outputs = [first.clone(), second.clone()];
    assert_eq!(
        agnostic_layer_output(
            true,
            Some((5000., 300.).into()),
            outputs.iter(),
            second.clone()
        ),
        second
    );
    assert_eq!(
        agnostic_layer_output(true, None, outputs.iter(), first.clone()),
        first
    );
}
