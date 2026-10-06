use super::*;
use smithay::{
    output::{PhysicalProperties, Subpixel},
    reexports::wayland_server::Display,
};

fn handles(count: usize) -> Vec<WorkspaceHandle> {
    let display = Display::<State>::new().unwrap();
    let mut state = WorkspaceState::<State>::new(&display.handle(), |_| true);
    let output = Output::new(
        "eDP-test".into(),
        PhysicalProperties {
            size: (300, 200).into(),
            subpixel: Subpixel::Unknown,
            make: "test".into(),
            model: "test".into(),
            serial_number: String::new(),
        },
    );
    let mut set = WorkspaceSet::new(
        &mut state.update(),
        &output,
        false,
        &crate::comp_theme::CompTheme::default(),
        AppearanceConfig::default(),
    );
    for _ in 0..count {
        set.add_empty_workspace(&mut state.update());
    }
    set.workspaces.iter().map(|w| w.handle).collect()
}

#[test]
fn desktops_are_named_as_the_design_names_them() {
    assert_eq!(desktop_label(0, None), "Main");
    assert_eq!(desktop_label(2, None), "Desktop 3");
    assert_eq!(desktop_label(1, Some("Rooftop fight")), "Rooftop fight");
    assert_eq!(desktop_label(1, Some("")), "Desktop 2");
}

#[test]
fn the_chooser_offers_every_desktop_but_the_spare() {
    let h = handles(3);
    let desktops = [
        (h[0], None, 2),
        (h[1], Some("Notes".to_owned()), 1),
        (h[2], None, 0),
    ];
    let dynamic = desktop_choices(&desktops, &h[1], true);
    assert_eq!(dynamic.len(), 2);
    assert_eq!(dynamic[0].label, "Main");
    assert!(!dynamic[0].current && dynamic[1].current);
    assert_eq!(
        (dynamic[1].label.as_str(), dynamic[1].windows),
        ("Notes", 1)
    );
    // With desktops the user keeps, an empty one is a destination like any other.
    assert_eq!(desktop_choices(&desktops, &h[1], false).len(), 3);
}
