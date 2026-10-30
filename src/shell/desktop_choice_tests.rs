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

#[test]
fn an_app_over_a_game_reuses_the_nearest_empty_desktop_after_it() {
    let h = handles(5);
    let desktops = [
        (h[0], true),
        (h[1], false),
        (h[2], false),
        (h[3], true),
        (h[4], true),
    ];
    assert_eq!(empty_desktop_beside(&desktops, &h[1]), Some(h[3]));
    assert_eq!(
        empty_desktop_beside(&desktops[..3], &h[1]),
        Some(h[0]),
        "else the nearest before"
    );
}

#[test]
fn an_app_over_a_game_with_no_empty_desktop_needs_a_new_one() {
    let h = handles(2);
    assert_eq!(
        empty_desktop_beside(&[(h[0], false), (h[1], false)], &h[0]),
        None
    );
    // The game's own desktop is never the app's, and a desktop of another set is no answer.
    assert_eq!(empty_desktop_beside(&[(h[0], true)], &h[0]), None);
    assert_eq!(empty_desktop_beside(&[(h[0], true)], &h[1]), None);
}

#[test]
fn only_an_apps_own_window_leaves_the_game() {
    let game = handles(1)[0];
    assert_eq!(over_game(true, Parent::None, game), OverGame::Beside);
    // Not the user's: the game's own, the launcher's, what the controller started.
    assert_eq!(over_game(false, Parent::None, game), OverGame::Stays);
}

#[test]
fn a_dialog_over_the_game_goes_with_its_parent_and_takes_no_screen() {
    let h = handles(2);
    let (game, other) = (h[0], h[1]);
    for app in [true, false] {
        // The game's or the launcher's dialog stays over the game.
        assert_eq!(over_game(app, Parent::On(game), game), OverGame::Stays);
        // An app's dialog joins the app on its desktop, without switching to it.
        assert_eq!(
            over_game(app, Parent::On(other), game),
            OverGame::WithParent(other)
        );
        // A parent on no desktop here, or a view embedded in another surface, moves nothing.
        assert_eq!(over_game(app, Parent::Elsewhere, game), OverGame::Stays);
    }
}
