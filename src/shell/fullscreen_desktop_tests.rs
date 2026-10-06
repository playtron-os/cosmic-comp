use super::*;
use smithay::{
    output::{Mode, PhysicalProperties, Subpixel},
    reexports::wayland_server::Display,
};

fn with_desktops(
    count: usize,
    test: impl FnOnce(&mut WorkspaceSet, &mut WorkspaceState<State>, &XdgActivationState),
) {
    let display = Display::<State>::new().unwrap();
    let dh = display.handle();
    let mut state = WorkspaceState::<State>::new(&dh, |_| true);
    let activation = XdgActivationState::new::<State>(&dh);
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
    output.change_current_state(
        Some(Mode {
            size: (1280, 800).into(),
            refresh: 60_000,
        }),
        None,
        None,
        None,
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
    test(&mut set, &mut state, &activation);
}

fn facts(desktop: usize, active: usize) -> FullscreenDesktopFacts {
    FullscreenDesktopFacts {
        owner_here: false,
        leaving: false,
        desktop,
        home: Some(desktop - 1),
        len: 3,
        active,
        previously_active: None,
        on_screen: true,
    }
}

/// The desktop goes right after the one on screen, so the previous-desktop
/// gesture walks straight back, and it is named after its window.
#[test]
fn a_fullscreen_desktop_is_minted_beside_the_one_on_screen() {
    with_desktops(3, |set, state, _| {
        set.active = 1;
        let after = set.workspaces[2].handle;
        let handle = set.insert_workspace(2, "Rooftop fight".into(), &mut state.update());
        assert_eq!(set.workspaces[2].handle, handle);
        assert_eq!(set.workspaces[2].name.as_deref(), Some("Rooftop fight"));
        assert_eq!(set.workspaces[3].handle, after);
        assert_eq!(set.active, 1);
    });
}

#[test]
fn inserting_before_the_active_desktop_keeps_it_and_its_animation_on_screen() {
    with_desktops(3, |set, state, _| {
        set.active = 2;
        set.previously_active = Some((1, WorkspaceDelta::new_shortcut()));
        let active = set.workspaces[2].handle;
        let previous = set.workspaces[1].handle;
        set.insert_workspace(0, "Notes".into(), &mut state.update());
        assert_eq!(set.workspaces[set.active].handle, active);
        let (previous_idx, _) = set.previously_active.unwrap();
        assert_eq!(set.workspaces[previous_idx].handle, previous);
    });
}

/// With dynamic desktops an empty desktop is pruned, but a fullscreen window's
/// home must still be there when it comes back.
#[test]
fn an_empty_home_waits_for_its_fullscreen_window() {
    with_desktops(3, |set, state, activation| {
        set.active = 2;
        let home = set.workspaces[0].handle;
        set.ensure_last_empty(&mut state.update(), activation, &[home]);
        assert!(set.workspaces.iter().any(|w| w.handle == home));
        set.ensure_last_empty(&mut state.update(), activation, &[]);
        assert!(!set.workspaces.iter().any(|w| w.handle == home));
    });
}

#[test]
fn a_desktop_whose_window_is_still_fullscreen_stays() {
    let mut here = facts(2, 2);
    here.owner_here = true;
    assert_eq!(fullscreen_desktop_step(here), FullscreenDesktopStep::Keep);
    here.on_screen = false;
    assert_eq!(fullscreen_desktop_step(here), FullscreenDesktopStep::Keep);
}

/// Leaving fullscreen, parking or closing on screen switches home first, so
/// the window is seen arriving there.
#[test]
fn leaving_a_fullscreen_desktop_on_screen_shows_the_home() {
    let mut on_screen = facts(2, 2);
    on_screen.home = Some(0);
    assert_eq!(
        fullscreen_desktop_step(on_screen),
        FullscreenDesktopStep::Show(0)
    );
    on_screen.home = None;
    assert_eq!(
        fullscreen_desktop_step(on_screen),
        FullscreenDesktopStep::Show(1),
        "a home that is gone falls back to the desktop before"
    );
    let first = FullscreenDesktopFacts {
        desktop: 0,
        home: None,
        active: 0,
        ..facts(1, 1)
    };
    assert_eq!(
        fullscreen_desktop_step(first),
        FullscreenDesktopStep::Show(1)
    );
}

#[test]
fn a_fullscreen_desktop_folds_once_its_slide_and_exit_animation_end() {
    let mut sliding = facts(2, 1);
    sliding.previously_active = Some(2);
    assert_eq!(
        fullscreen_desktop_step(sliding),
        FullscreenDesktopStep::Wait
    );
    let mut leaving = facts(2, 1);
    leaving.leaving = true;
    assert_eq!(
        fullscreen_desktop_step(leaving),
        FullscreenDesktopStep::Wait
    );
    assert_eq!(
        fullscreen_desktop_step(facts(2, 1)),
        FullscreenDesktopStep::Fold(1)
    );
}

/// A realm off screen has nothing to show; its fullscreen desktop just folds.
#[test]
fn a_fullscreen_desktop_in_another_workspace_folds_without_switching() {
    let mut elsewhere = facts(2, 2);
    elsewhere.on_screen = false;
    assert_eq!(
        fullscreen_desktop_step(elsewhere),
        FullscreenDesktopStep::Fold(1)
    );
}

#[test]
fn the_only_desktop_left_folds_instead_of_switching_to_nothing() {
    let alone = FullscreenDesktopFacts {
        desktop: 0,
        home: None,
        len: 1,
        active: 0,
        ..facts(1, 1)
    };
    assert_eq!(
        fullscreen_desktop_step(alone),
        FullscreenDesktopStep::Fold(0)
    );
}

#[test]
fn fullscreen_keeps_its_place_unless_the_session_runs_workspaces() {
    assert!(!fullscreen_moves_to_own_desktop(
        false,
        WorkspaceMode::OutputBound,
        false
    ));
    assert!(fullscreen_moves_to_own_desktop(
        true,
        WorkspaceMode::OutputBound,
        false
    ));
}

#[test]
fn a_game_and_global_desktops_keep_fullscreen_in_place() {
    assert!(!fullscreen_moves_to_own_desktop(
        true,
        WorkspaceMode::OutputBound,
        true
    ));
    assert!(!fullscreen_moves_to_own_desktop(
        true,
        WorkspaceMode::Global,
        false
    ));
}
