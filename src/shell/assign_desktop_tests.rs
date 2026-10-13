use super::*;
use smithay::{
    output::{Mode, PhysicalProperties, Subpixel},
    reexports::wayland_server::Display,
};

fn output(name: &str) -> Output {
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
    output.change_current_state(
        Some(Mode {
            size: (1280, 800).into(),
            refresh: 60_000,
        }),
        None,
        None,
        None,
    );
    output
}

/// A built-in and an external output, `count` desktops each, the first on screen.
fn two_outputs(
    count: usize,
    test: impl FnOnce(&mut IndexMap<Output, WorkspaceSet>, &mut WorkspaceState<State>),
) {
    let display = Display::<State>::new().unwrap();
    let mut state = WorkspaceState::<State>::new(&display.handle(), |_| true);
    let mut sets = IndexMap::new();
    for name in ["eDP-test", "HDMI-test"] {
        let output = output(name);
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
        sets.insert(output, set);
    }
    test(&mut sets, &mut state);
}

fn handles(set: &WorkspaceSet) -> Vec<WorkspaceHandle> {
    set.workspaces.iter().map(|w| w.handle).collect()
}

#[test]
fn a_desktop_moves_to_the_output_whose_group_it_is_given() {
    two_outputs(2, |sets, state| {
        let external = sets[1].workspaces[1].handle;
        let built_in_group = sets[0].group;
        assert!(assign_desktop(
            sets,
            WorkspaceMode::OutputBound,
            &external,
            &built_in_group,
            &mut state.update(),
        ));
        // Right after the desktop on screen there, and gone from where it was.
        assert_eq!(sets[0].workspaces[1].handle, external);
        assert_eq!(sets[0].workspaces.len(), 3);
        assert!(!handles(&sets[1]).contains(&external));
        assert_eq!(sets[0].workspaces[1].output(), sets.get_index(0).unwrap().0);
    });
}

#[test]
fn the_output_a_desktop_left_still_shows_one() {
    two_outputs(1, |sets, state| {
        let only = sets[1].workspaces[0].handle;
        let built_in_group = sets[0].group;
        assert!(assign_desktop(
            sets,
            WorkspaceMode::OutputBound,
            &only,
            &built_in_group,
            &mut state.update(),
        ));
        assert_eq!(
            sets[1].workspaces.len(),
            1,
            "an empty desktop takes its place"
        );
        assert_ne!(sets[1].workspaces[0].handle, only);
        assert!(sets[1].active < sets[1].workspaces.len());
    });
}

/// The switcher's swap: the desktop on screen on each output trades places.
#[test]
fn two_assigns_swap_the_desktops_on_screen() {
    two_outputs(2, |sets, state| {
        sets[0].active = 0;
        sets[1].active = 1;
        let on_built_in = sets[0].workspaces[0].handle;
        let on_external = sets[1].workspaces[1].handle;
        let (built_in_group, external_group) = (sets[0].group, sets[1].group);
        let mut update = state.update();
        assert!(assign_desktop(
            sets,
            WorkspaceMode::OutputBound,
            &on_external,
            &built_in_group,
            &mut update,
        ));
        assert!(assign_desktop(
            sets,
            WorkspaceMode::OutputBound,
            &on_built_in,
            &external_group,
            &mut update,
        ));
        assert!(handles(&sets[0]).contains(&on_external));
        assert!(!handles(&sets[0]).contains(&on_built_in));
        assert!(handles(&sets[1]).contains(&on_built_in));
        assert!(!handles(&sets[1]).contains(&on_external));
    });
}

#[test]
fn desktops_that_span_outputs_stay_put() {
    two_outputs(2, |sets, state| {
        let external = sets[1].workspaces[1].handle;
        let built_in_group = sets[0].group;
        let before = (handles(&sets[0]), handles(&sets[1]));
        assert!(!assign_desktop(
            sets,
            WorkspaceMode::Global,
            &external,
            &built_in_group,
            &mut state.update(),
        ));
        assert_eq!((handles(&sets[0]), handles(&sets[1])), before);
    });
}

#[test]
fn its_own_group_or_one_from_elsewhere_is_refused() {
    two_outputs(2, |sets, state| {
        let external = sets[1].workspaces[1].handle;
        let own = sets[1].group;
        let elsewhere = state.update().create_workspace_group();
        let before = (handles(&sets[0]), handles(&sets[1]));
        for group in [own, elsewhere] {
            assert!(!assign_desktop(
                sets,
                WorkspaceMode::OutputBound,
                &external,
                &group,
                &mut state.update(),
            ));
        }
        assert_eq!((handles(&sets[0]), handles(&sets[1])), before);
    });
}
