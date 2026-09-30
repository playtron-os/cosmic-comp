use super::*;
use smithay::{
    output::{Mode, PhysicalProperties, Subpixel},
    reexports::wayland_server::Display,
};

fn with_workspaces(
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
    let mut theme = crate::comp_theme::CompTheme::default();
    theme.motion.slide_crossfade = Duration::from_secs(60);
    let mut set = WorkspaceSet::new(
        &mut state.update(),
        &output,
        false,
        &theme,
        AppearanceConfig::default(),
    );
    for _ in 0..count {
        set.add_empty_workspace(&mut state.update());
    }
    test(&mut set, &mut state, &activation);
}

#[test]
fn replacement_windows_keep_the_original_scene_after_empty_workspaces_are_pruned() {
    with_workspaces(4, |set, state, activation| {
        set.active = 1;
        let outgoing = set.workspaces[1].handle;
        let incoming = set.workspaces[3].handle;
        set.activate(2, WorkspaceDelta::new_crossfade(), &mut state.update())
            .unwrap();
        set.activate(3, WorkspaceDelta::new_crossfade(), &mut state.update())
            .unwrap();
        set.ensure_last_empty(&mut state.update(), activation);

        let (previous, _) = set.previously_active.unwrap();
        assert_eq!(set.workspaces[previous].handle, outgoing);
        assert_eq!(set.workspaces[set.active].handle, incoming);
        assert_eq!(set.workspaces.len(), 2);
    });
}

#[test]
fn an_empty_outgoing_workspace_survives_until_the_fade_finishes() {
    with_workspaces(2, |set, state, activation| {
        set.activate(1, WorkspaceDelta::new_crossfade(), &mut state.update())
            .unwrap();
        let incoming = set.workspaces[1].handle;
        set.ensure_last_empty(&mut state.update(), activation);
        assert_eq!(set.workspaces.len(), 2);

        set.previously_active = Some((
            0,
            WorkspaceDelta::Crossfade(Instant::now() - Duration::from_secs(61)),
        ));
        set.refresh();
        set.ensure_last_empty(&mut state.update(), activation);
        assert_eq!(set.workspaces.len(), 1);
        assert_eq!(set.workspaces[set.active].handle, incoming);
        assert!(set.previously_active.is_none());
    });
}

#[test]
fn reselecting_the_incoming_workspace_does_not_cancel_its_fade() {
    with_workspaces(2, |set, state, _| {
        let start = Instant::now();
        set.activate(1, WorkspaceDelta::Crossfade(start), &mut state.update())
            .unwrap();
        set.activate(1, WorkspaceDelta::new_crossfade(), &mut state.update())
            .unwrap();
        assert!(matches!(set.previously_active,
            Some((0, WorkspaceDelta::Crossfade(original))) if original == start));
    });
}
