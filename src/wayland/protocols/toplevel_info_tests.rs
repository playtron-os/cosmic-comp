use super::*;
use crate::wayland::protocols::workspace::{Request, delegate_workspace};
use std::sync::Arc;

#[derive(Clone, Default)]
struct TestWindow(Arc<UserDataMap>);

impl PartialEq for TestWindow {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

impl IsAlive for TestWindow {
    fn alive(&self) -> bool {
        true
    }
}

impl Window for TestWindow {
    fn title(&self) -> String {
        "Test".into()
    }
    fn app_id(&self) -> String {
        "test".into()
    }
    fn is_activated(&self) -> bool {
        false
    }
    fn is_maximized(&self) -> bool {
        false
    }
    fn is_fullscreen(&self) -> bool {
        false
    }
    fn is_minimized(&self) -> bool {
        false
    }
    fn is_sticky(&self) -> bool {
        false
    }
    fn is_resizing(&self) -> bool {
        false
    }
    fn global_geometry(&self) -> Option<Rectangle<i32, Global>> {
        None
    }
    fn user_data(&self) -> &UserDataMap {
        &self.0
    }
}

struct Model {
    info: ToplevelInfoState<Self, TestWindow>,
    workspaces: WorkspaceState<Self>,
}

smithay::delegate_dispatch2!(Model);
delegate_toplevel_info!(Model, TestWindow);
delegate_workspace!(Model);

impl ForeignToplevelListHandler for Model {
    fn foreign_toplevel_list_state(&mut self) -> &mut ForeignToplevelListState {
        &mut self.info.foreign_toplevel_list
    }
}

impl ToplevelInfoHandler for Model {
    type Window = TestWindow;
    fn toplevel_info_state(&self) -> &ToplevelInfoState<Self, TestWindow> {
        &self.info
    }
    fn toplevel_info_state_mut(&mut self) -> &mut ToplevelInfoState<Self, TestWindow> {
        &mut self.info
    }
}

impl WorkspaceHandler for Model {
    fn workspace_state(&self) -> &WorkspaceState<Self> {
        &self.workspaces
    }
    fn workspace_state_mut(&mut self) -> &mut WorkspaceState<Self> {
        &mut self.workspaces
    }
    fn commit_requests(&mut self, _: &DisplayHandle, _: Vec<Request>) {}
}

#[test]
fn hide_show_and_parking_keep_identity_but_actual_unmap_does_not() {
    let display = smithay::reexports::wayland_server::Display::<Model>::new().unwrap();
    let dh = display.handle();
    let mut model = Model {
        info: ToplevelInfoState::new(&dh, |_| true),
        workspaces: WorkspaceState::new(&dh, |_| true),
    };
    let window = TestWindow::default();
    model.info.new_toplevel(&window, &model.workspaces);
    let original = foreign_toplevel_identifier(&window).unwrap();
    let first = window
        .user_data()
        .get::<ToplevelState>()
        .unwrap()
        .lock()
        .unwrap()
        .foreign_handle
        .clone()
        .unwrap();
    for _ in 0..3 {
        model.info.set_visible(&model.workspaces, |_| false);
        assert!(first.is_closed());
        assert!(foreign_toplevel_identifier(&window).is_none());
        assert_eq!(
            mapped_toplevel_identifier(&window).as_deref(),
            Some(original.as_str())
        );
        model.info.set_visible(&model.workspaces, |_| true);
        assert_eq!(
            foreign_toplevel_identifier(&window).as_deref(),
            Some(original.as_str())
        );
        assert!(
            window_from_ext::<TestWindow, Model>(&model, first.clone()).is_none(),
            "the first closed handle stays retired"
        );
    }
    model.info.remove_toplevel(&window);
    assert!(mapped_toplevel_identifier(&window).is_none());
    model.info.new_toplevel(&window, &model.workspaces);
    assert_ne!(foreign_toplevel_identifier(&window).unwrap(), original);
}

#[test]
fn a_window_shown_again_keeps_its_output_and_workspace() {
    use cosmic_protocols::workspace::v2::server::zcosmic_workspace_handle_v2;
    use smithay::output::{Output, PhysicalProperties, Subpixel};
    let display = smithay::reexports::wayland_server::Display::<Model>::new().unwrap();
    let dh = display.handle();
    let mut model = Model {
        info: ToplevelInfoState::new(&dh, |_| true),
        workspaces: WorkspaceState::new(&dh, |_| true),
    };
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
    let workspace = {
        let mut update = model.workspaces.update();
        let group = update.create_workspace_group();
        update
            .create_workspace(
                &group,
                zcosmic_workspace_handle_v2::TilingState::FloatingOnly,
                None,
            )
            .unwrap()
    };
    let window = TestWindow::default();
    model.info.new_toplevel(&window, &model.workspaces);
    toplevel_enter_output(&window, &output);
    toplevel_enter_workspace(&window, &workspace);

    // A realm switch hides the window and shows it again; it never left its place.
    model.info.set_visible(&model.workspaces, |_| false);
    model.info.set_visible(&model.workspaces, |_| true);

    let state = window.user_data().get::<ToplevelState>().unwrap();
    let state = state.lock().unwrap();
    assert_eq!(state.outputs, vec![output]);
    assert!(state.in_workspace(&workspace));
}
