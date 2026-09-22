//! Every verb a window offers, from the compositor and from its desktop entry.
//!
//! The header, its menu and its command palette are three views of this one
//! list, so a verb cannot be pinnable in one and missing from another.

use std::collections::HashMap;
use std::sync::{Mutex, OnceLock};

use cosmic_settings_config::shortcuts;
use icetron_p::prelude::{HaloCommand, HaloCommandGroup, PinOutcome};
use icetron_themes::icons;

use crate::fl;
use crate::state::State;
use crate::utils::desktop_action::DesktopApp;

use super::Message;

/// Prefix for a command that came from the window's desktop entry rather than
/// from the compositor. Keeps the two id spaces apart, so an entry declaring an
/// action called `close` cannot shadow the window verb.
pub const ACTION_PREFIX: &str = "action:";

/// Pinned out of the box. The capture pair is the one worth a click: both are
/// about what is on screen *now*, on *this* window, without disturbing it.
const DEFAULT_PINS: [&str; 2] = ["shot", "record"];

/// What the window can currently do, as the compositor sees it.
pub struct WindowFacts<'a> {
    pub recording: bool,
    /// Maximized, or laid out to fill its zone.
    pub maximized: bool,
    pub fullscreen: bool,
    /// A window whose minimum and maximum sizes agree cannot be resized, so
    /// filling the screen is not a shape it has — the same test the tiling
    /// layout uses to spot a dialog.
    pub resizable: bool,
    /// Another window of this app is open, so closing them all means something.
    pub close_all: bool,
    pub app: Option<&'a DesktopApp>,
}

/// The window's verbs, in the fixed group order every window shares.
pub fn commands(facts: &WindowFacts<'_>) -> Vec<HaloCommand> {
    let mut commands = vec![
        HaloCommand::new(
            "shot",
            fl!("halo-screenshot-window"),
            HaloCommandGroup::Capture,
        )
        .icon(icons::CAMERA),
        HaloCommand::new(
            "record",
            if facts.recording {
                fl!("window-menu-stop-recording")
            } else {
                fl!("window-menu-record")
            },
            HaloCommandGroup::Capture,
        )
        .icon(icons::CIRCLE)
        .stateful(facts.recording),
    ];

    if facts.app.is_some_and(|app| app.new_window.is_some()) {
        commands.push(
            HaloCommand::new("neww", fl!("window-menu-new-window"), HaloCommandGroup::App)
                .icon(icons::PLUS),
        );
    }

    // The app's own verbs, from the `[Desktop Action …]` groups it declares.
    // The desktop already has a way for an application to say what it can do
    // outside itself; the header offers that rather than a parallel set only
    // apps built for this compositor could answer.
    if let Some(app) = facts.app {
        let section = app
            .name
            .clone()
            .unwrap_or_else(|| fl!("halo-app-actions-fallback"));
        for action in &app.actions {
            commands.push(
                HaloCommand::new(
                    format!("{ACTION_PREFIX}{}", action.id),
                    action.name.clone(),
                    HaloCommandGroup::App,
                )
                // An entry's `Icon` key is a runtime name, not one of the marks
                // this widget set embeds, so these wear the generic command
                // glyph rather than a wrong one.
                .icon(icons::COMMAND)
                .section(fl!("halo-app-actions", app = section.clone())),
            );
        }
    }

    if facts.close_all {
        commands.push(
            HaloCommand::new(
                "closeall",
                fl!("window-menu-close-all"),
                HaloCommandGroup::App,
            )
            .icon(icons::X)
            .pinnable(false),
        );
    }

    commands.push(
        HaloCommand::new(
            "minimize",
            fl!("window-menu-minimize"),
            HaloCommandGroup::Window,
        )
        .icon(icons::MINUS),
    );
    let restore = facts.maximized || facts.fullscreen;
    commands.push(
        HaloCommand::new(
            "maximize",
            if restore {
                fl!("window-menu-restore")
            } else {
                fl!("window-menu-maximize")
            },
            HaloCommandGroup::Window,
        )
        .icon(if restore {
            icons::MINIMIZE_2
        } else {
            icons::MAXIMIZE_2
        })
        .enabled(facts.resizable || restore),
    );
    commands.push(
        HaloCommand::new(
            "fullscreen",
            if facts.fullscreen {
                fl!("window-menu-leave-fullscreen")
            } else {
                fl!("window-menu-fullscreen")
            },
            HaloCommandGroup::Window,
        )
        .icon(if facts.fullscreen {
            icons::SHRINK
        } else {
            icons::FULLSCREEN
        })
        .enabled(facts.resizable || facts.fullscreen),
    );
    commands.push(
        // Never pinnable: Close is the one control the pill never sheds, so a
        // pin would only ever be a second copy of a button already there.
        HaloCommand::new("close", fl!("window-menu-close"), HaloCommandGroup::Window)
            .icon(icons::X)
            .pinnable(false),
    );
    commands
}

/// The compositor message a command id stands for, or `None` when the id names
/// one of the window's own desktop-entry actions.
pub fn message_for(id: &str) -> Option<Message> {
    Some(match id {
        "shot" => Message::Screenshot,
        "record" => Message::Record,
        "neww" => Message::NewWindow,
        "minimize" => Message::Minimize,
        "maximize" => Message::Maximize,
        "fullscreen" => Message::Fullscreen,
        "close" => Message::Close,
        _ => return None,
    })
}

/// The desktop-entry action `id` names, if it is one.
pub fn desktop_action<'a>(
    app: Option<&'a DesktopApp>,
    id: &str,
) -> Option<&'a crate::utils::desktop_action::DesktopAction> {
    let action = id.strip_prefix(ACTION_PREFIX)?;
    app?.actions.iter().find(|candidate| candidate.id == action)
}

/// Tray pins, per app id.
///
/// Per APP, not per window: pinning Screenshot in one window of an app pins it
/// in every window of that app, because a pin is a statement about the app. The
/// stateful half — whether *this* window is recording — stays on the window and
/// never comes from here.
fn store() -> &'static Mutex<HashMap<String, Vec<String>>> {
    static PINS: OnceLock<Mutex<HashMap<String, Vec<String>>>> = OnceLock::new();
    PINS.get_or_init(Default::default)
}

/// This app's pins, or the default tray when it has none.
pub fn pins(app_id: &str) -> Vec<String> {
    store()
        .lock()
        .unwrap()
        .get(app_id)
        .cloned()
        .unwrap_or_else(|| DEFAULT_PINS.iter().map(|id| (*id).to_owned()).collect())
}

/// Pin or unpin `id` for `app_id`, honouring the tray cap.
pub fn toggle_pin(app_id: &str, id: &str) -> PinOutcome {
    let mut current = pins(app_id);
    let outcome = icetron_p::prelude::toggle_pin(&mut current, id);
    if outcome != PinOutcome::Full {
        store().lock().unwrap().insert(app_id.to_owned(), current);
    }
    outcome
}

/// The chat panel's own command with `query` appended as one shell-quoted
/// argument, so a question typed into the palette arrives as the
/// conversation's opening line. An empty query adds nothing.
pub fn chat_command(base: &str, query: &str) -> String {
    let query = query.trim();
    if query.is_empty() {
        return base.to_owned();
    }
    match shlex::try_quote(query) {
        Ok(quoted) => format!("{base} {quoted}"),
        Err(_) => base.to_owned(),
    }
}

/// Ask Chat: the `ChatPanel` system action — the one Super+C is bound to —
/// run with the palette's query as its argument.
pub fn open_chat(state: &mut State, query: &str) {
    let Some(base) = state
        .common
        .config
        .system_actions
        .get(&shortcuts::action::System::ChatPanel)
        .cloned()
    else {
        tracing::warn!("no ChatPanel system action is configured, so Ask Chat has nowhere to go");
        return;
    };
    state.spawn_command(chat_command(&base, query));
}

#[cfg(test)]
mod tests {
    #[test]
    fn the_chat_command_carries_the_query_as_one_argument() {
        use super::chat_command;
        assert_eq!(chat_command("agentos-chat-panel", ""), "agentos-chat-panel");
        assert_eq!(
            chat_command("agentos-chat-panel", "  "),
            "agentos-chat-panel"
        );
        assert_eq!(
            chat_command("agentos-chat-panel", "plain"),
            "agentos-chat-panel plain"
        );
        assert_eq!(
            chat_command("agentos-chat-panel", "how do I fill this window?"),
            "agentos-chat-panel 'how do I fill this window?'"
        );
    }

    use super::*;
    use crate::utils::desktop_action::DesktopApp;
    use icetron_p::prelude::TRAY_CAP;

    fn facts<'a>(app: Option<&'a DesktopApp>) -> WindowFacts<'a> {
        WindowFacts {
            recording: false,
            maximized: false,
            fullscreen: false,
            resizable: true,
            close_all: false,
            app,
        }
    }

    fn app(entry: &str) -> DesktopApp {
        DesktopApp::from_content(std::path::Path::new("/apps/example.desktop"), entry).unwrap()
    }

    /// A window that cannot be resized has no filled shape to go to, so the
    /// verbs that would put it in one say so rather than silently doing nothing.
    #[test]
    fn a_fixed_size_window_keeps_its_size_verbs_visible_but_dead() {
        let mut facts = facts(None);
        facts.resizable = false;
        let fixed = commands(&facts);
        for id in ["maximize", "fullscreen"] {
            let command = fixed.iter().find(|c| c.id == id).unwrap();
            assert!(!command.enabled, "{id}");
        }
        // Already in that state, leaving it is exactly what it needs.
        facts.fullscreen = true;
        let leaving = commands(&facts);
        assert!(
            leaving
                .iter()
                .find(|c| c.id == "fullscreen")
                .unwrap()
                .enabled
        );
    }

    #[test]
    fn a_desktop_entry_contributes_its_own_actions_under_the_app_name() {
        let app = app(
            "[Desktop Entry]\nType=Application\nName=Example\nActions=compose;\n\
             [Desktop Action compose]\nName=Compose\nExec=example --compose\n",
        );
        let commands = commands(&facts(Some(&app)));
        let action = commands
            .iter()
            .find(|c| c.id == format!("{ACTION_PREFIX}compose"))
            .expect("the entry's action is offered");
        assert_eq!(action.label, "Compose");
        assert_eq!(action.group, HaloCommandGroup::App);
        assert!(action.section.is_some());
        assert!(desktop_action(Some(&app), &action.id).is_some());
        // Namespaced, so an entry cannot shadow a window verb.
        assert!(message_for(&action.id).is_none());
        assert!(message_for("close").is_some());
    }

    /// Close is always in the pill, so a pin would only ever duplicate it.
    #[test]
    fn close_is_never_pinnable() {
        let commands = commands(&facts(None));
        assert!(!commands.iter().find(|c| c.id == "close").unwrap().pinnable);
        assert!(commands.iter().find(|c| c.id == "shot").unwrap().pinnable);
    }

    #[test]
    fn pins_start_at_the_capture_pair_and_are_kept_per_app() {
        assert_eq!(pins("pins-test-a"), DEFAULT_PINS);
        assert_eq!(toggle_pin("pins-test-a", "shot"), PinOutcome::Unpinned);
        assert_eq!(pins("pins-test-a"), ["record"]);
        assert_eq!(pins("pins-test-b"), DEFAULT_PINS);
        assert_eq!(toggle_pin("pins-test-a", "minimize"), PinOutcome::Pinned);
        assert_eq!(toggle_pin("pins-test-a", "maximize"), PinOutcome::Pinned);
        assert_eq!(pins("pins-test-a").len(), TRAY_CAP);
        assert_eq!(toggle_pin("pins-test-a", "fullscreen"), PinOutcome::Full);
        assert_eq!(pins("pins-test-a").len(), TRAY_CAP);
    }
}
