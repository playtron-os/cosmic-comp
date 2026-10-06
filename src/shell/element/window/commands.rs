//! Every verb a window offers: the compositor's, the ones the app publishes
//! over `kora_app_commands_v1`, and its desktop entry's.
//!
//! The header, its menu and its command palette are three views of this one
//! list, so a verb cannot be pinnable in one and missing from another.

use std::collections::HashMap;
use std::sync::{Mutex, OnceLock};

use cosmic_settings_config::shortcuts;
use icetron_p::prelude::{HaloCommand, HaloCommandGroup, PinOutcome};
use icetron_themes::{Icon, icons};

use crate::dbus::notifications::Tone;
use crate::fl;
use crate::state::State;
use crate::utils::desktop_action::DesktopApp;
use crate::wayland::protocols::app_commands::catalog::{Catalog, MENU, STATEFUL};

use super::Message;

/// Prefix for a command that came from the window's desktop entry rather than
/// from the compositor. Keeps the two id spaces apart, so an entry declaring an
/// action called `close` cannot shadow the window verb.
pub const ACTION_PREFIX: &str = "action:";

/// Prefix for a command the app published itself, for the same reason.
pub const APP_PREFIX: &str = "app:";

/// The shell shows at most this many of an app's menu nominees.
pub const MENU_NOMINEE_CAP: usize = 6;

/// Pinned out of the box. The capture pair is the one worth a click: both are
/// about what is on screen *now*, on *this* window, without disturbing it.
const DEFAULT_PINS: [&str; 2] = ["shot", "record"];

/// The edit verbs a window may answer itself, with the chords apps bind them to.
fn edit_verbs() -> [(&'static str, String, &'static str, Icon); 7] {
    [
        ("undo", fl!("halo-undo"), "Ctrl+Z", icons::UNDO_2),
        ("redo", fl!("halo-redo"), "Ctrl+Shift+Z", icons::REDO_2),
        ("cut", fl!("halo-cut"), "Ctrl+X", icons::SCISSORS),
        ("copy", fl!("halo-copy"), "Ctrl+C", icons::COPY),
        ("paste", fl!("halo-paste"), "Ctrl+V", icons::CLIPBOARD_PASTE),
        (
            "selall",
            fl!("halo-select-all"),
            "Ctrl+A",
            icons::TEXT_SELECT,
        ),
        ("markv", fl!("halo-mark-version"), "Ctrl+S", icons::BOOKMARK),
    ]
}

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
    /// What the window published, when its app speaks `kora_app_commands_v1`.
    pub catalog: Option<&'a Catalog>,
}

impl WindowFacts<'_> {
    /// Whether the window answers standard verb `id` itself, and can right now.
    pub fn handles(&self, id: &str) -> Option<bool> {
        self.catalog
            .and_then(|catalog| catalog.handles.get(id).copied())
    }

    /// A second window comes from the app, or from its desktop entry.
    pub fn new_window(&self) -> Option<bool> {
        self.handles("neww").or(self
            .app
            .and_then(|app| app.new_window.as_ref())
            .map(|_| true))
    }
}

/// The glyph for a symbolic icon name an app gave. Names are a closed set the
/// compositor embeds; anything else wears the generic command mark.
pub fn app_icon(name: &str) -> Icon {
    match name {
        "plus" => icons::PLUS,
        "x" => icons::X,
        "trash" | "trash-2" => icons::TRASH_2,
        "search" => icons::SEARCH,
        "settings" => icons::SETTINGS,
        "zoom-in" => icons::ZOOM_IN,
        "zoom-out" => icons::ZOOM_OUT,
        "refresh-cw" => icons::REFRESH_CW,
        "rotate-cw" => icons::ROTATE_CW,
        "share" | "share-2" => icons::SHARE_2,
        "download" => icons::DOWNLOAD,
        "upload" => icons::UPLOAD,
        "file-plus" => icons::FILE_PLUS,
        "folder-open" => icons::FOLDER_OPEN,
        "message-square-plus" => icons::MESSAGE_SQUARE_PLUS,
        "pencil" => icons::PENCIL,
        "eraser" => icons::ERASER,
        "arrow-up" => icons::ARROW_UP,
        "arrow-down" => icons::ARROW_DOWN,
        "chevron-left" => icons::CHEVRON_LEFT,
        "chevron-right" => icons::CHEVRON_RIGHT,
        "panel-left" => icons::PANEL_LEFT,
        "printer" => icons::PRINTER,
        "star" => icons::STAR,
        "pin" => icons::PIN,
        "send" => icons::SEND,
        "sparkles" => icons::SPARKLES,
        "external-link" => icons::EXTERNAL_LINK,
        "link" => icons::LINK,
        "copy" => icons::COPY,
        "history" => icons::HISTORY,
        "info" => icons::INFO,
        _ => icons::COMMAND,
    }
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

    // Every window lists the whole Edit set: the verbs it declares go to it, the shell answers the rest.
    for (id, label, keys, icon) in edit_verbs() {
        commands.push(
            HaloCommand::new(id, label, HaloCommandGroup::Edit)
                .icon(icon)
                .shortcut(keys)
                .enabled(facts.handles(id).unwrap_or(true)),
        );
    }

    let settings = HaloCommand::new("settings", fl!("halo-settings"), HaloCommandGroup::App)
        .icon(icons::SETTINGS);
    commands.push(match facts.handles("settings") {
        Some(enabled) => settings.shortcut("Ctrl+,").enabled(enabled),
        None => settings,
    });
    // Always offered: an app that cannot open another window says so.
    let neww = HaloCommand::new("neww", fl!("halo-new-window-row"), HaloCommandGroup::App)
        .icon(icons::PLUS)
        .enabled(facts.new_window().unwrap_or(true));
    commands.push(if facts.handles("neww").is_some() {
        neww.shortcut("Ctrl+N")
    } else {
        neww
    });
    commands.push(
        HaloCommand::new("find", fl!("halo-find"), HaloCommandGroup::App)
            .icon(icons::SEARCH)
            .shortcut("Ctrl+F")
            .enabled(facts.handles("find").unwrap_or(true)),
    );
    commands.push(
        HaloCommand::new("info", fl!("halo-info"), HaloCommandGroup::App)
            .icon(icons::INFO)
            .enabled(facts.handles("info").unwrap_or(true)),
    );
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
            fl!("halo-park-window"),
            HaloCommandGroup::Window,
        )
        .icon(icons::MINUS),
    );
    commands.push(
        HaloCommand::new("maximize", fl!("halo-fill"), HaloCommandGroup::Window)
            .icon(icons::MAXIMIZE_2)
            .enabled(facts.resizable || facts.maximized || facts.fullscreen),
    );
    commands.push(
        HaloCommand::new(
            "fullscreen",
            fl!("halo-fullscreen-toggle"),
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
        HaloCommand::new("close", fl!("halo-close-window"), HaloCommandGroup::Window)
            .icon(icons::X)
            .pinnable(false),
    );

    // The app's own block. A window that publishes nothing keeps its desktop
    // entry's actions there instead.
    if let Some(catalog) = facts.catalog {
        for command in &catalog.commands {
            let mut row = HaloCommand::new(
                format!("{APP_PREFIX}{}", command.id),
                command.name.clone(),
                HaloCommandGroup::Own,
            )
            .icon(app_icon(&command.icon))
            .enabled(command.enabled);
            if !command.keys.is_empty() {
                row = row.shortcut(command.keys.clone());
            }
            if !command.section.is_empty() {
                row = row.section(command.section.clone());
            }
            if command.flags & STATEFUL != 0 {
                row = row.stateful(command.active);
            }
            commands.push(row);
        }
    } else if let Some(app) = facts.app {
        for action in &app.actions {
            commands.push(
                HaloCommand::new(
                    format!("{ACTION_PREFIX}{}", action.id),
                    action.name.clone(),
                    HaloCommandGroup::Own,
                )
                // An entry's `Icon` key is a runtime name, not one of the marks
                // this widget set embeds, so these wear the generic command
                // glyph rather than a wrong one.
                .icon(icons::COMMAND),
            );
        }
    }
    commands
}

/// What the shell says for a standard verb the window does not answer.
pub fn shell_answer(id: &str) -> Option<String> {
    Some(match id {
        "markv" => fl!("halo-version-marked"),
        "find" => fl!("halo-find-toast"),
        _ => {
            let (_, verb, ..) = edit_verbs().into_iter().find(|(verb, ..)| *verb == id)?;
            fl!("halo-system-verb", verb = verb)
        }
    })
}

/// The commands an app put forward for its `⌄` menu, in its order, capped.
pub fn menu_nominees(
    catalog: &Catalog,
) -> impl Iterator<Item = &crate::wayland::protocols::app_commands::catalog::Command> {
    catalog
        .commands
        .iter()
        .filter(|command| command.flags & MENU != 0)
        .take(MENU_NOMINEE_CAP)
}

/// The compositor message a command id stands for, or `None` when the id names
/// one of the window's own commands or desktop-entry actions.
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

/// What the shell says about a pin, as the prototype's `togglePin` does: the
/// header shows a pin, so only an unpin and a refusal are said.
pub fn pin_receipt(outcome: PinOutcome, palette_keys: Option<String>) -> Option<(String, Tone)> {
    match outcome {
        PinOutcome::Pinned => None,
        PinOutcome::Unpinned => Some((
            match palette_keys {
                Some(keys) => fl!("halo-unpinned", keys = keys),
                None => fl!("halo-unpinned-unbound"),
            },
            Tone::Neutral,
        )),
        PinOutcome::Full => Some((
            fl!("halo-pin-full", cap = icetron_p::prelude::TRAY_CAP),
            Tone::NeedsYou,
        )),
    }
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
    use crate::dbus::notifications::plain;
    use crate::utils::desktop_action::DesktopApp;
    use icetron_p::prelude::TRAY_CAP;

    #[test]
    fn pin_receipts_read_as_the_prototype_writes_them() {
        let receipt = |outcome, keys: Option<&str>| {
            pin_receipt(outcome, keys.map(str::to_owned)).map(|(text, tone)| (plain(text), tone))
        };
        assert_eq!(receipt(PinOutcome::Pinned, Some("Super+K")), None);
        assert_eq!(
            receipt(PinOutcome::Unpinned, Some("Super+K")),
            Some((
                "Unpinned — still one Super+K away".to_owned(),
                Tone::Neutral
            ))
        );
        assert_eq!(
            receipt(PinOutcome::Full, None),
            Some((
                format!("Tray is full ({TRAY_CAP}) — the halo is a pill, not a toolbar"),
                Tone::NeedsYou
            ))
        );
    }

    fn facts<'a>(app: Option<&'a DesktopApp>) -> WindowFacts<'a> {
        WindowFacts {
            recording: false,
            maximized: false,
            fullscreen: false,
            resizable: true,
            close_all: false,
            app,
            catalog: None,
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

    /// Its block is headed by the app's name, as an app's own commands are.
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
        assert_eq!(action.group, HaloCommandGroup::Own);
        assert!(action.section.is_none());
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

    fn catalog() -> Catalog {
        use crate::wayland::protocols::app_commands::catalog::{BOUND, Command};
        let mut catalog = Catalog::default();
        catalog.handle("copy".into(), 0).unwrap();
        catalog.handle("neww".into(), 1).unwrap();
        for (index, flags) in [
            MENU,
            0,
            MENU | STATEFUL,
            MENU,
            MENU,
            MENU,
            MENU,
            MENU | BOUND,
        ]
        .into_iter()
        .enumerate()
        {
            catalog
                .add(Command {
                    id: format!("slate.c{index}"),
                    name: format!("Command {index}"),
                    keys: String::new(),
                    section: if index < 4 {
                        "File".into()
                    } else {
                        String::new()
                    },
                    icon: "zoom-in".into(),
                    flags,
                    enabled: true,
                    active: false,
                })
                .unwrap();
        }
        catalog.set_state("slate.c2", 1).unwrap();
        catalog
    }

    #[test]
    fn every_window_lists_the_whole_edit_set_and_find() {
        let ids = ["undo", "redo", "cut", "copy", "paste", "selall", "markv"];
        let silent = commands(&facts(None));
        let edit: Vec<_> = silent
            .iter()
            .filter(|c| c.group == HaloCommandGroup::Edit)
            .collect();
        assert_eq!(edit.iter().map(|c| c.id.as_str()).collect::<Vec<_>>(), ids);
        assert!(edit.iter().all(|c| c.enabled && c.shortcut.is_some()));
        assert!(silent.iter().any(|c| c.id == "find" && c.enabled));

        // A declared verb follows what the window says it can do now.
        let catalog = catalog();
        let mut facts = facts(None);
        facts.catalog = Some(&catalog);
        let listed = commands(&facts);
        let copy = listed.iter().find(|c| c.id == "copy").unwrap();
        assert!(!copy.enabled);
        assert_eq!(copy.shortcut.as_deref(), Some("Ctrl+C"));
        assert!(listed.iter().find(|c| c.id == "paste").unwrap().enabled);
        assert!(listed.iter().any(|c| c.id == "neww" && c.enabled));
    }

    #[test]
    fn the_shell_answers_an_undeclared_verb_as_the_design_does() {
        let answer = |id| shell_answer(id).map(plain);
        assert_eq!(
            answer("copy").as_deref(),
            Some("Copy — system verb, guaranteed in every text and canvas surface")
        );
        assert_eq!(
            answer("selall").as_deref(),
            Some("Select all — system verb, guaranteed in every text and canvas surface")
        );
        assert_eq!(answer("markv").as_deref(), Some("Version marked"));
        assert_eq!(
            answer("find").as_deref(),
            Some("Find in window — standard command, every app")
        );
        assert_eq!(answer("close"), None);
    }

    /// The app's own commands make the last block, apart from the shell's ids.
    #[test]
    fn a_window_s_own_commands_follow_the_system_groups() {
        let catalog = catalog();
        let mut facts = facts(None);
        facts.catalog = Some(&catalog);
        let commands = commands(&facts);
        let own: Vec<_> = commands
            .iter()
            .filter(|c| c.group == HaloCommandGroup::Own)
            .collect();
        assert_eq!(own.len(), 8);
        assert_eq!(own[0].id, format!("{APP_PREFIX}slate.c0"));
        assert_eq!(own[0].section.as_deref(), Some("File"));
        assert_eq!(own[0].icon, Some(icons::ZOOM_IN));
        assert!(own[2].stateful && own[2].on);
        assert_eq!(commands.last().unwrap().group, HaloCommandGroup::Own);
        assert!(message_for(&own[0].id).is_none());
    }

    /// An app that publishes its own commands no longer gets its desktop entry's.
    #[test]
    fn desktop_entry_actions_stand_in_only_for_a_silent_window() {
        let app = app(
            "[Desktop Entry]\nType=Application\nName=Example\nActions=compose;\n\
             [Desktop Action compose]\nName=Compose\nExec=example --compose\n",
        );
        let silent = commands(&facts(Some(&app)));
        assert!(
            silent
                .iter()
                .any(|c| c.id == format!("{ACTION_PREFIX}compose")
                    && c.group == HaloCommandGroup::Own)
        );
        let catalog = catalog();
        let mut facts = facts(Some(&app));
        facts.catalog = Some(&catalog);
        assert!(
            commands(&facts)
                .iter()
                .all(|c| !c.id.starts_with(ACTION_PREFIX))
        );
    }

    #[test]
    fn the_menu_takes_the_first_six_nominees_in_order() {
        let catalog = catalog();
        let nominees: Vec<_> = menu_nominees(&catalog).map(|c| c.id.as_str()).collect();
        assert_eq!(
            nominees,
            [
                "slate.c0", "slate.c2", "slate.c3", "slate.c4", "slate.c5", "slate.c6"
            ]
        );
    }

    #[test]
    fn an_unknown_icon_name_wears_the_generic_mark() {
        assert_eq!(app_icon("zoom-in"), icons::ZOOM_IN);
        assert_eq!(app_icon("../../etc/passwd"), icons::COMMAND);
        assert_eq!(app_icon(""), icons::COMMAND);
    }
}
