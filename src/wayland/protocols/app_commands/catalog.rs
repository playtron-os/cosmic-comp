use std::collections::BTreeMap;

pub const HANDLED_VERBS: &[&str] = &[
    "undo", "redo", "cut", "copy", "paste", "selall", "markv", "settings", "neww", "find", "info",
];
pub const MENU: u32 = 1;
pub const STATEFUL: u32 = 2;
pub const BOUND: u32 = 4;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Command {
    pub id: String,
    pub name: String,
    pub keys: String,
    pub section: String,
    pub icon: String,
    pub flags: u32,
    pub enabled: bool,
    pub active: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Recent {
    pub id: String,
    pub label: String,
    pub sublabel: String,
    pub timestamp: u64,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Catalog {
    pub generation: u32,
    pub handles: BTreeMap<String, bool>,
    pub commands: Vec<Command>,
    pub recents: Vec<Recent>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Invalid {
    Value,
    Reserved,
    Unknown,
    TooMany,
}

fn id(value: &str) -> bool {
    !value.is_empty()
        && value.len() <= 128
        && value
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || b"._-".contains(&byte))
}

fn text(value: &str, required: bool) -> bool {
    value.len() <= 256
        && (!required || !value.trim().is_empty())
        && !value.chars().any(char::is_control)
}

fn flag(value: u32) -> Result<bool, Invalid> {
    match value {
        0 => Ok(false),
        1 => Ok(true),
        _ => Err(Invalid::Value),
    }
}

impl Catalog {
    pub fn handle(&mut self, id: String, enabled: u32) -> Result<(), Invalid> {
        if !HANDLED_VERBS.contains(&id.as_str()) {
            return Err(Invalid::Reserved);
        }
        self.handles.insert(id, flag(enabled)?);
        Ok(())
    }

    pub fn add(&mut self, command: Command) -> Result<(), Invalid> {
        if HANDLED_VERBS.contains(&command.id.as_str())
            || [
                "shot",
                "record",
                "closeall",
                "park",
                "fill",
                "fullscreen",
                "movews",
                "closew",
                "minimize",
                "maximize",
                "close",
            ]
            .contains(&command.id.as_str())
        {
            return Err(Invalid::Reserved);
        }
        if !id(&command.id)
            || !command.id.contains('.')
            || !text(&command.name, true)
            || !text(&command.keys, false)
            || !text(&command.section, false)
            || !text(&command.icon, false)
            || command.icon.contains(['/', '\\'])
            || command.flags & !(MENU | STATEFUL | BOUND) != 0
        {
            return Err(Invalid::Value);
        }
        if let Some(previous) = self.commands.iter_mut().find(|c| c.id == command.id) {
            *previous = command;
        } else {
            if self.commands.len() >= 256 {
                return Err(Invalid::TooMany);
            }
            self.commands.push(command);
        }
        Ok(())
    }

    pub fn set_state(&mut self, id: &str, active: u32) -> Result<(), Invalid> {
        let active = flag(active)?;
        let command = self
            .commands
            .iter_mut()
            .find(|command| command.id == id)
            .ok_or(Invalid::Unknown)?;
        if command.flags & STATEFUL == 0 {
            return Err(Invalid::Value);
        }
        command.active = active;
        Ok(())
    }

    pub fn set_enabled(&mut self, id: &str, enabled: u32) -> Result<(), Invalid> {
        let enabled = flag(enabled)?;
        let command = self
            .commands
            .iter_mut()
            .find(|command| command.id == id)
            .ok_or(Invalid::Unknown)?;
        command.enabled = enabled;
        Ok(())
    }

    pub fn add_recent(&mut self, recent: Recent) -> Result<(), Invalid> {
        if !id(&recent.id) || !text(&recent.label, true) || !text(&recent.sublabel, false) {
            return Err(Invalid::Value);
        }
        if let Some(previous) = self.recents.iter_mut().find(|r| r.id == recent.id) {
            *previous = recent;
        } else {
            self.recents.push(recent);
        }
        if self.recents.len() > 10 {
            let oldest = self
                .recents
                .iter()
                .enumerate()
                .min_by_key(|(index, recent)| (recent.timestamp, std::cmp::Reverse(*index)))
                .map(|(index, _)| index)
                .unwrap_or(0);
            self.recents.remove(oldest);
        }
        Ok(())
    }

    pub fn recent_items(&self) -> Vec<&Recent> {
        let mut items: Vec<_> = self.recents.iter().collect();
        // Keep storage in declaration order, including across timestamp updates.
        items.sort_by_key(|recent| std::cmp::Reverse(recent.timestamp));
        items
    }

    pub fn enabled(&self, id: &str) -> bool {
        self.handles.get(id).copied().unwrap_or_else(|| {
            self.commands
                .iter()
                .any(|command| command.id == id && command.enabled)
        })
    }
}

#[derive(Debug, Default)]
pub struct Staged {
    pub pending: Catalog,
    pub committed: Option<Catalog>,
}

impl Staged {
    pub fn commit(&mut self, generation: u32) -> Result<(), Invalid> {
        if self
            .committed
            .as_ref()
            .is_some_and(|catalog| catalog.generation == generation)
        {
            return Err(Invalid::Value);
        }
        self.pending.generation = generation;
        self.committed = Some(self.pending.clone());
        Ok(())
    }

    pub fn can_invoke(&self, generation: u32, id: &str, recent: bool) -> bool {
        self.committed.as_ref().is_some_and(|catalog| {
            catalog.generation == generation
                && if recent {
                    catalog.recents.iter().any(|item| item.id == id)
                } else {
                    catalog.enabled(id)
                }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    pub(crate) fn command(id: &str, flags: u32) -> Command {
        Command {
            id: id.into(),
            name: id.into(),
            keys: String::new(),
            section: String::new(),
            icon: String::new(),
            flags,
            enabled: true,
            active: false,
        }
    }

    fn recent(id: &str, timestamp: u64) -> Recent {
        Recent {
            id: id.into(),
            label: id.into(),
            sublabel: String::new(),
            timestamp,
        }
    }

    #[test]
    fn only_standard_verbs_can_be_handled_and_shell_verbs_never_shadowed() {
        let mut catalog = Catalog::default();
        assert_eq!(catalog.handle("copy".into(), 1), Ok(()));
        assert_eq!(catalog.handle("copy".into(), 2), Err(Invalid::Value));
        assert_eq!(catalog.handle("close".into(), 1), Err(Invalid::Reserved));
        assert_eq!(
            catalog.handle("term.copy".into(), 1),
            Err(Invalid::Reserved)
        );
        for reserved in ["copy", "close", "fullscreen", "shot"] {
            assert_eq!(catalog.add(command(reserved, 0)), Err(Invalid::Reserved));
        }
        assert!(catalog.enabled("copy"));
    }

    #[test]
    fn a_command_needs_a_namespaced_id_and_plain_text() {
        let mut catalog = Catalog::default();
        assert_eq!(catalog.add(command("nodot", 0)), Err(Invalid::Value));
        assert_eq!(catalog.add(command("a/b.c", 0)), Err(Invalid::Value));
        let mut multiline = command("slate.x", 0);
        multiline.name = "two\nlines".into();
        assert_eq!(catalog.add(multiline), Err(Invalid::Value));
        let mut path = command("slate.y", 0);
        path.icon = "/usr/share/icon.svg".into();
        assert_eq!(catalog.add(path), Err(Invalid::Value));
        assert_eq!(catalog.add(command("slate.z", 8)), Err(Invalid::Value));
        assert_eq!(
            catalog.add(command("slate.ok", MENU | STATEFUL | BOUND)),
            Ok(())
        );
    }

    /// Replacing keeps the row where it was, enabled and off.
    #[test]
    fn replacing_a_command_keeps_its_place_and_resets_its_state() {
        let mut catalog = Catalog::default();
        catalog.add(command("a.one", STATEFUL)).unwrap();
        catalog.add(command("a.two", 0)).unwrap();
        catalog.set_state("a.one", 1).unwrap();
        catalog.set_enabled("a.one", 0).unwrap();
        catalog.add(command("a.one", STATEFUL)).unwrap();
        assert_eq!(catalog.commands[0].id, "a.one");
        assert!(catalog.commands[0].enabled && !catalog.commands[0].active);
        assert_eq!(catalog.set_state("a.two", 1), Err(Invalid::Value));
        assert_eq!(catalog.set_enabled("a.gone", 1), Err(Invalid::Unknown));
    }

    #[test]
    fn a_catalog_holds_at_most_256_commands() {
        let mut catalog = Catalog::default();
        for index in 0..256 {
            catalog.add(command(&format!("a.c{index}"), 0)).unwrap();
        }
        assert_eq!(catalog.add(command("a.extra", 0)), Err(Invalid::TooMany));
        assert_eq!(catalog.add(command("a.c3", 0)), Ok(()));
    }

    #[test]
    fn only_the_ten_newest_recents_are_kept_and_listed_newest_first() {
        let mut catalog = Catalog::default();
        for at in 0..12 {
            catalog.add_recent(recent(&format!("r{at}"), at)).unwrap();
        }
        assert_eq!(catalog.recents.len(), 10);
        let listed: Vec<_> = catalog
            .recent_items()
            .iter()
            .map(|r| r.id.clone())
            .collect();
        assert_eq!(listed.first().unwrap(), "r11");
        assert_eq!(listed.last().unwrap(), "r2");
        assert_eq!(catalog.add_recent(recent("bad id", 1)), Err(Invalid::Value));
    }

    /// A selection is only delivered against the catalog it was made from.
    #[test]
    fn a_stale_or_missing_selection_is_never_invoked() {
        let mut staged = Staged::default();
        assert!(!staged.can_invoke(1, "a.one", false));
        staged.pending.add(command("a.one", 0)).unwrap();
        staged.pending.add_recent(recent("doc", 5)).unwrap();
        staged.commit(1).unwrap();
        assert!(staged.can_invoke(1, "a.one", false));
        assert!(staged.can_invoke(1, "doc", true));
        assert!(!staged.can_invoke(1, "doc", false));
        assert!(!staged.can_invoke(2, "a.one", false));
        staged.pending.set_enabled("a.one", 0).unwrap();
        assert_eq!(staged.commit(1), Err(Invalid::Value));
        staged.commit(2).unwrap();
        assert!(!staged.can_invoke(2, "a.one", false));
        assert!(!staged.can_invoke(1, "a.one", false));
    }

    /// Pending edits stay invisible until done.
    #[test]
    fn edits_apply_only_on_commit() {
        let mut staged = Staged::default();
        staged.pending.add(command("a.one", 0)).unwrap();
        staged.commit(7).unwrap();
        staged.pending = Catalog::default();
        staged.pending.add(command("a.two", 0)).unwrap();
        let committed = staged.committed.as_ref().unwrap();
        assert_eq!(committed.commands.len(), 1);
        assert_eq!(committed.generation, 7);
    }
}
