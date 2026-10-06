use std::{
    fmt,
    sync::{
        Arc, Mutex,
        atomic::{AtomicBool, Ordering},
    },
};

use calloop::LoopHandle;
use xkbcommon::xkb::Keysym;
// MERGE: upstream's import churn here is all libcosmic (`cosmic::widget::…`, `theme::…`,
// `menu::menu_column::MenuColumn`). This fork does not depend on the libcosmic widget crate —
// the menu is built from raw iced widgets styled with icetron design tokens — so the icetron
// imports stay.
use iced_core::{
    Alignment, Length, Rectangle as IcedRectangle, alignment::Horizontal, mouse::Interaction,
};
use iced_runtime::Task;
use iced_widget::{self, Column, Row, Space, button, container, svg::Svg};

const ARROW_RIGHT_S_LINE: &[u8] = icetron_themes::icons::CHEVRON_RIGHT.bytes;
const CHECK_LINE: &[u8] = icetron_themes::icons::CHECK.bytes;

use icetron_p::components::{
    dropdown::DropdownVariant,
    menu_card::{self, SubmenuRow},
};
use icetron_p::prelude::styled_text;
use icetron_p::prelude::{DropdownItem, DropdownSection, dropdown, halo_ask_footer, matching};
use icetron_themes::Icon;
use smithay::{
    backend::{
        input::{ButtonState, KeyState, Keycode, TouchSlot},
        renderer::ImportMem,
    },
    desktop::space::SpaceElement,
    input::{
        Seat, SeatHandler,
        keyboard::{
            GrabStartData as KeyboardGrabStartData, KeyboardGrab, KeyboardInnerHandle,
            KeyboardTarget, ModifiersState,
        },
        pointer::{
            AxisFrame, ButtonEvent, CursorImageStatus, GestureHoldBeginEvent, GestureHoldEndEvent,
            GesturePinchBeginEvent, GesturePinchEndEvent, GesturePinchUpdateEvent,
            GestureSwipeBeginEvent, GestureSwipeEndEvent, GestureSwipeUpdateEvent,
            GrabStartData as PointerGrabStartData, MotionEvent as PointerMotionEvent, PointerGrab,
            PointerInnerHandle, PointerTarget, RelativeMotionEvent,
        },
        touch::{
            DownEvent, GrabStartData as TouchGrabStartData, MotionEvent as TouchMotionEvent,
            TouchGrab, TouchInnerHandle, TouchTarget, UpEvent,
        },
    },
    output::Output,
    utils::{Logical, Point, Rectangle, Serial, Size},
};

use crate::{
    backend::render::{cursor::CursorState, element::AsGlowRenderer},
    comp_theme::CompTheme,
    shell::{
        SeatExt, element::window::mouse_interaction_to_cursor_icon,
        focus::target::PointerFocusTarget,
    },
    state::State,
    utils::{
        apply::Apply,
        iced::CompElement,
        iced::{IcedElement, IcedRenderElement, Program},
        prelude::*,
    },
};

use super::{GrabStartData, ResizeEdge};

mod default;
mod item;
mod move_dialog;
#[cfg(test)]
mod tests;
pub use self::default::*;
pub use self::move_dialog::{MoveDialog, MoveRow};

/// Cheap to clone: a render thread takes a copy rather than rasterising under
/// the seat's lock, which input on the main thread waits on.
#[derive(Clone)]
pub struct MenuGrabState {
    elements: Arc<Mutex<Vec<Element>>>,
    screen_space_relative: Option<Output>,
    scale: Arc<Mutex<f64>>,
}
pub type SeatMenuGrabState = Mutex<Option<MenuGrabState>>;
/// Render-only exits. These never participate in input routing or menu grabs.
#[derive(Default)]
pub struct ClosingMenus(Vec<MenuGrabState>);
pub type SeatClosingMenus = Mutex<ClosingMenus>;

impl ClosingMenus {
    fn cleanup(&mut self, now: iced_core::time::Instant) {
        self.0.retain(|menu| {
            !menu
                .elements
                .lock()
                .unwrap()
                .iter()
                .all(|element| element.iced.is_fully_hidden_at(now))
        });
    }
    pub fn render<R>(
        &mut self,
        renderer: &mut R,
        output: &Output,
        push: &mut dyn FnMut(IcedRenderElement<R>, bool),
    ) where
        R: AsGlowRenderer + ImportMem,
        R::TextureId: Send + Clone + 'static,
    {
        self.cleanup(iced_core::time::Instant::now());
        // Newest exiting popup is nearest the still-active popup, if any.
        for menu in self.0.iter().rev() {
            menu.render(renderer, output, &mut |elem| {
                push(elem, !menu.is_in_screen_space())
            });
        }
    }

    fn push(&mut self, menu: MenuGrabState) {
        self.cleanup(iced_core::time::Instant::now());
        if !menu.is_finished() {
            self.0.push(menu);
        }
    }
}

impl MenuGrabState {
    fn begin_close(&self) {
        let mut elements = self.elements.lock().unwrap();
        // Legacy menus have no surface transition and continue closing at once.
        elements.retain(|element| element.iced.with_program(|menu| menu.halo));
        for element in &mut *elements {
            element.pointer_entered = false;
            element.touch_entered = None;
            element
                .iced
                .with_program(|menu| menu.closing.store(true, Ordering::SeqCst));
            element.iced.animate_exit();
        }
    }

    fn is_finished(&self) -> bool {
        self.elements
            .lock()
            .unwrap()
            .iter()
            .all(|element| element.iced.is_fully_hidden())
    }

    pub fn render<R>(
        &self,
        renderer: &mut R,
        output: &Output,
        push: &mut dyn FnMut(IcedRenderElement<R>),
    ) where
        R: AsGlowRenderer + ImportMem,
        R::TextureId: Send + Clone + 'static,
    {
        let scale = output.current_scale().fractional_scale();
        // Rasterising can take milliseconds, and pointer motion over the menu
        // locks `elements` first; render from handles taken out of it instead.
        let elements: Vec<_> = self
            .elements
            .lock()
            .unwrap()
            .iter()
            .map(|elem| (elem.iced.clone(), elem.position))
            .collect();
        for (iced, position) in elements {
            // Another output's frame has nothing of this menu to draw.
            if self.screen_space_relative.is_none()
                && !Rectangle::new(position, iced.current_size().as_global())
                    .overlaps(output.geometry())
            {
                continue;
            }
            iced.push_render_elements(
                renderer,
                position
                    .to_local(output)
                    .as_logical()
                    .to_physical_precise_round(scale),
                scale.into(),
                1.0,
                iced.with_theme(|theme| theme.radius_s())
                    .map(|x| x.round() as u8),
                push,
                None,
            )
        }
    }

    pub fn is_in_screen_space(&self) -> bool {
        self.screen_space_relative.is_some()
    }

    pub fn set_theme(&self, theme: CompTheme) {
        for element in &*self.elements.lock().unwrap() {
            element.iced.set_theme(theme.clone())
        }
    }
}

#[derive(Clone)]
pub enum Item {
    Separator,
    Submenu {
        title: String,
        items: Vec<Item>,
        icon: Option<Icon>,
    },
    Entry {
        title: String,
        shortcut: Option<String>,
        on_press: Arc<Box<dyn Fn(&crate::utils::iced::ProgramLoop) + Send + Sync>>,
        toggled: bool,
        submenu: bool,
        disabled: bool,
        /// A second line under the title. Drawn by Halo menus only.
        subtitle: Option<String>,
        /// A glyph before the title. Drawn by Halo menus only.
        icon: Option<Icon>,
    },
}

impl fmt::Debug for Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Separator => write!(f, "Separator"),
            Self::Submenu { title, items, .. } => f
                .debug_struct("Submenu")
                .field("title", title)
                .field("items", items)
                .finish(),
            Self::Entry {
                title,
                shortcut,
                on_press: _,
                toggled,
                submenu,
                disabled,
                ..
            } => f
                .debug_struct("Entry")
                .field("title", title)
                .field("shortcut", shortcut)
                .field("on_press", &"...")
                .field("toggled", toggled)
                .field("submenu", submenu)
                .field("disabled", disabled)
                .finish(),
        }
    }
}

impl Item {
    pub fn new<S: Into<String>, F: Fn(&crate::utils::iced::ProgramLoop) + Send + Sync + 'static>(
        title: S,
        on_press: F,
    ) -> Item {
        Item::Entry {
            title: title.into(),
            shortcut: None,
            on_press: Arc::new(Box::new(on_press)),
            toggled: false,
            submenu: false,
            disabled: false,
            subtitle: None,
            icon: None,
        }
    }

    pub fn new_submenu<S: Into<String>>(title: S, items: Vec<Item>) -> Item {
        Item::Submenu {
            title: title.into(),
            items,
            icon: None,
        }
    }

    pub fn subtitle(mut self, subtitle: impl Into<String>) -> Self {
        if let Item::Entry {
            subtitle: ref mut s,
            ..
        } = self
        {
            *s = Some(subtitle.into());
        }
        self
    }

    pub fn icon(mut self, glyph: Icon) -> Self {
        match self {
            Item::Entry { ref mut icon, .. } | Item::Submenu { ref mut icon, .. } => {
                *icon = Some(glyph);
            }
            Item::Separator => {}
        }
        self
    }

    pub fn shortcut(mut self, shortcut: impl Into<Option<String>>) -> Self {
        if let Item::Entry {
            shortcut: ref mut s,
            ..
        } = self
        {
            *s = shortcut.into();
        }
        self
    }

    pub fn toggled(mut self, toggled: bool) -> Self {
        if let Item::Entry {
            toggled: ref mut t, ..
        } = self
        {
            *t = toggled;
        }
        self
    }

    pub fn disabled(mut self, disabled: bool) -> Self {
        if let Item::Entry {
            disabled: ref mut d,
            ..
        } = self
        {
            *d = disabled;
        }
        self
    }
}

/// The palette look: the same items, drawn as the window's command list with a
/// pin beside every verb that may live in the header.
///
/// `commands` is index-aligned with [`ContextMenu::items`], so a row press is
/// the ordinary [`Message::ItemPressed`] and nothing has to map ids back to
/// callbacks twice.
pub struct Palette {
    /// Whose pins these are. Per app, never per window.
    pub app_id: String,
    /// The app's name, for the palette's own heading.
    pub scope: String,
    pub commands: Vec<icetron_p::prelude::HaloCommand>,
    /// What has been typed into the search field.
    query: Mutex<String>,
}

impl fmt::Debug for Palette {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Palette")
            .field("app_id", &self.app_id)
            .field("commands", &self.commands.len())
            .finish()
    }
}

impl Palette {
    pub fn new(
        app_id: impl Into<String>,
        scope: impl Into<String>,
        commands: Vec<icetron_p::prelude::HaloCommand>,
    ) -> Self {
        Self {
            app_id: app_id.into(),
            scope: scope.into(),
            commands,
            query: Mutex::new(String::new()),
        }
    }
}

/// Menu that comes up when right-clicking an application header bar
#[derive(Debug)]
pub struct ContextMenu {
    items: Vec<Item>,
    selected: AtomicBool,
    row_width: Mutex<Option<f32>>,
    halo: bool,
    palette: Option<Palette>,
    dialog: Option<MoveDialog>,
    /// Set by a press that must not dismiss the grab. Read and cleared by
    /// [`MenuGrab::button`] right after it hands the press to the widget, which
    /// dispatches messages synchronously.
    keep_open: AtomicBool,
    closing: AtomicBool,
    /// The Halo menu row whose fly-out is open.
    flyout: Mutex<Option<usize>>,
    /// Whether the pointer is on that row.
    on_flyout_row: AtomicBool,
    /// The Halo menu row holding keyboard focus, when the keyboard opened it.
    keyboard_row: Mutex<Option<usize>>,
    /// Whether keyboard focus is in the open fly-out rather than on its row.
    keyboard_flyout: AtomicBool,
}

impl ContextMenu {
    pub fn new(items: Vec<Item>) -> ContextMenu {
        ContextMenu {
            items,
            selected: AtomicBool::new(false),
            row_width: Mutex::new(None),
            halo: false,
            palette: None,
            dialog: None,
            keep_open: AtomicBool::new(false),
            closing: AtomicBool::new(false),
            flyout: Mutex::new(None),
            on_flyout_row: AtomicBool::new(false),
            keyboard_row: Mutex::new(None),
            keyboard_flyout: AtomicBool::new(false),
        }
    }

    fn focus_ring(&self) -> Option<usize> {
        let row = *self.keyboard_row.lock().unwrap();
        row.filter(|_| !self.keyboard_flyout.load(Ordering::SeqCst))
    }

    /// The Halo menu's rows as the menu card draws them, and the pointer
    /// reports every row makes so a fly-out opens and closes as it moves.
    fn halo_sections(&self) -> (Vec<DropdownSection<'_, Message>>, Vec<SubmenuRow<Message>>) {
        let open = *self.flyout.lock().unwrap();
        let mut rows = Vec::new();
        let sections = self
            .items
            .iter()
            .enumerate()
            .map(|(idx, item)| {
                let (icon, item) = match item {
                    Item::Separator => return DropdownSection::Divider,
                    Item::Entry {
                        title,
                        shortcut,
                        disabled,
                        toggled,
                        subtitle,
                        icon,
                        ..
                    } => {
                        let mut row = DropdownItem::new(title, Message::ItemPressed(idx))
                            .disabled(*disabled)
                            .active(*toggled);
                        if let Some(shortcut) = shortcut {
                            row = row.shortcut(shortcut);
                        }
                        if let Some(subtitle) = subtitle {
                            row = row.subtitle(subtitle);
                        }
                        (icon, row)
                    }
                    Item::Submenu { title, icon, .. } => (
                        icon,
                        DropdownItem::new(title, Message::RowEntered(idx))
                            .submenu()
                            .active(open == Some(idx)),
                    ),
                };
                rows.push(SubmenuRow {
                    section: idx,
                    on_enter: Message::RowEntered(idx),
                    on_exit: Message::RowLeft(idx),
                });
                DropdownSection::Item(match icon {
                    Some(icon) => item.icon((*icon).into()),
                    None => item,
                })
            })
            .collect();
        (sections, rows)
    }

    pub fn set_row_width(&self, width: f32) {
        *self.row_width.lock().unwrap() = Some(width);
    }
}

#[derive(Debug, Clone)]
pub enum Message {
    ItemEntered(usize, IcedRectangle<f32>),
    ItemPressed(usize),
    ItemLeft(usize, IcedRectangle<f32>),
    /// Pin or unpin a command. Deliberately NOT a selection: pinning is a
    /// statement about the header, so the palette stays up to show the result.
    TogglePin(String),
    /// The search field changed.
    Query(String),
    /// Enter in the search field: run the first row the query leaves, or hand
    /// the query to chat when it leaves none.
    Submit,
    /// The Ask row: open chat with the query.
    AskChat,
    /// The pointer reached a Halo menu row: open its fly-out, or close another's.
    RowEntered(usize),
    /// The pointer left a Halo menu row.
    RowLeft(usize),
    /// A dialog's Cancel or close button: done, with nothing chosen.
    Dismiss,
    /// Tab (forward) or Shift+Tab in a dialog.
    DialogStep(bool),
    /// Enter or Space on the dialog's focused button.
    DialogActivate,
    /// The keyboard in a Halo menu.
    MenuKey(MenuKey),
}

impl item::CursorEvents for Message {
    fn cursor_entered(idx: usize, bounds: IcedRectangle<f32>) -> Self {
        Message::ItemEntered(idx, bounds)
    }

    fn cursor_left(idx: usize, bounds: IcedRectangle<f32>) -> Self {
        Message::ItemLeft(idx, bounds)
    }
}

impl Program for ContextMenu {
    type Message = Message;

    fn program_name() -> &'static str {
        "ContextMenu"
    }

    fn visibility(&self, theme: &CompTheme) -> Option<crate::utils::iced::Visibility> {
        self.halo.then(|| crate::utils::iced::Visibility {
            visible: !self.closing.load(Ordering::SeqCst),
            ..crate::utils::iced::Visibility::context_menu(theme.motion)
        })
    }

    fn update(
        &mut self,
        message: Self::Message,
        loop_handle: &crate::utils::iced::ProgramLoop,
        last_seat: Option<&(Seat<State>, Serial)>,
    ) -> Task<Self::Message> {
        if self.closing.load(Ordering::SeqCst) {
            return Task::none();
        }
        match message {
            Message::ItemPressed(idx) => {
                if let Some(Item::Entry {
                    on_press,
                    disabled: false,
                    ..
                }) = self.items.get_mut(idx)
                {
                    (on_press)(loop_handle);
                    self.selected.store(true, Ordering::SeqCst);
                }
                // TODO: If Submenu, then also expand on "Pressed" for touch events.
                // But right now we don't have any touch responsive menus with submenus
            }
            Message::TogglePin(id) => {
                // A pin is a statement about the header, not a choice of verb, so
                // the palette stays up and shows what it did.
                self.keep_open.store(true, Ordering::SeqCst);
                if let Some(palette) = self.palette.as_ref() {
                    use crate::shell::element::window::commands;
                    let outcome = commands::toggle_pin(&palette.app_id, &id);
                    let app_id = palette.app_id.clone();
                    loop_handle.insert_idle(move |state| {
                        if outcome != icetron_p::prelude::PinOutcome::Full {
                            crate::shell::element::window::CosmicWindow::refresh_app_halos(
                                &state.common.shell.read(),
                                &app_id,
                            );
                        }
                        let keys = state.common.config.shortcut_for_action(
                            &cosmic_settings_config::shortcuts::Action::WindowCommands,
                        );
                        if let Some((message, tone)) = commands::pin_receipt(outcome, keys) {
                            state.common.dbus_state.system_toast(message, tone);
                        }
                    });
                }
            }
            Message::Query(query) => {
                if let Some(palette) = self.palette.as_ref() {
                    *palette.query.lock().unwrap() = query;
                }
            }
            Message::Submit => {
                if let Some(palette) = self.palette.as_ref() {
                    let query = palette.query.lock().unwrap().clone();
                    let first = matching(&palette.commands, &query)
                        .first()
                        .and_then(|hit| palette.commands.iter().position(|c| c.id == hit.id));
                    let next = first.map_or(Message::AskChat, Message::ItemPressed);
                    return self.update(next, loop_handle, last_seat);
                }
            }
            Message::Dismiss => self.selected.store(true, Ordering::SeqCst),
            Message::MenuKey(key) => {
                let (sections, _) = self.halo_sections();
                let mut row = self.keyboard_row.lock().unwrap();
                let on_submenu = row
                    .is_some_and(|row| matches!(self.items.get(row), Some(Item::Submenu { .. })));
                let press = match key {
                    MenuKey::First => {
                        *row = menu_card::first_focusable(&sections);
                        None
                    }
                    MenuKey::Last => {
                        *row =
                            menu_card::step_focus(&sections, None, menu_card::FocusStep::Previous);
                        None
                    }
                    MenuKey::Step(forward) => {
                        let step = if forward {
                            menu_card::FocusStep::Next
                        } else {
                            menu_card::FocusStep::Previous
                        };
                        *row = menu_card::step_focus(&sections, *row, step);
                        None
                    }
                    // The caller shows the fly-out and moves focus into it.
                    MenuKey::Press | MenuKey::Open if on_submenu => {
                        *self.flyout.lock().unwrap() = *row;
                        self.keyboard_flyout.store(true, Ordering::SeqCst);
                        None
                    }
                    MenuKey::Press => row.and_then(|row| menu_card::row_press(&sections, row)),
                    MenuKey::Open | MenuKey::Back => None,
                };
                drop(row);
                drop(sections);
                if let Some(press) = press {
                    return self.update(press, loop_handle, last_seat);
                }
            }
            Message::DialogStep(forward) => {
                if let Some(dialog) = self.dialog.as_mut() {
                    dialog.step(forward);
                }
            }
            Message::DialogActivate => {
                let next = match self.dialog.as_ref().map(MoveDialog::focused) {
                    Some(move_dialog::Target::Row(idx)) => Message::ItemPressed(idx),
                    Some(_) => Message::Dismiss,
                    None => return Task::none(),
                };
                return self.update(next, loop_handle, last_seat);
            }
            Message::AskChat => {
                if let Some(palette) = self.palette.as_ref() {
                    let query = palette.query.lock().unwrap().clone();
                    loop_handle.insert_idle(move |state| {
                        crate::shell::element::window::commands::open_chat(state, &query);
                    });
                    self.selected.store(true, Ordering::SeqCst);
                }
            }
            Message::RowEntered(idx) => {
                let submenu = match self.items.get(idx) {
                    Some(Item::Submenu { items, .. }) => Some(items.clone()),
                    _ => None,
                };
                self.on_flyout_row
                    .store(submenu.is_some(), Ordering::SeqCst);
                let mut flyout = self.flyout.lock().unwrap();
                if *flyout == Some(idx) {
                    return Task::none();
                }
                let had = flyout.is_some();
                *flyout = submenu.is_some().then_some(idx);
                drop(flyout);
                self.keyboard_flyout.store(false, Ordering::SeqCst);
                if (had || submenu.is_some())
                    && let Some((seat, _)) = last_seat.cloned()
                {
                    loop_handle.insert_idle(move |_| {
                        show_flyout(&seat, submenu.map(|items| (idx, items)));
                    });
                }
            }
            Message::RowLeft(idx) => {
                self.on_flyout_row.store(false, Ordering::SeqCst);
                if *self.flyout.lock().unwrap() == Some(idx)
                    && let Some((seat, _)) = last_seat.cloned()
                {
                    // A grace period, so the pointer can cross into the fly-out.
                    loop_handle.insert_idle(move |state| {
                        let _ = state.common.event_loop_handle.insert_source(
                            calloop::timer::Timer::from_duration(menu_card::grace_delay()),
                            move |_, _, _| {
                                settle_flyout(&seat, idx);
                                calloop::timer::TimeoutAction::Drop
                            },
                        );
                    });
                }
            }
            Message::ItemEntered(idx, bounds) => {
                if let Some(Item::Submenu { items, .. }) = self.items.get_mut(idx)
                    && let Some((seat, _)) = last_seat.cloned()
                {
                    let items = items.clone();
                    loop_handle.insert_idle(move |state| {
                        let grab_state = seat
                            .user_data()
                            .get::<SeatMenuGrabState>()
                            .unwrap()
                            .lock()
                            .unwrap();

                        if let Some(grab_state) = &*grab_state {
                            let mut elements = grab_state.elements.lock().unwrap();

                            let position = elements.last().unwrap().position;
                            // MERGE: upstream sets `theme.transparent =
                            // theme.cosmic().frosted_system_interface` here to opt the submenu into
                            // its frosted-glass backdrop. `CompTheme` has no `transparent` flag —
                            // the fork's menu surface token is opaque by design — so the theme is
                            // handed over unchanged.
                            let element = IcedElement::new(
                                ContextMenu::new(items),
                                Size::default(),
                                state.common.event_loop_handle.clone(),
                                state.common.theme.clone(),
                            );

                            let min_size = element.minimum_size();
                            element.with_program(|p| {
                                *p.row_width.lock().unwrap() = Some(min_size.w as f32);
                            });
                            let min_size = element.minimum_size();
                            element.resize(min_size);

                            let output = seat.active_output();
                            let position = [
                                // to the right -> down
                                Rectangle::new(
                                    position
                                        + Point::from((
                                            bounds.width.floor() as i32,
                                            bounds.y.ceil() as i32,
                                        )),
                                    min_size.as_global(),
                                ),
                                // to the right -> up
                                Rectangle::new(
                                    position
                                        + Point::from((
                                            bounds.width.floor() as i32,
                                            bounds.y.ceil() as i32 + bounds.height.ceil() as i32
                                                - min_size.h,
                                        )),
                                    min_size.as_global(),
                                ),
                                // to the left -> down
                                Rectangle::new(
                                    position
                                        + Point::from((-min_size.w + 1, bounds.y.ceil() as i32)),
                                    min_size.as_global(),
                                ),
                                // to the left -> up
                                Rectangle::new(
                                    position
                                        + Point::from((
                                            -min_size.w + 1,
                                            bounds.y.ceil() as i32 + bounds.height.ceil() as i32
                                                - min_size.h,
                                        )),
                                    min_size.as_global(),
                                ),
                            ]
                            .iter()
                            .rev() // preference of max_by_key is backwards
                            .max_by_key(|rect| {
                                output
                                    .geometry()
                                    .intersection(**rect)
                                    .map(|rect| rect.size.w * rect.size.h)
                            })
                            .unwrap()
                            .loc;
                            element.output_enter(&output, element.bbox());
                            element.set_additional_scale(*grab_state.scale.lock().unwrap());

                            elements.push(Element {
                                iced: element,
                                position,
                                pointer_entered: false,
                                touch_entered: None,
                            })
                        }
                    });
                }
            }
            Message::ItemLeft(idx, _) => {
                if let Some(Item::Submenu { .. }) = self.items.get_mut(idx)
                    && let Some((seat, _)) = last_seat.cloned()
                {
                    loop_handle.insert_idle(move |_| {
                        let grab_state = seat
                            .user_data()
                            .get::<SeatMenuGrabState>()
                            .unwrap()
                            .lock()
                            .unwrap();

                        if let Some(grab_state) = &*grab_state {
                            let mut elements = grab_state.elements.lock().unwrap();
                            elements.pop();
                        }
                    });
                }
            }
        };

        Task::none()
    }

    fn view<'a>(&'a self, theme: &'a CompTheme) -> CompElement<'a, Self::Message> {
        if let Some(dialog) = &self.dialog {
            return move_dialog::view(dialog, theme);
        }
        if let Some(palette) = &self.palette {
            let commands = &palette.commands;
            let mut card = icetron_p::prelude::halo_palette(&palette.scope, &**theme)
                .commands(commands.clone())
                .pins(crate::shell::element::window::commands::pins(
                    &palette.app_id,
                ))
                .query(palette.query.lock().unwrap().clone())
                .on_query(Message::Query)
                .on_submit(Message::Submit)
                .search_id(search_field_id())
                .on_run(move |id| {
                    Message::ItemPressed(
                        commands
                            .iter()
                            .position(|command| command.id == id)
                            .unwrap_or(usize::MAX),
                    )
                })
                .on_pin(Message::TogglePin)
                // The compositor blurs the surface behind this one already.
                .backdrop(false)
                // A menu grab does not route a scroll to its surface, so the
                // card grows to its rows instead of hiding some below a
                // cut-off nothing can move.
                .rows_height(None);
            card = card.footer(halo_ask_footer(
                crate::fl!("halo-ask-chat"),
                crate::fl!("halo-ask-chat-hint"),
                Message::AskChat,
                &**theme,
            ));
            let card: CompElement<'a, Self::Message> = card.into();
            return container(card).padding(palette_padding(theme)).into();
        }
        if self.halo {
            let (sections, rows) = self.halo_sections();
            return container(
                dropdown(&**theme)
                    .variant(DropdownVariant::Menu)
                    .focus_ring(self.focus_ring())
                    .shadow(true)
                    .sections_with_submenus(sections, rows),
            )
            .padding(halo_menu_padding(theme))
            .into();
        }
        let width = self
            .row_width
            .lock()
            .unwrap()
            .map(Length::Fixed)
            .unwrap_or(Length::Shrink);
        let mode = match width {
            Length::Shrink => Length::Shrink,
            _ => Length::Fill,
        };

        // Theme tokens matching icetron dropdown styling
        let text_style = theme.text_styles().body();
        let text_primary = theme.text_primary();
        let text_secondary = theme.text_secondary();
        let text_tertiary = theme.text_tertiary();
        let text_quaternary = theme.text_quaternary();
        let state_hovered_neutral = theme.state_hovered_neutral();
        let state_pressed_neutral = theme.state_pressed_neutral();
        let divider_color = theme.dropdown_divider();
        let border_width = theme.border_width1();
        let border_color = theme.stroke_subtler();
        let fill_default = {
            let mut c = theme.fill_default();
            c.a = 1.0; // Force opaque — no blur behind compositor menus
            c
        };
        let border_radius = theme.radii_md();
        let padding_item_v = theme.spacing_1_5();
        let padding_item_h = theme.spacing_3();
        let divider_padding_h = theme.spacing_2();
        let divider_padding_v = theme.spacing_1();
        let padding_outer_v = theme.spacing_1();
        let gap_between_items = theme.ui_gap_3xs();

        // MERGE: upstream switched this to libcosmic's `MenuColumn` + `divider::horizontal::light()`
        // so its frosted-glass menu background paints behind the items. This fork has no libcosmic
        // widgets and paints an opaque token-styled surface, so the icetron `Column` + custom
        // divider are kept.
        Column::with_children(self.items.iter().enumerate().map(|(idx, item)| {
            match item {
                Item::Separator => {
                    // Divider matching icetron dropdown style
                    container(
                        container(Space::new())
                            .width(Length::Fill)
                            .height(iced_core::Length::Fixed(border_width))
                            .class(Box::new(move |_: &iced_core::Theme| container::Style {
                                text_color: None,
                                background: Some(iced_core::Background::Color(divider_color)),
                                ..Default::default()
                            })
                                as Box<dyn Fn(&iced_core::Theme) -> container::Style>),
                    )
                    .width(Length::Fill)
                    .padding(iced_core::Padding {
                        top: divider_padding_v,
                        bottom: divider_padding_v,
                        left: divider_padding_h,
                        right: divider_padding_h,
                    })
                    .into()
                }
                Item::Submenu { title, .. } => Row::with_children(vec![
                    Space::new().width(16.0).height(Length::Shrink).into(),
                    styled_text(title.as_str(), text_style, text_secondary)
                        .width(mode)
                        .into(),
                    Svg::new(iced_core::svg::Handle::from_memory(ARROW_RIGHT_S_LINE))
                        .width(16.0)
                        .height(16.0)
                        .style(move |_theme, _status| iced_widget::svg::Style {
                            color: Some(text_tertiary),
                        })
                        .into(),
                ])
                .spacing(8)
                .width(width)
                .padding(iced_core::Padding {
                    top: padding_item_v,
                    bottom: padding_item_v,
                    left: padding_item_h,
                    right: padding_item_h,
                })
                .align_y(Alignment::Center)
                .apply(|row| item::SubmenuItem::new(row, idx, theme))
                .into(),
                Item::Entry {
                    title,
                    shortcut,
                    toggled,
                    disabled,
                    ..
                } => {
                    let is_disabled = *disabled;
                    let is_toggled = *toggled;
                    let content_color = if is_disabled {
                        text_quaternary
                    } else {
                        text_secondary
                    };

                    let mut components: Vec<CompElement<'_, Message>> = vec![
                        if is_toggled {
                            Svg::new(iced_core::svg::Handle::from_memory(CHECK_LINE))
                                .width(16.0)
                                .height(16.0)
                                .style(move |_theme, _status| iced_widget::svg::Style {
                                    color: Some(content_color),
                                })
                                .into()
                        } else {
                            Space::new().width(16.0).height(Length::Shrink).into()
                        },
                        // MERGE: upstream dims the disabled label by halving the alpha of the
                        // cosmic component palette; the fork bakes that into `content_color`
                        // (`text_quaternary` when disabled) above.
                        styled_text(title.as_str(), text_style, content_color)
                            .width(mode)
                            .into(),
                        Space::new().width(16.0).height(Length::Shrink).into(),
                    ];
                    if let Some(shortcut) = shortcut.as_ref() {
                        components.push(
                            // MERGE: upstream tints the shortcut with a 0.75-alpha cosmic
                            // component color; the fork uses the `text_tertiary` token instead.
                            styled_text(shortcut.as_str(), text_style, text_tertiary)
                                .align_x(Horizontal::Right)
                                .width(Length::Shrink)
                                .into(),
                        );
                    }

                    button(
                        Row::with_children(components)
                            .spacing(8)
                            .width(mode)
                            .align_y(Alignment::Center),
                    )
                    .width(width)
                    .padding(iced_core::Padding {
                        top: padding_item_v,
                        bottom: padding_item_v,
                        left: padding_item_h,
                        right: padding_item_h,
                    })
                    .style(move |_theme, status| {
                        let (bg_color, txt_color) = match status {
                            button::Status::Hovered => (state_hovered_neutral, text_primary),
                            button::Status::Pressed => (state_pressed_neutral, content_color),
                            _ => (iced_core::Color::TRANSPARENT, content_color),
                        };
                        button::Style {
                            background: Some(iced_core::Background::Color(bg_color)),
                            border: iced_core::Border::default(),
                            text_color: txt_color,
                            ..Default::default()
                        }
                    })
                    .on_press_maybe((!disabled).then_some(Message::ItemPressed(idx)))
                    .into()
                }
            }
        }))
        .spacing(gap_between_items)
        .width(Length::Shrink)
        // MERGE: upstream repainted this container from the frosted cosmic component palette
        // (`cosmic.background(frosted_windows)`). The fork keeps its opaque token-styled surface —
        // `CompTheme` has no frosted/alpha-map concept.
        .apply(container)
        .padding(iced_core::Padding {
            top: padding_outer_v,
            bottom: padding_outer_v,
            left: 0.0,
            right: 0.0,
        })
        .class(Box::new(move |_: &iced_core::Theme| container::Style {
            text_color: None,
            background: Some(iced_core::Background::Color(fill_default)),
            border: iced_core::Border {
                radius: border_radius.into(),
                width: border_width,
                color: border_color,
                ..Default::default()
            },
            ..Default::default()
        })
            as Box<dyn Fn(&iced_core::Theme) -> container::Style>)
        .width(Length::Shrink)
        .into()
    }

    fn backdrop_blur(
        &self,
        theme: &CompTheme,
        size: Size<i32, Logical>,
        _layers: &[iced_tiny_skia::Layer],
        radii: [u8; 4],
    ) -> Option<(IcedRectangle, [u8; 4])> {
        if self.halo {
            // The palette's card is rounded to `radii_xl` under the popover
            // shadow; the dropdown to its own radius under the menu shadow.
            let (padding, radius) = if self.dialog.is_some() {
                (move_dialog::padding(theme), theme.radii_md())
            } else if self.palette.is_some() {
                (palette_padding(theme), theme.radii_xl())
            } else {
                (halo_menu_padding(theme), theme.radii_md())
            };
            Some((
                IcedRectangle {
                    x: padding.left,
                    y: padding.top,
                    width: (size.w as f32 - padding.left - padding.right).max(0.0),
                    height: (size.h as f32 - padding.top - padding.bottom).max(0.0),
                },
                [radius.round().clamp(0.0, 255.0) as u8; 4],
            ))
        } else {
            theme.header_backdrop_blur().then_some((
                IcedRectangle::with_size(iced_core::Size::new(size.w as f32, size.h as f32)),
                radii,
            ))
        }
    }
}

/// The Halo menu is the design's menu card, which wears the elevated shadow.
fn halo_menu_padding(theme: &CompTheme) -> iced_core::Padding {
    shadow_padding(&theme.shadow_elevated())
}

/// Replace the Halo menu's fly-out with `flyout`'s rows beside row `idx`, or close it.
fn show_flyout(seat: &Seat<State>, flyout: Option<(usize, Vec<Item>)>) {
    let grab_state = seat
        .user_data()
        .get::<SeatMenuGrabState>()
        .unwrap()
        .lock()
        .unwrap();
    let Some(grab_state) = &*grab_state else {
        return;
    };
    let mut elements = grab_state.elements.lock().unwrap();
    elements.truncate(1);
    let Some(root) = elements.first() else {
        return;
    };
    root.iced.force_update();
    let Some((idx, items)) = flyout else {
        return;
    };
    let theme = root.iced.with_theme(|theme| theme.clone());
    let Some(span) = root.iced.with_program(|menu| {
        let (sections, _) = menu.halo_sections();
        menu_card::row_span(&sections, idx, &*theme)
    }) else {
        return;
    };
    let mut menu = ContextMenu::new(items);
    menu.halo = true;
    let element = IcedElement::new(
        menu,
        Size::default(),
        root.iced.loop_handle(),
        theme.clone(),
    );
    let size = element.minimum_size();
    element.resize(size);
    // Both cards sit the same shadow padding inside their surfaces.
    let origin = menu_card::flyout_origin(span, &*theme);
    let position = root.position + Point::from((origin.x.round() as i32, origin.y.round() as i32));
    element.output_enter(&seat.active_output(), element.bbox());
    element.set_additional_scale(*grab_state.scale.lock().unwrap());
    elements.push(Element {
        iced: element,
        position,
        pointer_entered: false,
        touch_entered: None,
    });
}

/// After the grace period: keep the fly-out only if the pointer went into it.
fn settle_flyout(seat: &Seat<State>, idx: usize) {
    let grab_state = seat
        .user_data()
        .get::<SeatMenuGrabState>()
        .unwrap()
        .lock()
        .unwrap();
    let Some(grab_state) = &*grab_state else {
        return;
    };
    let mut elements = grab_state.elements.lock().unwrap();
    let in_flyout = elements.get(1).is_some_and(|flyout| flyout.pointer_entered);
    let Some(root) = elements.first() else {
        return;
    };
    let still_open = root.iced.with_program(|menu| {
        let mut flyout = menu.flyout.lock().unwrap();
        let open = *flyout == Some(idx);
        if open && !in_flyout && !menu.on_flyout_row.load(Ordering::SeqCst) {
            *flyout = None;
            menu.keyboard_flyout.store(false, Ordering::SeqCst);
            return false;
        }
        open
    });
    if !still_open {
        elements.truncate(1);
        if let Some(root) = elements.first() {
            root.iced.force_update();
        }
    }
}

/// The palette's card wears the popover shadow, not the dropdown's.
fn palette_padding(theme: &CompTheme) -> iced_core::Padding {
    shadow_padding(&theme.shadow_popover())
}

/// Room a surface leaves around its body for the shadow it draws.
fn shadow_padding(shadows: &[iced_core::Shadow]) -> iced_core::Padding {
    let mut padding = iced_core::Padding::ZERO;
    for shadow in shadows.iter().filter(|s| !s.inset && s.color.a > 0.0) {
        let reach = shadow.blur_radius.max(0.0) + shadow.spread_radius.max(0.0);
        padding.top = padding.top.max((reach - shadow.offset.y).ceil());
        padding.bottom = padding.bottom.max((reach + shadow.offset.y).ceil());
        padding.left = padding.left.max((reach - shadow.offset.x).ceil());
        padding.right = padding.right.max((reach + shadow.offset.x).ceil());
    }
    padding
}

pub struct Element {
    iced: IcedElement<ContextMenu>,
    position: Point<i32, Global>,
    pointer_entered: bool,
    touch_entered: Option<TouchSlot>,
}

impl Element {
    /// Re-measure a palette whose rows just changed, so the surface — and with
    /// it the blur and the click-through region — fits the card rather than
    /// the tallest list it ever showed.
    fn refit(&self) {
        if self.iced.with_program(|p| p.palette.is_some()) {
            self.iced.resize(self.iced.minimum_size());
        }
    }

    /// Give the palette's search field the caret back.
    fn refocus_search(&self) {
        if self.iced.with_program(|p| p.palette.is_some()) {
            self.iced
                .queue_operation(iced_core::widget::operation::focusable::focus(
                    search_field_id(),
                ));
        }
    }

    fn input_bbox(&self) -> Rectangle<f64, Logical> {
        if self.iced.with_program(|p| p.halo)
            && let Some(mut bounds) = self.iced.backdrop_input_bounds()
        {
            bounds.loc += self.position.as_logical().to_f64();
            return bounds;
        }
        let mut bounds = self.iced.bbox().to_f64();
        bounds.loc = self.position.as_logical().to_f64();
        bounds
    }
}

pub struct MenuGrab {
    elements: Arc<Mutex<Vec<Element>>>,
    start_data: GrabStartData,
    seat: Seat<State>,
    screen_space_relative: Option<Output>,
    scale: Arc<Mutex<f64>>,
    on_close: Option<Box<dyn FnOnce() + Send>>,
}

fn set_menu_cursor(seat: &Seat<State>, interaction: Option<Interaction>) {
    if let Some(cursor) = seat.user_data().get::<CursorState>() {
        let mut cursor = cursor.lock().unwrap();
        if let Some(interaction) = interaction {
            cursor.set_shape(mouse_interaction_to_cursor_icon(interaction));
        } else {
            cursor.unset_shape();
        }
    }
    seat.set_cursor_image_status(
        interaction.map_or_else(CursorImageStatus::default_named, |interaction| {
            CursorImageStatus::Named(mouse_interaction_to_cursor_icon(interaction))
        }),
    );
}

impl PointerGrab<State> for MenuGrab {
    fn motion(
        &mut self,
        state: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        _focus: Option<(PointerFocusTarget, Point<f64, Logical>)>,
        event: &PointerMotionEvent,
    ) {
        let mut interaction = Interaction::None;
        {
            let mut guard = self.elements.lock().unwrap();
            let elements = &mut *guard;
            let event_location = if let Some(output) = self.screen_space_relative.as_ref() {
                if state.common.shell.read().zoom_state().is_some() {
                    event
                        .location
                        .as_global()
                        .to_zoomed(output)
                        .to_global(output)
                        .as_logical()
                } else {
                    event.location
                }
            } else {
                event.location
            };

            if let Some(i) = elements.iter().position(|elem| {
                let bbox = elem.input_bbox();

                bbox.contains(event_location)
            }) {
                let element = &mut elements[i];

                let new_event = PointerMotionEvent {
                    location: event_location - element.position.as_logical().to_f64(),
                    serial: event.serial,
                    time: event.time,
                };
                if !element.pointer_entered {
                    PointerTarget::enter(&element.iced, &self.seat, state, &new_event);
                    element.pointer_entered = true;
                } else {
                    PointerTarget::motion(&element.iced, &self.seat, state, &new_event);
                }
                interaction = element.iced.mouse_interaction();
            } else {
                // Legacy nested menus keep the root selected while travelling to
                // a submenu. Halo is a single dropdown: leaving must clear its row.
                let keep_root = !elements
                    .first()
                    .is_some_and(|element| element.iced.with_program(|p| p.halo));
                elements
                    .iter_mut()
                    .filter(|element| element.pointer_entered)
                    .skip(usize::from(keep_root))
                    .for_each(|element| {
                        PointerTarget::leave(
                            &element.iced,
                            &self.seat,
                            state,
                            event.serial,
                            event.time,
                        );
                        element.pointer_entered = false;
                    })
            }
        }
        handle.motion(state, None, event);
        // Clearing the old focus can reset the cursor in its leave handler.
        // Apply the menu's choice afterwards, without re-locking the pointer handle.
        set_menu_cursor(&self.seat, Some(interaction));
    }

    fn relative_motion(
        &mut self,
        state: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        _focus: Option<(PointerFocusTarget, Point<f64, Logical>)>,
        event: &RelativeMotionEvent,
    ) {
        // While the grab is active, no client has pointer focus
        handle.relative_motion(state, None, event);
    }

    fn button(
        &mut self,
        state: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &ButtonEvent,
    ) {
        let any_entered = self
            .elements
            .lock()
            .unwrap()
            .iter()
            .any(|elem| elem.pointer_entered);
        if !any_entered {
            if event.state == ButtonState::Pressed {
                handle.unset_grab(self, state, event.serial, event.time, true);
            }
        } else {
            let (selected, keep_open) = {
                let elements = self.elements.lock().unwrap();
                let mut selected = false;
                let mut keep_open = false;
                for element in elements.iter().filter(|elem| elem.pointer_entered) {
                    PointerTarget::button(&element.iced, &self.seat, state, event);
                    element.refit();
                    // Dispatched synchronously above, so the flag is already set
                    // if the press was one the surface wants to survive.
                    let kept = element
                        .iced
                        .with_program(|p| p.keep_open.swap(false, Ordering::SeqCst));
                    if kept {
                        // The press took the caret with it, and a palette that
                        // stays up is one the search field should still own.
                        element.refocus_search();
                    }
                    keep_open |= kept;
                    selected = true;
                }
                (selected, keep_open)
            };
            if selected && !keep_open && event.state == ButtonState::Released {
                handle.unset_grab(self, state, event.serial, event.time, true);
            } else {
                handle.button(state, event);
            }
        }
    }

    fn axis(
        &mut self,
        state: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        details: AxisFrame,
    ) {
        handle.axis(state, details);
    }

    fn frame(&mut self, data: &mut State, handle: &mut PointerInnerHandle<'_, State>) {
        handle.frame(data)
    }

    fn gesture_swipe_begin(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GestureSwipeBeginEvent,
    ) {
        handle.gesture_swipe_begin(data, event)
    }

    fn gesture_swipe_update(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GestureSwipeUpdateEvent,
    ) {
        handle.gesture_swipe_update(data, event)
    }

    fn gesture_swipe_end(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GestureSwipeEndEvent,
    ) {
        handle.gesture_swipe_end(data, event)
    }

    fn gesture_pinch_begin(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GesturePinchBeginEvent,
    ) {
        handle.gesture_pinch_begin(data, event)
    }

    fn gesture_pinch_update(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GesturePinchUpdateEvent,
    ) {
        handle.gesture_pinch_update(data, event)
    }

    fn gesture_pinch_end(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GesturePinchEndEvent,
    ) {
        handle.gesture_pinch_end(data, event)
    }

    fn gesture_hold_begin(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GestureHoldBeginEvent,
    ) {
        handle.gesture_hold_begin(data, event)
    }

    fn gesture_hold_end(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GestureHoldEndEvent,
    ) {
        handle.gesture_hold_end(data, event)
    }

    fn start_data(&self) -> &PointerGrabStartData<State> {
        match &self.start_data {
            GrabStartData::Pointer(start_data) => start_data,
            _ => unreachable!(),
        }
    }

    fn unset(&mut self, data: &mut State) {
        set_menu_cursor(&self.seat, None);
        release_palette_keyboard(&self.seat, data);
    }
}

impl TouchGrab<State> for MenuGrab {
    fn down(
        &mut self,
        data: &mut State,
        handle: &mut TouchInnerHandle<'_, State>,
        _focus: Option<(PointerFocusTarget, Point<f64, Logical>)>,
        event: &DownEvent,
    ) {
        {
            let mut guard = self.elements.lock().unwrap();
            let elements = &mut *guard;
            let event_location = if let Some(output) = self.screen_space_relative.as_ref() {
                if data.common.shell.read().zoom_state().is_some() {
                    event
                        .location
                        .as_global()
                        .to_zoomed(output)
                        .to_global(output)
                        .as_logical()
                } else {
                    event.location
                }
            } else {
                event.location
            };

            if let Some(i) = elements.iter().position(|elem| {
                let bbox = elem.input_bbox();

                bbox.contains(event_location)
            }) {
                let element = &mut elements[i];

                let new_event = DownEvent {
                    slot: event.slot,
                    location: event_location - element.position.as_logical().to_f64(),
                    serial: event.serial,
                    time: event.time,
                };
                if element.touch_entered.is_none() {
                    TouchTarget::down(&element.iced, &self.seat, data, &new_event);
                    element.touch_entered = Some(event.slot);
                }
            }
        }
        handle.down(data, None, event);
    }

    fn up(&mut self, data: &mut State, handle: &mut TouchInnerHandle<'_, State>, event: &UpEvent) {
        {
            let elements = self.elements.lock().unwrap();
            for element in elements.iter().filter(|elem| {
                elem.touch_entered
                    .as_ref()
                    .is_some_and(|slot| *slot == event.slot)
            }) {
                TouchTarget::up(&element.iced, &self.seat, data, event);
            }
        }
        handle.unset_grab(self, data);
    }

    fn motion(
        &mut self,
        data: &mut State,
        handle: &mut TouchInnerHandle<'_, State>,
        _focus: Option<(PointerFocusTarget, Point<f64, Logical>)>,
        event: &TouchMotionEvent,
    ) {
        {
            let elements = self.elements.lock().unwrap();
            for element in elements.iter().filter(|elem| {
                elem.touch_entered
                    .as_ref()
                    .is_some_and(|slot| *slot == event.slot)
            }) {
                TouchTarget::motion(&element.iced, &self.seat, data, event);
            }
        }
        handle.motion(data, None, event);
    }

    fn frame(&mut self, data: &mut State, handle: &mut TouchInnerHandle<'_, State>) {
        handle.frame(data);
    }

    fn cancel(&mut self, data: &mut State, handle: &mut TouchInnerHandle<'_, State>) {
        {
            let mut elements = self.elements.lock().unwrap();
            for element in elements.iter_mut() {
                let _ = element.touch_entered.take();
            }
        }
        handle.cancel(data);
    }

    fn shape(
        &mut self,
        data: &mut State,
        handle: &mut TouchInnerHandle<'_, State>,
        event: &smithay::input::touch::ShapeEvent,
    ) {
        handle.shape(data, event);
    }

    fn orientation(
        &mut self,
        data: &mut State,
        handle: &mut TouchInnerHandle<'_, State>,
        event: &smithay::input::touch::OrientationEvent,
    ) {
        handle.orientation(data, event);
    }

    fn start_data(&self) -> &TouchGrabStartData<State> {
        match &self.start_data {
            GrabStartData::Touch(start_data) => start_data,
            _ => unreachable!(),
        }
    }

    fn unset(&mut self, data: &mut State) {
        release_palette_keyboard(&self.seat, data);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MenuAlignment {
    pub x: AxisAlignment,
    pub y: AxisAlignment,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AxisAlignment {
    Corner(u32),
    Centered,
    PreferCentered,
}

impl MenuAlignment {
    pub const CORNER: Self = MenuAlignment {
        x: AxisAlignment::Corner(0),
        y: AxisAlignment::Corner(0),
    };
    pub const PREFER_CENTERED: Self = MenuAlignment {
        x: AxisAlignment::PreferCentered,
        y: AxisAlignment::PreferCentered,
    };
    pub const CENTERED: Self = MenuAlignment {
        x: AxisAlignment::Centered,
        y: AxisAlignment::Centered,
    };
    pub const HORIZONTALLY_CENTERED: Self = MenuAlignment {
        x: AxisAlignment::Centered,
        y: AxisAlignment::Corner(0),
    };
    pub const VERTICALLY_CENTERED: Self = MenuAlignment {
        x: AxisAlignment::Corner(0),
        y: AxisAlignment::Centered,
    };

    pub fn horizontally_centered(offset: u32, fixed: bool) -> MenuAlignment {
        MenuAlignment {
            x: if fixed {
                AxisAlignment::Centered
            } else {
                AxisAlignment::PreferCentered
            },
            y: AxisAlignment::Corner(offset),
        }
    }

    pub fn vertically_centered(offset: u32, fixed: bool) -> MenuAlignment {
        MenuAlignment {
            x: AxisAlignment::Corner(offset),
            y: if fixed {
                AxisAlignment::Centered
            } else {
                AxisAlignment::PreferCentered
            },
        }
    }

    fn rectangles(
        &self,
        position: Point<i32, Global>,
        size: Size<i32, Global>,
    ) -> Vec<Rectangle<i32, Global>> {
        fn for_alignment(
            position: Point<i32, Global>,
            size: Size<i32, Global>,
            x: AxisAlignment,
            y: AxisAlignment,
        ) -> Vec<Rectangle<i32, Global>> {
            match (x, y) {
                (AxisAlignment::Corner(x_offset), AxisAlignment::Corner(y_offset)) => {
                    let offset = Point::from((x_offset as i32, y_offset as i32));
                    vec![
                        Rectangle::new(position + offset, size), // normal
                        Rectangle::new(
                            position - Point::from((size.w, 0))
                                + Point::from((-(x_offset as i32), y_offset as i32)),
                            size,
                        ), // flipped left
                        Rectangle::new(
                            position
                                - Point::from((0, size.h))
                                - Point::from((x_offset as i32, -(y_offset as i32))),
                            size,
                        ), // flipped up
                        Rectangle::new(position - size.to_point() - offset, size), // flipped left & up
                    ]
                }
                (AxisAlignment::Centered, AxisAlignment::Corner(offset)) => {
                    let x = position.x - ((size.w as f64 / 2.).round() as i32);
                    vec![
                        Rectangle::new(Point::from((x, position.y + offset as i32)), size), // below
                        Rectangle::new(Point::from((x, position.y - size.h - offset as i32)), size), // above
                    ]
                }
                (AxisAlignment::Corner(offset), AxisAlignment::Centered) => {
                    let y = position.y - ((size.h as f64 / 2.).round() as i32);
                    vec![
                        Rectangle::new(Point::from((position.x + offset as i32, y)), size), // left
                        Rectangle::new(Point::from((position.x - size.w - offset as i32, y)), size), // right
                    ]
                }
                (AxisAlignment::Centered, AxisAlignment::Centered) => {
                    vec![Rectangle::new(
                        position - size.to_f64().downscale(2.).to_i32_round().to_point(),
                        size,
                    )]
                }
                (AxisAlignment::PreferCentered, AxisAlignment::PreferCentered) => for_alignment(
                    position,
                    size,
                    AxisAlignment::Centered,
                    AxisAlignment::Centered,
                )
                .into_iter()
                .chain(for_alignment(
                    position,
                    size,
                    AxisAlignment::Centered,
                    AxisAlignment::Corner(0),
                ))
                .chain(for_alignment(
                    position,
                    size,
                    AxisAlignment::Corner(0),
                    AxisAlignment::Centered,
                ))
                .chain(for_alignment(
                    position,
                    size,
                    AxisAlignment::Corner(0),
                    AxisAlignment::Corner(0),
                ))
                .collect(),
                (AxisAlignment::PreferCentered, y) => {
                    for_alignment(position, size, AxisAlignment::Centered, y)
                        .into_iter()
                        .chain(for_alignment(position, size, AxisAlignment::Corner(0), y))
                        .collect()
                }
                (x, AxisAlignment::PreferCentered) => {
                    for_alignment(position, size, x, AxisAlignment::Centered)
                        .into_iter()
                        .chain(for_alignment(position, size, x, AxisAlignment::Corner(0)))
                        .collect()
                }
            }
        }

        for_alignment(position, size, self.x, self.y)
    }
}

impl MenuGrab {
    pub fn new(
        start_data: GrabStartData,
        seat: &Seat<State>,
        items: impl Iterator<Item = Item>,
        position: Point<i32, Global>,
        alignment: MenuAlignment,
        screen_space_relative: Option<f64>,
        handle: LoopHandle<'static, crate::state::State>,
        theme: CompTheme,
    ) -> MenuGrab {
        Self::new_styled(
            start_data,
            seat,
            items,
            position,
            alignment,
            screen_space_relative,
            handle,
            theme,
            false,
            None,
            None,
            None,
        )
    }

    pub fn new_halo(
        start_data: GrabStartData,
        seat: &Seat<State>,
        items: impl Iterator<Item = Item>,
        position: Point<i32, Global>,
        handle: LoopHandle<'static, State>,
        theme: CompTheme,
    ) -> MenuGrab {
        Self::new_styled(
            start_data,
            seat,
            items,
            position,
            MenuAlignment::CORNER,
            None,
            handle,
            theme,
            true,
            None,
            None,
            None,
        )
    }

    /// The window's commands, as the palette rather than a menu.
    ///
    /// `items` must be index-aligned with `palette.commands`, so pressing a row
    /// runs the same callback the menu would have.
    pub fn new_palette(
        start_data: GrabStartData,
        seat: &Seat<State>,
        items: impl Iterator<Item = Item>,
        place: impl FnOnce(Size<i32, Logical>) -> Point<i32, Global> + 'static,
        handle: LoopHandle<'static, State>,
        theme: CompTheme,
        palette: Palette,
    ) -> MenuGrab {
        Self::new_styled(
            start_data,
            seat,
            items,
            Point::default(),
            MenuAlignment::CORNER,
            None,
            handle,
            theme,
            true,
            Some(palette),
            None,
            Some(Box::new(place)),
        )
    }

    /// The Move to Desktop chooser. `items` are index-aligned with `dialog.rows`.
    pub fn new_dialog(
        start_data: GrabStartData,
        seat: &Seat<State>,
        items: impl Iterator<Item = Item>,
        place: impl FnOnce(Size<i32, Logical>) -> Point<i32, Global> + 'static,
        handle: LoopHandle<'static, State>,
        theme: CompTheme,
        dialog: MoveDialog,
    ) -> MenuGrab {
        Self::new_styled(
            start_data,
            seat,
            items,
            Point::default(),
            MenuAlignment::CORNER,
            None,
            handle,
            theme,
            true,
            None,
            Some(dialog),
            Some(Box::new(place)),
        )
    }

    #[allow(clippy::too_many_arguments)]
    fn new_styled(
        start_data: GrabStartData,
        seat: &Seat<State>,
        items: impl Iterator<Item = Item>,
        position: Point<i32, Global>,
        alignment: MenuAlignment,
        screen_space_relative: Option<f64>,
        handle: LoopHandle<'static, State>,
        theme: CompTheme,
        halo: bool,
        palette: Option<Palette>,
        dialog: Option<MoveDialog>,
        // The card's top-left for a measured card size: a palette is placed
        // once its height is known, so it can go above the pill when it would
        // not fit below.
        place: Option<Box<dyn FnOnce(Size<i32, Logical>) -> Point<i32, Global>>>,
    ) -> MenuGrab {
        let items = items.collect::<Vec<_>>();
        let mut menu = ContextMenu::new(items);
        menu.halo = halo;
        let is_palette = palette.is_some();
        menu.palette = palette;
        let is_dialog = dialog.is_some();
        menu.dialog = dialog;
        let padding = if !halo {
            iced_core::Padding::ZERO
        } else if is_dialog {
            move_dialog::padding(&theme)
        } else if is_palette {
            palette_padding(&theme)
        } else {
            halo_menu_padding(&theme)
        };
        let position = position - Point::from((padding.left as i32, padding.top as i32));
        let element = IcedElement::new(menu, Size::default(), handle, theme);
        if is_palette {
            // The search field takes the keyboard the moment the palette is up.
            element.queue_operation(iced_core::widget::operation::focusable::focus(
                search_field_id(),
            ));
        }
        // Two-pass sizing: first pass measures natural width, second pass measures
        // final height with that width locked (mode switches from Shrink to Fill).
        let natural_size = element.minimum_size();
        element.with_program(|p| {
            *p.row_width.lock().unwrap() = Some(natural_size.w as f32);
        });
        let min_size = element.minimum_size();
        element.resize(min_size);

        let output = seat.active_output();
        // TODO: This feels a lot like cheap xdg-positioner. Refactor and unify
        let position = if let Some(place) = place {
            let card = Size::from((
                min_size.w - (padding.left + padding.right) as i32,
                min_size.h - (padding.top + padding.bottom) as i32,
            ));
            place(card) - Point::from((padding.left as i32, padding.top as i32))
        } else {
            alignment
                .rectangles(
                    position,
                    min_size
                        .to_f64()
                        .upscale(screen_space_relative.unwrap_or(1.))
                        .to_i32_round()
                        .as_global(),
                )
                .iter()
                .rev() // preference of max_by_key is backwards
                .max_by_key(|rect| {
                    output
                        .geometry()
                        .intersection(**rect)
                        .map(|rect| rect.size.w * rect.size.h)
                })
                .unwrap()
                .loc
        };

        element.output_enter(&output, element.bbox());
        if let Some(scale) = screen_space_relative {
            element.set_additional_scale(scale);
        }

        let elements = Arc::new(Mutex::new(vec![Element {
            iced: element,
            position,
            pointer_entered: false,
            touch_entered: None,
        }]));

        let scale = Arc::new(Mutex::new(screen_space_relative.unwrap_or(1.)));
        let screen_space_relative = screen_space_relative.is_some().then_some(output);

        let grab_state = MenuGrabState {
            elements: elements.clone(),
            screen_space_relative: screen_space_relative.clone(),
            scale: scale.clone(),
        };

        *seat
            .user_data()
            .get::<SeatMenuGrabState>()
            .unwrap()
            .lock()
            .unwrap() = Some(grab_state);

        MenuGrab {
            elements,
            start_data,
            seat: seat.clone(),
            screen_space_relative,
            scale,
            on_close: None,
        }
    }

    pub fn set_additional_scale(&self, scale: f64) {
        *self.scale.lock().unwrap() = scale;
        for element in &*self.elements.lock().unwrap() {
            element.iced.set_additional_scale(scale);
        }
    }

    /// Set a callback to be invoked when the menu is dismissed.
    pub fn on_close(mut self, callback: impl FnOnce() + Send + 'static) -> Self {
        self.on_close = Some(Box::new(callback));
        self
    }

    pub fn is_touch_grab(&self) -> bool {
        match self.start_data {
            GrabStartData::Touch(_) => true,
            GrabStartData::Pointer(_) => false,
        }
    }
}

impl Drop for MenuGrab {
    fn drop(&mut self) {
        let mut active = self
            .seat
            .user_data()
            .get::<SeatMenuGrabState>()
            .unwrap()
            .lock()
            .unwrap();
        // A replacement grab can be installed before this old grab is dropped.
        // Never remove the replacement's render/input state.
        if active
            .as_ref()
            .is_some_and(|menu| Arc::ptr_eq(&menu.elements, &self.elements))
        {
            active.take();
        }
        drop(active);
        let closing = MenuGrabState {
            elements: self.elements.clone(),
            screen_space_relative: self.screen_space_relative.clone(),
            scale: self.scale.clone(),
        };
        closing.begin_close();
        if !closing.is_finished() {
            self.seat
                .user_data()
                .insert_if_missing_threadsafe(SeatClosingMenus::default);
            self.seat
                .user_data()
                .get::<SeatClosingMenus>()
                .unwrap()
                .lock()
                .unwrap()
                .push(closing);
        }
        if let Some(on_close) = self.on_close.take() {
            on_close();
        }
    }
}

/// The palette's search field, named so the grab can focus it on open.
fn search_field_id() -> iced_core::widget::Id {
    iced_core::widget::Id::new("halo-palette-search")
}

/// While the palette is up, its search field has the keyboard: every key goes
/// to the palette's own widget tree, Escape ends the grab the way a press
/// outside it would, and nothing reaches the window underneath.
pub struct PaletteKeyboardGrab {
    seat: Seat<State>,
    start_data: KeyboardGrabStartData<State>,
    on_escape: Mutex<Option<EscapeHook>>,
}

/// Run once Escape has closed the palette, to hand focus back to its opener.
pub type EscapeHook = Box<dyn FnOnce(&mut State) + Send>;

/// What forwarding a key to the palette asked of the grab.
enum Forwarded {
    /// The palette took it and stays up.
    Kept,
    /// The palette is done (a row ran, or chat was asked) or is already gone.
    Close,
}

impl PaletteKeyboardGrab {
    pub fn new(seat: Seat<State>) -> Self {
        let focus = seat
            .get_keyboard()
            .and_then(|keyboard| keyboard.current_focus());
        Self {
            seat,
            start_data: KeyboardGrabStartData { focus },
            on_escape: Mutex::new(None),
        }
    }

    pub fn on_escape(self, hook: Option<EscapeHook>) -> Self {
        *self.on_escape.lock().unwrap() = hook;
        self
    }

    /// What Escape owes the palette's opener, taken once.
    pub fn take_escape_hook(&self) -> Option<EscapeHook> {
        self.on_escape.lock().unwrap().take()
    }

    /// Hand a key to the palette's widget tree.
    fn forward(
        &self,
        data: &mut State,
        handle: &KeyboardInnerHandle<'_, State>,
        keycode: Keycode,
        state: KeyState,
        modifiers: Option<ModifiersState>,
        serial: Serial,
        time: u32,
    ) -> Forwarded {
        let Some(grab_state) = self.seat.user_data().get::<SeatMenuGrabState>() else {
            return Forwarded::Close;
        };
        let guard = grab_state.lock().unwrap();
        let Some(menu_state) = guard.as_ref() else {
            return Forwarded::Close;
        };
        let elements = menu_state.elements.lock().unwrap();
        let Some(element) = elements.first() else {
            return Forwarded::Close;
        };
        if let Some(modifiers) = modifiers {
            element.iced.modifiers(&self.seat, data, modifiers, serial);
        }
        element.iced.key(
            &self.seat,
            data,
            handle.keysym_handle(keycode),
            state,
            serial,
            time,
        );
        element.refit();
        // Enter ran a row, or asked chat: the palette is done.
        if element
            .iced
            .with_program(|menu| menu.selected.load(Ordering::SeqCst))
        {
            Forwarded::Close
        } else {
            Forwarded::Kept
        }
    }

    /// The first menu element, when it is a dialog.
    fn dialog(&self) -> Option<IcedElement<ContextMenu>> {
        let grab_state = self.seat.user_data().get::<SeatMenuGrabState>()?;
        let element = grab_state.lock().unwrap().as_ref().and_then(|menu| {
            menu.elements
                .lock()
                .unwrap()
                .first()
                .map(|e| e.iced.clone())
        })?;
        element
            .with_program(|menu| menu.dialog.is_some())
            .then_some(element)
    }

    /// Hand a dialog key to the dialog on its press; its release goes nowhere.
    fn send_to_dialog(&self, message: Message, state: KeyState) -> bool {
        let Some(element) = self.dialog() else {
            return false;
        };
        if state == KeyState::Pressed {
            icetron_themes::set_focus_visible(true);
            element.queue_message(message);
            element.force_update();
        }
        true
    }

    fn dialog_done(&self) -> bool {
        self.dialog().is_some_and(|element| {
            element.with_program(|menu| menu.selected.load(Ordering::SeqCst))
        })
    }

    /// End the palette. This grab goes now; the pointer grab that owns the
    /// palette is released once the keyboard is no longer mid-dispatch, since
    /// unsetting it reaches back into the keyboard to release this grab.
    fn close(
        &mut self,
        data: &mut State,
        handle: &mut KeyboardInnerHandle<'_, State>,
        serial: Serial,
        time: u32,
    ) {
        handle.unset_grab(self, data, serial, false);
        let pointer = self.seat.get_pointer();
        data.common.event_loop_handle.insert_idle(move |state| {
            if let Some(pointer) = pointer
                && pointer.is_grabbed()
            {
                pointer.unset_grab(state, serial, time);
            }
        });
    }
}

impl KeyboardGrab<State> for PaletteKeyboardGrab {
    fn input(
        &mut self,
        data: &mut State,
        handle: &mut KeyboardInnerHandle<'_, State>,
        keycode: Keycode,
        state: KeyState,
        modifiers: Option<ModifiersState>,
        serial: Serial,
        time: u32,
    ) {
        let sym = handle.keysym_handle(keycode).modified_sym();
        if sym == Keysym::Escape && state == KeyState::Pressed {
            self.close(data, handle, serial, time);
            if let Some(hook) = self.take_escape_hook() {
                data.common.event_loop_handle.insert_idle(hook);
            }
            return;
        }
        if let Some(message) = dialog_key(sym)
            && self.send_to_dialog(message, state)
        {
            if self.dialog_done() {
                self.close(data, handle, serial, time);
            }
            return;
        }
        if let Forwarded::Close =
            self.forward(data, handle, keycode, state, modifiers, serial, time)
        {
            self.close(data, handle, serial, time);
        }
    }

    fn set_focus(
        &mut self,
        _data: &mut State,
        _handle: &mut KeyboardInnerHandle<'_, State>,
        _focus: Option<<State as SeatHandler>::KeyboardFocus>,
        _serial: Serial,
    ) {
        // The palette keeps the keyboard until it closes.
    }

    fn start_data(&self) -> &KeyboardGrabStartData<State> {
        &self.start_data
    }

    fn unset(&mut self, _data: &mut State) {}
}

/// What the keyboard does in a Halo menu it opened.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MenuKey {
    First,
    Last,
    Step(bool),
    Press,
    Open,
    Back,
}

/// Hand `key` to the seat's open Halo menu. Returns whether one took it and, for a
/// press, whether the press chose a row, which closes the menu.
pub(crate) fn halo_menu_key(seat: &Seat<State>, key: MenuKey, state: &mut State) -> Option<bool> {
    let chosen = route_menu_key(seat, key)?;
    if chosen && let Some(pointer) = seat.get_pointer() {
        state.common.event_loop_handle.insert_idle(move |state| {
            if pointer.is_grabbed() {
                let time = state.common.clock.now().as_millis();
                pointer.unset_grab(state, smithay::utils::SERIAL_COUNTER.next_serial(), time);
            }
        });
    }
    Some(chosen)
}

fn halo_menu(
    seat: &Seat<State>,
) -> Option<(IcedElement<ContextMenu>, Option<IcedElement<ContextMenu>>)> {
    let (root, flyout) = seat
        .user_data()
        .get::<SeatMenuGrabState>()?
        .lock()
        .unwrap()
        .as_ref()
        .and_then(|menu| {
            let elements = menu.elements.lock().unwrap();
            let root = elements.first()?.iced.clone();
            Some((root, elements.get(1).map(|e| e.iced.clone())))
        })?;
    root.with_program(|menu| menu.halo && menu.palette.is_none() && menu.dialog.is_none())
        .then_some((root, flyout))
}

fn route_menu_key(seat: &Seat<State>, key: MenuKey) -> Option<bool> {
    let (root, flyout) = halo_menu(seat)?;
    if key == MenuKey::Back {
        close_halo_flyout(seat);
        return Some(false);
    }
    let (in_flyout, was_open) = root.with_program(|menu| {
        (
            menu.keyboard_flyout.load(Ordering::SeqCst),
            *menu.flyout.lock().unwrap(),
        )
    });
    let target = match flyout {
        Some(ref flyout) if in_flyout => flyout.clone(),
        _ => root.clone(),
    };
    target.queue_message(Message::MenuKey(key));
    target.force_update();
    if in_flyout {
        return Some(target.with_program(|menu| menu.selected.load(Ordering::SeqCst)));
    }
    let entered = root.with_program(|menu| {
        let idx = (*menu.flyout.lock().unwrap())
            .filter(|_| menu.keyboard_flyout.load(Ordering::SeqCst))?;
        match menu.items.get(idx) {
            Some(Item::Submenu { items, .. }) => Some((idx, items.clone())),
            _ => None,
        }
    });
    let Some((idx, items)) = entered else {
        return Some(root.with_program(|menu| menu.selected.load(Ordering::SeqCst)));
    };
    // A fly-out the pointer already opened stays; the keyboard only moves into it.
    if was_open == Some(idx) && flyout.is_some() {
        root.force_update();
    } else {
        show_flyout(seat, Some((idx, items)));
    }
    match halo_menu(seat) {
        Some((_, Some(flyout))) => {
            flyout.queue_message(Message::MenuKey(MenuKey::First));
            flyout.force_update();
        }
        _ => {
            close_halo_flyout(seat);
        }
    }
    Some(false)
}

/// Close the fly-out keyboard focus is in, if there is one, back to its row.
pub(crate) fn close_halo_flyout(seat: &Seat<State>) -> bool {
    let Some((root, _)) = halo_menu(seat) else {
        return false;
    };
    let held = root.with_program(|menu| {
        let held = menu.keyboard_flyout.swap(false, Ordering::SeqCst);
        if held {
            menu.flyout.lock().unwrap().take();
        }
        held
    });
    if held {
        show_flyout(seat, None);
    }
    held
}

/// A dialog's keys: Tab and Down walk forward, Shift+Tab and Up back, Enter and Space press.
fn dialog_key(sym: Keysym) -> Option<Message> {
    match sym {
        Keysym::Tab | Keysym::Down => Some(Message::DialogStep(true)),
        Keysym::ISO_Left_Tab | Keysym::Up => Some(Message::DialogStep(false)),
        Keysym::Return | Keysym::KP_Enter | Keysym::space => Some(Message::DialogActivate),
        _ => None,
    }
}

/// Close an open palette from outside it (Super+K pressed again).
pub(crate) fn close_palette(seat: &Seat<State>, state: &mut State) {
    let palette = seat
        .get_keyboard()
        .and_then(|keyboard| keyboard.with_grab(|_, grab| grab.is::<PaletteKeyboardGrab>()));
    if palette != Some(true) {
        return;
    }
    release_palette_keyboard(seat, state);
    if let Some(pointer) = seat.get_pointer()
        && pointer.is_grabbed()
    {
        let time = state.common.clock.now().as_millis();
        pointer.unset_grab(state, smithay::utils::SERIAL_COUNTER.next_serial(), time);
    }
}

/// The palette's keyboard grab lives and dies with its pointer grab.
fn release_palette_keyboard(seat: &Seat<State>, data: &mut State) {
    if let Some(keyboard) = seat.get_keyboard()
        && keyboard.with_grab(|_, grab| grab.is::<PaletteKeyboardGrab>()) == Some(true)
    {
        keyboard.unset_grab(data);
    }
}
