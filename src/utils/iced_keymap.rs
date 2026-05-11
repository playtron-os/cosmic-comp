//! Convert smithay `Keysym` / `Keycode` to iced keyboard types.
//!
//! Based on the xkb keysym → iced key mapping from `waycrate_xkbkeycode`.

use iced_core::keyboard::{self, key};
use smithay::input::keyboard::{Keycode, Keysym};

/// Convert an xkb `Keysym` to an iced [`keyboard::Key`].
///
/// Named keys (modifiers, function keys, arrows, etc.) map to [`Key::Named`].
/// Character-producing keysyms map to [`Key::Character`].
/// Unknown keysyms become [`Key::Unidentified`].
pub fn keysym_to_iced_key(keysym: Keysym) -> keyboard::Key {
    if let Some(named) = keysym_to_named(keysym) {
        return keyboard::Key::Named(named);
    }

    // Character-producing keysyms
    if let Some(ch) = keysym.key_char()
        && !ch.is_control()
    {
        let mut buf = [0u8; 4];
        let s = ch.encode_utf8(&mut buf);
        return keyboard::Key::Character(s.into());
    }

    keyboard::Key::Unidentified
}

/// Convert an xkb `Keysym` to an iced `key::Named`, if applicable.
#[allow(clippy::too_many_lines)]
fn keysym_to_named(keysym: Keysym) -> Option<key::Named> {
    use key::Named;
    Some(match keysym {
        // TTY function keys
        Keysym::BackSpace => Named::Backspace,
        Keysym::Tab => Named::Tab,
        Keysym::Clear => Named::Clear,
        Keysym::Return => Named::Enter,
        Keysym::Pause => Named::Pause,
        Keysym::Scroll_Lock => Named::ScrollLock,
        Keysym::Sys_Req => Named::PrintScreen,
        Keysym::Escape => Named::Escape,
        Keysym::Delete => Named::Delete,

        // IME keys
        Keysym::Multi_key => Named::Compose,
        Keysym::Codeinput => Named::CodeInput,
        Keysym::SingleCandidate => Named::SingleCandidate,
        Keysym::MultipleCandidate => Named::AllCandidates,
        Keysym::PreviousCandidate => Named::PreviousCandidate,

        // Japanese keys
        Keysym::Kanji => Named::KanjiMode,
        Keysym::Muhenkan => Named::NonConvert,
        Keysym::Henkan_Mode => Named::Convert,
        Keysym::Romaji => Named::Romaji,
        Keysym::Hiragana => Named::Hiragana,
        Keysym::Hiragana_Katakana => Named::HiraganaKatakana,
        Keysym::Zenkaku => Named::Zenkaku,
        Keysym::Hankaku => Named::Hankaku,
        Keysym::Zenkaku_Hankaku => Named::ZenkakuHankaku,
        Keysym::Kana_Lock | Keysym::Kana_Shift => Named::KanaMode,
        Keysym::Eisu_Shift | Keysym::Eisu_toggle => Named::Alphanumeric,

        // Cursor control
        Keysym::Home => Named::Home,
        Keysym::Left => Named::ArrowLeft,
        Keysym::Up => Named::ArrowUp,
        Keysym::Right => Named::ArrowRight,
        Keysym::Down => Named::ArrowDown,
        Keysym::Page_Up => Named::PageUp,
        Keysym::Page_Down => Named::PageDown,
        Keysym::End => Named::End,

        // Misc functions
        Keysym::Select => Named::Select,
        Keysym::Print => Named::PrintScreen,
        Keysym::Execute => Named::Execute,
        Keysym::Insert => Named::Insert,
        Keysym::Undo => Named::Undo,
        Keysym::Redo => Named::Redo,
        Keysym::Menu => Named::ContextMenu,
        Keysym::Find => Named::Find,
        Keysym::Cancel => Named::Cancel,
        Keysym::Help => Named::Help,
        Keysym::Break => Named::Pause,
        Keysym::Mode_switch => Named::ModeChange,
        Keysym::Num_Lock => Named::NumLock,

        // Keypad
        Keysym::KP_Tab => Named::Tab,
        Keysym::KP_Enter => Named::Enter,
        Keysym::KP_F1 => Named::F1,
        Keysym::KP_F2 => Named::F2,
        Keysym::KP_F3 => Named::F3,
        Keysym::KP_F4 => Named::F4,
        Keysym::KP_Home => Named::Home,
        Keysym::KP_Left => Named::ArrowLeft,
        Keysym::KP_Up => Named::ArrowUp,
        Keysym::KP_Right => Named::ArrowRight,
        Keysym::KP_Down => Named::ArrowDown,
        Keysym::KP_Page_Up => Named::PageUp,
        Keysym::KP_Page_Down => Named::PageDown,
        Keysym::KP_End => Named::End,
        Keysym::KP_Insert => Named::Insert,
        Keysym::KP_Delete => Named::Delete,

        // Function keys
        Keysym::F1 => Named::F1,
        Keysym::F2 => Named::F2,
        Keysym::F3 => Named::F3,
        Keysym::F4 => Named::F4,
        Keysym::F5 => Named::F5,
        Keysym::F6 => Named::F6,
        Keysym::F7 => Named::F7,
        Keysym::F8 => Named::F8,
        Keysym::F9 => Named::F9,
        Keysym::F10 => Named::F10,
        Keysym::F11 => Named::F11,
        Keysym::F12 => Named::F12,
        Keysym::F13 => Named::F13,
        Keysym::F14 => Named::F14,
        Keysym::F15 => Named::F15,
        Keysym::F16 => Named::F16,
        Keysym::F17 => Named::F17,
        Keysym::F18 => Named::F18,
        Keysym::F19 => Named::F19,
        Keysym::F20 => Named::F20,
        Keysym::F21 => Named::F21,
        Keysym::F22 => Named::F22,
        Keysym::F23 => Named::F23,
        Keysym::F24 => Named::F24,
        Keysym::F25 => Named::F25,
        Keysym::F26 => Named::F26,
        Keysym::F27 => Named::F27,
        Keysym::F28 => Named::F28,
        Keysym::F29 => Named::F29,
        Keysym::F30 => Named::F30,
        Keysym::F31 => Named::F31,
        Keysym::F32 => Named::F32,
        Keysym::F33 => Named::F33,
        Keysym::F34 => Named::F34,
        Keysym::F35 => Named::F35,

        // Modifiers
        Keysym::Shift_L | Keysym::Shift_R => Named::Shift,
        Keysym::Control_L | Keysym::Control_R => Named::Control,
        Keysym::Caps_Lock => Named::CapsLock,
        Keysym::Alt_L | Keysym::Alt_R => Named::Alt,
        Keysym::Super_L | Keysym::Super_R => Named::Super,
        Keysym::Hyper_L | Keysym::Hyper_R => Named::Hyper,

        // ISO keys
        Keysym::ISO_Level3_Shift | Keysym::ISO_Level3_Latch | Keysym::ISO_Level3_Lock => {
            Named::AltGraph
        }
        Keysym::ISO_Next_Group => Named::GroupNext,
        Keysym::ISO_Prev_Group => Named::GroupPrevious,
        Keysym::ISO_First_Group => Named::GroupFirst,
        Keysym::ISO_Last_Group => Named::GroupLast,
        Keysym::ISO_Left_Tab => Named::Tab,
        Keysym::ISO_Enter => Named::Enter,

        // XF86 media/browser keys
        Keysym::XF86_MonBrightnessUp => Named::BrightnessUp,
        Keysym::XF86_MonBrightnessDown => Named::BrightnessDown,
        Keysym::XF86_Standby => Named::Standby,
        Keysym::XF86_AudioLowerVolume => Named::AudioVolumeDown,
        Keysym::XF86_AudioRaiseVolume => Named::AudioVolumeUp,
        Keysym::XF86_AudioPlay => Named::MediaPlay,
        Keysym::XF86_AudioStop => Named::MediaStop,
        Keysym::XF86_AudioPrev => Named::MediaTrackPrevious,
        Keysym::XF86_AudioNext => Named::MediaTrackNext,
        Keysym::XF86_HomePage => Named::BrowserHome,
        Keysym::XF86_Mail => Named::LaunchMail,
        Keysym::XF86_Search => Named::BrowserSearch,
        Keysym::XF86_AudioRecord => Named::MediaRecord,
        Keysym::XF86_Calculator | Keysym::XF86_Calculater => Named::LaunchApplication2,
        Keysym::XF86_Calendar => Named::LaunchCalendar,
        Keysym::XF86_PowerDown | Keysym::XF86_PowerOff => Named::Power,
        Keysym::XF86_Back => Named::BrowserBack,
        Keysym::XF86_Forward => Named::BrowserForward,
        Keysym::XF86_Refresh | Keysym::XF86_Reload => Named::BrowserRefresh,
        Keysym::XF86_WakeUp => Named::WakeUp,
        Keysym::XF86_Eject => Named::Eject,
        Keysym::XF86_ScreenSaver => Named::LaunchScreenSaver,
        Keysym::XF86_WWW => Named::LaunchWebBrowser,
        Keysym::XF86_Sleep | Keysym::XF86_Suspend => Named::Standby,
        Keysym::XF86_Favorites | Keysym::XF86_MySites => Named::BrowserFavorites,
        Keysym::XF86_AudioPause => Named::MediaPause,
        Keysym::XF86_MyComputer => Named::LaunchApplication1,
        Keysym::XF86_AudioRewind => Named::MediaRewind,
        Keysym::XF86_Close => Named::Close,
        Keysym::XF86_Copy | Keysym::SUN_Copy => Named::Copy,
        Keysym::XF86_Cut | Keysym::SUN_Cut => Named::Cut,
        Keysym::XF86_Excel => Named::LaunchSpreadsheet,
        Keysym::XF86_LogOff => Named::LogOff,
        Keysym::XF86_New => Named::New,
        Keysym::XF86_Open | Keysym::SUN_Open => Named::Open,
        Keysym::XF86_Paste | Keysym::SUN_Paste => Named::Paste,
        Keysym::XF86_Phone => Named::LaunchPhone,
        Keysym::XF86_Reply => Named::MailReply,
        Keysym::XF86_Save => Named::Save,
        Keysym::XF86_Send => Named::MailSend,
        Keysym::XF86_Spell => Named::SpellCheck,
        Keysym::XF86_SplitScreen => Named::SplitScreenToggle,
        Keysym::XF86_Video => Named::LaunchMediaPlayer,
        Keysym::XF86_Word => Named::LaunchWordProcessor,
        Keysym::XF86_ZoomIn => Named::ZoomIn,
        Keysym::XF86_ZoomOut => Named::ZoomOut,
        Keysym::XF86_WebCam => Named::LaunchWebCam,
        Keysym::XF86_MailForward => Named::MailForward,
        Keysym::XF86_Music => Named::LaunchMusicPlayer,
        Keysym::XF86_AudioForward => Named::MediaFastForward,
        Keysym::XF86_AudioRandomPlay => Named::RandomToggle,
        Keysym::XF86_Subtitle => Named::Subtitle,
        Keysym::XF86_AudioCycleTrack => Named::MediaAudioTrack,
        Keysym::XF86_Hibernate => Named::Hibernate,
        Keysym::XF86_AudioMute => Named::AudioVolumeMute,
        Keysym::XF86_Next_VMode => Named::VideoModeNext,
        Keysym::SUN_AudioLowerVolume => Named::AudioVolumeDown,
        Keysym::SUN_AudioMute => Named::AudioVolumeMute,
        Keysym::SUN_AudioRaiseVolume => Named::AudioVolumeUp,
        Keysym::SUN_VideoLowerBrightness => Named::BrightnessDown,
        Keysym::SUN_VideoRaiseBrightness => Named::BrightnessUp,

        Keysym::space => Named::Space,

        _ => return None,
    })
}

/// Determine the key location from a keysym.
pub fn keysym_to_location(keysym: Keysym) -> keyboard::Location {
    match keysym {
        Keysym::Shift_L
        | Keysym::Control_L
        | Keysym::Meta_L
        | Keysym::Alt_L
        | Keysym::Super_L
        | Keysym::Hyper_L => keyboard::Location::Left,

        Keysym::Shift_R
        | Keysym::Control_R
        | Keysym::Meta_R
        | Keysym::Alt_R
        | Keysym::Super_R
        | Keysym::Hyper_R => keyboard::Location::Right,

        Keysym::KP_0
        | Keysym::KP_1
        | Keysym::KP_2
        | Keysym::KP_3
        | Keysym::KP_4
        | Keysym::KP_5
        | Keysym::KP_6
        | Keysym::KP_7
        | Keysym::KP_8
        | Keysym::KP_9
        | Keysym::KP_Space
        | Keysym::KP_Tab
        | Keysym::KP_Enter
        | Keysym::KP_F1
        | Keysym::KP_F2
        | Keysym::KP_F3
        | Keysym::KP_F4
        | Keysym::KP_Home
        | Keysym::KP_Left
        | Keysym::KP_Up
        | Keysym::KP_Right
        | Keysym::KP_Down
        | Keysym::KP_Page_Up
        | Keysym::KP_Page_Down
        | Keysym::KP_End
        | Keysym::KP_Begin
        | Keysym::KP_Insert
        | Keysym::KP_Delete
        | Keysym::KP_Equal
        | Keysym::KP_Multiply
        | Keysym::KP_Add
        | Keysym::KP_Separator
        | Keysym::KP_Subtract
        | Keysym::KP_Decimal
        | Keysym::KP_Divide => keyboard::Location::Numpad,

        _ => keyboard::Location::Standard,
    }
}

/// Convert an X11-style keycode (evdev scancode + 8) to an iced [`key::Physical`].
pub fn keycode_to_physical(keycode: Keycode) -> key::Physical {
    // X11/evdev keycodes are offset by 8 from Linux scancodes
    let scancode = keycode.raw().saturating_sub(8);
    key::Physical::Code(match scancode {
        1 => key::Code::Escape,
        2 => key::Code::Digit1,
        3 => key::Code::Digit2,
        4 => key::Code::Digit3,
        5 => key::Code::Digit4,
        6 => key::Code::Digit5,
        7 => key::Code::Digit6,
        8 => key::Code::Digit7,
        9 => key::Code::Digit8,
        10 => key::Code::Digit9,
        11 => key::Code::Digit0,
        12 => key::Code::Minus,
        13 => key::Code::Equal,
        14 => key::Code::Backspace,
        15 => key::Code::Tab,
        16 => key::Code::KeyQ,
        17 => key::Code::KeyW,
        18 => key::Code::KeyE,
        19 => key::Code::KeyR,
        20 => key::Code::KeyT,
        21 => key::Code::KeyY,
        22 => key::Code::KeyU,
        23 => key::Code::KeyI,
        24 => key::Code::KeyO,
        25 => key::Code::KeyP,
        26 => key::Code::BracketLeft,
        27 => key::Code::BracketRight,
        28 => key::Code::Enter,
        29 => key::Code::ControlLeft,
        30 => key::Code::KeyA,
        31 => key::Code::KeyS,
        32 => key::Code::KeyD,
        33 => key::Code::KeyF,
        34 => key::Code::KeyG,
        35 => key::Code::KeyH,
        36 => key::Code::KeyJ,
        37 => key::Code::KeyK,
        38 => key::Code::KeyL,
        39 => key::Code::Semicolon,
        40 => key::Code::Quote,
        41 => key::Code::Backquote,
        42 => key::Code::ShiftLeft,
        43 => key::Code::Backslash,
        44 => key::Code::KeyZ,
        45 => key::Code::KeyX,
        46 => key::Code::KeyC,
        47 => key::Code::KeyV,
        48 => key::Code::KeyB,
        49 => key::Code::KeyN,
        50 => key::Code::KeyM,
        51 => key::Code::Comma,
        52 => key::Code::Period,
        53 => key::Code::Slash,
        54 => key::Code::ShiftRight,
        55 => key::Code::NumpadMultiply,
        56 => key::Code::AltLeft,
        57 => key::Code::Space,
        58 => key::Code::CapsLock,
        59 => key::Code::F1,
        60 => key::Code::F2,
        61 => key::Code::F3,
        62 => key::Code::F4,
        63 => key::Code::F5,
        64 => key::Code::F6,
        65 => key::Code::F7,
        66 => key::Code::F8,
        67 => key::Code::F9,
        68 => key::Code::F10,
        69 => key::Code::NumLock,
        70 => key::Code::ScrollLock,
        71 => key::Code::Numpad7,
        72 => key::Code::Numpad8,
        73 => key::Code::Numpad9,
        74 => key::Code::NumpadSubtract,
        75 => key::Code::Numpad4,
        76 => key::Code::Numpad5,
        77 => key::Code::Numpad6,
        78 => key::Code::NumpadAdd,
        79 => key::Code::Numpad1,
        80 => key::Code::Numpad2,
        81 => key::Code::Numpad3,
        82 => key::Code::Numpad0,
        83 => key::Code::NumpadDecimal,
        87 => key::Code::F11,
        88 => key::Code::F12,
        96 => key::Code::NumpadEnter,
        97 => key::Code::ControlRight,
        98 => key::Code::NumpadDivide,
        99 => key::Code::PrintScreen,
        100 => key::Code::AltRight,
        102 => key::Code::Home,
        103 => key::Code::ArrowUp,
        104 => key::Code::PageUp,
        105 => key::Code::ArrowLeft,
        106 => key::Code::ArrowRight,
        107 => key::Code::End,
        108 => key::Code::ArrowDown,
        109 => key::Code::PageDown,
        110 => key::Code::Insert,
        111 => key::Code::Delete,
        119 => key::Code::Pause,
        125 => key::Code::SuperLeft,
        126 => key::Code::SuperRight,
        127 => key::Code::ContextMenu,
        183 => key::Code::F13,
        184 => key::Code::F14,
        185 => key::Code::F15,
        186 => key::Code::F16,
        187 => key::Code::F17,
        188 => key::Code::F18,
        189 => key::Code::F19,
        190 => key::Code::F20,
        191 => key::Code::F21,
        192 => key::Code::F22,
        193 => key::Code::F23,
        194 => key::Code::F24,
        _ => return key::Physical::Unidentified(key::NativeCode::Xkb(keycode.raw())),
    })
}
