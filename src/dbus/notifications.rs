// SPDX-License-Identifier: GPL-3.0-only

//! Toasts through the freedesktop `org.freedesktop.Notifications` interface,
//! sent over the compositor's session-bus connection.

use std::collections::HashMap;

use tracing::warn;
use zbus::zvariant::Value;

use super::DBusState;

#[zbus::proxy(
    interface = "org.freedesktop.Notifications",
    default_service = "org.freedesktop.Notifications",
    default_path = "/org/freedesktop/Notifications"
)]
trait Notifications {
    #[allow(clippy::too_many_arguments)]
    fn notify(
        &self,
        app_name: &str,
        replaces_id: u32,
        app_icon: &str,
        summary: &str,
        body: &str,
        actions: &[&str],
        hints: HashMap<&str, &Value<'_>>,
        expire_timeout: i32,
    ) -> zbus::Result<u32>;
}

/// The shell's system toasts: a one-line confirmation of something the user
/// just did, never a notification (no history, no unread, no sound).
#[zbus::proxy(
    interface = "one.playtron.AgentOS.Notifications1",
    default_service = "one.playtron.AgentOS.Notifications1",
    default_path = "/one/playtron/AgentOS/Notifications1"
)]
trait SystemToasts {
    fn toast(&self, message: &str, tone: &str) -> zbus::Result<()>;
}

/// The colour grammar a system toast's dot carries.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Tone {
    Neutral,
    /// Work the machine is doing for you.
    Ai,
    NeedsYou,
    Destructive,
}

impl Tone {
    pub fn as_str(self) -> &'static str {
        match self {
            Tone::Neutral => "neutral",
            Tone::Ai => "ai",
            Tone::NeedsYou => "needs-you",
            Tone::Destructive => "destructive",
        }
    }
}

/// One toast. `app_icon` is a freedesktop icon name.
#[derive(Debug, Clone)]
pub struct Notification {
    pub app_name: String,
    pub app_icon: String,
    pub summary: String,
    pub body: String,
    /// Milliseconds before the daemon dismisses the toast on its own.
    pub expire_timeout: i32,
    /// A transient toast is shown once and kept out of the history.
    pub transient: bool,
}

impl DBusState {
    /// Send a toast, fire and forget. A missing daemon is only logged.
    pub fn notify(&self, notification: Notification) {
        let state = self.clone();
        self.spawn(async move {
            if let Err(err) = send(&state, &notification).await {
                warn!(?err, summary = %notification.summary, "Failed to send notification");
            }
        });
    }
}

/// Whether the shell confirms with system toasts rather than notifications.
/// `COSMIC_SYSTEM_TOASTS` decides; unset, toasts follow `COSMIC_WORKSPACES`.
pub fn toasts_enabled() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED.get_or_init(|| {
        crate::utils::env::bool_var("COSMIC_SYSTEM_TOASTS")
            .unwrap_or_else(super::workspaces::enabled)
    })
}

impl DBusState {
    /// Confirm something the user just did, as a toast or a plain notification.
    pub fn system_toast(&self, message: String, tone: Tone) {
        let notification = Notification {
            app_name: system_name(),
            app_icon: String::new(),
            summary: plain(message.clone()),
            body: String::new(),
            expire_timeout: 5000,
            transient: true,
        };
        self.system_toast_or(message, tone, notification);
    }

    /// A system toast when [`toasts_enabled`], otherwise `notification`.
    pub fn system_toast_or(&self, message: String, tone: Tone, notification: Notification) {
        if !toasts_enabled() {
            self.notify(notification);
            return;
        }
        let message = plain(message);
        let state = self.clone();
        self.spawn(async move {
            let sent = async {
                let conn = state.session_conn().await?;
                SystemToastsProxy::new(conn)
                    .await?
                    .toast(&message, tone.as_str())
                    .await
            };
            if let Err(err) = sent.await {
                warn!(?err, %message, "Failed to show a system toast");
            }
        });
    }
}

/// The system's own name, from os-release, so each distribution names itself.
fn system_name() -> String {
    static NAME: std::sync::OnceLock<String> = std::sync::OnceLock::new();
    NAME.get_or_init(|| {
        ["/etc/os-release", "/usr/lib/os-release"]
            .iter()
            .find_map(|path| std::fs::read_to_string(path).ok())
            .and_then(|text| os_release_name(&text))
            .unwrap_or_else(|| crate::fl!("shell-notification-app"))
    })
    .clone()
}

fn os_release_name(text: &str) -> Option<String> {
    text.lines()
        .find_map(|line| line.strip_prefix("NAME="))
        .map(|value| value.trim().trim_matches('"').to_owned())
        .filter(|value| !value.is_empty())
}

/// Fluent's bidi isolate marks around a placeable surface as tofu in the
/// toast's Latin-only fonts.
pub fn plain(text: String) -> String {
    text.replace(['\u{2068}', '\u{2069}'], "")
}

async fn send(state: &DBusState, notification: &Notification) -> zbus::Result<()> {
    let conn = state.session_conn().await?;
    let proxy = NotificationsProxy::new(conn).await?;
    let transient = Value::Bool(notification.transient);
    proxy
        .notify(
            &notification.app_name,
            0,
            &notification.app_icon,
            &notification.summary,
            &notification.body,
            &[],
            HashMap::from([("transient", &transient)]),
            notification.expire_timeout,
        )
        .await?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fl;

    #[test]
    fn tones_go_by_the_names_the_daemon_reads() {
        let names = [Tone::Neutral, Tone::Ai, Tone::NeedsYou, Tone::Destructive].map(Tone::as_str);
        assert_eq!(names, ["neutral", "ai", "needs-you", "destructive"]);
    }

    #[test]
    fn the_system_names_itself_from_os_release() {
        let text = "PRETTY_NAME=\"Example OS 1\"\nNAME=\"Example OS\"\nID=example\n";
        assert_eq!(os_release_name(text).as_deref(), Some("Example OS"));
        assert_eq!(os_release_name("NAME=Plain\n").as_deref(), Some("Plain"));
        assert_eq!(os_release_name("NAME=\"\"\n"), None);
        assert_eq!(os_release_name("ID=example\n"), None);
    }

    #[test]
    fn the_shells_toasts_read_as_the_prototype_writes_them() {
        assert_eq!(fl!("screenshot-saved"), "Screenshot saved");
        assert_eq!(fl!("recording-started"), "Recording this window");
        assert_eq!(
            fl!("recording-stopped-toast"),
            "Recording stopped — saved with provenance"
        );
        assert_eq!(fl!("halo-merged"), "Merged — this window is a tab now");
        assert_eq!(
            plain(fl!("halo-merged-tabs", tabs = 3)),
            "Merged 3 tabs into the other window"
        );
        assert_eq!(
            plain(fl!("halo-one-window", app = "Files")),
            "Files opens one window"
        );
        assert_eq!(
            plain(fl!("halo-info-toast", app = "Files")),
            "Files — standard command set, provenance on every capture"
        );
    }
}
