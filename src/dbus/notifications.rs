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

impl DBusState {
    /// Show a system toast, fire and forget. A missing daemon is only logged.
    pub fn system_toast(&self, message: String, tone: Tone) {
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
    fn the_shells_toasts_read_as_the_prototype_writes_them() {
        assert_eq!(
            fl!("halo-parked"),
            "Parked — click its app in the bar to restore"
        );
        assert_eq!(fl!("screenshot-saved"), "Screenshot saved");
        assert_eq!(fl!("recording-started"), "Recording this window");
        assert_eq!(
            fl!("recording-stopped"),
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
