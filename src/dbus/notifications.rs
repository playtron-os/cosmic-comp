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
        let state = self.clone();
        self.spawn(async move {
            let tone = match tone {
                Tone::Neutral => "neutral",
                Tone::Ai => "ai",
            };
            let sent = async {
                let conn = state.session_conn().await?;
                SystemToastsProxy::new(conn)
                    .await?
                    .toast(&message, tone)
                    .await
            };
            if let Err(err) = sent.await {
                warn!(?err, %message, "Failed to show a system toast");
            }
        });
    }
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
