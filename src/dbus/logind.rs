use std::{os::fd::OwnedFd, time::Duration};

use anyhow::{Context, Result, anyhow};
use calloop::timer::TimeoutFuture;
use futures_util::future::{Either, select};
use logind_zbus::manager::{InhibitType::HandleLidSwitch, ManagerProxy};

use crate::state::{Common, State};

const REQUEST_TIMEOUT: Duration = Duration::from_secs(3);

#[derive(Debug)]
pub(crate) struct LidInhibitor {
    pub fd: OwnedFd,
    pub lid_closed: bool,
}

/// Acquire the lid-switch inhibitor without blocking the compositor's event loop.
///
/// The shared zbus connection is driven by `DBusState`'s calloop executor. Blocking
/// this thread while awaiting a reply therefore deadlocks the connection itself.
pub(crate) fn request_lid_inhibitor(common: &Common, request_id: u64) -> bool {
    let dbus_state = common.dbus_state.clone();
    let task_dbus_state = dbus_state.clone();
    let event_loop_handle = common.event_loop_handle.clone();
    let timeout = TimeoutFuture::from_duration(&event_loop_handle, REQUEST_TIMEOUT);

    dbus_state.try_spawn(async move {
        let request = Box::pin(acquire_lid_inhibitor(&task_dbus_state));
        let result = match select(request, Box::pin(timeout)).await {
            Either::Left((result, _)) => result,
            Either::Right(((), _)) => Err(anyhow!(
                "Timed out after {}s while talking to logind",
                REQUEST_TIMEOUT.as_secs()
            )),
        };

        event_loop_handle.insert_idle(move |state: &mut State| {
            state.complete_lid_inhibitor_request(request_id, result);
        });
    })
}

async fn acquire_lid_inhibitor(dbus_state: &super::DBusState) -> Result<LidInhibitor> {
    let conn = dbus_state.system_conn().await?;
    let manager = ManagerProxy::new(conn).await?;
    let fd = manager
        .inhibit(
            HandleLidSwitch,
            "cosmic-comp",
            "External output connected",
            "block",
        )
        .await
        .context("Failed to inhibit the logind lid switch")?;

    // Keep the inhibitor when only the initial property read fails. This matches
    // the previous fail-open behavior and lets subsequent libinput lid events
    // update the connector state.
    let lid_closed = match manager
        .lid_closed()
        .await
        .context("Failed to talk to logind")
    {
        Ok(closed) => closed,
        Err(err) => {
            tracing::warn!(?err, "Unable to read the initial lid state; assuming open");
            false
        }
    };

    Ok(LidInhibitor {
        fd: fd.into(),
        lid_closed,
    })
}
