// SPDX-License-Identifier: GPL-3.0-only

//! Compositor-side clipboard persistence.
//!
//! On Wayland the clipboard is served *live* by the client that owns the
//! selection: pasting asks that client to write the bytes to a pipe. When the
//! owning client exits, smithay clears the selection and the copied data is
//! gone. That is why e.g. a screenshot copied via `xdg-desktop-portal-cosmic`
//! becomes un-pasteable the moment the (frequently crashing) portal process
//! dies — there is no clipboard manager holding the bytes.
//!
//! When the `clipboard_persistence` config option is enabled we snapshot every
//! new *regular clipboard* selection into compositor memory and re-offer it as
//! a compositor-owned selection (`SelectionUserData::Persisted`). From then on
//! the clipboard is served from our cache, so the copy survives the source
//! client dying. The primary (middle-click) selection is intentionally left
//! untouched.
//!
//! Flow:
//! 1. A client sets the clipboard → [`SelectionHandler::new_selection`] calls
//!    [`on_new_clipboard`], which defers a snapshot to the end of the loop
//!    iteration (so the new source is installed as the current selection first).
//! 2. [`start_snapshot`] asks the current selection to write every offered mime
//!    type into a pipe, and reads each pipe asynchronously via calloop.
//! 3. Once every read completes, [`promote`] replaces the selection with a
//!    compositor-owned one backed by the cached bytes.
//! 4. Pasting a promoted selection routes back through
//!    [`SelectionHandler::send_selection`] / the Xwayland bridge, which call
//!    [`serve`] to write the cached bytes to the requester.

use std::{
    collections::HashMap,
    os::unix::io::{AsFd, BorrowedFd, OwnedFd},
    sync::Arc,
    time::Duration,
};

use smithay::{
    input::Seat,
    reexports::{
        calloop::{
            Interest, LoopHandle, Mode, PostAction, RegistrationToken,
            generic::Generic,
            timer::{TimeoutAction, Timer},
        },
        rustix,
    },
    wayland::selection::data_device::{
        current_data_device_selection_userdata, request_data_device_client_selection,
        set_data_device_selection,
    },
};
use tracing::debug;

use crate::{state::State, wayland::handlers::selection::SelectionUserData};

/// Give up snapshotting a selection after this long, so a client that never
/// finishes writing cannot pin the read sources / buffers indefinitely.
const SNAPSHOT_TIMEOUT: Duration = Duration::from_secs(5);
/// Refuse to persist selections whose contents exceed this in total, to bound
/// the memory the compositor holds on behalf of the clipboard.
const MAX_TOTAL_BYTES: usize = 16 * 1024 * 1024;
/// Per-read chunk size.
const READ_CHUNK: usize = 64 * 1024;

/// Per-compositor clipboard-persistence state, stored on [`crate::state::Common`].
#[derive(Debug, Default)]
pub struct ClipboardState {
    /// Bumped on every clipboard change; lets in-flight snapshots detect that
    /// they have been superseded and bail out.
    generation: u64,
    /// The snapshot currently being gathered, if any.
    active: Option<Snapshot>,
    /// The promoted selection currently being served from memory.
    cache: HashMap<String, Arc<[u8]>>,
}

#[derive(Debug)]
struct Snapshot {
    generation: u64,
    seat: Seat<State>,
    /// Timeout guard; self-drops when it fires (see [`snapshot_timed_out`]).
    timeout: RegistrationToken,
    /// One entry per mime type still being read.
    pending: HashMap<usize, PendingRead>,
    /// Fully-read mime types.
    collected: HashMap<String, Vec<u8>>,
    total_bytes: usize,
}

impl Snapshot {
    /// Tear down without promoting: remove the timeout and every pending read.
    fn cancel(self, handle: &LoopHandle<'static, State>) {
        handle.remove(self.timeout);
        for pending in self.pending.into_values() {
            handle.remove(pending.token);
        }
    }
}

#[derive(Debug)]
struct PendingRead {
    mime: String,
    token: RegistrationToken,
    buf: Vec<u8>,
}

/// A client set the regular clipboard and persistence is enabled: schedule an
/// async snapshot of the new selection. Deferred to a loop idle so the new
/// source is the current selection by the time we read it.
pub fn on_new_clipboard(state: &mut State, seat: Seat<State>, mimes: Vec<String>) {
    if mimes.is_empty() {
        return;
    }
    // Supersede any snapshot that is still in flight.
    if let Some(old) = state.common.clipboard_state.active.take() {
        old.cancel(&state.common.event_loop_handle);
    }
    state.common.clipboard_state.generation =
        state.common.clipboard_state.generation.wrapping_add(1);
    let generation = state.common.clipboard_state.generation;

    state
        .common
        .event_loop_handle
        .insert_idle(move |state| start_snapshot(state, seat, mimes, generation));
}

/// A client cleared the clipboard: drop any in-flight snapshot and forget the
/// cached contents so we stop serving stale data.
pub fn on_clipboard_cleared(state: &mut State) {
    if let Some(old) = state.common.clipboard_state.active.take() {
        old.cancel(&state.common.event_loop_handle);
    }
    state.common.clipboard_state.generation =
        state.common.clipboard_state.generation.wrapping_add(1);
    state.common.clipboard_state.cache.clear();
}

/// Write cached bytes for `mime_type` to `fd` on a detached thread. Used when a
/// client (Wayland or Xwayland) pastes a promoted, compositor-owned selection.
pub fn serve(state: &State, mime_type: &str, fd: OwnedFd) {
    let Some(bytes) = state.common.clipboard_state.cache.get(mime_type).cloned() else {
        debug!(
            mime = mime_type,
            "clipboard serve: mime not cached, closing fd"
        );
        return; // dropping `fd` closes it, the reader just gets EOF
    };
    if let Err(err) = std::thread::Builder::new()
        .name("clipboard-paste".into())
        .spawn(move || {
            use std::io::Write;
            let mut file = std::fs::File::from(fd);
            if let Err(err) = file.write_all(&bytes) {
                debug!(?err, "clipboard serve: write to paste fd failed");
            }
        })
    {
        debug!(?err, "clipboard serve: failed to spawn writer thread");
    }
}

fn start_snapshot(state: &mut State, seat: Seat<State>, mimes: Vec<String>, generation: u64) {
    // Superseded by a newer selection while we were queued?
    if state.common.clipboard_state.generation != generation {
        return;
    }
    // Config toggled off between scheduling and now?
    if !state.common.config.cosmic_conf.clipboard_persistence {
        return;
    }
    // Never re-snapshot our own compositor-owned selection.
    if matches!(
        current_data_device_selection_userdata(&seat).as_deref(),
        Some(SelectionUserData::Persisted)
    ) {
        return;
    }

    let handle = state.common.event_loop_handle.clone();
    let mut pending: HashMap<usize, PendingRead> = HashMap::new();
    let mut next_id = 0usize;

    for mime in mimes {
        let (read_fd, write_fd) = match rustix::pipe::pipe_with(rustix::pipe::PipeFlags::CLOEXEC) {
            Ok(fds) => fds,
            Err(err) => {
                debug!(?err, "clipboard snapshot: pipe() failed");
                continue;
            }
        };
        // Hand the write end to the source client; it writes the bytes for us.
        if let Err(err) = request_data_device_client_selection(&seat, mime.clone(), write_fd) {
            debug!(?err, %mime, "clipboard snapshot: could not request selection");
            continue;
        }
        let id = next_id;
        next_id += 1;
        // Blocking read end + `Mode::Level` + a single read per callback means
        // the callback never blocks (calloop only fires it when readable).
        let token = match handle.insert_source(
            Generic::new(read_fd, Interest::READ, Mode::Level),
            move |_, source, state: &mut State| {
                // SAFETY: we never move the fd out of the source.
                let fd = unsafe { source.get_mut() };
                Ok(read_ready(state, generation, id, fd.as_fd()))
            },
        ) {
            Ok(token) => token,
            Err(err) => {
                debug!(?err, "clipboard snapshot: failed to register read source");
                continue;
            }
        };
        pending.insert(
            id,
            PendingRead {
                mime,
                token,
                buf: Vec::new(),
            },
        );
    }

    if pending.is_empty() {
        return;
    }

    let timeout = match handle.insert_source(
        Timer::from_duration(SNAPSHOT_TIMEOUT),
        move |_, _, state: &mut State| {
            snapshot_timed_out(state, generation);
            TimeoutAction::Drop
        },
    ) {
        Ok(token) => token,
        Err(err) => {
            debug!(?err, "clipboard snapshot: failed to arm timeout");
            for pending in pending.into_values() {
                handle.remove(pending.token);
            }
            return;
        }
    };

    state.common.clipboard_state.active = Some(Snapshot {
        generation,
        seat,
        timeout,
        pending,
        collected: HashMap::new(),
        total_bytes: 0,
    });
}

/// How a single read callback wants to conclude, decided while the snapshot is
/// borrowed and acted on afterwards (so we can re-borrow `state`).
enum Step {
    Continue,
    Remove,
    Finalize,
    Abort,
}

fn read_ready(state: &mut State, generation: u64, id: usize, fd: BorrowedFd<'_>) -> PostAction {
    let step = {
        let Some(snapshot) = state.common.clipboard_state.active.as_mut() else {
            return PostAction::Remove;
        };
        if snapshot.generation != generation {
            return PostAction::Remove;
        }
        let mut buf = [0u8; READ_CHUNK];
        match rustix::io::read(fd, &mut buf) {
            Ok(0) => {
                if let Some(pending) = snapshot.pending.remove(&id) {
                    snapshot.collected.insert(pending.mime, pending.buf);
                }
                if snapshot.pending.is_empty() {
                    Step::Finalize
                } else {
                    Step::Remove
                }
            }
            Ok(n) => {
                snapshot.total_bytes += n;
                if snapshot.total_bytes > MAX_TOTAL_BYTES {
                    debug!(
                        limit = MAX_TOTAL_BYTES,
                        "clipboard snapshot: selection too large, not persisting"
                    );
                    // Drop this mime's entry so the abort below does not try to
                    // remove the token this callback already self-removes.
                    snapshot.pending.remove(&id);
                    Step::Abort
                } else {
                    if let Some(pending) = snapshot.pending.get_mut(&id) {
                        pending.buf.extend_from_slice(&buf[..n]);
                    }
                    Step::Continue
                }
            }
            Err(rustix::io::Errno::INTR) | Err(rustix::io::Errno::AGAIN) => Step::Continue,
            Err(err) => {
                debug!(?err, "clipboard snapshot: read error, dropping mime");
                snapshot.pending.remove(&id);
                if snapshot.pending.is_empty() {
                    Step::Finalize
                } else {
                    Step::Remove
                }
            }
        }
    };

    match step {
        Step::Continue => PostAction::Continue,
        Step::Remove => PostAction::Remove,
        Step::Finalize => {
            finalize(state, generation);
            PostAction::Remove
        }
        Step::Abort => {
            abort(state, generation);
            PostAction::Remove
        }
    }
}

/// Every read finished: remove the (still-armed) timeout and promote.
fn finalize(state: &mut State, generation: u64) {
    let Some(snapshot) = state.common.clipboard_state.active.take() else {
        return;
    };
    if snapshot.generation != generation {
        state.common.clipboard_state.active = Some(snapshot);
        return;
    }
    state.common.event_loop_handle.remove(snapshot.timeout);
    promote(state, snapshot);
}

/// A hard limit was hit (too large): tear down without promoting.
fn abort(state: &mut State, generation: u64) {
    let Some(snapshot) = state.common.clipboard_state.active.take() else {
        return;
    };
    if snapshot.generation != generation {
        state.common.clipboard_state.active = Some(snapshot);
        return;
    }
    snapshot.cancel(&state.common.event_loop_handle);
}

/// The timeout fired: promote whatever mime types completed and drop the rest.
fn snapshot_timed_out(state: &mut State, generation: u64) {
    let Some(snapshot) = state.common.clipboard_state.active.take() else {
        return;
    };
    if snapshot.generation != generation {
        state.common.clipboard_state.active = Some(snapshot);
        return;
    }
    // The timer self-drops via `TimeoutAction::Drop`; only cancel the reads
    // that never completed.
    for pending in snapshot.pending.values() {
        state.common.event_loop_handle.remove(pending.token);
    }
    promote(state, snapshot);
}

/// Replace the current selection with a compositor-owned copy backed by the
/// snapshot's bytes. No-op if nothing was captured.
fn promote(state: &mut State, snapshot: Snapshot) {
    if snapshot.collected.is_empty() {
        return;
    }
    let mimes: Vec<String> = snapshot.collected.keys().cloned().collect();
    let dh = state.common.display_handle.clone();
    set_data_device_selection(&dh, &snapshot.seat, mimes, SelectionUserData::Persisted);
    state.common.clipboard_state.cache = snapshot
        .collected
        .into_iter()
        .map(|(mime, bytes)| (mime, Arc::from(bytes.into_boxed_slice())))
        .collect();
    debug!(
        mimes = state.common.clipboard_state.cache.len(),
        "clipboard: promoted selection to compositor-owned cache"
    );
}
