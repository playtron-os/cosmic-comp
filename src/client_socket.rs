// SPDX-License-Identifier: GPL-3.0-only

//! The protocol library reports a client dropped for not reading its events
//! the same way as one that hung up; the client's socket tells them apart.

use std::{
    os::fd::{AsFd, AsRawFd, OwnedFd},
    sync::Mutex,
};

/// A duplicate of a client's socket, held until the client disconnects.
#[derive(Debug, Default)]
pub struct ClientSocket(Mutex<Option<OwnedFd>>);

impl ClientSocket {
    pub fn new(stream: &impl AsFd) -> Self {
        Self(Mutex::new(stream.as_fd().try_clone_to_owned().ok()))
    }

    /// At disconnect: the bytes left unread in the socket of a client that is
    /// still connected but stopped reading, or `None` when it hung up. Closes
    /// the duplicate, or the client would never see the hang-up.
    pub fn close(&self) -> Option<usize> {
        let fd = self.0.lock().unwrap().take()?;
        unread_while_connected(&fd)
    }
}

fn unread_while_connected(fd: &OwnedFd) -> Option<usize> {
    let mut poll = libc::pollfd {
        fd: fd.as_raw_fd(),
        events: libc::POLLOUT,
        revents: 0,
    };
    // SAFETY: one valid pollfd, no timeout.
    if unsafe { libc::poll(&raw mut poll, 1, 0) } != 0 {
        // Hung up, failed, or still writable: not a client that stopped reading.
        return None;
    }
    let mut unread: libc::c_int = 0;
    // SAFETY: SIOCOUTQ (TIOCOUTQ) stores one int: the bytes the peer has not read.
    unsafe { libc::ioctl(fd.as_raw_fd(), libc::TIOCOUTQ, &raw mut unread) };
    Some(unread.max(0) as usize)
}
