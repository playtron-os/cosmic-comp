// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    state::State, wayland::protocols::kora_capture_exclusion::delegate_kora_capture_exclusion,
};

delegate_kora_capture_exclusion!(State);
