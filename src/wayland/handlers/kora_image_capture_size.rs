// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    state::State, wayland::protocols::kora_image_capture_size::delegate_kora_image_capture_size,
};

delegate_kora_image_capture_size!(State);
