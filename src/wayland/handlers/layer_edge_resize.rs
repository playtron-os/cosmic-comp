// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    state::State,
    wayland::protocols::layer_edge_resize::{
        EdgeResizeHandler, EdgeResizeState, delegate_layer_edge_resize,
    },
};

impl EdgeResizeHandler for State {
    fn edge_resize_state(&mut self) -> &mut EdgeResizeState {
        &mut self.common.edge_resize_state
    }
}

delegate_layer_edge_resize!(State);
