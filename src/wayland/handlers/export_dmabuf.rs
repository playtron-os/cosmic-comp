// SPDX-License-Identifier: GPL-3.0-only

use smithay::output::Output;
use smithay::reexports::wayland_server::protocol::wl_output::WlOutput;

use crate::{
    state::State,
    wayland::protocols::export_dmabuf::{ExportDmabufHandler, ExportDmabufState},
};

impl ExportDmabufHandler for State {
    fn export_dmabuf_state(&mut self) -> &mut ExportDmabufState {
        &mut self.common.export_dmabuf_state
    }

    fn output_from_wl_output(&self, wl_output: &WlOutput) -> Option<Output> {
        Output::from_resource(wl_output)
    }
}

crate::wayland::protocols::export_dmabuf::delegate_export_dmabuf!(State);
