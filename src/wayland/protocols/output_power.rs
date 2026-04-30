// SPDX-License-Identifier: GPL-3.0-only

use smithay::{
    output::{Output, WeakOutput},
    reexports::{
        wayland_protocols_wlr::output_power_management::v1::server::{
            zwlr_output_power_manager_v1::{self, ZwlrOutputPowerManagerV1},
            zwlr_output_power_v1::{self, ZwlrOutputPowerV1},
        },
        wayland_server::{
            Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource,
            backend::GlobalId,
        },
    },
    wayland::{Dispatch2, GlobalDispatch2},
};
use std::{collections::HashMap, mem};
use wayland_backend::{protocol::WEnum, server::ClientId};

pub trait OutputPowerHandler {
    fn output_power_state(&mut self) -> &mut OutputPowerState;
    fn get_dpms(&mut self, output: &Output) -> Option<bool>;
    fn set_dpms(&mut self, output: &Output, on: bool);
}

#[derive(Debug)]
pub struct OutputPowerState {
    global: GlobalId,
    output_powers: HashMap<ZwlrOutputPowerV1, zwlr_output_power_v1::Mode>,
}

impl OutputPowerState {
    pub fn new<D, F>(dh: &DisplayHandle, client_filter: F) -> OutputPowerState
    where
        D: GlobalDispatch<ZwlrOutputPowerManagerV1, OutputPowerManagerGlobalData> + 'static,
        F: for<'a> Fn(&'a Client) -> bool + Clone + Send + Sync + 'static,
    {
        let global = dh.create_global::<D, ZwlrOutputPowerManagerV1, _>(
            1,
            OutputPowerManagerGlobalData {
                filter: Box::new(client_filter.clone()),
            },
        );

        OutputPowerState {
            global,
            output_powers: HashMap::new(),
        }
    }

    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }

    /// Send `mode` events for any output powers where dpms state has changed.
    ///
    /// This is handled automatically for changes made through the protocol.
    pub fn refresh<D: OutputPowerHandler>(state: &mut D) {
        let mut output_powers = mem::take(&mut state.output_power_state().output_powers);
        for (output_power, old_mode) in output_powers.iter_mut() {
            let data = output_power.data::<OutputPowerData>().unwrap();
            if let Some(output) = data.output.upgrade() {
                if let Some(on) = state.get_dpms(&output) {
                    let mode = output_power_mode(on);
                    if mode != *old_mode {
                        output_power.mode(mode);
                        *old_mode = mode;
                    }
                }
            }
        }
        state.output_power_state().output_powers = output_powers;
    }
}

pub struct OutputPowerManagerGlobalData {
    filter: Box<dyn for<'a> Fn(&'a Client) -> bool + Send + Sync>,
}

/// User data for `ZwlrOutputPowerManagerV1` resource instances.
pub struct OutputPowerManagerData;

pub struct OutputPowerData {
    output: WeakOutput,
}

impl<D> GlobalDispatch2<ZwlrOutputPowerManagerV1, D> for OutputPowerManagerGlobalData
where
    D: Dispatch<ZwlrOutputPowerManagerV1, OutputPowerManagerData> + 'static,
{
    fn bind(
        &self,
        _state: &mut D,
        _dh: &DisplayHandle,
        _client: &Client,
        resource: New<ZwlrOutputPowerManagerV1>,
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, OutputPowerManagerData);
    }

    fn can_view(&self, client: &Client) -> bool {
        (self.filter)(client)
    }
}

impl<D> Dispatch2<ZwlrOutputPowerManagerV1, D> for OutputPowerManagerData
where
    D: Dispatch<ZwlrOutputPowerV1, OutputPowerData> + OutputPowerHandler + 'static,
{
    fn request(
        &self,
        state: &mut D,
        _client: &Client,
        _obj: &ZwlrOutputPowerManagerV1,
        request: zwlr_output_power_manager_v1::Request,
        _dh: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zwlr_output_power_manager_v1::Request::GetOutputPower { id, output } => {
                let output = Output::from_resource(&output);
                let output_power = data_init.init(
                    id,
                    OutputPowerData {
                        output: output.as_ref().map(|o| o.downgrade()).unwrap_or_default(),
                    },
                );
                if let Some(on) = output.as_ref().and_then(|o| state.get_dpms(o)) {
                    let mode = output_power_mode(on);
                    output_power.mode(mode);
                    state
                        .output_power_state()
                        .output_powers
                        .insert(output_power, mode);
                } else {
                    output_power.failed();
                }
            }
            zwlr_output_power_manager_v1::Request::Destroy => {}
            _ => unreachable!(),
        }
    }
}

impl<D> Dispatch2<ZwlrOutputPowerV1, D> for OutputPowerData
where
    D: OutputPowerHandler + 'static,
{
    fn request(
        &self,
        state: &mut D,
        _client: &Client,
        obj: &ZwlrOutputPowerV1,
        request: zwlr_output_power_v1::Request,
        _dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zwlr_output_power_v1::Request::SetMode { mode } => {
                if let Some(output) = self.output.upgrade() {
                    let on = match mode {
                        WEnum::Value(zwlr_output_power_v1::Mode::On) => true,
                        WEnum::Value(zwlr_output_power_v1::Mode::Off) => false,
                        _ => {
                            return;
                        }
                    };
                    state.set_dpms(&output, on);
                    if let Some(on) = state.get_dpms(&output) {
                        let mode = output_power_mode(on);
                        for (output_power, old_mode) in
                            state.output_power_state().output_powers.iter_mut()
                        {
                            let data = output_power.data::<OutputPowerData>().unwrap();
                            if data.output == output && mode != *old_mode {
                                output_power.mode(mode);
                                *old_mode = mode;
                            }
                        }
                    } else {
                        obj.failed();
                        state.output_power_state().output_powers.remove(obj);
                    }
                } else {
                    obj.failed();
                    state.output_power_state().output_powers.remove(obj);
                }
            }
            zwlr_output_power_v1::Request::Destroy => {}
            _ => unreachable!(),
        }
    }

    fn destroyed(&self, state: &mut D, _client: ClientId, obj: &ZwlrOutputPowerV1) {
        state.output_power_state().output_powers.remove(obj);
    }
}

fn output_power_mode(on: bool) -> zwlr_output_power_v1::Mode {
    if on {
        zwlr_output_power_v1::Mode::On
    } else {
        zwlr_output_power_v1::Mode::Off
    }
}
