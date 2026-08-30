// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    shell::Shell,
    state::{BackendData, State},
    utils::prelude::OutputExt,
    wayland::protocols::{
        output_configuration::OutputConfigurationState, workspace::WorkspaceUpdateGuard,
    },
};
use anyhow::Context;
use cosmic_config::{ConfigGet, CosmicConfigEntry};
use cosmic_settings_config::window_rules::ApplicationException;
use cosmic_settings_config::{Shortcuts, shortcuts, window_rules};
use serde::{Deserialize, Serialize};
use smithay::utils::{Clock, Monotonic};
use smithay::wayland::xdg_activation::XdgActivationState;
pub use smithay::{
    backend::input::{self as smithay_input, KeyState},
    input::keyboard::{Keysym, ModifiersState, keysyms as KeySyms},
    output::{Mode, Output},
    reexports::{
        calloop::LoopHandle,
        input::{
            AccelProfile, ClickMethod, Device as InputDevice, ScrollMethod, SendEventsMode,
            TapButtonMap,
        },
    },
    utils::{Logical, Physical, Point, SERIAL_COUNTER, Size, Transform},
};
use std::{
    cell::{Ref, RefCell},
    collections::{BTreeMap, HashMap, HashSet},
    fs::OpenOptions,
    io::Write,
    path::PathBuf,
    sync::{Arc, atomic::AtomicBool},
};
use tracing::{error, info, warn};

mod input_config;
pub mod key_bindings;
mod types;
pub mod voice;

use crate::toolkit_config::{ToolkitConfig, icon_theme_default, icon_theme_set_default};
pub use cosmic_comp_config::EdidProduct;
use cosmic_comp_config::{
    ActivationPolicy, AppearanceConfig, CosmicCompConfig, KeyboardConfig, TileBehavior, XkbConfig,
    XwaylandDescaling, XwaylandEavesdropping, ZoomConfig,
    input::{DeviceState as InputDeviceState, InputConfig, TouchpadOverride},
    output::comp::{
        OutputConfig, OutputInfo, OutputState, OutputsConfig, TransformDef, load_outputs,
    },
    workspace::WorkspaceConfig,
};
pub use key_bindings::{Action, PrivateAction};
use types::WlXkbConfig;

#[derive(Debug)]
pub struct Config {
    pub dynamic_conf: DynamicConfig,
    pub cosmic_helper: cosmic_config::Config,
    /// cosmic-config comp configuration for `com.system76.CosmicComp`
    pub cosmic_conf: CosmicCompConfig,
    /// cosmic-config context for `com.system76.CosmicSettings.Shortcuts`
    pub settings_context: cosmic_config::Config,
    /// Key bindings from `com.system76.CosmicSettings.Shortcuts`
    pub shortcuts: Shortcuts,
    // Tiling exceptions from `com.system76.CosmicSettings.WindowRules`
    pub tiling_exceptions: Vec<ApplicationException>,
    /// System actions from `com.system76.CosmicSettings.Shortcuts`
    pub system_actions: BTreeMap<shortcuts::action::System, String>,
    /// Voice mode configuration
    pub voice_config: voice::VoiceConfig,
}

#[derive(Debug)]
pub struct DynamicConfig {
    outputs: (Option<PathBuf>, OutputsConfig),
    numlock: (Option<PathBuf>, NumlockStateConfig),
    accessibility_filter: (Option<PathBuf>, ScreenFilter),
}

#[derive(Default, Debug, Deserialize, Serialize)]
pub struct NumlockStateConfig {
    pub last_state: bool,
}

pub struct CompOutputConfig<'a>(pub Ref<'a, OutputConfig>);

impl CompOutputConfig<'_> {
    pub fn mode_size(&self) -> Size<i32, Physical> {
        self.0.mode.0.into()
    }

    pub fn mode_refresh(&self) -> u32 {
        self.0.mode.1.unwrap_or(60_000)
    }

    pub fn transformed_size(&self) -> Size<i32, Physical> {
        self.transform().transform_size(self.mode_size())
    }

    pub fn output_mode(&self) -> Mode {
        Mode {
            size: self.mode_size(),
            refresh: self.mode_refresh() as i32,
        }
    }

    pub fn transform(&self) -> Transform {
        Transform::from(CompTransformDef(self.0.transform))
    }
}

pub struct CompTransformDef(pub TransformDef);

impl From<Transform> for CompTransformDef {
    fn from(transform: Transform) -> Self {
        let def = match transform {
            Transform::Normal => TransformDef::Normal,
            Transform::_90 => TransformDef::_90,
            Transform::_180 => TransformDef::_180,
            Transform::_270 => TransformDef::_270,
            Transform::Flipped => TransformDef::Flipped,
            Transform::Flipped90 => TransformDef::Flipped90,
            Transform::Flipped180 => TransformDef::Flipped180,
            Transform::Flipped270 => TransformDef::Flipped270,
        };
        CompTransformDef(def)
    }
}

impl From<CompTransformDef> for Transform {
    fn from(comp_transform: CompTransformDef) -> Self {
        match comp_transform.0 {
            TransformDef::Normal => Transform::Normal,
            TransformDef::_90 => Transform::_90,
            TransformDef::_180 => Transform::_180,
            TransformDef::_270 => Transform::_270,
            TransformDef::Flipped => Transform::Flipped,
            TransformDef::Flipped90 => Transform::Flipped90,
            TransformDef::Flipped180 => Transform::Flipped180,
            TransformDef::Flipped270 => Transform::Flipped270,
        }
    }
}

#[derive(Debug, Default, Deserialize, Serialize, Clone, PartialEq)]
pub struct ScreenFilter {
    pub inverted: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub color_filter: Option<ColorFilter>,
    #[serde(default)]
    pub night_shift: u16,
}

impl ScreenFilter {
    pub fn is_noop(&self) -> bool {
        !self.inverted && self.color_filter.is_none() && self.night_shift == 0
    }

    /// What the shader still has to do once the CRTC gamma LUT owns night shift.
    pub fn without_night_shift(&self) -> Self {
        Self {
            night_shift: 0,
            ..self.clone()
        }
    }
}

#[derive(Debug, Deserialize, Serialize, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
// these values need to match with offscreen.frag
pub enum ColorFilter {
    Greyscale = 1,
    Protanopia = 2,
    Deuteranopia = 3,
    Tritanopia = 4,
}

impl Config {
    pub fn load(loop_handle: &LoopHandle<'_, State>) -> Config {
        let config = cosmic_config::Config::new("com.system76.CosmicComp", 1).unwrap();
        let source = cosmic_config::calloop::ConfigWatchSource::new(&config).unwrap();
        loop_handle
            .insert_source(source, |(config, keys), (), state| {
                config_changed(config, keys, state);
            })
            .expect("Failed to add cosmic-config to the event loop");
        let xdg = xdg::BaseDirectories::new();

        let cosmic_comp_config =
            CosmicCompConfig::get_entry(&config).unwrap_or_else(|(errs, c)| {
                if cfg!(debug_assertions) {
                    for err in errs {
                        warn!(?err, "");
                    }
                }
                c
            });

        // Seed the render-side blur strength from config, so the first frame
        // already uses the configured intensity rather than a default.
        crate::backend::render::wayland::blur_effect::set_blur_config(
            cosmic_comp_config.blur_enabled,
            cosmic_comp_config.blur_intensity,
            cosmic_comp_config.blur_noise,
        );

        // Listen for updates to the toolkit config
        if let Ok(tk_config_ctx) = cosmic_config::Config::new("com.system76.CosmicTk", 1) {
            fn handle_new_toolkit_config(config: ToolkitConfig, state: &mut State) {
                if icon_theme_default() != config.icon_theme {
                    icon_theme_set_default(config.icon_theme.clone());
                    state.common.update_xwayland_settings();
                }

                let mut workspace_guard = state.common.workspace_state.update();
                state.common.shell.write().update_toolkit(
                    config,
                    &state.common.xdg_activation_state,
                    &mut workspace_guard,
                );
            }

            // Read initial toolkit config
            let config = ToolkitConfig::default();
            let _ = loop_handle.insert_idle(move |state| {
                handle_new_toolkit_config(config, state);
            });

            // Watch for toolkit config changes via cosmic-config
            match cosmic_config::calloop::ConfigWatchSource::new(&tk_config_ctx) {
                Ok(source) => {
                    if let Err(err) =
                        loop_handle.insert_source(source, |(config, _keys), (), state| {
                            // Re-read all toolkit fields on any change
                            let tk = ToolkitConfig {
                                icon_theme: config
                                    .get::<String>("icon_theme")
                                    .unwrap_or_else(|_| "Adwaita".to_string()),
                                show_minimize: config.get::<bool>("show_minimize").unwrap_or(true),
                                show_maximize: config.get::<bool>("show_maximize").unwrap_or(true),
                                apply_theme_global: config
                                    .get::<bool>("apply_theme_global")
                                    .unwrap_or(true),
                            };
                            handle_new_toolkit_config(tk, state);
                        })
                    {
                        warn!(?err, "Failed to watch com.system76.CosmicTk config");
                    }
                }
                Err(err) => {
                    warn!(
                        ?err,
                        "Failed to create config watch source for com.system76.CosmicTk"
                    );
                }
            }
        }

        // Source key bindings from com.system76.CosmicSettings.Shortcuts
        let settings_context = shortcuts::context().expect("Failed to load shortcuts config");
        let system_actions = shortcuts::system_actions(&settings_context);
        let shortcuts = shortcuts::shortcuts(&settings_context);

        // Listen for updates to the keybindings config.
        match cosmic_config::calloop::ConfigWatchSource::new(&settings_context) {
            Ok(source) => {
                if let Err(err) = loop_handle.insert_source(source, |(config, keys), (), state| {
                    for key in keys {
                        match key.as_str() {
                            // Reload the keyboard shortcuts config.
                            "custom" | "defaults" => {
                                state.common.config.shortcuts = shortcuts::shortcuts(&config);
                            }

                            "system_actions" => {
                                state.common.config.system_actions =
                                    shortcuts::system_actions(&config);
                            }

                            _ => (),
                        }
                    }
                }) {
                    warn!(
                        ?err,
                        "Failed to watch com.system76.CosmicSettings.Shortcuts config"
                    );
                }
            }
            Err(err) => warn!(
                ?err,
                "failed to create config watch source for com.system76.CosmicSettings.Shortcuts"
            ),
        };

        let window_rules_context =
            window_rules::context().expect("Failed to load window rules config");
        let tiling_exceptions = window_rules::tiling_exceptions(&window_rules_context);

        match cosmic_config::calloop::ConfigWatchSource::new(&window_rules_context) {
            Ok(source) => {
                if let Err(err) = loop_handle.insert_source(source, |(config, keys), (), state| {
                    for key in keys {
                        match key.as_str() {
                            "tiling_exception_defaults" | "tiling_exception_custom" => {
                                let new_exceptions = window_rules::tiling_exceptions(&config);
                                state.common.config.tiling_exceptions = new_exceptions;
                                state.common.shell.write().update_tiling_exceptions(
                                    state.common.config.tiling_exceptions.iter(),
                                );
                            }
                            _ => (),
                        }
                    }
                }) {
                    warn!(
                        ?err,
                        "Failed to watch com.system76.CosmicSettings.WindowRules config"
                    );
                }
            }
            Err(err) => warn!(
                ?err,
                "failed to create config watch source for com.system76.CosmicSettings.WindowRules"
            ),
        };

        let _ = loop_handle.insert_idle(|state| {
            let filter_conf = state.common.config.dynamic_conf.screen_filter();
            state
                .common
                .a11y_state
                .set_screen_inverted(filter_conf.inverted);
            state
                .common
                .a11y_state
                .set_screen_filter(filter_conf.color_filter);

            // Sync night_shift from cosmic-config into the screen filter
            let night_shift = state.common.config.cosmic_conf.night_shift;
            if night_shift != state.common.config.dynamic_conf.screen_filter().night_shift {
                let mut filter = state.common.config.dynamic_conf.screen_filter_mut();
                filter.night_shift = night_shift;
            }
        });

        // Load voice mode configuration
        let voice_config = voice::VoiceConfig::load();

        // Watch for voice config changes
        if let Ok(voice_context) = voice::VoiceConfig::context() {
            match cosmic_config::calloop::ConfigWatchSource::new(&voice_context) {
                Ok(source) => {
                    if let Err(err) =
                        loop_handle.insert_source(source, |(_config, _keys), (), state| {
                            state.common.config.voice_config = voice::VoiceConfig::load();
                            tracing::info!("Voice mode configuration reloaded");
                        })
                    {
                        warn!(?err, "Failed to watch voice mode config");
                    }
                }
                Err(err) => warn!(?err, "Failed to create voice config watch source"),
            }
        }

        Config {
            dynamic_conf: Self::load_dynamic(&xdg),
            cosmic_conf: cosmic_comp_config,
            cosmic_helper: config,
            settings_context,
            shortcuts,
            system_actions,
            tiling_exceptions,
            voice_config,
        }
    }

    fn load_dynamic(xdg: &xdg::BaseDirectories) -> DynamicConfig {
        let output_path = xdg.place_state_file("cosmic-comp/outputs.ron").ok();
        let outputs = load_outputs(output_path.as_ref());
        let numlock_path = xdg.place_state_file("cosmic-comp/numlock.ron").ok();
        let numlock = Self::load_numlock(&numlock_path);

        let filter_path = xdg
            .place_state_file("cosmic-comp/a11y_screen_filter.ron")
            .ok();
        let filter = Self::load_filter_state(&filter_path);

        DynamicConfig {
            outputs: (output_path, outputs),
            numlock: (numlock_path, numlock),
            accessibility_filter: (filter_path, filter),
        }
    }

    fn load_numlock(path: &Option<PathBuf>) -> NumlockStateConfig {
        path.as_deref()
            .filter(|path| path.exists())
            .and_then(|path| {
                ron::de::from_reader::<_, NumlockStateConfig>(
                    OpenOptions::new().read(true).open(path).unwrap(),
                )
                .map_err(|err| {
                    warn!(?err, "Failed to read numlock.ron, resetting..");
                    if let Err(err) = std::fs::remove_file(path) {
                        error!(?err, "Failed to remove numlock.ron.");
                    }
                })
                .ok()
            })
            .unwrap_or_default()
    }

    fn load_filter_state(path: &Option<PathBuf>) -> ScreenFilter {
        if let Some(path) = path.as_ref()
            && path.exists()
        {
            match ron::de::from_reader::<_, ScreenFilter>(
                OpenOptions::new().read(true).open(path).unwrap(),
            ) {
                Ok(config) => return config,
                Err(err) => {
                    warn!(?err, "Failed to read screen_filter state, resetting..");
                    if let Err(err) = std::fs::remove_file(path) {
                        error!(?err, "Failed to remove screen_filter state.");
                    }
                }
            };
        }

        ScreenFilter {
            inverted: false,
            color_filter: None,
            night_shift: 0,
        }
    }

    pub fn shortcut_for_action(&self, action: &shortcuts::Action) -> Option<String> {
        self.shortcuts.shortcut_for_action(action)
    }

    pub fn read_outputs(
        &mut self,
        output_state: &mut OutputConfigurationState<State>,
        backend: &mut BackendData,
        shell: &Arc<parking_lot::RwLock<Shell>>,
        loop_handle: &LoopHandle<'static, State>,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
        xdg_activation_state: &XdgActivationState,
        startup_done: Arc<AtomicBool>,
        clock: &Clock<Monotonic>,
    ) -> anyhow::Result<()> {
        let outputs = output_state.outputs().collect::<Vec<_>>();
        let mut infos = outputs
            .iter()
            .cloned()
            .map(Into::<crate::config::CompOutputInfo>::into)
            .map(|i| i.0)
            .collect::<Vec<_>>();
        infos.sort();

        if let Some(configs) = self
            .dynamic_conf
            .outputs()
            .config
            .get(&infos)
            .filter(|configs| {
                if configs
                    .iter()
                    .all(|config| config.enabled == OutputState::Disabled)
                {
                    if !configs.is_empty() {
                        error!(
                            "Broken config, all outputs disabled. Resetting... {:?}",
                            configs
                        );
                    }
                    false
                } else {
                    true
                }
            })
            .cloned()
        {
            let known_good_configs = outputs
                .iter()
                .map(|output| {
                    output
                        .user_data()
                        .get::<RefCell<OutputConfig>>()
                        .unwrap()
                        .borrow()
                        .clone()
                })
                .collect::<Vec<_>>();

            let mut found_outputs = Vec::new();
            for (name, output_config) in infos.iter().map(|o| &o.connector).zip(configs) {
                let output = outputs.iter().find(|o| &o.name() == name).unwrap().clone();
                let enabled = output_config.enabled.clone();
                *output
                    .user_data()
                    .get::<RefCell<OutputConfig>>()
                    .unwrap()
                    .borrow_mut() = output_config;
                found_outputs.push((output.clone(), enabled));
            }

            let mut backend = backend.lock();
            if let Err(err) = backend.apply_config_for_outputs(
                false,
                loop_handle,
                self.dynamic_conf.screen_filter(),
                shell.clone(),
                workspace_state,
                xdg_activation_state,
                startup_done.clone(),
                clock,
            ) {
                warn!(?err, "Failed to set new config.");
                found_outputs.clear();
                for (output, output_config) in outputs.clone().into_iter().zip(known_good_configs) {
                    let enabled = output_config.enabled.clone();
                    *output
                        .user_data()
                        .get::<RefCell<OutputConfig>>()
                        .unwrap()
                        .borrow_mut() = output_config;
                    found_outputs.push((output.clone(), enabled));
                }

                backend
                    .apply_config_for_outputs(
                        false,
                        loop_handle,
                        self.dynamic_conf.screen_filter(),
                        shell.clone(),
                        workspace_state,
                        xdg_activation_state,
                        startup_done,
                        clock,
                    )
                    .context("Failed to reset config")?;

                for (output, enabled) in found_outputs {
                    if enabled == OutputState::Enabled {
                        output_state.enable_head(&output);
                    } else {
                        output_state.disable_head(&output);
                    }
                }
            } else {
                for (output, enabled) in found_outputs {
                    if enabled == OutputState::Enabled {
                        output_state.enable_head(&output);
                    } else {
                        output_state.disable_head(&output);
                    }
                }
            }

            output_state.update();
            self.write_outputs(output_state.outputs());
        } else {
            if outputs
                .iter()
                .all(|o| o.config().enabled == OutputState::Disabled)
            {
                for output in &outputs {
                    output.config_mut().enabled = OutputState::Enabled;
                }
            }

            if mirror_new_outputs_enabled() {
                mirror_new_outputs(&outputs, &self.known_outputs());
            }

            // we don't have a config, so lets generate somewhat sane positions
            let mut w = 0;
            if !outputs.iter().any(|o| o.config().xwayland_primary) {
                // if we don't have a primary output for xwayland from a previous config, pick one
                if let Some(primary) = outputs.iter().find(|o| extends_desktop(o)) {
                    primary.config_mut().xwayland_primary = true;
                }
            }
            for output in outputs.iter().filter(|o| extends_desktop(o)) {
                {
                    let mut config = output.config_mut();
                    config.position = (w, 0);
                }
                w += output.geometry().size.w as u32;
            }

            let mut backend = backend.lock();
            backend
                .apply_config_for_outputs(
                    false,
                    loop_handle,
                    self.dynamic_conf.screen_filter(),
                    shell.clone(),
                    workspace_state,
                    xdg_activation_state,
                    startup_done.clone(),
                    clock,
                )
                .context("Failed to set new config")?;

            for output in outputs {
                if output
                    .user_data()
                    .get::<RefCell<OutputConfig>>()
                    .unwrap()
                    .borrow()
                    .enabled
                    == OutputState::Enabled
                {
                    output_state.enable_head(&output);
                } else {
                    output_state.disable_head(&output);
                }
            }
            output_state.update();
            self.write_outputs(output_state.outputs());
        }

        Ok(())
    }

    pub fn write_outputs(
        &mut self,
        outputs: impl Iterator<Item = impl std::borrow::Borrow<Output>>,
    ) {
        let mut infos = outputs
            .map(|o| {
                let o = o.borrow();
                (
                    Into::<CompOutputInfo>::into(o.clone()).0,
                    o.user_data()
                        .get::<RefCell<OutputConfig>>()
                        .unwrap()
                        .borrow()
                        .clone(),
                )
            })
            .collect::<Vec<(OutputInfo, OutputConfig)>>();
        infos.sort_by(|(a, _), (b, _)| a.cmp(b));
        let (infos, configs) = infos.into_iter().unzip();
        self.dynamic_conf
            .outputs_mut()
            .config
            .insert(infos, configs);
    }

    /// Every display that took part in a stored configuration, in any combination.
    fn known_outputs(&self) -> HashSet<OutputInfo> {
        self.dynamic_conf
            .outputs()
            .config
            .keys()
            .flatten()
            .cloned()
            .collect()
    }

    pub fn xkb_config(&self) -> XkbConfig {
        self.cosmic_conf.xkb_config.clone()
    }

    pub fn read_device(&self, device: &mut InputDevice) {
        let (device_config, default_config) = self.get_device_config(device);
        input_config::update_device(device, device_config.as_ref(), default_config);
    }

    pub fn scroll_factor(&self, device: &InputDevice) -> f64 {
        let (device_config, default_config) = self.get_device_config(device);
        input_config::get_config(device_config.as_ref(), default_config, |x| {
            x.scroll_config.as_ref()?.scroll_factor
        })
        .map_or(1.0, |x| x.0)
    }

    pub fn map_to_output(&self, device: &InputDevice) -> Option<String> {
        let (device_config, default_config) = self.get_device_config(device);
        Some(
            input_config::get_config(device_config.as_ref(), default_config, |x| {
                x.map_to_output.clone()
            })?
            .0,
        )
    }

    fn get_device_config(&self, device: &InputDevice) -> (Option<InputConfig>, &InputConfig) {
        let is_touchpad = device.config_tap_finger_count() > 0;

        let default_config = if is_touchpad {
            &self.cosmic_conf.input_touchpad
        } else {
            &self.cosmic_conf.input_default
        };

        let mut device_config = self.cosmic_conf.input_devices.get(&*device.name()).cloned();
        if is_touchpad && self.cosmic_conf.input_touchpad_override == TouchpadOverride::ForceDisable
        {
            device_config = Some({
                let mut config = device_config.unwrap_or_default();
                config.state = InputDeviceState::Disabled;
                config
            });
        }

        (device_config, default_config)
    }
}

/// A mirroring output shows another output's content, so it occupies no space
/// of its own in the global layout.
fn extends_desktop(output: &Output) -> bool {
    !matches!(output.config().enabled, OutputState::Mirroring(_)) && output.mirroring().is_none()
}

/// `COSMIC_MIRROR_NEW_OUTPUTS=1` opts into [`mirror_new_outputs`]. Off by
/// default, so an unconfigured display extends the desktop as usual.
fn mirror_new_outputs_enabled() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED.get_or_init(|| {
        matches!(
            std::env::var("COSMIC_MIRROR_NEW_OUTPUTS").as_deref(),
            Ok("1") | Ok("true") | Ok("yes") | Ok("on")
        )
    })
}

/// Default a display we have never stored a configuration for to mirroring the
/// primary output, instead of silently extending the desktop onto it.
fn mirror_new_outputs(outputs: &[Output], known: &HashSet<OutputInfo>) {
    let is_new = |output: &Output| !known.contains(&CompOutputInfo::from(output.clone()).0);
    let is_enabled = |output: &Output| output.config().enabled == OutputState::Enabled;

    // Mirror onto the internal panel, or else onto a display the user has configured
    // before. With neither we have no established primary, so extend as usual.
    let Some(target) = outputs
        .iter()
        .find(|o| o.is_internal() && is_enabled(o))
        .or_else(|| outputs.iter().find(|o| is_enabled(o) && !is_new(o)))
        .cloned()
    else {
        return;
    };

    for output in outputs
        .iter()
        .filter(|o| **o != target && is_enabled(o) && is_new(o))
    {
        info!(
            output = output.name(),
            target = target.name(),
            "New display, defaulting to mirroring"
        );
        output.config_mut().enabled = OutputState::Mirroring(target.name());
    }
}

pub struct PersistenceGuard<'a, T: Serialize>(Option<PathBuf>, &'a mut T);

impl<T: Serialize> std::ops::Deref for PersistenceGuard<'_, T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.1
    }
}

impl<T: Serialize> std::ops::DerefMut for PersistenceGuard<'_, T> {
    fn deref_mut(&mut self) -> &mut T {
        self.1
    }
}

impl<T: Serialize> Drop for PersistenceGuard<'_, T> {
    fn drop(&mut self) {
        if let Some(path) = self.0.as_ref() {
            let content = match ron::ser::to_string_pretty(&self.1, Default::default()) {
                Ok(content) => content,
                Err(err) => {
                    warn!("Failed to serialize: {:?}", err);
                    return;
                }
            };

            let mut writer = match OpenOptions::new()
                .create(true)
                .truncate(true)
                .write(true)
                .open(path)
            {
                Ok(writer) => writer,
                Err(err) => {
                    warn!(?err, "Failed to persist {}.", path.display());
                    return;
                }
            };

            if let Err(err) = writer.write_all(content.as_bytes()) {
                warn!(?err, "Failed to persist {}", path.display());
            } else {
                let _ = writer.flush();
            }
        }
    }
}

impl DynamicConfig {
    pub fn outputs(&self) -> &OutputsConfig {
        &self.outputs.1
    }

    pub fn outputs_mut(&mut self) -> PersistenceGuard<'_, OutputsConfig> {
        PersistenceGuard(self.outputs.0.clone(), &mut self.outputs.1)
    }

    pub fn numlock(&self) -> &NumlockStateConfig {
        &self.numlock.1
    }

    pub fn numlock_mut(&mut self) -> PersistenceGuard<'_, NumlockStateConfig> {
        PersistenceGuard(self.numlock.0.clone(), &mut self.numlock.1)
    }

    pub fn screen_filter(&self) -> &ScreenFilter {
        &self.accessibility_filter.1
    }

    pub fn screen_filter_mut(&mut self) -> PersistenceGuard<'_, ScreenFilter> {
        PersistenceGuard(
            self.accessibility_filter.0.clone(),
            &mut self.accessibility_filter.1,
        )
    }
}

pub fn xkb_config_to_wl(config: &XkbConfig) -> WlXkbConfig<'_> {
    WlXkbConfig {
        rules: &config.rules,
        model: &config.model,
        layout: &config.layout,
        variant: &config.variant,
        options: config.options.clone(),
    }
}

fn get_config<T: Default + serde::de::DeserializeOwned>(
    config: &cosmic_config::Config,
    key: &str,
) -> T {
    config.get(key).unwrap_or_else(|err| {
        error!(?err, "Failed to read config '{}'", key);
        T::default()
    })
}

fn update_input(state: &mut State) {
    if let BackendData::Kms(kms_state) = &mut state.backend {
        for device in kms_state.input_devices.values_mut() {
            state.common.config.read_device(device);
        }
    }
}

pub fn change_modifier_state(
    keyboard: &smithay::input::keyboard::KeyboardHandle<State>,
    scan_code: u32,
    state: &mut State,
) {
    /// Offset used to convert Linux scancode to X11 keycode.
    const X11_KEYCODE_OFFSET: u32 = 8;

    let mut input = |key_state, scan_code| {
        let time = state.common.clock.now().as_millis();
        let _ = keyboard.input(
            state,
            smithay_input::Keycode::new(scan_code + X11_KEYCODE_OFFSET),
            key_state,
            SERIAL_COUNTER.next_serial(),
            time,
            |_, _, _| smithay::input::keyboard::FilterResult::<()>::Forward,
        );
    };

    input(smithay_input::KeyState::Pressed, scan_code);
    input(smithay_input::KeyState::Released, scan_code);
}

fn config_changed(config: cosmic_config::Config, keys: Vec<String>, state: &mut State) {
    for key in &keys {
        match key.as_str() {
            "xkb_config" => {
                let value = get_config::<XkbConfig>(&config, "xkb_config");
                let seats = state
                    .common
                    .shell
                    .read()
                    .seats
                    .iter()
                    .cloned()
                    .collect::<Vec<_>>();
                for seat in seats.into_iter() {
                    if let Some(keyboard) = seat.get_keyboard() {
                        let old_modifier_state = keyboard.modifier_state();
                        keyboard.change_repeat_info(
                            (value.repeat_rate as i32).abs(), // Negative values are illegal
                            (value.repeat_delay as i32).abs(),
                        );
                        if let Err(err) = keyboard.set_xkb_config(state, xkb_config_to_wl(&value)) {
                            error!(?err, "Failed to load provided xkb config");
                            // TODO Revert to default?
                        }

                        // Press and release the numlock key to update modifiers.
                        if old_modifier_state.num_lock != keyboard.modifier_state().num_lock {
                            const NUMLOCK_SCANCODE: u32 = 69;
                            change_modifier_state(&keyboard, NUMLOCK_SCANCODE, state);
                        }
                        if old_modifier_state.caps_lock != keyboard.modifier_state().caps_lock {
                            const CAPSLOCK_SCANCODE: u32 = 58;
                            change_modifier_state(&keyboard, CAPSLOCK_SCANCODE, state);
                        }
                    }
                }
                state.common.config.cosmic_conf.xkb_config = value;
            }
            "keyboard_config" => {
                let value = get_config::<KeyboardConfig>(&config, "keyboard_config");
                state.common.config.cosmic_conf.keyboard_config = value;
                let shell = state.common.shell.read();
                let seat = shell.seats.last_active();
                state.common.config.dynamic_conf.numlock_mut().last_state =
                    seat.get_keyboard().unwrap().modifier_state().num_lock;
            }
            "input_default" => {
                let value = get_config::<InputConfig>(&config, "input_default");
                state.common.config.cosmic_conf.input_default = value;
                update_input(state);
            }
            "input_touchpad" => {
                let value = get_config::<InputConfig>(&config, "input_touchpad");
                state.common.config.cosmic_conf.input_touchpad = value;
                update_input(state);
            }
            "input_touchpad_override" => {
                let value = get_config::<TouchpadOverride>(&config, "input_touchpad_override");
                state.common.config.cosmic_conf.input_touchpad_override = value;
                update_input(state)
            }
            "input_devices" => {
                let value = get_config::<HashMap<String, InputConfig>>(&config, "input_devices");
                state.common.config.cosmic_conf.input_devices = value;
                update_input(state);
            }
            "workspaces" => {
                state.common.config.cosmic_conf.workspaces =
                    get_config::<WorkspaceConfig>(&config, "workspaces");
                state.common.update_config();
            }
            "tiling_enabled" => {
                let new = get_config::<bool>(&config, "tiling_enabled");
                if new != state.common.config.cosmic_conf.tiling_enabled {
                    state.common.config.cosmic_conf.tiling_enabled = new;

                    let mut shell = state.common.shell.write();
                    let shell_ref = &mut *shell;
                    shell_ref.workspaces.update_tiling_enabled(
                        new,
                        &mut state.common.workspace_state.update(),
                        shell_ref.seats.iter(),
                    );
                }
            }
            "autotile" => {
                let new = get_config::<bool>(&config, "autotile");
                if new != state.common.config.cosmic_conf.autotile {
                    state.common.config.cosmic_conf.autotile = new;

                    let mut shell = state.common.shell.write();
                    let shell_ref = &mut *shell;
                    shell_ref.workspaces.update_autotile(
                        new,
                        &mut state.common.workspace_state.update(),
                        shell_ref.seats.iter(),
                    );
                }
            }
            "autotile_behavior" => {
                let new = get_config::<TileBehavior>(&config, "autotile_behavior");
                if new != state.common.config.cosmic_conf.autotile_behavior {
                    state.common.config.cosmic_conf.autotile_behavior = new;

                    let mut shell = state.common.shell.write();
                    let shell_ref = &mut *shell;
                    shell_ref.workspaces.update_autotile_behavior(
                        new,
                        &mut state.common.workspace_state.update(),
                        shell_ref.seats.iter(),
                    );
                }
            }
            "active_hint" => {
                let new = get_config::<bool>(&config, "active_hint");
                if new != state.common.config.cosmic_conf.active_hint {
                    state.common.config.cosmic_conf.active_hint = new;
                    state.common.update_config();
                }
            }
            "descale_xwayland" => {
                let new = get_config::<XwaylandDescaling>(&config, "descale_xwayland");
                if new != state.common.config.cosmic_conf.descale_xwayland {
                    state.common.config.cosmic_conf.descale_xwayland = new;
                    state.common.update_xwayland_settings();
                }
            }
            "xwayland_eavesdropping" => {
                let new = get_config::<XwaylandEavesdropping>(&config, "xwayland_eavesdropping");
                if new != state.common.config.cosmic_conf.xwayland_eavesdropping {
                    state.common.config.cosmic_conf.xwayland_eavesdropping = new;
                    state
                        .common
                        .xwayland_reset_eavesdropping(SERIAL_COUNTER.next_serial());
                }
            }
            "focus_follows_cursor" => {
                let new = get_config::<bool>(&config, "focus_follows_cursor");
                if new != state.common.config.cosmic_conf.focus_follows_cursor {
                    state.common.config.cosmic_conf.focus_follows_cursor = new;
                }
            }
            "cursor_follows_focus" => {
                let new = get_config::<bool>(&config, "cursor_follows_focus");
                if new != state.common.config.cosmic_conf.cursor_follows_focus {
                    state.common.config.cosmic_conf.cursor_follows_focus = new;
                }
            }
            "focus_follows_cursor_delay" => {
                let new = get_config::<u64>(&config, "focus_follows_cursor_delay");
                if new != state.common.config.cosmic_conf.focus_follows_cursor_delay {
                    state.common.config.cosmic_conf.focus_follows_cursor_delay = new;
                }
            }
            "edge_snap_threshold" => {
                let new = get_config::<u32>(&config, "edge_snap_threshold");
                if new != state.common.config.cosmic_conf.edge_snap_threshold {
                    state.common.config.cosmic_conf.edge_snap_threshold = new;
                }
            }
            "accessibility_zoom" => {
                let new = get_config::<ZoomConfig>(&config, "accessibility_zoom");
                if new != state.common.config.cosmic_conf.accessibility_zoom {
                    state.common.config.cosmic_conf.accessibility_zoom = new;
                    state.common.update_config();
                }
            }
            "appearance_settings" => {
                let new = get_config::<AppearanceConfig>(&config, "appearance_settings");
                if new != state.common.config.cosmic_conf.appearance_settings {
                    state.common.config.cosmic_conf.appearance_settings = new;
                    state.common.update_config();
                    for output in state.common.shell.read().outputs() {
                        state.backend.schedule_render(output);
                    }
                }
            }
            "night_shift" => {
                let new = get_config::<u16>(&config, "night_shift");
                if new != state.common.config.cosmic_conf.night_shift {
                    state.common.config.cosmic_conf.night_shift = new;
                    let mut filter = state.common.config.dynamic_conf.screen_filter_mut();
                    let mut updated = (*filter).clone();
                    updated.night_shift = new;
                    if let Err(err) = state.backend.update_screen_filter(&updated) {
                        warn!("Failed to apply night shift: {}", err);
                    } else {
                        *filter = updated;
                    }
                }
            }
            "blur_enabled" => {
                let new = get_config::<bool>(&config, "blur_enabled");
                if new != state.common.config.cosmic_conf.blur_enabled {
                    state.common.config.cosmic_conf.blur_enabled = new;
                    crate::backend::render::wayland::blur_effect::set_blur_config(
                        new,
                        state.common.config.cosmic_conf.blur_intensity,
                        state.common.config.cosmic_conf.blur_noise,
                    );
                    for output in state.common.shell.read().outputs() {
                        state.backend.schedule_render(output);
                    }
                }
            }
            "blur_intensity" => {
                let new = get_config::<f32>(&config, "blur_intensity");
                if (new - state.common.config.cosmic_conf.blur_intensity).abs() > f32::EPSILON {
                    state.common.config.cosmic_conf.blur_intensity = new;
                    crate::backend::render::wayland::blur_effect::set_blur_config(
                        state.common.config.cosmic_conf.blur_enabled,
                        new,
                        state.common.config.cosmic_conf.blur_noise,
                    );
                    // Invalidate blur caches so new intensity takes effect
                    for output in state.common.shell.read().outputs() {
                        state.backend.schedule_render(output);
                    }
                }
            }
            "blur_noise" => {
                let new = get_config::<f32>(&config, "blur_noise");
                if (new - state.common.config.cosmic_conf.blur_noise).abs() > f32::EPSILON {
                    state.common.config.cosmic_conf.blur_noise = new;
                    crate::backend::render::wayland::blur_effect::set_blur_config(
                        state.common.config.cosmic_conf.blur_enabled,
                        state.common.config.cosmic_conf.blur_intensity,
                        new,
                    );
                    for output in state.common.shell.read().outputs() {
                        state.backend.schedule_render(output);
                    }
                }
            }
            "cursor_hide_timeout" => {
                let new = get_config::<Option<u32>>(&config, "cursor_hide_timeout");
                if new != state.common.config.cosmic_conf.cursor_hide_timeout {
                    state.common.config.cosmic_conf.cursor_hide_timeout = new;
                    let seats: Vec<_> = state.common.shell.read().seats.iter().cloned().collect();
                    let mut needs_render = false;
                    for seat in seats {
                        needs_render |=
                            crate::backend::render::cursor::notify_cursor_activity(state, &seat);
                    }
                    if needs_render {
                        let outputs: Vec<_> =
                            state.common.shell.read().outputs().cloned().collect();
                        for output in outputs {
                            state.backend.schedule_render(&output);
                        }
                    }
                }
            }
            "activation_policy" => {
                let new = get_config::<ActivationPolicy>(&config, "activation_policy");
                if new != state.common.config.cosmic_conf.activation_policy {
                    state.common.config.cosmic_conf.activation_policy = new;
                }
            }
            "clipboard_persistence" => {
                let new = get_config::<bool>(&config, "clipboard_persistence");
                if new != state.common.config.cosmic_conf.clipboard_persistence {
                    state.common.config.cosmic_conf.clipboard_persistence = new;
                }
            }
            _ => {}
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CompOutputInfo(OutputInfo);

impl From<Output> for CompOutputInfo {
    fn from(o: Output) -> CompOutputInfo {
        let physical = o.physical_properties();
        CompOutputInfo(OutputInfo {
            connector: o.name(),
            make: physical.make,
            model: physical.model,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use smithay::output::{PhysicalProperties, Subpixel};

    fn output(name: &str) -> Output {
        let output = Output::new(
            name.into(),
            PhysicalProperties {
                size: (0, 0).into(),
                subpixel: Subpixel::Unknown,
                make: "make".into(),
                model: "model".into(),
                serial_number: String::new(),
            },
        );
        output
            .user_data()
            .insert_if_missing(|| RefCell::new(OutputConfig::default()));
        output
    }

    fn mirroring(target: &str) -> OutputState {
        OutputState::Mirroring(target.into())
    }

    fn info(name: &str) -> OutputInfo {
        OutputInfo {
            connector: name.into(),
            make: "make".into(),
            model: "model".into(),
        }
    }

    #[test]
    fn new_external_output_mirrors_the_internal_panel() {
        let internal = output("eDP-1");
        let external = output("DP-1");
        let known = HashSet::from([info("eDP-1")]);

        mirror_new_outputs(&[internal.clone(), external.clone()], &known);

        assert_eq!(internal.config().enabled, OutputState::Enabled);
        assert_eq!(external.config().enabled, mirroring("eDP-1"));
        assert!(!extends_desktop(&external));
    }

    /// A display the user has configured before keeps whatever it had, even when
    /// another new output turns up alongside it.
    #[test]
    fn known_external_output_keeps_extending() {
        let internal = output("eDP-1");
        let known = output("DP-1");
        let new = output("DP-2");
        let stored = HashSet::from([info("eDP-1"), info("DP-1")]);

        mirror_new_outputs(&[internal.clone(), known.clone(), new.clone()], &stored);

        assert_eq!(known.config().enabled, OutputState::Enabled);
        assert_eq!(new.config().enabled, mirroring("eDP-1"));
    }

    /// Without an internal panel a previously configured display is the primary.
    #[test]
    fn new_output_mirrors_a_known_one_when_there_is_no_panel() {
        let known = output("DP-1");
        let new = output("HDMI-A-1");
        let stored = HashSet::from([info("DP-1")]);

        mirror_new_outputs(&[new.clone(), known.clone()], &stored);

        assert_eq!(known.config().enabled, OutputState::Enabled);
        assert_eq!(new.config().enabled, mirroring("DP-1"));
    }

    /// A first run with nothing but new displays has no primary to mirror onto.
    #[test]
    fn unknown_outputs_alone_extend() {
        let first = output("DP-1");
        let second = output("DP-2");

        mirror_new_outputs(&[first.clone(), second.clone()], &HashSet::new());

        assert_eq!(first.config().enabled, OutputState::Enabled);
        assert_eq!(second.config().enabled, OutputState::Enabled);
    }

    /// A disabled panel is no mirror target - it shows nothing to mirror.
    #[test]
    fn disabled_internal_panel_is_not_a_mirror_target() {
        let internal = output("eDP-1");
        internal.config_mut().enabled = OutputState::Disabled;
        let external = output("DP-1");
        let known = HashSet::from([info("eDP-1")]);

        mirror_new_outputs(&[internal.clone(), external.clone()], &known);

        assert_eq!(external.config().enabled, OutputState::Enabled);
    }
}
