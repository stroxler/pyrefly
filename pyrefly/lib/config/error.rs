/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashMap;

use serde::Deserialize;
use serde::Serialize;

use crate::error::kind::ErrorKind;
use crate::module::module_path::ModulePath;

/// Represents overrides for errors to emit when collecting/printing errors.
/// The boolean in the map represents whether the error is enabled or disabled
/// (true = show error, false = don't show error).
/// Not all error kinds are required to be defined in this map. Any that are missing
/// will be treated as `<error-kind> = true`.
#[derive(Debug, PartialEq, Eq, Deserialize, Serialize, Clone, Default)]
#[serde(transparent)]
pub struct ErrorDisplayConfig(HashMap<ErrorKind, bool>);

impl ErrorDisplayConfig {
    pub fn new(config: HashMap<ErrorKind, bool>) -> Self {
        Self(config)
    }

    /// Gets whether the given `ErrorKind` is enabled. If the value isn't
    /// found, then assume it should be enabled.
    pub fn is_enabled(&self, kind: ErrorKind) -> bool {
        self.0.get(&kind) != Some(&false)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct ErrorConfig {
    pub display_config: ErrorDisplayConfig,
    pub ignore_errors_in_generated_code: bool,
}

impl ErrorConfig {
    pub fn new(display_config: ErrorDisplayConfig, ignore_errors_in_generated_code: bool) -> Self {
        Self {
            display_config,
            ignore_errors_in_generated_code,
        }
    }
}

/// Represents a collection of `ErrorConfig`s keyed on the `ModulePath` of the file.
/// Internal detail: the `ErrorConfig` in `default_config` is an `ErrorConfig::default()`,
/// which is used in the `ErrorConfigs::get()` function when no config is found, so that we can
/// return a reference without dropping the original immediately after.
#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct ErrorConfigs {
    overrides: HashMap<ModulePath, ErrorConfig>,
    default_config: ErrorConfig,
}

impl ErrorConfigs {
    pub fn new(overrides: HashMap<ModulePath, ErrorConfig>) -> Self {
        Self {
            overrides,
            default_config: ErrorConfig::default(),
        }
    }

    /// Gets a reference to the `ErrorConfig` for the given path, or returns a reference to
    /// the 'default' error config if none could be found.
    pub fn get(&self, path: &ModulePath) -> &ErrorConfig {
        self.overrides.get(path).unwrap_or(&self.default_config)
    }
}
