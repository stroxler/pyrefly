/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashMap;
use std::path::PathBuf;

use configparser::ini::Ini;

use crate::config::error::ErrorDisplayConfig;
use crate::error::kind::Severity;

/// Convert a comma-separated string to a vector of strings
pub fn string_to_array(value: &Option<String>) -> Vec<String> {
    match value {
        Some(value) => value
            .split(',')
            .map(|x| x.trim().to_owned())
            .filter(|s| !s.is_empty())
            .collect(),
        _ => Vec::new(),
    }
}

/// Get a boolean value from the config, with a default value if not present
pub fn get_bool_or_default(config: &Ini, section: &str, key: &str) -> bool {
    config
        .getboolcoerce(section, key)
        .ok()
        .flatten()
        .unwrap_or_default()
}

/// Convert a colon or comma-separated string to a vector of PathBufs
pub fn string_to_paths(value: &str) -> Vec<PathBuf> {
    value
        .split([',', ':'])
        .map(|x| x.trim().to_owned())
        .filter(|x| !x.is_empty())
        .map(PathBuf::from)
        .collect()
}

/// Create an error config from disable and enable error codes
pub fn make_error_config(
    disables: Vec<String>,
    enables: Vec<String>,
) -> Option<ErrorDisplayConfig> {
    let mut errors = HashMap::new();
    for error_code in disables {
        errors.insert(error_code, Severity::Ignore);
    }
    // enable_error_code overrides disable_error_code
    for error_code in enables {
        errors.insert(error_code, Severity::Error);
    }
    crate::config::migration::mypy::code_to_kind(errors)
}
