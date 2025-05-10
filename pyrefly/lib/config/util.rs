/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use serde::Deserialize;
use serde::Serialize;
use toml::Table;

use crate::config::config::ConfigFile;

#[derive(Debug, Deserialize, Serialize, Clone, Default)]
#[serde(transparent)]
pub struct ExtraConfigs(pub Table);

// `Value` types in `Table` might not be `Eq`, but we don't actually care about that w.r.t. `ConfigFile`
impl Eq for ExtraConfigs {}

impl PartialEq for ExtraConfigs {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

/// Wrapper used to (de)serialize pyrefly configs from pyproject.toml files.
#[derive(Debug, Serialize, Deserialize)]
struct Tool {
    #[serde(default)]
    pyrefly: Option<ConfigFile>,
}

/// Wrapper used to (de)serialize pyrefly configs from pyproject.toml files.
#[derive(Debug, Serialize, Deserialize)]
pub struct PyProject {
    #[serde(default)]
    tool: Option<Tool>,
}

impl PyProject {
    /// Wrap the given ConfigFile in a `PyProject { Tool { ... }}`
    pub fn new(cfg: ConfigFile) -> Self {
        Self {
            tool: Some(Tool { pyrefly: Some(cfg) }),
        }
    }

    pub fn pyrefly(self) -> Option<ConfigFile> {
        self.tool.and_then(|t| t.pyrefly)
    }
}

/// Used in serde's skip_serializing_if attribute to skip serializing a boolean field that defaults to true.
#[allow(clippy::trivially_copy_pass_by_ref)]
pub fn skip_default_true(v: &bool) -> bool {
    *v
}

pub fn none_or_empty<T>(v: &Option<Vec<T>>) -> bool {
    v.as_ref().is_none_or(|v| v.is_empty())
}
