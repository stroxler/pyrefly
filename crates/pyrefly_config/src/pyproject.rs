/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use serde::Deserialize;
use serde::Serialize;

use crate::config::ConfigFile;

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

    pub(crate) fn pyrefly(self) -> Option<ConfigFile> {
        self.tool.and_then(|t| t.pyrefly)
    }
}
