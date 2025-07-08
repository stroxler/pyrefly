/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::PathBuf;

use serde::Deserialize;
use serde::Serialize;

use crate::config::util::ConfigOrigin;

#[derive(Debug, PartialEq, Eq, Deserialize, Serialize, Clone, Default)]
#[serde(rename_all = "kebab-case")]
pub struct Interpreters {
    // TODO(connernilsen): make this mutually exclusive with venv/conda env
    /// The python executable that will be queried for `python_version`,
    /// `python_platform`, or `site_package_path` if any of the values are missing.
    #[serde(
                default,
                skip_serializing_if = "ConfigOrigin::should_skip_serializing_option",
                // TODO(connernilsen): DON'T COPY THIS TO NEW FIELDS. This is a temporary
                // alias while we migrate existing fields from snake case to kebab case.
                alias = "python_interpreter"
            )]
    pub python_interpreter: Option<ConfigOrigin<PathBuf>>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub conda_environment: Option<ConfigOrigin<String>>,

    /// Should we do any querying of an interpreter?
    #[serde(
        default,
        skip_serializing_if = "crate::config::util::skip_default_false"
    )]
    pub skip_interpreter_query: bool,
}
