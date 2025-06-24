/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::Path;
use std::path::PathBuf;

use crate::config::environment::finder::walk_interpreter;

const SEARCH_DEPTH: usize = 2;
pub const ENV_VAR: &str = "CONDA_PREFIX";

pub fn find(env_path: &Path) -> Option<PathBuf> {
    walk_interpreter(env_path, SEARCH_DEPTH).next()
}
