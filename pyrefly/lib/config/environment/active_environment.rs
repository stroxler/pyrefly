/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::Path;
use std::path::PathBuf;

use crate::config::environment::venv;

/// Gets the first Python interpreter set in environment variables.
pub struct ActiveEnvironment {}

impl ActiveEnvironment {
    pub fn find() -> Option<PathBuf> {
        // TODO(connernilsen): don't perform upward search for `VIRTUAL_ENV` finding
        if let Ok(venv_path) = std::env::var("VIRTUAL_ENV")
            && let Some(env) = venv::find(Path::new(&venv_path))
        {
            return Some(env);
        }

        // TODO(connernilsen): also handle $CONDA_PREFIX

        None
    }
}
