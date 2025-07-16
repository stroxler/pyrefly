/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::str::FromStr;

use configparser::ini::Ini;
use pyrefly_python::sys_info::PythonVersion;

use crate::config::config::ConfigFile;
use crate::config::migration::config_option_migrater::ConfigOptionMigrater;

/// Configuration option for Python version
pub struct PythonVersionConfig;

impl ConfigOptionMigrater for PythonVersionConfig {
    fn migrate_from_mypy(
        &self,
        mypy_cfg: &Ini,
        pyrefly_cfg: &mut ConfigFile,
    ) -> anyhow::Result<()> {
        // https://mypy.readthedocs.io/en/latest/config_file.html#import-discovery
        // python_version can only be set in the top level `[mypy]` global section
        let python_version = mypy_cfg.get("mypy", "python_version");

        if python_version.is_none() {
            return Err(anyhow::anyhow!("No python_version found in mypy config"));
        }

        let version = python_version.unwrap();
        pyrefly_cfg.python_environment.python_version = PythonVersion::from_str(&version).ok();
        Ok(())
    }
}
