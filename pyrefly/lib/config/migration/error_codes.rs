/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use configparser::ini::Ini;

use crate::config::config::ConfigFile;
use crate::config::migration::config_option_migrater::ConfigOptionMigrater;
use crate::config::migration::utils;

/// Configuration option for error codes
pub struct ErrorCodes;

impl ConfigOptionMigrater for ErrorCodes {
    fn migrate_from_mypy(
        &self,
        mypy_cfg: &Ini,
        pyrefly_cfg: &mut ConfigFile,
    ) -> anyhow::Result<()> {
        // Get disable_error_code and enable_error_code from the mypy.ini file
        let disable_error_code =
            utils::string_to_array(&mypy_cfg.get("mypy", "disable_error_code"));
        let enable_error_code = utils::string_to_array(&mypy_cfg.get("mypy", "enable_error_code"));

        let error_config = utils::make_error_config(disable_error_code, enable_error_code)
            .ok_or_else(|| anyhow::anyhow!("Failed to create error config"))?;
        pyrefly_cfg.root.errors = Some(error_config);
        Ok(())
    }
}
