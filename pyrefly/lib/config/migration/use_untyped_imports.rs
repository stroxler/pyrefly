/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use configparser::ini::Ini;

use crate::config::config::ConfigFile;
use crate::config::migration::config_option_migrater::ConfigOptionMigrater;

/// Configuration option for using untyped imports
pub struct UseUntypedImports;

impl ConfigOptionMigrater for UseUntypedImports {
    fn migrate_from_mypy(
        &self,
        mypy_cfg: &Ini,
        pyrefly_cfg: &mut ConfigFile,
    ) -> anyhow::Result<()> {
        // follow_untyped_imports may be used as a global or per-module setting. As a per-module setting, it's used to
        // indicate that the module should be ignored if it's untyped.
        // Pyrefly's use_untyped_imports is only a global setting.
        // We handle this by *only* checking the for the global config.
        let value = mypy_cfg.getboolcoerce("mypy", "follow_untyped_imports");
        if !matches!(value, Ok(Some(_))) {
            return Err(anyhow::anyhow!(
                "No follow_untyped_imports found in mypy config"
            ));
        }

        pyrefly_cfg.use_untyped_imports = value.unwrap().unwrap();

        Ok(())
    }
}
