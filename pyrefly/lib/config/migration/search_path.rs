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

/// Configuration option for search path
pub struct SearchPath;

impl ConfigOptionMigrater for SearchPath {
    fn migrate_from_mypy(
        &self,
        mypy_cfg: &Ini,
        pyrefly_cfg: &mut ConfigFile,
    ) -> anyhow::Result<()> {
        // https://mypy.readthedocs.io/en/latest/config_file.html#import-discovery
        // mypy_path can only be set in the top level `[mypy]` global section
        let mypy_path = match mypy_cfg.get("mypy", "mypy_path") {
            Some(path) => path,
            None => return Err(anyhow::anyhow!("No mypy_path found in mypy config")),
        };

        let paths = utils::string_to_paths(&mypy_path);
        if paths.is_empty() {
            return Err(anyhow::anyhow!("Empty search paths found in mypy config"));
        }
        pyrefly_cfg.search_path_from_file = paths;
        Ok(())
    }
}
