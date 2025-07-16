/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use configparser::ini::Ini;
use pyrefly_util::globs::Globs;

use crate::config::config::ConfigFile;
use crate::config::migration::config_option_migrater::ConfigOptionMigrater;
use crate::config::migration::utils;

/// Configuration option for project includes (files, packages, modules)
pub struct ProjectIncludes;

impl ConfigOptionMigrater for ProjectIncludes {
    fn migrate_from_mypy(
        &self,
        mypy_cfg: &Ini,
        pyrefly_cfg: &mut ConfigFile,
    ) -> anyhow::Result<()> {
        // https://mypy.readthedocs.io/en/latest/config_file.html#import-discovery
        // files, packages, modules can only be set in the top level `[mypy]` global section
        let files = utils::string_to_array(&mypy_cfg.get("mypy", "files"));
        let packages = utils::string_to_array(&mypy_cfg.get("mypy", "packages"));
        let modules = utils::string_to_array(&mypy_cfg.get("mypy", "modules"));

        let includes: Vec<String> = [files, packages, modules].into_iter().flatten().collect();

        if includes.is_empty() {
            return Err(anyhow::anyhow!("No project includes found in mypy config"));
        }

        pyrefly_cfg.project_includes = Globs::new(includes);
        Ok(())
    }
}
