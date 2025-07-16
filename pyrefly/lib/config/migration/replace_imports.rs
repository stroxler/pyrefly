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
use crate::module::wildcard::ModuleWildcard;

/// Configuration option for replacing imports with Any
pub struct ReplaceImports;

impl ConfigOptionMigrater for ReplaceImports {
    fn migrate_from_mypy(
        &self,
        mypy_cfg: &Ini,
        pyrefly_cfg: &mut ConfigFile,
    ) -> anyhow::Result<()> {
        let mut replace_imports: Vec<String> = Vec::new();

        // Check all sections for ignore_missing_imports or follow_imports=skip
        for section_name in &mypy_cfg.sections() {
            if !section_name.starts_with("mypy-") {
                continue;
            }

            if utils::get_bool_or_default(mypy_cfg, section_name, "ignore_missing_imports")
                || mypy_cfg
                    .get(section_name, "follow_imports")
                    .is_some_and(|val| val == "skip")
            {
                replace_imports.push(section_name.to_owned());
            }
        }

        if replace_imports.is_empty() {
            return Err(anyhow::anyhow!("No replace imports found in mypy config"));
        }

        // Convert to ModuleWildcard objects
        let result: Vec<ModuleWildcard> = replace_imports
            .into_iter()
            .flat_map(|x| {
                if let Some(stripped) = x.strip_prefix("mypy-") {
                    stripped
                        .split(",")
                        .filter(|x| !x.is_empty())
                        .map(|x| ModuleWildcard::new(x).ok())
                        .collect()
                } else {
                    vec![ModuleWildcard::new(&x).ok()]
                }
            })
            .flatten()
            .collect();

        pyrefly_cfg.root.replace_imports_with_any = Some(result);
        Ok(())
    }
}
