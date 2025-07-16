/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use configparser::ini::Ini;
use pyrefly_util::globs::Glob;

use crate::config::base::ConfigBase;
use crate::config::config::ConfigFile;
use crate::config::config::SubConfig;
use crate::config::error::ErrorDisplayConfig;
use crate::config::migration::config_option_migrater::ConfigOptionMigrater;
use crate::config::migration::utils;
/// Configuration option for sub-configs (per-module options)
pub struct SubConfigs;

impl ConfigOptionMigrater for SubConfigs {
    fn migrate_from_mypy(
        &self,
        mypy_cfg: &Ini,
        pyrefly_cfg: &mut ConfigFile,
    ) -> anyhow::Result<()> {
        let mut sub_configs: Vec<(String, ErrorDisplayConfig)> = vec![];

        // Check all sections for per-module options
        utils::visit_ini_sections(
            mypy_cfg,
            |section_name| section_name.starts_with("mypy-"),
            |section_name, ini| {
                if let Some(stripped) = section_name.strip_prefix("mypy-") {
                    // For subconfigs, the only config that needs to be extracted is enable/disable error codes.
                    let disable_error_code =
                        utils::string_to_array(&ini.get(section_name, "disable_error_code"));
                    let enable_error_code =
                        utils::string_to_array(&ini.get(section_name, "enable_error_code"));

                    if disable_error_code.is_empty() && enable_error_code.is_empty() {
                        return;
                    }

                    if let Some(error_config) =
                        utils::make_error_config(disable_error_code, enable_error_code)
                    {
                        sub_configs.push((stripped.to_owned(), error_config));
                    }
                }
            },
        );

        if sub_configs.is_empty() {
            return Err(anyhow::anyhow!("No sub configs found in mypy config"));
        }

        let sub_configs_vec = sub_configs
            .into_iter()
            .flat_map(|(section, errors)| {
                // Split the section headers into individual modules and pair them with the section's error config.
                // mypy uses module wildcards for its per-module sections, but we use globs.
                // A simple translation: turn `.` into `/` and `*` into `**`, e.g. `a.*.b` -> `a/**/b`.
                section
                    .split(",")
                    .map(|x| x.trim())
                    .filter(|x| !x.is_empty())
                    .map(|module| Glob::new(module.replace('.', "/").replace('*', "**")))
                    .collect::<Vec<_>>()
                    .into_iter()
                    .zip(std::iter::repeat(Some(errors)))
            })
            .map(|(matches, errors)| SubConfig {
                matches,
                settings: ConfigBase {
                    errors,
                    ..Default::default()
                },
            })
            .collect::<Vec<_>>();

        if sub_configs_vec.is_empty() {
            return Err(anyhow::anyhow!("No valid sub configs found in mypy config"));
        }
        pyrefly_cfg.sub_configs = sub_configs_vec;
        Ok(())
    }
}
