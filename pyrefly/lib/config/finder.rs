/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::Path;
use std::path::PathBuf;

use anyhow::anyhow;
use anyhow::Context as _;
use path_absolutize::Absolutize;
use starlark_map::small_map::SmallMap;
use tracing::debug;
use tracing::info;

use crate::config::config::ConfigFile;
use crate::util::fs_upward_search::first_match;

pub fn get_open_source_config(file: &Path) -> anyhow::Result<ConfigFile> {
    ConfigFile::from_file(file, true).map_err(|err| {
        let file_str = file.display();
        anyhow!("Failed to parse configuration at {file_str}: {err}")
    })
}

fn get_implicit_config_path_from(path: &Path) -> anyhow::Result<PathBuf> {
    if let Some(search_result) = first_match(path, &ConfigFile::CONFIG_FILE_NAMES) {
        Ok(search_result.to_path_buf())
    } else {
        Err(anyhow!(
            "Cannot locate a config file using upward-searching heuristics from `{}`",
            path.display()
        ))
    }
}

pub fn get_implicit_config_for_project(
    override_config: impl Fn(ConfigFile) -> ConfigFile,
) -> ConfigFile {
    fn get_config_path() -> anyhow::Result<ConfigFile> {
        let current_dir = std::env::current_dir().context("cannot identify current dir")?;
        let config_path = get_implicit_config_path_from(&current_dir)?;
        info!("Using config found at {}", config_path.display());
        get_open_source_config(&config_path)
    }
    let config = get_config_path().unwrap_or_else(|err| {
        debug!("{err}. Default configuration will be used as fallback.");
        ConfigFile::default()
    });
    override_config(config)
}

pub fn get_implicit_config_for_file<'a>(
    override_config: &'a impl Fn(ConfigFile) -> ConfigFile,
) -> impl Fn(&Path) -> ConfigFile + 'a {
    let mut config_cache: SmallMap<PathBuf, ConfigFile> = SmallMap::new();
    fn get_implicit_config_path(path: &Path) -> anyhow::Result<PathBuf> {
        let parent_dir = path
            .parent()
            .with_context(|| format!("Path `{}` has no parent directory", path.display()))?
            .absolutize()
            .with_context(|| format!("Path `{}` cannot be absolutized", path.display()))?;
        get_implicit_config_path_from(parent_dir.as_ref())
    }
    let log_err = |err| {
        debug!("{err}. Default configuration will be used as fallback.");
    };

    let get_implicit_config = move |config_path: PathBuf| -> ConfigFile {
        if let Some(config) = config_cache.get(&config_path) {
            return config.clone();
        }
        let config = override_config(get_open_source_config(&config_path).unwrap_or_else(|err| {
            log_err(err);
            ConfigFile::default()
        }));
        debug!("Config for {} is: {}", config_path.display(), config);
        config_cache.insert(config_path, config.clone());
        config
    };
    move |path| {
        get_implicit_config_path(path).map_or_else(
            |err| {
                log_err(err);
                override_config(ConfigFile::default())
            },
            |config_path| {
                debug!(
                    "Config for {} found at {}",
                    path.display(),
                    config_path.display()
                );
                get_implicit_config.clone()(config_path)
            },
        )
    }
}
