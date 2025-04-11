/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;

use anyhow::anyhow;
use anyhow::Context as _;
use dupe::Dupe;
use path_absolutize::Absolutize;
use starlark_map::small_map::SmallMap;
use tracing::debug;
use tracing::info;

use crate::config::config::ConfigFile;
use crate::util::fs_upward_search::first_match;
use crate::util::lock::RwLock;

pub fn get_open_source_config(file: &Path) -> anyhow::Result<ConfigFile> {
    ConfigFile::from_file(file, true)
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

pub fn get_implicit_config_for_project() -> ConfigFile {
    fn get_config_path() -> anyhow::Result<ConfigFile> {
        let current_dir = std::env::current_dir().context("cannot identify current dir")?;
        let config_path = get_implicit_config_path_from(&current_dir)?;
        info!("Using config found at {}", config_path.display());
        get_open_source_config(&config_path)
    }
    get_config_path().unwrap_or_else(|err| {
        debug!("{err}. Default configuration will be used as fallback.");
        ConfigFile::default()
    })
}

pub fn get_implicit_config_for_file(
    override_config: impl Fn(ConfigFile) -> ConfigFile,
) -> impl Fn(&Path) -> ConfigFile {
    let config_cache = RwLock::new(SmallMap::<PathBuf, ConfigFile>::new());
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

    let override_config = Arc::new(override_config);
    let override_config2 = override_config.dupe();
    let get_implicit_config = move |config_path: PathBuf| -> ConfigFile {
        if let Some(config) = config_cache.read().get(&config_path) {
            return config.clone();
        }
        let config = override_config(get_open_source_config(&config_path).unwrap_or_else(|err| {
            log_err(err);
            ConfigFile::default()
        }));
        debug!("Config for {} is: {}", config_path.display(), config);
        // If there was a race condition, make sure we use whoever wrote first
        config_cache
            .write()
            .entry(config_path)
            .or_insert(config)
            .clone()
    };
    move |path| match get_implicit_config_path(path) {
        Ok(config_path) => {
            debug!(
                "Config for {} found at {}",
                path.display(),
                config_path.display()
            );
            get_implicit_config(config_path)
        }
        Err(err) => {
            log_err(err);
            override_config2(ConfigFile::default())
        }
    }
}
