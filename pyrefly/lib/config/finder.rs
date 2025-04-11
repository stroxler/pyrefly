/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt::Display;
use std::path::Path;
use std::path::PathBuf;

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

pub struct ConfigFinder<T> {
    loader: Box<dyn Fn(Option<&Path>) -> T>,
    cache: RwLock<SmallMap<Option<PathBuf>, T>>,
}

impl<T: Dupe + Display> ConfigFinder<T> {
    /// Create a new ConfigFinder with the given loader function.
    /// The loader function should return either the default config (`None`) or
    /// the config file at the given path (`Some(path)`).
    pub fn new(loader: impl Fn(Option<&Path>) -> T + 'static) -> Self {
        Self {
            loader: Box::new(loader),
            cache: RwLock::new(SmallMap::new()),
        }
    }

    fn get(&self, path: Option<PathBuf>) -> T {
        if let Some(config) = self.cache.read().get(&path) {
            return config.dupe();
        }
        let config = (self.loader)(path.as_deref());
        if let Some(config_path) = &path {
            debug!("Config for {} is: {}", config_path.display(), config);
        }
        // If there was a race condition, make sure we use whoever wrote first
        self.cache.write().entry(path).or_insert(config).dupe()
    }

    /// Get the config file given an explicit config file path.
    pub fn config_file(&self, config_path: &Path) -> T {
        self.get(Some(config_path.to_owned()))
    }

    /// Get the config file given a Python file.
    pub fn python_file(&self, path: &Path) -> T {
        fn get_implicit_config_path(path: &Path) -> anyhow::Result<PathBuf> {
            let parent_dir = path
                .parent()
                .with_context(|| format!("Path `{}` has no parent directory", path.display()))?
                .absolutize()
                .with_context(|| format!("Path `{}` cannot be absolutized", path.display()))?;
            get_implicit_config_path_from(parent_dir.as_ref())
        }
        match get_implicit_config_path(path) {
            Ok(config_path) => {
                debug!(
                    "Config for {} found at {}",
                    path.display(),
                    config_path.display()
                );
                self.get(Some(config_path))
            }
            Err(err) => {
                debug!("{err}. Default configuration will be used as fallback.");
                self.get(None)
            }
        }
    }
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
        ConfigFile::from_file(&config_path, true)
    }
    get_config_path().unwrap_or_else(|err| {
        debug!("{err}. Default configuration will be used as fallback.");
        ConfigFile::default()
    })
}
