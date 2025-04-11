/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt::Display;
use std::mem;
use std::path::Path;
use std::path::PathBuf;
use std::sync::LazyLock;

use anyhow::anyhow;
use dupe::Dupe;
use path_absolutize::Absolutize;
use starlark_map::small_map::SmallMap;
use tracing::debug;
use tracing::info;

use crate::config::config::ConfigFile;
use crate::util::fs_upward_search::first_match;
use crate::util::lock::RwLock;

pub struct ConfigFinder<T> {
    default: LazyLock<T, Box<dyn FnOnce() -> T + 'static>>,
    load: Box<dyn Fn(&Path) -> anyhow::Result<T>>,
    state: RwLock<ConfigFinderState<T>>,
}

struct ConfigFinderState<T> {
    /// A cache mapping the path of a configuration file to the result.
    cache: SmallMap<PathBuf, T>,
    /// The errors that have occurred when loading.
    errors: Vec<anyhow::Error>,
}

impl<T> Default for ConfigFinderState<T> {
    fn default() -> Self {
        Self {
            cache: SmallMap::new(),
            errors: Vec::new(),
        }
    }
}

impl<T: Dupe + Display> ConfigFinder<T> {
    /// Create a new ConfigFinder a way to produce a default, and to load a given file.
    pub fn new(
        default: impl FnOnce() -> T + 'static,
        load: impl Fn(&Path) -> anyhow::Result<T> + 'static,
    ) -> Self {
        Self {
            default: LazyLock::new(Box::new(default)),
            load: Box::new(load),
            state: RwLock::new(ConfigFinderState::default()),
        }
    }

    /// Collect all the current errors that have been produced, and clear them.
    pub fn errors(&self) -> Vec<anyhow::Error> {
        mem::take(&mut self.state.write().errors)
    }

    fn get(&self, path: &Path) -> T {
        if let Some(config) = self.state.read().cache.get(path) {
            return config.dupe();
        }
        match (self.load)(path) {
            Ok(config) => {
                debug!("Config for {} is: {}", path.display(), config);
                // If there was a race condition, make sure we use whoever wrote first
                self.state
                    .write()
                    .cache
                    .entry(path.to_owned())
                    .or_insert(config)
                    .dupe()
            }
            Err(err) => {
                self.state.write().errors.push(err);
                self.default.dupe()
            }
        }
    }

    /// Get the config file associated with a directory.
    pub fn directory(&self, dir: &Path) -> T {
        match get_implicit_config_path_from(dir) {
            Ok(config_path) => {
                info!("Using config found at {}", config_path.display());
                self.get(&config_path)
            }
            Err(_) => self.default.dupe(),
        }
    }

    /// Get the config file given a Python file.
    pub fn python_file(&self, path: &Path) -> T {
        let absolute = path.absolutize().ok();
        let parent = absolute.as_ref().and_then(|x| x.parent());
        match parent {
            Some(parent) => self.directory(parent),
            None => self.default.dupe(),
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
