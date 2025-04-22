/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ffi::OsString;
use std::fmt::Debug;
use std::mem;
use std::path::Path;
use std::sync::Arc;

use dupe::Dupe;
use path_absolutize::Absolutize;

use crate::config::config::ConfigFile;
use crate::util::arc_id::ArcId;
use crate::util::lock::Mutex;
use crate::util::prelude::SliceExt;
use crate::util::upward_search::UpwardSearch;

/// A way to find a config file given a directory or Python file.
/// Uses a lot of caching.
pub struct ConfigFinder<T = ArcId<ConfigFile>> {
    /// The cached state, with previously found entries.
    search: UpwardSearch<Option<T>>,
    /// The errors that have occurred when loading.
    errors: Arc<Mutex<Vec<anyhow::Error>>>,

    /// Function to run before checking the state. If this returns a value, it is _not_ cached.
    /// If this returns anything other than `Ok`, the rest of the functions are used.
    before: Box<dyn Fn(&Path) -> anyhow::Result<Option<T>> + Send + Sync>,
    /// If there is no config file, or loading it fails, use this fallback.
    fallback: Box<dyn Fn() -> T + Send + Sync>,
}

impl<T: Dupe + Debug + Send + Sync + 'static> ConfigFinder<T> {
    /// Create a new ConfigFinder a way to load a config file, and a default if that errors or there is no file.
    pub fn new(
        load: impl Fn(&Path) -> anyhow::Result<T> + Send + Sync + 'static,
        fallback: impl Fn() -> T + Send + Sync + 'static,
    ) -> Self {
        Self::new_custom(|_| Ok(None), load, fallback)
    }

    /// Create a new ConfigFinder, but with a custom way to produce a result from a Python file.
    /// If the `before` function fails to produce a config, then the other methods will be used.
    /// The `before` function is not cached in any way.
    pub fn new_custom(
        before: impl Fn(&Path) -> anyhow::Result<Option<T>> + Send + Sync + 'static,
        load: impl Fn(&Path) -> anyhow::Result<T> + Send + Sync + 'static,
        fallback: impl Fn() -> T + Send + Sync + 'static,
    ) -> Self {
        let errors = Arc::new(Mutex::new(Vec::new()));
        let errors2 = errors.dupe();

        Self {
            search: UpwardSearch::new(
                ConfigFile::CONFIG_FILE_NAMES.map(OsString::from),
                move |x| match load(x) {
                    Ok(v) => Some(v),
                    Err(e) => {
                        errors2.lock().push(e);
                        None
                    }
                },
            ),
            errors,
            before: Box::new(before),
            fallback: Box::new(fallback),
        }
    }

    /// Create a new ConfigFinder that always returns the same constant.
    pub fn new_constant(constant: T) -> Self {
        let c1 = constant.dupe();
        let c2 = constant.dupe();
        let c3 = constant;

        Self::new_custom(
            move |_| Ok(Some(c1.dupe())),
            move |_| Ok(c3.dupe()),
            move || c2.dupe(),
        )
    }

    /// Collect all the current errors that have been produced, and clear them.
    pub fn errors(&self) -> Vec<anyhow::Error> {
        mem::take(&mut self.errors.lock())
    }

    /// Get the config file associated with a directory.
    pub fn directory(&self, dir: &Path) -> T {
        self.search
            .directory(dir)
            .flatten()
            .unwrap_or_else(&self.fallback)
    }

    /// Get the config file given a Python file.
    pub fn python_file(&self, path: &Path) -> T {
        match (self.before)(path) {
            Ok(Some(x)) => return x,
            Ok(None) => {}
            Err(e) => {
                self.errors.lock().push(e);
            }
        }

        let absolute = path.absolutize().ok();
        let parent = absolute.as_ref().and_then(|x| x.parent());
        match parent {
            Some(parent) => self
                .search
                .directory_absolute(parent)
                .flatten()
                .unwrap_or_else(&self.fallback),
            None => (self.fallback)(),
        }
    }
}
