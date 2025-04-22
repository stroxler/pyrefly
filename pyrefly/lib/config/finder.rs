/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt::Debug;
use std::mem;
use std::path::Path;
use std::path::PathBuf;

use dupe::Dupe;
use path_absolutize::Absolutize;
use starlark_map::small_map::Entry;
use starlark_map::small_map::SmallMap;

use crate::config::config::ConfigFile;
use crate::util::arc_id::ArcId;
use crate::util::lock::RwLock;

/// A way to find a config file given a directory or Python file.
/// Uses a lot of caching.
pub struct ConfigFinder<T = ArcId<ConfigFile>> {
    /// The cached state, with previously found entries.
    state: RwLock<ConfigFinderState<T>>,

    /// Function to run before checking the state. If this returns a value, it is _not_ cached.
    /// If this returns anything other than `Ok`, the rest of the functions are used.
    before: Box<dyn Fn(&Path) -> anyhow::Result<Option<T>> + Send + Sync>,
    /// Given a config file that exists on disk, load it.
    load: Box<dyn Fn(&Path) -> anyhow::Result<T> + Send + Sync>,
    /// If there is no config file, or loading it fails, use this fallback.
    fallback: Box<dyn Fn() -> T + Send + Sync>,
}

struct ConfigFinderState<T> {
    /// A cache mapping a directory to a result.
    /// Note that if there is no result at `foo/bar`, but there is at `foo`,
    /// then `foo/bar` will gain an entry pointing at the same value.
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

impl<T: Dupe + Debug> ConfigFinder<T> {
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
        Self {
            before: Box::new(before),
            load: Box::new(load),
            fallback: Box::new(fallback),
            state: RwLock::new(ConfigFinderState::default()),
        }
    }

    /// Create a new ConfigFinder that always returns the same constant.
    pub fn new_constant(constant: T) -> Self
    where
        T: Send + Sync + 'static,
    {
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
        mem::take(&mut self.state.write().errors)
    }

    fn directory_absolute(&self, dir: &Path) -> T {
        // This function iterates through ancestors 3 times, to ensure we don't hold the lock while doing IO.
        // 1. Find the nearest result in the cache (read lock).
        // 2. Find the nearest config on disk and load it (no lock, doing IO).
        // 3. Fill in all the results (write lock).
        const FULL_PATH: usize = 100; // If we want to hit everything in the path

        let mut cache_answer = None; // The (index, result) further up
        let mut found_answer = None;

        let lock = self.state.read();
        for (i, x) in dir.ancestors().enumerate() {
            if let Some(res) = lock.cache.get(x) {
                if i == 0 {
                    // We found a perfect hit
                    return res.dupe();
                } else {
                    cache_answer = Some((i, res.dupe()));
                    break;
                }
            }
        }
        drop(lock);

        // We didn't find a perfect hit, so now search the actual path
        let mut buffer = dir.to_owned();
        'outer: for i in 0..cache_answer.as_ref().map_or(FULL_PATH, |x| x.0) {
            for stem in ConfigFile::CONFIG_FILE_NAMES {
                buffer.push(stem);
                if buffer.exists() {
                    let c = match (self.load)(&buffer) {
                        Ok(v) => v,
                        Err(e) => {
                            self.state.write().errors.push(e);
                            (self.fallback)()
                        }
                    };
                    found_answer = Some((i + 1, c));
                    break 'outer;
                }
                buffer.pop();
            }
            if !buffer.pop() {
                break;
            }
        }

        let (applicable, result) = found_answer
            .or(cache_answer)
            .unwrap_or_else(|| (FULL_PATH, (self.fallback)()));

        let mut lock = self.state.write();
        for (i, x) in dir.ancestors().take(applicable).enumerate() {
            match lock.cache.entry(x.to_owned()) {
                Entry::Occupied(e) => {
                    if i == 0 {
                        // Race condition and someone else won on the leaf, so use their result
                        return e.get().dupe();
                    } else {
                        // Someone else filled in some of our prefix, so ours just applies further down
                        return result;
                    }
                }
                Entry::Vacant(e) => {
                    e.insert(result.dupe());
                }
            }
        }
        result
    }

    /// Get the config file associated with a directory.
    pub fn directory(&self, dir: &Path) -> T {
        self.directory_absolute(dir.absolutize().as_deref().unwrap_or(dir))
    }

    /// Get the config file given a Python file.
    pub fn python_file(&self, path: &Path) -> T {
        match (self.before)(path) {
            Ok(Some(x)) => return x,
            Ok(None) => {}
            Err(e) => {
                self.state.write().errors.push(e);
            }
        }

        let absolute = path.absolutize().ok();
        let parent = absolute.as_ref().and_then(|x| x.parent());
        match parent {
            Some(parent) => self.directory_absolute(parent),
            None => (self.fallback)(),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::sync::Arc;

    use tempfile::TempDir;

    use super::*;
    use crate::util::lock::Mutex;
    use crate::util::prelude::SliceExt;

    const DEFAULT: &str = "__default__";

    /// Given the files in a directory, and a series of questions, we expect these config files to be loaded
    fn assert(files: &[&str], ask: &[&str], expect: &[&str]) {
        let root = TempDir::new().unwrap();
        for file in files {
            let file_path = root.path().join(file);
            if let Some(parent) = file_path.parent() {
                fs::create_dir_all(parent).unwrap();
            }
            fs::write(&file_path, "").unwrap();
        }

        let loaded = Arc::new(Mutex::new(Vec::new()));
        let loaded2 = loaded.dupe();
        let loaded3 = loaded.dupe();
        let root_path = root.path().to_string_lossy().into_owned();
        let finder = ConfigFinder::new(
            move |file| {
                if let Some(file) = file.to_string_lossy().strip_prefix(&root_path) {
                    let mut file = file.chars();
                    file.next();
                    loaded3.lock().push(file.as_str().to_owned());
                }
                Ok(())
            },
            move || loaded2.lock().push(DEFAULT.to_owned()),
        );

        for a in ask {
            finder.python_file(&root.path().join(a));
        }

        let asked = mem::take(&mut *loaded.lock());
        let asked = asked.map(|x| x.replace('\\', "/"));
        let asked = asked.map(|x| x.as_str());
        assert_eq!(asked, expect);
    }

    #[test]
    fn test_config_finder() {
        assert(
            &["hello/pyrefly.toml"],
            &["hello/file.py", "hello/file2.py", "root.py"],
            &["hello/pyrefly.toml", DEFAULT],
        );
        assert(
            &["foo/pyrefly.toml", "pyrefly.toml"],
            &[
                "foo/file.py",
                "bar/file.py",
                "foo/bar/file.py",
                "foo/bar/file2.py",
                "bar/magic/file.py",
            ],
            &["foo/pyrefly.toml", "pyrefly.toml"],
        );
    }
}
