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
use std::sync::Arc;
use std::sync::LazyLock;

use dupe::Dupe;
use path_absolutize::Absolutize;
use starlark_map::small_map::Entry;
use starlark_map::small_map::SmallMap;

use crate::config::config::ConfigFile;
use crate::util::lock::RwLock;

pub struct ConfigFinder<T = Arc<ConfigFile>> {
    default: LazyLock<T, Box<dyn FnOnce() -> T + 'static>>,
    load: Box<dyn Fn(&Path) -> anyhow::Result<T>>,
    state: RwLock<ConfigFinderState<T>>,
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
                            self.default.dupe()
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
            .unwrap_or_else(|| (FULL_PATH, self.default.dupe()));

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
        let absolute = path.absolutize().ok();
        let parent = absolute.as_ref().and_then(|x| x.parent());
        match parent {
            Some(parent) => self.directory(parent),
            None => self.default.dupe(),
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
            move || loaded2.lock().push(DEFAULT.to_owned()),
            move |file| {
                if let Some(file) = file.to_string_lossy().strip_prefix(&root_path) {
                    let mut file = file.chars();
                    file.next();
                    loaded3.lock().push(file.as_str().to_owned());
                }
                Ok(())
            },
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
