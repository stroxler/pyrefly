/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::VecDeque;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;

use dupe::Dupe as _;
use pyrefly_python::module_name::ModuleName;
use pyrefly_python::module_path::ModulePath;
use pyrefly_python::sys_info::SysInfo;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;

use crate::buck::query::Include;
use crate::buck::query::PythonLibraryManifest;
use crate::buck::query::TargetManifestDatabase;
use crate::buck::query::query_source_db;
use crate::handle::Handle;
use crate::source_db::SourceDatabase;
use crate::source_db::Target;

#[derive(Debug, PartialEq, Eq)]
pub struct BuckSourceDatabase {
    /// The mapping from targets to their manifests, including sources, dependencies,
    /// and metadata. This is the source of truth.
    db: SmallMap<Target, PythonLibraryManifest>,
    /// An index for doing fast lookups of a path to its owning target.
    /// Invariants:
    /// - if a path exists in `path_lookup`, its target must exist in `db`.
    /// - if a path exists in `path_lookup`, its target's `srcs` must have a
    ///   module name with `path` as a module path.
    path_lookup: SmallMap<Arc<PathBuf>, Target>,
    /// The set of items the sourcedb has been queried for. Not all of the targets
    /// or files listed here will necessarily appear in the sourcedb, for example,
    /// if the given target does not exist, or if the file is not tracked by Buck.
    includes: SmallSet<Include>,
    /// The directory that will be passed into the sourcedb query shell-out. Should
    /// be the same as the directory containing the config this sourcedb is a part of.
    cwd: PathBuf,
}

impl BuckSourceDatabase {
    pub fn new(cwd: PathBuf) -> Self {
        BuckSourceDatabase {
            cwd,
            db: SmallMap::new(),
            path_lookup: SmallMap::new(),
            includes: SmallSet::new(),
        }
    }

    fn update_with_target_manifest(&mut self, raw_db: TargetManifestDatabase) -> bool {
        let new_db = raw_db.produce_map();
        if new_db == self.db {
            return false;
        }
        self.db = new_db;
        self.path_lookup = SmallMap::new();
        for (target, manifest) in self.db.iter() {
            for source in manifest.srcs.values().flatten() {
                if let Some(old_target) = self.path_lookup.get_mut(&**source) {
                    let new_target = (&*old_target).min(target);
                    *old_target = new_target.dupe();
                } else {
                    self.path_lookup.insert(source.clone(), target.dupe());
                }
            }
        }
        true
    }
}

impl SourceDatabase for BuckSourceDatabase {
    fn modules_to_check(&self) -> Vec<crate::handle::Handle> {
        // TODO(connernilsen): implement modules_to_check
        vec![]
    }

    fn lookup(&self, module: &ModuleName, origin: Option<&Path>) -> Option<ModulePath> {
        let origin = origin?;
        let start_target = self.path_lookup.get(&origin.to_path_buf())?;
        let mut queue = VecDeque::new();
        let mut visited = SmallSet::new();
        queue.push_front(start_target);

        while let Some(current_target) = queue.pop_front() {
            if visited.contains(current_target) {
                continue;
            }
            visited.insert(current_target);
            let Some(manifest) = self.db.get(current_target) else {
                continue;
            };

            if let Some(paths) = manifest.srcs.get(module) {
                // TODO(connernilsen): for now, just take the first item on the path, but we
                // should respect preferences at some point
                return Some(ModulePath::filesystem(paths.first().to_path_buf()));
            }

            manifest.deps.iter().for_each(|t| queue.push_back(t));
        }

        None
    }

    fn handle_from_module_path(&self, _module_path: ModulePath) -> Handle {
        // TODO(connernilsen): implement handles_from_module_path
        Handle::new(
            ModuleName::unknown(),
            ModulePath::memory(PathBuf::new()),
            SysInfo::default(),
        )
    }

    fn requery_source_db(&mut self, files: SmallSet<PathBuf>) -> anyhow::Result<bool> {
        let new_includes = files.into_iter().map(Include::path).collect();
        if self.includes == new_includes {
            return Ok(false);
        }
        self.includes = new_includes;
        let raw_db = query_source_db(self.includes.iter(), &self.cwd)?;
        Ok(self.update_with_target_manifest(raw_db))
    }
}
