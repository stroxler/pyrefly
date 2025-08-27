/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::PathBuf;

use pyrefly_python::module_name::ModuleName;
use pyrefly_python::module_path::ModulePath;
use starlark_map::small_map::SmallMap;
use vec1::Vec1;

#[derive(Debug, PartialEq, Eq)]
pub struct BuckSourceDatabase {
    pub(crate) sources: SmallMap<ModuleName, Vec1<PathBuf>>,
    pub(crate) dependencies: SmallMap<ModuleName, Vec1<PathBuf>>,
}

impl BuckSourceDatabase {
    pub fn modules_to_check(&self) -> Vec<(ModuleName, PathBuf)> {
        self.sources
            .iter()
            .flat_map(|(name, paths)| paths.iter().map(|path| (*name, path.clone())))
            .collect()
    }

    pub fn list(&self) -> SmallMap<ModuleName, ModulePath> {
        // Iterate the sources second so if there are any conflicts the source wins.
        self.dependencies
            .iter()
            .chain(self.sources.iter())
            .map(|(name, paths)| (*name, ModulePath::filesystem(paths.first().clone())))
            .collect()
    }
}
