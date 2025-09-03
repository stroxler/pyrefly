/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ops::Deref;
use std::ops::DerefMut;
use std::path::PathBuf;

use dupe::Dupe as _;
use pyrefly_python::module_name::ModuleName;
use pyrefly_python::module_path::ModulePath;
use starlark_map::small_map::SmallMap;

use crate::handle::Handle;
use crate::source_db::SourceDatabase;

/// A simple [`SourceDatabase`] that can be used for easy setup and testing.
#[derive(Debug, PartialEq, Eq)]
pub struct MapDatabase(SmallMap<ModuleName, ModulePath>);

impl MapDatabase {
    pub fn new() -> Self {
        Self(SmallMap::new())
    }
}

impl Deref for MapDatabase {
    type Target = SmallMap<ModuleName, ModulePath>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for MapDatabase {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl SourceDatabase for MapDatabase {
    fn modules_to_check(&self) -> Vec<(ModuleName, PathBuf)> {
        self.0
            .iter()
            .map(|(name, path)| (name.dupe(), path.as_path().to_path_buf()))
            .collect()
    }

    fn list(&self) -> SmallMap<ModuleName, ModulePath> {
        self.0.clone()
    }

    fn lookup(&self, module: &ModuleName, _: Option<&Handle>) -> Option<ModulePath> {
        self.0.get(module).cloned()
    }
}
