/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ops::Deref;
use std::path::PathBuf;

use pyrefly_python::module_name::ModuleName;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;
use vec1::Vec1;

use crate::source_db::Target;

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct TargetManifest {
    deps: SmallSet<Target>,
    srcs: SmallMap<ModuleName, Vec1<PathBuf>>,
}

impl TargetManifest {
    #[expect(unused)]
    pub fn new(srcs: SmallMap<ModuleName, Vec1<PathBuf>>, deps: SmallSet<Target>) -> Self {
        Self { srcs, deps }
    }

    pub(crate) fn iter_srcs<'a>(
        &'a self,
    ) -> impl Iterator<Item = (&'a ModuleName, &'a Vec1<PathBuf>)> + 'a {
        self.srcs.iter()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct TargetManifestDatabase(SmallMap<Target, TargetManifest>);

impl Deref for TargetManifestDatabase {
    type Target = SmallMap<Target, TargetManifest>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl TargetManifestDatabase {
    #[expect(unused)]
    pub fn new(database: SmallMap<Target, TargetManifest>) -> Self {
        Self(database)
    }

    #[allow(unused)]
    pub(crate) fn iter_srcs<'a>(
        &'a self,
    ) -> impl Iterator<Item = (&'a Target, &'a ModuleName, &'a Vec1<PathBuf>)> + 'a {
        self.0.iter().flat_map(|(target, manifest)| {
            manifest
                .iter_srcs()
                .map(move |(module, paths)| (target, module, paths))
        })
    }
}
