/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ops::Deref;
use std::path::PathBuf;

use dupe::Dupe as _;
use pyrefly_python::module_name::ModuleName;
use pyrefly_python::sys_info::PythonPlatform;
use pyrefly_python::sys_info::PythonVersion;
use pyrefly_python::sys_info::SysInfo;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;
use vec1::Vec1;

use crate::source_db::Target;

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct TargetManifest {
    deps: SmallSet<Target>,
    srcs: SmallMap<ModuleName, Vec1<PathBuf>>,
    python_version: PythonVersion,
    python_platform: PythonPlatform,
}

impl TargetManifest {
    #[expect(unused)]
    pub fn new(
        srcs: SmallMap<ModuleName, Vec1<PathBuf>>,
        deps: SmallSet<Target>,
        sys_info: SysInfo,
    ) -> Self {
        Self {
            srcs,
            deps,
            python_version: sys_info.version().dupe(),
            python_platform: sys_info.platform().clone(),
        }
    }

    pub(crate) fn iter_srcs<'a>(
        &'a self,
    ) -> impl Iterator<Item = (&'a ModuleName, &'a Vec1<PathBuf>)> + 'a {
        self.srcs.iter()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct TargetManifestDatabase {
    db: SmallMap<Target, TargetManifest>,
    root: PathBuf,
}

impl Deref for TargetManifestDatabase {
    type Target = SmallMap<Target, TargetManifest>;

    fn deref(&self) -> &Self::Target {
        &self.db
    }
}

impl TargetManifestDatabase {
    #[expect(unused)]
    pub fn new(db: SmallMap<Target, TargetManifest>, root: PathBuf) -> Self {
        Self { db, root }
    }

    #[allow(unused)]
    pub(crate) fn iter_srcs<'a>(
        &'a self,
    ) -> impl Iterator<Item = (&'a Target, &'a ModuleName, Vec1<PathBuf>)> + 'a {
        let root = &self.root;
        self.db.iter().flat_map(move |(target, manifest)| {
            manifest
                .iter_srcs()
                .map(move |(module, paths)| (target, module, paths.mapped_ref(|p| root.join(p))))
        })
    }
}
