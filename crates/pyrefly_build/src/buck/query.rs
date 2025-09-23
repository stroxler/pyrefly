/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ffi::OsStr;
use std::ops::Deref;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

use anyhow::Context as _;
use dupe::Dupe as _;
use pyrefly_python::module_name::ModuleName;
use pyrefly_python::sys_info::SysInfo;
use serde::Deserialize;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;
use vec1::Vec1;

use crate::source_db::Target;

/// An enum representing something that has been included by the build system, and
/// which the build system should query for when building the sourcedb.
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Include {
    #[expect(unused)]
    Target(Target),
    Path(PathBuf),
}

impl Include {
    fn to_bxl_args(this: &Self) -> impl Iterator<Item = &OsStr> {
        match &this {
            Include::Target(target) => [OsStr::new("--target"), target.to_os_str()].into_iter(),
            Include::Path(path) => [OsStr::new("--file"), path.as_os_str()].into_iter(),
        }
    }
}

#[expect(unused)]
pub fn query_source_db<'a>(
    files: impl Iterator<Item = &'a Include>,
    cwd: &Path,
) -> anyhow::Result<TargetManifestDatabase> {
    let mut files = files.peekable();
    if files.peek().is_none() {
        return Ok(TargetManifestDatabase {
            db: SmallMap::new(),
            root: cwd.to_path_buf(),
        });
    }

    let mut cmd = Command::new("buck2");
    cmd.arg("bxl");
    cmd.arg("--reuse-current-config");
    cmd.arg("prelude//python/sourcedb/pyrefly.bxl:main");
    cmd.arg("--");
    cmd.args(files.flat_map(Include::to_bxl_args));
    cmd.current_dir(cwd);

    let result = cmd.output()?;
    if !result.status.success() {
        let stdout = String::from_utf8(result.stdout)
            .unwrap_or_else(|_| "<Failed to parse stdout from Buck source db query>".to_owned());
        let stderr = String::from_utf8(result.stderr)
            .unwrap_or_else(|_| "<Failed to parse stderr from Buck source db query>".to_owned());

        return Err(anyhow::anyhow!(
            "Buck source db query failed...\nSTDOUT: {stdout}\nSTDERR: {stderr}"
        ));
    }

    serde_json::from_slice(&result.stdout).with_context(|| {
        "Failed to construct valid `TargetManifestDatabase` from BXL query result".to_owned()
    })
}

#[derive(Debug, PartialEq, Eq, Deserialize)]
#[serde(untagged)]
pub(crate) enum TargetManifest {
    Library {
        deps: SmallSet<Target>,
        srcs: SmallMap<ModuleName, Vec1<PathBuf>>,
        #[serde(flatten)]
        sys_info: SysInfo,
    },
    Alias {
        alias: Target,
    },
}

impl TargetManifest {
    #[expect(unused)]
    pub fn new_library(
        srcs: SmallMap<ModuleName, Vec1<PathBuf>>,
        deps: SmallSet<Target>,
        sys_info: SysInfo,
    ) -> Self {
        Self::Library {
            srcs,
            deps,
            sys_info: sys_info.dupe(),
        }
    }

    #[expect(unused)]
    pub fn new_alias(alias: Target) -> Self {
        Self::Alias { alias }
    }

    pub(crate) fn iter_srcs<'a>(
        &'a self,
    ) -> Box<dyn Iterator<Item = (&'a ModuleName, &'a Vec1<PathBuf>)> + 'a> {
        match &self {
            Self::Library { srcs, .. } => Box::new(srcs.iter()),
            Self::Alias { .. } => Box::new(std::iter::empty::<(&ModuleName, &Vec1<PathBuf>)>()),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Deserialize)]
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
