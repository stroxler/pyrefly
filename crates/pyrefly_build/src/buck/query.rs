/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ffi::OsStr;
use std::fmt::Debug;
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
    #[expect(unused)]
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

#[derive(Debug, PartialEq, Eq, Deserialize, Clone)]
pub(crate) struct PythonLibraryManifest {
    pub deps: SmallSet<Target>,
    pub srcs: SmallMap<ModuleName, Vec1<PathBuf>>,
    #[serde(flatten)]
    pub sys_info: SysInfo,
}

impl PythonLibraryManifest {
    fn replace_alias_deps(&mut self, aliases: &SmallMap<Target, Target>) {
        self.deps = self
            .deps
            .iter()
            .map(|t| {
                if let Some(replace) = aliases.get(t) {
                    replace.dupe()
                } else {
                    t.dupe()
                }
            })
            .collect();
    }

    fn rewrite_relative_to_root(&mut self, root: &Path) {
        self.srcs
            .iter_mut()
            .for_each(|(_, paths)| paths.iter_mut().for_each(|p| *p = root.join(&p)));
    }
}

#[derive(Debug, PartialEq, Eq, Deserialize, Clone)]
#[serde(untagged)]
enum TargetManifest {
    Library(PythonLibraryManifest),
    Alias { alias: Target },
}

#[derive(Debug, PartialEq, Eq, Deserialize, Clone)]
pub(crate) struct TargetManifestDatabase {
    db: SmallMap<Target, TargetManifest>,
    pub root: PathBuf,
}

impl TargetManifestDatabase {
    #[expect(unused)]
    pub fn produce_map(self) -> SmallMap<Target, PythonLibraryManifest> {
        let mut result = SmallMap::new();
        let aliases: SmallMap<Target, Target> = self
            .db
            .iter()
            .filter_map(|(t, manifest)| match manifest {
                TargetManifest::Alias { alias } => Some((t.dupe(), alias.dupe())),
                _ => None,
            })
            .collect();
        for (target, manifest) in self.db {
            match manifest {
                TargetManifest::Alias { .. } => continue,
                TargetManifest::Library(mut lib) => {
                    lib.replace_alias_deps(&aliases);
                    lib.rewrite_relative_to_root(&self.root);
                    result.insert(target, lib);
                }
            }
        }
        result
    }
}
