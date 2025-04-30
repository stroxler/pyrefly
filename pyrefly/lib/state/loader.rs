/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt::Debug;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;

use anyhow::anyhow;
use dupe::Dupe;

use crate::config::config::ConfigFile;
use crate::module::module_name::ModuleName;
use crate::module::module_path::ModulePath;
use crate::util::arc_id::ArcId;
use crate::util::display::commas_iter;
use crate::util::locked_map::LockedMap;

#[derive(Debug, Clone, Dupe)]
pub enum FindError {
    /// This module could not be found, and we should emit an error
    NotFound(Arc<anyhow::Error>, ModuleName),
    /// This import could not be found, but the user configured it to be ignored
    Ignored,
    /// This site package path entry was found, but does not have a py.typed entry
    /// and use_untyped_imports is disabled
    NoPyTyped,
    /// We found stubs, but no source files were found. This means it's likely stubs
    /// are installed for a project, but the library is not actually importable
    NoSource(ModuleName),
}

impl FindError {
    pub fn not_found(err: anyhow::Error, module: ModuleName) -> Self {
        Self::NotFound(Arc::new(err), module)
    }

    pub fn no_source(module: ModuleName) -> Self {
        Self::NoSource(module)
    }

    pub fn search_path(
        search_roots: &[PathBuf],
        site_package_path: &[PathBuf],
        module: ModuleName,
    ) -> FindError {
        if search_roots.is_empty() && site_package_path.is_empty() {
            Self::not_found(anyhow!("no search roots or site package path"), module)
        } else {
            Self::not_found(
                anyhow!(
                    "looked at search roots ({}) and site package path ({})",
                    commas_iter(|| search_roots.iter().map(|x| x.display())),
                    commas_iter(|| site_package_path.iter().map(|x| x.display())),
                ),
                module,
            )
        }
    }

    pub fn display(&self) -> String {
        match self {
            Self::NotFound(err, module) => {
                format!("Could not find import of `{module}`, {:#}", err)
            }
            Self::Ignored => "Ignored import".to_owned(),
            Self::NoPyTyped => "Imported package does not contain a py.typed file, \
                and therefore cannot be typed. Try installing a `<package name>-stubs` version
                of your package to get the released stubs, or enable `use_untyped_imports` to
                disable this error."
                .to_owned(),
            Self::NoSource(module) => {
                format!(
                    "Found stubs for `{module}`, but no source. This means it's likely not \
                    installed/unimportable. See `ignore_missing_source` to disable this error."
                )
            }
        }
    }
}

#[derive(Debug)]
pub struct LoaderFindCache {
    config: ArcId<ConfigFile>,
    cache: LockedMap<ModuleName, Result<ModulePath, FindError>>,
}

impl LoaderFindCache {
    pub fn new(config: ArcId<ConfigFile>) -> Self {
        Self {
            config,
            cache: Default::default(),
        }
    }

    pub fn find_import(
        &self,
        module: ModuleName,
        path: Option<&Path>,
    ) -> Result<ModulePath, FindError> {
        self.cache
            .ensure(&module, || self.config.find_import(module, path))
            .dupe()
    }
}
