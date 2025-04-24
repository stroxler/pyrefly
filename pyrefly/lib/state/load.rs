/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use anyhow::anyhow;
use dupe::Dupe;
use dupe::OptionDupedExt;
use ruff_text_size::TextRange;
use starlark_map::small_map::SmallMap;

use crate::config::error::ErrorConfigs;
use crate::error::collector::CollectedErrors;
use crate::error::collector::ErrorCollector;
use crate::error::expectation::Expectation;
use crate::error::kind::ErrorKind;
use crate::error::style::ErrorStyle;
use crate::module::bundled::typeshed;
use crate::module::ignore::Ignore;
use crate::module::module_info::ModuleInfo;
use crate::module::module_name::ModuleName;
use crate::module::module_path::ModulePath;
use crate::module::module_path::ModulePathDetails;
use crate::state::memory::MemoryFilesLookup;
use crate::util::fs_anyhow;

/// The result of loading a module, including its `ModuleInfo` and `ErrorCollector`.
#[derive(Debug)]
pub struct Load {
    pub errors: ErrorCollector,
    pub module_info: ModuleInfo,
}

/// The errors from a collection of modules.
#[derive(Debug)]
pub struct Errors {
    loads: Vec<Arc<Load>>,
}

impl Load {
    /// Return the code for this module, and whether there was an error while loading (a self-error).
    pub fn load_from_path(
        path: &ModulePath,
        memory_lookup: &MemoryFilesLookup,
    ) -> (Arc<String>, Option<anyhow::Error>) {
        let res = match path.details() {
            ModulePathDetails::FileSystem(path) => fs_anyhow::read_to_string(path).map(Arc::new),
            ModulePathDetails::Namespace(_) => Ok(Arc::new("".to_owned())),
            ModulePathDetails::Memory(path) => memory_lookup
                .get(path)
                .duped()
                .ok_or_else(|| anyhow!("memory path not found")),
            ModulePathDetails::BundledTypeshed(path) => typeshed().and_then(|x| {
                x.load(path)
                    .ok_or_else(|| anyhow!("bundled typeshed problem"))
            }),
        };
        match res {
            Err(err) => (Arc::new(String::new()), Some(err)),
            Ok(res) => (res, None),
        }
    }

    pub fn load_from_data(
        name: ModuleName,
        path: ModulePath,
        error_style: ErrorStyle,
        code: Arc<String>,
        self_error: Option<anyhow::Error>,
    ) -> Self {
        let module_info = ModuleInfo::new(name, path, code);
        let errors = ErrorCollector::new(module_info.dupe(), error_style);
        if let Some(err) = self_error {
            errors.add(
                TextRange::default(),
                format!(
                    "Failed to load `{name}` from `{}`, got {err:#}",
                    module_info.path()
                ),
                ErrorKind::ImportError,
                None,
            );
        }
        Self {
            errors,
            module_info,
        }
    }
}

impl Errors {
    pub fn new(loads: Vec<Arc<Load>>) -> Self {
        Self { loads }
    }

    pub fn collect_errors(&self, error_configs: &ErrorConfigs) -> CollectedErrors {
        let mut errors = CollectedErrors::default();
        let mut sorted_loads = self.loads.clone();
        sorted_loads.sort_by_key(|load| load.module_info.name());
        for load in sorted_loads.iter() {
            let module_path = load.module_info.path();
            let error_config = error_configs.get(module_path);
            load.errors.collect_into(error_config, &mut errors);
        }
        errors
    }

    pub fn collect_ignores(&self) -> SmallMap<&ModulePath, &Ignore> {
        let mut ignore_collection: SmallMap<&ModulePath, &Ignore> = SmallMap::new();
        for load in self.loads.iter() {
            let module_path = load.module_info.path();
            let ignores = load.module_info.ignore();
            ignore_collection.insert(module_path, ignores);
        }
        ignore_collection
    }

    pub fn check_against_expectations(&self, error_configs: &ErrorConfigs) -> anyhow::Result<()> {
        for load in self.loads.iter() {
            let module_info = &load.module_info;
            let error_config = error_configs.get(module_info.path());
            Expectation::parse(module_info.dupe(), module_info.contents())
                .check(&load.errors.collect(error_config).shown)?;
        }
        Ok(())
    }
}
