/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use dupe::Dupe;
use starlark_map::small_map::SmallMap;

use crate::config::config::ConfigFile;
use crate::config::error::ErrorConfigs;
use crate::error::collector::CollectedErrors;
use crate::error::expectation::Expectation;
use crate::module::ignore::Ignore;
use crate::module::module_path::ModulePath;
use crate::state::load::Load;
use crate::util::arc_id::ArcId;

/// The errors from a collection of modules.
#[derive(Debug)]
pub struct Errors {
    loads: Vec<(Arc<Load>, ArcId<ConfigFile>)>,
}

impl Errors {
    pub fn new(loads: Vec<(Arc<Load>, ArcId<ConfigFile>)>) -> Self {
        Self { loads }
    }

    pub fn collect_errors(&self, error_configs: &ErrorConfigs) -> CollectedErrors {
        let mut errors = CollectedErrors::default();
        let mut sorted_loads = self.loads.clone();
        sorted_loads.sort_by_key(|x| x.0.module_info.name());
        for (load, _) in &sorted_loads {
            let module_path = load.module_info.path();
            let error_config = error_configs.get(module_path);
            load.errors.collect_into(error_config, &mut errors);
        }
        errors
    }

    pub fn collect_ignores(&self) -> SmallMap<&ModulePath, &Ignore> {
        let mut ignore_collection: SmallMap<&ModulePath, &Ignore> = SmallMap::new();
        for (load, _) in &self.loads {
            let module_path = load.module_info.path();
            let ignores = load.module_info.ignore();
            ignore_collection.insert(module_path, ignores);
        }
        ignore_collection
    }

    pub fn check_against_expectations(&self, error_configs: &ErrorConfigs) -> anyhow::Result<()> {
        for (load, _) in &self.loads {
            let module_info = &load.module_info;
            let error_config = error_configs.get(module_info.path());
            Expectation::parse(module_info.dupe(), module_info.contents())
                .check(&load.errors.collect(error_config).shown)?;
        }
        Ok(())
    }
}
