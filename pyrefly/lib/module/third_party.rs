/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::PathBuf;
use std::sync::Arc;

use pyrefly_config::config::ConfigFile;
use pyrefly_python::module_name::ModuleName;
use pyrefly_python::module_path::ModulePath;
use pyrefly_util::arc_id::ArcId;
use starlark_map::small_map::SmallMap;

use crate::module::bundled::BundledStub;

#[expect(dead_code)]
pub struct BundledThirdParty {
    pub find: SmallMap<ModuleName, PathBuf>,
    pub load: SmallMap<PathBuf, Arc<String>>,
}

impl BundledStub for BundledThirdParty {
    fn new() -> anyhow::Result<Self> {
        // todo:(jvansch) This is just a mock, actual logic will be implemented later
        Ok(Self {
            find: SmallMap::new(),
            load: SmallMap::new(),
        })
    }

    fn find(&self, _module: ModuleName) -> Option<ModulePath> {
        // todo:(jvansch) This is just a mock, actual logic will be implemented later
        None
    }

    fn load(&self, _path: &std::path::Path) -> Option<Arc<String>> {
        // todo:(jvansch) This is just a mock, actual logic will be implemented later
        None
    }

    fn modules(&self) -> impl Iterator<Item = ModuleName> {
        // todo:(jvansch) This is just a mock, actual logic will be implemented later
        std::iter::empty()
    }

    fn config() -> ArcId<ConfigFile> {
        // todo:(jvansch) This is just a mock, actual logic will be implemented later
        ArcId::new(ConfigFile::default())
    }

    fn get_path_name(&self) -> String {
        // todo:(jvansch) This is just a mock, actual logic will be implemented later
        String::from("third_party")
    }

    fn load_map(&self) -> impl Iterator<Item = (&PathBuf, &Arc<String>)> {
        self.load.iter()
    }
}
