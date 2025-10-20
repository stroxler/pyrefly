/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::PathBuf;
use std::sync::Arc;

use pyrefly_bundled::bundled_third_party_stubs;
use pyrefly_python::module_name::ModuleName;
use starlark_map::small_map::SmallMap;

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct BundledTypeshedThirdParty {
    pub find: SmallMap<ModuleName, PathBuf>,
    pub load: SmallMap<PathBuf, Arc<String>>,
}

impl BundledTypeshedThirdParty {
    #[allow(dead_code)]
    fn new() -> anyhow::Result<Self> {
        let contents = bundled_third_party_stubs()?;
        let mut res = Self {
            find: SmallMap::new(),
            load: SmallMap::new(),
        };
        for (relative_path, contents) in contents {
            let module_name = ModuleName::from_relative_path(&relative_path)?;
            res.find.insert(module_name, relative_path.clone());
            res.load.insert(relative_path, Arc::new(contents));
        }
        Ok(res)
    }
}
