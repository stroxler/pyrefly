/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;

use crate::metadata::RuntimeMetadata;
use crate::module::module_name::ModuleName;
use crate::module::module_path::ModulePath;

#[derive(Debug, Clone, Dupe, PartialEq, Eq, Hash)]
pub struct Handle {
    module: ModuleName,
    path: ModulePath,
    config: RuntimeMetadata,
}

impl Handle {
    pub fn new(module: ModuleName, path: ModulePath, config: RuntimeMetadata) -> Self {
        Self {
            module,
            path,
            config,
        }
    }

    pub fn module(&self) -> ModuleName {
        self.module
    }

    pub fn path(&self) -> &ModulePath {
        &self.path
    }

    pub fn config(&self) -> &RuntimeMetadata {
        &self.config
    }
}
