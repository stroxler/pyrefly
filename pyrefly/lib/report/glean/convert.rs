/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use ruff_python_ast::ModModule;

use crate::alt::answers::Solutions;
use crate::binding::bindings::Bindings;
use crate::module::module_info::ModuleInfo;
use crate::report::glean::schema::*;

impl Glean {
    #[allow(unused_variables)]
    pub fn new(
        module_info: &ModuleInfo,
        ast: &ModModule,
        bindings: &Bindings,
        solutions: &Solutions,
    ) -> Self {
        Glean {
            entries: Vec::new(),
        }
    }
}
