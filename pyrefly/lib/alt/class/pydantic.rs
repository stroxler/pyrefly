/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use pyrefly_python::module_name::ModuleName;

use crate::alt::answers::LookupAnswer;
use crate::alt::answers_solver::AnswersSolver;
use crate::types::class::Class;
use crate::types::types::Type;

impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    #[allow(dead_code)]
    pub fn get_pydantic_root_model_type_via_mro(&self, class: &Class) -> Option<Type> {
        let mro = self.get_mro_for_class(class);
        for base_type in mro.ancestors_no_object() {
            if base_type.has_qname(ModuleName::pydantic_root_model().as_str(), "RootModel") {
                let targs = base_type.targs().as_slice();
                let root_type = targs.last().cloned();
                if root_type.is_some() {
                    return root_type;
                }
            }
        }

        None
    }
}
