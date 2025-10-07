/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use pyrefly_python::module_name::ModuleName;
use pyrefly_types::types::Type;
use ruff_python_ast::name::Name;

use crate::alt::answers::LookupAnswer;
use crate::alt::answers_solver::AnswersSolver;

/// Django stubs use this attribute to specify the Python type that a field should infer to
const DJANGO_PRIVATE_GET_TYPE: Name = Name::new_static("_pyi_private_get_type");

impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    pub fn get_django_field_type(&self, ty: &Type) -> Option<Type> {
        if let Type::ClassType(cls) = ty
            && self.inherits_from_django_field(cls.class_object())
            && let Some(member) =
                self.get_class_member(cls.class_object(), &DJANGO_PRIVATE_GET_TYPE)
        {
            let field_type = member.value.ty();
            return Some(field_type);
        }

        None
    }

    /// Check if a class inherits from Django's Field class
    fn inherits_from_django_field(&self, cls: &crate::types::class::Class) -> bool {
        self.get_mro_for_class(cls)
            .ancestors(self.stdlib)
            .any(|ancestor| {
                ancestor.has_qname(ModuleName::django_models_fields().as_str(), "Field")
            })
    }
}
