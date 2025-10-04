/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use pyrefly_python::dunder;
use pyrefly_python::module_name::ModuleName;
use pyrefly_types::callable::Callable;
use pyrefly_types::callable::FuncMetadata;
use pyrefly_types::callable::Function;
use pyrefly_types::callable::Param;
use pyrefly_types::callable::ParamList;
use pyrefly_types::callable::Required;
use ruff_python_ast::name::Name;

use crate::alt::answers::LookupAnswer;
use crate::alt::answers_solver::AnswersSolver;
use crate::alt::types::class_metadata::ClassMetadata;
use crate::alt::types::class_metadata::ClassSynthesizedField;
use crate::alt::types::pydantic::PydanticModelKind::RootModel;
use crate::binding::pydantic::ROOT;
use crate::types::class::Class;
use crate::types::types::Type;

impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    pub fn get_pydantic_root_model_type_via_mro(
        &self,
        class: &Class,
        metadata: &ClassMetadata,
    ) -> Option<Type> {
        if !matches!(metadata.pydantic_model_kind(), Some(RootModel)) {
            return None;
        }

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

    pub fn get_root_model_init(&self, cls: &Class, root_model_type: Type) -> ClassSynthesizedField {
        let root_requiredness =
            if root_model_type.is_any() || matches!(root_model_type, Type::Quantified(_)) {
                Required::Optional(None)
            } else {
                Required::Required
            };
        let root_param = Param::Pos(ROOT, root_model_type, root_requiredness);
        let params = vec![self.class_self_param(cls, false), root_param];
        let ty = Type::Function(Box::new(Function {
            signature: Callable::list(ParamList::new(params), Type::None),
            metadata: FuncMetadata::def(self.module().name(), cls.name().clone(), dunder::INIT),
        }));
        ClassSynthesizedField::new(ty)
    }

    pub fn get_pydantic_root_model_class_field_type(
        &self,
        cls: &Class,
        attr_name: &Name,
    ) -> Option<Type> {
        if !cls.has_toplevel_qname(ModuleName::pydantic_root_model().as_str(), "RootModel")
            || *attr_name != dunder::INIT
        {
            return None;
        }
        let tparams = self.get_class_tparams(cls);
        // `RootModel` should always have a type parameter unless we're working with a broken copy
        // of Pydantic.
        let tparam = tparams.iter().next()?;
        let root_model_type = Type::Quantified(Box::new(tparam.quantified.clone()));
        Some(self.get_root_model_init(cls, root_model_type).inner.ty())
    }
}
