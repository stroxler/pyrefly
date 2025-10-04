/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use pyrefly_config::error_kind::ErrorKind;
use pyrefly_python::dunder;
use pyrefly_python::module_name::ModuleName;
use pyrefly_types::annotation::Annotation;
use pyrefly_types::callable::Callable;
use pyrefly_types::callable::FuncMetadata;
use pyrefly_types::callable::Function;
use pyrefly_types::callable::Param;
use pyrefly_types::callable::ParamList;
use pyrefly_types::callable::Required;
use pyrefly_types::literal::Lit;
use ruff_python_ast::name::Name;
use ruff_text_size::TextRange;

use crate::alt::answers::LookupAnswer;
use crate::alt::answers_solver::AnswersSolver;
use crate::alt::types::class_metadata::ClassMetadata;
use crate::alt::types::class_metadata::ClassSynthesizedField;
use crate::alt::types::pydantic::PydanticConfig;
use crate::alt::types::pydantic::PydanticModelKind;
use crate::alt::types::pydantic::PydanticModelKind::RootModel;
use crate::binding::pydantic::FROZEN_DEFAULT;
use crate::binding::pydantic::PydanticConfigDict;
use crate::binding::pydantic::ROOT;
use crate::binding::pydantic::STRICT;
use crate::binding::pydantic::VALIDATE_BY_ALIAS;
use crate::binding::pydantic::VALIDATE_BY_NAME;
use crate::error::collector::ErrorCollector;
use crate::error::context::ErrorInfo;
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

    pub fn get_pydantic_root_model_init(
        &self,
        cls: &Class,
        root_model_type: Type,
    ) -> ClassSynthesizedField {
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
        Some(
            self.get_pydantic_root_model_init(cls, root_model_type)
                .inner
                .ty(),
        )
    }

    pub fn pydantic_config(
        &self,
        bases_with_metadata: &[(Class, Arc<ClassMetadata>)],
        pydantic_config_dict: &PydanticConfigDict,
        keywords: &[(Name, Annotation)],
        errors: &ErrorCollector,
        range: TextRange,
    ) -> Option<PydanticConfig> {
        let has_pydantic_base_model_base_class =
            bases_with_metadata.iter().any(|(base_class_object, _)| {
                base_class_object.has_toplevel_qname(ModuleName::pydantic().as_str(), "BaseModel")
            });

        let is_pydantic_base_model = has_pydantic_base_model_base_class
            || bases_with_metadata
                .iter()
                .any(|(_, metadata)| metadata.is_pydantic_base_model());

        if !is_pydantic_base_model {
            return None;
        }

        let has_pydantic_root_model_base_class =
            bases_with_metadata.iter().any(|(base_class_object, _)| {
                base_class_object
                    .has_toplevel_qname(ModuleName::pydantic_root_model().as_str(), "RootModel")
            });

        let has_root_model_kind = bases_with_metadata.iter().any(|(_, metadata)| {
            matches!(
                metadata.pydantic_model_kind(),
                Some(PydanticModelKind::RootModel)
            )
        });

        let pydantic_model_kind = if has_pydantic_root_model_base_class || has_root_model_kind {
            PydanticModelKind::RootModel
        } else {
            PydanticModelKind::BaseModel
        };

        let PydanticConfigDict {
            frozen,
            extra,
            validation_flags,
        } = pydantic_config_dict;

        // Note: class keywords take precedence over ConfigDict keywords.
        // But another design choice is to error if there is a conflict. We can consider this design for v2.
        let mut validation_flags = validation_flags.clone();
        validation_flags.validate_by_alias = self.extract_bool_flag(
            keywords,
            &VALIDATE_BY_ALIAS,
            validation_flags.validate_by_alias,
        );
        validation_flags.validate_by_name = self.extract_bool_flag(
            keywords,
            &VALIDATE_BY_NAME,
            validation_flags.validate_by_name,
        );

        // Here, "ignore" and "allow" translate to true, while "forbid" translates to false.
        // With no keyword, the default is "true" and I default to "false" on a wrong keyword.
        // If we were to consider type narrowing in the "allow" case, we would need to propagate more data
        // and narrow downstream. We are not following the narrowing approach in v1 though, but should discuss it
        // for v2.
        let extra = match keywords.iter().find(|(name, _)| name.as_str() == "extra") {
            Some((_, ann)) => match ann.get_type() {
                Type::Literal(Lit::Str(s)) => match s.as_str() {
                    "allow" | "ignore" => true,
                    "forbid" => false,
                    _ => {
                        self.error(
                    errors,
                    range,
                    ErrorInfo::Kind(ErrorKind::InvalidLiteral),
                    "Invalid value for `extra`. Expected one of 'allow', 'ignore', or 'forbid'"
                        .to_owned(),
                );
                        true
                    }
                },
                _ => {
                    self.error(
                        errors,
                        range,
                        ErrorInfo::Kind(ErrorKind::InvalidLiteral),
                        "Invalid value for `extra`. Expected one of 'allow', 'ignore', or 'forbid'"
                            .to_owned(),
                    );
                    true
                }
            },
            None => {
                // No "extra" keyword in the class-level keywords,
                // so check if configdict has it, otherwise inherit from base classes
                if let Some(configdict_extra) = extra {
                    *configdict_extra
                } else {
                    // Check for inherited extra configuration from base classes
                    bases_with_metadata
                        .iter()
                        .find_map(|(_, metadata)| {
                            if metadata.is_pydantic_base_model() {
                                metadata.dataclass_metadata().map(|dm| dm.kws.extra)
                            } else {
                                None
                            }
                        })
                        .unwrap_or(true) // Default to true (ignore) if no base class has extra config
                }
            }
        };

        let frozen = match frozen {
            Some(value) => value,
            None => &bases_with_metadata
                .iter()
                .find_map(|(_, metadata)| {
                    metadata
                        .dataclass_metadata()
                        .as_ref()
                        .map(|dm| dm.kws.frozen)
                })
                .unwrap_or(FROZEN_DEFAULT),
        };

        let strict = self.extract_bool_flag(keywords, &STRICT, false);

        Some(PydanticConfig {
            frozen: *frozen,
            validation_flags,
            extra,
            strict,
            pydantic_model_kind,
        })
    }

    fn extract_bool_flag(
        &self,
        keywords: &[(Name, Annotation)],
        key: &Name,
        default: bool,
    ) -> bool {
        keywords
            .iter()
            .find(|(name, _)| name == key)
            .map_or(default, |(_, ann)| {
                ann.get_type().as_bool().unwrap_or(default)
            })
    }
}
