/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;

use pyrefly_derive::TypeEq;
use pyrefly_derive::VisitMut;
use pyrefly_util::display::commas_iter;
use ruff_text_size::Ranged;

use crate::alt::answers::LookupAnswer;
use crate::alt::answers_solver::AnswersSolver;
use crate::alt::solve::TypeFormContext;
use crate::binding::base_class::BaseClass;
use crate::config::error_kind::ErrorKind;
use crate::error::collector::ErrorCollector;
use crate::error::context::ErrorInfo;
use crate::types::types::Type;

/// The bases of a class, in type form.
/// This is intended to be used for any downstream computation that needs to inspect the full types
/// (in particular, the targs of generic bases) of the bases of a class. If only the class objects are
/// needed, query `ClassMetadata` instead since that one doesn't require calculating the full types.
#[derive(Debug, Clone, TypeEq, PartialEq, Eq, VisitMut, Default)]
pub struct ClassBases(Box<[Type]>);

impl ClassBases {
    pub fn new(base_types: Vec<Type>) -> Self {
        Self(base_types.into_boxed_slice())
    }
}

impl fmt::Display for ClassBases {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", commas_iter(|| self.0.iter()))
    }
}

impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    pub fn class_bases_of(
        &self,
        bases: &[BaseClass],
        special_base: &Option<Box<BaseClass>>,
        is_new_type: bool,
        errors: &ErrorCollector,
    ) -> ClassBases {
        let mut bases: Vec<BaseClass> = bases.to_vec();
        if let Some(special_base) = special_base {
            bases.push((**special_base).clone());
        }
        let base_types = bases
            .iter()
            .filter_map(|x| match x {
                BaseClass::Expr(x) => {
                    Some(self.expr_untype(x, TypeFormContext::BaseClassList, errors))
                }
                BaseClass::NamedTuple(..) => {
                    Some(self.stdlib.named_tuple_fallback().clone().to_type())
                }
                BaseClass::TypedDict(..) => {
                    if is_new_type {
                        self.error(
                            errors,
                            x.range(),
                            ErrorInfo::Kind(ErrorKind::InvalidArgument),
                            "Second argument to NewType is invalid".to_owned(),
                        );
                    }
                    None
                }
                BaseClass::Generic(args, _) | BaseClass::Protocol(args, _) => {
                    if is_new_type {
                        self.error(
                            errors,
                            x.range(),
                            ErrorInfo::Kind(ErrorKind::InvalidArgument),
                            "Second argument to NewType is invalid".to_owned(),
                        );
                    } else {
                        let mut type_var_tuple_count = 0;
                        args.iter().for_each(|x| {
                            let ty = self.expr_untype(x, TypeFormContext::GenericBase, errors);
                            if let Type::Unpack(unpacked) = &ty
                                && unpacked.is_kind_type_var_tuple()
                            {
                                if type_var_tuple_count == 1 {
                                    self.error(
                                        errors,
                                        x.range(),
                                        ErrorInfo::Kind(ErrorKind::InvalidInheritance),
                                        "There cannot be more than one TypeVarTuple type parameter"
                                            .to_owned(),
                                    );
                                }
                                type_var_tuple_count += 1;
                            }
                        });
                    }
                    None
                }
            })
            .collect::<Vec<_>>();
        ClassBases::new(base_types)
    }
}
