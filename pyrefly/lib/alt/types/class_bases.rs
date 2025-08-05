/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;

use dupe::Dupe;
use pyrefly_derive::TypeEq;
use pyrefly_derive::VisitMut;
use pyrefly_types::class::ClassType;
use pyrefly_util::display::commas_iter;
use ruff_text_size::Ranged;

use crate::alt::answers::LookupAnswer;
use crate::alt::answers_solver::AnswersSolver;
use crate::alt::solve::TypeFormContext;
use crate::binding::base_class::BaseClass;
use crate::config::error_kind::ErrorKind;
use crate::error::collector::ErrorCollector;
use crate::error::context::ErrorInfo;
use crate::types::class::Class;
use crate::types::tuple::Tuple;
use crate::types::types::Type;

/// The bases of a class, in type form.
/// This is intended to be used for any downstream computation that needs to inspect the full types
/// (in particular, the targs of generic bases) of the bases of a class. If only the class objects are
/// needed, query `ClassMetadata` instead since that one doesn't require calculating the full types.
#[derive(Debug, Clone, TypeEq, PartialEq, Eq, VisitMut, Default)]
pub struct ClassBases {
    base_types: Box<[ClassType]>,
    tuple_base: Option<Tuple>,
    /// Is it possible for this class to have type parameters that we don't know about?
    /// This can happen if, e.g., a class inherits from Any.
    has_unknown_tparams: bool,
}

impl ClassBases {
    pub fn recursive() -> Self {
        Self {
            base_types: Box::new([]),
            tuple_base: None,
            has_unknown_tparams: false,
        }
    }

    pub fn tuple_base(&self) -> Option<&Tuple> {
        self.tuple_base.as_ref()
    }

    pub fn has_unknown_tparams(&self) -> bool {
        self.has_unknown_tparams
    }

    pub fn iter(&self) -> impl Iterator<Item = &ClassType> {
        self.base_types.iter()
    }
}

impl fmt::Display for ClassBases {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ClassBases({})", commas_iter(|| self.base_types.iter()))
    }
}

impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    pub fn class_bases_of(
        &self,
        cls: &Class,
        bases: &[BaseClass],
        special_base: &Option<Box<BaseClass>>,
        is_new_type: bool,
        errors: &ErrorCollector,
    ) -> ClassBases {
        let mut bases: Vec<BaseClass> = bases.to_vec();
        if let Some(special_base) = special_base {
            bases.push((**special_base).clone());
        }
        let has_generic_base_class = bases.iter().any(|x| x.is_generic());
        let base_types_with_ranges = bases
            .iter()
            .filter_map(|x| match x {
                BaseClass::Expr(x) => Some((
                    self.expr_untype(x, TypeFormContext::BaseClassList, errors),
                    x.range(),
                )),
                BaseClass::NamedTuple(..) => Some((
                    self.stdlib.named_tuple_fallback().clone().to_type(),
                    x.range(),
                )),
                BaseClass::TypedDict(..) => None,
                BaseClass::Generic(args, _) | BaseClass::Protocol(args, _) => {
                    if !is_new_type {
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

        let mut tuple_base = if is_new_type {
            None
        } else {
            base_types_with_ranges.iter().find_map(|(ty, _)| {
                if let Type::Tuple(tuple) = ty {
                    Some(tuple.clone())
                } else {
                    None
                }
            })
        };

        let base_type_base_and_range = base_types_with_ranges
            .into_iter()
            .filter_map(|base_type_and_range| {
                // Return Ok() if the base class is valid, or Err() if it is not.
                match base_type_and_range {
                    (Type::ClassType(c), range) => {
                        let bases = self.get_base_types_for_class(c.class_object());
                        Some((c, bases, range))
                    }
                    (Type::Tuple(tuple), range) => {
                        let class_ty = self.erase_tuple_type(tuple);
                        let bases = self.get_base_types_for_class(class_ty.class_object());
                        Some((class_ty, bases, range))
                    }
                    (Type::TypedDict(typed_dict), range) => {
                        if is_new_type {
                            // Error will be reported in `class_metadata_of`
                            None
                        } else {
                            // HACK HACK HACK - TypedDict instances behave very differently from instances of other
                            // classes, so we don't represent them as ClassType in normal typechecking logic. However,
                            // class ancestors are represented as ClassType all over the code base, and changing this
                            // would be quite painful. So we convert TypedDict to ClassType in this one spot. Please do
                            // not do this anywhere else.
                            Some((
                                ClassType::new(
                                    typed_dict.class_object().dupe(),
                                    typed_dict.targs().clone(),
                                ),
                                self.get_base_types_for_class(typed_dict.class_object()),
                                range,
                            ))
                        }
                    }
                    (_, _) => None,
                }
            })
            .collect::<Vec<_>>();

        let mut base_class_types = base_type_base_and_range
            .into_iter()
            .map(|(base_class_type, base_class_bases, range)| {
                if is_new_type
                    && base_class_type.targs().as_slice().iter().any(|ty| {
                        ty.any(|ty| {
                            matches!(
                                ty,
                                Type::TypeVar(_) | Type::TypeVarTuple(_) | Type::ParamSpec(_)
                            )
                        })
                    })
                {
                    self.error(
                        errors,
                        range,
                        ErrorInfo::Kind(ErrorKind::InvalidArgument),
                        "Second argument to NewType cannot be an unbound generic".to_owned(),
                    );
                }
                if let Some(base_class_tuple_base) = base_class_bases.tuple_base() {
                    if let Some(existing_tuple_base) = &tuple_base {
                        if existing_tuple_base.is_any_tuple() {
                            tuple_base = Some(base_class_tuple_base.clone());
                        } else if !base_class_tuple_base.is_any_tuple()
                            && base_class_tuple_base != existing_tuple_base
                        {
                            self.error(
                                errors,
                                range,
                                ErrorInfo::Kind(ErrorKind::InvalidInheritance),
                                format!(
                                    "Cannot extend multiple incompatible tuples: `{}` and `{}`",
                                    self.for_display(Type::Tuple(existing_tuple_base.clone())),
                                    self.for_display(Type::Tuple(base_class_tuple_base.clone())),
                                ),
                            );
                        }
                    } else {
                        tuple_base = Some(base_class_tuple_base.clone());
                    }
                }
                base_class_type
            })
            .collect::<Vec<_>>();

        let metadata = self.get_metadata_for_class(cls);
        if metadata.is_typed_dict() && base_class_types.is_empty() {
            // This is a "fallback" class that contains attributes that are available on all TypedDict subclasses.
            // Note that this also makes those attributes available on *instances* of said subclasses; this is
            // desirable for methods but problematic for fields like `__total__` that should be available on the class
            // but not the instance. For now, we make all fields available on both classes and instances.
            base_class_types.push(self.stdlib.typed_dict_fallback().clone());
        }

        let empty_tparams = self.get_class_tparams(cls).is_empty();
        let has_base_any = metadata.has_base_any();
        // We didn't find any type parameters for this class, but it may have ones we don't know about if:
        // - the class inherits from Any, or
        // - the class inherits from Generic[...] or Protocol [...]. We probably dropped the type
        //   arguments because we found an error in them.
        let has_unknown_tparams = empty_tparams && (has_base_any || has_generic_base_class);

        ClassBases {
            base_types: base_class_types.into_boxed_slice(),
            tuple_base,
            has_unknown_tparams,
        }
    }
}
