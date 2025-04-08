/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use ruff_python_ast::name::Name;
use ruff_text_size::TextRange;
use starlark_map::small_map::SmallMap;

use crate::alt::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
use crate::error::collector::ErrorCollector;
use crate::error::kind::ErrorKind;
use crate::types::callable::Param;
use crate::types::callable::ParamList;
use crate::types::callable::Required;
use crate::types::class::TArgs;
use crate::types::tuple::Tuple;
use crate::types::types::TParams;
use crate::types::types::Type;
use crate::util::display::count;
use crate::util::prelude::SliceExt;

impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    pub fn check_and_create_targs(
        &self,
        name: &Name,
        tparams: &TParams,
        targs: Vec<Type>,
        range: TextRange,
        errors: &ErrorCollector,
    ) -> TArgs {
        let nparams = tparams.len();
        let nargs = targs.len();
        let mut checked_targs = Vec::new();
        let mut targ_idx = 0;
        let mut name_to_idx = SmallMap::new();
        for (param_idx, param) in tparams.iter().enumerate() {
            if param.quantified.is_type_var_tuple() && targs.get(targ_idx).is_some() {
                let n_remaining_params = tparams.len() - param_idx - 1;
                let n_remaining_args = nargs - targ_idx;
                let mut prefix = Vec::new();
                let mut middle = Vec::new();
                let mut suffix = Vec::new();
                let args_to_consume = n_remaining_args.saturating_sub(n_remaining_params);
                for _ in 0..args_to_consume {
                    match targs.get(targ_idx) {
                        Some(Type::Unpack(box Type::Tuple(Tuple::Concrete(elts)))) => {
                            if middle.is_empty() {
                                prefix.extend(elts.clone());
                            } else {
                                suffix.extend(elts.clone());
                            }
                        }
                        Some(Type::Unpack(box t)) => {
                            if !suffix.is_empty() {
                                middle.push(Type::Tuple(Tuple::Unbounded(Box::new(
                                    self.unions(suffix),
                                ))));
                                suffix = Vec::new();
                            } else {
                                middle.push(t.clone())
                            }
                        }
                        Some(arg) => {
                            let arg = if arg.is_kind_type_var_tuple() {
                                self.error(
                                    errors,
                                    range,
                                    ErrorKind::InvalidTypeVarTuple,
                                    None,
                                    "TypeVarTuple must be unpacked".to_owned(),
                                )
                            } else {
                                arg.clone()
                            };
                            if middle.is_empty() {
                                prefix.push(arg);
                            } else {
                                suffix.push(arg);
                            }
                        }
                        _ => {}
                    }
                    targ_idx += 1;
                }
                let tuple_type = match middle.as_slice() {
                    [] => Type::tuple(prefix),
                    [middle] => Type::Tuple(Tuple::unpacked(prefix, middle.clone(), suffix)),
                    // We can't precisely model unpacking two unbounded iterables, so we'll keep any
                    // concrete prefix and suffix elements and merge everything in between into an unbounded tuple
                    _ => {
                        let middle_types: Vec<Type> = middle
                            .iter()
                            .map(|t| {
                                self.unwrap_iterable(t)
                                    .unwrap_or(self.stdlib.object().clone().to_type())
                            })
                            .collect();
                        Type::Tuple(Tuple::unpacked(
                            prefix,
                            Type::Tuple(Tuple::Unbounded(Box::new(self.unions(middle_types)))),
                            suffix,
                        ))
                    }
                };
                checked_targs.push(tuple_type);
            } else if param.quantified.is_param_spec()
                && nparams == 1
                && let Some(arg) = targs.get(targ_idx)
            {
                if arg.is_kind_param_spec() {
                    checked_targs.push(arg.clone());
                    targ_idx += 1;
                } else {
                    // If the only type param is a ParamSpec and the type argument
                    // is not a parameter expression, then treat the entire type argument list
                    // as a parameter list
                    let params: Vec<Param> =
                        targs.map(|t| Param::PosOnly(t.clone(), Required::Required));
                    checked_targs.push(Type::ParamSpecValue(ParamList::new(params)));
                    targ_idx = nargs;
                }
            } else if param.quantified.is_param_spec()
                && let Some(arg) = targs.get(targ_idx)
            {
                if arg.is_kind_param_spec() {
                    checked_targs.push(arg.clone());
                } else {
                    self.error(
                        errors,
                        range,
                        ErrorKind::InvalidParamSpec,
                        None,
                        format!("Expected a valid ParamSpec expression, got `{arg}`"),
                    );
                    checked_targs.push(Type::Ellipsis);
                }
                targ_idx += 1;
            } else if let Some(arg) = targs.get(targ_idx) {
                match arg {
                    Type::Unpack(_) => {
                        checked_targs.push(self.error(
                            errors,
                            range,
                            ErrorKind::BadUnpacking,
                            None,
                            format!(
                                "Unpacked argument cannot be used for type parameter {}",
                                param.name()
                            ),
                        ));
                    }
                    _ => {
                        let arg = if arg.is_kind_type_var_tuple() {
                            self.error(
                                errors,
                                range,
                                ErrorKind::InvalidTypeVarTuple,
                                None,
                                "TypeVarTuple must be unpacked".to_owned(),
                            )
                        } else if arg.is_kind_param_spec() {
                            self.error(
                                errors,
                                range,
                                ErrorKind::InvalidParamSpec,
                                None,
                                "ParamSpec cannot be used for type parameter".to_owned(),
                            )
                        } else {
                            arg.clone()
                        };
                        checked_targs.push(arg);
                    }
                }
                targ_idx += 1;
            } else if let Some(default) = param.default() {
                let resolved_default = default.clone().transform(&mut |default| {
                    let typevar_name = match default {
                        Type::TypeVar(t) => Some(t.qname().id()),
                        Type::TypeVarTuple(t) => Some(t.qname().id()),
                        Type::ParamSpec(p) => Some(p.qname().id()),
                        Type::Quantified(q) => Some(q.name()),
                        _ => None,
                    };
                    if let Some(typevar_name) = typevar_name {
                        *default = if let Some(i) = name_to_idx.get(typevar_name) {
                            // The default of this TypeVar contains the value of a previous TypeVar.
                            let val: &Type = &checked_targs[*i];
                            val.clone()
                        } else {
                            // The default refers to the value of a TypeVar that isn't in scope. We've
                            // already logged an error in TParams::new(); return a sensible default.
                            Type::any_implicit()
                        }
                    }
                });
                checked_targs.push(resolved_default);
            } else {
                let only_type_var_tuples_left = tparams
                    .iter()
                    .skip(param_idx)
                    .all(|x| x.quantified.is_type_var_tuple());
                if !only_type_var_tuples_left {
                    self.error(
                        errors,
                        range,
                        ErrorKind::BadSpecialization,
                        None,
                        format!(
                            "Expected {} for `{}`, got {}",
                            count(tparams.len(), "type argument"),
                            name,
                            nargs
                        ),
                    );
                }
                let defaults = tparams
                    .iter()
                    .skip(param_idx)
                    .map(|x| x.quantified.as_gradual_type());
                checked_targs.extend(defaults);
                break;
            }
            name_to_idx.insert(param.name(), param_idx);
        }
        if targ_idx < nargs {
            self.error(
                errors,
                range,
                ErrorKind::BadSpecialization,
                None,
                format!(
                    "Expected {} for `{}`, got {}",
                    count(tparams.len(), "type argument"),
                    name,
                    nargs
                ),
            );
        }
        TArgs::new(checked_targs)
    }
}
