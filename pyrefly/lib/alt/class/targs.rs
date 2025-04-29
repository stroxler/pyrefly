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
use crate::types::quantified::QuantifiedKind;
use crate::types::tuple::Tuple;
use crate::types::types::TParam;
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
            if let Some(arg) = targs.get(targ_idx) {
                // Get next type argument
                match param.quantified.kind() {
                    QuantifiedKind::TypeVarTuple => {
                        // We know that ParamSpec params must be matched by ParamSpec args, so chop off both params and args
                        // at the next ParamSpec when computing how many args the TypeVarTuple should consume.
                        let paramspec_param_idx =
                            self.peek_next_paramspec_param(param_idx + 1, tparams);
                        let paramspec_arg_idx = self.peek_next_paramspec_arg(targ_idx, &targs);
                        let nparams_for_tvt = paramspec_param_idx.unwrap_or(nparams);
                        let nargs_for_tvt = paramspec_arg_idx.unwrap_or(nargs);
                        let args_to_consume = self.num_typevartuple_args_to_consume(
                            param_idx,
                            nparams_for_tvt,
                            targ_idx,
                            nargs_for_tvt,
                        );
                        let new_targ_idx = targ_idx + args_to_consume;
                        checked_targs.push(self.create_next_typevartuple_arg(
                            &targs[targ_idx..new_targ_idx],
                            range,
                            errors,
                        ));
                        targ_idx = new_targ_idx;
                    }
                    QuantifiedKind::ParamSpec if nparams == 1 && !arg.is_kind_param_spec() => {
                        // If the only type param is a ParamSpec and the type argument
                        // is not a parameter expression, then treat the entire type argument list
                        // as a parameter list
                        checked_targs.push(self.create_paramspec_value(&targs));
                        targ_idx = nargs;
                    }
                    QuantifiedKind::ParamSpec => {
                        checked_targs.push(self.create_next_paramspec_arg(arg, range, errors));
                        targ_idx += 1;
                    }
                    QuantifiedKind::TypeVar => {
                        checked_targs.push(self.create_next_typevar_arg(param, arg, range, errors));
                        targ_idx += 1;
                    }
                }
            } else {
                // We've run out of arguments, and we have type parameters left to consume.
                checked_targs.extend(self.consume_remaining_tparams(
                    name,
                    tparams,
                    param_idx,
                    &checked_targs,
                    nargs,
                    &name_to_idx,
                    range,
                    errors,
                ));
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
                    count(nparams, "type argument"),
                    name,
                    nargs
                ),
            );
        }
        TArgs::new(checked_targs)
    }

    fn peek_next_paramspec_param(&self, start_idx: usize, tparams: &TParams) -> Option<usize> {
        for (i, param) in tparams.iter().enumerate().skip(start_idx) {
            if param.quantified.is_param_spec() {
                return Some(i);
            }
        }
        None
    }

    fn peek_next_paramspec_arg(&self, start_idx: usize, args: &[Type]) -> Option<usize> {
        for (i, arg) in args.iter().enumerate().skip(start_idx) {
            if arg.is_kind_param_spec() {
                return Some(i);
            }
        }
        None
    }

    fn num_typevartuple_args_to_consume(
        &self,
        param_idx: usize,
        nparams: usize,
        targ_idx: usize,
        nargs: usize,
    ) -> usize {
        let n_remaining_params = nparams - param_idx - 1;
        let n_remaining_args = nargs - targ_idx;
        n_remaining_args.saturating_sub(n_remaining_params)
    }

    fn create_next_typevartuple_arg(
        &self,
        args: &[Type],
        range: TextRange,
        errors: &ErrorCollector,
    ) -> Type {
        let mut prefix = Vec::new();
        let mut middle = Vec::new();
        let mut suffix = Vec::new();
        for arg in args {
            match arg {
                Type::Unpack(box Type::Tuple(Tuple::Concrete(elts))) => {
                    if middle.is_empty() {
                        prefix.extend_from_slice(elts);
                    } else {
                        suffix.extend_from_slice(elts);
                    }
                }
                Type::Unpack(box t) => {
                    if !suffix.is_empty() {
                        middle.push(Type::Tuple(Tuple::Unbounded(Box::new(self.unions(suffix)))));
                        suffix = Vec::new();
                    } else {
                        middle.push(t.clone())
                    }
                }
                arg => {
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
            }
        }
        match middle.as_slice() {
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
        }
    }

    fn create_paramspec_value(&self, targs: &[Type]) -> Type {
        let params: Vec<Param> = targs.map(|t| Param::PosOnly(t.clone(), Required::Required));
        Type::ParamSpecValue(ParamList::new(params))
    }

    fn create_next_paramspec_arg(
        &self,
        arg: &Type,
        range: TextRange,
        errors: &ErrorCollector,
    ) -> Type {
        if arg.is_kind_param_spec() {
            arg.clone()
        } else {
            self.error(
                errors,
                range,
                ErrorKind::InvalidParamSpec,
                None,
                format!(
                    "Expected a valid ParamSpec expression, got `{}`",
                    self.for_display(arg.clone())
                ),
            );
            Type::Ellipsis
        }
    }

    fn create_next_typevar_arg(
        &self,
        param: &TParam,
        arg: &Type,
        range: TextRange,
        errors: &ErrorCollector,
    ) -> Type {
        match arg {
            Type::Unpack(_) => self.error(
                errors,
                range,
                ErrorKind::BadUnpacking,
                None,
                format!(
                    "Unpacked argument cannot be used for type parameter {}",
                    param.name()
                ),
            ),
            _ => {
                if arg.is_kind_type_var_tuple() {
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
                }
            }
        }
    }

    fn get_tparam_default(
        &self,
        param: &TParam,
        checked_targs: &[Type],
        name_to_idx: &SmallMap<&Name, usize>,
    ) -> Type {
        if let Some(default) = param.default() {
            default.clone().transform(&mut |default| {
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
                        checked_targs[*i].clone()
                    } else {
                        // The default refers to the value of a TypeVar that isn't in scope. We've
                        // already logged an error in TParams::new(); return a sensible default.
                        Type::any_implicit()
                    }
                }
            })
        } else {
            param.quantified.as_gradual_type()
        }
    }

    /// Consume all remaining type parameters after we've run out of arguments.
    fn consume_remaining_tparams(
        &self,
        name: &Name,
        tparams: &TParams,
        param_idx: usize,
        checked_targs: &[Type],
        nargs: usize,
        name_to_idx: &SmallMap<&Name, usize>,
        range: TextRange,
        errors: &ErrorCollector,
    ) -> Vec<Type> {
        let all_remaining_params_can_be_empty = tparams
            .iter()
            .skip(param_idx)
            .all(|x| x.quantified.is_type_var_tuple() || x.default().is_some());
        if !all_remaining_params_can_be_empty {
            self.error(
                errors,
                range,
                ErrorKind::BadSpecialization,
                None,
                format!(
                    "Expected {} for `{}`, got {}",
                    count(tparams.len(), "type argument"),
                    name,
                    nargs,
                ),
            );
        }
        tparams
            .iter()
            .skip(param_idx)
            .map(|x| self.get_tparam_default(x, checked_targs, name_to_idx))
            .collect()
    }
}
