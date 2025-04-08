/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use ruff_python_ast::name::Name;
use ruff_python_ast::Arguments;
use ruff_python_ast::Expr;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;

use crate::alt::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
use crate::alt::callable::CallArg;
use crate::binding::narrow::AtomicNarrowOp;
use crate::binding::narrow::NarrowOp;
use crate::error::collector::ErrorCollector;
use crate::error::kind::ErrorKind;
use crate::types::callable::FunctionKind;
use crate::types::class::ClassType;
use crate::types::literal::Lit;
use crate::types::tuple::Tuple;
use crate::types::type_info::TypeInfo;
use crate::types::types::CalleeKind;
use crate::types::types::Type;
use crate::util::prelude::SliceExt;

impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    // Get the union of all members of an enum, minus the specified member
    fn subtract_enum_member(&self, cls: &ClassType, name: &Name) -> Type {
        let e = self.get_enum_from_class_type(cls).unwrap();
        // Enums derived from enum.Flag cannot be treated as a union of their members
        if e.is_flag {
            return Type::ClassType(cls.clone());
        }
        self.unions(
            self.get_enum_members(cls.class_object())
                .into_iter()
                .filter_map(|f| {
                    if let Lit::Enum(box (_, member_name, _)) = &f
                        && *member_name == *name
                    {
                        None
                    } else {
                        Some(Type::Literal(f))
                    }
                })
                .collect::<Vec<_>>(),
        )
    }

    fn intersect(&self, left: &Type, right: &Type) -> Type {
        // Get our best approximation of ty & right.
        self.distribute_over_union(left, |t| {
            if self.solver().is_subset_eq(right, t, self.type_order()) {
                right.clone()
            } else if self.solver().is_subset_eq(t, right, self.type_order()) {
                t.clone()
            } else {
                Type::never()
            }
        })
    }

    fn intersects(&self, ts: &[Type]) -> Type {
        match ts {
            [] => Type::ClassType(self.stdlib.object().clone()),
            [ty] => ty.clone(),
            [ty0, ty1] => self.intersect(ty0, ty1),
            [ty0, ts @ ..] => self.intersect(ty0, &self.intersects(ts)),
        }
    }

    fn subtract(&self, left: &Type, right: &Type) -> Type {
        self.distribute_over_union(left, |left| {
            // Special is_any check because `Any <: int` as a special case, but would mess up this.
            if !left.is_any() && self.solver().is_subset_eq(left, right, self.type_order()) {
                Type::never()
            } else {
                left.clone()
            }
        })
    }

    fn resolve_narrowing_call(
        &self,
        func: &Expr,
        args: &Arguments,
        errors: &ErrorCollector,
    ) -> Option<AtomicNarrowOp> {
        let func_ty = self.expr_infer(func, errors);
        if args.args.len() > 1 {
            let second_arg = &args.args[1];
            let op = match func_ty.callee_kind() {
                Some(CalleeKind::Function(FunctionKind::IsInstance)) => {
                    Some(AtomicNarrowOp::IsInstance(second_arg.clone()))
                }
                Some(CalleeKind::Function(FunctionKind::IsSubclass)) => {
                    Some(AtomicNarrowOp::IsSubclass(second_arg.clone()))
                }
                _ => None,
            };
            if op.is_some() {
                return op;
            }
        }
        if func_ty.is_typeis() {
            Some(AtomicNarrowOp::TypeIs(func_ty.clone(), args.clone()))
        } else if func_ty.is_typeguard() {
            Some(AtomicNarrowOp::TypeGuard(func_ty.clone(), args.clone()))
        } else {
            None
        }
    }

    fn unwrap_class_object_or_error(
        &self,
        ty: &Type,
        range: TextRange,
        errors: &ErrorCollector,
    ) -> Option<Type> {
        let unwrapped = self.unwrap_class_object_silently(ty);
        if unwrapped.is_none() && !ty.is_any() {
            self.error(
                errors,
                range,
                ErrorKind::InvalidArgument,
                None,
                format!("Expected class object, got {}", ty),
            );
        }
        unwrapped
    }

    fn narrow_isinstance(
        &self,
        left: &Type,
        right: &Type,
        range: TextRange,
        errors: &ErrorCollector,
    ) -> Type {
        if let Some(ts) = as_decomposed_tuple_or_union(right) {
            self.unions(
                ts.iter()
                    .map(|t| self.narrow_isinstance(left, t, range, errors))
                    .collect(),
            )
        } else if let Some(right) = self.unwrap_class_object_or_error(right, range, errors) {
            self.intersect(left, &right)
        } else {
            left.clone()
        }
    }

    fn narrow_is_not_instance(
        &self,
        left: &Type,
        right: &Type,
        range: TextRange,
        errors: &ErrorCollector,
    ) -> Type {
        if let Some(ts) = as_decomposed_tuple_or_union(right) {
            self.intersects(&ts.map(|t| self.narrow_is_not_instance(left, t, range, errors)))
        } else if let Some(right) = self.unwrap_class_object_or_error(right, range, errors) {
            self.subtract(left, &right)
        } else {
            left.clone()
        }
    }

    pub fn atomic_narrow(
        &self,
        ty: &Type,
        op: &AtomicNarrowOp,
        range: TextRange,
        errors: &ErrorCollector,
    ) -> Type {
        match op {
            AtomicNarrowOp::Is(v) => {
                let right = self.expr_infer(v, errors);
                // Get our best approximation of ty & right.
                self.intersect(ty, &right)
            }
            AtomicNarrowOp::IsNot(v) => {
                let right = self.expr_infer(v, errors);
                // Get our best approximation of ty - right.
                self.distribute_over_union(ty, |t| {
                    // Only certain literal types can be compared by identity.
                    match (t, &right) {
                        (
                            _,
                            Type::None | Type::Literal(Lit::Bool(_)) | Type::Literal(Lit::Enum(_)),
                        ) if *t == right => Type::never(),
                        (Type::ClassType(cls), Type::Literal(Lit::Bool(b)))
                            if cls.class_object().has_qname("builtins", "bool") =>
                        {
                            Type::Literal(Lit::Bool(!b))
                        }
                        (
                            Type::ClassType(left_cls),
                            Type::Literal(Lit::Enum(box (right_cls, name, _))),
                        ) if *left_cls == *right_cls => self.subtract_enum_member(left_cls, name),
                        _ => t.clone(),
                    }
                })
            }
            AtomicNarrowOp::IsInstance(v) => {
                let right = self.expr_infer(v, errors);
                self.narrow_isinstance(ty, &right, v.range(), errors)
            }
            AtomicNarrowOp::IsNotInstance(v) => {
                let right = self.expr_infer(v, errors);
                self.narrow_is_not_instance(ty, &right, v.range(), errors)
            }
            AtomicNarrowOp::IsSubclass(v) => {
                let right = self.expr_infer(v, errors);
                if let Some(left) = self.untype_opt(ty.clone(), v.range())
                    && let Some(right) =
                        self.unwrap_class_object_or_error(&right, v.range(), errors)
                {
                    Type::type_form(self.intersect(&left, &right))
                } else {
                    ty.clone()
                }
            }
            AtomicNarrowOp::IsNotSubclass(v) => {
                let right = self.expr_infer(v, errors);
                if let Some(left) = self.untype_opt(ty.clone(), v.range())
                    && let Some(right) =
                        self.unwrap_class_object_or_error(&right, v.range(), errors)
                {
                    Type::type_form(self.subtract(&left, &right))
                } else {
                    ty.clone()
                }
            }
            AtomicNarrowOp::TypeGuard(t, arguments) => {
                if let Some(call_target) = self.as_call_target(t.clone()) {
                    let args = arguments.args.map(|arg| match arg {
                        Expr::Starred(x) => CallArg::Star(&x.value, x.range),
                        _ => CallArg::Expr(arg),
                    });
                    let ret = self.call_infer(call_target, &args, &arguments.keywords, range, errors, None);
                    if let Type::TypeGuard(box t) = ret {
                        return t.clone();
                    }
                }
                ty.clone()
            },
            AtomicNarrowOp::NotTypeGuard(_, _) => ty.clone(),
            AtomicNarrowOp::TypeIs(t, arguments) => {
                if let Some(call_target) = self.as_call_target(t.clone()) {
                    let args = arguments.args.map(|arg| match arg {
                        Expr::Starred(x) => CallArg::Star(&x.value, x.range),
                        _ => CallArg::Expr(arg),
                    });
                    let ret = self.call_infer(call_target, &args, &arguments.keywords, range, errors, None);
                    if let Type::TypeIs(box t) = ret {
                        return self.intersect(ty, &t);
                    }
                }
                ty.clone()
            },
            AtomicNarrowOp::NotTypeIs(t, arguments) => {
                if let Some(call_target) = self.as_call_target(t.clone()) {
                    let args = arguments.args.map(|arg| match arg {
                        Expr::Starred(x) => CallArg::Star(&x.value, x.range),
                        _ => CallArg::Expr(arg),
                    });
                    let ret = self.call_infer(call_target, &args, &arguments.keywords, range, errors, None);
                    if let Type::TypeIs(box t) = ret {
                        return self.subtract(ty, &t);
                    }
                }
                ty.clone()
            },
            AtomicNarrowOp::Truthy | AtomicNarrowOp::Falsy => self.distribute_over_union(ty, |t| {
                let boolval = matches!(op, AtomicNarrowOp::Truthy);
                if t.as_bool() == Some(!boolval) {
                    Type::never()
                } else if matches!(t, Type::ClassType(cls) if cls.class_object().has_qname("builtins", "bool")) {
                    Type::Literal(Lit::Bool(boolval))
                } else {
                    t.clone()
                }
            }),
            AtomicNarrowOp::Eq(v) => {
                let right = self.expr_infer(v, errors);
                if matches!(right, Type::Literal(_) | Type::None) {
                    self.intersect(ty, &right)
                } else {
                    ty.clone()
                }
            }
            AtomicNarrowOp::NotEq(v) => {
                let right = self.expr_infer(v, errors);
                if matches!(right, Type::Literal(_) | Type::None) {
                    self.distribute_over_union(ty, |t| match (t, &right) {
                        (_, _) if *t == right => Type::never(),
                        (Type::ClassType(cls), Type::Literal(Lit::Bool(b)))
                            if cls.class_object().has_qname("builtins", "bool") =>
                        {
                            Type::Literal(Lit::Bool(!b))
                        }
                        (
                            Type::ClassType(left_cls),
                            Type::Literal(Lit::Enum(box (right_cls, name, _))),
                        ) if *left_cls == *right_cls => self.subtract_enum_member(left_cls, name),
                        _ => t.clone(),
                    })
                } else {
                    ty.clone()
                }
            }
            AtomicNarrowOp::Call(func, args) | AtomicNarrowOp::NotCall(func, args) => {
                if let Some(resolved_op) = self.resolve_narrowing_call(func, args, errors) {
                    if matches!(op, AtomicNarrowOp::Call(..)) {
                        self.atomic_narrow(ty, &resolved_op, range, errors)
                    } else {
                        self.atomic_narrow(ty, &resolved_op.negate(), range, errors)
                    }
                } else {
                    ty.clone()
                }
            }
        }
    }

    fn narrow_type(
        &self,
        ty: &Type,
        op: &NarrowOp,
        range: TextRange,
        errors: &ErrorCollector,
    ) -> Type {
        match op {
            NarrowOp::Atomic(None, op) => self.atomic_narrow(ty, op, range, errors),
            NarrowOp::Atomic(Some(_), _) => {
                // TODO(stroxler): Handle attribute narrowing, we ignore it for now.
                ty.clone()
            }
            NarrowOp::And(ops) => {
                let mut ops_iter = ops.iter();
                if let Some(first_op) = ops_iter.next() {
                    let mut ret = self.narrow_type(ty, first_op, range, errors);
                    for next_op in ops_iter {
                        ret = self.narrow_type(&ret, next_op, range, errors);
                    }
                    ret
                } else {
                    ty.clone()
                }
            }
            NarrowOp::Or(ops) => self.unions(ops.map(|op| self.narrow_type(ty, op, range, errors))),
        }
    }

    pub fn narrow(
        &self,
        type_info: &TypeInfo,
        op: &NarrowOp,
        range: TextRange,
        errors: &ErrorCollector,
    ) -> TypeInfo {
        // TODO(stroxler): Push this down to handle actual attribute narrowing.
        TypeInfo::of_ty(self.narrow_type(type_info.ty(), op, range, errors))
    }
}

fn as_decomposed_tuple_or_union(ty: &Type) -> Option<&[Type]> {
    if let Type::Tuple(Tuple::Concrete(ts)) = ty {
        Some(ts)
    } else if let Type::Type(box Type::Union(ts)) = ty {
        Some(ts)
    } else {
        None
    }
}
