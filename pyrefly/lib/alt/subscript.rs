/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use pyrefly_python::dunder;
use pyrefly_types::tuple::Tuple;
use ruff_python_ast::Expr;
use ruff_python_ast::ExprSlice;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;

use crate::alt::answers::LookupAnswer;
use crate::alt::answers_solver::AnswersSolver;
use crate::alt::callable::CallArg;
use crate::config::error_kind::ErrorKind;
use crate::error::collector::ErrorCollector;
use crate::error::context::ErrorContext;
use crate::error::context::ErrorInfo;
use crate::types::types::Type;

impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    /// When indexing/slicing tuples with literals, try to infer a more precise type
    pub fn infer_tuple_subscript(
        &self,
        tuple: Tuple,
        index: &Expr,
        range: TextRange,
        errors: &ErrorCollector,
        context: Option<&dyn Fn() -> ErrorContext>,
    ) -> Type {
        let fallback = || {
            self.call_method_or_error(
                &Type::Tuple(tuple.clone()),
                &dunder::GETITEM,
                range,
                &[CallArg::expr(index)],
                &[],
                errors,
                context,
            )
        };
        match index {
            Expr::Slice(slice) => self
                .infer_tuple_slice(&tuple, slice)
                .unwrap_or_else(fallback),
            _ => self
                .infer_tuple_index(&tuple, index, errors)
                .unwrap_or_else(fallback),
        }
    }

    fn infer_tuple_slice(&self, tuple: &Tuple, slice: &ExprSlice) -> Option<Type> {
        if slice.step.is_some() {
            return None;
        }
        match tuple {
            Tuple::Concrete(elts) => self.infer_concrete_slice(elts, &slice.lower, &slice.upper),
            Tuple::Unpacked(box (prefix, middle, suffix)) => {
                self.infer_unpacked_slice(prefix, middle, suffix, &slice.lower, &slice.upper)
            }
            _ => None,
        }
    }

    fn infer_tuple_index(
        &self,
        tuple: &Tuple,
        index: &Expr,
        errors: &ErrorCollector,
    ) -> Option<Type> {
        let idx_type = self.expr_infer(index, errors);
        match &idx_type {
            Type::Literal(lit) if let Some(idx) = lit.as_index_i64() => match tuple {
                Tuple::Concrete(elts) => {
                    self.infer_concrete_index(elts, idx, index.range(), errors)
                }
                Tuple::Unpacked(box (prefix, _middle, suffix)) => {
                    self.infer_unpacked_index(prefix, suffix, idx)
                }
                _ => None,
            },
            _ => None,
        }
    }

    /// Returns Ok(Some(_)) if the expression is a literal integer
    /// Returns Ok(None) if the expression is absent
    /// Returns Err(()) if the expression is present but is not a literal integer
    fn parse_slice_literal(&self, expr: &Option<Box<Expr>>) -> Result<Option<i64>, ()> {
        if let Some(expr) = expr {
            let literal_type = self.expr_infer(expr, &self.error_swallower());
            match &literal_type {
                Type::Literal(lit) => Ok(lit.as_index_i64()),
                _ => Err(()),
            }
        } else {
            Ok(None)
        }
    }

    fn infer_concrete_slice(
        &self,
        elts: &[Type],
        lower_expr: &Option<Box<Expr>>,
        upper_expr: &Option<Box<Expr>>,
    ) -> Option<Type> {
        let lower_literal = self.parse_slice_literal(lower_expr).ok()?.unwrap_or(0);
        let upper_literal = self
            .parse_slice_literal(upper_expr)
            .ok()?
            .unwrap_or(elts.len() as i64);
        if lower_literal <= upper_literal
            && lower_literal >= 0
            && upper_literal >= 0
            && upper_literal <= elts.len() as i64
        {
            Some(Type::concrete_tuple(
                elts[lower_literal as usize..upper_literal as usize].to_vec(),
            ))
        } else {
            None
        }
    }

    fn infer_concrete_index(
        &self,
        elts: &[Type],
        idx: i64,
        range: TextRange,
        errors: &ErrorCollector,
    ) -> Option<Type> {
        let elt_idx = if idx >= 0 {
            idx
        } else {
            elts.len() as i64 + idx
        } as usize;
        if let Some(elt) = elts.get(elt_idx) {
            Some(elt.clone())
        } else {
            Some(self.error(
                errors,
                range,
                ErrorInfo::Kind(ErrorKind::BadIndex),
                format!(
                    "Index {idx} out of range for tuple with {} elements",
                    elts.len()
                ),
            ))
        }
    }

    fn infer_unpacked_slice(
        &self,
        prefix: &[Type],
        middle: &Type,
        suffix: &[Type],
        lower_expr: &Option<Box<Expr>>,
        upper_expr: &Option<Box<Expr>>,
    ) -> Option<Type> {
        let lower_literal = self.parse_slice_literal(lower_expr).ok()?.unwrap_or(0);
        let upper_literal = self.parse_slice_literal(upper_expr).ok()?;
        match (lower_literal, upper_literal) {
            // Positive lower, positive upper
            (lower, Some(upper)) if lower >= 0 && upper >= 0 => {
                let lower_usize = lower as usize;
                let upper_usize = upper as usize;
                if upper_usize <= prefix.len() {
                    // Slice is entirely within prefix
                    Some(Type::concrete_tuple(
                        prefix[lower_usize..upper_usize].to_vec(),
                    ))
                } else if lower_usize <= prefix.len() {
                    // Slice starts in/immediately after prefix and extends beyond
                    let prefix_tail = prefix[lower_usize..].to_vec();
                    Some(Type::Tuple(Tuple::unpacked(
                        prefix_tail,
                        middle.clone(),
                        suffix.to_vec(),
                    )))
                } else {
                    // Slice starts beyond prefix
                    None
                }
            }
            // Positive lower, negative upper
            (lower, Some(upper)) if lower >= 0 && upper < 0 => {
                let lower_usize = lower as usize;
                let neg_upper = (-upper) as usize;
                if neg_upper <= suffix.len() {
                    let prefix_tail = prefix[lower_usize.min(prefix.len())..].to_vec();
                    let suffix_head = suffix[..suffix.len() - neg_upper].to_vec();
                    if lower_usize <= prefix.len() {
                        Some(Type::Tuple(Tuple::unpacked(
                            prefix_tail,
                            middle.clone(),
                            suffix_head,
                        )))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            // No lower, negative upper
            (0, Some(upper)) if upper < 0 => {
                let neg_upper = (-upper) as usize;
                if neg_upper <= suffix.len() {
                    let suffix_head = suffix[..suffix.len() - neg_upper].to_vec();
                    Some(Type::Tuple(Tuple::unpacked(
                        prefix.to_vec(),
                        middle.clone(),
                        suffix_head,
                    )))
                } else {
                    None
                }
            }
            // Positive lower, no upper
            (lower, None) if lower >= 0 && (lower as usize) <= prefix.len() => {
                let lower_usize = lower as usize;
                let prefix_tail = prefix[lower_usize..].to_vec();
                Some(Type::Tuple(Tuple::unpacked(
                    prefix_tail,
                    middle.clone(),
                    suffix.to_vec(),
                )))
            }
            _ => None,
        }
    }

    // Handle a positive index within the prefix or a negative index within the suffix
    fn infer_unpacked_index(&self, prefix: &[Type], suffix: &[Type], idx: i64) -> Option<Type> {
        if idx >= 0 {
            let elt_idx = idx as usize;
            if elt_idx < prefix.len() {
                Some(prefix[elt_idx].clone())
            } else {
                None
            }
        } else {
            let neg_idx = (-idx) as usize;
            if neg_idx <= suffix.len() {
                Some(suffix[suffix.len() - neg_idx].clone())
            } else {
                None
            }
        }
    }
}
