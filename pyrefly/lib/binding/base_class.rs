/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use pyrefly_python::ast::Ast;
use ruff_python_ast::Expr;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;

use crate::binding::bindings::BindingsBuilder;
use crate::export::special::SpecialExport;

/// Private helper type used to share part of the logic needed for the
/// binding-level work of finding legacy type parameters versus the type-level
/// work of computing inheritance information and the MRO.
#[derive(Debug, Clone)]
pub enum BaseClass {
    TypedDict(TextRange),
    Generic(Box<[Expr]>, TextRange),
    Protocol(Box<[Expr]>, TextRange),
    Expr(Expr),
    NamedTuple(TextRange),
}

impl BaseClass {
    pub fn is_generic(&self) -> bool {
        match self {
            BaseClass::Generic(ts, ..) | BaseClass::Protocol(ts, ..) if !ts.is_empty() => true,
            _ => false,
        }
    }

    pub fn is_typed_dict(&self) -> bool {
        match self {
            BaseClass::TypedDict(_) => true,
            _ => false,
        }
    }
}

impl Ranged for BaseClass {
    fn range(&self) -> TextRange {
        match self {
            BaseClass::TypedDict(range) => *range,
            BaseClass::Generic(_, range) => *range,
            BaseClass::Protocol(_, range) => *range,
            BaseClass::Expr(expr) => expr.range(),
            BaseClass::NamedTuple(range) => *range,
        }
    }
}

impl<'a> BindingsBuilder<'a> {
    pub fn base_class_of(&self, base_expr: Expr) -> BaseClass {
        match self.as_special_export(&base_expr) {
            Some(SpecialExport::TypedDict) => BaseClass::TypedDict(base_expr.range()),
            Some(SpecialExport::TypingNamedTuple) | Some(SpecialExport::CollectionsNamedTuple) => {
                BaseClass::NamedTuple(base_expr.range())
            }
            Some(SpecialExport::Protocol) => BaseClass::Protocol(Box::new([]), base_expr.range()),
            Some(SpecialExport::Generic) => BaseClass::Generic(Box::new([]), base_expr.range()),
            _ => {
                if let Expr::Subscript(subscript) = &base_expr {
                    match self.as_special_export(&subscript.value) {
                        Some(SpecialExport::Protocol) => {
                            return BaseClass::Protocol(
                                Ast::unpack_slice(&subscript.slice).into(),
                                base_expr.range(),
                            );
                        }
                        Some(SpecialExport::Generic) => {
                            return BaseClass::Generic(
                                Ast::unpack_slice(&subscript.slice).into(),
                                base_expr.range(),
                            );
                        }
                        _ => {}
                    }
                }
                BaseClass::Expr(base_expr)
            }
        }
    }
}
