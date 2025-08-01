/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use ruff_python_ast::Expr;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;

/// Private helper type used to share part of the logic needed for the
/// binding-level work of finding legacy type parameters versus the type-level
/// work of computing inheritance information and the MRO.
#[derive(Debug, Clone)]
pub enum BaseClass {
    TypedDict(TextRange),
    Generic(Vec<Expr>, TextRange),
    Protocol(Vec<Expr>, TextRange),
    Expr(Expr),
    NamedTuple(TextRange),
}

impl BaseClass {
    pub fn can_apply(&self) -> bool {
        matches!(self, BaseClass::Generic(..) | BaseClass::Protocol(..))
    }

    pub fn apply(&mut self, args: Vec<Expr>) {
        match self {
            BaseClass::Generic(xs, ..) | BaseClass::Protocol(xs, ..) => {
                xs.extend(args);
            }
            _ => panic!("cannot apply base class"),
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
