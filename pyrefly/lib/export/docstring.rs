/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use ruff_python_ast::Stmt;

#[derive(Debug, Clone)]
pub struct Docstring(Arc<String>);
impl Docstring {
    pub fn from_stmts(xs: &[Stmt]) -> Option<Self> {
        xs.first().and_then(|stmt| {
            if let Stmt::Expr(expr_stmt) = stmt
                && let ruff_python_ast::Expr::StringLiteral(string_lit) = &*expr_stmt.value
            {
                return Some(Docstring(Arc::new(string_lit.value.to_string())));
            }
            None
        })
    }

    pub fn as_string(&self) -> String {
        self.0.to_string()
    }
}
