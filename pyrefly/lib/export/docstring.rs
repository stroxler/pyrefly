/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cmp::min;
use std::sync::Arc;

use dupe::Dupe;
use ruff_python_ast::Stmt;

#[derive(Debug, Clone, Dupe)]
pub struct Docstring(Arc<String>);

impl Docstring {
    fn new(docstring: &str) -> Self {
        let mut result = docstring.to_owned();
        result = result.replace("\r", "");
        result = result.replace("\t", "    ");

        // Remove the shortest amount of whitespace from the beginning of each line
        let min_indent = result
            .lines()
            .filter(|line| !line.trim().is_empty())
            .map(|line| line.chars().take_while(|&c| c == ' ').count())
            .min()
            .unwrap_or(0);

        Self(Arc::new(
            result
                .lines()
                .map(|line| &line[min(min_indent, line.len())..])
                .collect::<Vec<_>>()
                .join("\n"),
        ))
    }

    pub fn from_stmts(xs: &[Stmt]) -> Option<Self> {
        xs.first().and_then(|stmt| {
            if let Stmt::Expr(expr_stmt) = stmt
                && let ruff_python_ast::Expr::StringLiteral(string_lit) = &*expr_stmt.value
            {
                return Some(Docstring::new(string_lit.value.to_str()));
            }
            None
        })
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

#[cfg(test)]
mod tests {
    use crate::export::docstring::Docstring;

    #[test]
    fn test_clean_removes_carriage_returns() {
        assert_eq!(Docstring::new("hello\rworld").as_str(), "helloworld");
    }

    #[test]
    fn test_clean_replaces_tabs_with_spaces() {
        assert_eq!(Docstring::new("hello\tworld").as_str(), "hello    world");
    }

    #[test]
    fn test_clean_trims_shortest_whitespace() {
        assert_eq!(
            Docstring::new("  hello\n    world\n  test").as_str(),
            "hello\n  world\ntest"
        );
    }
}
