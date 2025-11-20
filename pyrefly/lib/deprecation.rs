/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use ruff_python_ast::Decorator;
use ruff_python_ast::Expr;
use ruff_python_ast::ExprAttribute;
use ruff_python_ast::ExprCall;
use ruff_python_ast::ExprStringLiteral;

/// Metadata extracted from a `@deprecated` decorator.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeprecatedDecoration {
    pub message: Option<String>,
}

impl DeprecatedDecoration {
    pub fn new(message: Option<String>) -> Self {
        Self { message }
    }
}

fn is_deprecated_target(e: &Expr) -> bool {
    let (base, value) = match e {
        Expr::Name(x) => (None, &x.id),
        Expr::Attribute(ExprAttribute {
            value: box Expr::Name(base),
            attr,
            ..
        }) => (Some(&base.id), &attr.id),
        _ => return false,
    };
    base.is_none_or(|base| base == "warnings" || base == "typing_extensions")
        && value == "deprecated"
}

fn extract_message(call: &ExprCall) -> Option<String> {
    if let Some(arg) = call.arguments.args.first()
        && let Expr::StringLiteral(ExprStringLiteral { value, .. }) = arg
    {
        return Some(value.to_string());
    }
    for kw in &call.arguments.keywords {
        if let Some(keyword) = &kw.arg
            && keyword == "msg"
            && let Expr::StringLiteral(ExprStringLiteral { value, .. }) = &kw.value
        {
            return Some(value.to_string());
        }
    }
    None
}

/// Parse a decorator and return its deprecation metadata if it represents `@deprecated`.
pub fn parse_deprecated_decorator(decorator: &Decorator) -> Option<DeprecatedDecoration> {
    let call = decorator.expression.as_call_expr()?;
    if !is_deprecated_target(&call.func) {
        return None;
    }
    Some(DeprecatedDecoration::new(extract_message(call)))
}

/// Format a base description (`"`foo` is deprecated"`) with an optional detail message.
pub fn format_deprecated_message(base: impl Into<String>, message: Option<&str>) -> String {
    let base = base.into();
    match message.map(str::trim).filter(|msg| !msg.is_empty()) {
        Some(msg) => format!("{base}: {msg}"),
        None => base,
    }
}

/// Format a base description using metadata from a parsed decorator.
pub fn format_deprecated_with_decoration(
    base: impl Into<String>,
    decoration: Option<&DeprecatedDecoration>,
) -> String {
    format_deprecated_message(base, decoration.and_then(|d| d.message.as_deref()))
}
