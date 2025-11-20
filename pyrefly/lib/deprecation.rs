/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use pyrefly_types::callable::Deprecation;
use ruff_python_ast::Expr;
use ruff_python_ast::ExprAttribute;
use ruff_python_ast::ExprCall;
use ruff_python_ast::ExprStringLiteral;
use vec1::Vec1;
use vec1::vec1;

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
        Some(value.to_string())
    } else {
        None
    }
}

/// Parse a decorator and return its deprecation metadata if it represents `@deprecated`.
pub fn parse_deprecation(e: &Expr) -> Option<Deprecation> {
    let call = e.as_call_expr()?;
    if !is_deprecated_target(&call.func) {
        return None;
    }
    Some(Deprecation::new(extract_message(call)))
}

/// Format a base description using metadata from a parsed decorator.
pub fn format_deprecation(base: String, deprecation: Option<&Deprecation>) -> Vec1<String> {
    match deprecation.and_then(|d| d.message.as_ref().map(|s| s.trim())) {
        Some(msg) if !msg.is_empty() => vec1![base, msg.to_owned()],
        _ => vec1![base],
    }
}
