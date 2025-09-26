/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashMap;

use pyrefly_python::ast::Ast;
use pyrefly_python::short_identifier::ShortIdentifier;
use pyrefly_util::visit::Visit;
use ruff_python_ast::Expr;
use ruff_python_ast::ExprName;
use ruff_python_ast::Stmt;
use ruff_python_ast::name::Name;
use ruff_text_size::Ranged;
use serde::Serialize;
use starlark_map::Hashed;

use crate::binding::binding::Key;
use crate::report::pysa::context::ModuleContext;
use crate::report::pysa::location::PysaLocation;
use crate::report::pysa::types::PysaType;

/// Visit the AST of a module and collect information.

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct GlobalVariable {
    #[serde(rename = "type", skip_serializing_if = "Option::is_none")]
    pub type_: Option<PysaType>,
    pub location: PysaLocation,
}

pub struct ModuleAstVisitorResult {
    pub type_of_expression: HashMap<PysaLocation, PysaType>,
    pub global_variables: HashMap<Name, GlobalVariable>,
}

struct VisitorContext<'a> {
    module_context: &'a ModuleContext<'a>,
    type_of_expression: &'a mut HashMap<PysaLocation, PysaType>,
    global_variables: &'a mut HashMap<Name, GlobalVariable>,
}

fn visit_expression(e: &Expr, context: &mut VisitorContext) {
    let range = e.range();

    // If the expression has a type, export it.
    if let Some(type_) = context.module_context.answers.get_type_trace(range) {
        let display_range = context.module_context.module_info.display_range(range);

        assert!(
            context
                .type_of_expression
                .insert(
                    PysaLocation::new(display_range),
                    PysaType::from_type(&type_, context.module_context)
                )
                .is_none(),
            "Found expressions with the same location"
        );
    }

    e.recurse(&mut |e| visit_expression(e, context));
}

fn visit_assign_target(target: &Expr, is_top_level: bool, context: &mut VisitorContext) {
    if !is_top_level {
        return;
    }

    Ast::expr_lvalue(target, &mut |global: &ExprName| {
        let type_ = context
            .module_context
            .bindings
            .key_to_idx_hashed_opt(Hashed::new(&Key::Definition(ShortIdentifier::expr_name(
                global,
            ))))
            .and_then(|idx| context.module_context.answers.get_idx(idx));
        if let Some(type_) = type_.as_ref()
            && type_.ty().is_type_variable()
        {
            // Don't export type variable globals.
            return;
        }
        let location = PysaLocation::new(
            context
                .module_context
                .module_info
                .display_range(global.range()),
        );
        context
            .global_variables
            .entry(global.id.clone())
            .or_insert(GlobalVariable {
                type_: type_.map(|type_| PysaType::from_type(type_.ty(), context.module_context)),
                location,
            });
    });
}

fn visit_statement(stmt: &Stmt, is_top_level: bool, context: &mut VisitorContext) {
    match stmt {
        Stmt::FunctionDef(function_def) => {
            visit_expressions(
                function_def
                    .decorator_list
                    .iter()
                    .map(|decorator| &decorator.expression),
                context,
            );
            visit_expressions(
                function_def
                    .parameters
                    .posonlyargs
                    .iter()
                    .filter_map(|argument| argument.default.as_deref()),
                context,
            );
            visit_expressions(
                function_def
                    .parameters
                    .args
                    .iter()
                    .filter_map(|argument| argument.default.as_deref()),
                context,
            );
            visit_expressions(
                function_def
                    .parameters
                    .kwonlyargs
                    .iter()
                    .filter_map(|argument| argument.default.as_deref()),
                context,
            );
            visit_statements(
                function_def.body.iter(),
                /* is_top_level */ false,
                context,
            );
        }
        Stmt::ClassDef(class_def) => {
            visit_expressions(
                class_def
                    .decorator_list
                    .iter()
                    .map(|decorator| &decorator.expression),
                context,
            );
            if let Some(arguments) = &class_def.arguments {
                visit_expressions(arguments.args.iter(), context);
                visit_expressions(
                    arguments.keywords.iter().map(|keyword| &keyword.value),
                    context,
                );
            }
            visit_statements(
                class_def.body.iter(),
                /* is_top_level */ false,
                context,
            );
        }
        Stmt::Expr(e) => {
            visit_expression(&e.value, context);
        }
        Stmt::Assign(assign) => {
            stmt.visit(&mut |e| visit_expression(e, context));
            for t in &assign.targets {
                visit_assign_target(t, is_top_level, context);
            }
        }
        Stmt::AnnAssign(assign) => {
            stmt.visit(&mut |e| visit_expression(e, context));
            visit_assign_target(&assign.target, is_top_level, context);
        }
        Stmt::AugAssign(assign) => {
            stmt.visit(&mut |e| visit_expression(e, context));
            visit_assign_target(&assign.target, is_top_level, context);
        }
        Stmt::Return(_) | Stmt::Delete(_) | Stmt::Raise(_) => {
            // Statements that only contains expressions, use Visit<Expr>
            stmt.visit(&mut |e| visit_expression(e, context));
        }
        Stmt::For(for_stmt) => {
            visit_expression(&for_stmt.iter, context);
            visit_expression(&for_stmt.target, context);
            visit_statements(for_stmt.body.iter(), is_top_level, context);
            visit_statements(for_stmt.orelse.iter(), is_top_level, context);
        }
        Stmt::While(while_stmt) => {
            visit_expression(&while_stmt.test, context);
            visit_statements(while_stmt.body.iter(), is_top_level, context);
            visit_statements(while_stmt.orelse.iter(), is_top_level, context);
        }
        Stmt::If(if_stmt) => {
            visit_expression(&if_stmt.test, context);
            visit_statements(if_stmt.body.iter(), is_top_level, context);
            for elif_else_clause in &if_stmt.elif_else_clauses {
                if let Some(test) = &elif_else_clause.test {
                    visit_expression(test, context);
                }
                visit_statements(elif_else_clause.body.iter(), is_top_level, context);
            }
        }
        Stmt::With(with_stmt) => {
            for item in &with_stmt.items {
                visit_expression(&item.context_expr, context);
                visit_expressions(item.optional_vars.iter().map(|x| &**x), context);
            }
            visit_statements(with_stmt.body.iter(), is_top_level, context);
        }
        Stmt::Match(match_stmt) => {
            visit_expression(&match_stmt.subject, context);
            for case in &match_stmt.cases {
                if let Some(guard) = &case.guard {
                    visit_expression(guard, context);
                }
                visit_statements(case.body.iter(), is_top_level, context);
            }
        }
        Stmt::Try(try_stmt) => {
            visit_statements(try_stmt.body.iter(), is_top_level, context);
            visit_statements(try_stmt.orelse.iter(), is_top_level, context);
            visit_statements(try_stmt.finalbody.iter(), is_top_level, context);
            for ruff_python_ast::ExceptHandler::ExceptHandler(except_handler) in &try_stmt.handlers
            {
                if let Some(annotation) = &except_handler.type_ {
                    visit_expression(annotation, context);
                }
                visit_statements(except_handler.body.iter(), is_top_level, context);
            }
        }
        Stmt::Assert(assert_stmt) => {
            visit_expression(&assert_stmt.test, context);
            if let Some(msg) = &assert_stmt.msg {
                visit_expression(msg, context);
            }
        }
        Stmt::TypeAlias(_)
        | Stmt::Import(_)
        | Stmt::ImportFrom(_)
        | Stmt::Global(_)
        | Stmt::Nonlocal(_)
        | Stmt::Pass(_)
        | Stmt::Break(_)
        | Stmt::Continue(_)
        | Stmt::IpyEscapeCommand(_) => {
            // do nothing.
        }
    }
}

fn visit_expressions<'a>(
    expressions: impl Iterator<Item = &'a Expr>,
    context: &mut VisitorContext,
) {
    for expr in expressions {
        visit_expression(expr, context);
    }
}

fn visit_statements<'a>(
    statements: impl Iterator<Item = &'a Stmt>,
    is_top_level: bool,
    context: &mut VisitorContext,
) {
    for stmt in statements {
        visit_statement(stmt, is_top_level, context);
    }
}

pub fn visit_module_ast(context: &ModuleContext) -> ModuleAstVisitorResult {
    let mut type_of_expression = HashMap::new();
    let mut global_variables = HashMap::new();
    let mut visitor_context = VisitorContext {
        module_context: context,
        type_of_expression: &mut type_of_expression,
        global_variables: &mut global_variables,
    };

    for stmt in &context.ast.body {
        visit_statement(stmt, /* is_top_level */ true, &mut visitor_context);
    }

    ModuleAstVisitorResult {
        type_of_expression,
        global_variables,
    }
}
