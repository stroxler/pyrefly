/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashMap;

use pyrefly_util::lined_buffer::DisplayRange;
use ruff_python_ast::Expr;
use ruff_python_ast::Stmt;
use ruff_python_ast::identifier::Identifier;
use ruff_python_ast::visitor;
use ruff_python_ast::visitor::Visitor;

use crate::report::pysa::DefinitionRef;
use crate::report::pysa::FunctionId;
use crate::report::pysa::ModuleContext;
use crate::report::pysa::ModuleId;

#[allow(dead_code)]
pub struct CallCallees {
    call_targets: Vec<DefinitionRef>,
}

#[allow(dead_code)]
pub struct CallGraphs(HashMap<DefinitionRef, HashMap<DisplayRange, CallCallees>>);

impl CallGraphs {
    #[allow(dead_code)]
    fn new() -> Self {
        Self(HashMap::new())
    }
}

#[allow(dead_code)]
struct CallGraphVisitor<'a> {
    module_context: &'a ModuleContext<'a>,
    module_id: ModuleId,
    module_name: String,
    function_names: &'a HashMap<FunctionId, String>,
    // A stack where the top element is always the current callable that we are
    // building a call graph for. The stack is updated each time we enter and exit
    // a function definition or a class definition.
    definition_nesting: Vec<DefinitionRef>,
    call_graphs: &'a mut CallGraphs,
    ignore_calls_if_cannot_find_current_definition: bool,
}

impl<'a> Visitor<'a> for CallGraphVisitor<'a> {
    fn visit_stmt(&mut self, stmt: &'a Stmt) {
        match stmt {
            Stmt::FunctionDef(function_def) => {
                let function_id = FunctionId::Function {
                    location: self
                        .module_context
                        .module_info
                        .display_range(function_def.identifier()),
                };
                if let Some(function_name) = self.function_names.get(&function_id) {
                    self.definition_nesting.push(DefinitionRef {
                        module_id: self.module_id,
                        module_name: self.module_name.clone(),
                        function_id,
                        identifier: function_name.clone(),
                    });
                    visitor::walk_stmt(self, stmt);
                    self.definition_nesting.pop();
                } else if !self.ignore_calls_if_cannot_find_current_definition {
                    visitor::walk_stmt(self, stmt);
                }
            }
            Stmt::ClassDef(_class_def) => {
                // TODO: Push the class id into `definition_nesting`
                visitor::walk_stmt(self, stmt);
                // TODO: Pop the class id from `definition_nesting`
            }
            _ => {
                visitor::walk_stmt(self, stmt);
            }
        }
    }

    fn visit_expr(&mut self, expr: &'a Expr) {
        visitor::walk_expr(self, expr);
    }
}

#[allow(dead_code)]
pub fn build_call_graphs_for_module(
    context: &ModuleContext,
    ignore_calls_if_cannot_find_current_definition: bool,
    function_names: &HashMap<FunctionId, String>,
) -> CallGraphs {
    let mut call_graphs = CallGraphs::new();

    let module_name = context.module_info.name().to_string();
    let module_toplevel = DefinitionRef {
        module_id: context.module_id,
        module_name: module_name.clone(),
        function_id: FunctionId::ModuleTopLevel,
        identifier: "$module_top_level".to_owned(),
    };
    let mut visitor = CallGraphVisitor {
        module_context: context,
        module_id: context.module_id,
        module_name: module_name.clone(),
        definition_nesting: vec![module_toplevel],
        call_graphs: &mut call_graphs,
        function_names,
        ignore_calls_if_cannot_find_current_definition,
    };

    for stmt in &context.ast.body {
        visitor.visit_stmt(stmt);
    }
    call_graphs
}
