/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashMap;

use dashmap::DashMap;
use pyrefly_python::ast::Ast;
use pyrefly_util::lined_buffer::DisplayRange;
use ruff_python_ast::Expr;
use ruff_python_ast::ExprAttribute;
use ruff_python_ast::ExprName;
use ruff_python_ast::Stmt;
use ruff_python_ast::identifier::Identifier;
use ruff_python_ast::visitor;
use ruff_python_ast::visitor::Visitor;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;

use crate::report::pysa::DefinitionRef;
use crate::report::pysa::FunctionId;
use crate::report::pysa::ModuleContext;
use crate::report::pysa::ModuleId;
use crate::state::lsp::FindPreference;

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq)]
pub struct CallCallees<Target> {
    pub(crate) call_targets: Vec<Target>,
}

impl<Target> CallCallees<Target> {
    #[cfg(test)]
    fn map_target<TargetForTest, MapTarget>(&self, map: MapTarget) -> CallCallees<TargetForTest>
    where
        MapTarget: Fn(&Target) -> TargetForTest,
    {
        CallCallees {
            call_targets: self.call_targets.iter().map(map).collect(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq)]
pub struct AttributeAccessCallees<Target> {
    pub(crate) callable_targets: Vec<Target>,
}

impl<Target> AttributeAccessCallees<Target> {
    #[cfg(test)]
    fn map_target<TargetForTest, MapTarget>(
        &self,
        map: MapTarget,
    ) -> AttributeAccessCallees<TargetForTest>
    where
        MapTarget: Fn(&Target) -> TargetForTest,
    {
        AttributeAccessCallees {
            callable_targets: self.callable_targets.iter().map(map).collect(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq)]
pub struct IdentifierCallees<Target> {
    pub(crate) callable_targets: Vec<Target>,
}

impl<Target> IdentifierCallees<Target> {
    #[cfg(test)]
    fn map_target<TargetForTest, MapTarget>(
        &self,
        map: MapTarget,
    ) -> IdentifierCallees<TargetForTest>
    where
        MapTarget: Fn(&Target) -> TargetForTest,
    {
        IdentifierCallees {
            callable_targets: self.callable_targets.iter().map(map).collect(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq)]
pub enum ExpressionCallees<Target> {
    Call(CallCallees<Target>),
    Identifier(IdentifierCallees<Target>),
    AttributeAccess(AttributeAccessCallees<Target>),
}

impl<Target> ExpressionCallees<Target> {
    #[cfg(test)]
    pub fn map_target<TargetForTest, MapTarget>(
        &self,
        map: MapTarget,
    ) -> ExpressionCallees<TargetForTest>
    where
        MapTarget: Fn(&Target) -> TargetForTest,
    {
        match self {
            ExpressionCallees::Call(call_callees) => {
                ExpressionCallees::Call(call_callees.map_target(map))
            }
            ExpressionCallees::Identifier(identifier_callees) => {
                ExpressionCallees::Identifier(identifier_callees.map_target(map))
            }
            ExpressionCallees::AttributeAccess(attribute_access_callees) => {
                ExpressionCallees::AttributeAccess(attribute_access_callees.map_target(map))
            }
        }
    }
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct CallGraphs<Target, Location>(
    pub(crate) HashMap<Target, HashMap<Location, ExpressionCallees<Target>>>,
);

impl<Target, Location> PartialEq for CallGraphs<Target, Location>
where
    Target: PartialEq + Eq + std::hash::Hash,
    Location: PartialEq + Eq + std::hash::Hash,
{
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<Target, Location> Eq for CallGraphs<Target, Location>
where
    Target: PartialEq + Eq + std::hash::Hash,
    Location: PartialEq + Eq + std::hash::Hash,
{
}

impl<Target, Location> CallGraphs<Target, Location> {
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    #[allow(dead_code)]
    pub fn from_map(map: HashMap<Target, HashMap<Location, ExpressionCallees<Target>>>) -> Self {
        Self(map)
    }
}

#[allow(dead_code)]
struct CallGraphVisitor<'a> {
    module_context: &'a ModuleContext<'a>,
    module_id: ModuleId,
    module_name: String,
    function_names: &'a DashMap<ModuleId, HashMap<FunctionId, String>>,
    // A stack where the top element is always the current callable that we are
    // building a call graph for. The stack is updated each time we enter and exit
    // a function definition or a class definition.
    definition_nesting: Vec<DefinitionRef>,
    call_graphs: &'a mut CallGraphs<DefinitionRef, DisplayRange>,
    ignore_calls_if_cannot_find_current_definition: bool,
}

impl<'a> CallGraphVisitor<'a> {
    fn current_definition(&self) -> DefinitionRef {
        self.definition_nesting.last().unwrap().clone()
    }

    fn add_callees(&mut self, location: TextRange, callees: ExpressionCallees<DefinitionRef>) {
        assert!(
            self.call_graphs
                .0
                .entry(self.current_definition())
                .or_default()
                .insert(
                    self.module_context.module_info.display_range(location),
                    callees,
                )
                .is_none(),
            "Adding callees to the same location"
        );
    }

    fn resolve_name(&self, name: &ExprName) -> Vec<DefinitionRef> {
        let identifier = Ast::expr_name_identifier(name.clone());
        self.module_context
            .transaction
            .find_definition_for_name_use(
                self.module_context.handle,
                &identifier,
                &FindPreference::default(),
            )
            .map_or(vec![], |d| vec![d])
            .iter()
            .filter_map(|definition| {
                DefinitionRef::from_find_definition_item_with_docstring(
                    definition,
                    self.function_names,
                    self.module_context,
                )
            })
            .collect::<Vec<_>>()
    }

    fn resolve_attribute_access(&self, attribute: &ExprAttribute) -> Vec<DefinitionRef> {
        self.module_context
            .transaction
            .find_definition_for_attribute(
                self.module_context.handle,
                attribute.value.range(),
                &attribute.attr,
                &FindPreference::default(),
            )
            .iter()
            .filter_map(|definition| {
                DefinitionRef::from_find_definition_item_with_docstring(
                    definition,
                    self.function_names,
                    self.module_context,
                )
            })
            .collect::<Vec<_>>()
    }
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
                if let Some(function_name) = self
                    .function_names
                    .get(&self.module_id)
                    .and_then(|function_names| function_names.get(&function_id).cloned())
                {
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

        match expr {
            Expr::Call(call) => {
                let call_targets = match &*call.func {
                    Expr::Name(name) => self.resolve_name(name),
                    Expr::Attribute(attribute) => self.resolve_attribute_access(attribute),
                    _ => Vec::new(),
                };
                if !call_targets.is_empty() {
                    self.add_callees(
                        expr.range(),
                        ExpressionCallees::Call(CallCallees { call_targets }),
                    );
                }
            }
            Expr::Name(name) => {
                // TODO: Avoid visiting when the parent expression is a `Call`
                let callable_targets = self.resolve_name(name);
                if !callable_targets.is_empty() {
                    self.add_callees(
                        expr.range(),
                        ExpressionCallees::Identifier(IdentifierCallees { callable_targets }),
                    );
                }
            }
            Expr::Attribute(attribute) => {
                // TODO: Avoid visiting when the parent expression is a `Call`
                let callable_targets = self.resolve_attribute_access(attribute);
                if !callable_targets.is_empty() {
                    self.add_callees(
                        expr.range(),
                        ExpressionCallees::AttributeAccess(AttributeAccessCallees {
                            callable_targets,
                        }),
                    );
                }
            }
            _ => (),
        };
    }
}

#[allow(dead_code)]
pub fn build_call_graphs_for_module(
    context: &ModuleContext,
    ignore_calls_if_cannot_find_current_definition: bool,
    function_names: &DashMap<ModuleId, HashMap<FunctionId, String>>,
) -> CallGraphs<DefinitionRef, DisplayRange> {
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
