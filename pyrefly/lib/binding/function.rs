/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::mem;

use itertools::Either;
use ruff_python_ast::AnyParameterRef;
use ruff_python_ast::Decorator;
use ruff_python_ast::ExceptHandler;
use ruff_python_ast::Expr;
use ruff_python_ast::Identifier;
use ruff_python_ast::Parameters;
use ruff_python_ast::Stmt;
use ruff_python_ast::StmtExpr;
use ruff_python_ast::StmtFunctionDef;
use ruff_python_ast::name::Name;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;
use starlark_map::small_map::SmallMap;

use crate::binding::binding::AnnotationTarget;
use crate::binding::binding::Binding;
use crate::binding::binding::BindingAnnotation;
use crate::binding::binding::BindingFunction;
use crate::binding::binding::BindingYield;
use crate::binding::binding::BindingYieldFrom;
use crate::binding::binding::FunctionStubOrImpl;
use crate::binding::binding::IsAsync;
use crate::binding::binding::Key;
use crate::binding::binding::KeyAnnotation;
use crate::binding::binding::KeyClass;
use crate::binding::binding::KeyFunction;
use crate::binding::binding::KeyLegacyTypeParam;
use crate::binding::binding::KeyYield;
use crate::binding::binding::KeyYieldFrom;
use crate::binding::binding::LastStmt;
use crate::binding::binding::ReturnExplicit;
use crate::binding::binding::ReturnImplicit;
use crate::binding::binding::ReturnType;
use crate::binding::bindings::BindingsBuilder;
use crate::binding::bindings::FuncYieldsAndReturns;
use crate::binding::bindings::LegacyTParamBuilder;
use crate::binding::scope::FlowStyle;
use crate::binding::scope::InstanceAttribute;
use crate::binding::scope::Scope;
use crate::binding::scope::ScopeKind;
use crate::config::base::UntypedDefBehavior;
use crate::export::special::SpecialExport;
use crate::graph::index::Idx;
use crate::module::short_identifier::ShortIdentifier;
use crate::ruff::ast::Ast;
use crate::sys_info::SysInfo;
use crate::types::types::Type;
use crate::util::prelude::VecExt;
use crate::util::visit::Visit;

struct Decorators {
    has_no_type_check: bool,
    decorators: Box<[Idx<Key>]>,
}

struct SelfAssignments {
    method_name: Name,
    instance_attributes: SmallMap<Name, InstanceAttribute>,
}

/// Determine whether a function definition is annotated.
/// Used in the `untyped-def-behavior = "skip-and-infer-returns-any"` mode.
fn is_annotated<T>(returns: &Option<T>, params: &Parameters) -> bool {
    if returns.is_some() {
        return true;
    }
    for p in params.iter() {
        if p.annotation().is_some() {
            return true;
        }
    }
    false
}
struct SelfAttrNames<'a> {
    self_name: &'a Name,
    names: SmallMap<Name, TextRange>,
}

impl<'a> SelfAttrNames<'a> {
    fn expr_lvalue(&mut self, x: &Expr) {
        match x {
            Expr::Attribute(x) => {
                if let Expr::Name(v) = x.value.as_ref()
                    && &v.id == self.self_name
                    && !self.names.contains_key(&x.attr.id)
                {
                    self.names.insert(x.attr.id.clone(), x.attr.range());
                }
            }
            Expr::Tuple(x) => {
                for x in &x.elts {
                    self.expr_lvalue(x);
                }
            }

            Expr::List(x) => {
                for x in &x.elts {
                    self.expr_lvalue(x);
                }
            }
            _ => {}
        }
    }

    fn stmt(&mut self, x: &Stmt) {
        match x {
            Stmt::Assign(x) => {
                for e in x.targets.iter() {
                    self.expr_lvalue(e);
                }
            }
            Stmt::AnnAssign(x) => {
                self.expr_lvalue(x.target.as_ref());
            }
            _ => {}
        }
        x.recurse(&mut |x| self.stmt(x))
    }

    /// Given an unannotated method (it is the caller's responsibility to
    /// check these conditions), traverse the body to find the name and range
    /// of `self.<attr>` assignments.
    fn find(
        func_name: &Identifier,
        parameters: &mut Box<Parameters>,
        body: Vec<Stmt>,
    ) -> Option<SelfAssignments> {
        let self_name = if let Some(p) = parameters.iter_non_variadic_params().next() {
            &p.parameter.name.id
        } else {
            return None;
        };
        let mut finder = SelfAttrNames {
            self_name,
            names: SmallMap::new(),
        };
        for x in body.iter() {
            finder.stmt(x);
        }
        let instance_attributes = finder
            .names
            .into_iter()
            .map(|(n, r)| {
                (
                    n,
                    InstanceAttribute(
                        super::binding::ExprOrBinding::Binding(Binding::Type(Type::any_implicit())),
                        None,
                        r,
                    ),
                )
            })
            .collect();
        Some(SelfAssignments {
            method_name: func_name.id.clone(),
            instance_attributes,
        })
    }
}

impl<'a> BindingsBuilder<'a> {
    fn parameters(
        &mut self,
        x: &mut Parameters,
        function_idx: Idx<KeyFunction>,
        class_key: Option<Idx<KeyClass>>,
    ) {
        let mut self_name = None;
        for x in x.iter_non_variadic_params() {
            if class_key.is_some() && self_name.is_none() {
                self_name = Some(x.parameter.name.clone());
            }
            self.bind_function_param(
                AnnotationTarget::Param(x.parameter.name.id.clone()),
                AnyParameterRef::NonVariadic(x),
                function_idx,
                class_key,
            );
        }
        if let Some(box args) = &x.vararg {
            self.bind_function_param(
                AnnotationTarget::ArgsParam(args.name.id.clone()),
                AnyParameterRef::Variadic(args),
                function_idx,
                class_key,
            );
        }
        if let Some(box kwargs) = &x.kwarg {
            self.bind_function_param(
                AnnotationTarget::KwargsParam(kwargs.name.id.clone()),
                AnyParameterRef::Variadic(kwargs),
                function_idx,
                class_key,
            );
        }
        if let Scope {
            kind: ScopeKind::Method(method),
            ..
        } = self.scopes.current_mut()
        {
            method.self_name = self_name;
        }
    }

    fn to_return_annotation_with_range(
        &mut self,
        mut x: Expr,
        func_name: &Identifier,
        class_key: Option<Idx<KeyClass>>,
        tparams_builder: &mut Option<LegacyTParamBuilder>,
    ) -> (TextRange, Idx<KeyAnnotation>) {
        self.ensure_type(&mut x, tparams_builder);
        (
            x.range(),
            self.table.insert(
                KeyAnnotation::ReturnAnnotation(ShortIdentifier::new(func_name)),
                BindingAnnotation::AnnotateExpr(
                    AnnotationTarget::Return(func_name.id.clone()),
                    x,
                    class_key,
                ),
            ),
        )
    }

    fn function_header(
        &mut self,
        x: &mut StmtFunctionDef,
        func_name: &Identifier,
        class_key: Option<Idx<KeyClass>>,
    ) -> (
        Option<(TextRange, Idx<KeyAnnotation>)>,
        Vec<Idx<KeyLegacyTypeParam>>,
    ) {
        let tparams = x
            .type_params
            .as_mut()
            .map(|tparams| self.type_params(tparams));

        let mut legacy = Some(LegacyTParamBuilder::new(tparams.is_some()));

        // We need to bind all the parameters expressions _after_ the type params, but before the parameter names,
        // which might shadow some types.
        for (param, default) in Ast::parameters_iter_mut(&mut x.parameters) {
            self.ensure_type_opt(param.annotation.as_deref_mut(), &mut legacy);
            if let Some(default) = default {
                self.ensure_expr_opt(default.as_deref_mut());
            }
        }

        let return_ann_with_range = mem::take(&mut x.returns).map(|box e| {
            self.to_return_annotation_with_range(e, func_name, class_key, &mut legacy)
        });

        let legacy_tparam_builder = legacy.unwrap();
        legacy_tparam_builder.add_name_definitions(self);
        let legacy_tparams = legacy_tparam_builder.lookup_keys();
        (return_ann_with_range, legacy_tparams)
    }

    /// Handle creating a scope and binding the function body.
    ///
    /// Note that are some aspects of function analysis (such as implicit return analysis) that also depend on the
    /// function body but are not handled here.
    fn function_body_scope(
        &mut self,
        parameters: &mut Box<Parameters>,
        body: Vec<Stmt>,
        range: TextRange,
        func_name: &Identifier,
        function_idx: Idx<KeyFunction>,
        class_key: Option<Idx<KeyClass>>,
    ) -> (FuncYieldsAndReturns, Option<SelfAssignments>) {
        if class_key.is_none() {
            self.scopes.push(Scope::function(range));
        } else {
            self.scopes.push(Scope::method(range, func_name.clone()));
        }
        self.function_yields_and_returns
            .push(FuncYieldsAndReturns::default());
        self.parameters(parameters, function_idx, class_key);
        self.init_static_scope(&body, false);
        self.stmts(body);
        let func_scope = self.scopes.pop();
        let self_assignments = match func_scope.kind {
            ScopeKind::Method(m) => Some(SelfAssignments {
                method_name: m.name.id,
                instance_attributes: m.instance_attributes,
            }),
            _ => None,
        };
        let yields_and_returns = self.function_yields_and_returns.pop().unwrap();
        (yields_and_returns, self_assignments)
    }

    fn unchecked_function_body_scope(
        &mut self,
        parameters: &mut Box<Parameters>,
        body: Vec<Stmt>,
        range: TextRange,
        func_name: &Identifier,
        function_idx: Idx<KeyFunction>,
        class_key: Option<Idx<KeyClass>>,
    ) -> Option<SelfAssignments> {
        // Push a scope to create the parameter keys (but do nothing else with it).
        if class_key.is_none() {
            self.scopes.push(Scope::function(range));
        } else {
            self.scopes.push(Scope::method(range, func_name.clone()));
        }
        self.parameters(parameters, function_idx, class_key);
        self.scopes.pop();
        // If we are in a class, use a simple visiter to find `self.<attr>` assignments.
        if class_key.is_some() {
            SelfAttrNames::find(func_name, parameters, body)
        } else {
            None
        }
    }

    fn implicit_return(&mut self, body: &[Stmt], func_name: &Identifier) -> Idx<Key> {
        let last_exprs = function_last_expressions(body, self.sys_info).map(|x| {
            x.into_map(|(last, x)| {
                (
                    last,
                    // @nocommit(stroxler): clowntown
                    //
                    // This one is relying on an invariant that we're going to type check all
                    // of the statements, and the `function_last_expressions` traversal will
                    // match the bindings traversal.
                    //
                    // There may not be a clean way to protect this one, there's no dataflow
                    // invariant. It should just be extracted into a helper that explains why
                    // this is safe.
                    self.table.types.0.insert(match last {
                        LastStmt::Expr => Key::StmtExpr(x.range()),
                        LastStmt::With(_) => Key::ContextExpr(x.range()),
                    }),
                )
            })
            .into_boxed_slice()
        });
        self.table.insert(
            Key::ReturnImplicit(ShortIdentifier::new(func_name)),
            Binding::ReturnImplicit(ReturnImplicit { last_exprs }),
        )
    }

    /// Handles both checking yield / return expressions and binding the return type.
    ///
    /// The `implicit_return_if_inferring_return_type` argument should be None when
    /// return type inference is disabled; it must be `Some(implicit_return_key)` to
    /// get return type inference.
    fn analyze_return_type(
        &mut self,
        func_name: &Identifier,
        is_async: bool,
        yields_and_returns: FuncYieldsAndReturns,
        return_ann_with_range: Option<(TextRange, Idx<KeyAnnotation>)>,
        implicit_return_if_inferring_return_type: Option<Idx<Key>>,
        stub_or_impl: FunctionStubOrImpl,
        decorators: Box<[Idx<Key>]>,
    ) {
        let is_generator = !yields_and_returns.yields.is_empty();
        let return_ann = return_ann_with_range.as_ref().map(|(_, key)| *key);

        // Collect the keys of explicit returns.
        let return_keys = yields_and_returns
            .returns
            .into_map(|x| {
                self.table.insert(
                    Key::ReturnExplicit(x.range),
                    Binding::ReturnExplicit(ReturnExplicit {
                        annot: return_ann,
                        expr: x.value,
                        is_generator,
                        is_async,
                    }),
                )
            })
            .into_boxed_slice();

        // Collect the keys of yield expressions.
        let yield_keys = yields_and_returns
            .yields
            .into_map(|x| match x {
                Either::Left(x) => {
                    // Add binding to get the send type for a yield expression.
                    Either::Left(
                        self.table
                            .insert(KeyYield(x.range), BindingYield::Yield(return_ann, x)),
                    )
                }
                Either::Right(x) => {
                    // Add binding to get the return type for a yield from expression.
                    Either::Right(self.table.insert(
                        KeyYieldFrom(x.range),
                        BindingYieldFrom::YieldFrom(return_ann, x),
                    ))
                }
            })
            .into_boxed_slice();

        let return_type_binding =
            if let Some(implicit_return) = implicit_return_if_inferring_return_type {
                Binding::ReturnType(Box::new(ReturnType {
                    annot: return_ann_with_range,
                    returns: return_keys,
                    implicit_return,
                    yields: yield_keys,
                    is_async,
                    stub_or_impl,
                    decorators,
                }))
            } else {
                let inferred_any = Binding::Type(Type::any_implicit());
                match return_ann {
                    Some(ann) => Binding::AnnotatedType(ann, Box::new(inferred_any)),
                    None => inferred_any,
                }
            };
        self.table.insert(
            Key::ReturnType(ShortIdentifier::new(func_name)),
            return_type_binding,
        );
    }

    fn mark_as_returns_any(&mut self, func_name: &Identifier) {
        self.table.insert(
            Key::ReturnType(ShortIdentifier::new(func_name)),
            Binding::Type(Type::any_implicit()),
        );
    }

    fn decorators(&mut self, decorator_list: Vec<Decorator>) -> Decorators {
        let has_no_type_check = decorator_list
            .iter()
            .any(|d| self.as_special_export(&d.expression) == Some(SpecialExport::NoTypeCheck));

        let decorators = self
            .ensure_and_bind_decorators(decorator_list)
            .into_boxed_slice();
        Decorators {
            has_no_type_check,
            decorators,
        }
    }

    fn function_body(
        &mut self,
        parameters: &mut Box<Parameters>,
        body: Vec<Stmt>,
        decorators: &Decorators,
        range: TextRange,
        is_async: bool,
        return_ann_with_range: Option<(TextRange, Idx<KeyAnnotation>)>,
        func_name: &Identifier,
        function_idx: Idx<KeyFunction>,
        class_key: Option<Idx<KeyClass>>,
    ) -> (FunctionStubOrImpl, Option<SelfAssignments>) {
        let stub_or_impl = if is_ellipse(&body) {
            FunctionStubOrImpl::Stub
        } else {
            FunctionStubOrImpl::Impl
        };

        let self_assignments = if decorators.has_no_type_check
            || (self.untyped_def_behavior == UntypedDefBehavior::SkipAndInferReturnAny
                && !is_annotated(&return_ann_with_range, parameters))
        {
            self.mark_as_returns_any(func_name);
            self.unchecked_function_body_scope(
                parameters,
                body,
                range,
                func_name,
                function_idx,
                class_key,
            )
        } else {
            match self.untyped_def_behavior {
                UntypedDefBehavior::SkipAndInferReturnAny
                | UntypedDefBehavior::CheckAndInferReturnAny => {
                    let (yields_and_returns, self_assignments) = self.function_body_scope(
                        parameters,
                        body,
                        range,
                        func_name,
                        function_idx,
                        class_key,
                    );
                    self.analyze_return_type(
                        func_name,
                        is_async,
                        yields_and_returns,
                        return_ann_with_range,
                        None, // this disables return type inference
                        stub_or_impl,
                        decorators.decorators.clone(),
                    );
                    self_assignments
                }
                UntypedDefBehavior::CheckAndInferReturnType => {
                    let implicit_return = self.implicit_return(&body, func_name);
                    let (yields_and_returns, self_assignments) = self.function_body_scope(
                        parameters,
                        body,
                        range,
                        func_name,
                        function_idx,
                        class_key,
                    );
                    self.analyze_return_type(
                        func_name,
                        is_async,
                        yields_and_returns,
                        return_ann_with_range,
                        Some(implicit_return),
                        stub_or_impl,
                        decorators.decorators.clone(),
                    );
                    self_assignments
                }
            }
        };

        (stub_or_impl, self_assignments)
    }

    pub fn function_def(&mut self, mut x: StmtFunctionDef) {
        // Get preceding function definition, if any. Used for building an overload type.
        let mut pred_idx = None;
        let mut pred_function_idx = None;
        if let Some(flow) = self.scopes.current().flow.info.get(&x.name.id) {
            if let FlowStyle::FunctionDef(fidx, _) = flow.style {
                pred_idx = Some(flow.key);
                pred_function_idx = Some(fidx);
            }
        }

        let func_name = x.name.clone();
        let function_idx = self
            .table
            .functions
            .0
            .insert(KeyFunction(ShortIdentifier::new(&func_name)));
        let (class_key, class_meta) = match &self.scopes.current().kind {
            ScopeKind::ClassBody(body) => (
                // @nocommit(stroxler): clowntown
                //
                // Same situation and same solution as the one in `expr.rs`: *if* we are in a class
                // body, then we can store the Idx in the scope and then we won't need this insert.
                //
                // In the meantime, it's probably okay
                Some(self.table.classes.0.insert(body.as_class_key())),
                Some(
                    // @nocommit(stroxler): clowntown
                    //
                    // Similar situation as above (every class has both a class key *and* a class metadata key)
                    // The solution is probably similar.
                    self.table
                        .class_metadata
                        .0
                        .insert(body.as_class_metadata_key()),
                ),
            ),
            _ => (None, None),
        };

        self.scopes.push(Scope::annotation(x.range));
        let (return_ann_with_range, legacy_tparams) =
            self.function_header(&mut x, &func_name, class_key);

        let decorators = self.decorators(mem::take(&mut x.decorator_list));

        let (stub_or_impl, self_assignments) = self.function_body(
            &mut x.parameters,
            mem::take(&mut x.body),
            &decorators,
            x.range,
            x.is_async,
            return_ann_with_range,
            &func_name,
            function_idx,
            class_key,
        );

        // Pop the annotation scope to get back to the parent scope, and handle this
        // case where we need to track assignments to `self` from methods.
        self.scopes.pop();
        if let Some(self_assignments) = self_assignments
            && let ScopeKind::ClassBody(body) = &mut self.scopes.current_mut().kind
        {
            body.add_attributes_defined_by_method(
                self_assignments.method_name,
                self_assignments.instance_attributes,
            );
        }

        let function_idx = self.table.insert(
            KeyFunction(ShortIdentifier::new(&func_name)),
            BindingFunction {
                def: x,
                stub_or_impl,
                class_key,
                decorators: decorators.decorators,
                legacy_tparams: legacy_tparams.into_boxed_slice(),
                successor: None,
            },
        );

        if let Some(pred_function_idx) = pred_function_idx {
            // @nocommit(stroxler): clowntown
            //
            // (Note really - using a get is fine... but I should hide this behind an interface)
            //
            let pred_binding = self.table.functions.1.get_mut(pred_function_idx).unwrap();
            pred_binding.successor = Some(function_idx);
        }

        self.bind_definition(
            &func_name,
            Binding::Function(function_idx, pred_idx, class_meta),
            FlowStyle::FunctionDef(function_idx, return_ann_with_range.is_some()),
        );
    }
}

/// Given the body of a function, what are the potential expressions that
/// could be the last ones to be executed, where the function then falls off the end.
///
/// * Return None to say there are branches that fall off the end always.
/// * Return Some([]) to say that we can never reach the end (e.g. always return, raise)
/// * Return Some(xs) to say this set might be the last expression.
fn function_last_expressions<'a>(
    x: &'a [Stmt],
    sys_info: &SysInfo,
) -> Option<Vec<(LastStmt, &'a Expr)>> {
    fn f<'a>(sys_info: &SysInfo, x: &'a [Stmt], res: &mut Vec<(LastStmt, &'a Expr)>) -> Option<()> {
        match x.last()? {
            Stmt::Expr(x) => res.push((LastStmt::Expr, &x.value)),
            Stmt::Return(_) | Stmt::Raise(_) => {}
            Stmt::With(x) => {
                let kind = IsAsync::new(x.is_async);
                for y in &x.items {
                    res.push((LastStmt::With(kind), &y.context_expr));
                }
                f(sys_info, &x.body, res)?;
            }
            Stmt::If(x) => {
                let mut last_test = None;
                for (test, body) in sys_info.pruned_if_branches(x) {
                    last_test = test;
                    f(sys_info, body, res)?;
                }
                if last_test.is_some() {
                    // The final `if` can fall through, so the `if` itself might be the last statement.
                    return None;
                }
            }
            Stmt::Try(x) => {
                if !x.finalbody.is_empty() {
                    f(sys_info, &x.finalbody, res)?;
                } else {
                    if x.orelse.is_empty() {
                        f(sys_info, &x.body, res)?;
                    } else {
                        f(sys_info, &x.orelse, res)?;
                    }
                    for handler in &x.handlers {
                        match handler {
                            ExceptHandler::ExceptHandler(x) => f(sys_info, &x.body, res)?,
                        }
                    }
                    // If we don't have a matching handler, we raise an exception, which is fine.
                }
            }
            Stmt::Match(x) => {
                let mut exhaustive = false;
                for case in x.cases.iter() {
                    f(sys_info, &case.body, res)?;
                    if case.pattern.is_wildcard() || case.pattern.is_irrefutable() {
                        exhaustive = true;
                        break;
                    }
                }
                if !exhaustive {
                    return None;
                }
            }
            _ => return None,
        }
        Some(())
    }

    let mut res = Vec::new();
    f(sys_info, x, &mut res)?;
    Some(res)
}

fn is_docstring(x: &Stmt) -> bool {
    match x {
        Stmt::Expr(StmtExpr {
            value: box Expr::StringLiteral(..),
            ..
        }) => true,
        _ => false,
    }
}

fn is_ellipse(x: &[Stmt]) -> bool {
    match x.iter().find(|x| !is_docstring(x)) {
        Some(Stmt::Expr(StmtExpr {
            value: box Expr::EllipsisLiteral(_),
            ..
        })) => true,
        _ => false,
    }
}
