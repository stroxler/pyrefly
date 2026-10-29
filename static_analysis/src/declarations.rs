/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! The declarations of one lexical scope, as a stream of events.
//!
//! [`walk_suite`] and [`walk_child`] decide which syntax belongs to the scope being walked
//! and report every name the scope binds or declares, every name expression it contains,
//! and every nested scope at the point where it is created. A [`DeclarationSink`] interprets the
//! events; it never walks statements or expressions itself.
//!
//! Statements are reported in source order. Within a statement, the walker owns the
//! order and the structural rules that do not depend on the consumer:
//!
//! - the suites of `if`, `while`, `for`, `with`, `try` and `match` belong to the scope,
//!   while function, lambda, class and comprehension bodies are nested scopes;
//! - the names of every `except` clause are declared before the `try` body, and the
//!   captures of every `case` before the case bodies;
//! - an assigned value is visited before its targets; the names an assignment target
//!   binds are declared before the expressions inside it are visited; a deleted target is
//!   visited before its names are declared; a walrus target is declared before its value
//!   is visited;
//! - every name expression is reported with its [`NameSite`], in any context. Names that
//!   are identifiers rather than expressions (`def`, `class`, import, `except`, capture,
//!   `global`, `nonlocal` and parameter names) are reported only as declarations. Name
//!   occurrences are syntactic: they are not the runtime's reads, which lowering models;
//! - a walrus binds in the nearest enclosing scope that is not a comprehension (PEP 572),
//!   but not past a lambda body;
//! - decorators, parameter defaults, class arguments and a comprehension's first iterable
//!   are evaluated in the enclosing scope;
//! - annotations, type parameters and `type` alias values are never visited: they are
//!   evaluated lazily in annotation scopes, which the walker does not model.
//!
//! The consumer owns policy: [`Position`] lets it leave out expression positions, and
//! [`DeclarationSink::static_truth`] lets it prune `if` arms and conditional expressions
//! (a statically false arm is skipped; a statically true arm or an `else` ends the chain).
//! A sink that keeps every role and answers `None` sees what the runtime binds.

use pyrefly_python::ast::Ast;
use pyrefly_util::visit::Visit;
use ruff_python_ast::Alias;
use ruff_python_ast::Comprehension;
use ruff_python_ast::ExceptHandler;
use ruff_python_ast::Expr;
use ruff_python_ast::ExprIf;
use ruff_python_ast::ExprLambda;
use ruff_python_ast::ExprName;
use ruff_python_ast::Identifier;
use ruff_python_ast::Parameters;
use ruff_python_ast::Stmt;
use ruff_python_ast::StmtAnnAssign;
use ruff_python_ast::StmtAssign;
use ruff_python_ast::StmtAugAssign;
use ruff_python_ast::StmtClassDef;
use ruff_python_ast::StmtFunctionDef;
use ruff_python_ast::StmtImportFrom;
use ruff_text_size::TextRange;

/// A site that binds or declares a name in the scope being walked.
#[derive(Debug, Clone, Copy)]
pub enum Declaration<'a> {
    /// `import a.b` binds `a`; `import a.b as c` binds `c`.
    Import(&'a Alias),
    /// `from m import x, y as z`, or `from m import *`, which binds names the walker
    /// cannot know.
    ImportFrom(&'a StmtImportFrom),
    Function(&'a StmtFunctionDef),
    Class(&'a StmtClassDef),
    /// A name that is an entire target of an assignment, `x = value`.
    Assign(&'a ExprName, &'a StmtAssign),
    /// The name target of an augmented assignment, `x += value`.
    AugAssign(&'a ExprName, &'a StmtAugAssign),
    /// The name target of an annotated assignment, with or without a value.
    Annotated(&'a ExprName, &'a StmtAnnAssign),
    /// Any other stored name: part of an assignment target, a `for`, `with` or
    /// comprehension target, or a `type` alias.
    Store(&'a ExprName),
    Delete(&'a ExprName),
    /// A walrus target, reported to the scope it binds in.
    Walrus(&'a ExprName),
    /// The name of an `except` clause.
    Handler(&'a Identifier),
    /// A capture in a `case` pattern.
    Capture(&'a Identifier),
    Global(&'a Identifier),
    Nonlocal(&'a Identifier),
    Parameter(&'a Identifier),
}

/// An expression position whose inclusion is the sink's choice. Leaving a position out
/// skips everything inside it: its reads, its walrus targets and its nested scopes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Position {
    /// The default values of `def` or `lambda` parameters.
    ParameterDefault,
    /// The bases and keywords of a `class` statement.
    ClassArguments,
    /// The test of an `elif` clause. The first test of an `if` is always visited.
    ElifTest,
    /// The guard of a `case` clause.
    MatchGuard,
    /// The exception type of an `except` clause.
    ExceptType,
    /// A target of an augmented assignment that is not a name, such as `a[i] += 1`.
    AugmentedTarget,
}

/// A region of the walk that a sink may want to track. Contexts nest, and every
/// `enter` is followed by an `exit` of the same context.
#[derive(Debug, Clone, Copy)]
pub enum Context<'a> {
    /// The part of a comprehension that the enclosing scope evaluates: the first iterable,
    /// and the walrus targets the comprehension binds in the enclosing scope.
    Comprehension,
    /// A live arm of an `if` statement: its test, and its static truth. An `else` arm has
    /// no test and is statically true.
    IfArm {
        test: Option<&'a Expr>,
        truth: Option<bool>,
    },
}

/// Where a name expression occurs.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NameSite {
    /// An expression the walk visits, including assignment, `for`, `with`, `del` and
    /// comprehension targets and the name of a `type` alias.
    Expression,
    /// The name target of an annotated assignment.
    AnnAssignTarget,
    /// The name target of an augmented assignment.
    AugAssignTarget,
    /// A walrus target, reported to the scope it binds in.
    WalrusTarget,
    /// A name in a value or class pattern of a `case`, such as `Color` in `Color.RED`.
    Pattern,
}

/// Where a test whose static truth is asked about appears.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TestSite {
    If,
    /// The test of a conditional expression, `a if test else b`.
    Conditional,
}

/// A scope nested inside the scope being walked.
#[derive(Debug, Clone, Copy)]
pub enum ChildScope<'a> {
    Function(&'a StmtFunctionDef),
    Lambda(&'a ExprLambda),
    Class(&'a StmtClassDef),
    Comprehension {
        range: TextRange,
        generators: &'a [Comprehension],
        /// The key of a dict comprehension.
        key: Option<&'a Expr>,
        /// The element, or the value of a dict comprehension.
        elt: &'a Expr,
        /// Generator expressions run deferred, other comprehensions inline.
        is_generator: bool,
    },
}

/// The consumer of a scope's events. Only `declare` is required.
pub trait DeclarationSink<'a> {
    fn declare(&mut self, declaration: Declaration<'a>);

    /// A name expression of the scope.
    fn name(&mut self, _name: &'a ExprName, _site: NameSite) {}

    /// Whether to visit an expression in this position.
    fn keeps(&mut self, _position: Position) -> bool {
        true
    }

    fn enter(&mut self, _context: Context<'a>) {}

    fn exit(&mut self, _context: Context<'a>) {}

    fn static_truth(&mut self, _test: &'a Expr, _site: TestSite) -> Option<bool> {
        None
    }

    /// A statement of the scope, after all of its events, including its nested suites.
    fn statement(&mut self, _stmt: &'a Stmt) {}

    /// A nested scope, at the point where the enclosing scope creates it. The sink walks
    /// it with [`walk_child`] if it wants its contents.
    fn child(&mut self, _child: ChildScope<'a>) {}
}

/// Walk a module, class or function body.
pub fn walk_suite<'a>(body: &'a [Stmt], sink: &mut impl DeclarationSink<'a>) {
    Walker {
        sink,
        in_comprehension: false,
    }
    .suite(body)
}

/// Walk the scope a [`ChildScope`] introduces: parameters and body, or the parts of a
/// comprehension that its own scope evaluates. A walrus inside a comprehension is not
/// reported here: the enclosing walk reported it to the scope it binds in.
pub fn walk_child<'a>(child: ChildScope<'a>, sink: &mut impl DeclarationSink<'a>) {
    let mut walker = Walker {
        sink,
        in_comprehension: false,
    };
    match child {
        ChildScope::Function(x) => {
            walker.parameters(&x.parameters);
            walker.suite(&x.body);
        }
        ChildScope::Lambda(x) => {
            if let Some(parameters) = &x.parameters {
                walker.parameters(parameters);
            }
            walker.expr(&x.body);
        }
        ChildScope::Class(x) => walker.suite(&x.body),
        ChildScope::Comprehension {
            generators,
            key,
            elt,
            ..
        } => {
            walker.in_comprehension = true;
            for (i, g) in generators.iter().enumerate() {
                if i > 0 {
                    walker.expr(&g.iter);
                }
                walker.target(&g.target);
                for cond in &g.ifs {
                    walker.expr(cond);
                }
            }
            if let Some(key) = key {
                walker.expr(key);
            }
            walker.expr(elt);
        }
    }
}

struct Walker<'s, S> {
    sink: &'s mut S,
    /// Whether the walked scope is a comprehension, whose walrus targets bind elsewhere.
    in_comprehension: bool,
}

impl<'a, S: DeclarationSink<'a>> Walker<'_, S> {
    fn suite(&mut self, body: &'a [Stmt]) {
        for x in body {
            self.stmt(x);
        }
    }

    fn declare(&mut self, declaration: Declaration<'a>) {
        self.sink.declare(declaration)
    }

    /// Run `f` if the sink keeps `position`.
    fn at(&mut self, position: Position, f: impl FnOnce(&mut Self)) {
        if self.sink.keeps(position) {
            f(self);
        }
    }

    fn within(&mut self, context: Context<'a>, f: impl FnOnce(&mut Self)) {
        self.sink.enter(context);
        f(self);
        self.sink.exit(context);
    }

    /// Declare the names an assignment target binds, then visit it.
    fn target(&mut self, x: &'a Expr) {
        Ast::expr_lvalue(x, &mut |name| self.declare(Declaration::Store(name)));
        self.expr(x);
    }

    fn parameters(&mut self, x: &'a Parameters) {
        for p in x.iter() {
            self.declare(Declaration::Parameter(p.name()));
        }
    }

    fn defaults(&mut self, x: &'a Parameters, mut visit: impl FnMut(&mut Self, &'a Expr)) {
        self.at(Position::ParameterDefault, |w| {
            for p in x.iter_non_variadic_params() {
                if let Some(default) = &p.default {
                    visit(w, default);
                }
            }
        });
    }

    fn stmt(&mut self, x: &'a Stmt) {
        match x {
            Stmt::FunctionDef(f) => {
                for d in &f.decorator_list {
                    self.expr(&d.expression);
                }
                self.defaults(&f.parameters, Self::expr);
                self.declare(Declaration::Function(f));
                self.sink.child(ChildScope::Function(f));
            }
            Stmt::ClassDef(c) => {
                for d in &c.decorator_list {
                    self.expr(&d.expression);
                }
                if let Some(arguments) = &c.arguments {
                    self.at(Position::ClassArguments, |w| {
                        arguments.recurse(&mut |e| w.expr(e))
                    });
                }
                self.declare(Declaration::Class(c));
                self.sink.child(ChildScope::Class(c));
            }
            Stmt::Import(x) => {
                for a in &x.names {
                    self.declare(Declaration::Import(a));
                }
            }
            Stmt::ImportFrom(x) => self.declare(Declaration::ImportFrom(x)),
            Stmt::Global(x) => {
                for n in &x.names {
                    self.declare(Declaration::Global(n));
                }
            }
            Stmt::Nonlocal(x) => {
                for n in &x.names {
                    self.declare(Declaration::Nonlocal(n));
                }
            }
            Stmt::Assign(assign) => {
                self.expr(&assign.value);
                for t in &assign.targets {
                    match t {
                        Expr::Name(name) => {
                            self.declare(Declaration::Assign(name, assign));
                            self.sink.name(name, NameSite::Expression);
                        }
                        t => self.target(t),
                    }
                }
            }
            Stmt::AnnAssign(x) => {
                if let Some(value) = &x.value {
                    self.expr(value);
                }
                match &*x.target {
                    Expr::Name(name) => {
                        self.declare(Declaration::Annotated(name, x));
                        self.sink.name(name, NameSite::AnnAssignTarget);
                    }
                    target => self.target(target),
                }
            }
            Stmt::AugAssign(x) => {
                self.expr(&x.value);
                match &*x.target {
                    Expr::Name(name) => {
                        self.declare(Declaration::AugAssign(name, x));
                        self.sink.name(name, NameSite::AugAssignTarget);
                    }
                    target => self.at(Position::AugmentedTarget, |w| w.expr(target)),
                }
            }
            Stmt::Delete(x) => {
                for t in &x.targets {
                    self.expr(t);
                    Ast::expr_lvalue(t, &mut |name| self.declare(Declaration::Delete(name)));
                }
            }
            Stmt::TypeAlias(x) => {
                if let Expr::Name(name) = &*x.name {
                    self.declare(Declaration::Store(name));
                    self.sink.name(name, NameSite::Expression);
                }
            }
            Stmt::Expr(x) => self.expr(&x.value),
            Stmt::Return(x) => {
                if let Some(value) = &x.value {
                    self.expr(value);
                }
            }
            Stmt::Raise(x) => {
                if let Some(exc) = &x.exc {
                    self.expr(exc);
                }
                if let Some(cause) = &x.cause {
                    self.expr(cause);
                }
            }
            Stmt::Assert(x) => {
                self.expr(&x.test);
                if let Some(msg) = &x.msg {
                    self.expr(msg);
                }
            }
            Stmt::For(x) => {
                self.expr(&x.iter);
                self.target(&x.target);
                self.suite(&x.body);
                self.suite(&x.orelse);
            }
            Stmt::While(x) => {
                self.expr(&x.test);
                self.suite(&x.body);
                self.suite(&x.orelse);
            }
            Stmt::With(x) => {
                for item in &x.items {
                    self.expr(&item.context_expr);
                    if let Some(target) = &item.optional_vars {
                        self.target(target);
                    }
                }
                self.suite(&x.body);
            }
            Stmt::Match(x) => {
                self.expr(&x.subject);
                for case in &x.cases {
                    Ast::pattern_lvalue(&case.pattern, &mut |id| {
                        self.declare(Declaration::Capture(id))
                    });
                }
                for case in &x.cases {
                    case.pattern.recurse(&mut |e| self.pattern_names(e));
                    if let Some(guard) = &case.guard {
                        self.at(Position::MatchGuard, |w| w.expr(guard));
                    }
                    self.suite(&case.body);
                }
            }
            Stmt::Try(x) => {
                for ExceptHandler::ExceptHandler(h) in &x.handlers {
                    if let Some(name) = &h.name {
                        self.declare(Declaration::Handler(name));
                    }
                }
                self.suite(&x.body);
                for ExceptHandler::ExceptHandler(h) in &x.handlers {
                    if let Some(type_) = &h.type_ {
                        self.at(Position::ExceptType, |w| w.expr(type_));
                    }
                    self.suite(&h.body);
                }
                self.suite(&x.orelse);
                self.suite(&x.finalbody);
            }
            Stmt::If(x) => {
                self.expr(&x.test);
                for (i, (test, body)) in Ast::if_branches(x).enumerate() {
                    let truth = match test {
                        None => Some(true),
                        Some(test) => {
                            if i > 0 {
                                self.at(Position::ElifTest, |w| w.expr(test));
                            }
                            self.sink.static_truth(test, TestSite::If)
                        }
                    };
                    if truth == Some(false) {
                        continue;
                    }
                    self.within(Context::IfArm { test, truth }, |w| w.suite(body));
                    if truth == Some(true) {
                        break;
                    }
                }
            }
            Stmt::Pass(_) | Stmt::Break(_) | Stmt::Continue(_) | Stmt::IpyEscapeCommand(_) => {}
        }
        self.sink.statement(x);
    }

    /// Visit an expression the walked scope evaluates.
    fn expr(&mut self, x: &'a Expr) {
        match x {
            Expr::Name(name) => self.sink.name(name, NameSite::Expression),
            Expr::Named(x) => {
                self.walrus(&x.target);
                self.expr(&x.value);
            }
            Expr::Lambda(lambda) => {
                if let Some(parameters) = &lambda.parameters {
                    self.defaults(parameters, Self::expr);
                }
                self.sink.child(ChildScope::Lambda(lambda));
            }
            Expr::If(x) => {
                self.expr(&x.test);
                self.conditional(x, Self::expr);
            }
            Expr::ListComp(c) => self.comprehension(c.range, &c.generators, None, &c.elt, false),
            Expr::SetComp(c) => self.comprehension(c.range, &c.generators, None, &c.elt, false),
            Expr::Generator(c) => self.comprehension(c.range, &c.generators, None, &c.elt, true),
            Expr::DictComp(c) => {
                self.comprehension(c.range, &c.generators, c.key.as_deref(), &c.value, false)
            }
            _ => x.recurse(&mut |x| self.expr(x)),
        }
    }

    /// Declare a walrus target, unless the walked scope is a comprehension, where the
    /// enclosing walk declared it.
    fn walrus(&mut self, target: &'a Expr) {
        if !self.in_comprehension {
            Ast::expr_lvalue(target, &mut |name| {
                self.declare(Declaration::Walrus(name));
                self.sink.name(name, NameSite::WalrusTarget);
            });
        }
    }

    /// Report the names in an expression of a `case` pattern, which can only be a dotted
    /// name, a literal or a mapping key.
    fn pattern_names(&mut self, x: &'a Expr) {
        match x {
            Expr::Name(name) => self.sink.name(name, NameSite::Pattern),
            _ => x.recurse(&mut |x| self.pattern_names(x)),
        }
    }

    /// Visit the branches of `a if test else b` that the sink does not rule out.
    fn conditional(&mut self, x: &'a ExprIf, mut visit: impl FnMut(&mut Self, &'a Expr)) {
        let truth = self.sink.static_truth(&x.test, TestSite::Conditional);
        if truth != Some(false) {
            visit(self, &x.body);
        }
        if truth != Some(true) {
            visit(self, &x.orelse);
        }
    }

    /// The enclosing scope's share of a comprehension: the walrus targets in it and the
    /// first iterable, in source order, then the comprehension's own scope.
    fn comprehension(
        &mut self,
        range: TextRange,
        generators: &'a [Comprehension],
        key: Option<&'a Expr>,
        elt: &'a Expr,
        is_generator: bool,
    ) {
        self.within(Context::Comprehension, |w| {
            if let Some(key) = key {
                w.hoist(key);
            }
            w.hoist(elt);
            for (i, g) in generators.iter().enumerate() {
                if i == 0 {
                    w.expr(&g.iter);
                } else {
                    w.hoist(&g.iter);
                }
                for cond in &g.ifs {
                    w.hoist(cond);
                }
            }
        });
        self.sink.child(ChildScope::Comprehension {
            range,
            generators,
            key,
            elt,
            is_generator,
        });
    }

    /// Find the walrus targets in an expression a nested comprehension evaluates, which
    /// bind in the walked scope. Everything else there belongs to the comprehension.
    fn hoist(&mut self, x: &'a Expr) {
        match x {
            Expr::Named(x) => {
                self.walrus(&x.target);
                self.hoist(&x.value);
            }
            // A lambda's defaults run in the comprehension; its body is another scope.
            Expr::Lambda(lambda) => {
                if let Some(parameters) = &lambda.parameters {
                    self.defaults(parameters, Self::hoist);
                }
            }
            Expr::If(x) => {
                self.hoist(&x.test);
                self.conditional(x, Self::hoist);
            }
            _ => x.recurse(&mut |x| self.hoist(x)),
        }
    }
}
