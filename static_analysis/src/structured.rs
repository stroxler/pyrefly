/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! A structured statement driver over the parsed AST.
//!
//! The driver owns the statement-level structure of Python code: the order of every
//! suite, the decomposition of `if`/`elif`/`else`, `while` and `for` (with `else`), the
//! attribution of `break` and `continue` to their loop, and the loop-free boundary of a
//! function body. A [`Structured`] backend owns the values that flow along that structure
//! (control-flow blocks, type-flow environments, inference keys) and the policy deciding
//! whether a test is statically known.
//!
//! The driver applies a backend's static truth uniformly: a statically false arm is
//! skipped, a statically true arm (or an `else`) ends the chain, makes the fork exhaustive
//! and leaves the remaining arms dead. A backend that answers `None` for every test, such
//! as runtime lowering, gets an unpruned walk.
//!
//! Every other statement goes to [`Structured::other_stmt`]. Nested suites of such statements
//! (`with`, `try`, `match`, class bodies) are expected to come back through [`suite`], and
//! function bodies through [`region`], so that loop attribution and coverage stay with the
//! driver. Expression-level control flow is not decomposed.

use std::marker::PhantomData;
use std::mem;

use pyrefly_python::ast::Ast;
use ruff_python_ast::Expr;
use ruff_python_ast::Stmt;
use ruff_python_ast::Suite;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;

use crate::flow_point::FlowPointId;
use crate::flow_point::FlowPointKind;

/// How a backend holds the AST: by value, so payloads can move into its output without
/// cloning, or by reference.
pub trait Syntax: Sized {
    type Stmt;
    type Expr;
    type Suite: IntoIterator<Item = Self::Stmt>;
    fn stmt(stmt: &Self::Stmt) -> &Stmt;
    fn suite(suite: &Self::Suite) -> &[Stmt];
    fn shape(stmt: Self::Stmt) -> Shape<Self>;
}

/// The AST is consumed.
pub struct Owned;

/// The AST is borrowed for `'a`.
pub struct Borrowed<'a>(PhantomData<&'a ()>);

/// One arm of an `if` statement: the `if` itself, an `elif`, or an `else` (no test).
pub struct Arm<S: Syntax> {
    pub range: TextRange,
    pub test: Option<S::Expr>,
    pub body: S::Suite,
}

/// The loop header, whose evaluation precedes the body.
pub enum LoopHeader<S: Syntax> {
    While {
        range: TextRange,
        test: S::Expr,
    },
    For {
        range: TextRange,
        target: S::Expr,
        iter: S::Expr,
        is_async: bool,
    },
}

/// The statement-level structure the driver decomposes.
pub enum Shape<S: Syntax> {
    If {
        range: TextRange,
        /// Never empty; the first arm always has a test.
        arms: Vec<Arm<S>>,
    },
    Loop {
        header: LoopHeader<S>,
        body: S::Suite,
        orelse: S::Suite,
    },
    Jump(Jump, TextRange),
    /// Handled entirely by the backend.
    Other(S::Stmt),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Jump {
    Break,
    Continue,
}

impl Syntax for Owned {
    type Stmt = Stmt;
    type Expr = Expr;
    type Suite = Suite;

    fn stmt(stmt: &Stmt) -> &Stmt {
        stmt
    }

    fn suite(suite: &Suite) -> &[Stmt] {
        suite
    }

    fn shape(stmt: Stmt) -> Shape<Self> {
        match stmt {
            Stmt::If(x) => Shape::If {
                range: x.range,
                arms: Ast::if_branches_owned(x)
                    .map(|(range, test, body)| Arm { range, test, body })
                    .collect(),
            },
            Stmt::While(x) => Shape::Loop {
                header: LoopHeader::While {
                    range: x.range,
                    test: *x.test,
                },
                body: x.body,
                orelse: x.orelse,
            },
            Stmt::For(x) => Shape::Loop {
                header: LoopHeader::For {
                    range: x.range,
                    target: *x.target,
                    iter: *x.iter,
                    is_async: x.is_async,
                },
                body: x.body,
                orelse: x.orelse,
            },
            Stmt::Break(x) => Shape::Jump(Jump::Break, x.range),
            Stmt::Continue(x) => Shape::Jump(Jump::Continue, x.range),
            x => Shape::Other(x),
        }
    }
}

impl<'a> Syntax for Borrowed<'a> {
    type Stmt = &'a Stmt;
    type Expr = &'a Expr;
    type Suite = &'a [Stmt];

    fn stmt<'b>(stmt: &'b &'a Stmt) -> &'b Stmt {
        stmt
    }

    fn suite<'b>(suite: &'b &'a [Stmt]) -> &'b [Stmt] {
        suite
    }

    fn shape(stmt: &'a Stmt) -> Shape<Self> {
        match stmt {
            Stmt::If(x) => Shape::If {
                range: x.range,
                arms: Ast::if_branches(x)
                    .zip(
                        [x.range]
                            .into_iter()
                            .chain(x.elif_else_clauses.iter().map(|c| c.range)),
                    )
                    .map(|((test, body), range)| Arm { range, test, body })
                    .collect(),
            },
            Stmt::While(x) => Shape::Loop {
                header: LoopHeader::While {
                    range: x.range,
                    test: &x.test,
                },
                body: &x.body,
                orelse: &x.orelse,
            },
            Stmt::For(x) => Shape::Loop {
                header: LoopHeader::For {
                    range: x.range,
                    target: &x.target,
                    iter: &x.iter,
                    is_async: x.is_async,
                },
                body: &x.body,
                orelse: &x.orelse,
            },
            Stmt::Break(x) => Shape::Jump(Jump::Break, x.range),
            Stmt::Continue(x) => Shape::Jump(Jump::Continue, x.range),
            x => Shape::Other(x),
        }
    }
}

/// What the driver did, for coverage reports. Statements are counted once, by shape.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Coverage {
    pub suites: usize,
    pub regions: usize,
    /// Statements handed to [`Structured::other_stmt`].
    pub other: usize,
    pub ifs: usize,
    pub arms_visited: usize,
    /// Arms whose test the backend decided is statically false.
    pub arms_skipped: usize,
    /// Arms after a statically true arm.
    pub arms_dead: usize,
    pub loops: usize,
    pub jumps: usize,
    /// `break`/`continue` statements outside any loop of their region.
    pub jumps_outside_loop: usize,
}

/// Driver state kept by the backend: the loops enclosing the current point of the
/// current region, innermost last, and the coverage counters.
pub struct Driver<T> {
    loops: Vec<T>,
    /// The lexical source suite, populated only while an observer is active.
    source_region: Option<TextRange>,
    /// A definition's body suite, distinguished from suites synthesized for its headers.
    scope_body: Option<TextRange>,
    pub coverage: Coverage,
}

impl<T> Default for Driver<T> {
    fn default() -> Self {
        Self {
            loops: Vec::new(),
            source_region: None,
            scope_body: None,
            coverage: Coverage::default(),
        }
    }
}

type StmtOf<B> = <<B as Structured>::Syntax as Syntax>::Stmt;
type ExprOf<B> = <<B as Structured>::Syntax as Syntax>::Expr;
type SuiteOf<B> = <<B as Structured>::Syntax as Syntax>::Suite;

/// A consumer of the driver. Hooks are called in source order; between `arm_enter` and
/// `arm_exit`, and between `loop_start` and `loop_else`, the driver drives the body.
pub trait Structured: Sized {
    type Syntax: Syntax;
    /// Per-suite state.
    type Suite;
    /// Per-`if` state, from `if_start` to `if_end`.
    type If;
    /// Per-loop state, from `loop_start` to `loop_end`.
    type Loop;
    /// What a `break` or `continue` needs to know about its loop.
    type Target: Clone;

    fn driver(&mut self) -> &mut Driver<Self::Target>;

    /// Enable source-point observation for this traversal. Keep this choice fixed while
    /// driving a suite; the default path allocates no observer storage.
    fn observes_flow_points(&self) -> bool {
        false
    }

    /// Observe a source boundary in traversal order, bracketing the corresponding
    /// backend step. No reachability or type-dependent policy is implied by its presence.
    fn flow_point(&mut self, _point: FlowPointId) {}

    fn suite_start(&mut self, suite: &[Stmt]) -> Self::Suite;
    /// Whether `stmt` absorbs the statement that follows it, dropping `next` from the
    /// suite. The backend may record whatever it needs from `next`.
    fn absorbs(&mut self, stmt: &Stmt, next: &Stmt) -> bool;
    /// Called before each statement of a suite, with the statement that follows it once
    /// any absorbed statement is dropped.
    fn stmt_before(&mut self, suite: &mut Self::Suite, stmt: &Stmt, next: Option<&Stmt>);
    fn stmt_after(&mut self, suite: &mut Self::Suite);
    fn suite_end(&mut self, suite: Self::Suite);

    /// A statement the driver does not decompose.
    fn other_stmt(&mut self, stmt: StmtOf<Self>);

    /// Before any arm. A backend may evaluate the first test here, before forking, or in
    /// `arm_start` like the other tests.
    fn if_start(&mut self, range: TextRange, first_test: &mut ExprOf<Self>) -> Self::If;
    /// Start an arm and evaluate its test, which is `None` for an `else`. Returns the
    /// test's static truth if the backend knows it; ignored for an `else`, which is true.
    fn arm_start(
        &mut self,
        state: &mut Self::If,
        range: TextRange,
        first: bool,
        test: Option<&mut ExprOf<Self>>,
    ) -> Option<bool>;
    /// The started arm is statically false: its body is not driven.
    fn arm_skip(&mut self, state: &mut Self::If, arm: Arm<Self::Syntax>);
    fn arm_enter(
        &mut self,
        state: &mut Self::If,
        range: TextRange,
        test: Option<ExprOf<Self>>,
        body: &[Stmt],
    );
    fn arm_exit(&mut self, state: &mut Self::If, range: TextRange);
    /// An arm after a statically true arm. It was not started.
    fn arm_dead(&mut self, state: &mut Self::If, arm: Arm<Self::Syntax>);
    /// `exhaustive` is true if some visited arm was statically true (or an `else`), so
    /// control cannot pass the statement without running an arm.
    fn if_end(&mut self, state: Self::If, exhaustive: bool);

    fn loop_start(
        &mut self,
        header: LoopHeader<Self::Syntax>,
        body: &[Stmt],
    ) -> (Self::Loop, Self::Target);
    /// After the body, before the `else` suite.
    fn loop_else(&mut self, state: &mut Self::Loop);
    fn loop_end(&mut self, state: Self::Loop);
    /// `target` is the innermost enclosing loop of the current region, if any.
    fn loop_jump(&mut self, jump: Jump, range: TextRange, target: Option<Self::Target>);
}

/// The span is computed from the complete suite, before any backend pruning or absorption.
fn suite_range(body: &[Stmt]) -> Option<TextRange> {
    Some(TextRange::new(
        body.first()?.range().start(),
        body.last()?.range().end(),
    ))
}

fn point<B: Structured>(b: &mut B, range: TextRange, kind: FlowPointKind) {
    if b.observes_flow_points() {
        let region = b
            .driver()
            .source_region
            .expect("an observed nonempty suite has a lexical source region");
        b.flow_point(FlowPointId {
            region,
            range,
            kind,
        });
    }
}

/// Drive a suite. Empty suites emit no points because they have no source occurrence.
pub fn suite<B: Structured>(b: &mut B, suite: SuiteOf<B>) {
    let outer_region = b.driver().source_region;
    let range = b
        .observes_flow_points()
        .then(|| suite_range(B::Syntax::suite(&suite)))
        .flatten();
    if let Some(range) = range {
        if outer_region.is_none() || b.driver().scope_body == Some(range) {
            b.driver().source_region = Some(range);
        }
        point(b, range, FlowPointKind::SuiteEntry);
    }
    b.driver().coverage.suites += 1;
    let mut state = b.suite_start(B::Syntax::suite(&suite));
    let mut iter = suite.into_iter().peekable();
    while let Some(x) = iter.next() {
        if let Some(next) = iter.peek()
            && b.absorbs(B::Syntax::stmt(&x), B::Syntax::stmt(next))
        {
            iter.next();
        }
        b.stmt_before(
            &mut state,
            B::Syntax::stmt(&x),
            iter.peek().map(B::Syntax::stmt),
        );
        statement(b, x);
        b.stmt_after(&mut state);
    }
    b.suite_end(state);
    if let Some(range) = range {
        point(b, range, FlowPointKind::SuiteExit);
    }
    b.driver().source_region = outer_region;
}

/// Drive the body of a function: a suite with no enclosing loop.
pub fn region<B: Structured>(b: &mut B, body: SuiteOf<B>) {
    b.driver().coverage.regions += 1;
    let outer = mem::take(&mut b.driver().loops);
    let outer_region = b.driver().source_region;
    if b.observes_flow_points() {
        b.driver().source_region = suite_range(B::Syntax::suite(&body));
    }
    suite(b, body);
    b.driver().source_region = outer_region;
    b.driver().loops = outer;
}

fn statement<B: Structured>(b: &mut B, x: StmtOf<B>) {
    match B::Syntax::shape(x) {
        Shape::If { range, arms } => if_(b, range, arms),
        Shape::Loop {
            header,
            body,
            orelse,
        } => {
            b.driver().coverage.loops += 1;
            let range = match &header {
                LoopHeader::While { range, .. } | LoopHeader::For { range, .. } => *range,
            };
            point(b, range, FlowPointKind::LoopHeader);
            let (mut state, target) = b.loop_start(header, B::Syntax::suite(&body));
            b.driver().loops.push(target);
            point(b, range, FlowPointKind::LoopBody);
            suite(b, body);
            b.driver().loops.pop();
            point(b, range, FlowPointKind::LoopBackedge);
            b.loop_else(&mut state);
            point(b, range, FlowPointKind::LoopExit);
            if !B::Syntax::suite(&orelse).is_empty() {
                point(b, range, FlowPointKind::LoopElse);
            }
            suite(b, orelse);
            point(b, range, FlowPointKind::LoopJoin);
            b.loop_end(state);
        }
        Shape::Jump(jump, range) => {
            let driver = b.driver();
            driver.coverage.jumps += 1;
            let target = driver.loops.last().cloned();
            if target.is_none() {
                driver.coverage.jumps_outside_loop += 1;
            }
            point(
                b,
                range,
                match jump {
                    Jump::Break => FlowPointKind::Break,
                    Jump::Continue => FlowPointKind::Continue,
                },
            );
            b.loop_jump(jump, range, target);
        }
        Shape::Other(x) => {
            b.driver().coverage.other += 1;
            let scope = if b.observes_flow_points() {
                match B::Syntax::stmt(&x) {
                    Stmt::FunctionDef(f) => Some((f.range, suite_range(&f.body))),
                    Stmt::ClassDef(c) => Some((c.range, suite_range(&c.body))),
                    _ => None,
                }
            } else {
                None
            };
            let outer_scope_body = b.driver().scope_body;
            if let Some((range, body_region)) = scope {
                point(b, range, FlowPointKind::ScopeEntry);
                b.driver().scope_body = body_region;
            }
            b.other_stmt(x);
            if let Some((range, _)) = scope {
                b.driver().scope_body = outer_scope_body;
                point(b, range, FlowPointKind::ScopeExit);
            }
        }
    }
}

fn if_<B: Structured>(b: &mut B, range: TextRange, arms: Vec<Arm<B::Syntax>>) {
    b.driver().coverage.ifs += 1;
    let mut arms = arms.into_iter();
    let mut first = arms.next().expect("an `if` statement has a first arm");
    point(b, range, FlowPointKind::IfFork);
    let mut state = b.if_start(
        range,
        first
            .test
            .as_mut()
            .expect("the first arm of an `if` statement has a test"),
    );
    let mut exhaustive = false;
    let mut is_first = true;
    let mut next = Some(first);
    while let Some(mut arm) = next.take().or_else(|| arms.next()) {
        if arm.test.is_some() {
            point(b, arm.range, FlowPointKind::ArmTest);
        }
        let truth = b.arm_start(&mut state, arm.range, is_first, arm.test.as_mut());
        is_first = false;
        let truth = if arm.test.is_none() {
            Some(true)
        } else {
            truth
        };
        if truth == Some(false) {
            b.driver().coverage.arms_skipped += 1;
            b.arm_skip(&mut state, arm);
            continue;
        }
        b.driver().coverage.arms_visited += 1;
        point(b, arm.range, FlowPointKind::ArmEntry);
        b.arm_enter(&mut state, arm.range, arm.test, B::Syntax::suite(&arm.body));
        suite(b, arm.body);
        point(b, arm.range, FlowPointKind::ArmExit);
        b.arm_exit(&mut state, arm.range);
        if truth == Some(true) {
            exhaustive = true;
            for rest in arms.by_ref() {
                b.driver().coverage.arms_dead += 1;
                b.arm_dead(&mut state, rest);
            }
        }
    }
    point(b, range, FlowPointKind::IfJoin);
    b.if_end(state, exhaustive);
}
