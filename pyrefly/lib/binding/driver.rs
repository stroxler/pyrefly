/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Binding with the shared structured driver (`bindings-driver` feature).
//!
//! The driver from `pyrefly_static_analysis::structured` sequences every suite of the
//! module, decomposes `if`, `while`, `for`, `break` and `continue`, applies the static
//! truth that Pyrefly reports for each `if` test, and attributes jumps to their loop. The
//! hooks below run the binding steps in `binding/structure.rs`, which the legacy walk runs
//! too. Every other statement is bound by `stmt_impl`; the suites nested in it (`with`,
//! `try`, `match`, function and class bodies) come back to the driver.
//!
//! The driver does not own Pyrefly's static scope tables, expression binding (including
//! expression-level control flow), flow merging, narrowing or termination. While the driver
//! is active, the legacy sequencing of the constructs it decomposes panics.

use std::mem;

use dupe::Dupe;
use pyrefly_python::nesting_context::NestingContext;
use pyrefly_python::sys_info::SysInfo;
use pyrefly_static_analysis::structured;
use pyrefly_static_analysis::structured::Arm;
use pyrefly_static_analysis::structured::Coverage;
use pyrefly_static_analysis::structured::Driver;
use pyrefly_static_analysis::structured::Jump;
use pyrefly_static_analysis::structured::LoopHeader;
use pyrefly_static_analysis::structured::Owned;
use pyrefly_static_analysis::structured::Structured;
use ruff_python_ast::Expr;
use ruff_python_ast::ModModule;
use ruff_python_ast::Stmt;
use ruff_text_size::TextRange;
use thin_vec::ThinVec;

use crate::binding::bindings::Bindings;
use crate::binding::bindings::BindingsBuilder;
use crate::binding::scope::LoopExit;
use crate::binding::structure::IfState;
use crate::binding::structure::LoopState;
use crate::binding::structure::SuiteState;
use crate::config::base::InferReturnTypes;
use crate::error::collector::ErrorCollector;
use crate::export::exports::Exports;
use crate::export::exports::LookupExport;
use crate::module::module_info::ModuleInfo;
use crate::solver::solver::Solver;

/// What the driver did while binding one module.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DriverReport {
    /// Structure sequenced by the driver.
    pub coverage: Coverage,
    /// Statements without nested suites, bound by `stmt_impl`.
    pub leaf: usize,
    /// Statements whose own structure `stmt_impl` binds, while the driver sequences their
    /// nested suites: `with`, `try`, `match`, function and class definitions.
    pub mixed: usize,
}

/// The driver's state while it binds a module.
pub struct DriverState {
    driver: Driver<()>,
    /// The nesting context of the suite being driven.
    parent: NestingContext,
    leaf: usize,
    mixed: usize,
}

impl DriverState {
    pub fn new() -> Self {
        Self {
            driver: Driver::default(),
            parent: NestingContext::toplevel(),
            leaf: 0,
            mixed: 0,
        }
    }

    pub fn into_report(self) -> DriverReport {
        DriverReport {
            coverage: self.driver.coverage,
            leaf: self.leaf,
            mixed: self.mixed,
        }
    }
}

/// A suite being driven, with the statement in progress.
pub struct DrivenSuite {
    state: SuiteState,
    may_raise_if_completed: bool,
}

impl Bindings {
    /// Bind a module without the structured driver, as builds without the feature do.
    pub fn new_legacy(
        x: ModModule,
        module_info: ModuleInfo,
        exports: &Exports,
        solver: &Solver,
        lookup: &dyn LookupExport,
        sys_info: SysInfo,
        errors: &ErrorCollector,
        enable_trace: bool,
        check_unannotated_defs: bool,
        analyze_unannotated_for_ide: bool,
        infer_return_types: InferReturnTypes,
        treat_all_caps_as_final: bool,
    ) -> Self {
        Self::build(
            x,
            module_info,
            exports,
            solver,
            lookup,
            sys_info,
            errors,
            enable_trace,
            check_unannotated_defs,
            analyze_unannotated_for_ide,
            infer_return_types,
            treat_all_caps_as_final,
            false,
        )
    }

    /// Bind a module with the structured driver, returning its report.
    pub fn new_with_driver(
        x: ModModule,
        module_info: ModuleInfo,
        exports: &Exports,
        solver: &Solver,
        lookup: &dyn LookupExport,
        sys_info: SysInfo,
        errors: &ErrorCollector,
        enable_trace: bool,
        check_unannotated_defs: bool,
        analyze_unannotated_for_ide: bool,
        infer_return_types: InferReturnTypes,
        treat_all_caps_as_final: bool,
    ) -> (Self, DriverReport) {
        let bindings = Self::build(
            x,
            module_info,
            exports,
            solver,
            lookup,
            sys_info,
            errors,
            enable_trace,
            check_unannotated_defs,
            analyze_unannotated_for_ide,
            infer_return_types,
            treat_all_caps_as_final,
            true,
        );
        let report = bindings
            .driver_report()
            .expect("a driven build has a driver report")
            .clone();
        (bindings, report)
    }
}

impl<'a> BindingsBuilder<'a> {
    fn driver_state(&mut self) -> &mut DriverState {
        self.driver
            .as_mut()
            .expect("the structured driver is active")
    }

    /// Fail if the legacy sequencing of a construct the driver decomposes runs while the
    /// driver is active, which would mean the driver did not own that structure.
    pub fn assert_not_driven(&self, construct: &str) {
        assert!(
            self.driver.is_none(),
            "legacy `{construct}` sequencing ran while the structured driver is active"
        );
    }

    pub fn driven_suite(&mut self, xs: ThinVec<Stmt>, parent: &NestingContext) {
        let outer = mem::replace(&mut self.driver_state().parent, parent.dupe());
        structured::suite(self, xs);
        self.driver_state().parent = outer;
    }

    pub fn driven_region(&mut self, xs: ThinVec<Stmt>, parent: &NestingContext) {
        let outer = mem::replace(&mut self.driver_state().parent, parent.dupe());
        structured::region(self, xs);
        self.driver_state().parent = outer;
    }
}

impl<'a> Structured for BindingsBuilder<'a> {
    type Syntax = Owned;
    type Suite = DrivenSuite;
    type If = IfState;
    type Loop = LoopState;
    /// Pyrefly keeps loop flows in its scopes; the driver's target says whether one exists.
    type Target = ();

    fn driver(&mut self) -> &mut Driver<()> {
        &mut self.driver_state().driver
    }

    fn suite_start(&mut self, suite: &[Stmt]) -> DrivenSuite {
        DrivenSuite {
            state: self.start_suite(suite),
            may_raise_if_completed: false,
        }
    }

    fn absorbs(&mut self, stmt: &Stmt, next: &Stmt) -> bool {
        self.absorbs_next_stmt(stmt, next)
    }

    fn stmt_before(&mut self, suite: &mut DrivenSuite, stmt: &Stmt, next: Option<&Stmt>) {
        self.suite_stmt_before(&mut suite.state, stmt, next.is_some());
        suite.may_raise_if_completed = self.stmt_prelude(stmt);
    }

    fn stmt_after(&mut self, suite: &mut DrivenSuite) {
        self.stmt_postlude(suite.may_raise_if_completed);
        self.suite_stmt_after(&mut suite.state);
    }

    fn suite_end(&mut self, suite: DrivenSuite) {
        self.finish_suite(suite.state);
    }

    fn other_stmt(&mut self, stmt: Stmt) {
        let state = self.driver_state();
        match stmt {
            Stmt::With(_)
            | Stmt::Try(_)
            | Stmt::Match(_)
            | Stmt::FunctionDef(_)
            | Stmt::ClassDef(_) => state.mixed += 1,
            _ => state.leaf += 1,
        }
        let parent = state.parent.dupe();
        self.stmt_impl(stmt, &parent);
    }

    fn if_start(&mut self, range: TextRange, first_test: &mut Expr) -> IfState {
        self.if_header(range, first_test)
    }

    fn arm_start(
        &mut self,
        state: &mut IfState,
        range: TextRange,
        first: bool,
        test: Option<&mut Expr>,
    ) -> Option<bool> {
        self.if_arm_header(state, range, first, test)
    }

    fn arm_skip(&mut self, state: &mut IfState, arm: Arm<Owned>) {
        self.if_arm_skipped(state, &arm.body);
    }

    fn arm_enter(
        &mut self,
        state: &mut IfState,
        range: TextRange,
        test: Option<Expr>,
        body: &[Stmt],
    ) {
        self.if_arm_body_start(state, range, test, body);
    }

    fn arm_exit(&mut self, state: &mut IfState, range: TextRange) {
        self.if_arm_body_end(state, range);
    }

    fn arm_dead(&mut self, state: &mut IfState, arm: Arm<Owned>) {
        self.if_arm_dead(state, arm.test.as_ref(), &arm.body);
    }

    fn if_end(&mut self, state: IfState, exhaustive: bool) {
        self.if_finish(state, exhaustive);
    }

    fn loop_start(&mut self, header: LoopHeader<Owned>, body: &[Stmt]) -> (LoopState, ()) {
        let state = match header {
            LoopHeader::While { range, test } => self.while_header(range, test, body),
            LoopHeader::For {
                range,
                target,
                iter,
                is_async,
            } => self.for_header(range, target, iter, is_async),
        };
        (state, ())
    }

    fn loop_else(&mut self, state: &mut LoopState) {
        self.loop_body_finish(state);
    }

    fn loop_end(&mut self, state: LoopState) {
        self.loop_finish(state);
    }

    fn loop_jump(&mut self, jump: Jump, range: TextRange, target: Option<()>) {
        let exit = match jump {
            Jump::Break => LoopExit::Break,
            Jump::Continue => LoopExit::Continue,
        };
        let in_loop = self.loop_exit_stmt(range, exit);
        assert_eq!(
            in_loop,
            target.is_some(),
            "the structured driver and the scopes disagree on the loop of a jump at {range:?}"
        );
    }
}
