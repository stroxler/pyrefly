/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Binding of statement structure: suites, `if` statements, loops, `break` and
//! `continue`.
//!
//! Each construct is split into steps around the points where its nested suites are bound.
//! The legacy walk in this file sequences the steps itself. With the `bindings-driver`
//! feature, the shared structured driver sequences the same steps instead (see
//! `binding/driver.rs`). The steps make the binding decisions; a walk only decides which
//! steps run and in what order.

use std::slice;

use pyrefly_graph::index::Idx;
use pyrefly_python::ast::Ast;
use pyrefly_python::nesting_context::NestingContext;
use pyrefly_python::sys_info::SysInfo;
use ruff_python_ast::Expr;
use ruff_python_ast::Stmt;
use ruff_python_ast::StmtIf;
use ruff_python_ast::name::Name;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;
use ruff_text_size::TextSize;
use starlark_map::small_set::SmallSet;
use thin_vec::ThinVec;

use crate::binding::binding::Binding;
use crate::binding::binding::BindingExpect;
use crate::binding::binding::BranchEnd;
use crate::binding::binding::BranchSuite;
use crate::binding::binding::BranchTest;
use crate::binding::binding::ExhaustiveBinding;
use crate::binding::binding::ExhaustivenessKind;
use crate::binding::binding::GateCondition;
use crate::binding::binding::IsAsync;
use crate::binding::binding::Key;
use crate::binding::binding::KeyExpect;
use crate::binding::binding::LastStmt;
use crate::binding::binding::LinkedKey;
use crate::binding::binding::NarrowUseLocation;
use crate::binding::binding::SuiteGate;
use crate::binding::bindings::BindingsBuilder;
use crate::binding::bindings::CurrentIdx;
use crate::binding::bindings::extract_new_defaults;
use crate::binding::bindings::is_empty_generator_yield;
use crate::binding::expr::Usage;
use crate::binding::function::function_last_expressions;
use crate::binding::narrow::NarrowOps;
use crate::binding::scope::LoopExit;
use crate::binding::scope::LoopTail;
use crate::binding::scope::TerminationKind;
use crate::config::error_kind::ErrorKind;
use crate::export::special::SpecialExport;

/// The reachability bookkeeping of one suite.
///
/// A suite has at most two dead regions, and the certain one is always last: nothing after
/// a definite exit is live, so no gate can open past it. They are kept disjoint so the same
/// code is never blamed twice.
pub struct SuiteState {
    suite_end: Option<TextSize>,
    gates: Vec<SuiteGate>,
    gated_end: Option<TextSize>,
    certain_start: Option<TextSize>,
    prev_end: Option<TextSize>,
    /// Whether the statement being bound is an expression statement.
    is_expr_stmt: bool,
    /// The path endings of the statement being bound, if it may open a divergence gate.
    paths: Option<Box<[Box<[(LastStmt, Idx<Key>)]>]>>,
}

/// The state of an `if` statement while its arms are bound.
pub struct IfState {
    range: TextRange,
    was_definitely_unreachable: bool,
    /// Every test is checked by one `BranchSuiteReachability` binding, reserved before the
    /// tests so that a link to it can own the lambdas in the tests.
    reachability_idx: Idx<KeyExpect>,
    tests_link: CurrentIdx,
    tests_usage: Usage,
    /// Type narrowing operations that are carried over from one branch to the next. For
    /// example, in:
    ///   if x is None:
    ///     pass
    ///   else:
    ///     pass
    /// x is bound to Narrow(x, Is(None)) in the if branch, and the negation,
    /// Narrow(x, IsNot(None)), is carried over to the else branch.
    negated_prev_ops: NarrowOps,
    branch_suites: Vec<BranchSuite>,
    /// The `BranchTest` of the latest clause bound so far, which decides whether control
    /// reaches the next clause.
    previous_test: Option<Idx<Key>>,
    contains_environment_test_with_no_else: bool,
    following_runtime_only_branch: bool,
    arm: ArmState,
    /// After a statically chosen arm: whether every remaining suite is dead in every
    /// environment.
    report_all_remaining: bool,
}

/// The arm of an `if` statement being bound.
#[derive(Default)]
struct ArmState {
    chosen: Option<bool>,
    test_is_environment_independent: bool,
    is_type_checking_branch: bool,
    /// The test deciding whether this suite runs, and the value for which it runs.
    decided_by: Option<(Idx<Key>, bool)>,
}

/// The state of a `while` or `for` loop while its suites are bound.
pub struct LoopState {
    range: TextRange,
    narrow_ops: NarrowOps,
    /// A statically true `while` test: only a `break` leaves the loop.
    is_while_true: bool,
    loop_definitely_runs: bool,
    /// A statically true `while` test that depends on the environment, in a reachable flow:
    /// another Python environment can skip the loop.
    environment_while_true: bool,
    /// The termination flags saved around a statically false `while` body, and whether
    /// binding that body owns the unreachable suite.
    dead_body: Option<((bool, bool), bool)>,
    tail: Option<LoopTail>,
}

impl<'a> BindingsBuilder<'a> {
    pub fn start_suite(&self, xs: &[Stmt]) -> SuiteState {
        SuiteState {
            suite_end: xs.last().map(|x| x.range().end()),
            gates: Vec::new(),
            gated_end: None,
            certain_start: None,
            prev_end: None,
            is_expr_stmt: false,
            paths: None,
        }
    }

    /// Whether `x` absorbs the statement after it: a namedtuple call followed by an
    /// assignment of its `__new__.__defaults__`, which namedtuple synthesis consumes.
    pub fn absorbs_next_stmt(&mut self, x: &Stmt, next: &Stmt) -> bool {
        if let Stmt::Assign(assign) = x
            && let [Expr::Name(name)] = assign.targets.as_slice()
            && let Expr::Call(call) = assign.value.as_ref()
            && let Some(defaults) = extract_new_defaults(next, &name.id)
            && let Some(special) = self.as_special_export(&call.func)
            && matches!(
                special,
                SpecialExport::TypingNamedTuple | SpecialExport::CollectionsNamedTuple
            )
        {
            self.adjacent_namedtuple_defaults = Some(defaults);
            true
        } else {
            false
        }
    }

    /// Reachability bookkeeping before binding `x`; `has_next` says whether a statement
    /// follows it in the suite.
    pub fn suite_stmt_before(&mut self, suite: &mut SuiteState, x: &Stmt, has_next: bool) {
        // A leading `yield` is load-bearing and never blamed, so a region starts past it.
        // See `is_empty_generator_yield`.
        let is_yield = is_empty_generator_yield(x);
        let can_open_region =
            !is_yield && !self.in_unreachable_suite && suite.certain_start.is_none();
        // A statement that begins the certain region cannot also begin a gated one: the
        // gated region ends where the certain one starts, so it would be empty.
        let starts_certain_region = can_open_region && self.scopes.is_definitely_unreachable();
        // Set while binding an earlier statement that control passes only conditionally.
        // This statement begins that gate's region; a leading `yield` leaves the value
        // pending so the region starts past it too.
        if !is_yield
            && let Some(condition) = self.pending_gate.take()
            && can_open_region
            && !starts_certain_region
        {
            suite.gates.push(SuiteGate {
                condition,
                start: x.range().start(),
            });
        }
        if starts_certain_region {
            suite.certain_start = Some(x.range().start());
            // The gated region stops where the certain one takes over, so the two abut
            // rather than overlap.
            suite.gated_end = suite.prev_end;
            self.in_unreachable_suite = true;
        }
        suite.prev_end = Some(x.range().end());
        // A `with` body can leave `last_stmt_expr` set, and the `with` arm already gates on
        // it, so only an expression statement of this suite opens a divergence gate.
        suite.is_expr_stmt = matches!(x, Stmt::Expr(_));
        // Every path through a compound statement may end in an expression, which makes the
        // statement itself a gate. The keys are promised before binding, and the walker
        // prunes the same statically-dead branches the binder abandons, so each one is
        // fulfilled by binding `x` below. An empty list means every path terminates
        // syntactically, which the flow already knows.
        //
        // A gate opened by the last statement of a suite is dropped unread, so finding one
        // is wasted work. It is worth skipping rather than discarding: the walk covers the
        // whole region reachable along `x`'s last statement, and binding descends into that
        // same region and walks it again for each statement nested inside, so a suite whose
        // last statement is a deep nest of compound statements is rescanned once per level.
        suite.paths = (!suite.is_expr_stmt && has_next)
            .then(|| function_last_expressions(slice::from_ref(x), self.sys_info))
            .flatten()
            // An answer the environment decided holds for this configuration only, and a
            // suite dead under one configuration is live under another. An exhaustive path
            // is refused for a different reason: its deadness comes from narrowing, which
            // `assert_never` deliberately relies on, so reporting it condemns the idiom.
            .filter(|paths| {
                !paths.decided_by_environment
                    && !paths.clauses.is_empty()
                    && paths.clauses.iter().any(|c| !c.is_empty())
                    && !paths.clauses.iter().any(|clause| {
                        clause
                            .iter()
                            .any(|(last, _)| matches!(last, LastStmt::Exhaustive(..)))
                    })
            })
            .map(|paths| {
                paths
                    .clauses
                    .into_iter()
                    .map(|clause| {
                        clause
                            .into_iter()
                            .map(|(last, e)| {
                                let idx =
                                    self.last_statement_idx_for_implicit_return(last.clone(), e);
                                (last, idx)
                            })
                            .collect::<Box<[_]>>()
                    })
                    .collect::<Box<[_]>>()
            });
    }

    /// Reachability bookkeeping after binding a statement.
    pub fn suite_stmt_after(&mut self, suite: &mut SuiteState) {
        // A gate still pending was skipped by the `yield` carve-out above and must survive to
        // the statement that carve-out is holding it for. A `yield` cannot diverge anyway.
        if self.pending_gate.is_none() {
            if suite.is_expr_stmt
                && let Some(result) = self.scopes.last_stmt_expr()
            {
                self.pending_gate = Some(GateCondition::ExpressionReturns { result });
            } else if let Some(last_exprs) = suite.paths.take() {
                self.pending_gate = Some(GateCondition::SomePathReturns { last_exprs });
            }
        }
        suite.paths = None;
        self.adjacent_namedtuple_defaults = None;
    }

    pub fn finish_suite(&mut self, suite: SuiteState) {
        // A gate in the final position has no following code to judge.
        self.pending_gate = None;
        let SuiteState {
            suite_end,
            mut gates,
            gated_end,
            certain_start,
            ..
        } = suite;
        // Without a certain region the gated one runs to the end of the suite; with one it
        // stops where that takes over, since the certain region is reported on its own.
        let gated_end = if certain_start.is_some() {
            gated_end
        } else {
            suite_end
        };
        // Every gate must open before the region ends, or it describes nothing. Construction
        // above upholds that; dropping any that do not keeps the range handed to the solver
        // well formed rather than relying on it.
        if let Some(end) = gated_end {
            gates.retain(|gate| gate.start < end);
        }
        if let Some(first) = gates.first()
            && let Some(end) = gated_end
        {
            self.insert_binding(
                KeyExpect::GatedSuiteReachability(TextRange::new(first.start, end)),
                BindingExpect::GatedSuiteReachability {
                    gates: gates.into_boxed_slice(),
                    end,
                },
            );
        }
        if let (Some(start), Some(end)) = (certain_start, suite_end) {
            self.error(
                TextRange::new(start, end),
                ErrorKind::Unreachable,
                "This code is unreachable".to_owned(),
            );
        }
        if certain_start.is_some() {
            self.in_unreachable_suite = false;
        }
    }

    /// Bind an `if` statement, sequencing its steps without the structured driver.
    pub fn if_stmt(&mut self, x: StmtIf, parent: &NestingContext) {
        #[cfg(feature = "bindings-driver")]
        self.assert_not_driven("if");
        let range = x.range;
        let mut arms = Ast::if_branches_owned(x);
        let (first_range, mut first_test, first_body) =
            arms.next().expect("an `if` statement has a first arm");
        let mut state = self.if_header(
            range,
            first_test
                .as_mut()
                .expect("the first arm of an `if` statement has a test"),
        );
        let mut exhaustive = false;
        let mut first = true;
        let mut next = Some((first_range, first_test, first_body));
        while let Some((arm_range, mut test, body)) = next.take().or_else(|| arms.next()) {
            let chosen = self.if_arm_header(&mut state, arm_range, first, test.as_mut());
            first = false;
            if chosen == Some(false) {
                self.if_arm_skipped(&mut state, &body);
                continue;
            }
            self.if_arm_body_start(&mut state, arm_range, test, &body);
            self.stmts(body, parent);
            self.if_arm_body_end(&mut state, arm_range);
            if chosen == Some(true) {
                // We definitely picked this branch if we got here, nothing below is reachable.
                for (_, test, body) in arms.by_ref() {
                    self.if_arm_dead(&mut state, test.as_ref(), &body);
                }
                exhaustive = true;
            }
        }
        self.if_finish(state, exhaustive);
    }

    /// Bind what precedes the arms of an `if` statement.
    pub fn if_header(&mut self, range: TextRange, first_test: &mut Expr) -> IfState {
        let was_definitely_unreachable = self.scopes.is_definitely_unreachable();
        let reachability_idx = self.idx_for_promise(KeyExpect::BranchSuiteReachability(range));
        let tests_link = self.declare_current_idx(Key::UsageLink(range));
        let tests_usage = Usage::NonPinningValue(Some(tests_link.idx()));
        // Process the first `if` test before forking so that walrus-defined names are in the
        // base flow and visible after the if-statement. This mirrors the fix for ternary
        // expressions in expr.rs (Expr::If handling).
        self.ensure_expr(first_test, &mut tests_usage.clone());
        self.start_fork(range);
        IfState {
            range,
            was_definitely_unreachable,
            reachability_idx,
            tests_link,
            tests_usage,
            negated_prev_ops: NarrowOps::new(),
            branch_suites: Vec::new(),
            previous_test: None,
            contains_environment_test_with_no_else: false,
            following_runtime_only_branch: false,
            arm: ArmState::default(),
            report_all_remaining: false,
        }
    }

    /// Start an arm and bind its test, which is `None` for an `else`. Returns whether the
    /// arm is statically chosen (`Some(true)`, always for an `else`) or statically skipped
    /// (`Some(false)`).
    pub fn if_arm_header(
        &mut self,
        state: &mut IfState,
        range: TextRange,
        first: bool,
        mut test: Option<&mut Expr>,
    ) -> Option<bool> {
        self.start_branch();
        self.bind_narrow_ops(
            &state.negated_prev_ops,
            NarrowUseLocation::Start(range),
            &Usage::NonPinningValue(None),
        );
        let chosen = match &test {
            None => {
                state.contains_environment_test_with_no_else = false;
                Some(true)
            }
            Some(x) => self.sys_info.evaluate_bool(x),
        };
        // The first `if` test was already processed before the fork.
        if !first {
            self.ensure_expr_opt(test.as_deref_mut(), &mut state.tests_usage.clone());
            // Lift walrus-defined names from the elif condition into the fork's base flow.
            // The elif condition always executes when control reaches past the preceding
            // branch, so any walrus bindings must be visible after the if/elif block.
            if test.is_some() {
                self.scopes.propagate_new_flow_entries_to_fork_base();
            }
        }
        let test = test.as_deref();
        let later_branches_are_type_checking =
            test.is_some_and(SysInfo::is_not_type_checking_guard);
        // A suite is only dead everywhere if its test never consults the runtime environment.
        // A `sys.version_info`, `sys.platform`, `os.name`, or `TYPE_CHECKING` guard is dead
        // under this configuration alone, and the suite is live under another, so reporting
        // it would be a false positive. An `else` has no test of its own and inherits the
        // ones above it.
        let test_is_environment_independent =
            test.is_none_or(|test| !SysInfo::depends_on_sys_info(test));
        let is_type_checking_branch = (test.is_none() && state.following_runtime_only_branch)
            || test.is_some_and(SysInfo::is_type_checking_guard);
        // Record this before the arm can be skipped: a `not TYPE_CHECKING` guard always
        // evaluates statically to `false`, so its branch is skipped, yet the following `else`
        // branch must still be treated as type-checking-only.
        state.following_runtime_only_branch |= later_branches_are_type_checking;
        state.arm = ArmState {
            chosen,
            test_is_environment_independent,
            is_type_checking_branch,
            decided_by: None,
        };
        chosen
    }

    /// The started arm is statically skipped. Its body is typically not statically
    /// analyzable (e.g. a sys version, platform, or TYPE_CHECKING check).
    pub fn if_arm_skipped(&mut self, state: &mut IfState, body: &[Stmt]) {
        if state.arm.test_is_environment_independent {
            self.report_unreachable_body(body);
        }
        // Python determines generator status syntactically at compile time, regardless of
        // reachability, so a skipped body still matters for `yield`.
        if Ast::body_contains_yield(body) {
            self.scopes.mark_has_yield_in_dead_code();
        }
        self.abandon_branch();
    }

    /// Bind what precedes the body of a started arm.
    pub fn if_arm_body_start(
        &mut self,
        state: &mut IfState,
        range: TextRange,
        test: Option<Expr>,
        body: &[Stmt],
    ) {
        let new_narrow_ops = NarrowOps::from_expr(self, test.as_ref());
        // The solver decides from the declarations in each test whether this suite is dead.
        // An environment-independent test that is always true also makes every suite below
        // it dead.
        let this_test = test.map(|test| {
            self.insert_binding(
                Key::BranchTest(test.range()),
                Binding::BranchTest(Box::new(BranchTest {
                    test,
                    previous: state.previous_test,
                    is_environment_independent: state.arm.test_is_environment_independent,
                })),
            )
        });
        state.branch_suites.push(BranchSuite {
            range: self.unreachable_body_range(body),
            test: this_test,
        });
        // An `if` or `elif` suite runs when its own test is true, and an `else` suite when
        // the latest test is false.
        state.arm.decided_by = this_test
            .map(|test| (test, true))
            .or(state.previous_test.map(|test| (test, false)));
        state.previous_test = this_test.or(state.previous_test);
        self.bind_narrow_ops(
            &new_narrow_ops,
            NarrowUseLocation::Span(range),
            &Usage::NonPinningValue(None),
        );
        state.negated_prev_ops.and_all(new_narrow_ops.negate());
        if state.arm.is_type_checking_branch {
            self.type_checking_depth += 1;
        }
    }

    /// Bind what follows the body of an arm, and finish the branch. Return the actual
    /// continuation binding when this arm is controlled by a test.
    pub fn if_arm_body_end(&mut self, state: &mut IfState, range: TextRange) -> Option<Idx<Key>> {
        if state.arm.is_type_checking_branch {
            self.type_checking_depth -= 1;
        }
        let end = state.arm.decided_by.map(|(test, runs_when)| {
            let end = self.insert_binding(
                Key::BranchEnd(range),
                Binding::BranchEnd(Box::new(BranchEnd {
                    test,
                    runs_when,
                    last: self.scopes.last_stmt_expr(),
                })),
            );
            self.scopes.set_last_stmt_expr(Some(end));
            end
        });
        let chosen = state.arm.chosen == Some(true);
        if chosen && !state.arm.test_is_environment_independent && self.scopes.has_terminated() {
            state.contains_environment_test_with_no_else = true;
        }
        self.finish_branch();
        // Choosing an environment-independent branch kills every later suite in every
        // environment. Choosing an environment-dependent one only kills those we can rule out
        // without consulting the environment.
        if chosen {
            state.report_all_remaining = state.arm.test_is_environment_independent;
        }
        end
    }

    /// An arm after a statically chosen one. It is not started.
    pub fn if_arm_dead(&mut self, state: &mut IfState, test: Option<&Expr>, body: &[Stmt]) {
        // `Some(false)`: this suite is dead everywhere. `Some(true)`: this branch is taken
        // wherever it is reached, so every suite after it is dead everywhere, even though this
        // one is live where the branch is chosen. `None`: the answer depends on the
        // environment.
        let unconditional = test.and_then(|test| {
            if SysInfo::depends_on_sys_info(test) {
                None
            } else {
                self.sys_info.evaluate_bool(test)
            }
        });
        if state.report_all_remaining || unconditional == Some(false) {
            self.report_unreachable_body(body);
        }
        state.report_all_remaining |= unconditional == Some(true);
        if Ast::body_contains_yield(body) {
            self.scopes.mark_has_yield_in_dead_code();
        }
    }

    /// Bind what follows the arms. `exhaustive` says whether some arm was statically chosen.
    pub fn if_finish(&mut self, state: IfState, exhaustive: bool) {
        let IfState {
            range,
            was_definitely_unreachable,
            reachability_idx,
            tests_link,
            negated_prev_ops,
            branch_suites,
            previous_test,
            contains_environment_test_with_no_else,
            ..
        } = state;
        self.insert_binding_idx(
            reachability_idx,
            BindingExpect::BranchSuiteReachability(branch_suites.into_boxed_slice()),
        );
        self.insert_binding_current(
            tests_link,
            Binding::UsageLink(LinkedKey::Expect(reachability_idx)),
        );
        // Create Exhaustive binding for type-based exhaustiveness checking.
        // This is done BEFORE finish_*_fork() so the binding exists in the right scope.
        // Only do this when there's no else clause (not syntactically exhaustive).
        let exhaustive_key = if !exhaustive {
            let narrow_entries = self.build_narrow_entries(&negated_prev_ops);
            Some(self.insert_binding(
                Key::Exhaustive(ExhaustivenessKind::IfElif, range),
                Binding::Exhaustive(Box::new(ExhaustiveBinding {
                    kind: ExhaustivenessKind::IfElif,
                    narrow_entries,
                })),
            ))
        } else {
            None
        };
        if exhaustive {
            self.finish_exhaustive_fork();
        } else {
            // The fall-through runs when the latest test is false.
            let fall_through_key = match previous_test {
                Some(test) => Some(self.insert_binding(
                    Key::BranchEnd(TextRange::empty(range.start())),
                    Binding::BranchEnd(Box::new(BranchEnd {
                        test,
                        runs_when: false,
                        last: exhaustive_key,
                    })),
                )),
                None => exhaustive_key,
            };
            self.finish_non_exhaustive_fork(&negated_prev_ops, fall_through_key);
        }
        // Preserve the configured-environment termination without treating it as
        // universally unreachable. This keeps later bindings out of a dead branch while
        // suppressing diagnostics that only apply to code live in this config.
        if contains_environment_test_with_no_else
            && !was_definitely_unreachable
            && self.scopes.has_terminated()
        {
            self.scopes
                .mark_flow_termination(TerminationKind::StaticTest);
            self.scopes.set_definitely_unreachable(false);
        }
    }

    /// Bind a `while` or `for` loop, sequencing its steps without the structured driver.
    pub fn loop_stmt(
        &mut self,
        mut state: LoopState,
        body: ThinVec<Stmt>,
        orelse: ThinVec<Stmt>,
        parent: &NestingContext,
    ) {
        #[cfg(feature = "bindings-driver")]
        self.assert_not_driven("loop");
        self.stmts(body, parent);
        self.loop_body_finish(&mut state);
        self.stmts(orelse, parent);
        self.loop_finish(state);
    }

    /// Bind the header of a `while` loop and set up the loop.
    pub fn while_header(&mut self, range: TextRange, mut test: Expr, body: &[Stmt]) -> LoopState {
        let was_unreachable = self.scopes.is_definitely_unreachable();
        self.setup_loop(range, &SmallSet::new());
        // Note that it is important we ensure *after* we set up the loop, so that both the
        // narrowing and type checking are aware that the test might be impacted by changes
        // made in the loop (e.g. if we reassign the test variable).
        // Typecheck the test condition during solving.
        let test_link = self.bind_bool_condition(&mut test);
        // The while condition always evaluates at least once, so walrus
        // targets are guaranteed to be assigned after the loop.
        self.scopes.propagate_new_flow_entries_to_loop_base();
        let static_test = self.sys_info.evaluate_bool(&test);
        let test_is_environment_independent = !SysInfo::depends_on_sys_info(&test);
        let is_while_true = static_test == Some(true);
        let narrow_ops = NarrowOps::from_expr(self, Some(&test));
        self.insert_bool_condition(test_link, test);
        // An environment-dependent condition is false only under the configuration being
        // checked, so its body stays ordinary live code: binding it as dead would silence
        // real diagnostics in it, such as an undefined name.
        let dead_body = if static_test == Some(false) && test_is_environment_independent {
            // Both termination flags must be restored, not just one:
            // `is_unreachable_from_static_test` is defined in terms of the pair, and a body
            // ending in `return` leaves `has_terminated` set behind it.
            let termination = self.scopes.save_termination();
            self.scopes.set_definitely_unreachable(true);
            let owns_unreachable_suite = !self.in_unreachable_suite;
            if owns_unreachable_suite {
                self.report_unreachable_body(body);
                self.in_unreachable_suite = true;
            }
            Some((termination, owns_unreachable_suite))
        } else {
            self.bind_narrow_ops(
                &narrow_ops,
                NarrowUseLocation::Span(range),
                &Usage::NonPinningValue(None),
            );
            None
        };
        LoopState {
            range,
            narrow_ops,
            is_while_true,
            // For while True: loops, the loop body definitely runs at least once
            loop_definitely_runs: is_while_true,
            environment_while_true: is_while_true
                && !test_is_environment_independent
                && !was_unreachable,
            dead_body,
            tail: None,
        }
    }

    /// Bind the header of a `for` loop and set up the loop.
    pub fn for_header(
        &mut self,
        range: TextRange,
        mut target: Expr,
        mut iter: Expr,
        is_async: bool,
    ) -> LoopState {
        if is_async && !self.scopes.is_in_async_def() && !self.module_info.allows_top_level_await()
        {
            self.error(
                range,
                ErrorKind::InvalidSyntax,
                "`async for` can only be used inside an async function".to_owned(),
            );
        }
        let mut loop_header_targets: SmallSet<Name> = SmallSet::new();
        Ast::expr_lvalue(&target, &mut |name| {
            loop_header_targets.insert(name.id.clone());
        });
        // Check if the iterable is definitely non-empty before binding it.
        let loop_definitely_runs = self.is_definitely_nonempty_iterable(&iter);
        let in_class_body = self.scopes.in_class_body();
        self.bind_target_with_expr(&mut target, &mut iter, &|expr, ann| {
            Binding::IterableValueLoop(
                ann,
                Box::new(expr.clone()),
                IsAsync::new(is_async),
                in_class_body,
            )
        });
        // Note that we set up the loop *after* the header is fully bound, because the loop
        // iterator is only evaluated once before the loop begins. But the loop header targets
        // - which get re-bound each iteration - are excluded from the loop Phi logic.
        self.setup_loop(range, &loop_header_targets);
        LoopState {
            range,
            narrow_ops: NarrowOps::new(),
            is_while_true: false,
            loop_definitely_runs,
            environment_while_true: false,
            dead_body: None,
            tail: None,
        }
    }

    /// After the loop body: merge the loop back edges and enter the `else` suite.
    pub fn loop_body_finish(&mut self, state: &mut LoopState) {
        if let Some((termination, owns_unreachable_suite)) = state.dead_body {
            if owns_unreachable_suite {
                self.in_unreachable_suite = false;
            }
            self.scopes.restore_termination(termination);
        }
        state.tail = Some(self.teardown_loop_body(
            state.range,
            &state.narrow_ops,
            state.loop_definitely_runs,
        ));
    }

    /// After the `else` suite: merge the `break` exits.
    pub fn loop_finish(&mut self, state: LoopState) {
        self.teardown_loop_else(
            state
                .tail
                .expect("`loop_body_finish` runs before `loop_finish`"),
            state.is_while_true,
        );
        if state.environment_while_true && self.scopes.has_terminated() {
            // Another Python environment can skip the loop.
            self.scopes.set_definitely_unreachable(false);
            self.scopes
                .mark_flow_termination(TerminationKind::StaticTest);
        }
    }

    /// Bind a `break` or `continue`. Returns whether a loop of the current scope encloses it.
    pub fn loop_exit_stmt(&mut self, range: TextRange, exit: LoopExit) -> bool {
        // PEP 765: Disallow break and continue in a finally block if not inside a nested loop.
        if self.sys_info.version().at_least(3, 14)
            && self.scopes.in_finally()
            && !self.scopes.loop_protects_from_finally_exit()
        {
            let keyword = match exit {
                LoopExit::Break => "break",
                LoopExit::Continue => "continue",
            };
            self.error(
                range,
                ErrorKind::InvalidSyntax,
                format!("`{keyword}` in a `finally` block will silence exceptions"),
            );
        }
        self.scopes.add_loop_exit(exit)
    }
}
