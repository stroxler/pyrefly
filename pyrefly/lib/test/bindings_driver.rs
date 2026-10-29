/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Binding with the structured driver compared to the legacy walk on identical inputs.
//!
//! Each fixture is bound twice, by `Bindings::build` with and without the driver, from the
//! same AST, exports and configuration. The bindings must be identical: the same table in
//! the same order with the same dependencies, and the same metadata and binding-time
//! errors. The driver report must show that the driver sequenced the structure.
//!
//! With the `bindings-driver` feature, every other test in this crate also binds through
//! the driver, because `Bindings::new` selects it.

use std::time::Duration;
use std::time::Instant;

use dupe::Dupe;
use pretty_assertions::assert_eq;
use pyrefly_python::ignore::Tool;
use pyrefly_python::ignore::TypeIgnoreUnknownTagBehavior;
use ruff_text_size::Ranged;

use crate::binding::bindings::Bindings;
use crate::binding::driver::DriverReport;
use crate::config::error::ErrorConfig;
use crate::error::collector::ErrorCollector;
use crate::error::style::ErrorStyle;
use crate::solver::solver::Solver;
use crate::solver::solver::SolverConfig;
use crate::state::state::State;
use crate::test::util::TestEnv;
use pyrefly_build::handle::Handle;

/// A checked module whose bindings can be rebuilt with either constructor.
pub(super) struct Fixture {
    state: State,
    handle: Handle,
}

/// One binding of a fixture.
pub(super) struct Built {
    pub bindings: Bindings,
    /// The errors reported while binding, as `range kind: message`.
    pub errors: Vec<String>,
    /// The time spent in the constructor alone. Only the optimized timing comparison reads it.
    #[cfg_attr(debug_assertions, expect(dead_code))]
    pub elapsed: Duration,
}

/// Check `source` as module `main`, so that its inputs can be bound again.
pub(super) fn prepare(source: &str) -> Fixture {
    let (state, handle) = TestEnv::one("main", source).to_state();
    Fixture {
        state,
        handle: handle("main"),
    }
}

impl Fixture {
    /// Bind the module, with the driver if `driven`, keeping the scope trace and IDE
    /// bodies as `Require::Everything` does. The AST is cloned, and the solver and error
    /// collector created, before the constructor is timed.
    pub(super) fn build(&self, driven: bool) -> Built {
        self.build_observed(driven, false)
    }

    /// Like [`Fixture::build`], but a driven binding keeps the full flow-point trace.
    pub(super) fn build_with_flow_points(&self, driven: bool) -> Built {
        self.build_observed(driven, true)
    }

    fn build_observed(&self, driven: bool, full_flow_trace: bool) -> Built {
        let handle = &self.handle;
        self.state.transaction().with_bindings_inputs(
            handle,
            |ast, load, exports, lookup, config, tensor_shapes| {
                let path = handle.path();
                let solver = Solver::new(SolverConfig {
                    infer_with_first_use: config.infer_with_first_use(path),
                    tensor_shapes,
                    strict_callable_subtyping: config.strict_callable_subtyping(path),
                    strict_partial_subtyping: config.strict_partial_subtyping(path),
                    spec_compliant_overloads: config.spec_compliant_overloads(path),
                    legacy_overload_expansion: config.legacy_overload_expansion(path),
                });
                let errors = ErrorCollector::new(load.module_info.dupe(), ErrorStyle::Delayed);
                let ast = ast.clone();
                let module_info = load.module_info.dupe();
                let build = if full_flow_trace {
                    Bindings::build_with_flow_points
                } else {
                    Bindings::build
                };
                let start = Instant::now();
                let bindings = build(
                    ast,
                    module_info,
                    exports,
                    &solver,
                    lookup,
                    *handle.sys_info(),
                    &errors,
                    true,
                    config.check_unannotated_defs(path),
                    true,
                    config.infer_return_types(path),
                    config.treat_all_caps_as_final(path),
                    driven,
                );
                let elapsed = start.elapsed();
                let errors = errors
                    .collect(&ErrorConfig::new(
                        Default::default(),
                        false,
                        Tool::default_enabled(),
                        TypeIgnoreUnknownTagBehavior::NoEffect,
                    ))
                    .ordinary
                    .iter()
                    .map(|e| format!("{:?} {:?}: {}", e.range(), e.error_kind(), e.msg()))
                    .collect();
                Built {
                    bindings,
                    errors,
                    elapsed,
                }
            },
        )
    }

    /// Bind both ways, assert that the bindings are identical, and return the driver report.
    pub(super) fn assert_parity(&self) -> DriverReport {
        let legacy = self.build(false);
        let driven = self.build(true);
        assert!(legacy.bindings.driver_report().is_none());
        let (legacy_graph, legacy_fields) = legacy.bindings.parity_view();
        let (driven_graph, driven_fields) = driven.bindings.parity_view();
        assert_eq!(legacy_graph, driven_graph);
        assert_eq!(legacy_fields, driven_fields);
        assert_eq!(legacy.errors, driven.errors);
        driven
            .bindings
            .driver_report()
            .expect("a driven build has a report")
            .clone()
    }

    /// The driver report of the bindings the checker itself built for this module.
    fn checker_report(&self) -> DriverReport {
        self.state
            .transaction()
            .get_answers(&self.handle)
            .expect("answers are kept for `Require::Everything`")
            .bindings()
            .driver_report()
            .expect("with the feature, the checker binds through the driver")
            .clone()
    }
}

fn parity(source: &str) -> DriverReport {
    let fixture = prepare(source);
    let report = fixture.assert_parity();
    assert_eq!(fixture.checker_report(), report);
    report
}

#[test]
fn function_branches_annotations_and_calls() {
    let report = parity(
        r#"
from typing import Callable

def pick(flag: bool, f: Callable[[int], int]) -> int:
    if flag:
        x: int = f(1)
    else:
        x = 0
    return x

pick(True, lambda n: n)
pick(True, "wrong")
"#,
    );
    assert_eq!(report.coverage.ifs, 1);
    assert_eq!(report.coverage.arms_visited, 2);
    assert_eq!(report.coverage.regions, 2);
    assert!(report.leaf >= 5);
}

#[test]
fn narrowing_early_return_and_missing_definition() {
    let report = parity(
        r#"
from typing import NoReturn

def fail() -> NoReturn:
    raise ValueError()

def f(x: int | None, flag: bool) -> int:
    if x is None:
        return 0
    elif flag:
        fail()
    if flag:
        y = 1
    return x + y
"#,
    );
    assert_eq!(report.coverage.ifs, 2);
    assert_eq!(report.coverage.arms_visited, 3);
}

#[test]
fn loops_with_else_break_and_continue() {
    let report = parity(
        r#"
def f(xs: list[int]) -> int:
    total = 0
    for x in xs:
        if x < 0:
            continue
        if x > 100:
            break
        total += x
    else:
        total = -1
    while total > 10:
        total -= 1
        if total == 5:
            break
    else:
        total = 0
    while True:
        if total:
            break
    return total
"#,
    );
    assert_eq!(report.coverage.loops, 3);
    assert_eq!(report.coverage.jumps, 4);
    assert_eq!(report.coverage.jumps_outside_loop, 0);
}

#[test]
fn first_use_contextual_typing_and_deferred_reads() {
    let report = parity(
        r#"
xs: list[int] = []
xs.append(1)
ys = []
ys.append("a")
x = ""
def read() -> str:
    return x
x = 1
def outer() -> None:
    n = 0
    def inner() -> None:
        nonlocal n
        n += 1
    inner()
"#,
    );
    assert_eq!(report.coverage.regions, 4);
    assert_eq!(report.coverage.ifs, 0);
}

#[test]
fn static_tests_are_pruned_by_the_driver() {
    let report = parity(
        r#"
import sys
from typing import TYPE_CHECKING

if sys.version_info >= (3, 0):
    a = 1
else:
    a = ""
if TYPE_CHECKING:
    b = 1
if sys.version_info < (3, 0):
    c = 1
elif sys.platform == "linux":
    c = 2
else:
    c = 3
if False:
    d = undefined_name
while False:
    e = 1
"#,
    );
    // The first `if` chooses its first arm, leaving the `else` dead. The third skips its
    // first arm and, on the Linux test configuration, chooses the `elif`, leaving the
    // `else` dead. `if False` skips its only arm.
    assert_eq!(report.coverage.arms_dead, 2);
    assert_eq!(report.coverage.arms_skipped, 2);
    assert_eq!(report.coverage.arms_visited, 3);
}

#[test]
fn mixed_statements_drive_their_nested_suites() {
    let report = parity(
        r#"
import contextlib
from typing import NamedTuple

class C:
    x: int = 0
    for i in range(3):
        if i:
            x = i

def g(v: object) -> int:
    with contextlib.suppress(Exception):
        while True:
            try:
                if v:
                    break
            except ValueError:
                continue
            finally:
                pass
    match v:
        case int():
            if v > 0:
                return v
        case _:
            pass
    return 0

P = NamedTuple("P", [("a", int)])
P.__new__.__defaults__ = (1,)

if (n := len("x")) > 0:
    pass
elif (m := n) > 1:
    pass
"#,
    );
    // `class`, `def`, `with`, `try` and `match`.
    assert_eq!(report.mixed, 5);
    assert_eq!(report.coverage.ifs, 4);
    assert_eq!(report.coverage.loops, 2);
    assert_eq!(report.coverage.jumps, 2);
    // The `__defaults__` assignment is absorbed by the namedtuple before it.
    assert_eq!(report.coverage.other, report.leaf + report.mixed);
}

#[test]
fn jumps_outside_their_region_are_attributed_to_no_loop() {
    let report = parity(
        r#"
for x in []:
    def f() -> None:
        break
    class C:
        continue
"#,
    );
    assert_eq!(report.coverage.jumps, 2);
    assert_eq!(report.coverage.jumps_outside_loop, 2);
}
