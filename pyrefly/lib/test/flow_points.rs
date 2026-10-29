/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Source identity alignment between actual Pyrefly binding and CFG lowering.

use std::collections::HashSet;

use pyrefly_static_analysis::ModuleName;
use pyrefly_static_analysis::analyze_python_with_flow_points;
use pyrefly_static_analysis::flow_point::FlowPointKind;

use crate::binding::binding::Key;
use crate::binding::driver::DriverState;
use crate::binding::driver::FlowObservation;
use crate::test::bindings_driver::prepare;

#[test]
fn actual_backends_align_source_points_with_nested_scopes_and_loops() {
    let source = r#"
def outer(flag: bool, items: list[int]) -> None:
    for item in items:
        def inner(flag: bool) -> None:
            while flag:
                if flag:
                    break
                else:
                    continue
            else:
                pass
        if flag:
            pass
        else:
            break
    else:
        pass
    class C:
        if flag:
            pass
"#;
    let fixture = prepare(source);
    fixture.assert_parity();
    let built = fixture.build_with_flow_points(true);
    let report = built.bindings.driver_report().unwrap();
    let lowered = analyze_python_with_flow_points(source, ModuleName::from_str("main"), false);
    assert_eq!(report.observation, FlowObservation::Full);
    assert!(!report.flow_points.is_empty());
    assert_eq!(report.flow_points, lowered.flow_points);
    assert!(!report.flow_bindings.is_empty());
    let mapped = fixture.build(true);
    let mapped_report = mapped.bindings.driver_report().unwrap();
    assert_eq!(mapped_report.observation, FlowObservation::Bindings);
    assert!(mapped_report.flow_points.is_empty());
    assert_eq!(mapped_report.flow_points.capacity(), 0);
    assert_eq!(mapped_report.flow_bindings, report.flow_bindings);
}

#[test]
fn sys_info_pruning_is_a_subset_of_the_actual_unpruned_cfg_ids() {
    let source = r#"
import sys
if sys.version_info < (3, 0):
    def skipped() -> None:
        while True:
            break
elif sys.version_info >= (3, 0):
    def kept(flag: bool) -> None:
        while flag:
            if flag:
                break
else:
    pass
if False:
    pass
"#;
    let fixture = prepare(source);
    let built = fixture.build_with_flow_points(true);
    let report = built.bindings.driver_report().unwrap();
    let full = analyze_python_with_flow_points(source, ModuleName::from_str("main"), false);
    let full: HashSet<_> = full.flow_points.into_iter().collect();
    assert!(report.flow_points.len() < full.len());
    assert!(report.flow_points.iter().all(|point| full.contains(point)));
    for binding in &report.flow_bindings {
        assert_eq!(binding.point.kind, FlowPointKind::ArmExit);
        match built.bindings.idx_to_key(binding.key) {
            Key::BranchEnd(range) => assert_eq!(*range, binding.point.range),
            other => panic!("an arm continuation mapped to {other:?}"),
        }
    }
    assert_eq!(
        fixture
            .build_with_flow_points(true)
            .bindings
            .driver_report(),
        Some(report)
    );
    fixture.assert_parity();
}

#[test]
fn completion_evidence_is_finalized_per_function_activation() {
    let source = r#"
from typing import Generator, Never

def outer(before_yield: bool, class_flag: bool) -> Generator[int, Never, None]:
    if before_yield:
        pass
    else:
        pass
    def inner(inner_flag: bool) -> None:
        if inner_flag:
            pass
        else:
            pass
    class C:
        if class_flag:
            pass
        else:
            pass
    if False:
        yield 1

async def coro(async_flag: bool) -> None:
    if async_flag:
        pass
    else:
        pass

def header_outer(before_header: bool) -> Generator[int, Never, None]:
    if before_header:
        pass
    else:
        pass
    def inner_default(value=(yield 1)) -> None:
        pass
"#;
    let report = prepare(source).assert_parity();
    for (test, safe) in [
        ("if before_yield:", false),
        ("if inner_flag:", true),
        ("if class_flag:", false),
        ("if async_flag:", false),
        ("if before_header:", false),
    ] {
        let start = source.find(test).unwrap();
        let mapping = report
            .flow_bindings
            .iter()
            .find(|mapping| mapping.point.range.start().to_usize() == start)
            .expect("a driven arm has an actual continuation mapping");
        assert_eq!(mapping.completion_safe, safe, "{test}");
    }
}

#[test]
fn observation_default_stores_no_points_or_key_mappings() {
    let report = DriverState::new().into_report();
    assert_eq!(report.observation, FlowObservation::Disabled);
    assert!(report.flow_points.is_empty());
    assert!(report.flow_bindings.is_empty());
    assert_eq!(report.flow_points.capacity(), 0);
    assert_eq!(report.flow_bindings.capacity(), 0);
    let mapped = DriverState::with_flow_bindings().into_report();
    assert_eq!(mapped.observation, FlowObservation::Bindings);
    assert_eq!(mapped.flow_points.capacity(), 0);
    assert_eq!(mapped.flow_bindings.capacity(), 0);
    let full = DriverState::with_flow_points().into_report();
    assert_eq!(full.observation, FlowObservation::Full);
}
