/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! A downstream tool can consume source-qualified, optional Pyrefly facts without
//! inference keys. The demo does not automatically prune the operation CFG.

use pyrefly_python::module_name::ModuleName;
use pyrefly_static_analysis::analyze_python_with_flow_points;
use pyrefly_static_analysis::facts::FlowFacts;
use pyrefly_static_analysis::facts::NormalCompletion;
use pyrefly_static_analysis::flow_point::FlowPointId;
use pyrefly_static_analysis::flow_point::FlowPointKind;
use ruff_text_size::TextRange;

use crate::binding::flow_facts::DiagnosticCheck;
use crate::binding::flow_facts::FactAssumptions;
use crate::binding::flow_facts::ObservationCoverage;
use crate::binding::flow_facts::SourceMismatch;
use crate::binding::flow_facts::TypedFlowFacts;
use crate::test::bindings_driver::prepare;
use crate::test::util::TestEnv;

const SOURCE: &str = "from typing import NoReturn\n\ndef stop() -> NoReturn:\n    raise ValueError()\n\ndef normal() -> int:\n    return 1\n\ndef choose(flag: bool) -> int:\n    if flag:\n        stop()\n    else:\n        value = normal()\n    return value\n\nchoose(True)\n";

#[test]
fn typed_precheck_connects_to_neutral_source_points() {
    let module = ModuleName::from_str("main");
    let lowered = analyze_python_with_flow_points(SOURCE, module, false);
    let (state, handle) = TestEnv::one("main", SOURCE).to_state();
    let handle = handle("main");
    let transaction = state.transaction();
    let answers = transaction.get_answers(&handle).unwrap();
    let snapshot = answers.flow_facts();
    let view = snapshot.for_source(module, SOURCE).unwrap();
    // This is the only interface a downstream domain needs for the demo.
    let provider: &dyn FlowFacts = &view;
    assert_eq!(
        provider.metadata().assumptions,
        FactAssumptions::DeclaredTypesAndTypingRulesHold
    );
    assert_eq!(
        provider.metadata().diagnostics,
        DiagnosticCheck::NotProvided
    );
    let report = answers.bindings().driver_report().unwrap();
    assert!(
        !report.flow_bindings.is_empty(),
        "the typed driver must be observed"
    );
    let mut never_arms = 0;
    let mut unknown_arms = 0;
    for mapping in &report.flow_bindings {
        assert!(
            lowered.flow_points.contains(&mapping.point),
            "typed and lowered source IDs agree"
        );
        match provider.normal_completion(mapping.point) {
            NormalCompletion::NoNormalCompletionIfTypesHold => never_arms += 1,
            NormalCompletion::Unknown => unknown_arms += 1,
        }
    }
    assert!(
        never_arms > 0,
        "a NoReturn branch must have solved continuation evidence"
    );
    assert!(
        unknown_arms > 0,
        "ordinary normal completion is not a no-return proof"
    );
    let absent: Option<&dyn FlowFacts> = None;
    assert_eq!(
        absent.map_or(NormalCompletion::Unknown, |facts| facts
            .normal_completion(report.flow_bindings[0].point)),
        NormalCompletion::Unknown
    );
}

#[test]
fn feedback_is_source_bound_and_missing_is_unknown() {
    let (state, handle) = TestEnv::one("main", SOURCE).to_state();
    let handle = handle("main");
    let answers = state.transaction().get_answers(&handle).unwrap();
    let snapshot = answers.flow_facts();
    assert!(matches!(
        snapshot.for_source(ModuleName::from_str("other"), SOURCE),
        Err(SourceMismatch::Module)
    ));
    let edited = SOURCE.replace("stop()\n", "none()\n");
    assert_eq!(
        edited.len(),
        SOURCE.len(),
        "same-range edits are still stale"
    );
    assert!(matches!(
        snapshot.for_source(ModuleName::from_str("main"), &edited),
        Err(SourceMismatch::Source)
    ));
    let view = snapshot
        .for_source(ModuleName::from_str("main"), SOURCE)
        .unwrap();
    let missing = FlowPointId {
        region: TextRange::default(),
        range: TextRange::default(),
        kind: FlowPointKind::ArmExit,
    };
    assert_eq!(view.normal_completion(missing), NormalCompletion::Unknown);
}

#[test]
fn any_continuation_does_not_become_a_proof() {
    let source = "from typing import Any\ndef choose(flag: bool, callback: Any) -> int:\n    if flag:\n        value = callback()\n    else:\n        value = 1\n    return value\nchoose(True, 1)\n";
    let (state, handle) = TestEnv::one("main", source).to_state();
    let handle = handle("main");
    let answers = state.transaction().get_answers(&handle).unwrap();
    let snapshot = answers.flow_facts();
    let view = snapshot
        .for_source(ModuleName::from_str("main"), source)
        .unwrap();
    let report = answers.bindings().driver_report().unwrap();
    assert!(!report.flow_bindings.is_empty());
    for mapping in &report.flow_bindings {
        assert_eq!(
            view.normal_completion(mapping.point),
            NormalCompletion::Unknown
        );
    }
}

#[test]
fn generator_send_never_is_not_a_completion_proof() {
    let sources = [
        "from typing import Generator, Never\ndef f(flag: bool) -> Generator[int, Never, None]:\n    if flag:\n        yield 1\n    else:\n        pass\nlist(f(True))\n",
        "from typing import Generator, NoReturn\ndef f(flag: bool) -> Generator[int, NoReturn, None]:\n    if flag:\n        yield 1\n    else:\n        pass\nlist(f(True))\n",
        "from typing import AsyncGenerator, Never\nasync def f(flag: bool) -> AsyncGenerator[int, Never]:\n    if flag:\n        yield 1\n    else:\n        pass\nf(True)\n",
        // A yield in a nested definition's default belongs to this outer activation.
        "from typing import Generator, Never\ndef f(flag: bool) -> Generator[int, Never, None]:\n    if flag:\n        def nested(x=(yield 1)):\n            pass\n    else:\n        pass\nf(True)\n",
    ];
    for source in sources {
        let (state, handle) = TestEnv::one("main", source).to_state();
        let handle = handle("main");
        let transaction = state.transaction();
        let errors = transaction.get_errors([&handle]).collect_errors();
        assert!(
            errors.ordinary.is_empty(),
            "the regression is valid typed code: {:?}",
            errors
                .ordinary
                .iter()
                .map(|error| error.msg())
                .collect::<Vec<_>>()
        );
        let answers = transaction.get_answers(&handle).unwrap();
        let snapshot = answers.flow_facts();
        let view = snapshot
            .for_source(ModuleName::from_str("main"), source)
            .unwrap();
        let report = answers.bindings().driver_report().unwrap();
        assert!(!report.flow_bindings.is_empty());
        for mapping in &report.flow_bindings {
            assert!(
                !mapping.completion_safe,
                "suspension must invalidate the whole activation"
            );
            assert_eq!(
                view.normal_completion(mapping.point),
                NormalCompletion::Unknown
            );
        }
    }
}

#[test]
fn ordinary_nested_function_keeps_its_own_completion_evidence() {
    let source = "from typing import Generator, Never, NoReturn\ndef stop() -> NoReturn:\n    raise ValueError()\ndef outer() -> Generator[int, Never, None]:\n    def inner(flag: bool) -> int:\n        if flag:\n            stop()\n        else:\n            value = 1\n        return value\n    inner(True)\n    if True:\n        yield 1\nouter()\n";
    let (state, handle) = TestEnv::one("main", source).to_state();
    let handle = handle("main");
    let answers = state.transaction().get_answers(&handle).unwrap();
    let snapshot = answers.flow_facts();
    let view = snapshot
        .for_source(ModuleName::from_str("main"), source)
        .unwrap();
    let report = answers.bindings().driver_report().unwrap();
    assert!(
        report
            .flow_bindings
            .iter()
            .any(|mapping| mapping.completion_safe
                && view.normal_completion(mapping.point)
                    == NormalCompletion::NoNormalCompletionIfTypesHold)
    );
    assert!(
        report
            .flow_bindings
            .iter()
            .any(|mapping| !mapping.completion_safe)
    );
}

#[test]
fn conditional_facts_make_typing_assumptions_explicit() {
    let sources = [
        "from typing import Any, cast\ndef h(n: int) -> int:\n    if isinstance(n, str):\n        n.upper()\n    return n\nh(cast(Any, 's'))\n",
        "from typing import Any, Literal, cast\ndef h(flag: Literal[False]) -> None:\n    if flag:\n        print('defensive')\nh(cast(Any, True))\n",
        "value: int = 's'\nif isinstance(value, str):\n    value.upper()\n",
    ];
    for source in sources {
        let (state, handle) = TestEnv::one("main", source).to_state();
        let handle = handle("main");
        let answers = state.transaction().get_answers(&handle).unwrap();
        let snapshot = answers.flow_facts();
        let view = snapshot
            .for_source(ModuleName::from_str("main"), source)
            .unwrap();
        assert_eq!(
            view.metadata().assumptions,
            FactAssumptions::DeclaredTypesAndTypingRulesHold
        );
        assert_eq!(view.metadata().diagnostics, DiagnosticCheck::NotProvided);
        assert!(matches!(
            view.metadata().coverage,
            ObservationCoverage::Observed { .. }
        ));
        // Untyped callers/casts or bad annotations can violate these conditions.
        // The qualified enum never claims unconditional runtime impossibility.
        let report = answers.bindings().driver_report().unwrap();
        assert!(report.flow_bindings.iter().any(|mapping| {
            view.normal_completion(mapping.point) == NormalCompletion::NoNormalCompletionIfTypesHold
        }));
    }
}

#[test]
fn observed_unknown_is_distinct_from_unobserved_feedback() {
    let fixture = prepare(SOURCE);
    let legacy = fixture.build(false);
    let unobserved = TypedFlowFacts::collect(&legacy.bindings, |_| None);
    assert_eq!(
        unobserved.metadata().coverage,
        ObservationCoverage::Unobserved
    );
    let driven = fixture.build(true);
    let observed_unknown = TypedFlowFacts::collect(&driven.bindings, |_| None);
    assert!(matches!(
        observed_unknown.metadata().coverage,
        ObservationCoverage::Observed {
            mapped_arms: 1..,
            conditional_facts: 0,
            ..
        }
    ));
    let view = observed_unknown
        .for_source(ModuleName::from_str("main"), SOURCE)
        .unwrap();
    for mapping in &driven.bindings.driver_report().unwrap().flow_bindings {
        assert_eq!(
            view.normal_completion(mapping.point),
            NormalCompletion::Unknown
        );
    }
}
