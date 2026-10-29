/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Serial, binding-construction-only comparison of the legacy and driven paths.
//! Parsing, AST cloning, state/solver setup and solving are outside the timed region.
//! Run explicitly in release mode; this is not an allocation or whole-check benchmark.

#![cfg(not(debug_assertions))]

use std::time::Duration;

use super::bindings_driver::prepare;

fn median(samples: &[Duration]) -> Duration {
    let mut samples = samples.to_vec();
    samples.sort();
    samples[samples.len() / 2]
}

#[test]
#[ignore = "phase timing; run serially in an optimized build"]
fn bindings_driver_construction_timing() {
    let simple = "from typing import Callable\ndef pick(flag: bool, f: Callable[[int], int]) -> int:\n    if flag:\n        x: int = f(1)\n    else:\n        x = 0\n    return x\n".to_owned();
    let contextual = "from typing import NoReturn\ndef fail() -> NoReturn:\n    raise ValueError()\ndef f(x: int | None, flag: bool) -> int:\n    xs: list[int] = []\n    xs.append(1)\n    if x is None:\n        return 0\n    if flag:\n        fail()\n    for item in xs:\n        x += item\n    return x\n".to_owned();
    let many: String = (0..100).map(|i| format!(
        "def f{i}(x: int, flag: bool) -> int:\n    if flag:\n        y = x + 1\n    else:\n        y = x - 1\n    return y\n"
    )).collect();
    for (name, source) in [
        ("branch", simple),
        ("contextual-loop", contextual),
        ("100-functions", many),
    ] {
        let fixture = prepare(&source);
        let report = fixture.assert_parity();
        assert!(report.coverage.suites > 0 && report.coverage.ifs > 0);
        for _ in 0..2 {
            fixture.build(false);
            fixture.build(true);
        }
        let mut legacy = Vec::new();
        let mut driven = Vec::new();
        for sample in 0..20 {
            // Alternate which constructor is first to avoid a systematic warm-cache advantage.
            let order = if sample % 2 == 0 {
                [false, true]
            } else {
                [true, false]
            };
            for is_driven in order {
                let built = fixture.build(is_driven);
                assert_eq!(built.bindings.driver_report().is_some(), is_driven);
                if is_driven {
                    driven.push(built.elapsed);
                } else {
                    legacy.push(built.elapsed);
                }
            }
        }
        let old = median(&legacy);
        let new = median(&driven);
        println!(
            "fixture={name} legacy_median_ns={} driven_median_ns={} ratio={:.4} suites={} ifs={} loops={} mixed={} legacy_ns={:?} driven_ns={:?}",
            old.as_nanos(),
            new.as_nanos(),
            new.as_secs_f64() / old.as_secs_f64(),
            report.coverage.suites,
            report.coverage.ifs,
            report.coverage.loops,
            report.mixed,
            legacy.iter().map(Duration::as_nanos).collect::<Vec<_>>(),
            driven.iter().map(Duration::as_nanos).collect::<Vec<_>>()
        );
    }
}
