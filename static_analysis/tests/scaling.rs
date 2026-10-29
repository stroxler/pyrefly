/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! A scaling probe for the domain-absent paths: lowering, an engine run with a trivial
//! domain, and history. Run with
//! `cargo test --release -p pyrefly_static_analysis --test scaling -- --ignored --nocapture`.

use std::time::Instant;

use pyrefly_static_analysis::*;

struct Unit;

impl Domain for Unit {
    type Value = ();
    type State = ();
    type Diagnostic = ();
    fn top(&self) {}
    fn join(&self, _: &(), _: &()) {}
    fn join_state(&self, _: &(), _: &()) {}
}

#[test]
#[ignore = "timing probe; run explicitly in release mode"]
fn scaling_probe() {
    for n in [100, 800, 3200] {
        let source: String = (0..n)
            .map(|i| format!("if c{i}:\n    x{} = f(a, b) + g(c)\n", i % 50))
            .collect();
        let start = Instant::now();
        let p = analyze_python(&source, ModuleName::from_str("m"), false);
        let lower = start.elapsed();
        let start = Instant::now();
        let result = Engine::new(&p, &Unit).analyze(Program::MODULE, Entry::new(()));
        let engine = start.elapsed();
        let start = Instant::now();
        History::new(&p);
        let history = start.elapsed();
        assert_eq!(result.convergence, Convergence::Converged);
        println!(
            "branches={n} ops={} lower={lower:?} engine={engine:?} history={history:?}",
            p.ops.len()
        );
    }
}
