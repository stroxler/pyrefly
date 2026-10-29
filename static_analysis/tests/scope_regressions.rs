/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use pyrefly_static_analysis::GapKind;
use pyrefly_static_analysis::ModuleName;
use pyrefly_static_analysis::OpKind;
use pyrefly_static_analysis::analyze_python;

#[test]
fn nonlocal_global_barrier() {
    let source = "def outer():\n    x = 1\n    def middle():\n        global x\n        def inner():\n            nonlocal x\n            return x\n";
    let program = analyze_python(source, ModuleName::from_str("main"), false);
    let has_scope_error = program.ops.iter().any(|op| {
        matches!(
            op.kind,
            OpKind::Gap {
                kind: GapKind::ScopeError,
                ..
            }
        )
    });
    // An enclosing global directive blocks nonlocal lookup, even when a farther
    // enclosing function owns a name with the same spelling.
    assert!(has_scope_error);
}
