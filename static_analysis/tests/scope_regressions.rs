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
    // Known bug: the old lookup passes the middle function's global declaration
    // and incorrectly finds outer.x. CPython rejects this nonlocal declaration.
    assert!(!has_scope_error);
}
