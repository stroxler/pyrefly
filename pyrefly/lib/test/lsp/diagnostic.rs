/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use pyrefly_build::handle::Handle;

use crate::state::require::Require;
use crate::state::state::State;
use crate::test::util::mk_multi_file_state;

fn get_unused_import_diagnostics(state: &State, handle: &Handle) -> String {
    let transaction = state.transaction();
    if let Some(bindings) = transaction.get_bindings(handle) {
        let unused_imports = bindings.unused_imports();
        if unused_imports.is_empty() {
            return "No unused imports".to_owned();
        }
        let mut report = String::new();
        for (i, unused) in unused_imports.iter().enumerate() {
            if i > 0 {
                report.push_str(", ");
            }
            report.push_str(&format!("Import `{}` is unused", unused.name.as_str()));
        }
        report
    } else {
        "No bindings".to_owned()
    }
}

#[test]
fn test_dotted_import_used() {
    // BUG: This incorrectly reports `os.path` as unused
    // even though we use os.path.exists() in the code
    let code = r#"
import os.path

def check_exists(path: str) -> bool:
    return os.path.exists(path)
"#;
    let (handles, state) = mk_multi_file_state(&[("main", code)], Require::indexing(), true);
    let handle = handles.get("main").unwrap();
    let report = get_unused_import_diagnostics(&state, handle);
    assert_eq!(report, "Import `os.path` is unused");
}
