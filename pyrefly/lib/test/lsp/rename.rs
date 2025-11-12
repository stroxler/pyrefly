/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use itertools::Itertools;
use pretty_assertions::assert_eq;
use pyrefly_build::handle::Handle;
use ruff_text_size::TextSize;

use crate::state::state::State;
use crate::test::util::code_frame_of_source_at_range;
use crate::test::util::get_batched_lsp_operations_report;

fn get_test_report(state: &State, handle: &Handle, position: TextSize) -> String {
    let transaction = state.transaction();
    let ranges = transaction.find_local_references(handle, position);
    let module_info = transaction.get_module_info(handle).unwrap();
    format!(
        "Rename locations:\n{}",
        ranges
            .into_iter()
            .map(|range| code_frame_of_source_at_range(module_info.contents(), range))
            .join("\n")
    )
}

#[test]
fn test_rename_parameter_does_not_update_keyword_arguments() {
    let code = r#"
def greet(name, message):
    """Greet someone with a message."""
    print(f"{message}, {name}!")
    return name

def another_func():
    result = greet(name="Alice", message="Hello")
#                  ^
    return result
"#;
    let report = get_batched_lsp_operations_report(&[("main", code)], get_test_report);
    assert_eq!(
        r#"
# main.py
8 |     result = greet(name="Alice", message="Hello")
                       ^
Rename locations:
2 | def greet(name, message):
              ^^^^
4 |     print(f"{message}, {name}!")
                            ^^^^
5 |     return name
               ^^^^
"#
        .trim(),
        report.trim(),
    );
}
