/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use pretty_assertions::assert_eq;

use crate::state::handle::Handle;
use crate::state::state::State;
use crate::test::util::get_batched_lsp_operations_report_no_cursor;

fn get_test_report(state: &State, handle: &Handle) -> String {
    let transaction = state.transaction();
    if let Some(symbols) = transaction.symbols(handle) {
        serde_json::to_string_pretty(&symbols).unwrap()
    } else {
        "No document symbols found".to_owned()
    }
}

#[test]
fn function_test() {
    let code = r#"
def function1():
    """Test docstring"""
    x = 1
    return x

def function2(param1, param2):
    y = param1 + param2
    return y
"#;
    let report = get_batched_lsp_operations_report_no_cursor(&[("main", code)], get_test_report);
    assert_eq!(
        r#"# main.py

[
  {
    "name": "function1",
    "kind": 12,
    "range": {
      "start": {
        "line": 1,
        "character": 0
      },
      "end": {
        "line": 4,
        "character": 12
      }
    },
    "selectionRange": {
      "start": {
        "line": 1,
        "character": 4
      },
      "end": {
        "line": 1,
        "character": 13
      }
    },
    "children": []
  },
  {
    "name": "function2",
    "kind": 12,
    "range": {
      "start": {
        "line": 6,
        "character": 0
      },
      "end": {
        "line": 8,
        "character": 12
      }
    },
    "selectionRange": {
      "start": {
        "line": 6,
        "character": 4
      },
      "end": {
        "line": 6,
        "character": 13
      }
    },
    "children": []
  }
]"#
        .trim(),
        report.trim(),
    );
}
