/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use lsp_types::FoldingRangeKind;
use pretty_assertions::assert_eq;
use pyrefly_build::handle::Handle;
use serde::Serialize;

use crate::state::state::State;
use crate::test::util::get_batched_lsp_operations_report_no_cursor;

#[derive(Serialize)]
struct FoldingRangeInfo {
    start_line: u32,
    end_line: u32,
    #[serde(skip_serializing_if = "Option::is_none")]
    kind: Option<String>,
}

fn get_folding_ranges_report(state: &State, handle: &Handle) -> String {
    let transaction = state.transaction();
    let Some(module) = transaction.get_module_info(handle) else {
        return "[]".to_owned();
    };
    let ranges = transaction.folding_ranges(handle).unwrap_or_default();
    let mut folding_ranges: Vec<FoldingRangeInfo> = ranges
        .into_iter()
        .map(|(text_range, kind)| {
            let range = module.lined_buffer().to_lsp_range(text_range);
            FoldingRangeInfo {
                start_line: range.start.line,
                end_line: range.end.line,
                kind: kind.map(|k| match k {
                    FoldingRangeKind::Comment => "comment".to_owned(),
                    FoldingRangeKind::Imports => "imports".to_owned(),
                    FoldingRangeKind::Region => "region".to_owned(),
                }),
            }
        })
        .collect();
    // Sort for stable output
    folding_ranges.sort_by_key(|r| (r.start_line, r.end_line));
    serde_json::to_string_pretty(&folding_ranges).unwrap()
}

fn get_docstring_ranges_report(state: &State, handle: &Handle) -> String {
    let transaction = state.transaction();
    let Some(module) = transaction.get_module_info(handle) else {
        return "[]".to_owned();
    };
    let ranges = transaction.docstring_ranges(handle).unwrap_or_default();
    let lines: Vec<(u32, u32)> = ranges
        .into_iter()
        .map(|text_range| {
            let range = module.lined_buffer().to_lsp_range(text_range);
            (range.start.line, range.end.line)
        })
        .collect();
    serde_json::to_string_pretty(&lines).unwrap()
}

#[test]
fn folding_ranges_for_functions_and_classes() {
    let code = r#"
"""Module doc"""

class Foo:
    """Class doc"""

    def method(self):
        """Method doc"""
        pass

def outer():
    """Outer doc"""
    pass
"#;

    let report =
        get_batched_lsp_operations_report_no_cursor(&[("main", code)], get_folding_ranges_report);

    assert_eq!(
        r#"# main.py

[
  {
    "start_line": 1,
    "end_line": 1,
    "kind": "comment"
  },
  {
    "start_line": 3,
    "end_line": 8
  },
  {
    "start_line": 4,
    "end_line": 4,
    "kind": "comment"
  },
  {
    "start_line": 6,
    "end_line": 8
  },
  {
    "start_line": 7,
    "end_line": 7,
    "kind": "comment"
  },
  {
    "start_line": 10,
    "end_line": 12
  },
  {
    "start_line": 11,
    "end_line": 11,
    "kind": "comment"
  }
]"#
        .trim(),
        report.trim(),
    );
}

#[test]
fn folding_ranges_for_conditionals() {
    let code = r#"
x = 5
if x > 0:
    print("positive")
elif x < 0:
    print("negative")
else:
    print("zero")

if True:
    if False:
        pass
"#;

    let report =
        get_batched_lsp_operations_report_no_cursor(&[("main", code)], get_folding_ranges_report);

    assert_eq!(
        r#"# main.py

[
  {
    "start_line": 2,
    "end_line": 7
  },
  {
    "start_line": 4,
    "end_line": 5
  },
  {
    "start_line": 6,
    "end_line": 7
  },
  {
    "start_line": 9,
    "end_line": 11
  },
  {
    "start_line": 10,
    "end_line": 11
  }
]"#
        .trim(),
        report.trim(),
    );
}

#[test]
fn folding_ranges_for_loops() {
    let code = r#"
for i in range(10):
    print(i)

while True:
    break

for x in [1, 2, 3]:
    if x > 1:
        break
else:
    print("done")
"#;

    let report =
        get_batched_lsp_operations_report_no_cursor(&[("main", code)], get_folding_ranges_report);

    assert_eq!(
        r#"# main.py

[
  {
    "start_line": 1,
    "end_line": 2
  },
  {
    "start_line": 4,
    "end_line": 5
  },
  {
    "start_line": 7,
    "end_line": 11
  },
  {
    "start_line": 8,
    "end_line": 9
  }
]"#
        .trim(),
        report.trim(),
    );
}

#[test]
fn folding_ranges_for_with_statements() {
    let code = r#"
with open("file.txt") as f:
    content = f.read()

with open("a") as f1, open("b") as f2:
    pass
"#;

    let report =
        get_batched_lsp_operations_report_no_cursor(&[("main", code)], get_folding_ranges_report);

    assert_eq!(
        r#"# main.py

[
  {
    "start_line": 1,
    "end_line": 2
  },
  {
    "start_line": 4,
    "end_line": 5
  }
]"#
        .trim(),
        report.trim(),
    );
}

#[test]
fn folding_ranges_for_try_except() {
    let code = r#"
def handle_error(e):
    pass

try:
    print("test")
except ValueError as e:
    handle_error(e)
except Exception:
    pass
else:
    print("success")
finally:
    print("done")
"#;

    let report =
        get_batched_lsp_operations_report_no_cursor(&[("main", code)], get_folding_ranges_report);

    assert_eq!(
        r#"# main.py

[
  {
    "start_line": 1,
    "end_line": 2
  },
  {
    "start_line": 4,
    "end_line": 13
  },
  {
    "start_line": 6,
    "end_line": 7
  },
  {
    "start_line": 8,
    "end_line": 9
  }
]"#
        .trim(),
        report.trim(),
    );
}

#[test]
fn folding_ranges_for_match_statements() {
    let code = r#"
value = 1
match value:
    case 1:
        print("one")
    case 2:
        print("two")
    case _:
        print("other")
"#;

    let report =
        get_batched_lsp_operations_report_no_cursor(&[("main", code)], get_folding_ranges_report);

    assert_eq!(
        r#"# main.py

[
  {
    "start_line": 2,
    "end_line": 8
  },
  {
    "start_line": 3,
    "end_line": 4
  },
  {
    "start_line": 5,
    "end_line": 6
  },
  {
    "start_line": 7,
    "end_line": 8
  }
]"#
        .trim(),
        report.trim(),
    );
}

#[test]
fn folding_ranges_nested_structures() {
    let code = r#"
class MyClass:
    """Class docstring"""
    
    def method(self):
        """Method docstring"""
        for i in range(10):
            if i > 5:
                try:
                    print(i)
                except Exception:
                    pass
"#;

    let report =
        get_batched_lsp_operations_report_no_cursor(&[("main", code)], get_folding_ranges_report);

    assert_eq!(
        r#"# main.py

[
  {
    "start_line": 1,
    "end_line": 11
  },
  {
    "start_line": 2,
    "end_line": 2,
    "kind": "comment"
  },
  {
    "start_line": 4,
    "end_line": 11
  },
  {
    "start_line": 5,
    "end_line": 5,
    "kind": "comment"
  },
  {
    "start_line": 6,
    "end_line": 11
  },
  {
    "start_line": 7,
    "end_line": 11
  },
  {
    "start_line": 8,
    "end_line": 11
  },
  {
    "start_line": 10,
    "end_line": 11
  }
]"#
        .trim(),
        report.trim(),
    );
}

#[test]
fn docstring_ranges_cover_nested_definitions() {
    let code = r#"
"""Module doc"""

class Foo:
    """Class doc"""

    def method(self):
        """Method doc"""
        if True:
            def inner():
                """Inner doc"""
                pass

    async def async_method(self):
        """Async method doc"""
        pass

def outer():
    """Outer doc"""
    def inner_outer():
        """Inner outer doc"""
        pass
    return inner_outer

if True:
    class Conditional:
        """Conditional class doc"""
        pass
"#;

    let report =
        get_batched_lsp_operations_report_no_cursor(&[("main", code)], get_docstring_ranges_report);

    assert_eq!(
        r#"# main.py

[
  [
    1,
    1
  ],
  [
    4,
    4
  ],
  [
    7,
    7
  ],
  [
    10,
    10
  ],
  [
    14,
    14
  ],
  [
    18,
    18
  ],
  [
    20,
    20
  ],
  [
    26,
    26
  ]
]"#
        .trim(),
        report.trim(),
    );
}
