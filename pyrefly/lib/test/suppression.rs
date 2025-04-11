/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

testcase!(
    test_pyrefly_suppression,
    r#"
def foo() -> str:
  # pyrefly: ignore
  return 1
"#,
);

testcase!(
    test_pyrefly_top_level_ignore,
    r#"
# pyrefly: ignore-all-errors
3 + "3"
3 + "3"
"#,
);

testcase!(
    bug = "Should not silence any errors because this is a top level flag",
    test_pyrefly_top_level_ignore_wrong_same_line,
    r#"
3 + "3" # pyrefly: ignore-all-errors
3 + "3"
"#,
);

testcase!(
    bug = "Should not silence any errors because this is a top level flag",
    test_pyrefly_top_level_ignore_wrong_own_line,
    r#"
3 + "3" # E:
# pyrefly: ignore-all-errors
3 + "3"
"#,
);
