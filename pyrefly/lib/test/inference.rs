/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::test::util::TestEnv;
use crate::testcase;

testcase!(
    test_no_empty_container_inference,
    TestEnv::new_with_infer_with_first_use(false),
    r#"
from typing import assert_type, Any
x = []
x.append(1)
x.append("foo")
assert_type(x, list[Any])
x = {}
x[1] = 2
x["1"] = "2"
assert_type(x, dict[Any, Any])
"#,
);
