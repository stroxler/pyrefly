/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

testcase!(
    test_literal_dict,
    r#"
dict(x = 1, y = "test") # E:  No matching overload found for function `dict.__init__`
    "#,
);
