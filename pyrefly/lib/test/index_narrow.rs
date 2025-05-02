/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

testcase!(
    test_index_narrow,
    r#"
from typing import assert_type
class C1:
    x: list[object]
class C2:
    x: object
def test(x: list[object], c1: C1, c2s: list[C2]):
    assert_type(x[0], object)
    assert isinstance(x[0], int)
    assert_type(x[0], int)

    assert_type(c1.x[0], object)
    assert isinstance(c1.x[0], int)
    assert_type(c1.x[0], int)

    assert_type(c2s[0].x, object)
    assert isinstance(c2s[0].x, int)
    assert_type(c2s[0].x, int)
 "#,
);
