/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

testcase!(
    test_awaitable_any,
    r#"
from typing import Any, reveal_type
async def foo() -> Any: ...
async def test() -> None:
    z = await foo()
    reveal_type(z)  # E: revealed type: Any
#   ^
"#,
);
