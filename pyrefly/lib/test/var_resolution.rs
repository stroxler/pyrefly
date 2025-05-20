/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

testcase!(
    test_await_any,
    r#"
from typing import Any, reveal_type
async def async_return_any() -> Any: ...
def return_any() -> Any: ...
async def test() -> None:
    z = await async_return_any()
    reveal_type(z)  # E: revealed type: Any
    z = await return_any()
"#,
);

testcase!(
    bug = "We should not leak vars here",
    test_await_bottom,
    r#"
from typing import Never, NoReturn, reveal_type
def returns_never() -> Never: ...
def returns_noreturn() -> NoReturn: ...
async def test() -> None:
    z = await returns_never()
    reveal_type(z)  # E: revealed type: @_
    z = await returns_noreturn()
    reveal_type(z)  # E: revealed type: @_
"#,
);
