/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

testcase!(
    test_function_basic,
    r#"
from typing import dataclass_transform

@dataclass_transform()
def create[T](cls: type[T]) -> type[T]: ...

@create
class C:
    x: int
C(x=0)
C(x="oops")  # E: `Literal['oops']` is not assignable to parameter `x` with type `int`
    "#,
);

testcase!(
    bug = "Not yet supported",
    test_class_basic,
    r#"
from typing import dataclass_transform

@dataclass_transform()
class C: ...

class D(C):
    x: int
D(x=0)  # Should be ok  # E: Unexpected keyword
D(x="oops")  # E: Unexpected keyword
    "#,
);

testcase!(
    bug = "Not yet supported",
    test_metaclass_basic,
    r#"
from typing import dataclass_transform

@dataclass_transform()
class Meta(type): ...
class C(metaclass=Meta): ...

class D(C):
    x: int
D(x=0)  # Should be ok  # E: Unexpected keyword
D(x="oops")  # E: Unexpected keyword
    "#,
);
