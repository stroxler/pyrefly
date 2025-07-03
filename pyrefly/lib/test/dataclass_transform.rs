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
    test_class_basic,
    r#"
from typing import dataclass_transform

@dataclass_transform()
class C: ...

class D(C):
    x: int
D(x=0)
D(x="oops")  # E: `Literal['oops']` is not assignable to parameter `x` with type `int`
    "#,
);

testcase!(
    test_class_inheritance,
    r#"
from typing import dataclass_transform

@dataclass_transform()
class C: ...

class D(C):
    x: int
class E(D):
    y: str
E(x=0, y="")
    "#,
);

testcase!(
    test_metaclass_basic,
    r#"
from typing import dataclass_transform

@dataclass_transform()
class Meta(type): ...
class C(metaclass=Meta): ...

class D(C):
    x: int
D(x=0)
D(x="oops")  # E: `Literal['oops']` is not assignable to parameter `x` with type `int`
    "#,
);

testcase!(
    test_call_transform,
    r#"
from typing import dataclass_transform

@dataclass_transform()
def build(**kwargs): ...

@build()
class C:
    x: int
C(x=0)
C(x="oops")  # E: `Literal['oops']` is not assignable to parameter `x` with type `int`
    "#,
);

testcase!(
    test_frozen_default,
    r#"
from typing import dataclass_transform, Any
@dataclass_transform()
def build_default_mutable(**kwargs) -> Any: ...
@dataclass_transform(frozen_default=True)
def build_default_frozen(**kwargs) -> Any: ...

@build_default_mutable()
class Mutable1:
    x: int
@build_default_mutable(frozen=True)
class Frozen1:
    x: int
@build_default_frozen()
class Frozen2:
    x: int
@build_default_frozen(frozen=False)
class Mutable2:
    x: int

def f(mut1: Mutable1, mut2: Mutable2, froz1: Frozen1, froz2: Frozen2):
    mut1.x = 42
    mut2.x = 42
    froz1.x = 42  # E: frozen dataclass member
    froz2.x = 42  # E: frozen dataclass member
    "#,
);

testcase!(
    test_class_keyword,
    r#"
from typing import dataclass_transform
@dataclass_transform()
class Base: ...
class Data(Base, frozen=True):
    x: int
data = Data(x=0)
data.x = 42  # E: frozen dataclass member
    "#,
);

testcase!(
    bug = "`field` should be treated as a field specifier rather than a default",
    test_field_specifier,
    r#"
from typing import dataclass_transform, Any
def field(**kwargs) -> Any: ...
@dataclass_transform(field_specifiers=(field,))
def build(x): ...
@build
class C:
    x: int = field()
C(x=0)
C()  # Should be an error
    "#,
);
