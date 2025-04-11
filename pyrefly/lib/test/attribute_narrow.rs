/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

testcase!(
    test_one_level_simple,
    r#"
from typing import reveal_type
class Foo:
    x: Foo
class Bar(Foo):
    pass
def f(foo: Foo):
    if isinstance(foo.x, Bar):
        reveal_type(foo)  # E: revealed type: Foo (_.x: Bar)
"#,
);

testcase!(
    test_one_level_and,
    r#"
from typing import reveal_type
class Foo:
    x: Foo
    y: Foo
class Bar(Foo):
    pass
class Baz(Foo):
    pass
def f(foo: Foo):
    if isinstance(foo.x, Bar) and isinstance(foo.y, Baz):
        reveal_type(foo)  # E: revealed type: Foo (_.x: Bar, _.y: Baz)
"#,
);

testcase!(
    test_two_levels,
    r#"
from typing import reveal_type
class Foo:
    x: Foo
    y: Foo
class Bar(Foo):
    pass
def f(foo: Foo):
    if isinstance(foo.x.y, Bar):
        reveal_type(foo)  # E: revealed type: Foo (_.x.y: Bar)
"#,
);

testcase!(
    test_attribute_read_simple,
    r#"
from typing import assert_type
class Foo:
    x: Foo
class Bar(Foo):
    pass
def f(foo: Foo):
    if isinstance(foo.x, Bar):
        assert_type(foo.x, Bar)
    if isinstance(foo.x.x, Bar):
        assert_type(foo.x, Foo)
        assert_type(foo.x.x, Bar)
        assert_type(foo.x.x.x, Foo)
"#,
);

testcase!(
    test_attribute_narrow_type_algebra_for_correct_narrows,
    r#"
from typing import assert_type
class Foo:
    x: Foo | None
def f(foo: Foo):
    if foo.x is not None:
        assert_type(foo.x, Foo)
    if foo.x is not None and foo.x.x is not None:
        assert_type(foo.x, Foo)
        assert_type(foo.x.x, Foo)
    if foo.x is not None and foo.x.x is not None and foo.x.x.x is None:
        assert_type(foo.x, Foo)
        assert_type(foo.x.x, Foo)
        assert_type(foo.x.x.x, None)
"#,
);

// The expected behavior when narrowing an invalid attribute chain is to produce
// type errors at the narrow site, but apply the narrowing downstream
// (motivation: being noisy downstream could be quite frustrating for gradually
// typed code)
testcase!(
    test_invalid_narrows_on_bad_attribute_access,
    r#"
from typing import assert_type, Any
class Foo:
    x: Foo | None
def f(foo: Foo):
    if foo.x is not None:
        assert_type(foo.x, Foo)
    if foo.x.x is not None:  # E: Object of class `NoneType` has no attribute `x`
        assert_type(foo.x, Foo | None)
        # Why `Foo | object`? Because the lookup on `None` fails, and we fall back to `object`
        # in that branch of the union.
        assert_type(foo.x.x, Foo | object)
    if isinstance(foo.x.y, Foo) and foo.x.y.x is not None:  # E: Object of class `Foo` has no attribute `y`  # E: Object of class `NoneType` has no attribute `y`
        assert_type(foo.x, Foo | None)
        assert_type(foo.x.y, Foo)
        assert_type(foo.x.y.x, Foo)
"#,
);

testcase!(
    bug = "PyTorch TODO: implement attribute narrowing",
    test_attr_refine,
    r#"
from typing import Any, Optional, reveal_type

class N:
    type: Optional[Any]

def add_inference_rule(n: N):
    reveal_type(n) # E: revealed type: N
    reveal_type(n.type) # E: revealed type: Any | None
    n.type = 3
    reveal_type(n.type + 3) # E: revealed type: int | Unknown # E: `+` is not supported between `None` and `Literal[3]`
"#,
);

testcase!(
    bug = "PyTorch TODO: Implement gettitem narrowing",
    test_attr_arg,
    r#"
from typing import Any, Optional, reveal_type

Arg = Optional[tuple["Arg", ...]]

class N:
    _args: tuple["Arg", ...]
    type: Optional[Any]

    @property
    def args(self) -> tuple[Arg, ...]:
        return self._args


def f(n: N):
    assert isinstance(n.args[0], N)
    t1 = n.args[0].type # E:  Object of class `NoneType` has no attribute `type` # E: Object of class `tuple` has no attribute `type`
    reveal_type(t1) # E: revealed type: Error
"#,
);

testcase!(
    test_attribute_access_after_narrowing_in_subclass,
    r#"
from typing import assert_type
from unittest import TestCase

class A:
    def __init__(self) -> None:
        self.id = 5

class B:
    def __init__(self, a : A | None) -> None:
        self.a = a

b = B(A())
if b.a is not None:
    x = b.a
    assert_type(x, A)
"#,
);
