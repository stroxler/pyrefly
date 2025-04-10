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
    bug = "We do not yet handle attribute narrows in attribute reads at all",
    test_attribute_read_simple,
    r#"
from typing import assert_type
class Foo:
    x: Foo
class Bar(Foo):
    pass
def f(foo: Foo):
    if isinstance(foo.x, Bar):
        assert_type(foo.x, Foo)  # Should be Bar
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
