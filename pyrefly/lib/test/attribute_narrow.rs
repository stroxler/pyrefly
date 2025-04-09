/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

testcase!(
    bug = "We do not yet support trivial attribute narrowing",
    test_one_level_simple,
    r#"
from typing import reveal_type
class Foo:
    x: Foo
class Bar(Foo):
    pass
def f(foo: Foo):
    if isinstance(foo.x, Bar):
        reveal_type(foo)  # E: revealed type: Foo
"#,
);

testcase!(
    bug = "We do not yet support trivial attribute narrowing",
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
        reveal_type(foo)  # E: revealed type: Foo
"#,
);

testcase!(
    bug = "We do not yet support trivial attribute narrowing",
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
        reveal_type(foo)  # E: revealed type: Foo
"#,
);
