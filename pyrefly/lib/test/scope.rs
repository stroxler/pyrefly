/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

testcase!(
    test_method_cannot_see_class_scope,
    r#"
class C:
    x: int

    def m(self) -> None:
        x  # E: Could not find name `x`
"#,
);

testcase!(
    test_more_class_scope,
    r#"
x: int = 0
class C:
    x: str = x # E: `int` is not assignable to `str`
    y: int = x # E: `str` is not assignable to `int`
    def m(self) -> str:
        # x refers to global x: int
        return x # E: Returned type `int` is not assignable to declared return type `str`
"#,
);

testcase!(
    test_global_simple,
    r#"
x: str = ""
def f():
  global x
  x = "foo"
"#,
);

testcase!(
    test_global_ref_before_def,
    r#"
def f():
    global x
    x = "foo"
x: str = ""
"#,
);

testcase!(
    test_global_not_found,
    r#"
x: str = ""
global a  # E: Could not find name `a`
"#,
);

testcase!(
    test_global_assign_before_def,
    r#"
x: str = ""
global x  # E: `x` was assigned in the current scope before the global declaration
"#,
);

testcase!(
    bug = "We fail to correctly mark `g()` as allowing augmented assign.",
    test_global_aug_assign,
    r#"
x: str = ""
def f():
    x += "a"  # E: `x` is not mutable from the current scope
def g():
    global x
    # Should be okay!!
    x += "a"  # E: `x` is not mutable from the current scope
def h0():
    global x
    def h1():
        x += "a"   # E: `x` is not mutable from the current scope
"#,
);

testcase!(
    bug = "We should not allow `del` on globals or nonlocals. It is allowed at runtime but is not safe / statically analyzable.",
    test_global_del,
    r#"
x: str = ""
def f():
    global x
    del x  # Not okay: it will work at runtime, but is not statically analyzable
f()
x += "foo"  # This will crash at runtime!
"#,
);

testcase!(
    test_global_assign_incompatible,
    r#"
x: str = ""
def f():
    global x
    x = 1  # E: `Literal[1]` is not assignable to variable `x` with type `str`
def g():
    x = 1  # OK, this is a new x
"#,
);

testcase!(
    test_global_after_local_define,
    r#"
x: str = ""
def f() -> None:
    y: int = 1
    global y  # E: `y` was assigned in the current scope before the global declaration
"#,
);

testcase!(
    bug = "We should not treat global like nonlocal with an error, we should actually find global",
    test_global_can_see_past_enclosing_scopes,
    r#"
from typing import assert_type
x: str = ""
def outer():
    x: int = 5
    def f():
        # This works fine in Python, `x` is the global `x`, not the one from `outer`.
        global x  # E: Found `x`, but it was not the global scope
        assert_type(x, str)  # E: assert_type(int, str)
"#,
);

testcase!(
    test_nonlocal_simple,
    r#"
def f(x: int) -> None:
    def g():
        nonlocal x
        x = 1
"#,
);

testcase!(
    test_nonlocal_ref_before_def,
    r#"
def f(x: int) -> None:
    def g():
        nonlocal x
        x = 1
"#,
);

testcase!(
    test_nonlocal_not_found,
    r#"
def f() -> None:
    def g() -> None:
        nonlocal a
        a = "foo"
    a: str = ""
"#,
);

testcase!(
    test_nonlocal_assign_before_def,
    r#"
def f() -> None:
    a: str = ""
    nonlocal a  # E: `a` was assigned in the current scope before the nonlocal declaration
"#,
);

testcase!(
    bug = "We fail to correctly mark `g()` as allowing an aug assign",
    test_nonlocal_aug_assign,
    r#"
def outer():
    x: str = ""
    def f():
        x += "a"  # E: `x` is not mutable from the current scope
    def g():
        nonlocal x
        # Should be okay!!
        x += "a"  # E: `x` is not mutable from the current scope
    def h0():
        nonlocal x
        def h1():
            x += "a"   # E: `x` is not mutable from the current scope
"#,
);

testcase!(
    bug = "We should not allow `del` on globals or nonlocals. It is allowed at runtime but is not safe / statically analyzable.",
    test_nonlocal_del,
    r#"
def outer():
    x: str = ""
    def f():
        nonlocal x
        del x  # Not okay: it will work at runtime, but is not statically analyzable
    f()
    x += "foo"  # This will crash at runtime!
"#,
);

testcase!(
    test_nonlocal_assign_incompatible,
    r#"
def f() -> None:
    a: str = ""
    def g() -> None:
        nonlocal a
        a = 1  # E: `Literal[1]` is not assignable to variable `a` with type `str`
    def h() -> None:
        a = 1  # OK, this is a new a
"#,
);

testcase!(
    test_nonlocal_multiple_annotations,
    r#"
def f() -> None:
    a: str = ""
    def g() -> None:
        nonlocal a
        a: int = 1  # E: `a` cannot be annotated with `int`, it is already defined with type `str`
"#,
);

testcase!(
    test_nonlocal_not_in_local_scope,
    r#"
x: str = ""
def f() -> None:
    nonlocal x  # E: Found `x`, but it was not in a valid enclosing scope
"#,
);

testcase!(
    test_del_name,
    r#"
x: int
x + 1  # E: `x` is uninitialized
x = 1
x + 1  # OK
del x
x + 1  # E: `x` is uninitialized

y = 1
y + 1  # OK
del y
y + 1  # E: `y` is uninitialized

# check that we don't fall back to Any when the variable is annotated
z: int
z = str(z)  # E: `z` is uninitialized  # E: `str` is not assignable to variable `z` with type `int`
"#,
);

testcase!(
    test_uninitialized_merge_flow,
    r#"
def test(cond: bool):
    if cond:
        a: int
    else:
        a = 1
    a  # E: `a` may be uninitialized
    if cond:
        b: int
    else:
        b = 1
        del b
    b  # E: `b` is uninitialized
    if cond:
        c: int
    else:
        c: int = 1
    c  # E: `c` may be uninitialized
    if cond:
        d = 1
    else:
        d = 1
        del d
    d  # E: `d` may be uninitialized
    if cond:
        e = 1
    else:
        e: int
    e  # E: `e` may be uninitialized
    if cond:
        f = 1
        del f
    else:
        f: int
    f  # E: `f` is uninitialized
    if cond:
        g = 1
        del g
    else:
        g = 1
    g  # E: `g` may be uninitialized
    if cond:
        h = 1
        del h
    else:
        h = 1
        del h
    h  # E: `h` is uninitialized
"#,
);
