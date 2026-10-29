/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Regressions for cases where an earlier version reported a module as safe (or
//! panicked) although it has an import-time effect or cannot be analyzed.

mod common;

use common::check;
use common::check_all;

/// A class created inside a function captures the function's parameter; the class
/// escapes through the return value, so the parameter is substituted inside it.
#[test]
fn returned_class_member_is_substituted() {
    check(
        r#"
import foreign
def make(x):
    class C:
        y = x
    return C
make(foreign.items).y.append(1)  # E: imported-mutation
"#,
    );
}

/// `staticmethod` and `classmethod` bind as descriptors: no receiver, or the class.
#[test]
fn static_and_class_methods_bind_correctly() {
    check(
        r#"
import foreign

class C:
    @staticmethod
    def f(x):
        x.append(1)

    @classmethod
    def g(cls, x):
        x.append(1)

C().f(foreign.items)  # E: imported-argument
C.f(foreign.items)  # E: imported-argument
C.g(foreign.items)  # E: imported-argument
C().g(foreign.items)  # E: imported-argument
C.f([])
C().g([])
"#,
    );
}

/// Writing or deleting a property runs its setter or deleter.
#[test]
fn property_setters_and_deleters_run() {
    check(
        r#"
import foreign

class P:
    @property
    def v(self):
        return 1

    @v.setter
    def v(self, value):
        foreign.items.append(value)

    @v.deleter
    def v(self):
        foreign.items.clear()

class ReadOnly:
    @property
    def v(self):
        return 1

p = P()
p.v = 1  # E: unsafe-call
del p.v  # E: unsafe-call
ReadOnly().v = 1  # E: unknown-call
"#,
    );
}

/// A class's `__setattr__` and `__delattr__` run on instance attribute writes.
#[test]
fn custom_setattr_runs() {
    check(
        r#"
import foreign

class S:
    def __setattr__(self, name, value):
        foreign.items.append(value)

    def __delattr__(self, name):
        foreign.items.clear()

s = S()
s.x = 1  # E: unsafe-call
del s.x  # E: unsafe-call
setattr(s, "y", 2)  # E: unsafe-call
"#,
    );
}

/// Class values are snapshots of the class body, so later writes that change method
/// dispatch are reported as unknown rather than silently ignored.
#[test]
fn dynamic_class_writes_are_unknown() {
    check(
        r#"
import foreign

class K:
    def m(self):
        pass

K.m = lambda self: foreign.items.append(1)  # E: unknown-mutation
k = K()
k.m = print  # E: unknown-mutation
k.data = 1
"#,
    );
}

/// `typing.cast` returns its second argument, and `min`/`max` return an element.
/// Container contents are not tracked, so an element of a copied container is unknown.
#[test]
fn returned_arguments_keep_their_origin() {
    check(
        r#"
import foreign
import typing
from typing import cast

x = cast("list", foreign.items)
x[0] = 1  # E: imported-mutation
y = typing.cast(typ="list", val=foreign.other)
y.append(1)  # E: imported-mutation
z = max(foreign.lists)
z.append(1)  # E: imported-mutation
t = tuple(foreign.lists)
t[0].append(1)  # E: unknown-mutation
"#,
    );
}

/// `next` runs a generator, and a generator keeps the arguments it was created with.
#[test]
fn generators_run_with_their_arguments() {
    check(
        r#"
import foreign

def gen():
    foreign.items.append(1)
    yield 1

def mutator(x):
    x.append(1)
    yield x

first = next(gen())  # E: unsafe-call
list(mutator(foreign.items))  # E: imported-argument
list(mutator([]))
"#,
    );
}

/// Special methods that a project class defines run when operators, iteration,
/// subscripts, truth tests, attribute fallbacks or builtins use its instances.
#[test]
fn protocol_methods_run() {
    check(
        r#"
import foreign

class Loud:
    def __iter__(self):
        foreign.items.append(1)
        yield 1
    def __getitem__(self, key):
        foreign.items.append(key)
    def __add__(self, other):
        foreign.items.append(other)
    def __bool__(self):
        foreign.items.clear()
        return True
    def __getattr__(self, name):
        foreign.items.append(name)
    def __len__(self):
        foreign.items.append(0)
        return 0

class Quiet:
    pass

loud = Loud()
for _ in loud:  # E: unsafe-call
    pass
loud[0]  # E: unsafe-call
loud + 1  # E: unsafe-call
if loud:  # E: unsafe-call
    pass
loud.missing  # E: unsafe-call
len(loud)  # E: unsafe-call
quiet = Quiet()
if quiet:
    pass
len(quiet)
"#,
    );
}

/// `functools.cached_property` runs its function on read, a dataclass default factory
/// runs on construction (not modeled), and `async with` uses `__aenter__`.
#[test]
fn library_descriptors_and_async_context_managers() {
    check(
        r#"
import dataclasses
import foreign
import functools

class C:
    @functools.cached_property
    def value(self):
        foreign.items.append(1)
        return 1

@dataclasses.dataclass
class D:
    items: list = dataclasses.field(default_factory=list)  # E: unknown-call

class Manager:
    async def __aenter__(self):
        foreign.items.append(1)
    async def __aexit__(self, *args):
        pass

async def use():
    async with Manager():
        pass

C().value  # E: unsafe-call
for _ in use():  # E: unsafe-call
    pass
"#,
    );
}

/// Builtins run eager callbacks (`sorted`, `min`, `max` with `key=`, `list.sort`). Any
/// callback given to a lazy builtin (`map`, `filter`, `iter(callable, sentinel)`) is
/// unknown, and so is a builtin callback that may run special methods of the items.
#[test]
fn builtin_callbacks() {
    check(
        r#"
import foreign

def key(x):
    foreign.registry.append(x)
    return x

def pure_key(x):
    return x

sorted([1], key=key)  # E: unsafe-call
max([1], key=key)  # E: unsafe-call
items = [2, 1]
items.sort(key=key)  # E: unsafe-call
sorted([1], key=pure_key)
sorted([1], key=None)
list(map(key, [1]))  # E: unknown-call
list(map(str, [1]))  # E: unknown-call
list(map(exec, ["x = 1"]))  # E: unknown-call
list(map(open, ["/tmp/not-opened"]))  # E: unknown-call
list(filter(input, ["prompt"]))  # E: unknown-call
next(iter(input, "stop"))  # E: unknown-call
sorted(["/tmp/x"], key=open)  # E: prohibited-call
sorted([1], key=id)
filter(None, [1])
list(key(x) for x in [1])  # E: unsafe-call

class Loud:
    def __str__(self):
        foreign.registry.append(self)
        return ""

list(map(str, [Loud()]))  # E: unknown-call
sorted([Loud()], key=str)  # E: unknown-call
"#,
    );
}

/// A generator passed to a project function runs if the function iterates it.
#[test]
fn generators_passed_to_functions_run_where_iterated() {
    check(
        r#"
import foreign

def gen():
    foreign.registry.append(1)
    yield 1

def drain(xs):
    for _ in xs:
        pass

def keep(xs):
    return xs

drain(gen())  # E: unsafe-call
held = keep(gen())
for _ in held:  # E: unsafe-call
    pass
for _ in enumerate(gen()):  # E: unsafe-call
    pass
"#,
    );
}

/// The class namespace holds the class body's own bindings: a deleted class attribute
/// is absent, not the global of the same name.
#[test]
fn deleted_class_attribute_is_absent() {
    check(
        r#"
import foreign

def helper():
    foreign.registry.append(1)

class C:
    helper = 1
    del helper

C.helper()  # E: unknown-call
"#,
    );
}

/// A module that may raise still exposes the globals bound on the raising path to an
/// importer that catches the exception.
#[test]
fn globals_join_normal_and_raising_exits() {
    check_all(&[
        (
            "a",
            r#"
import foreign
items = []
if foreign.flag:
    items = foreign.items
    raise ValueError()  # E: unhandled-raise
"#,
        ),
        (
            "b",
            r#"
from a import items
items.append(1)  # E: imported-mutation
"#,
        ),
    ]);
}

/// Many functions and globals: call summaries carry only the cells a call can observe.
#[test]
fn many_decorated_functions() {
    let mut source = String::from(
        "def deco(f):
    return f
",
    );
    for i in 0..1000 {
        source.push_str(&format!(
            "@deco
def f{i}():
    pass
"
        ));
    }
    check(&source);
}

/// Deleting a global exposes the builtin of the same name, both after a call that
/// deletes it and inside a function called after the module deletes it.
#[test]
fn deleted_globals_fall_back_to_builtins() {
    check(
        r#"
open = lambda *args: None

def remove():
    global open
    del open

def use():
    open("x")

open("fine")
use()
remove()
open("oops")  # E: prohibited-call
use()  # E: unsafe-call
"#,
    );
}

#[test]
fn module_level_delete_falls_back_to_builtin() {
    check(
        r#"
open = lambda *args: None

def use():
    open("x")

use()
del open
use()  # E: unsafe-call
"#,
    );
}
