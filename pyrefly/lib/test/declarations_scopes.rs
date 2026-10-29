/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! The shared declaration walker and lexical navigation compared to Pyrefly's own.
//!
//! `Definitions::new` (the shared walker with Pyrefly's policies) must produce exactly what
//! `Definitions::new_legacy` produces, every field in insertion order, under several
//! environments. Bindings built with the shared machinery must be identical to legacy
//! bindings, and their report must show that the shared machinery ran.

use pretty_assertions::assert_eq;
use pyrefly_python::ast::Ast;
use pyrefly_python::module_name::ModuleName;
use pyrefly_python::sys_info::PythonPlatform;
use pyrefly_python::sys_info::PythonVersion;
use pyrefly_python::sys_info::SysInfo;
use pyrefly_util::visit::Visit;
use ruff_python_ast::ExceptHandler;
use ruff_python_ast::PySourceType;
use ruff_python_ast::Stmt;

use crate::binding::scope::ScopeReport;
use crate::export::definitions::Definitions;
use crate::module::bundled::BundledStub;
use crate::module::typeshed::typeshed;
use crate::test::bindings_driver::prepare;

fn environments() -> [SysInfo; 3] {
    [
        SysInfo::default(),
        SysInfo::new(PythonVersion::new(3, 8, 0), PythonPlatform::new("win32")),
        SysInfo::new_without_type_checking(
            PythonVersion::new(3, 14, 0),
            PythonPlatform::new("darwin"),
        ),
    ]
}

/// The bodies the binder collects definitions for: the module, every function and
/// class body, and every `except` clause body (for `try` fallbacks).
fn scope_bodies<'a>(body: &'a [Stmt], out: &mut Vec<&'a [Stmt]>) {
    fn visit<'a>(x: &'a Stmt, out: &mut Vec<&'a [Stmt]>) {
        match x {
            Stmt::FunctionDef(x) => scope_bodies(&x.body, out),
            Stmt::ClassDef(x) => scope_bodies(&x.body, out),
            _ => {
                if let Stmt::Try(x) = x {
                    for ExceptHandler::ExceptHandler(h) in &x.handlers {
                        out.push(&h.body);
                    }
                }
                x.recurse(&mut |x| visit(x, out));
            }
        }
    }
    out.push(body);
    for x in body {
        visit(x, out);
    }
}

/// Assert that both collections of definitions agree on every scope body of `source`.
fn assert_definitions_parity(source: &str, module: ModuleName, is_init: bool) {
    let body = Ast::parse(source, PySourceType::Stub).0.body;
    let mut bodies = Vec::new();
    scope_bodies(&body, &mut bodies);
    for sys_info in environments() {
        for body in &bodies {
            let shared = Definitions::new(body, module, is_init, sys_info);
            let legacy = Definitions::new_legacy(body, module, is_init, sys_info);
            assert_eq!(format!("{shared:#?}"), format!("{legacy:#?}"));
        }
    }
}

/// Fixtures for the definitions of every scope body.
const DEFINITIONS: &[&str] = &[
    // Walrus targets in every position, in comprehensions and around lambdas.
    r#"
y: int = (x0 := 42)
y = (x1 := 42)
y += (x2 := 42)
(x3 := 42)
with (x4 := 42) as y: pass
for y in (x5 := 42): pass
while (x6 := True): pass
match (x7 := 42):
    case int(z) if (x8 := z): pass
    case [*rest] if (x9 := rest): pass
(x10 := 42)[y] = 42
assert (x11 := 42), (x12 := "oops")
@decorator(x13 := 42)
def foo(a=(x14 := 1), *, b=(x15 := 2)): pass
@decorator(x16 := 42)
class Foo(Base, metaclass=(x17 := Meta)): pass
type Alias = (x18 := int)
lambda a=(x19 := 1): (x20 := 42)
[(e1 := 1) for _ in (e2 := source) if (e3 := 2)]
{(k := 1): (v := 2) for _ in [(e4 := 1) for _ in (e5 := source)]}
[lambda a=(e6 := 1): (e7 := 2) for _ in []]
((a := (b := source)) for _ in range(3))
[[(deep := 1) for _ in inner] for inner in (outer := [])]
x21 if (x22 := c) else (x23 := d)
a[(x24 := 0)] += 1
del (x25 := {"x": 42})["x"], gone
if a:
    pass
elif (x26 := b):
    pass
try:
    pass
except (x27 := Exception) as err:
    pass
raise (x28 := E) from (x29 := F)
def ret():
    return (x30 := 1)
"#,
    // Environment pruning, main guards and conditional expressions.
    r#"
import sys
from typing import TYPE_CHECKING
if sys.version_info >= (3, 10):
    new = 1
elif sys.version_info >= (3, 9):
    mid = 1
else:
    old = 1
if sys.platform == "win32":
    win = 1
elif (also := sys.platform == "darwin"):
    mac = 1
if TYPE_CHECKING:
    checked = 1
else:
    unchecked = 1
if False:
    never = 1
if __name__ == "__main__":
    main_only = 1
    __all__ = ["main_only"]
    if inner:
        nested_main = 1
    __all__.append("x")
else:
    not_main = 1
pick = (t := 1) if sys.version_info >= (3, 12) else (f := 2)
picked = [(c1 := 1) if sys.platform == "linux" else (c2 := 2) for _ in []]
"#,
    // Imports, aliases, submodules and the `__all__` conventions.
    r#"
from foo import *
from bar import baz as qux, moo, moo as moo
from . import sibling
from .sub import thing
from ...too.far import nothing
import mod.ule
import mod.lue as alias
import plain as plain_alias
import plain.sub
import os as os
from _collections_abc import __all__ as __all__
__all__ = ["qux"] + foo.__all__
__all__ = alias = alias.__all__
__all__: list[str] = __all__.__all__
__all__ += ["moo"]
__all__ += [*sibling.__all__]
__all__.extend(["x"])
__all__.extend(sibling.__all__)
__all__.append("y")
__all__.remove("x")
import_thrift("path/a.thrift", "*")
import_thrift("path/b.thrift", "b_alias")
import_thrift("path/c.thrift", ["C1", (cw := "C2"), "C3"])
"#,
    // Declarations, annotations and decorators the exports care about.
    r#"
from typing import Final, final, overload
import typing
from warnings import deprecated
X: Final = "x"
Y: typing.Final[int] = 1
Z: Final[str]
w: int
w = 2
(paren): int = 3
obj.attr: int = 4
@deprecated("gone")
class Old: ...
@deprecated("gone")
def old(): ...
@overload
@deprecated("one")
def over(x: int) -> int: ...
def over(x): ...
global g
nonlocal n
g = 1
del d
del d
for i, (j, *k) in []:
    pass
with a as (b, c), d as e[f]:
    pass
try:
    import tried
except ImportError as e1:
    tried = None
except* OSError as e2:
    pass
else:
    elsed = 1
finally:
    fin = 1
match subject:
    case {"key": value, **others}:
        pass
    case Point(x=px, y=py) | Point(px, py):
        pass
    case _:
        wild = 1
type Generic[T] = list[T]
class C:
    attr = 1
    def method(self): ...
def outer():
    inner = 1
"#,
];

#[test]
fn definitions_match_legacy() {
    for source in DEFINITIONS {
        assert_definitions_parity(source, ModuleName::from_str("main"), false);
        assert_definitions_parity(source, ModuleName::from_str("pkg.sub"), true);
    }
}

/// Definitions of every module in the bundled typeshed, which exercises the environment
/// pruning that stubs rely on.
#[test]
fn typeshed_definitions_match_legacy() {
    let typeshed = typeshed().unwrap();
    let mut modules = 0;
    for (path, contents) in typeshed.load_map() {
        let mut components: Vec<_> = path
            .with_extension("")
            .components()
            .map(|c| c.as_os_str().to_string_lossy().into_owned())
            .collect();
        let is_init = components.last().is_some_and(|c| c == "__init__");
        if is_init {
            components.pop();
        }
        let module = ModuleName::from_str(&components.join("."));
        assert_definitions_parity(contents, module, is_init);
        modules += 1;
    }
    assert!(modules > 500, "only {modules} typeshed modules");
}

/// Bind `source` with and without the shared machinery, assert identical bindings, and
/// return what the shared machinery did.
fn bindings_parity(source: &str) -> ScopeReport {
    let fixture = prepare(source);
    fixture.assert_parity();
    assert!(fixture.build(false).bindings.scope_report().is_none());
    *fixture
        .build(true)
        .bindings
        .scope_report()
        .expect("a driven build uses the shared declarations and navigation")
}

#[test]
fn bindings_with_scopes_match_legacy() {
    let report = bindings_parity(
        r#"
import sys
from typing import TypeVar, Generic

T = TypeVar("T")
counter = 0

class Box(Generic[T]):
    value: T
    alias = value
    def get(self) -> T:
        return self.value
    class Inner:
        def peek(self) -> "Box[int]":
            return Box()
    items = [value for _ in range(3)]
    pairs = [x for x in alias]
    type Field = list[T]
    def generic_method[S](self, x: S, y: "alias") -> S:
        return x

class Pair[K]:
    first: K
    def swap[V](self, v: V) -> tuple[V, K]:
        return v, self.first

def bump() -> int:
    global counter
    counter += 1
    return counter

def make():
    total = 0
    def add(n: int) -> int:
        nonlocal total
        total += n
        return total
    def reader():
        return total
    return add, reader

def walrus(xs: list[int]):
    if any((hit := x) > 0 for x in xs):
        return hit
    return [y for x in xs if (y := x * 2)]

def default(a=(d := 1)):
    return a

def generic[S](x: S) -> S:
    return x

type Pair[K] = tuple[K, K]

if sys.version_info >= (3, 12):
    only_new = 1
else:
    only_old = 1

def dead():
    if False:
        local = 1
    return local
"#,
    );
    assert_eq!(
        report,
        ScopeReport {
            // The module and the bodies of 15 functions and classes.
            declaration_walks: 16,
            visible_scope_searches: 101,
            nonlocal_searches: 1,
        }
    );
}

#[test]
fn bindings_with_capture_errors_match_legacy() {
    let report = bindings_parity(
        r#"
x = 1
def f():
    global y
    y = 1
    def g():
        nonlocal x
    def h():
        nonlocal y
    def i():
        nonlocal missing
class C:
    z = 1
    def m(self):
        nonlocal z
"#,
    );
    assert_eq!(
        report,
        ScopeReport {
            // The module, `C` and five functions.
            declaration_walks: 7,
            visible_scope_searches: 0,
            nonlocal_searches: 4,
        }
    );
}
