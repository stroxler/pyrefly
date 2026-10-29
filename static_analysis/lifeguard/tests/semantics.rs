/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Behavior that the shared frontend and engine make possible beyond the upstream
//! fixtures: flow-sensitive rebinding, invocation-time closure cells, deferred
//! generator bodies, value-flow aliases of hazards, and explicit unknown results.

mod common;

use std::fs;
use std::process::Command;

use common::check;
use common::check_all;
use pyrefly_static_analysis_lifeguard::Verdict;

#[test]
fn local_rebinding_versus_external_mutation() {
    check(
        r#"
import foo
x = foo.A
x = []
x.append(1)
y = foo.B
y.append(1)  # E: imported-mutation
if foo.flag:
    z = foo.C
else:
    z = []
z.append(1)  # E: imported-mutation
"#,
    );
}

/// A called function's `global` rebinding flows back into the module frame.
#[test]
fn global_rebinding_by_a_call_updates_the_module() {
    check(
        r#"
import foo
x = []
def f():
    global x
    x = foo.A
x.append(1)
f()
x.append(2)  # E: imported-mutation
"#,
    );
}

/// Closure cells are read when the closure runs, not when it is defined.
#[test]
fn closure_cells_are_read_at_invocation() {
    check(
        r#"
from foo import obj

def safe():
    target = []
    def g():
        target.append(1)
    g()

def unsafe():
    target = []
    def g():
        target.append(1)
    g()
    target = obj
    g()

safe()
unsafe()  # E: unsafe-call
"#,
    );
}

/// A closure that escapes its defining activation has unknown cells.
#[test]
fn escaped_closure_cells_are_unknown() {
    check(
        r#"
from foo import obj

def outer(x):
    def inner():
        x.enabled = True
    return inner

h = outer(obj)
h()  # E: unknown-call
"#,
    );
}

/// Calling a generator or coroutine function does not run its body; iterating does.
/// A generator stored in a collection is unknown, because iteration through the
/// collection is not followed, and passing one to a builtin that does not iterate it
/// runs nothing.
#[test]
fn generator_bodies_run_when_iterated() {
    check(
        r#"
import foo

def gen():
    foo.x = 1
    yield 1

async def coro():
    foo.y = 1

g = gen()
c = coro()
for _ in gen():  # E: unsafe-call
    pass
items = list(gen())  # E: unsafe-call

async def runner():
    await coro()

r = runner()
for _ in [runner()]:  # E: unknown-call
    pass
print(gen())
"#,
    );
}

/// Awaiting a coroutine inside a running coroutine runs the awaited body.
#[test]
fn awaited_coroutines_run() {
    check(
        r#"
import foo

async def coro():
    foo.y = 1

def drive():
    async def runner():
        await coro()
    for _ in runner():
        pass

drive()  # E: unsafe-call
"#,
    );
}

#[test]
fn property_reads_run_the_getter() {
    check(
        r#"
import foo

class C:
    @property
    def p(self):
        foo.x = 1
        return 1

c = C()
v = c.p  # E: unsafe-call
"#,
    );
}

/// The constructor's own mutation of the new instance is local, while mutating an
/// object stored in one of its attributes is unknown: attributes are not tracked.
#[test]
fn constructor_arguments_reach_init() {
    check(
        r#"
import foo

class C:
    def __init__(self, target):
        self.items = []
        target.x = 1

class D:
    def __init__(self):
        self.items = []
        self.items.append(1)

C([])
C(foo.obj)  # E: imported-argument
D()  # E: unknown-mutation
"#,
    );
}

/// Hazards are found through value flow, not by spelling.
#[test]
fn hazards_through_aliases() {
    let report = check(
        r#"
import sys
import builtins
e = exec
e("x = 1")  # E: exec-call
m = sys.modules
m["k"] = None  # E: sys-modules-access
b = builtins
b.__import__ = None  # E: builtins-import-override
"#,
    );
    assert!(report.load_imports_eagerly());
}

#[test]
fn class_creation_with_unresolved_bases_is_unknown() {
    let report = check(
        r#"
import abc
from foo import Base

class A(Exception):
    pass

class B(abc.ABC):
    pass

class C(Base):  # E: unknown-call
    pass
"#,
    );
    assert_eq!(report.verdict(), Verdict::Unknown);
}

/// Reachable frontend gaps are reported instead of a safety claim. A failing `assert`
/// is not a raise the analysis reports, and the re-raise path out of a `try` is part of
/// the `try` gap.
#[test]
fn unsupported_constructs_are_not_safe() {
    let report = check(
        r#"
from foo import *  # E: unsupported
try:  # E: unsupported
    import bar
except ImportError:
    bar = None
def g(x):
    assert x
    return x
g(1)
"#,
    );
    assert_eq!(report.verdict(), Verdict::Unknown);
}

/// Code after an unconditional raise is unreachable and reports nothing, and a raise
/// that a handler catches does not leave the module.
#[test]
fn unreachable_code_reports_nothing() {
    check(
        r#"
import foo
try:  # E: unsupported
    raise ValueError()
except ValueError:
    pass
raise SystemExit  # E: unhandled-raise
foo.x = 1
"#,
    );
}

/// A class-body name read before the class binds it falls back to the module global.
#[test]
fn class_body_name_fallback() {
    check(
        r#"
import foo
x = foo.A
class C:
    x.append(1)  # E: imported-mutation
    x = []
    x.append(2)
"#,
    );
}

/// In an import cycle, the partially initialized module's names are unknown.
#[test]
fn import_cycle_values_are_unknown() {
    let reports = check_all(&[
        ("a", "from b import Y\nX = []\n"),
        (
            "b",
            "from a import X\nY = 1\nX.append(1)  # E: unknown-call\n",
        ),
    ]);
    let verdicts: Vec<Verdict> = reports.iter().map(|r| r.verdict()).collect();
    assert_eq!(verdicts, [Verdict::Safe, Verdict::Unknown]);
}

#[test]
fn cli_reports_and_exit_status() {
    let dir = std::env::temp_dir().join(format!("lifeguard-spike-{}", std::process::id()));
    fs::create_dir_all(&dir).unwrap();
    let a = dir.join("a.py");
    let b = dir.join("b.py");
    fs::write(
        &a,
        "REGISTRY = []\ndef register(x):\n    REGISTRY.append(x)\n",
    )
    .unwrap();
    fs::write(&b, "from a import register\nregister(1)\n").unwrap();
    let run = |args: &[String]| {
        Command::new(env!("CARGO_BIN_EXE_lifeguard-spike"))
            .args(args)
            .output()
            .unwrap()
    };
    let arg = |name: &str, path: &std::path::Path| format!("{name}={}", path.display());

    let out = run(&[arg("a", &a), arg("b", &b)]);
    let stdout = String::from_utf8(out.stdout).unwrap();
    assert_eq!(out.status.code(), Some(1), "{stdout}");
    assert!(stdout.contains("b:2:1: unsafe-call: a:3: calls `append` on an object of module `a`"));
    assert!(stdout.ends_with("a: safe\nb: unsafe\n"), "{stdout}");

    assert_eq!(run(&[arg("a", &a)]).status.code(), Some(0));
    fs::write(&b, "import foo\nfoo.f()\n").unwrap();
    assert_eq!(run(&[arg("b", &b)]).status.code(), Some(2));
    assert_eq!(run(&["no-equals-sign".to_owned()]).status.code(), Some(64));
    fs::remove_dir_all(&dir).unwrap();
}
