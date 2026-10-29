/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Fixtures adapted from the public Lifeguard test suite
//! (https://github.com/facebook/Lifeguard, commit
//! c04fde1358c467d7ec84244ddfb066186381f066). Each test names its upstream file and
//! test. Expectations use this crate's diagnostic kinds; where the result differs from
//! upstream, a comment says how and why. See `README.md` for the parity matrix.

mod common;

use common::check;
use common::check_all;

// ---------------------------------------------------------------------------
// tests/assign.rs
// ---------------------------------------------------------------------------

/// `test_assign_to_module_var`, `test_annotated_assign_to_module_var`.
#[test]
fn assign_to_module_var() {
    check(
        r#"
import foo
foo.bar = 1  # E: imported-mutation
foo.baz: int = 1  # E: imported-mutation
"#,
    );
}

/// `test_augmented_assign_to_module_var`. The store is a definite mutation; the
/// in-place operator may additionally mutate `foo.bar` itself (upstream reports only
/// the store).
#[test]
fn augmented_assign_to_module_var() {
    check(
        r#"
import foo
foo.bar += 1  # E: imported-mutation  # E: unknown-mutation
"#,
    );
}

/// `test_update_to_import_array`.
#[test]
fn update_to_import_array() {
    check(
        r#"
from foo import bar
bar[0] = 1  # E: imported-mutation
"#,
    );
}

/// `test_assign_to_import_var`: rebinding the imported name is local.
#[test]
fn assign_to_import_var() {
    check(
        r#"
from foo import bar
bar = 1
"#,
    );
}

/// `test_augmented_assign_to_import_var`. Upstream treats this as a rebinding and
/// reports nothing. Without types, `bar += 1` may call `bar.__iadd__`, which mutates a
/// list in place, so this analysis reports an unknown mutation instead of a safe verdict.
#[test]
fn augmented_assign_to_import_var() {
    check(
        r#"
from foo import bar
bar += 1  # E: unknown-mutation
"#,
    );
}

/// `test_class_attribute_shadowing_import_is_not_a_mutation`.
#[test]
fn class_attribute_shadowing_import() {
    check(
        r#"
from foo import bar

class C:
    bar = 1
"#,
    );
}

/// `test_subscript_assign_to_alias`: the alias is followed through another project module.
#[test]
fn subscript_assign_to_alias() {
    check_all(&[
        ("mod1", "A = []\n"),
        (
            "mod2",
            r#"
import mod1
x = mod1.A
x[1] = 2  # E: imported-mutation
"#,
        ),
    ]);
}

/// `test_call_with_import_var_alias`. Upstream reports `unsafe-function-call` for the
/// unresolved `x.a()`; this analysis reports it as unknown.
#[test]
fn call_with_import_var_alias() {
    check(
        r#"
import foo

baz = foo.bar

def f(x):
    x.a()

f(baz)  # E: imported-argument  # E: unknown-call
"#,
    );
}

/// `test_assign_to_builtins_import`, `..._in_function`, `..._via_alias`,
/// `test_setattr_builtins_import`, `test_delete_builtins_import`. The override is
/// reported wherever it occurs, including in functions import time never calls.
#[test]
fn builtins_import_override() {
    check(
        r#"
import builtins
import builtins as b

def hook(name, *args, **kwargs):
    return name

builtins.__import__ = hook  # E: builtins-import-override

def install(hook):
    b.__import__ = hook  # E: builtins-import-override
    setattr(builtins, "__import__", hook)  # E: builtins-import-override

def uninstall():
    del builtins.__import__  # E: builtins-import-override
"#,
    );
}

/// `test_assign_to_other_builtins_attr`, `test_read_builtins_import`, and the
/// destructuring variants.
#[test]
fn other_builtins_attributes() {
    check(
        r#"
import builtins
builtins.my_helper = 1  # E: imported-mutation
original = builtins.__import__
builtins.other, x = 1, 2  # E: imported-mutation
[builtins.third, y] = 1, 2  # E: imported-mutation
a, (builtins.fourth, c) = 1, (2, 3)  # E: imported-mutation
"#,
    );
}

// ---------------------------------------------------------------------------
// tests/basic.rs
// ---------------------------------------------------------------------------

/// `test_getattr_literal`, `test_setattr_literal`: attributes of a function this module
/// defines are local state.
#[test]
fn literal_getattr_setattr() {
    check(
        r#"
def f(): ...
getattr(f, "__module__")
setattr(f, "__module__", "foo")
"#,
    );
}

/// `test_getattr_nonliteral`, `test_setattr_nonliteral`. The upstream fixtures bind the
/// name to a string literal first; flow-sensitive constants resolve that, so a truly
/// computed name is used here and the literal-variable form is safe.
#[test]
fn computed_getattr_setattr() {
    check(
        r#"
def f(): ...
x = "foo"
getattr(f, x)
name = x + "bar"
getattr(f, name)  # E: prohibited-call
setattr(f, name, 1)  # E: prohibited-call
"#,
    );
}

/// `test_setattr_global_assign`, `test_global_var_assign`: mutating or rebinding a
/// module's own state is safe while that module is importing, and an effect when
/// another module's import runs it.
#[test]
fn own_state_is_safe_only_for_its_own_import() {
    check_all(&[
        (
            "a",
            r#"
counter = 0
def f(): ...
def g():
    setattr(f, "__module__", "foo")
def h():
    global counter
    counter = 1
g()
h()
"#,
        ),
        (
            "b",
            r#"
from a import g, h
g()  # E: unsafe-call
h()  # E: unsafe-call
"#,
        ),
    ]);
}

/// `test_imported_subscript_access`, `test_imported_subscript_assignment`.
#[test]
fn imported_subscript() {
    check(
        r#"
from foo import bar
bar['hello']
bar['hello'] = 10  # E: imported-mutation
"#,
    );
}

/// `test_unsafe_subexpression`: a raise in the callee makes the call unsafe.
#[test]
fn unsafe_subexpression() {
    check(
        r#"
def f(x, y):
    raise(ValueError())
def g(x, y): return x + y
a = g(1, 2) + g(4, f(4, 5))  # E: unsafe-call
"#,
    );
}

/// `test_nested_class`. Reported at the class statement rather than at `__del__`.
#[test]
fn custom_finalizer() {
    check(
        r#"
class A:
    class B:  # E: custom-finalizer
        def __del__(self): ...
"#,
    );
}

// ---------------------------------------------------------------------------
// tests/calls.rs
// ---------------------------------------------------------------------------

/// `test_top_level_function_call`, `test_top_level_function_call_from_import`.
#[test]
fn computed_getattr_through_import() {
    check_all(&[
        (
            "a",
            r#"
def f(x, y):
    getattr(x, y)
"#,
        ),
        (
            "b",
            r#"
from a import f
getattr(x, y)  # E: prohibited-call
f(1, 2)  # E: unsafe-call
"#,
        ),
    ]);
}

/// `test_top_level_call_through_package_re_export`.
#[test]
fn call_through_package_re_export() {
    check_all(&[
        (
            "pkg.impl",
            r#"
REGISTRY = []
def register(x):
    REGISTRY.append(x)
class Registered:
    def __init__(self):
        REGISTRY.append(self)
"#,
        ),
        ("pkg", "from pkg.impl import register, Registered\n"),
        (
            "consumer",
            r#"
import pkg
from pkg import register
pkg.register(1)  # E: unsafe-call
register(1)  # E: unsafe-call
pkg.Registered()  # E: unsafe-call
"#,
        ),
    ]);
}

/// `test_safe_call_through_package_re_export`.
#[test]
fn safe_call_through_package_re_export() {
    check_all(&[
        (
            "pkg.impl",
            r#"
def pure(x):
    return x + 1
class Plain:
    def __init__(self):
        self.x = 1
"#,
        ),
        ("pkg", "from pkg.impl import pure, Plain\n"),
        (
            "consumer",
            r#"
import pkg
from pkg import pure
pkg.pure(1)
pure(1)
pkg.Plain()
"#,
        ),
    ]);
}

/// `test_parameterized_decorator_through_package_re_export`,
/// `test_helper_calling_package_re_export`.
#[test]
fn decorator_and_helper_through_re_export() {
    check_all(&[
        (
            "pkg.impl",
            r#"
REGISTRY = []
def register(name):
    def wrap(f):
        REGISTRY.append(f)
        return f
    return wrap
def add(x):
    REGISTRY.append(x)
"#,
        ),
        ("pkg", "from pkg.impl import register, add\n"),
        (
            "helper",
            r#"
import pkg
def add_later(x):
    pkg.add(x)
"#,
        ),
        (
            "consumer",
            r#"
from pkg import register
from helper import add_later
@register("f")  # E: unsafe-call
def f():
    pass
add_later(1)  # E: unsafe-call
"#,
        ),
    ]);
}

/// `test_definition_with_side_effect_call`, `test_local_function_call`.
#[test]
fn deferred_and_safe_local_calls() {
    check(
        r#"
import os

def foo():
    os.path.join("a", "b")

def f(x, y): ...
f("a", "b")
a = f("a", "b")
"#,
    );
}

/// `test_unsafe_call_returned_from_a_function`. `f` comes from a module outside the
/// project, so the call is unknown here (upstream: `unsafe-function-call`).
#[test]
fn unresolved_call_returned_from_a_function() {
    check(
        r#"
from foo import f
def g():
    return f()

a = g()  # E: unknown-call
"#,
    );
}

/// `test_unsafe_local_function_call`: only the import-time call site is reported.
#[test]
fn unsafe_local_function_chain() {
    check(
        r#"
from foo import f
def g():
    f.x = 10

def h():
    g()

a = h()  # E: unsafe-call
"#,
    );
}

/// `test_unsafe_recursive_function_call`. Recursion is solved to a fixpoint and its
/// termination is reported as unknown (upstream: `unsafe-function-call`).
#[test]
fn recursive_call() {
    check(
        r#"
def f():
    g()

def g():
    f()

a = f()  # E: unknown-call
"#,
    );
}

/// `test_exec`, `test_exec_in_nested_function`.
#[test]
fn exec_in_nested_function() {
    check(
        r#"
def f(x):
    def g(x):
        exec("import foo")  # E: exec-call
"#,
    );
}

/// `test_method_call`, `test_safe_method_call`, `test_method_call_on_constructor_result`.
#[test]
fn method_calls() {
    check(
        r#"
class C:
    def m(self):
        raise Exception()
    def ok(self):
        pass
a = C()
a.ok()
a.m()  # E: unsafe-call
C().ok()
C().m()  # E: unsafe-call
"#,
    );
}

/// `test_method_call_on_literal_safe`.
#[test]
fn method_call_on_literal() {
    check(
        r#"
a = ":".join(["x", "y"])
b = "hello".upper()
c = [1, 2].index(1)
d = {"k": 1}.get("k")
e = (1, 2).count(1)
f = {1, 2}.union({3})
g = b"x".decode()
h = f"v={a}".strip()
i = [x for x in range(3)].pop()
"#,
    );
}

/// `test_imported_method_call`, `test_method_call_on_reexported_class_instance`,
/// `test_imported_inherited_unsafe_method_call`.
#[test]
fn imported_method_calls() {
    check_all(&[
        (
            "m1",
            r#"
class Pure:
    def m(self):
        pass

class C:
    def m(self):
        raise Exception()

class Derived(C):
    pass
"#,
        ),
        ("m2", "from m1 import Pure, C, Derived\n"),
        (
            "m3",
            r#"
from m2 import Pure, C, Derived
Pure().m()
pure = Pure()
pure.m()
C().m()  # E: unsafe-call
c = C()
c.m()  # E: unsafe-call
Derived().m()  # E: unsafe-call
"#,
        ),
    ]);
}

/// `test_imported_class_alias_preserves_qualified_call`.
#[test]
fn imported_class_alias() {
    check_all(&[
        (
            "m1",
            r#"
class C:
    @classmethod
    def f(cls) -> None:
        input()
"#,
        ),
        (
            "m2",
            r#"
from m1 import C
Alias = C
Alias.f()  # E: unsafe-call
x = C.f
x()  # E: unsafe-call
"#,
        ),
    ]);
}

/// `test_builtins`, `test_dynamic_imports_dunder_import`,
/// `test_dynamic_imports_in_func_are_safe`.
#[test]
fn prohibited_builtins() {
    check(
        r#"
a = input()  # E: prohibited-call
print(a)
c = __import__("sys")  # E: prohibited-call

def test_func():
    c = __import__("sys")
"#,
    );
}

/// `test_eval_*`. Evaluating a computed string is an exec hazard. Upstream parses
/// literal strings; this analysis never reparses source, so a literal is unknown.
#[test]
fn eval() {
    check(
        r#"
a = eval("1 + 2")  # E: unknown-call
x = input()  # E: prohibited-call
eval(x)  # E: exec-call

def f():
    eval("input()")
"#,
    );
}

/// `test_lambda_body_not_analyzed`, `test_unsafe_lambda_call`, `test_lambda_default_analyzed`,
/// `test_lambda_call`, `test_aliased_function_call`. Lambdas and aliases of functions
/// resolve through value flow, so their calls run the body (upstream reports
/// `unknown-function-call` for both).
#[test]
fn lambdas_and_aliases() {
    check(
        r#"
import os
f = lambda: os.getcwd()
f()  # E: unknown-call
g = lambda x=os.system("true"): x  # E: prohibited-call
h = lambda x: x + 1
h(10)
def k(x): pass
alias = k
alias(10)
"#,
    );
}

/// `test_function_from_subscript`.
#[test]
fn function_from_subscript() {
    check(
        r#"
import foo
a = foo.funcs[1]()  # E: unknown-call
"#,
    );
}

/// `test_local_list_append_is_safe`.
#[test]
fn local_list_append() {
    check(
        r#"
x = []
x.append(10)
"#,
    );
}

/// `test_dynamic_imports_importlib`. Upstream treats `import_module` as safe; it is not
/// modeled here, so it is unknown.
#[test]
fn importlib() {
    check(
        r#"
import importlib
a = importlib.import_module("sys")  # E: unknown-call
b = importlib.__import__("math")  # E: prohibited-call
"#,
    );
}

// ---------------------------------------------------------------------------
// tests/sys_modules.rs
// ---------------------------------------------------------------------------

/// `test_reading_a_module_the_interpreter_always_has`, `test_reading_itself_or_an_ancestor`.
#[test]
fn sys_modules_reads_that_cannot_fail() {
    let reports = check_all(&[
        ("pkg", ""),
        (
            "pkg.child",
            r#"
import sys
main_file = sys.modules["__main__"].__file__
path = sys.modules["pkg"].__path__
me = sys.modules["pkg.child"]
"#,
        ),
    ]);
    assert!(reports.iter().all(|r| !r.load_imports_eagerly()));
}

/// `test_reading_another_module_stays_eager`, `test_literal_key_write_stays_eager`,
/// `test_method_call_stays_eager`, `test_del_call_still_triggers_eager`,
/// `test_computed_key_stays_eager`.
#[test]
fn sys_modules_accesses_that_force_eager_imports() {
    let report = check(
        r#"
import sys
import loud
x = sys.modules["loud"]  # E: sys-modules-access
sys.modules["sys"] = loud  # E: sys-modules-access
sys.modules.setdefault("sys", None)  # E: sys-modules-access
del sys.modules["builtins"]  # E: sys-modules-access
name = input()  # E: prohibited-call
y = sys.modules[name]  # E: sys-modules-access
"#,
    );
    assert!(report.load_imports_eagerly());
}

/// `test_membership_test_makes_the_read_a_probe`. Upstream recognizes the membership
/// guard and keeps the module lazy. Guards are not modeled here, so the read stays eager.
#[test]
fn sys_modules_membership_probe_is_not_modeled() {
    check(
        r#"
import sys

def numpy_array(values):
    if "loud" in sys.modules:
        loud = sys.modules["loud"]  # E: sys-modules-access
        return loud.array(values)
    return None
"#,
    );
}

// ---------------------------------------------------------------------------
// tests/nested_functions.rs
// ---------------------------------------------------------------------------

/// `test_nested_function_returned_not_called`, `test_closure_factory`,
/// `test_nested_function_stored_not_called`, `test_deeply_nested_uncalled`,
/// `test_multiple_nested_uncalled`.
#[test]
fn uncalled_nested_functions() {
    check(
        r#"
from foo import obj

def f():
    def g():
        obj.x = 10
    def h():
        def k():
            obj.y = 20
        return k
    alias = g
    return (alias, h)
f()
"#,
    );
}

/// `test_nested_function_called_in_parent`, `test_deeply_nested_called`,
/// `test_nested_one_called_one_not`.
#[test]
fn called_nested_functions() {
    check(
        r#"
from foo import obj

def f():
    def g():
        def h():
            obj.x = 10
        h()
    def unused():
        obj.y = 20
    g()
    return unused
f()  # E: unsafe-call
"#,
    );
}

/// `test_decorator_factory_no_call`, `test_decorator_with_side_effect_in_body`,
/// `test_decorator_calls_nested_function`.
#[test]
fn decorators_run_at_definition() {
    check_all(&[
        (
            "foo",
            r#"
from bar import obj

def returns_wrapper(f):
    def wrapper(*args):
        obj.x = 10
    return wrapper

def mutates(f):
    obj.x = 10
    return f

def calls_nested(f):
    def setup():
        obj.x = 10
    setup()
    return f
"#,
        ),
        (
            "main",
            r#"
from foo import returns_wrapper, mutates, calls_nested

@returns_wrapper
def a():
    pass

@mutates  # E: unsafe-call
def b():
    pass

@calls_nested  # E: unsafe-call
def c():
    pass
"#,
        ),
    ]);
}

/// `test_parameterized_decorator_factory`, `..._imported_var`, `..._transitive_call`.
#[test]
fn parameterized_decorators() {
    check_all(&[
        (
            "registry",
            r#"
from bar import obj
REGISTRY = {}

def register(name):
    def decorator(cls):
        REGISTRY[name] = cls
        return cls
    return decorator

def configure(key):
    def decorator(cls):
        obj[key] = cls
        return cls
    return decorator

def helper():
    obj.x = 10

def indirect(name):
    def decorator(cls):
        helper()
        return cls
    return decorator
"#,
        ),
        (
            "main",
            r#"
from registry import register, configure, indirect

@register("FiLMLayer")  # E: unsafe-call
class A:
    pass

@configure("test")  # E: unsafe-call
class B:
    pass

@indirect("name")  # E: unsafe-call
class C:
    pass
"#,
        ),
    ]);
}

/// `test_class_body_in_function`, `test_nested_class_body_in_function`,
/// `test_class_body_in_nested_uncalled_function`, `test_method_in_class_in_function_not_called`,
/// `test_class_with_side_effect_and_method`.
#[test]
fn class_bodies_inside_functions() {
    check(
        r#"
from foo import obj

def runs_body():
    class A:
        class B:
            obj.x = 10

def defers_method():
    class C:
        def m(self):
            obj.x = 10

def defers_nested():
    def g():
        class D:
            obj.x = 10
    return g

runs_body()  # E: unsafe-call
defers_method()
defers_nested()
"#,
    );
}

/// `test_imported_closure_factory`, `test_imported_function_that_calls_nested`.
#[test]
fn imported_closures() {
    check_all(&[
        (
            "foo",
            r#"
from bar import obj

def make_callback():
    def callback():
        obj.x = 10
    return callback

def do_setup():
    def setup_impl():
        obj.x = 10
    setup_impl()
"#,
        ),
        (
            "main",
            r#"
from foo import make_callback, do_setup
cb = make_callback()
do_setup()  # E: unsafe-call
"#,
        ),
    ]);
}

/// `test_param_mutation_in_called_nested`, `test_param_mutation_in_uncalled_nested`.
/// Upstream also flags the uncalled closure because it could mutate the argument later;
/// that happens only if something calls it, so it is not an import-time effect here.
#[test]
fn param_mutation_in_nested_functions() {
    check(
        r#"
from foo import obj

def called(x):
    def inner():
        x.enabled = True
    inner()

def uncalled(x):
    def inner():
        x.enabled = True
    return inner

called(obj)  # E: imported-argument
uncalled(obj)
"#,
    );
}

/// `test_parameterized_decorator_pure_wraps_wrapper`, `..._pure_wraps_from_import`,
/// `..._returned_wrapper_side_effect_is_safe`, `..._wraps_plus_mutation_still_unsafe`.
#[test]
fn functools_wraps_decorators() {
    check_all(&[
        (
            "lib",
            r#"
import functools
from functools import wraps

REGISTRY = {}

def trace(name):
    def decorator(func):
        @functools.wraps(func)
        def wrapper(*args, **kwargs):
            return func(*args, **kwargs)
        return wrapper
    return decorator

def validate(name):
    def decorator(func):
        @wraps(func)
        def wrapper(*args, **kwargs):
            raise ValueError("only at call time")
        return wrapper
    return decorator

def register(name):
    def decorator(func):
        REGISTRY[name] = func
        @functools.wraps(func)
        def wrapper(*args, **kwargs):
            return func(*args, **kwargs)
        return wrapper
    return decorator
"#,
        ),
        (
            "main",
            r#"
from lib import trace, validate, register

@trace("op")
def a():
    pass

@validate("op")
def b():
    pass

@register("op")  # E: unsafe-call
def c():
    pass
"#,
        ),
    ]);
}

// ---------------------------------------------------------------------------
// tests/decorators.rs
// ---------------------------------------------------------------------------

/// `test_local_decorator`, `test_imported_function_decorator`, `test_imported_class_decorator`,
/// `test_imported_method_decorator`, `test_unknown_decorator`.
#[test]
fn local_and_unresolved_decorators() {
    check(
        r#"
from foo import dec

def local(f):
    return f

@local
def a(x):
    ...

@dec  # E: unknown-call
def b(x):
    ...

@dec  # E: unknown-call
class B:
    @dec  # E: unknown-call
    def m(self):
        ...

@undefined  # E: unknown-call
def c():
    ...
"#,
    );
}

/// `test_safe_imported_decorator`, `test_unsafe_imported_decorator`.
#[test]
fn imported_decorators() {
    check_all(&[
        (
            "foo",
            r#"
def dec(f):
    raise()

def safe(f):
    return f
"#,
        ),
        (
            "main",
            r#"
from foo import dec, safe
import foo

class A:
    @dec  # E: unsafe-call
    def f(x):
        ...
    @safe
    def g(x):
        ...

@dec(args)  # E: unsafe-call
def f(x):
    ...

@foo.dec  # E: unsafe-call
def g(x):
    ...
"#,
        ),
    ]);
}

/// `test_decorator_arguments_on_definitions`.
#[test]
fn decorator_arguments() {
    check(
        r#"
def safe_factory(*args, **kwargs):
    return lambda f: f

def unsafe():
    raise()

@safe_factory(unsafe())  # E: unsafe-call
def function():
    ...

@safe_factory(value=unsafe())  # E: unsafe-call
class Class:
    ...

@safe_factory(*[unsafe()])  # E: unsafe-call
async def async_function():
    ...

class Container:
    @safe_factory(**{"value": unsafe()})  # E: unsafe-call
    def method(self):
        ...
"#,
    );
}

/// `test_property`, `test_property_setter_is_safe`, `test_property_deleter_is_safe`.
#[test]
fn properties() {
    check(
        r#"
class Foo:
    @property
    def bar(self):
        return self._bar

    @bar.setter
    def bar(self, value):
        self._bar = value

    @bar.deleter
    def bar(self):
        del self._bar
"#,
    );
}

/// `test_subscript_decorator`. The decorated name is then unknown, so calling it is too.
#[test]
fn subscript_decorator() {
    check(
        r#"
decorators = [
    lambda fn: fn,
]

@decorators[0]  # E: unknown-call
def foo(value):
    print(value)

foo(37)  # E: unknown-call
"#,
    );
}

// ---------------------------------------------------------------------------
// tests/param_mutation.rs
// ---------------------------------------------------------------------------

/// `test_param_subscript_mutation_*`, `test_param_attr_mutation_*`,
/// `test_multiple_param_attr_mutations`, `test_param_del_*`.
#[test]
fn param_mutations() {
    check(
        r#"
from foo import d, obj

def set_item(x):
    x["key"] = "value"

def set_attr(x):
    x.enabled = True
    x.verbose = False

def del_item(x):
    del x["key"]

def del_attr(x):
    del x.enabled

set_item(d)  # E: imported-argument
set_item({})
set_attr(obj)  # E: imported-argument
set_attr(10)
del_item(d)  # E: imported-argument
del_attr(obj)  # E: imported-argument
"#,
    );
}

/// `test_param_method_call_with_imported_arg`, `test_param_method_call_unsafe_without_imported_arg`.
/// Unresolved methods on parameters are unknown (upstream: `unsafe-function-call`) and
/// may mutate the argument.
#[test]
fn unresolved_param_method() {
    check(
        r#"
from foo import A

def f(x):
    x.bar()

f(A)  # E: imported-argument  # E: unknown-call
f(10)  # E: unknown-call
"#,
    );
}

/// `test_imported_function_mutates_param`, `test_registry_pattern_global_mutation`.
#[test]
fn cross_module_param_and_global_mutation() {
    check_all(&[
        (
            "setup",
            r#"
registry = []

def configure(x):
    x.enabled = True

def register(item):
    registry.append(item)
"#,
        ),
        (
            "main",
            r#"
from setup import configure, register
from config import settings

configure(settings)  # E: imported-argument
register("my_item")  # E: unsafe-call
"#,
        ),
    ]);
}

/// `test_precise_match_*`, `test_keyword_arg_precise_matching*`,
/// `test_mixed_positional_and_keyword_only_keyword_mutated`.
#[test]
fn precise_argument_matching() {
    check(
        r#"
from foo import A

def second(a, b):
    b.x = 1

def kwonly(a, *, target):
    target["k"] = a

second(A, 1)
second(1, A)  # E: imported-argument
second(1, b=A)  # E: imported-argument
kwonly(A, target={})
kwonly(1, target=A)  # E: imported-argument
"#,
    );
}

/// `test_safe_function_no_mutations_with_imported_arg`, `test_safe_subscript_read_with_imported_arg`,
/// `test_safe_builtin_calls_with_imported_arg`, `test_mutation_in_function_scope_only`,
/// `test_star_args_with_imported_var`, `test_kwargs_with_imported_var`.
#[test]
fn safe_uses_of_imported_arguments() {
    check(
        r#"
from foo import A

def reads(x):
    return x[0]

def ignores(*args, **kwargs):
    pass

def modify(x):
    x.append(1)

def caller():
    modify(A)

reads(A)
ignores(A, x=A)
x = len(A)
y = str(A)
z = list(A)
"#,
    );
}

/// `test_imported_var_alias_passed_to_mutating_function`, `test_module_attr_passed_to_mutating_function`,
/// `test_mutating_builtin_method_on_param_is_unsafe`.
#[test]
fn imported_values_passed_to_mutators() {
    check(
        r#"
import foo
from foo import bar

baz = bar

def modify(x):
    x.append(1)

modify(baz)  # E: imported-argument
modify(foo.bar)  # E: imported-argument
"#,
    );
}

/// `test_non_mutating_method_on_param_is_safe`, `test_dict_get_on_param_is_safe`,
/// `test_list_index_on_param_is_safe`, `test_param_copied_then_mutated_is_not_an_imported_var_argument`.
/// The copy is a fresh local object, so mutating it is safe (upstream still reports
/// `unsafe-function-call` for the unresolved `y.append`).
#[test]
fn non_mutating_methods_on_params() {
    check(
        r#"
from foo import A

def f(x):
    y = x.copy()
    y.append(1)
    return x.get("key"), x.index(42)

f(A)
"#,
    );
}

/// `test_transitive_param_mutation`, `test_three_level_transitive_param_forwarding`,
/// `test_diamond_param_forwarding`.
#[test]
fn forwarded_param_mutation() {
    check(
        r#"
from foo import A

def h(z):
    z.append(1)

def g(y):
    h(y)

def i(y):
    h(y)

def f(x):
    g(x)
    i(x)

f(A)  # E: imported-argument
"#,
    );
}

/// `test_cycle_param_forwarding`: the cycle converges with the mutation propagated,
/// and its termination is reported as unknown.
#[test]
fn cyclic_param_forwarding() {
    check(
        r#"
from foo import A

def g(y):
    y.append(1)
    f(y)

def f(x):
    g(x)

f(A)  # E: imported-argument  # E: unknown-call
"#,
    );
}

/// `test_reassigned_param_not_forwarded`, `test_param_reassigned_then_aliased_is_safe`:
/// documented upstream false positives. Reaching definitions after the rebinding see
/// only the fresh list, so neither is reported here.
#[test]
fn rebound_params_are_not_forwarded() {
    check(
        r#"
from foo import A

def g(y):
    y.append(1)

def f(x):
    x = []
    g(x)

def k(x):
    x = []
    y = x
    y.append(1)

f(A)
k(A)
"#,
    );
}

/// `test_param_aliased_then_mutated_is_unsafe`, `test_param_aliased_two_hops_then_mutated_is_unsafe`,
/// `test_param_alias_then_reassigned_is_safe`, `test_closure_capture_param_alias_mutation`.
#[test]
fn param_aliases() {
    check(
        r#"
from foo import A

def one_hop(x):
    y = x
    y.append(1)

def two_hops(x):
    y = x
    z = y
    z.append(1)

def rebound(x):
    y = x
    y = {}
    y["k"] = 1

def closure(x):
    def g():
        y = x
        y.append(1)
    g()

one_hop(A)  # E: imported-argument
two_hops(A)  # E: imported-argument
rebound(A)
closure(A)  # E: imported-argument
"#,
    );
}

/// `test_aliased_param_forwarded_to_mutating_call`, `test_aliased_param_forwarded_two_hops`.
#[test]
fn aliased_param_forwarded_across_modules() {
    check_all(&[
        (
            "a",
            r#"
def g(item):
    item["k"] = 1
"#,
        ),
        (
            "main",
            r#"
from a import g
from foo import A

def f(x):
    y = x
    z = y
    g(z)

f(A)  # E: imported-argument
"#,
        ),
    ]);
}

/// `test_tuple_unpack_alias_not_tracked`: the unpacked value is unknown, so the method
/// call on it is unknown.
#[test]
fn tuple_unpack_alias() {
    check(
        r#"
from foo import A

def f(x):
    y, _ = x, 1
    y.append(1)

f(A)  # E: unknown-call
"#,
    );
}

/// `test_nested_function_outer_param_is_not_inner_forwarded_param`. The inner call
/// forwards the outer parameter, which is bound to a fresh list (upstream additionally
/// reports the unresolved `v.append`).
#[test]
fn outer_param_is_not_inner_param() {
    check(
        r#"
from foo import A

def mutate(v):
    v.append(1)

def outer(x):
    def inner(**kwargs):
        mutate(x)
    inner(**A)

outer([])
"#,
    );
}

// ---------------------------------------------------------------------------
// tests/blocks.rs
// ---------------------------------------------------------------------------

/// `test_parameter_default_effects`, `test_class_base_effects`: defaults and bases run
/// where the function or class is defined.
#[test]
fn defaults_and_bases_run_at_definition() {
    check(
        r#"
from foo import f, g
def h(a, b=f(), *args, c=f(), **kwargs):  # E: unknown-call
    pass
class C(f(), metaclass=g()):  # E: unknown-call
    pass
"#,
    );
}

/// `test_for_target`.
#[test]
fn for_target() {
    check(
        r#"
import foo
for foo.x in [1, 2, 3]:  # E: imported-mutation
    ...
"#,
    );
}
