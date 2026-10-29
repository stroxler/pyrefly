/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Hand-written effect summaries for builtins, builtin methods and a few standard
//! library functions. Anything not listed here is unresolved, and the analysis reports
//! it as unknown rather than assuming it is safe.

/// What calling a known builtin or external function does at import time.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Known {
    /// No effect; returns an immutable value.
    Const,
    /// No effect; returns a new mutable object owned by the caller.
    Fresh,
    /// Returns an element of its positional arguments (`min`, `max`).
    Element,
    /// Returns the argument at this position or with this keyword (`typing.cast`).
    Returns(usize, &'static str),
    /// A decorator that returns the function or class it decorates (`functools.cache`).
    Identity,
    /// A decorator that turns a method into a property (`functools.cached_property`).
    Property,
    /// Returns a decorator that returns what it decorates (`functools.wraps(f)`).
    IdentityFactory,
    /// Has an effect that lazy imports can reorder or skip (I/O, process state).
    Prohibited,
}

/// Builtins by name. `exec`, `eval`, `getattr`, `setattr`, `delattr`, `next`,
/// `property`, `classmethod` and `staticmethod` are modeled separately by the domain.
pub fn builtin(name: &str) -> Option<Known> {
    use Known::*;
    Some(match name {
        "abs" | "all" | "any" | "ascii" | "bin" | "bool" | "callable" | "chr" | "complex"
        | "divmod" | "float" | "format" | "hasattr" | "hash" | "hex" | "id" | "int"
        | "isinstance" | "issubclass" | "len" | "oct" | "ord" | "pow" | "print" | "repr"
        | "round" | "str" | "sum" | "bytes" | "frozenset" | "tuple" | "range" | "slice"
        | "compile" => Const,
        "bytearray" | "dict" | "dir" | "enumerate" | "filter" | "iter" | "list" | "map"
        | "memoryview" | "object" | "reversed" | "set" | "sorted" | "zip" => Fresh,
        "max" | "min" => Element,
        "input" | "open" | "breakpoint" | "exit" | "quit" | "help" | "__import__" => Prohibited,
        _ if is_exception_class(name) => Fresh,
        _ => return None,
    })
}

/// How a builtin treats generator arguments.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Iteration {
    /// Iterates its arguments now.
    Consumes,
    /// Returns a lazy iterator over its arguments.
    Wraps,
    /// Does not iterate its arguments.
    Ignores,
}

pub fn builtin_iteration(name: &str) -> Iteration {
    match name {
        "list" | "tuple" | "set" | "frozenset" | "dict" | "sorted" | "sum" | "min" | "max"
        | "any" | "all" | "bytes" | "bytearray" | "next" => Iteration::Consumes,
        "iter" | "enumerate" | "zip" | "map" | "filter" | "reversed" => Iteration::Wraps,
        _ => Iteration::Ignores,
    }
}

/// Builtins that call a callback argument: `(position, keyword, eager)`. Eager callbacks
/// run during the call; lazy ones run when the result is iterated.
pub fn builtin_callback(name: &str) -> Option<(usize, &'static str, bool)> {
    match name {
        "sorted" | "min" | "max" => Some((usize::MAX, "key", true)),
        "map" | "filter" => Some((0, "function", false)),
        "iter" => Some((usize::MAX, "callable", false)),
        _ => None,
    }
}

/// Methods of builtin containers that store their arguments rather than iterate them.
pub fn is_storing_method(name: &str) -> bool {
    matches!(
        name,
        "append" | "insert" | "add" | "setdefault" | "__setitem__"
    )
}

/// Builtin classes that are safe to subclass: creating the subclass runs no user code.
pub fn is_safe_builtin_base(name: &str) -> bool {
    matches!(
        name,
        "object" | "int" | "str" | "bytes" | "float" | "dict" | "list" | "set" | "tuple"
    ) || is_exception_class(name)
}

fn is_exception_class(name: &str) -> bool {
    name.ends_with("Error")
        || name.ends_with("Exception")
        || name.ends_with("Warning")
        || matches!(
            name,
            "StopIteration" | "StopAsyncIteration" | "KeyboardInterrupt" | "SystemExit"
        )
}

/// Functions of modules outside the project, by dotted path.
pub fn external(path: &str) -> Option<Known> {
    use Known::*;
    Some(match path {
        "os.path.join" | "os.path.dirname" | "os.path.basename" | "os.path.abspath"
        | "os.path.splitext" | "os.path.normpath" | "os.fspath" | "re.escape"
        | "typing.TypeVar" | "typing.NewType" | "typing.ParamSpec" => Const,
        "re.compile"
        | "collections.namedtuple"
        | "collections.defaultdict"
        | "collections.OrderedDict"
        | "collections.Counter"
        | "collections.deque"
        | "functools.partial"
        | "dataclasses.field"
        | "threading.Lock"
        | "threading.RLock"
        | "enum.auto" => Fresh,
        "functools.cached_property" => Property,
        "functools.cache"
        | "functools.lru_cache"
        | "functools.total_ordering"
        | "typing.final"
        | "typing.overload"
        | "typing.runtime_checkable"
        | "abc.abstractmethod"
        | "dataclasses.dataclass"
        | "contextlib.contextmanager" => Identity,
        "typing.cast" => Returns(1, "val"),
        "functools.wraps" => IdentityFactory,
        "os.system"
        | "os.chdir"
        | "os.putenv"
        | "subprocess.run"
        | "subprocess.call"
        | "subprocess.check_call"
        | "subprocess.check_output"
        | "subprocess.Popen"
        | "sys.exit"
        | "sys.setrecursionlimit"
        | "signal.signal"
        | "atexit.register"
        | "warnings.filterwarnings"
        | "warnings.simplefilter"
        | "logging.basicConfig"
        | "random.seed"
        | "importlib.__import__" => Prohibited,
        _ => return None,
    })
}

/// External classes that are safe to subclass.
pub fn is_safe_external_base(path: &str) -> bool {
    matches!(
        path,
        "abc.ABC" | "typing.Generic" | "typing.Protocol" | "typing.NamedTuple" | "typing.TypedDict"
    )
}

/// Methods that mutate their receiver when it is a builtin container.
pub fn is_mutating_method(name: &str) -> bool {
    matches!(
        name,
        "append"
            | "extend"
            | "insert"
            | "pop"
            | "popitem"
            | "remove"
            | "clear"
            | "update"
            | "add"
            | "discard"
            | "setdefault"
            | "sort"
            | "reverse"
            | "__setitem__"
            | "__delitem__"
            | "__setattr__"
            | "__delattr__"
    )
}

/// Methods that builtin types define without side effects.
pub fn is_pure_method(name: &str) -> bool {
    matches!(
        name,
        "copy"
            | "get"
            | "index"
            | "count"
            | "keys"
            | "values"
            | "items"
            | "join"
            | "split"
            | "rsplit"
            | "strip"
            | "lstrip"
            | "rstrip"
            | "lower"
            | "upper"
            | "replace"
            | "startswith"
            | "endswith"
            | "format"
            | "encode"
            | "decode"
            | "union"
            | "intersection"
            | "difference"
            | "issubset"
            | "issuperset"
            | "find"
    )
}
