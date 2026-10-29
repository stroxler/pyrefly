# Lazy-import safety consumer (spike)

`pyrefly_static_analysis_lifeguard` is an experimental reimplementation of a subset of
[Lifeguard](https://github.com/facebook/Lifeguard)'s lazy-import safety analysis. It is a
consumer of the neutral `pyrefly_static_analysis` frontend and engine, written to test
whether that shared core can carry an effect analysis as well as the SPMD type
consumer. It is not a replacement for Lifeguard and does not reproduce Lifeguard's
output format, policy layer (`lazy_eligible`, import chains), stubs, or incremental cache.

## Running it

```sh
cargo run -p pyrefly_static_analysis_lifeguard --bin lifeguard-spike -- NAME=PATH [NAME=PATH ...]
```

Every module is named explicitly, and nothing is discovered. A path ending in
`__init__.py` marks a package `__init__`. Imports of modules that are not listed are
treated as external objects. The output has one line per diagnostic
(`module:line:column: kind: message`) followed by one verdict line per module:
`safe`, `unsafe` or `unknown`, plus `load-imports-eagerly` when the module's own imports
must run eagerly. The exit status is 0 when every module is safe, 1 when any module is
unsafe, 2 when none is unsafe but some module could not be fully analyzed, and 64 for
usage or I/O errors. An unknown result never exits as verified.

The `examples/` directory holds a small project:

```sh
E=static_analysis/lifeguard/examples
cargo run -p pyrefly_static_analysis_lifeguard --bin lifeguard-spike -- \
  registry=$E/registry.py plugin=$E/plugin.py helpers=$E/helpers.py settings=$E/settings.py
```

```text
plugin:9:1: unsafe-call: registry:10: calls `append` on an object of module `registry`
settings:11:10: unknown-call: settings:11: calls unresolved `json.loads`
settings:15:5: sys-modules-access: calls `sys.modules.pop`
helpers: safe
plugin: unsafe
registry: safe
settings: unsafe load-imports-eagerly
```

`registry` mutates its own list while it is being imported, which is safe. `plugin`
applies a decorator that mutates `registry`'s state, which is an import-time effect on
another module. `settings` calls a library function the analysis does not model, which
is unknown rather than safe. It also touches `sys.modules` in a function that import
time never calls, and that still forces its imports to run eagerly.

## How it uses the shared core

The consumer never parses or walks syntax. Each module is lowered once by
`analyze_python`, and everything else is computed from the shared IR:

- **Scopes and ownership.** Name operations carry the owning symbol after the core's
  Python scope rules (`global`, `nonlocal`, class-scope visibility, and the class-body
  fallback to globals and builtins).
- **Evaluation order and regions.** Operations arrive in runtime order, with the
  right-hand side evaluated before targets. Decorators and defaults run in the defining
  region, and function, lambda, generator and coroutine bodies are separate deferred
  regions whose trigger says when they run.
- **Engine and frames.** `domain.rs` implements `Domain` with a set-of-origins value
  lattice. The engine owns frames, joins at branches, loop fixpoints, reachability, the
  replay of reachable operations for diagnostics, and the bindings (presence and
  value) of symbols owned by other regions, including `global` and `nonlocal` writes
  and deletions visible in the exit frame. Loads use the engine's default, including
  the fallback from class scope to globals and from globals to builtins; the consumer
  only seeds builtin cells with builtin values.
- **Function summaries.** A call to a project function runs the same engine over the
  callee's region with symbolic parameters and the caller's cells as `Entry::outer`.
  The cells are the callee's `Program::free_symbols` plus the globals that any function
  of its module reads (which further calls may observe). Summaries are memoized by the
  function, the importing module, and those cell bindings at invocation, so closures and
  globals are read when the call happens, not when the function is defined. The
  interprocedural driver in `analyzer.rs` solves recursive cycles by iterating from an
  empty summary at the head of each strongly connected component. A summary that
  depends on a call cut off by the depth limit is not cached.
- **Exports.** `from m import n` goes through the generic `ExportProvider`. The CLI and
  tests use `SyntacticExports`, and re-exports between analyzed modules are followed
  through each module's own exit values.

The effect vocabulary (origins, ownership, mutation kinds, builtin summaries) and the
summary policy are consumer-owned. Porting this consumer led to three core utilities:
raise sites with a source range and cause, `Program::free_symbols` for the closure
cells a region needs, and `Program::module_symbols`.

## Semantics

A value is a set of origins: a constant, an object created while importing a given
module, an object reached through an external import (named by its dotted path), a
project module, a builtin, a project function, generator, property, class or instance,
a member (attribute or item) of another origin, a parameter of the current function, or
unknown.

Whether an operation is an effect depends on who owns the object, not on the syntax of
the target:

- Rebinding a name is a local write. `from foo import bar; bar = 1` is safe.
- Storing into or deleting an attribute or item of an object owned by another module,
  or calling a known mutating method on it, is an `imported-mutation`. This includes
  aliases (`x = mod.A; x[0] = 1`) and rebinding another module's global from one of
  its functions.
- Mutating an object owned by the module being imported is safe, even inside a
  function. The same function becomes unsafe when another module's import calls it.
- A function mutating a parameter records that in its summary. The caller reports
  `imported-argument` when it passes an imported object, or forwards the mutation to
  its own parameter. Values returned from a call, including classes, instances, bound
  methods and generators that capture parameters, have the arguments substituted.
- There is no heap: the contents of attributes and items are not tracked. Mutating an
  object itself is judged by its owner, but mutating an object reached through an
  attribute or item (`self.items.append(x)`, `lst[0].append(x)`) is `unknown-mutation`
  unless the chain starts at a parameter or an imported object.
- Methods bind as descriptors: functions bind the instance, `staticmethod` binds
  nothing, and `classmethod` binds the class. Reading a property runs its getter, and
  writing or deleting it runs its setter or deleter. A class's `__setattr__` and
  `__delattr__` run on instance attribute writes. Class values are snapshots of the
  class body, so writing an attribute on a project class, shadowing a method on an
  instance, or writing globals through the module object is `unknown-mutation`.
- A generator or coroutine keeps the arguments it was created with. Its body runs when
  it is iterated, awaited, delegated to with `yield from`, or passed to a builtin that
  iterates its arguments (`list`, `next`, `sorted`, ...) or to a project function that
  iterates the parameter. Lazy builtins (`map`, `zip`, `enumerate`, ...) carry it to
  whatever iterates their result. Storing one in a collection or attribute, yielding it,
  or passing it to a library function is `unknown-call`.
- Builtins run their eager callbacks (`key=` of `sorted`, `min`, `max` and
  `list.sort`). Any callback passed to a lazy builtin (`map`, `filter`,
  `iter(callable, sentinel)`) is `unknown-call`, because its calls happen during a
  later iteration that the analysis does not connect to it. A builtin used as an eager
  callback is `unknown-call` too, since it may run special methods of the untracked
  items (`str` runs `__str__`), unless it has effects of its own to report (`open`) or
  calls none (`id`).
- A class's namespace holds the class body's own bindings. A name the body deleted is
  absent, and a name it may not have bound is partly unknown.
- Special methods that a project class defines run where Python would call them:
  operators, comparisons, iteration, subscripts, truth tests, `__getattr__` and
  `__getattribute__`, context managers (including `async with`), and the conversion
  methods builtins call on their arguments.
- Cells carry presence as well as values, so deleting a global, in the module or in a
  called function, exposes the builtin of the same name to later reads. A module's
  globals, as importers see them, join its normal exit with its raising exits.
- Function bodies count only when import-time code calls them (including through
  decorators, properties, constructors, iterated generators and awaited coroutines).
  Defaults, decorators, class bases and class bodies count where they are defined.
  `exec`, `sys.modules` access, `builtins.__import__` overrides and `__del__` force
  eager imports wherever they occur.
- An unresolved callee, an unresolved method, a mutation of an unattributable value, an
  escaped closure's cells, a reachable frontend gap, non-convergence, and recursion are
  reported as `unknown-call`, `unknown-mutation` or `unsupported`. A module with only
  such diagnostics has the verdict `unknown`, never `safe`.

### Diagnostic kinds

| Kind | Meaning | Verdict |
| --- | --- | --- |
| `imported-mutation` | import-time mutation of another module's object | unsafe |
| `imported-argument` | imported object passed to a parameter the callee mutates | unsafe |
| `unsafe-call` | the callee's body has an import-time effect (the message names its root cause) | unsafe |
| `prohibited-call` | `input`, `open`, `__import__`, `os.system`, computed `getattr`/`setattr`, ... | unsafe |
| `unhandled-raise` | import-time code reaches a `raise` | unsafe |
| `custom-finalizer`, `exec-call`, `sys-modules-access`, `builtins-import-override` | also set `load-imports-eagerly` | unsafe |
| `unknown-call`, `unknown-mutation`, `unsupported` | something could not be analyzed | unknown |

### Assumptions shared with upstream

These are heuristics rather than proofs, and Lifeguard makes the same assumptions:
attribute and item reads on external objects, operators, comparisons and truthiness
tests do not run code with effects unless a project class defines the special method.

## Parity with the public test suite

The fixtures in `tests/parity.rs` are adapted from facebook/Lifeguard at commit
`c04fde1358c467d7ec84244ddfb066186381f066`. That commit is the only commit in the
reference clone. Each test names the upstream file and the tests it adapts; several
upstream tests are often combined into one fixture. Together they adapt cases from
roughly 140 upstream test functions in eight of the 35 upstream test files. That is a
meaningful subset, not a full port.

| Upstream file | Covered | Not covered |
| --- | --- | --- |
| `assign.rs` | module, attribute, subscript, annotated, augmented and destructuring stores; aliases; rebinding as safe; `builtins.__import__` overrides | thread-local stores |
| `basic.rs` | literal and computed `getattr`/`setattr`; own-module state; imported subscripts; raising callees; `__del__` | |
| `calls.rs` | cross-module calls, package re-exports, re-exported decorators, recursion, methods and constructors, literal methods, class aliases, prohibited builtins, `eval`, lambdas, `importlib` | `super()` dispatch, annotated return types from stubs, MRO edge cases, `import_module` targets |
| `sys_modules.rs` | reads that cannot fail, writes, deletes, method calls, computed keys | membership and `KeyError` probes |
| `nested_functions.rs` | called and uncalled nested functions, closure factories, decorator factories, class bodies inside functions, `functools.wraps` | |
| `decorators.rs` | local, imported, unresolved, parameterized and subscript decorators; decorator arguments; properties | decorator argument overflow, safelisted decorators |
| `param_mutation.rs` | attribute, item and method mutation of parameters; precise positional and keyword matching; transitive, diamond and cyclic forwarding; aliases; closures; copies | unbound methods called through the class, `*args` forwarding edge cases, the 64-argument limit |
| `blocks.rs` | defaults, class bases, `for` targets | per-statement expression coverage table |

Not attempted: stubs and stub effects, implicit imports, star-import semantics, import
cycles as a policy, `__main__` handling, the subclass-registration rule, output policy,
the cache and map/reduce path, and the remaining upstream test files.

### Intentional differences from upstream

- **Unknown is never safe.** Upstream treats some unresolved calls as safe and reports
  others as unsafe. Here they are always `unknown-call`, so unresolved externals,
  unresolved methods on parameters, recursion, and classes with unresolved bases (whose
  metaclass or `__init_subclass__` could run code) make a module `unknown`.
- **Flow-sensitive rebinding.** Reaching values come from the engine, so two documented
  upstream false positives are not reported: `test_reassigned_param_not_forwarded` and
  `test_param_reassigned_then_aliased_is_safe`. String constants also flow, so
  `x = "foo"; getattr(f, x)` is a literal access.
- **Value-flow resolution.** Calls through aliases and lambdas run the callee instead of
  being unknown, and hazards are found through aliases (`e = exec; e(...)`,
  `m = sys.modules; m[k] = v`).
- **Import-time semantics for closures.** A returned closure that would mutate its
  argument is not an effect until something calls it, and calling it is unknown because
  its cells escaped. Upstream reports `imported-var-argument` at the factory call.
- **In-place operators.** `bar += 1` on an imported name may call `__iadd__`, so it is
  an `unknown-mutation` rather than safe.
- **No reparsing.** `eval` of a string literal is unknown because the literal is not
  parsed.
- **Locations.** `custom-finalizer` is reported at the class statement.

## Known limitations

- `try` statements, `match`, star imports and other frontend gaps make the enclosing
  module `unknown`. Only explicit `raise` statements are reported as raising. A failing
  `assert` is treated like any other operation that may raise, and the re-raise path
  out of a `try` is covered by that `try`'s gap. Handler matching (upstream's caught
  `raise`) is not modeled.
- Function objects do not carry their default values, so a parameter left to its
  default is unknown.
- Class and instance values are built from class bodies in project modules. There is no
  MRO linearization, `super()` support, or metaclass modeling, and no heap for
  attribute and container contents, which makes many classes that mutate their own
  attributes `unknown` rather than `safe`.
- Builtin, method and library summaries are a short hand-written table in `known.rs`.
- Tests ran on Linux only. The analysis has no platform-specific code, and the CLI test
  uses portable temporary paths, but macOS and Windows were not validated.

## Tests

`cargo test -p pyrefly_static_analysis_lifeguard` runs:

- `tests/parity.rs`: 67 fixtures adapted from the upstream suite.
- `tests/regressions.rs`: 16 tests for cases that earlier versions reported as safe,
  panicked, or used too much memory. They cover:
  - parameters captured by returned classes;
  - `staticmethod` and `classmethod` binding, property setters and deleters,
    `__setattr__`, and dynamic class writes;
  - `typing.cast` and `max` provenance;
  - generator arguments, `next`, generators passed to functions, and generator
    expressions;
  - builtin callbacks;
  - special methods run by operators, iteration, subscripts, truth tests and builtins;
  - `functools.cached_property`, dataclass default factories, and `async with`;
  - deleted class attributes, and globals deleted so that a builtin becomes visible;
  - module globals on raising paths;
  - a module with a thousand decorated functions.
- `tests/semantics.rs`: 15 tests. They cover local rebinding versus external mutation,
  `global` writes flowing back from calls, invocation-time closure cells, escaped
  closures, generator and coroutine bodies, property getters, constructor arguments,
  hazards through aliases, unresolved bases, frontend gaps (including `try` re-raise
  and `assert` paths), unreachable code and caught raises, class-body name fallback,
  import cycles, and the CLI's output and exit status.

Expected diagnostics are written as `# E: <kind>` comments on the line they apply to.
