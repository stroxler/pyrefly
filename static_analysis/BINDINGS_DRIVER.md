# Pyrefly bindings on the shared structured driver

This branch makes Pyrefly's real binding pass a third consumer of the shared static-analysis
core, alongside the SPMD and Lifeguard domains. It is an **AI-assisted experiment**, opt-in
behind a Cargo feature, and it covers **statement-level control structure, declaration
collection and lexical scope navigation**. It is not a migration of the binding pass and does
not establish full replacement capability.

## What is shared

`static_analysis/src/structured.rs` is a structured statement driver over the parsed Ruff AST.
It is generic over how a backend holds the AST (`Owned`, which moves expressions into the
backend's output without cloning, or `Borrowed`) and over a `Structured` backend trait. The
driver owns:

- the iteration of every suite, including a backend hook to absorb the following statement;
- the decomposition of `if`/`elif`/`else`, `while` and `for` (with `else`);
- applying a backend's static truth for each `if` test: a statically false arm is skipped, a
  statically true arm (or an `else`) ends the chain, makes the statement exhaustive and leaves
  the remaining arms dead;
- the loop that each `break` and `continue` leaves, with a fresh loop stack for each function
  body. Which other suites are loop barriers is the backend's choice: Pyrefly drives class
  bodies as regions, matching its per-scope loops, while lowering keeps class bodies inline
  as before (a jump there is a syntax error either way);
- coverage counters.

Two backends use it:

| Backend | Holds AST | Backend values | Static truth |
| --- | --- | --- | --- |
| Core lowering (`lower.rs`), feeding SPMD, Lifeguard and `History` | `Borrowed` | blocks, terminators, registers | never decides a test; every arm is lowered |
| Pyrefly bindings (`pyrefly/lib/binding/driver.rs`, feature `bindings-driver`) | `Owned` | `Scopes` flow, phis, narrowing, `BranchTest`/`BranchEnd`, loop phis | `SysInfo`, `TYPE_CHECKING`, environment independence |

Lowering's CFGs are unchanged: the existing spike suite passes without modification. Lowering
no longer keeps its own loop stack.

## Ownership map

| Concern | Owner |
| --- | --- |
| Suite order, `if`/loop structure, arm pruning given static truth, exhaustiveness, loop attribution and region loop reset (Pyrefly: cross-checked; jump exit flows remain in `Scopes`) | shared driver |
| Whether a test is statically known | backend policy |
| Flow environments, merges, phis, narrowing, `BranchTest`/`BranchEnd`, termination and reachability, suite gates | Pyrefly (`binding/structure.rs`, `binding/scope.rs`) |
| Leaf statement bindings (assignments, annotations, imports, returns, expressions), contextual typing, first-use inference, deferred `BoundName` finalization | Pyrefly, unchanged |
| Expression binding, including expression-level control flow (`and`/`or`, conditional expressions, comprehensions, lambdas) | Pyrefly `ensure_expr`, not extracted |
| Which syntax declares what in which scope, walrus hoisting, header evaluation scope, `if` and conditional-expression pruning given static truth | shared declaration walker (`declarations.rs`), with the feature |
| Scope visibility for name reads (class bodies, annotation and `type` alias scopes), barrier accumulation, `nonlocal` resolution; for the runtime also `global` redirection | shared lexical engine (`lexical.rs`), with the feature |
| Definition styles, annotations, `Final`, `__all__`, deprecation, special exports and imports, environment truth, Pyrefly's declined expression positions | Pyrefly `Definitions` sink |
| What a found name means: flow versus static entries, initialization, keys, type parameter rules, class-body overload skipping, the barrier values themselves; `global` captures (a module lookup) | Pyrefly `look_up_name_for_read`, the other `visit_scopes` callers and `binding/scope.rs` |
| Other scope-stack walks: `outer_capture_info`, `has_nonlocal_binding` (for the semantic syntax checker), `get_flow_info`, `in_sync_comprehension`, the implicit-builtin shadowing check, `name_shadows_enclosing_annotation_scope`, and the binder's placement of a comprehension walrus target in its static and flow scopes | Pyrefly, not routed through the engine |
| Other syntax walks over statements: `function_last_expressions` (implicit returns and suite gates), `Ast::body_contains_yield`, `report_unreachable_body`, `SelfAttrNames`, `SuperMethodCallFinder` | Pyrefly, not extracted; they still traverse the AST independently of the driver |
| Structure of `with`, `try`, `match`, function and class headers | Pyrefly; the driver sequences their nested suites ("mixed") |

## Declarations and lexical scopes

The core's runtime `ScopeTable` and Pyrefly's `Definitions` are two sinks of one per-scope
walker, `declarations::walk_suite`. The walker reports, in source order, what a scope binds
or declares, which names it evaluates and where it creates nested scopes; neither sink walks
statements or expressions to find declarations. The policies differ explicitly:

| | Runtime `ScopeTable` | Pyrefly `Definitions` |
| --- | --- | --- |
| Static truth | none: a name bound only in dead code is still local | `SysInfo` for `if` tests, environment-dependent tests for conditional expressions |
| Parameter defaults, class arguments, `elif` tests, match guards, exception types, non-name augmented targets | visited | not visited, as before (walrus targets there are not definitions) |
| Nested scopes | walked with `walk_child` into the scope tree | ignored; the binder collects each body when it enters it |
| Name expressions | ignored (lowering models runtime reads) | recorded as `name_reads`: expressions, including assignment targets, except anywhere in a comprehension (even its first iterable, which the enclosing scope evaluates), plus walrus targets; annotated and augmented name targets and pattern names are dropped |

`ScopeTable`'s name lookup (`lexical::resolve`, built on `lexical::visible` and
`lexical::resolve_nonlocal`) and Pyrefly's scope iteration (`lexical::visible`) and
`nonlocal` capture search (`lexical::resolve_nonlocal`) run over their own scope trees
(`ScopeChain`). Pyrefly resolves `global` and `nonlocal` declarations once, when a scope's
static entries are created, so its name reads do not redirect. The engine knows only scope
categories and a barrier type that the backend chooses and joins; Pyrefly's flow barriers,
`Key`s and initialization checks stay in `binding/scope.rs`. Annotation and `type` alias scopes exist only in Pyrefly:
the runtime core still reports type parameters as a gap.

With the feature, every `Definitions::new` caller (exports, static scope initialization,
`try` fallbacks, the import tracker and stubgen) uses the walker. `Definitions::new_legacy`
keeps Pyrefly's own traversal as the only path without the feature and as a test oracle.
A driven binding (`Bindings::new_with_driver`) uses the walker for its static scopes and the
engine for every `visit_scopes` caller and capture search; `Bindings::new_legacy` uses
neither, except for `try` fallbacks, which call `Definitions::new`. `Bindings::scope_report`
counts what the shared machinery did.

Runtime changes that come with sharing: scope, region, symbol and slot numbering follow the
walker's order (for example, a lambda in a default is created before the function), and a
`nonlocal` declaration that would refer through an enclosing `global` declaration is now a
scope error, as it is in CPython. In the recovery from a module-level `nonlocal` (a syntax
error), the module's own binding of the name is found.

## How Pyrefly uses it

`BindingsBuilder::stmts` and `region_stmts` (function, class and module bodies) route to the
driver when it is active. The Pyrefly halves of `if`, loops and jumps are steps in
`binding/structure.rs` (`if_header`, `if_arm_header`, `if_arm_skipped`, `if_arm_body_start`,
`if_arm_body_end`, `if_arm_dead`, `if_finish`, `while_header`, `for_header`,
`loop_body_finish`, `loop_finish`, `loop_exit_stmt`) plus the suite gate steps. Without the
feature, a short legacy sequencer in the same file calls the same steps. The parity tests
therefore compare two sequencings of shared steps, which shows that the driver's sequencing and
pruning decisions match the legacy walk; they do not independently re-derive the semantics of
the original binding code. The full test suite without the feature validates that the
refactoring into steps preserved behavior.

While the driver is active:

- the legacy sequencing of `if`, loops, `break` and `continue` panics if it runs;
- every `break`/`continue` asserts that the driver's loop attribution agrees with Pyrefly's
  scopes. Pyrefly's driver target is `()`: the driver cross-checks whether a loop encloses
  the jump, while the loop exit flows themselves stay in Pyrefly's scopes;
- the bindings carry a `DriverReport`: driver coverage, plus statements bound by `stmt_impl`
  without nested suites ("leaf") and with nested suites ("mixed"). No module falls back to the
  legacy walk.

## Selecting a constructor

- Without the feature: `Bindings::new` is the legacy walk and the core is not a dependency.
- With `--features bindings-driver`: `Bindings::new` uses the driver. `Bindings::new_legacy`
  and `Bindings::new_with_driver` select explicitly; `Bindings::driver_report` and
  `Bindings::scope_report` expose the reports.

## Optional source identities and facts feedback

See [SHARED_FRONTEND.md](SHARED_FRONTEND.md) for the current connection API and its limits.
The shared visitor emits source-qualified branch/arm/merge/loop identities independently
of pruning and backend allocation order. Repeated `finally` copies repeat their source IDs;
these are not execution-instance IDs or a CFG-block map. Mapping them to concrete blocks
is a future requirement before automatically pruning the operation CFG.

Trace-enabled driven construction normally retains only arm-continuation key mappings
(`FlowObservation::Bindings`), not a full trace. `Bindings::new_with_flow_points` explicitly
requests `Full` observation; ordinary trace-disabled/legacy construction is `Disabled`.
The facts adapter reads already-solved `BranchEnd` evidence on request and exposes
`FlowFacts`, with no inference key in the neutral API. Its snapshot metadata reports
observation coverage, the requirement that declared types and typing rules hold, and
`DiagnosticCheck::NotProvided`. Consumers must check pre-check diagnostics separately.

Facts are `Unknown` or `NoNormalCompletionIfTypesHold`, not unconditional runtime proofs.
Generator and async activations are ineligible: a `Never` send type can still resume under
ordinary iteration. Exact source/module mismatches reject a view; rebuilding after
configuration or dependency changes remains the caller's job. This is a connection seam,
not a complete typed CFG or an incremental invalidation service.

## Evidence (Linux only)

- `cargo test -p pyrefly_static_analysis -p pyrefly_static_analysis_spmd -p pyrefly_static_analysis_lifeguard -p pyrefly_static_analysis_exports`:
  the 174 existing spike tests pass with lowering on the driver, plus driver tests in
  `static_analysis/tests/structured.rs` with a tracing backend that decides tests statically
  (pruning, dead arms, exhaustiveness, jump attribution across nested loops, functions and
  `with`, absorption).
- `cargo test -p pyrefly --lib --no-default-features --features bindings-driver`: all 9516
  pre-existing tests pass (3 ignored) with every module bound through the driver and the
  tripwires armed, as they do without the feature; the 7 parity tests below come on top.
- `pyrefly/lib/test/bindings_driver.rs`: fixtures bound by both constructors from the same
  AST, exports and configuration (with the scope trace and IDE bodies kept) must produce
  the same table rendered with dependencies as keys, the same `Debug` of every `Bindings`
  field (table indices, metadata, scope trace,
  unused names, ...), and the same binding-time errors. The checker's own bindings for each
  fixture carry the same driver report. Fixtures cover annotated functions with branches and
  calls, narrowing with early return and `NoReturn`, missing definitions, `while`/`for` with
  `else`/`break`/`continue`, first-use and contextual typing, deferred reads and `nonlocal`,
  statically pruned tests (`sys.version_info`, `sys.platform`, `TYPE_CHECKING`, `False`),
  mixed `with`/`try`/`match`/class bodies, namedtuple default absorption, walrus tests, and
  jumps outside their region.
- `pyrefly/lib/test/declarations_scopes.rs`: `Definitions::new` and `Definitions::new_legacy`
  produce the same `Debug` of every field, in insertion order, for every scope body the
  binder collects (module, function, class and `except` bodies) of fixtures covering walrus
  targets in every position, comprehensions, lambdas, optional positions, pruning, main
  guards, imports and every `__all__` convention, and for every bundled typeshed module, each
  under three environments. Driven and legacy bindings of scope fixtures (classes, methods,
  nested classes, `global`/`nonlocal`, capture errors, comprehension walrus targets, PEP 695
  scopes, dead code) are identical, and the driven report counts walker and engine uses.
- `binding/scope.rs` `shared_navigation_tests`: for every stack of up to four scope kinds,
  the engine yields the same scopes, depths and barriers as the legacy iteration; for every
  stack of up to three scopes with every kind of static entry (captures resolved as the
  binder resolves them), the shared `nonlocal` search returns what the legacy search
  returns.
- `static_analysis/tests/declarations.rs`: walker event order with a runtime and a pruning
  sink, the lexical engine on synthetic chains, and runtime ownership (unpruned dead code,
  defaults and regions, `.0`, class bodies inside functions, `nonlocal` through `global`,
  lookup from the builtins scope, module-level declarations). The existing `Definitions`
  unit tests pin the policy helpers' results with and without the feature.

Performance is measured separately in [the experiment report](BINDINGS_DRIVER_REPORT.md).
Functional parity does not imply acceptable overhead. The default build's binding of these constructs was refactored into steps
shared with the driver path, so its performance is part of what must be measured rather than
assumed unchanged.

## Not established

- Extraction of expression-level control flow, termination/reachability, narrowing or flow
  merging into shared code. Pyrefly's flow-sensitive name binding (`Scopes` flow,
  `add_lvalue_to_current_static`, first-use inference) does not use the walker.
- Runtime modeling of annotation scopes and type parameters.
- Pyrefly consumption of the core's operation CFG, `Engine` or `History`.
- Decomposition of `with`, `try` and `match` by the driver.
- macOS and Windows validation.
