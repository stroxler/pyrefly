# Pyrefly bindings on the shared structured driver

This branch makes Pyrefly's real binding pass a third consumer of the shared static-analysis
core, alongside the SPMD and Lifeguard domains. It is an **AI-assisted experiment**, opt-in
behind a Cargo feature, and it covers **statement-level control structure only**. It is not
a migration of the binding pass and does not establish full replacement capability.

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
| Static scope tables (`Definitions` with `SysInfo` pruning, annotations, `Final`, `__all__`) | Pyrefly, not extracted |
| Other syntax walks over statements: `function_last_expressions` (implicit returns and suite gates), `Ast::body_contains_yield`, `report_unreachable_body`, `SelfAttrNames`, `SuperMethodCallFinder` | Pyrefly, not extracted; they still traverse the AST independently of the driver |
| Structure of `with`, `try`, `match`, function and class headers | Pyrefly; the driver sequences their nested suites ("mixed") |

The core's lexical `ScopeTable` is shared by lowering only. Pyrefly does not consume it: the
runtime scope view and Pyrefly's `SysInfo`-pruned typing view differ, and projecting one onto
the other is not attempted here.

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
  and `Bindings::new_with_driver` select explicitly; `Bindings::driver_report` exposes the
  report.

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

Performance is measured separately in [the experiment report](BINDINGS_DRIVER_REPORT.md).
Functional parity does not imply acceptable overhead. The default build's binding of these constructs was refactored into steps
shared with the driver path, so its performance is part of what must be measured rather than
assumed unchanged.

## Not established

- Extraction of expression-level control flow, static scope tables, termination/reachability,
  narrowing or flow merging into shared code.
- Pyrefly consumption of the core's operation CFG, `Engine` or `History`.
- Decomposition of `with`, `try` and `match` by the driver.
- macOS and Windows validation.
