# Shared declarations, lexical scopes, and optional type feedback

This iteration extends the separate `spike/bindings-driver` experiment toward two goals:

1. Share the Python frontend's declaration, lexical-navigation and structured-flow mechanisms.
2. Let a downstream tool optionally connect facts from a Pyrefly pre-check through a neutral API.

It remains an **AI-assisted prototype**, not an upstream migration or a proof of arbitrary
Python execution. The Pyrefly-specific adapter stays separate from the reusable core.

## What is actually shared

### Declaration collection

`static_analysis::declarations` provides a per-scope event walker. Both the runtime
`ScopeTable` builder and feature-enabled Pyrefly `Definitions` consume it; neither sink
has its own competing declaration-discovery traversal.

The walker emits binding/directive occurrences, nested-scope boundaries, and syntactic
name occurrences. It handles target decomposition, comprehensions/walrus placement,
parameters, imports, handlers, patterns and body traversal. Name occurrences have an
explicit `NameSite`; they are **not** a runtime read stream. Use the operation IR and
history queries for runtime uses.

Consumer policy is explicit:

- Runtime lexical ownership is unpruned: a binding in dead code still makes a name local.
- Pyrefly's configured declaration/export view selects environment branches and interprets
  annotations, `Final`, deprecation, import conventions and `__all__` downstream.
- Legacy omissions of certain header/default/pattern-reference positions are implemented
  in Pyrefly's sink, not presented as universal Python semantics.

All five feature-enabled production `Definitions::new` callers use the shared scanner.
An independently selectable legacy traversal supports differential tests. Normal runs
replace the work; they do not run a shadow scanner first.

### Lexical navigation and capture search

`static_analysis::lexical` navigates a backend's scope tree or stack through `ScopeChain`.
It supplies visible-scope iteration, class/annotation visibility and nonlocal-search
structure. Runtime resolution and actual Pyrefly scope navigation use these mechanisms.

What is found in a candidate scope remains backend-owned: initialized values versus
static declarations, dynamic class fallback, overload handling, forward/Anywhere lookup,
inference keys and error recovery. Pyrefly's narrowing and merge algebra are not moved
into the core.

This is shared machinery, **not one shared table of type-specific values**. Runtime symbols
and Pyrefly static binding payloads remain different projections. Some specialized Pyrefly
walks remain, including semantic-syntax nonlocal checks, comprehension walrus placement,
annotation-shadow queries, implicit-return/yield checks and unreachable-body analysis.
Global capture in Pyrefly is a direct module lookup; it is not a substantial new shared
search algorithm.

### Structured flow

Both actual CFG lowering and binding construction continue to consume the same structured
statement driver. Its branch/loop sequencing is separate from backend-specific narrowing,
merging and type-dependent continuation.

The shared scope extraction also fixes a runtime bug: `nonlocal` cannot cross an enclosing
`global` declaration. That case is a scope error, consistent with CPython. Public resolution
from an exposed builtin-scope ID remains supported.

## Optional connection back from Pyrefly

### Source identities

The shared visitor can emit `FlowPointId { region, range, kind }` for suite/scope envelopes,
branches, arms, joins and loop boundaries. IDs describe source occurrences, qualified by
module and source snapshot; they do not depend on visitation ordinals or backend pruning.
Repeated `finally` expansions intentionally repeat source IDs.

`analyze_python_with_flow_points` exposes observation from actual CFG lowering. Ordinary
lowering does not allocate collector storage. Pyrefly's modes are explicit:

- `Disabled`: no observation.
- `Bindings`: only arm-continuation mappings, used by trace-enabled driver builds.
- `Full`: full source-point trace plus mappings, requested explicitly with
  `Bindings::new_with_flow_points`.

The ordinary IDE/trace path does not retain the entire source trace.

**There is no source-point-to-CFG-block map yet.** These points make the connection
identifiable, but a further location mapping is required before automatically pruning
concrete operation-CFG blocks. Expression-level control flow and lambda/comprehension
scope points are not covered by this statement-driver observer.

### Facts interface

The core's `FlowFacts` trait exposes metadata and a point query. It contains no Pyrefly
`Key`, `Binding`, `Answers`, type representation or merge policy. A tool may hold
`Option<&dyn FlowFacts>`; absent facts remain conservative.

The Pyrefly adapter uses genuine observed `ArmExit → BranchEnd` mappings and already-solved
answers. It neither reparses source nor solves missing keys on demand. A caller can compose:

```rust,ignore
let lowered = analyze_python_with_flow_points(source, module, false);
let snapshot = answers.flow_facts();
let facts = snapshot.for_source(module, source)?;
let provider: &dyn FlowFacts = &facts;
let conditions = provider.metadata();
for point in &lowered.flow_points {
    let continuation = provider.normal_completion(*point);
    // The downstream domain chooses what these conditional facts mean for its analysis.
}
```

The query returns `Unknown` or **`NoNormalCompletionIfTypesHold`**, not unconditional
runtime impossibility. The metadata states that declared types and typing rules must hold,
diagnostics are not provided, and observation is either unavailable or has mapped/eligible/
conditional-fact counts. Consumers must check pre-check diagnostics separately and decide
whether those assumptions are appropriate for their own use case.

For example, a defensive `isinstance` branch can be dead under an `int` annotation but
execute when untyped code passes a string. That is not a runtime proof. Invalid `NoReturn`
implementations likewise do not become sound because a declared signature yields a bottom
type.

Generator send types provide another important counterexample: `yield` can have type
`Never` as its accepted `.send()` value while ordinary iteration still resumes. The adapter
therefore withholds completion facts for generator/async activations. Eligibility is finalized
after the whole function body using existing checker state, including later/default-header
yields. Nested plain functions retain their own eligibility. No extra suspension AST walker
was introduced.

The snapshot rejects mismatched modules and exact-source edits, including same-length edits.
**Dependency/configuration freshness is caller-owned**: recreate the snapshot after those
change. This prototype is not an incremental invalidation service.

## Validation approach

The new declaration oracle constructs independent shared and legacy results and compares
every stored field in insertion order. It covers adversarial fixtures, nested scope bodies
and bundled typeshed modules under multiple configured environments. Binding parity also
checks metadata, scope traces, errors and actual shared-path counters. Prepared exports and
some feature-selected helpers are reused by that binding harness, so it is not a fully
independent end-to-end oracle.

Runtime ownership was reviewed against CPython `symtable`; focused tests cover dead-code
locals, headers/defaults, comprehensions, global/nonlocal barriers, class visibility and
builtin-origin resolution. The two original consumers remain regression gates.

The feedback tests cover real backend ID alignment, pruning-stable IDs, repeated source
occurrences, ordinary `NoReturn` evidence, missing/`Any` facts, generator send types,
independently eligible nested functions, conditional typing assumptions, explicit coverage
and stale-source rejection.

Fresh independent Claude and Codex reviews found substantive issues despite passing suites.
Their regressions prompted the generator guard, builtin-origin fix, portable oracle paths,
consistent syntactic-name events, explicit observation modes and assumptions/coverage
metadata. Passing those rechecks establishes the scoped prototype, not absence of all bugs.

## Verified results for this iteration

On the final integrated source, **9,540 Pyrefly feature-enabled tests** pass, with three
ignored; the feature-off suite passes **9,516**, also with three ignored. The independent
declaration, navigation, binding, source-ID and facts tests are included in the feature
suite. The core and original consumer packages pass **195 tests**, with the timing probe
ignored. Repository formatting/linting and strict feature-specific Clippy pass.

The same serial, optimized constructor-only harness was rerun after this iteration.
Both constructors keep trace/IDE information; parsing, AST cloning, solver setup, solving
and dropping results are outside measurement. Twenty samples alternate constructor order,
after a two-round warm-up. The reported median is the upper middle sample for an even-sized
sample set. The build uses release optimization with LTO disabled, debug info
disabled and 16 codegen units. Snapshot creation/query time and allocations are not measured.

| Fixture | Legacy median | Shared median | Ratio |
| --- | ---: | ---: | ---: |
| Annotated function with branch | 57.040 µs | 59.460 µs | 1.0424 |
| Contextual typing and loop | 59.631 µs | 62.481 µs | 1.0478 |
| 100 annotated functions | 1.421845 ms | 1.479196 ms | 1.0403 |

This single shared-devserver run suggests roughly 4–5% extra construction cost on those
fixtures. It is not a controlled comparison to the original pre-refactor binder, a default
CLI/whole-project benchmark, or proof of a zero-cost feature-off path. Shared mode replaces
binding-scope declaration scanning/navigation but uses common prepared exports in the
harness, so exported-module preprocessing cost is outside the comparison.

Rerun serially using the command in [BINDINGS_DRIVER_REPORT.md](BINDINGS_DRIVER_REPORT.md).
The harness prints raw samples and coverage. Do not run timing probes concurrently with
builds or other benchmarks. The old report's numbers are historical, not this result.

## Remaining work

- Broader scope/evaluation support for type parameters and annotations in the runtime core.
- A lossless optional syntax/typed-AST view and shared expression-level driving.
- Source-point-to-block mapping and consumer-specific use of conditional facts.
- Snapshot invalidation integrated with module/configuration dependencies.
- Optimized phase/allocation comparisons and default-path/corpus measurements before adoption.
- macOS and Windows execution. Platform policies in tests are not execution on those platforms.

The original structured-driver timing report is historical and does not measure this broader
scope/feedback iteration. No zero-overhead or full binding-pass replacement claim is made.
