# Pyrefly bindings as a downstream consumer

This is the earlier capability-gap sketch. The implemented structured driver, shared
declaration/lexical-navigation mechanisms and optional feedback seam are described in
[SHARED_FRONTEND.md](SHARED_FRONTEND.md); remaining full-syntax and expression-driving
work below is still separate from those demonstrated pieces.

## Goal and evidence boundary

The long-term capability target is an exact match or superset of today's binding pass, without making `Bindings` or `Answers` the shared frontend API. Adoption by Pyrefly remains optional: a substantial performance cost could make sharing undesirable even if it is technically possible.

**The current spike does not establish that capability target.** Its neutral scopes, regions, ordered operations and CFG provide a plausible substrate, but its runtime-focused IR discards information required by the actual bindings pass. This document is an implementation sketch and gap map, not a completed migration or a performance result.

Existing Pyrefly references below are against upstream commit `0424eebb19345ca0b40727a15cfca9e759c05d6b`. The spike's files are the working implementation in this branch.

## Dependency direction

```text
one parse + shared declarations / scope / flow construction
                         |
       neutral Program + optional lossless syntax view
                         |
             Pyrefly-specific binding emitter
                         |
              actual Bindings -> existing solver
```

The emitter belongs in Pyrefly or an optional adapter, not the generic core. Its job is to retain Pyrefly's binding tables, inference policy and result packaging while replacing frontend-facing work. Today's [Bindings::new](https://github.com/facebook/pyrefly/blob/0424eebb19345ca0b40727a15cfca9e759c05d6b/pyrefly/lib/binding/bindings.rs#L702-L897) combines that work with AST traversal, scope state, deferred name processing and exports.

Binding emission should **not** happen as side effects of `Domain::transfer`: the forward engine repeats transfers during fixpoint iteration and diagnostic replay. An emitter should reserve graph identities, obtain neutral flow relationships, and emit each computation once. It need not use the generic worklist engine as its dependency solver.

## Identity mapping

`SymbolId` denotes one name owned by one scope. It is not one definition or one typing computation. The emitter needs separate mappings:

- declaration/write occurrence -> `Key::Definition`;
- read occurrence -> `Key::BoundName` plus its selected value/narrowing dependency;
- scope/symbol -> annotation, capture and anywhere-summary bookkeeping;
- producing operation and merge port -> the computation supplying a value.

The spike's `Reg` is not SSA: both arms of a conditional expression can write the same temporary. A simple `(RegionId, Reg) -> Key` map would therefore lose producers. Use operation identities and predecessor ports.

Nor should every runtime operation become a separate `Binding::Expr`. Existing [NameAssign](https://github.com/facebook/pyrefly/blob/0424eebb19345ca0b40727a15cfca9e759c05d6b/pyrefly/lib/binding/binding.rs#L2327-L2357) retains a whole RHS expression so the existing solver can apply contextual typing. Runtime decomposition and typing-computation ownership must remain distinct.

## Typing dependencies stay downstream

Execution order is not inference order. An annotation or receiver can supply an expected type to the RHS; callable parameters constrain arguments; predicates generate narrowing dependencies; first-use inference can connect a definition to a later use.

The shared frontend should preserve source occurrences, expression relationships and declaration records. Pyrefly decides whether a particular occurrence is a type, metadata, ordinary value, or non-pinning read. This includes `cast`, type-like subscripts, and `Annotated` metadata. Retaining syntax does not require a second scope/control-flow walker: shared traversal supplies occurrence records and expression roots, while the existing solver retains its expression inference.

First-use resolution belongs in graph finalization. Pyrefly records uses with their computation owners, fills phis and then finalizes reads in insertion order. Runtime earliest-use order is insufficient; static/non-pinning reads and solve-time feedback differ. See [deferred name-use finalization](https://github.com/facebook/pyrefly/blob/0424eebb19345ca0b40727a15cfca9e759c05d6b/pyrefly/lib/binding/bindings.rs#L2027-L2117).

## Flow, termination and deferred references

CFG predecessors are inputs to Pyrefly phis, not finished phis. A branch's value can be paired with a typing computation that establishes whether that branch terminates. A dependency solving to `Never` may remove a branch from a typing merge; missing definitions generate deferred initialization checks.

Expose incoming-edge identities and definitions before generic joins discard them. The adapter adds branch tests/end computations, narrowing, exhaustiveness and loop-merge policies. Explicit runtime return/raise structure helps, but does not resolve type-dependent continuation.

Reserve identities before filling computations to support recursion and forward references. Runtime closure-cell reads are invocation-time facts; Pyrefly's static/anywhere lookup and forward typing references are separate adapter policies. Domain-free `History::reaching` marks possible writes from other regions with `Def::External`; such a set is not an exact local-only value history. It does not resolve invocation-time cross-region reaching definitions.

## Concrete example

```python
from typing import Callable


def pick(flag: bool, f: Callable[[int], int]) -> int:
    if flag:
        x: int = f(1)
    else:
        x = 0
    return x
```

An experimental emitter would:

1. Reserve function, parameter, annotation and return identities.
2. Bind the condition's `flag` occurrence and add downstream narrowing/branch computations.
3. Emit two definition/`NameAssign` pairs for `x`. Keep `f(1)` as the true RHS computation; attach its `f` read to that assignment's usage context. Forward the canonical annotation to the other assignment under Pyrefly's policy.
4. Emit the merge with predecessor/continuation dependencies, then connect the return's `x` read to it.
5. Emit return-type and explicit-return computations and finalize deferred uses.

This maps to existing statement/function constructor seams, but **requires additional syntax preservation**: current runtime lowering does not retain the `x: int` declaration sufficiently to reproduce its complete binding output.

## Capability gap map

The gap map is not limited to constructs already marked with a runtime `Gap`:

- **Annotation syntax and occurrences:** annotation-only declarations can emit no runtime operation. `StaticExpr::Other` retains only a range; structured static forms lose some occurrence ranges and collection distinctions. Preserve declaration records and lossless annotation/type-parameter syntax.
- **Expression payloads and computation grouping:** bytes, complex/oversized numerical values and formatted-string structure are compressed. Target groups, keyword identifier ranges and enclosing RHS ownership are incomplete. Source slices are not a replacement for a parsed syntax view.
- **Scope/capture provenance:** declaration occurrences (`global`/`nonlocal`), static capture anchors and diagnostic ranges need fuller records. Operations now carry their lexical evaluation scope, and class/module loads have shared binding-presence/fallback semantics; those runtime facts still differ from Pyrefly's typing lookup policy.
- **Flow ports:** structural CFG edges exist, but joined frames/history sets discard incoming-edge provenance. Raised exits now retain source range, cause and state. Return terminators still need explicit/implicit origin and range metadata for exact binding parity.
- **Function/class metadata:** headers, suites, decorator roles, overload adjacency, annotation-defined fields and synthesized `__class__` plumbing are incomplete. Shared generic declaration structure belongs upstream; overload, `Self`, dataclass/framework synthesis and shape-DSL policy stay downstream.
- **Typing declarations:** PEP 695 scopes, alias RHS syntax, bounds/constraints/defaults and recursive alias anchors are outside v0. Ordinary function regions are not enough to reproduce them.
- **Patterns, exceptions, imports and packaging:** approximation gaps are not lossless representations. Exact binding/IDE parity also includes suppression ranges, scope traces, unused-symbol data and other result metadata.

## Smallest next experiment

Three frontend additions would make the seam testable:

1. An **optional lossless syntax view**, with occurrence IDs, original expression roots, exact ranges, declaration/header/suite records and separately represented typing syntax.
2. A **neutral predecessor-port query**, including incoming definitions of symbols and temporary values plus entry/unbound paths.
3. A **parse-reuse entry point** accepting the already parsed module. Optional collectors should not allocate when absent.

Then implement an opt-in emitter for names, assignments, one function, calls and branches. Compare canonicalized actual binding tables with `Bindings::new`: source identities, kinds, dependency edges, expectations and metadata, not merely final inferred types. Add first-use, annotation-only, loop/unbound, recursion/capture, termination, decoration, class and type-parameter fixtures incrementally. Unsupported constructs must fail coverage checks rather than fabricate parity.

## Performance decision

Benchmark optimized, comparable configurations: parse only; parse plus neutral frontend; frontend plus history; frontend plus syntax view/emitter; existing bindings. Separate parsing-included and parsing-excluded results, allocations and peak memory, with comparable AST lifetimes.

Preserve existing skipped-body/lazy-work optimizations where possible. Measure the no-domain/no-history/no-syntax-overlay path explicitly. Do not adopt a percentage threshold without a baseline and measured noise; capability success and performance acceptability are independent decisions.

A small parity-tested adapter would increase confidence in reusability. Until then, the two implemented domains establish a narrower result: shared analysis machinery for their supported subsets, not replacement capability for the full Pyrefly binding pass.
