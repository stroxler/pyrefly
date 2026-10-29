# Shared static-analysis frontend: experimental design

This branch is an open-source feasibility spike. The question is whether two substantially different analyses can share Python scope, evaluation-order, and flow machinery—not whether this branch replaces either production checker.

## Dependency direction

```text
Python syntax and lexical ownership
              |
ordered operations, regions, CFG, symbol history
              |
   +----------+--------------+
   |          |              |
SPMD      Lifeguard      history queries
 domain     domain       without a domain
```

Pyrefly's type-specific `Bindings` and `Answers` are not the shared intermediate representation. A future Pyrefly binding emitter could consume this frontend and add inference dependencies downstream. The experimental export adapter depends on the existing checker crate but the core does not.

## Shared work versus domain work

The frontend owns declarations, scope visibility, evaluation order, deferred versus immediate code regions, and explicit flow edges. Statement ranges provide a coarse grouping of ordered expression operations. Consumers do not reparse source or reconstruct scopes by walking the AST.

The engine owns frame bookkeeping, branch/loop propagation, joins, convergence limits, and deterministic diagnostic collection. A domain defines abstract values plus arbitrary state: heap/typestate facts, capability state, ambient meshes, effects, or other context. An operation may change existing facts; it need not have a useful return value.

Assignment preserves a value reference, not a link that follows later rebinding of another name. Lexical symbol ownership, reaching definitions, and object aliasing are distinct. Domains that track mutable objects must preserve object identities and account for escapes. Captured bindings and globals are activation-time cells, not permanently frozen definition-time values.

Ambient context is carried and joined along paths, rather than kept as a process-global mode. Scoped entry/exit and call contracts belong in analysis semantics. Unknown context or differing predecessor contexts must not be resolved by choosing one predecessor arbitrarily.

## Expression ordering and inference ordering

Expression-flow consumption is useful for custom type systems, but runtime evaluation order is not type-inference dependency order. Ordinary assignment evaluates RHS before target expressions; augmented assignment evaluates its target/read before the RHS. Short-circuit operators and conditional expressions contain control flow.

A downstream type checker may introduce contextual/deferred dependencies without changing the shared execution structure. A statement-level client may summarize expression behavior; it must not treat a statement as effect-free merely because its interior is unexpanded.

## Evidence needed for generality

The consumers must both use the same neutral operations and engine. SPMD exercises tensor/gradient contracts and active-mesh context. Lifeguard exercises imported-object provenance, mutation, deferred function effects, and call summaries. A history-only client and a small typestate test probe additional reuse without adding a third production analysis.

Unsupported Python features and unconverged analyses are explicit incomplete outcomes. Passing a selected fixture subset is not full parity, and neither domain provides universal formal verification of arbitrary Python or custom kernels.

Temporary adapters into complete Pyrefly/Lifeguard forks are possible follow-up experiments; production migration is outside this spike.
