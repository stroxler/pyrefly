# Static-analysis sharing spike

This is an **AI-assisted experimental branch**, not a production checker or an upstream change proposal. It tests whether Python scope, evaluation-order and flow machinery can support two different analyses without making Pyrefly's `Bindings`/`Answers` their API. Human review is needed before production use or upstream submission.

## Packages

- **`pyrefly_static_analysis`** (`src/`): lexical declaration/scope analysis, neutral operations and CFG, deferred code regions, full-state dataflow engine, and domain-free history queries.
- **`pyrefly_static_analysis_spmd`** (`spmd/`): tensor/gradient contracts, sharding descriptors, explicit collective transitions and an ambient active mesh.
- **`pyrefly_static_analysis_lifeguard`** (`lifeguard/`): a subset of lazy-import safety analysis, tracking value provenance, mutation, deferred effects and function summaries.
- **`pyrefly_static_analysis_exports`** (`pyrefly_exports/`): an optional adapter to Pyrefly's real export calculation. The core does not depend on the checker crate; this adapter does, without building Bindings or solving Answers.

The consumers do not parse Python or walk its AST. They use the same resolved symbols, ordered operations, frames and engine. Their domain-specific values, effects, summaries and mesh rules remain outside the core.

## Try it

From the repository root, with the repository's Rust toolchain:

```sh
cargo test -p pyrefly_static_analysis \
  -p pyrefly_static_analysis_spmd \
  -p pyrefly_static_analysis_lifeguard \
  -p pyrefly_static_analysis_exports

cargo run -p pyrefly_static_analysis_spmd -- \
  static_analysis/spmd/examples/tp_mlp.py \
  static_analysis/spmd/examples/tp_mlp.json

E=static_analysis/lifeguard/examples
cargo run -p pyrefly_static_analysis_lifeguard --bin lifeguard-spike -- \
  registry=$E/registry.py plugin=$E/plugin.py helpers=$E/helpers.py settings=$E/settings.py
```

The Lifeguard demo intentionally exits nonzero because it contains unsafe and unknown examples. The SPMD example validates only the supported contracts under its trusted JSON sidecar assumptions. Neither command executes the input Python or starts distributed training.

See [SPMD usage and boundaries](spmd/README.md) and [Lifeguard usage, parity matrix and boundaries](lifeguard/README.md) for details and exit codes.

## What is shared

The frontend takes inspiration from the public Pyrefly binding/declaration pass and Lifeguard implementation, but separates Python execution structure from inference policy. It resolves static ownership before reachability pruning; records source scope, statements and expression operations; separates immediate code from call/iteration/await-triggered regions; and supplies control-flow joins and definition/use queries.

A domain can change an existing name's facts or object/typestate information and carry arbitrary ambient state. It is not limited to returning a type for an expression. Tests include a small resource/lock domain, alongside the two primary consumers.

Execution order is not type-inference dependency order. A downstream Pyrefly adapter could add contextual typing, narrowing, first-use dependencies and synthesized bindings without putting those policies in the common frontend.

## What this does not establish

- **Full Pyrefly binding-pass capability is not demonstrated.** The runtime-focused IR is lossy for typing syntax and declaration provenance. [The adapter sketch and gap map](PYREFLY_ADAPTER.md) describe the additions and parity experiment needed to test that goal.
- **Layered/lazy expression CFG construction is not implemented.** Statement ranges provide coarse provenance/grouping, but expression operations are currently lowered eagerly. History and capture queries are optional; this is not yet a zero-cost coarse-only frontend.
- **Python safety and formal verification are not universal claims.** Unknown effects, supported-library assumptions, incomplete control-flow modeling and user-supplied contracts delimit what each verdict means. The consumers cover selected reference scenarios, not complete upstream parity.
- **Performance adoption is undecided.** Passing functional tests does not establish acceptable overhead for replacing Pyrefly's binding phase. The engine's sparse live-temporary representation improves the branch probe, but History still has a blocks-by-symbols cost (about 24/96/352 ms for 250/500/1000 branches in one release probe). These are prototype measurements, not Pyrefly phase benchmarks. Optimized, allocation-aware comparisons and skipped-body/no-consumer measurements are necessary.
- **Nested cleanup expansion is bounded, not fully optimized.** A per-region budget limits duplicated `finally` bodies; even ten nested levels can hit it. The resulting `FinallyLimit` gap explicitly marks the analysis incomplete rather than risking unbounded expansion.
- **Cross-platform execution is unverified.** Validation for this spike was performed on Linux; macOS and Windows need separate runs.

The [spike outcome and review evidence](SPIKE_REPORT.md) summarize the experiment and its validation. [DESIGN.md](DESIGN.md) explains the dependency direction and intended abstraction boundaries. Both the implementation and its scope are experimental; unsupported cases must not be presented as validated.

## Public references

- Pyrefly base: `facebook/pyrefly` at `0424eebb19345ca0b40727a15cfca9e759c05d6b`.
- SPMD semantics: `meta-pytorch/spmd_types` at `e5ebc6d840fa2399bcd10306664faf4a2b5feaf6`.
- Lifeguard scenarios: `facebook/Lifeguard` at `c04fde1358c467d7ec84244ddfb066186381f066`.

The SPMD implementation is based on public semantics, not exact parity with a separate unavailable prototype. Each consumer documents intentional restrictions and differences from its reference.
