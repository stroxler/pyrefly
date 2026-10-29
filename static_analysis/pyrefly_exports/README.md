# Export-provenance bridge (experimental)

This optional package uses Pyrefly's real `Exports` calculation without constructing
`Bindings` or requesting solved `Answers`. It implements the generic core's
`ExportProvider`; the core and primary consumers do not depend on the checker crate.

Inputs are an explicit immutable set of source/stub modules and configured `SysInfo`.
There is no filesystem discovery or incremental mutation. Export dependencies are
recorded for inspection; changing the universe requires rebuilding the index.

## Resolution and boundaries

- Named and renamed re-exports follow declaration provenance through modules.
- `__all__` controls wildcard enumeration. A dynamic/unresolvable `__all__`, missing
  wildcard dependency or calculation cycle is incomplete, never an exhaustive empty
  surface. Failure evidence is cached per calculation and propagated to its dependents;
  it does not contaminate unrelated queries.
- Module-valued imports and simple assignments are classified through the shared
  frontend/engine, not a separate import AST walker. Rebinding does not redirect an
  existing alias. Ambiguous or effectful module candidates remain unknown.
- Supplied package submodules are distinguishable from named declarations. Dynamic
  module attributes (`__getattr__`) remain unknown.
- The direct API retains `Found`, `Module`, `Missing` and `Unknown` outcomes. The generic
  provider maps missing/unknown to `ExportTarget::Unknown` for consumer safety.

**Declaration provenance is not runtime purity or identity proof.** Missing runtime
packages, source/stub differences, dynamic namespace mutation and unsupported syntax
still require separate assumptions or validation. The bridge uses existing Pyrefly
export policies; it is not a lossless runtime module model.

The experimental integration exposes Pyrefly's `export` module with `#[doc(hidden)]`.
That is a deliberately unstable visibility change on this branch, not a supported
upstream plugin API.

The adapter currently parses for Pyrefly Exports and again for the shared module-value
classifier. A parse-reuse entry point is part of the proposed frontend evolution; this
bridge is not evidence of acceptable replacement-performance overhead.

```sh
cargo test -p pyrefly_static_analysis_exports
cargo clippy -p pyrefly_static_analysis_exports --all-targets --no-deps -- -D warnings
```

Tests cover named/wildcard/relative re-exports, module aliases, rebinding, submodules,
dynamic attributes/`__all__`, cached failures and cycles, independent queries after
failure, and provenance flowing through the shared name/frame bookkeeping.
