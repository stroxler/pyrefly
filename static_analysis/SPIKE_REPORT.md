# Spike outcome and review evidence

## What this experiment establishes

Two substantially different analyses can share the new Python frontend and full-state
engine without using Pyrefly's `Bindings`/`Answers` as their API or adding their own AST
walkers. SPMD uses tensor/gradient descriptors and scoped mesh context; Lifeguard uses
value origins, mutation effects and invocation-time summaries. Both drove changes to
the shared base rather than receiving separate scope/flow workarounds.

The shared pieces include static ownership, dynamic class/module fallback, presence-aware
slots and outer cells, ordered operations, deferred regions, branch/loop flow, cleanup
and exception provenance, free-symbol queries, and history queries. The tests also use
a resource/lock domain independent of either consumer.

This is **not** a capability match for the full Pyrefly binding pass, full upstream
Lifeguard/SPMD parity, or a production/formal-safety claim for arbitrary Python. The
[Pyrefly consumer sketch](PYREFLY_ADAPTER.md) identifies the lossless syntax, declaration
records, predecessor provenance and performance experiments needed to test that goal.

## Independent review

Fresh Claude and Codex reviews assessed the integrated implementation, not only the
individual consumers. Both supported the central dependency direction and reuse claim,
but the first passing suite was insufficient: independent reproductions found false
safe/validated results, a panic, incorrect history, orphan operations and scaling issues.

Those findings led to regression tests and changes including:

- runtime evaluation of annotation-only targets;
- binding presence carried across cells, summaries and builtin fallback;
- global tensor partition validation independent of an optional return contract;
- recursive substitution of returned class/instance values and descriptor dispatch;
- provenance-preserving `cast` and conservative callback/generator handling;
- query-scoped export incompleteness and module-valued re-export resolution;
- once-only lowering of deferred bodies, external-write markers in history, eager first
  generator iteration and cleanup of handler names;
- sparse live temporaries, ordered worklists, restricted capture keys and a bounded
  `finally` expansion budget.

Focused rechecks cleared the original blocking findings. This means the prototype is
ready to be examined and extended under its stated assumptions; it is not proof that
no other bugs remain.

## Verification

The targeted suite contains **174 passing tests**: 36 shared-core, 31 SPMD, 98 Lifeguard
and 9 export-bridge tests. The timing probe is ignored by default. The Lifeguard suite
includes 67 reference-adapted parity fixtures plus regression and spike-semantics cases;
its README records their provenance and intentional differences.

The repository formatting/lint script, package checks and Ruff checks on Python examples
were run. Both documented CLI demonstrations were executed: the SPMD example validates
its supplied contracts; the Lifeguard example intentionally reports unsafe/unknown cases
and exits nonzero. Validation was performed on Linux only.

Public reference commits are listed in [README.md](README.md). The SPMD consumer follows
public semantics rather than claiming exact parity with a separate unavailable prototype.

## Costs and remaining design questions

- **History remains quadratic.** A release probe over 250/500/1000 conditional statements
  measured about 24/96/352 ms. Sparse/persistent per-symbol representations remain work.
- **The engine's branch probe improved** after sparse live-temporary storage; one release
  probe measured about 0.28/1.9/7.5 ms for 100/800/3200 branches. These are isolated
  prototype measurements, not a comparison with Pyrefly's binding phase.
- **Capture-key cost improved**: a 2000-decorated-function fixture used roughly 13–15 MB
  and 0.01–0.02 s after restricting cells, versus a roughly 1.6 GB earlier snapshot.
  Module-wide function-global unions can still be costly in other shapes of program.
- **Cleanup expansion is capped**, and roughly 8–10 nested `try`/`finally` levels can
  already produce `FinallyLimit`, explicitly making results incomplete.
- **Invocation semantics remain partly consumer-owned.** A future reusable argument,
  descriptor and activation layer is a promising seam; it should not absorb Lifeguard
  effect policy into the core.
- **Expression lowering is eager**, despite statement provenance/grouping. A truly
  layered, opt-in expression CFG and optional lossless typing syntax view are unbuilt.
- **Export integration is unstable and optional.** It exposes the existing export module
  on this branch and currently parses again for module-value classification. That is not
  a supported upstream API or replacement-performance result.
- **macOS/Windows execution and full binding-emitter parity are unverified.** Adoption
  requires separate cross-platform, capability and optimized allocation-aware checks.

No upstream pull request or trunk landing is part of this experiment. Human review is
required before proposing a production migration.
