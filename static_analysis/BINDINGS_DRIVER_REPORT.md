# Bindings-driver experiment: result

## Conclusion

The shared base can drive Pyrefly's **actual statement-level binding traversal**, with the
existing solver unchanged. It also drives the original operation-CFG lowerer used by the
SPMD and Lifeguard consumers. This is not a lookalike graph or a validation walk in front
of the old sequencer: the same `structured` module selects and sequences suites, `if`
arms and loop steps for both backends.

This increases confidence in the shared-driver direction, but **does not establish a
complete scope/expression frontend replacement**. Pyrefly still owns its static declaration
scan, expression binder, narrowing, merge algebra and termination facts. Its jump targets
are cross-checked against the driver; actual jump flow remains in `Scopes`.

## Branch separation

- `spike/static-analysis` receives only the neutral structured-driver module, the existing
  CFG lowerer's adoption, shared tests and generic documentation.
- `spike/bindings-driver` additionally contains the Pyrefly backend-step extraction,
  feature-gated adapter, parity and timing harnesses, and this experiment's notes.
- No upstream/trunk landing or pull request is part of this experiment.

The adapter is selected with `cargo ... -p pyrefly --features bindings-driver`. In that
build, `Bindings::new` uses it. Explicit `new_legacy` and `new_with_driver` constructors
support comparisons. Without the feature, the driver is not a Pyrefly dependency and
no driver/report state is built, although the legacy path contains the backend-step
refactor and therefore must not be assumed performance-identical to the original.

## Evidence

The existing Pyrefly library suite passed **9,516 tests with three ignored**, both before
and after the refactor without the feature, and with driver-enabled binding construction.
Seven additional parity tests bring the final driver-enabled run to **9,523 passed and
three ignored**. They compare both constructors from identical ASTs, exports and
configuration. They require identical displayed dependency graphs, every `Bindings` field
including indexed tables/metadata/scope traces, and binding-time errors; only the new
coverage report is excluded. The actual checker's report must match the test constructor's.

Those seven tests compare two sequencings of common backend steps, **not an independent
binding-graph snapshot from the original revision**. The full no-feature suite is the
behavior-level regression evidence for the backend extraction.

The original 174-test spike suite and three new driver tests pass. The new tests exercise
backend-supplied pruning/exhaustiveness/dead arms, jump attribution and absorption. Driver
coverage counts happen at real structural decisions. Legacy structural sequencing panics
if entered while the driver is active, and Pyrefly jump handling checks agreement with
the driver's loop presence. Mixed `with`/`try`/`match`/class/header structure is reported
as mixed rather than counted as full extraction.

Fresh independent Claude and Codex architecture and cumulative reviews confirmed the
shared-driver path and found no remaining functional blocker in their bounded checks.
Review requests clarified the scope/evidence and feature-gated a test-only input helper.
Repository formatting/linting and feature-specific checks were run. Validation was Linux
only; macOS and Windows remain untested.

## Optimized construction probe

`pyrefly/lib/test/bindings_driver_bench.rs` is an ignored, release-only test. It prepares
a checked in-memory fixture, then alternates legacy/driven constructor order over 20
samples after two warm-ups. Parsing, AST cloning, solver/error-collector setup, solving,
error collection and dropping the result are outside the measured interval.

Both constructors have **scope tracing and IDE-body analysis enabled**. These are not
default CLI settings. The build used `--release`, LTO disabled, debug information disabled
and 16 codegen units for both paths. Results are a single run on a shared development
machine, not a statistical adoption threshold or an allocation benchmark.

| Fixture | Legacy median | Driven median | Ratio |
| --- | ---: | ---: | ---: |
| Annotated function with one branch | 29.350 µs | 30.100 µs | 1.0256 |
| Contextual typing, early exits and a loop | 55.950 µs | 56.760 µs | 1.0145 |
| 100 annotated functions with branches | 1.397295 ms | 1.408405 ms | 1.0080 |

This is encouraging evidence that structured sequencing need not be costly for these
fixtures. It does not compare the original pre-refactor binder to the feature-off path,
measure default CLI/whole-project latency, or prove acceptable memory overhead. The
shared lowerer now allocates an arm vector per `if`, and Pyrefly allocates driver/report
state per module when enabled; their cost is not universally zero.

Rerun serially:

```sh
CARGO_PROFILE_RELEASE_LTO=false \
CARGO_PROFILE_RELEASE_DEBUG=0 \
CARGO_PROFILE_RELEASE_CODEGEN_UNITS=16 \
cargo test --release -p pyrefly --lib --no-default-features \
  --features bindings-driver bindings_driver_construction_timing -- \
  --ignored --nocapture --test-threads=1
```

The harness prints raw samples, medians and driver coverage. Do not benchmark concurrently
with other builds or timing runs.

## Remaining seams

1. Project the unpruned shared declaration facts into Pyrefly's typing-specific view;
   its current `Definitions` scan remains independent.
2. Share optional expression-flow driving while retaining whole inference payloads and
   downstream contextual/first-use dependencies.
3. Reconcile remaining parallel statement walkers, including implicit-return, yield and
   unreachable-body analysis. Their policy currently must agree with the binding backend.
4. Decide whether more structured constructs belong in the shared driver. `with`, `try`,
   `match` and definition headers are not fully decomposed here.
5. Measure original/refactored/driver configurations with phase and allocation isolation,
   representative corpora, default CLI settings and platform coverage before adoption.

See [BINDINGS_DRIVER.md](BINDINGS_DRIVER.md) for the implementation ownership map and
[PYREFLY_ADAPTER.md](PYREFLY_ADAPTER.md) for the broader capability-gap sketch. This
experiment demonstrates a real reusable sequencing seam; the full binding-pass capability
and performance decision remains separate.
