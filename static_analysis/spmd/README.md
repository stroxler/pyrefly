# SPMD contract consumer (experimental)

`pyrefly_static_analysis_spmd` is a separate Cargo package using the shared
`pyrefly_static_analysis` frontend and worklist engine. It supplies an arbitrary
stateful domain: imported identities and tensor descriptors flow through the
core's frame, while active mesh context and unknown-effect invalidation flow
through the domain's state. It does not parse Python, traverse its AST, execute
Torch, insert communication, or depend on Pyrefly's type inference solver.

## Run a complete tensor-parallel MLP

From the repository root:

```bash
cargo run -p pyrefly_static_analysis_spmd -- \
  static_analysis/spmd/examples/tp_mlp.py \
  static_analysis/spmd/examples/tp_mlp.json
```

The example has this type cycle on `tp`:

```text
x:I -> invariant_to_replicate -> R -> column matmul -> S(1)
    -> ReLU -> S(1) -> row matmul [explicit Partial intent] -> P
    -> all_reduce(dst=I) -> I
```

The sidecar is a **trusted assumption**, not a claim inferred from the Python
body. It describes real tensors' entry contracts, the ambient named compute
mesh, and the required return contract. On a sharded contraction it supplies an
explicit per-call Partial output contract rather than inventing a keyword that
real `torch.matmul` does not accept.

Try deleting the `invariant_to_replicate` entry (retain the source line or update
the sidecar line), changing the exit to `dst=s.R`, or applying `t.relu(p)` before
the reduction. All are rejected: the entry and wrong exit can have identical
forward values while differing in the required backward communication. The
`p.stride()` metadata query is accepted without discharging Partial.

The CLI prints deterministic located diagnostics and `contract-validated` or
`not-validated`, plus a separate incompleteness flag. Exit codes are 0 for a
validated supported contract, 1 for violations/incompleteness, and 2 for file,
argument or sidecar errors. Use explicit source-file paths; no checkout scan,
project discovery, Python import execution, or tensor execution is performed.

## Sidecar schema

The example JSON is complete and executable with the command above. Additional
input forms are `{"mesh": ["dp", "tp"]}` for a trusted mesh context argument
and `{"predicate": true}` for a trusted Boolean of unknown truth value with
no user-defined truthiness effects. A tensor has:

- `rank` (0 through 32), `grad` (`yes`, `no`, or `unknown`), and `axes` mapping
  names to `R`, `I`, `V`, `P`, or `S(i)`;
- optional `partition`, one entry per dimension: `null`, an axis string, or an
  ordered array of axes (outermost first). Those axes must have local type V,
  and an axis may occur at most once. Use V plus `partition` for several axes
  sharding one dimension; several `S(i)` entries cannot express their ordering.

`partial_outputs` entries identify a source `line` and its `axes`. Exactly one
call/binary operation must start on that line in the target region; multiple
calls on one line are rejected rather than guessed. The Rust API accepts stable
`OpId`s directly. Sidecars reject unknown fields, invalid placements, and
ambiguous target functions. Function default expressions are analyzed by the
shared frontend, but parameter defaults are not filled into entry contracts:
provide every used parameter explicitly.

```python
import torch
import spmd_types as spmd


def nested(x, dp, inner_mesh):
    with spmd.set_current_mesh(inner_mesh):
        dp = torch.relu(dp)
    return torch.relu(x)
```

Give `inner_mesh` the mesh descriptor `{"mesh": ["dp"]}`, `dp` a dp tensor,
and `x` a tp tensor with ambient mesh `["tp"]`. The inner context overrides
and then restores the outer mesh; the engine also emits restoration on normal
early returns. Named axes are logical identities supplied by the integration:
this checker does **not** establish actual process-group orthogonality, size,
ranks, or cross-mesh equivalence.

## Supported rules

- Per-axis R/I/V/P gradient duality: R <-> P, I <-> I, V <-> V. Unknown
  gradient information is conservative, never treated as `requires_grad=False`.
- Ordinary **local** matmul/linear: R,V -> V and V,V -> V. P is not inferred
  simply because both inputs vary. Explicit Partial intent permits V -> P.
- Restricted global rank-two matmul (`mk,kn->mn`) and linear (`mk,nk->mn`):
  consistent shard dimensions/order, free-label shard propagation and explicit
  Partial intent for sharded contraction. Pointwise shard propagation is
  checked for ReLU, add and multiply; coincident output shards must have a
  consistent ordering supplied by their inputs, not an invented order. Every
  produced tensor is checked for representation invariants independently of the
  optional return contract; global V axes must all occur in its PartitionSpec.
  No tensor broadcasting is modeled.
- Addition P,P -> P; P,R and P,constant are rejected (an affine term would be
  summed once per rank). Multiply/matmul P,R -> P; P,P is rejected. ReLU(P)
  is rejected. Known no-grad R/I may adopt the other replicated flavor when
  needed; gradient-bearing I cannot mix with R/V/P.
- `invariant_to_replicate`, a conservative subset of `convert` (I->R,
  I/R->S(i), R->P and identity), and `reinterpret(V,P)`. Unbind-form
  convert, mixed V/S all-to-all signatures, expert-mode,
  unusual conversions and additional call options are unsupported.
- `all_reduce`, `all_gather`, `reduce_scatter`, `all_to_all`: validated source,
  destination, axis and shard dimension. Type-only `spmd_types.rules` reduction
  calls require explicit `src=V` when reducing V. Runtime reduction calls permit
  the reference API's convenience V reinterpret. Missing input facts are errors,
  never established by `src`.
- Negative shard dimensions normalize by rank. Local V all-gather adds a
  leading dimension; local V reduce-scatter unbinds dimension 0. S(i) forms
  preserve rank. Global collective variants require explicit S(i) whenever an
  endpoint is V. A single-axis collective may change only an innermost shard.
- Metadata attributes `shape`, `ndim`, `dtype`, `device`; methods `size`,
  `stride`, `dim`, `ndimension`, `storage_offset`; builtin `len` on nonscalars.
  They return metadata, not a newly replicated tensor. Unsupported methods
  cannot masquerade as metadata by a similar name.
- Renamed imports, imported function aliases and lexical shadowing use shared
  symbol/frame bookkeeping. `ExportProvider` can resolve re-exported functions;
  the CLI uses the face-value `SyntacticExports` provider and assumes stable
  Torch/SPMD library namespaces, not arbitrary monkey-patching.
- Named-mesh ambient context override/restoration and completeness checks on
  compute, not metadata. Branch joins preserve ambiguity; worklist convergence
  limits and reachable frontend gaps propagate to incompleteness.

## Honest boundaries

Validation means only that supported rules hold under the supplied entry/call
assumptions. It is **not** full Python safety, global-SPMD soundness, numerical
correctness, shape/dtype correctness, proof of equal R/I forward data, or exact
behavioral parity with a runtime checker. No backward graph is executed or
inferred; backward contracts follow from declared forward types.

Unknown calls/protocols, object or tensor mutation, unsupported contexts,
unsupported signatures (including Torch `out=`/inplace arguments), suspensions,
and reachable frontend gaps make the result incomplete. Unknown effects
permanently invalidate tensor knowledge for the activation, including aliases
and outer bindings; later annotations or metadata cannot turn it back into a
validated result. `assert_type` is intentionally unsupported as a fact setter:
use an explicitly trusted entry and a checked return contract instead.

Only direct module-level plain-function activations without variadic parameters
are checked. Calls to
user-defined helpers, custom autograd hooks, deferred/generator/async execution,
annotation overlays, tensor attributes on user objects, raw distributed APIs,
mesh folding/reinterpretation, reshape, batched matmul and general einsum are
not summarized. Module setup is analyzed separately; unsupported module effects
preclude function validation. Sidecar arguments override invocation inputs, not
lexical bindings; the Rust `outer` map is also an explicit trusted assumption.

## Public reference and validation

Reference: [meta-pytorch/spmd_types](https://github.com/meta-pytorch/spmd_types)
at commit `e5ebc6d840fa2399bcd10306664faf4a2b5feaf6`. The rules source is
`spmd_types/rules.py` (there is no `rules/source` directory at this revision).
Semantics were selected from `docs/{local_spmd_types,design,key_concepts,rules}.md`,
`spmd_types/rules.py`, the local inference source, and public
`tests/{checker,rules,global_spmd,collectives,types}_test.py` scenarios.

There is a deliberate scope distinction: public `docs/design.md` requests
explicit `out_partial_axes` intent for ordinary global operations, whereas
`tests/global_spmd_test.py::test_mm_contracted_sharded_dim` currently infers P
for `torch.mm`. Composable `rules.einsum` also derives P from its explicit
contraction equation. This spike chooses explicit Partial intent for ordinary
calls, while validating the rank-two descriptor rule. It also deliberately does
not copy the runtime rule helper's ability to establish missing input types
from a collective `src` assertion.

```bash
cargo test -p pyrefly_static_analysis_spmd
cargo clippy -p pyrefly_static_analysis_spmd --all-targets -- -D warnings
cargo fmt --check -p pyrefly_static_analysis_spmd
```

Tests include the TP MLP and both backward-only failures, mesh restoration,
renamed imports/shadowing, unknown-effect anti-fabrication, convergence/gaps,
collective contracts, JSON validation and the CLI. Pure Rust numerical
counterexamples distinguish partial addition from partial multiplication and
ReLU, without a Torch dependency. These are descriptor tests, not numerical
verification of arbitrary user kernels. Validation on other operating systems
must be reported separately; source fixtures and CLI paths use Rust path APIs.
