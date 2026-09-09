# Trusted computing base

Catena's bootstrap is not a proof-verified compiler. Its guarantees depend on
different components, and successful signatures, type checks, tests and artifact
comparisons establish different facts. The normative contract is in
[catena-research](https://github.com/pcharbon70/catena-research/blob/main/60-specification/trusted-computing-base/guarantees-assumptions-and-boundary-checks.md).

The reviewed [machine inventory](../priv/trust/inventory.json) assigns every
compiler/helper/generator source to a component and every guarantee to its
component dependencies, executable checks and residual trust. Its data inventory
pins ordinary package descriptions, Unicode inputs and generated tables. The
conformance profile exposes its identity and external assumptions through
`Catena.ConformanceInfo.document()["trusted_computing_base"]`.

| Guarantee family | Main enforcement | Residual dependencies |
| --- | --- | --- |
| Source/static acceptance | Decoders, inference, scope/type/evidence checks and core verifiers | Source interpretation and shared Type/Data/Row/Coverage helpers |
| Elaboration and execution correspondence | Typed core, lowering, exact artifacts, reference comparisons | Backend, OTP compiler, ERTS and correlated oracle defects |
| Lexical effects and lifetimes | Capability non-escape, affine resumptions, owned scope workers | Scheduler, OS cleanup and host adapters; no rollback/fairness theorem |
| Artifact/package identity | Canonical digests, exact rebuild, interfaces, lock replay | Serializers, hash implementation and uncompromised build inputs/host |
| Governance authenticity/admission | Ed25519, distinct-key thresholds, revocation and lifecycle | Key custody, explicit trust roots and OTP/native crypto; not semantic truth |
| Proof/law evidence | Typed evidence, bounded condition facts, derived checks and law suites | No general machine proof kernel for every prose theorem; evidence levels differ |
| Erasure and diagnostic provenance | Erased lowering and checked source sidecars | Lowering/serializer correctness and observability host |
| Foreign/native ingress | Complete codecs, owner-bound authority and native package admission | Trusted NIFs/drivers, approved executables, ERTS and OS |
| Standard operations | Ordinary packages, independent rational/Unicode vectors and laws | Generators, input tables, byte/numeric primitives and finite evidence scope |
| Reference/differential observations | Separate evaluators and bounded exploration | Shared semantic/value/codec helpers; agreement can miss common defects |
| Deployment limits | Source/carrier quotas, finite workers and mandatory cleanup | Fatal VM/OS exhaustion, scheduling and external resource availability |
| Maintained boundaries | Exact source/call/data inventory and fault witnesses | Scanner, syntax parsers, baseline review and CI host |

## What the verifiers establish

`TypedCore.Verifier` and `Kernel.Verifier` structurally recheck explicit core
without repeating the inference entry point. They still share type, data,
coverage and effect-row helpers with inference. They do not prove that the parser
preserved the user's intended text or that a backend translated every accepted
core correctly. The integrated theorem remains conditional on its separately
owned component/composition obligations.

A mutation test changes a core literal from 42 to 43: the changed core is still
well typed and its reference result changes. Another changes the generated
function to return 99. Raw OTP compilation accepts those valid Erlang forms, while
an exact artifact rebuilt from the original core rejects the substituted binary.
This demonstrates the artifact boundary's narrower benefit. If both build and
rebuild use the same defective backend, equality alone cannot discover the defect.

A valid signature over a false correctness claim authenticates that signed text;
it does not establish the claim. Trusted roots and policy decide whose evidence
is admitted, and evidence checking remains a separate dependency. Catena does not
contain a general executable proof kernel discharging every research theorem.

## Maintain the inventory

Run `mix run scripts/check_trust_inventory.exs` from the repository root. The same
audit runs in `test/catena/trust_boundary_test.exs` alongside mutation tests.

The scanner parses Elixir and Python without executing the scanned source. It
records normalized remote/dynamic calls, relevant local runtime calls,
alias/import/macro dependencies and generated remote-call forms; Python records
all calls and imports. Per-source target counts, multiplicity and expression
fingerprints are compared with the reviewed baseline. New/removed source paths,
changed calls and changed generated data fail the gate.

When a change intentionally alters the boundary, inspect each reported source,
its caller authority, relevant guarantee dependencies and tests. Update the
source's component assignment and exact call/data evidence in the same reviewed
change; recompute the inventory's canonical digest. The audit command never
rewrites or approves its own baseline. New guarantees need named checks and
residual trust. Changing a branch guard without changing call syntax can evade
this gate: a test records that limitation explicitly. Source review and semantic
checks remain necessary.

Scan coverage includes files in `lib`, `scripts`, `src`, `c_src` and `config`,
Python helpers under `priv`, and `mix.exs`. Elixir/Python sources are parsed;
other source formats fail closed until explicitly inventoried with an admitted
inspection method. Package/Unicode data under `priv` is separately hashed,
excluding the self-describing `priv/trust` profile. Whole `mix.exs` and any
`mix.lock` bytes are also hashed so a literal dependency change cannot hide
behind unchanged call syntax. Tests and docs are not
production sources; their review and execution environment remain trusted. Each
parsed source is limited to 2,000,000 bytes after reading; this is not a promise
that a hostile giant file cannot allocate memory before refusal. The gate is a
development check over a trusted checkout, not a hostile-build-host defense.

## Host boundaries

`OTP.Compiler` owns production BEAM compilation. The native adapter additionally
loads its explicitly admitted signed native package. Call wrappers and adapters
execute trusted host work only through their declared contracts. The inventory
lists these routes rather than claiming that any dynamic dispatch is harmless.

NIFs can crash or corrupt the VM; a BEAM worker timeout cannot isolate that.
Python helpers, process groups and sockets depend on their interpreter and OS.
Approved programs that detach descendants leave the declared process-group
boundary. Missing cleanup confirmation is reported, not converted into a rollback
claim. Generators and Unicode data are semantic inputs, not trustworthy merely
because the derived table has a digest. Compromised compiler, CI host, toolchain,
OS or trust-root provisioning remain explicit residual trust.

No general package-registry client is admitted in this bootstrap. P130 owns that
future acquisition boundary; governance trust roots do not establish registry security.
