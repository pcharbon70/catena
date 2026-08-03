# Diagnostics and Testing

Catena's tests are executable conformance evidence, not only regression checks.
They compare independent semantic paths, inspect generated artifacts, execute
BEAM modules, and exercise adversarial governance inputs.

## Diagnostic contract

Every public failure is a `Catena.Diagnostic` with:

```elixir
%Catena.Diagnostic{
  id: "M001",
  message: "match is not exhaustive",
  path: "$.definitions[0].body",
  details: %{witness: "Option.Some(_)"}
}
```

- `id` is the stable machine category.
- `message` explains the immediate problem.
- `path` identifies a JSON-AST, interface, package, or governance location.
- `details` carries structured evidence such as witnesses, expected/observed
  values, threshold counts, signing payloads, or backend errors.

The CLI wraps this as JSON on standard error. Tests should normally assert the
identifier and the meaning-sensitive details, not an entire prose message that
may improve without changing the contract.

## Diagnostic ownership

| Family | Owning concern |
| --- | --- |
| `T...` | types, schemes, inference, signatures, generalization |
| `K...` | kinds and type-constructor application |
| `A...` | nominal data declarations, abstraction, constructors |
| `M...` | patterns, matching, coverage, witnesses |
| `CND...` | clause conditions and safe lowering |
| `TRT...` | traits, instances, laws, derivation, specialization |
| `EFX...` / `CPS...` | effects, capabilities, handlers, resumptions, CPS boundaries |
| `SPC...` | specification shape, subjects, checker validity, examples |
| `EVD...` | evidence admission and checker outcomes |
| `GOV...` | policy, actors, signatures, lifecycle, trust roots |
| `ART...` | artifact paths, collisions, sizes, hashes, substitution |
| `ERS...` | verification or governance material escaping erasure |
| `LNK...` | package manifest and specialization/linking |
| `B...` | OTP rejecting generated Erlang Abstract Format |
| `I...` | inference-independent core invariant failure |

Use the exact identifiers reserved by the applicable normative diagnostics
chapter. Do not reuse an existing ID for a different repair action merely
because it has the same prefix.

## A useful diagnostic answers four questions

1. What source or governed action is blocked?
2. Which type, claim, evidence, authority, or artifact failed?
3. Why does the language or active policy require it?
4. What concrete change can repair it?

For governance thresholds, report valid, invalid, revoked, and duplicate
counts separately. For match coverage, report a deterministic witness when
the witness language can express one. For backend failure, preserve OTP errors
and warnings in structured details.

## Test layout

| File | Primary coverage |
| --- | --- |
| `compiler_test.exs` | principal core, signatures, deterministic compile/load/execute |
| `type_conformance_test.exs` | unification, rows, traits, advanced typing, verifier rejection |
| `ast_decoder_test.exs` | strict versioned protocol decoding |
| `c002_data_test.exs` | nominal ADTs, patterns, coverage, interfaces, layouts, folds |
| `c003_clause_condition_test.exs` | safe conditions, coverage facts, native/ordinary lowering, receive harness |
| `c004_categorical_test.exs` | traits, coherence, laws, derivation, templates, specialization, erasure |
| `c005_effects_test.exs` | lexical capabilities, deep handlers, affine resume, CPS, reference traces |
| `c006_specification_governance_test.exs` | rules, evidence, JCS, Ed25519, policy oracle, lifecycle, artifact transactions |
| `resumption_token_test.exs` | dynamic one-use continuation defense |

Tests construct JSON programs directly in Elixir. That is intentional: they
test the semantic frontend without pretending that source punctuation has been
selected.

## Run the suite

```bash
asdf exec mix format --check-formatted
asdf exec mix clean
asdf exec mix compile --warnings-as-errors
asdf exec mix test
asdf exec mix escript.build
git diff --check
```

Run one slice while developing:

```bash
asdf exec mix test test/catena/c005_effects_test.exs --trace
```

Always run the complete suite before handoff. A new slice must leave older
conformance cases green unless a newer normative version explicitly replaces
their behavior.

## Layers of evidence

### Positive and negative semantic tests

Every accepted form needs a positive type/elaboration case. Every invalid
boundary needs a case asserting the stable diagnostic. Include near misses:
wrong arity, wrong identity, wrong scope, ambiguous evidence, and exhausted
budgets often expose more bugs than wholly malformed input.

### Independent core verification

Construct or mutate typed core so the verifier sees evidence inference would
not normally produce. Confirm that forged constructor IDs, coverage markers,
trait derivations, effect rows, handler tables, and affine-use records are
rejected.

A test that only asks inference to produce valid core does not test the
verifier's distrust boundary.

### Reference-versus-BEAM comparison

For data layouts, conditions, and effects, compare independently structured
semantics with generated BEAM:

- uniform versus compact representation;
- native versus ordinary condition lowering;
- free-request effect evaluator versus CPS BEAM; and
- values plus traces when order, forwarding, resume, or abort is observable.

Do not implement the reference path by calling the production dispatch or
policy helper. Shared implementation would let one bug make both answers agree.

### Determinism and byte identity

Compile identical input twice and compare binaries or canonical artifacts.
When a rule states that compile-time evidence is erased, compare the complete
BEAM bytes with and without that evidence.

Determinism tests should bind source path, frontend/specification version,
layout, lowering mode, interface digests, and compiler identity. A test that
quietly changes one of these inputs does not establish byte identity.

### Artifact inspection

Inspect generated Erlang forms, BEAM exports/chunks, `.cati.json`, companion
modules, and assurance manifests. Assert both required presence and forbidden
retention.

For 0.6 erasure, check that verification definitions, claims, evidence,
policies, signatures, keys, and assurance digests do not appear in any BEAM
chunk or runtime export.

### Adversarial governance tests

Security-sensitive tests include:

- duplicate JSON names and noncanonical bytes;
- unsafe integers, floats, negative zero, and invalid Unicode;
- published RFC Ed25519 vectors and signature domain substitution;
- duplicate, revoked, wrong-role, and insufficient signers;
- delegation scope and sequence escape;
- evidence, claim, policy, approval, artifact, and root substitution;
- skipped, reordered, backward, terminal, and broken lifecycle chains;
- normal rotation and predeclared recovery;
- path traversal, symlink escape, collision, and input overwrite; and
- failed-gate absence of final outputs.

An adversarial test should prove why the input is otherwise plausible, then
change one security-relevant field at a time.

## Test runtime code safely

Tests that load generated modules use `async: false` because BEAM module names
are global to the VM. Always purge and delete a module after execution:

```elixir
assert {:module, module} = :code.load_binary(module, ~c"fixture.beam", binary)
assert apply(module, :main, []) == expected
:code.purge(module)
:code.delete(module)
```

Use unique temporary directories for artifact tests and register cleanup with
`on_exit`. Keep private-key fixtures synthetic and ephemeral.

## Test deterministic budgets

Coverage, condition normalization, specialization, specification evaluation,
and policy evaluation use explicit step budgets. Test both the greatest
supported useful case and the first rejected over-budget case.

Budget exhaustion must retain its own diagnostic; it is not evidence that a
match is non-exhaustive, a rule is false, or a policy explicitly denied.

## Adding a diagnostic

1. Identify the normative failure and repair action.
2. Reuse an existing ID only when both meaning and repair are the same.
3. Attach the most precise stable path available.
4. Put structured observations in `details`.
5. Add a negative source/protocol test.
6. Add a forged-core or adversarial test when a downstream trust boundary is
   involved.
7. Document the ID in the applicable normative diagnostics chapter and guide.

## Pre-handoff checklist

- formatting passes;
- compilation emits no warnings;
- focused tests pass;
- the complete suite passes;
- generated outputs are deterministic where required;
- the sole OTP form-compiler call remains sole;
- `git diff --check` passes;
- the worktree contains no generated escript or temporary artifacts; and
- documentation links point to the current normative status.

Continue with [Adding a Language Feature](adding-a-language-feature.md).
