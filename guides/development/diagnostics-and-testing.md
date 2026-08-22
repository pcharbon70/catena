# Diagnostics and Testing

Catena's tests are executable conformance evidence, not only regression checks.
They compare independent semantic paths, inspect generated artifacts, execute
BEAM modules, and exercise adversarial governance inputs.

Tests are not language authority. The research repository's
[Specification Authority](https://github.com/pcharbon70/catena-research/blob/main/SPECIFICATION-AUTHORITY.md)
requires each disputed or conformance-sensitive expectation to cite the
applicable normative document and heading.
The companion
[Conformance Vocabulary](https://github.com/pcharbon70/catena-research/blob/main/CONFORMANCE-VOCABULARY.md)
defines invalid input, implementation limits, explicit traps, and bounded
presentation variation. The current implementation's choices and limits are
published in [CONFORMANCE.md](../../CONFORMANCE.md).

## Diagnostic contract

Every public failure is a `Catena.Diagnostic` with:

```elixir
%Catena.Diagnostic{
  id: "M001",
  message: "match is not exhaustive",
  path: "$.definitions[0].body",
  span: nil,
  severity: :error,
  details: %{witness: "Option.Some(_)"},
  fixes: []
}
```

- `id` is the stable machine category.
- `message` explains the immediate problem.
- `path` identifies a JSON-AST, interface, package, or governance location.
- `span` identifies a half-open original-byte/line/column range in exact 0.1.8
  kernel input or the 0.1.9 source envelope; retained JSON and protocol
  diagnostics may leave it absent.
- `severity` distinguishes a default error from a warning that policy may
  promote to failure without changing its ID.
- `details` carries structured evidence such as witnesses, expected/observed
  values, threshold counts, signing payloads, or backend errors.
- `fixes` carries ordered structured suggestions; C008 reports safe edits but
  never applies them.

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
| `EDN...` / `PRV...` / `DEP...` | edition selection, preview propagation, migration, deprecation |
| `SYN...` / `PRC...` | exact kernel syntax/limits and typed process boundaries |
| `SRC...` | source encoding, BOM, and newline-envelope failures |
| `IDN...` | identifier spelling, normalization, security, qualification, and confusables |
| `LAY...` | layout whitespace, separator, and continuation failures |
| `CMT...` / `DOC...` | comment and documentation scanning/attachment failures |
| `LIT...` | atomic literal spelling, delimiter, and escape failures |
| `NUM...` | numeric literal meaning failures such as unrepresentable decimals |
| `OPR...` | reserved operator spellings and invalid operator-expression forms |
| `FIL...` | file-unit failures: extension, module multiplicity, name spelling, basename mismatch, and generated markers |
| `NSP...` | namespace failures: duplicates, spelling-class violations, unbound or ambiguous references, and qualification depth |
| `IMP...`/`EXP...` | import/export failures: invalid imports, unknown modules, and undeclared exports; `IMP001` unused-import warnings |
| `LIM...` | portable source and generated-artifact limits |
| `B...` | OTP rejecting generated Erlang Abstract Format |
| `I...` | inference-independent core invariant failure |

Use the exact identifiers reserved by the applicable normative diagnostics
chapter. Do not reuse an existing ID for a different repair action merely
because it has the same prefix.

## Explain failures in the public vocabulary

The diagnostic ID belongs to the compiler contract; the default message
belongs to the programmer's task. Lead with `variant`, `match`, `condition`,
`implementation`, `requirement`, `effect`, `request`, `promise`, `evidence`,
or `approval` when that is the concept the source author used. Put terms such
as constructor provenance, evidence dictionary, effect row, semantic digest,
and transition payload in optional technical details.

For example:

```text
`map2` needs `Report` to provide `MultiMapper`

You are combining two independent `Report` values, but `Report` only has an
implementation for changing one existing result with `map`.

Provide a `MultiMapper` implementation, or use `and_then` if the second report
depends on the first result.

Technical details: no coherent instance evidence for MultiMapper Report.
```

The headline names the operation and requirement. The explanation names the
dependency choice. The final line preserves the exact internal fact without
making it prerequisite vocabulary.

Formal labels such as “invalid,” “implementation limit,” and “explicit trap”
belong in specifications, contributor material, and optional technical detail.
Routine diagnostics should continue to say what the programmer attempted, why
it failed, and how to repair it in approachable Catena vocabulary.

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
| `language_version_test.exs` | canonical slice registry, hard-cutover rejection, format identities, and signature domains |
| `compiler_test.exs` | principal core, signatures, deterministic compile/load/execute |
| `type_conformance_test.exs` | unification, rows, traits, advanced typing, verifier rejection |
| `ast_decoder_test.exs` | strict versioned protocol decoding |
| `c002_data_test.exs` | nominal ADTs, patterns, coverage, interfaces, layouts, folds |
| `c003_clause_condition_test.exs` | safe conditions, coverage facts, native/ordinary lowering, receive harness |
| `c004_categorical_test.exs` | traits, coherence, laws, derivation, templates, specialization, erasure |
| `c005_effects_test.exs` | lexical capabilities, deep handlers, affine resume, CPS, reference traces |
| `c006_specification_governance_test.exs` | rules, evidence, JCS, Ed25519, policy oracle, lifecycle, artifact transactions |
| `c008_editions_lifecycle_test.exs` | exact pins, feature lifecycle, migration diagnostics, selection binding, versioned signatures |
| `c010_formal_semantic_kernel_test.exs` | exact parsing/spans, unified judgments, rows, actors, schedule exploration, interfaces, forged evidence, reference/BEAM agreement |
| `c012_implementation_limits_test.exs` | machine-readable disclosure, portable boundary pairs, evidence-bound classification, and mailbox-capacity policy |
| `c012_traceability_coverage_test.exs` | complete `IL-OBL-*` obligation coverage and explicit architectural allowlists |
| `c013_source_text_test.exs` | UTF-8, BOM, newline, normalization, source-span, revision, and CLI behavior |
| `c013_traceability_coverage_test.exs` | complete `ST-OBL-*` source-text obligation coverage |
| `c014_identifiers_test.exs` | Unicode identifiers, NFC, security, keywords, qualification, confusables, and CLI behavior |
| `c014_traceability_coverage_test.exs` | complete `ID-OBL-*` identifier obligation coverage |
| `c015_whitespace_layout_test.exs` | layout whitespace, indentation invariance, separators, continuation, frames, spans, and diagnostics |
| `c015_traceability_coverage_test.exs` | complete `LY-OBL-*` whitespace-and-layout obligation coverage |
| `c016_comments_documentation_test.exs` | comment scanning, nesting, spans, layout integration, documentation normalization, attachment, and diagnostics |
| `c017_literal_grammar_test.exs` | atomic literal forms, decoding, provenance, line ownership, exact limits, exclusions, and diagnostics |
| `c017_traceability_coverage_test.exs` | complete `LT-OBL-*` literal obligation coverage |
| `c018_numeric_literal_semantics_test.exs` | numeric domains, monomorphic typing, correct rounding, subnormals, overflow invalidity, negation, and the `LIM005` boundary |
| `c018_traceability_coverage_test.exs` | complete `NM-OBL-*` numeric obligation coverage |
| `c019_operators_test.exs` | closed inventory, maximal munch, capabilities, frames, ladder, chains, pipes, exclusions, and determinism |
| `c019_traceability_coverage_test.exs` | complete `OP-OBL-*` operator obligation coverage |
| `c020_file_unit_test.exs` | file units, extension, multiplicity, spelling, basename match, markers, and diagnostics |
| `c020_traceability_coverage_test.exs` | complete `FU-OBL-*` file-unit obligation coverage |
| `c021_namespaces_test.exs` | namespace categories, spelling classes, duplicates, shadowing, type variables, precedence, and ambiguity |
| `c021_traceability_coverage_test.exs` | complete `NS-OBL-*` namespace obligation coverage |
| `c022_import_exports_test.exs` | export events and transparency, admission lists, qualified-only imports, validation, exclusions, and unused-import warnings |
| `c022_traceability_coverage_test.exs` | complete `IM-OBL-*` import/export obligation coverage |
| `c016_traceability_coverage_test.exs` | complete `CM-OBL-*` comment and documentation obligation coverage |
| `resumption_token_test.exs` | dynamic one-use continuation defense |

Retained-slice tests construct JSON programs directly in Elixir. The C010
corpus instead parses exact kernel S-expressions; that punctuation is fixed for
the semantic kernel but is not presented as the ergonomic source frontend.

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
asdf exec mix test test/catena/c010_formal_semantic_kernel_test.exs --trace
asdf exec mix test test/catena/c013_source_text_test.exs --trace
```

Always run the complete suite before handoff. A new slice must leave older
conformance cases green unless applicable normative text explicitly replaces
their behavior. A version number alone does not establish that replacement.

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
- kernel values, explicit traps, proper tail calls, process traces, selective
  receive, and bounded schedule outcome sets.

Do not implement the reference path by calling the production dispatch or
policy helper. Shared implementation would let one bug make both answers agree.

Reference and production disagreement blocks the affected conformance claim.
Compare both paths with the exact normative heading; neither the reference nor
the compiler is a fallback definition when the specification is silent.

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

For 0.1.6 erasure, check that verification definitions, claims, evidence,
policies, signatures, keys, and assurance digests do not appear in any BEAM
chunk or runtime export.

For 0.1.7 selection, inspect interface content, specialization keys, assurance
fields, approval payloads, and the standard BEAM compile-information chunk.
Then inspect Erlang Abstract Format to prove that edition and preview values do
not enter executable function bodies.

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

All active bounds belong in `Catena.ImplementationLimits`. Production checks,
diagnostic details, tests, and `catena conformance-info` consume that registry;
do not introduce a second hard-coded configuration value. A refusal diagnostic
reports its limit identity, portable minimum, configured value, observed value,
and unit.

Budget exhaustion must retain its own diagnostic; it is not evidence that a
match is non-exhaustive, a rule is false, or a policy explicitly denied.
Keep every active budget and its outcome synchronized with the published
[compiler conformance profile](../../CONFORMANCE.md).

## Adding a diagnostic

1. Identify and cite the normative document, heading, failure, and repair
   action.
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
- the worktree contains no generated escript or temporary artifacts;
- documentation links point to the current normative status; and
- `CONFORMANCE.md` reflects every affected choice, deviation, and limit.

Continue with [Adding a Language Feature](adding-a-language-feature.md).
