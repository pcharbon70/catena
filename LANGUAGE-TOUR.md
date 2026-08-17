# Catena Language Tour

Catena is a strict functional language for the BEAM VM. Its design combines
type inference, variant data, exhaustive pattern matching, approachable
composition operations, and explicit effect handling. The aim is to give
ordinary programmers useful guarantees without requiring category-theory
terminology as a prerequisite.

This tour is the best starting point for understanding the language model. It
is not a replacement for the specification.

## Before you start

Catena is currently an executable language-design prototype, not yet a
source-language distribution:

- the compiler accepts retained versioned JSON AST and a separate exact 0.1.8
  semantic-kernel S-expression for compilation, while 0.1.9 validates source
  bytes and locations and 0.1.10 validates standalone names without yet
  tokenizing or parsing complete source files;
- snippets in this tour are illustrative notation unless a linked
  specification says that a particular form is fixed;
- public parser punctuation, layout, and several ordinary language facilities
  are still open design work; and
- the compiler repository is executable evidence, while
  [catena-research](https://github.com/pcharbon70/catena-research) contains the
  authoritative language specification, rationale, evidence, and open
  questions.

If an example here and a normative chapter disagree, follow the normative
chapter.

## The language in one view

Catena's initial model has nine connected parts:

1. **Functions and inference** — ordinary code receives principal
   Hindley–Milner types where possible; advanced features require explicit
   annotations.
2. **Data and patterns** — nominal variant types are consumed through
   ordered, exhaustively checked pattern matches.
3. **Clause conditions** — a deliberately small, pure, total condition
   language can refine clause selection without hiding arbitrary execution.
4. **Shared behavior** — coherent traits expose operations such as `map`,
   `combine`, and `and_then` under names that describe what programmers do.
5. **Effects and handlers** — `uses`, `request`, and `handle` make nonlocal
   behavior explicit and select handlers through lexical capability identity.
6. **Typed actors and formal semantics** — a send-only `Process M` handle,
   named entries, selective receive, explicit traps, and a small-step machine
   make local concurrency and failure precise.
7. **Specifications and governance** — optional typed rules and exact examples
   become strict, artifact-bound package gates once a project adopts them.
8. **Editions and previews** — packages pin one exact language contract;
   named experimental features cannot silently appear through a compiler
   update or dependency.
9. **BEAM execution** — verified typed core lowers to Erlang Abstract Format,
   which OTP 29 compiles into `.beam` modules.

The language is expression-oriented, strict, and immutable by default.

## Functions and inferred types

The principal core includes literals, variables, functions, application,
polymorphic local bindings, tuples, and signatures. In illustrative syntax:

```catena
identity(value) = value

pair_with(value) =
  let pair = fn other -> (value, other)
  pair
```

The compiler can infer that `identity` works for any value and can instantiate
one polymorphic local definition at different types. Public definitions must
declare signatures so module boundaries remain stable and separately
checkable:

```catena
identity : A -> A
```

Inference remains conservative around effects, recursion, GADTs, and
higher-rank types. Those boundaries use annotations rather than guessing.

Read the
[normative type-system specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/type-system)
for the exact type language and checking profiles.

## Variant data and patterns

Types have nominal identity: two declarations with the same shape are still
different types. A declaration can expose its variants or keep them abstract
across a module boundary. The compiler specification also uses the terms
algebraic datatype and constructor; `variant type`, `variant`, and `payload`
are the reader-facing vocabulary.

The canonical datatype notation is:

```catena
type Option A =
  | None
  | Some A

type DeliveryStatus =
  | Queued
  | InTransit { tracking_id: TrackingId }
```

Construction qualifies the variant:

```catena
Option.Some(7)
DeliveryStatus.InTransit { tracking_id: id }
```

Pattern matching is ordered, but the compiler also checks usefulness and
exhaustiveness. Missing alternatives receive concrete witnesses such as
`Option.Some(_)`; unreachable alternatives are rejected rather than silently
retained.

The initial pattern language includes wildcards, binders, integer and Boolean
literals, tuples, variants, `as` patterns, and `or` patterns. It does not
yet include list, map, binary, view, or pattern-synonym forms.

Read the
[normative data-and-pattern specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/data-and-patterns)
for construction, matching, GADTs, abstraction, and representation rules.

## Clause conditions

A structural pattern may be followed by a condition:

```text
pattern when condition -> body
```

Conditions are not arbitrary Boolean-returning functions. The initial
condition language is a closed, auditable fragment containing Boolean logic,
exact equality, integer comparison, total integer arithmetic, immutable
variables, and direct calls to verified nonrecursive condition predicates.
There is no truthiness conversion.

This restriction lets the compiler preserve source-order selection, evaluate
each condition exactly once, use condition facts conservatively during
coverage checking, and lower eligible conditions to native Erlang guards.
Ordinary calls, effects, recursion, partial arithmetic, and trait dispatch are
rejected inside this fragment.

Read the
[normative clause-condition specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/clause-conditions)
for the exact operation set and fallthrough behavior.

## Shared behavior without mathematical prerequisites

Catena uses coherent traits to express reusable behavior. The public
vocabulary describes programming operations; formal mathematical names are
reference metadata, not required source vocabulary.

| Programming task | Initial capabilities | Representative operations |
| --- | --- | --- |
| Compare values | `Equatable`, `Orderable` | `equals`, `compare` |
| Combine values | `Combiner`, `EmptyCombiner` | `combine`, `empty` |
| Transform containers | `Mapper`, `TwoSlotMapper` | `map`, `map_both` |
| Combine independent contexts | `MultiMapper`, `ValueEmbedder` | `map2`, `from_value` |
| Sequence dependent work | `Chainable`, `Workflow` | `and_then` |
| Reduce and collect | `Reducible`, `CollectingMapper` | `summarize`, `collect_map` |
| Compose transformations | `Composable`, `IdentityComposer`, `TransformRouter` | `compose`, `identity`, `from_transform` |
| Read contextual structures | `ContextualMapper`, `FocusReader` | `map_with_context`, `read_focus` |

Implementations are globally coherent: import order and local preference
cannot silently change which behavior is selected. The semantic ledger calls
their records instances and evidence. That data is checked during compilation,
specialized to direct calls, and erased from the resulting BEAM code.
Guarantees are explicit promises or test evidence; they do not authorize
silent compiler rewrites.

Read the
[normative trait and categorical-operation specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/traits-and-categorical-operations)
for all seventeen capabilities, their relationships, and operational rules.

## Effects and handlers

An effect declares a nominal family of typed requests:

```text
effect Prompt {
  ask(message: Text, validate: Text -> Bool) -> Text
}
```

Function signatures use `uses` to expose requests they may leave to a caller:

```text
ask_name : Unit -> Text uses prompt: Prompt
```

A request either names its lexical capability or relies on a unique compatible
capability:

```text
request prompt.ask("Name?", nonempty)
request ask("Name?", nonempty)
```

If two compatible capabilities are visible, the unqualified form is an error;
runtime nesting does not resolve the ambiguity. A named handler is applied
around one expression:

```text
handle ask_name() using TestPrompt(responses) as prompt
```

Resuming reinstalls the same handler around the remaining computation; the
semantic ledger calls this deep handling. A clause may discard its resumption
or invoke it once, using the dedicated form:

```text
resume continuation with reply
```

Resumptions cannot escape, be stored, or be called twice. Static checking is
backed by a runtime consumed token. Cleanup, cancellation, shallow handlers,
multi-shot control, and scoped or higher-order operations are deliberately not
implied by this first-order feature.

Read the
[effect and handler specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/effects-and-handlers)
for capability identity, effect rows, evaluation order, and CPS lowering.

## Specifications and governance

Catena 0.1.6 defines an optional assurance layer without making organizational
governance a prerequisite for ordinary programs. The proposed source
vocabulary says what a boundary `needs`, what an implementation `promises`,
which exact `example` should hold, and what broader `property` or `always`
statement may be investigated later. The implemented 0.1.6 spine is smaller: a
`rule` names a typed, effect-free verification definition and an exact
`example` invokes it under a fixed deterministic budget. Compiler evidence,
signed external attestations, and explicit assumptions retain different
meanings.

Once a package names a governance bundle, every matching package, module,
action, output, interface, and profile policy is combined additively. A local
`build` may be permitted while `publish` or `activate` remains blocked.
Approvals and lifecycle transitions bind exact claim, policy, evidence, and
artifact digests; signatures prove who signed canonical bytes, not that the
statement is mathematically true.

The governance words remain action-oriented: an `owner` defines policy,
authorized people `approve` an exact proposal, and the gate distinguishes
`build`, `publish`, `activate`, and later lifecycle replacement. The canonical
protocol represents those words with principals, roles, signatures, and
immutable transitions.

All 0.1.6 specification and governance material is build-time only. Verification
definitions disappear before Erlang Abstract Format lowering, and the package
sidecar records what was checked and which exact BEAM/interface bytes were
admitted. The compiler emits a canonical signing payload for an external
Ed25519 signer and never reads a private key.

This is a normative semantic JSON contract, not final Catena source
punctuation. Read the
[normative specification and governance chapters](https://github.com/pcharbon70/catena-research/tree/main/60-specification/specifications-and-governance)
for the exact formats, policy algebra, lifecycle, diagnostics, and promotion
gate.

## Editions, exact revisions, and previews

Catena separates an edition such as `0.1` from an exact language revision such
as `0.1.7`. The edition is a compatibility track; the exact revision selects
one cumulative set of language rules. Artifact formats and compiler releases
are separate identities.

A new package records its choice explicitly:

```json
{
  "version": "0.1.7",
  "edition": "0.1",
  "language_revision": "0.1.7",
  "previews": []
}
```

Dependencies retain their own selections and interoperate through checked
semantic interfaces. A preview that becomes part of an exported obligation
must be named in that interface, and a consumer that did not enable it is
rejected. Selection affects compile-time checking, metadata, and artifact
identity; generated functions never dispatch on edition or preview flags.

Catena 0.1.7 intentionally publishes no actual preview feature. It defines
the lifecycle—preview to stable or withdrawn, then stable to deprecated to
removed—without turning private compiler switches into public language
features.

The 0.1.7 edition and lifecycle chapters are normative, with their immutable
compiler evidence recorded in the research archive. Read
[Editions, Revisions, and Previews](guides/language/editions-and-previews.md)
for the user workflow and the
[normative specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/editions-and-feature-lifecycle)
for the exact contract.

## The source-text boundary

Normative revision 0.1.9 establishes the input shared by future ergonomic
syntax: strict UTF-8, no leading byte-order mark, LF and CRLF as one logical
newline, no whole-file Unicode normalization, and one original-byte span per
logical scalar. `Catena.decode_source_text/2` exposes that stream to a future
lexer, while `catena check-source-text` validates it without creating an
interface or BEAM file.

Passing this boundary does not mean that a file is a Catena program. Revision
0.1.9 deliberately defines no tokens, comments, literals, surface grammar, or
file-to-module rule. Revision 0.1.10 builds on it with Unicode 17 XID names,
required NFC spelling, case-sensitive role-neutral identity, backtick keyword
escapes, dot qualification, secure script checks, and deny-able confusable
warnings. `Catena.parse_identifier/2`, `Catena.parse_qualified_name/2`, and
`catena check-identifiers` expose that standalone boundary. Read the
[Source Text guide](guides/language/source-text.md) for the exact implemented
envelope and the [Identifiers guide](guides/language/identifiers.md) for names.

## The executable formal kernel and typed actors

Normative revision 0.1.8 integrates the executable portions of the earlier
semantic slices with structural records and variants in one exact
S-expression module. It adds named local process entries and a send-only
`Process M` handle. Messages must be closed first-order values; ordinary
effects must be handled before a process entry returns.

Send is asynchronous and returns Unit. A dead-target send drops its message.
Messages from one sender preserve order; receive removes the oldest matching
message and leaves skipped messages in place. Scheduling across senders is
nondeterministic and has no fairness promise. Normal return and explicit
`trap reason` terminate only the current process and discard its mailbox.

The compiler preserves source spans, independently rechecks the unified typed
core, and exposes a CEK-style small-step machine plus bounded schedule
exploration. The production path lowers the same verified meaning through
fixed maps/tagged tuples and OTP 29 Abstract Format. See the
[Formal Semantic Kernel guide](guides/language/formal-semantic-kernel.md).

The explicitly authorized C010 immutable compiler identity and post-commit
evidence are recorded in the research archive.

## From Catena to BEAM

The compiler path is:

```text
source bytes 0.1.9 → logical Unicode stream plus original-byte spans
                       ↓
standalone names 0.1.10 → validated identifiers and qualified names
                           (stops before the future whole-source lexer/parser)

retained JSON AST 0.1.1–0.1.7  OR  exact kernel S-expression 0.1.8
        ↓
inference and elaboration
        ↓
typed rules and bounded build-time evidence
        ↓
verified typed core
        ↓
pure direct lowering or effect-directed CPS
        ↓
Erlang Abstract Format
        ↓  compile:noenv_forms/2
BEAM module
        ↓
artifact-bound assurance sidecar (not runtime state)
```

The bootstrap compiler is written in Elixir, but Elixir is not Catena's target
semantics. Catena targets only BEAM. It does not generate Core Erlang, BEAM
assembly, or `.beam` binaries directly; OTP 29 performs the supported final
compilation step.

## Try the executable model

Install the pinned Erlang/OTP and Elixir versions, run the conformance suite,
and build the command-line tool:

```bash
asdf install
asdf exec mix test
asdf exec mix escript.build
./catena language-info
```

Validate the source envelope independently of grammar:

```bash
./catena check-source-text guides/language/source-text.md
```

The deterministic result reports the selected revision and byte, logical-
scalar, and newline counts. It creates no output files.

Validate standalone names independently of tokenization and resolution:

```bash
./catena check-identifiers alpha Option.Some '`type`'
```

The most approachable durable input is the
[`Option` JSON-AST fixture](test/fixtures/c002-option.catena.json). Validate it
without producing files:

```bash
./catena check-ir test/fixtures/c002-option.catena.json
```

To inspect generated artifacts without writing into the repository:

```bash
tour_directory="$(mktemp -d)"
cp test/fixtures/c002-option.catena.json "$tour_directory/"
./catena compile-ir "$tour_directory/c002-option.catena.json"
ls "$tour_directory"
```

The compiler writes an OTP-generated `.beam` and a deterministic
`.cati.json` module interface beside the input. The JSON is an explicit
bootstrap representation, not a preview of intended Catena source syntax.

The normative kernel fixture can be checked directly:

```bash
./catena check-kernel test/fixtures/c010-kernel.catena
```

## Explore executable features in order

The tests are executable conformance evidence. They construct JSON programs in
Elixir, so read them for semantics and diagnostics rather than surface syntax:

1. [`compiler_test.exs`](test/catena/compiler_test.exs) and
   [`type_conformance_test.exs`](test/catena/type_conformance_test.exs) —
   functions, inference, typed core, and BEAM compilation.
2. [`c002_data_test.exs`](test/catena/c002_data_test.exs) — algebraic data,
   patterns, coverage, interfaces, and representation independence.
3. [`c003_clause_condition_test.exs`](test/catena/c003_clause_condition_test.exs)
   — safe conditions, ordered clauses, coverage facts, and guard lowering.
4. [`c004_categorical_test.exs`](test/catena/c004_categorical_test.exs) —
   traits, derivation, law evidence, specialization, and erased direct calls.
5. [`c005_effects_test.exs`](test/catena/c005_effects_test.exs) — lexical
   capabilities, deep handlers, affine resumptions, and reference/BEAM traces.
6. [`c006_specification_governance_test.exs`](test/catena/c006_specification_governance_test.exs)
   — typed rules, exact examples, canonical signatures, trust rotation,
   additive policy, lifecycle replay, package staging, artifact binding, and
   complete BEAM erasure.
7. [`c008_editions_lifecycle_test.exs`](test/catena/c008_editions_lifecycle_test.exs)
   — exact pins, lifecycle states, migration diagnostics, selection-bound
   interfaces and artifacts, versioned signatures, policy constraints, and
   absence of runtime edition dispatch.
8. [`c010_formal_semantic_kernel_test.exs`](test/catena/c010_formal_semantic_kernel_test.exs)
   — exact parsing, rows, nominal data, traits, handlers, independent core
   verification, typed actors, schedule exploration, traps, interfaces, and
   reference/BEAM agreement.
9. [`c013_source_text_test.exs`](test/catena/c013_source_text_test.exs) — strict
   UTF-8, BOM and newline failures, normalization preservation, original-byte
   spans, source-only version separation, and deterministic CLI validation.

## Continue in catena-research

Use these documents as the reading path after this tour:

1. [Catena Language Overview](https://github.com/pcharbon70/catena-research/blob/main/language-overview.md)
   — architecture, layers, and the larger language direction.
2. [Language Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification)
   — the versioned normative rules implemented by this compiler.
3. [Research Home Map](https://github.com/pcharbon70/catena-research/blob/main/10-maps/home.md)
   — curated routes through the supporting research and open inquiries.
4. [Language Completeness Checklist](https://github.com/pcharbon70/catena-research/blob/main/00-inbox/language-specification-completeness-checklist.md)
   — what is complete, partial, deferred, or still missing.

The research archive is intentionally broader than the executable compiler.
It records not only selected rules, but also their sources, rationale,
limitations, alternatives, and unresolved questions.

## Current boundary

Catena does not yet provide an ergonomic source lexer or parser, formatter, REPL,
end-user package manager, complete standard library, distributed or supervised
concurrency, resource-scope semantics, exception boundary, foreign-term
validation, or finalized surface grammar. The compiler is valuable today as a
deterministic executable model of the completed specification slices—not yet
as a general-purpose language toolchain.

Continue with the [Catena Guides](guides/README.md) for detailed user tasks,
governance operations, BEAM boundaries, and compiler development.
