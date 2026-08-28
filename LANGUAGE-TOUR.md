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
  bytes and locations, 0.1.10 validates standalone names, and 0.1.11 resolves
  layout over lexer-supplied events. Revision 0.1.12 scans comments and attaches
  outer documentation over supplied events, revision 0.1.13 scans one
  atomic literal, revision 0.1.14 elaborates that literal's numeric
  meaning as a typed `Int` or finite binary64 `Float` value, revision
  0.1.15 tokenizes complete source files and resolves operator expressions,
  revision 0.1.16 binds `.cat` files to at most one declared module, and
  revision 0.1.18 validates imports against digest-bound export sets,
  revision 0.1.19 fixes the abstraction boundary, revision 0.1.20
  compiles dependency cycles as components, revision 0.1.21 resolves
  package dependencies, and revision 0.1.22 admits prelude selections,
  all before declaration
  grammar or import punctuation is fixed;
- snippets in this tour are illustrative notation unless a linked
  specification says that a particular form is fixed;
- public parser punctuation and several ordinary language facilities
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

Revision 0.1.11 makes indentation non-semantic and classifies logical LF as a
hard separator, a soft continuation, or blank layout. A semicolon is always a
hard separator. `Catena.resolve_layout/2` consumes opaque lexer events whose
tokens declare before/after joins and continued or block delimiter frames.
This preserves grammar-aware, Elixir-like continuation without prematurely
assigning concrete operators or punctuation. Read the
[Whitespace and Layout guide](guides/language/whitespace-and-layout.md).

Revision 0.1.12 adds `//` line comments, nested `/* ... */` block comments,
and their `///` and `/** ... */` outer-documentation forms. Every logical LF
inside a comment participates in the same layout classifier. Documentation
normalization preserves source text after defined delimiter, edge-line, and
common-margin removal, then attaches only to the next parser-supplied
declaration target. CommonMark and explicit doctest metadata are recorded, but
rendering and execution remain future tooling. Read the
[Comments and Documentation Comments guide](guides/language/comments-and-documentation-comments.md).

Revision 0.1.13 fixes atomic Boolean, unsigned integer, decimal-float, text,
character, and byte literal spelling. Cooked forms use a closed escape set;
raw text and bytes use arbitrary exact hash delimiters. The scanner preserves
the original units and spans, exposes decoded provenance pieces, and keeps raw
literal line breaks inside the token rather than sending them to layout. It
does not interpolate text, parse collections, or scan a whole file.

Revision 0.1.14 gives those numeric tokens their meaning: integer literals
are exact mathematical `Int` values, decimal literals are finite binary64
`Float` values produced by one correctly rounded exact conversion, mixed
integer/decimal operands are ill-typed without coercions, negation is a
total sign flip that can produce `-0.0`, and a decimal beyond the largest
finite magnitude is refused statically as `NUM001`. Read the
[Literals guide](guides/language/literals.md).

Revision 0.1.15 supplies the operators and punctuation: a closed
semantic-mapped inventory (`+ - *`, comparisons and equalities, `! && ||`,
`-> |> . , ; ( ) [ ] { }`) matched by maximal munch, concrete continuation
capabilities and delimiter frames for 0.1.11 layout, and one fixed
precedence ladder. Comparisons and equalities do not chain — `a < b < c` is
rejected — prefix `-` and `!` bind tightest, and `|>` is the loosest,
left-associative pipe applying its right operand to its left. Reserved
spellings such as `/` and `=` are rejected as `OPR001`, invalid forms as
`OPR002`, and no recovery or partial output exists.

Revision 0.1.16 gives files their identity: a `.cat` file contains at most
one module whose declared ASCII uppercase-initial name must equal the file
basename — a mismatch is `FIL004` — while empty and comment-only files are
valid no-module units. Generated files are recognized by one exact
first-line `// catena:generated by <tool>` marker; the same text anywhere
else is ordinary comment content, and a malformed first-line marker is
`FIL005`. The concrete module-header syntax remains future grammar work.

Revision 0.1.17 fixes how names resolve: per-category namespaces under the
two spelling classes, so `Vec` the type and `vec` the value coexist while
two `Vec` types collide. Shadowing is innermost-wins and silent; type
variables scope per quantifier and may shadow type names; a local
declaration beats an import; two imports colliding on one unqualified
spelling are `NSP004` with both origins named until you qualify
`Module.member`. Governed specification identities never resolve as
program names.

Revision 0.1.18 closes the module boundary: nothing is exported without an
explicit export declaration (types choose transparent or abstract), an
import admits a module for `Module.member` qualification against its
digest-bound export set plus an explicit possibly-empty list of names
admitted unqualified (the empty list is qualified-only), wildcards,
renaming, and re-exports are excluded, and unused admissions surface as
deny-able `IMP001` warnings.

Revision 0.1.19 draws the abstraction boundary: representation is never
observable (no stable-layout opt-in exists; both layouts stay mandatory),
the transparent/abstract pair is the complete constructor-authority
vocabulary, and invariants are built the sanctioned way — export the type
abstract, export a validating constructor returning a typed failure
(`parse : String -> Result EmailError Email`), export observers, and let
clients cover abstract scrutinees with a wildcard. Selective exposure and
views stay future work owned by the views and data-model gaps.

Revision 0.1.20 admits module dependency cycles: each
strongly-connected component is one checking and caching unit. Inside a
component, references resolve against companions' declared signatures —
no digests circulate — and presenting a digest for a companion or
exporting an unsigned name is `CYC001`. Across components, imports stay
digest-bound exactly as before, and the whole component gets one joint
digest, invariant to member order. Modules contribute definitions only,
so loading per component is the whole initialization story. When a cycle
is convenience rather than mutual definition, invert the dependency:
pass the collaborator as a function value.

Revision 0.1.21 gives packages their identity and dependencies: a
manifest's optional `dependencies` object maps names to exact, caret
(`^`, with the Cargo 0.x rule), or tilde (`~`) requirements over SemVer
versions; resolution picks one version per name — the highest satisfying
every requirement — and rejects conflicts with every requirer named; a
generated `catena.lock` replays as exact pins for byte-reproducible
builds; and package identity is (name, version, SHA-256 bundle digest),
with hex.pm as the bootstrap transport whose tarball checksum must equal
the bundle digest. Pre-releases match only requirements that name
pre-releases.

Revision 0.1.22 fixes the prelude: a manifest's optional `prelude` field
names one package; its exports enter scope as an ordinary import-class
origin — locals win, collisions with explicit imports reject naming both
origins until qualified, and absent or `null` means no prelude at all.
Edition 0.1 guarantees zero implicit names: nothing enters scope that
was not asked for.

Revision 0.1.23 fixes entry points: a manifest's optional `entries`
array names existing zero-argument, effect-closed exports with at most
one launch marker; zero entries means a library; and launching an entry
invokes it to completion, reporting its returned value as the shutdown
result or the trap identity on failure. No supervision, spawning, or
exit codes exist at this layer.

Revision 0.1.24 fixes API compatibility: interface changes classify
under a strict diff matrix (removals, renames, scheme changes, and
effect-row widening are breaking; additions are minor; representation
never breaks alone), version claims are validated against it — breaking
requires major at 1.0+ and minor below — and behavior and BEAM ABI
compatibility are declared absences: the kernel is the behavior
contract and binaries are deterministic outputs, not surfaces.

Revision 0.1.31 fixes recursion and termination: program recursion is
unrestricted — divergence is non-termination, never a trap — the tail
guarantee is the only stack promise, no totality checking exists, and
every meta-level evaluator (conditions, specification examples, laws)
is total-or-bounded, with compile-time evaluation gated to arrive the
same way or not at all.

Revision 0.1.30 fixes equality and ordering: the comparable set is
closed — Int, Bool, Float, plus structural composites — float
equality is bit-exact (`-0.0 != 0.0`), comparison is monomorphic,
closures and handles never compare (`EQN001`), and guards keep C003's
frozen fragment.

Revision 0.1.29 fixes branching: match is the only branch form, an
`if`-like conditional is promised to desugar to a Bool-pattern match,
the consolidated rules keep their citing areas' homes, and
statement-like control forms are declared absent — everything is an
expression.

Revision 0.1.28 fixes functions and calls: every function is
semantically unary (multi-parameter definitions are nested-unary
sugar), partial application is free — any prefix application is a
first-class closure value — capture is lexical and immutable, the
let-bound closure is the local-function form, and a call in tail
position consumes no unbounded stack, witnessed by a five-million-
iteration recursion on BEAM.

Revision 0.1.27 fixes bindings and sequencing: local `let` is strictly
non-recursive with sequential-lexical scope and silent innermost
shadowing; recursion lives in named definitions (the kernel's signed
environment) with SCC components as mutual recursion's home; an unused
binding stays valid with its effects preserved, warned by the deny-able
`BS001` unless its name is underscore-prefixed; and `let _ = e1; e2` is
the sequencing form.

Revision 0.1.26 fixes evaluation order: one closed ordered-forms table
covers every existing compound — the kernel's list plus curried
application, trait calls, handler installation, and annotate — with
trace-observable semantics: the reference evaluator and compiled BEAM
produce equal effect-request traces, order is never advisory, and a
future-form entry rule keeps collections and interpolation unordered
until their own slices.

Revision 0.1.25 fixes the value model: a closed ten-form value grammar
(the kernel's nine plus Float), uniformly first-class, with resumptions
and traps among the never-values; and a strictness invariant — every
subexpression evaluates at most once, to a value or a terminal trap,
before use — with `and`/`or` as the only skips and an edition-record
gate against any future lazy form.

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
                           ↓
layout events 0.1.11 → lossless soft/separator/blank classification
                       ↓
comment events 0.1.12 → nested scanning and outer documentation attachment
                       ↓
atomic literal 0.1.13 → decoded payload and exact source provenance
                       ↓
numeric meaning 0.1.14 → typed Int and finite binary64 Float values
                       ↓
operators 0.1.15 → whole-source token stream and operator-expression trees
                       ↓
file units 0.1.16 → one module per .cat file, verified by basename
                       ↓
namespaces 0.1.17 → per-category identities with deterministic precedence
                       ↓
imports 0.1.18 → export sets, admission lists, unused-import warnings
                       ↓
abstraction 0.1.19 → no stable layout, binary authority, invariant idiom
                       ↓
cycles 0.1.20 → SCC units, signature regimes, joint digests
                       ↓
packages 0.1.21 → requirements, single-version resolution, locks
                       ↓
prelude 0.1.22 → opt-in origin, ordinary precedence, zero implicit names
                       ↓
entries 0.1.23 → effect-closed entries, derived libraries, launch
                       ↓
compat 0.1.24 → strict diff matrix, claim validation, declared absences
                       ↓
values 0.1.25 → closed grammar, first-class values, strictness gate
                       ↓
order 0.1.26 → closed table, entry rule, trace observability
                       ↓
bindings 0.1.27 → non-recursive lets, sequencing idiom, BS001
                       ↓
functions 0.1.28 → semantic-unary currying, closures, proper tails
                       ↓
branching 0.1.29 → match-only dispatch, sugar promise, no statements
                       ↓
equality 0.1.30 → comparable set, bit-exact floats, EQN001
                       ↓
recursion 0.1.31 → unrestricted programs, bounded meta evaluators
                       (stops before the future declaration grammar)

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
10. [`c027_entry_points_test.exs`](test/catena/c027_entry_points_test.exs) —
   entry declaration shapes, library derivation, effect-closure
   validation, invocation-only launch, return-is-shutdown reports, and
   the `ENT001`–`ENT003` families.
11. [`c028_api_compat_test.exs`](test/catena/c028_api_compat_test.exs) —
   the full breaking matrix, entry-set classification, SemVer claim
   validation across the 0.x and 1.0+ rules, the declared absences, and
   the `CMP001`–`CMP003` families.
12. [`c029_values_test.exs`](test/catena/c029_values_test.exs) — the
   closed value grammar with Float, the non-value reasons, uniform
   first-classness witnesses, strictness terminals over the kernel
   stepper, and the zero-diagnostics definitional stance.
13. [`c030_evaluation_order_test.exs`](test/catena/c030_evaluation_order_test.exs) —
   dual reference/BEAM trace agreement for every ordered form,
   curried application, the `and`/`or` skips, handler installation,
   and determinism.
14. [`c031_bindings_test.exs`](test/catena/c031_bindings_test.exs) —
   non-recursion rejections, silent shadowing, kernel recursion
   witnesses, unused-binding effect preservation, the sequencing
   idiom, and the `BS001` warning matrix with deny promotion.
15. [`c032_functions_test.exs`](test/catena/c032_functions_test.exs) —
   curried application agreement, partial application as a callable
   value, immutable capture, let-bound local closures, exported named
   functions, and the deep tail-call witness on BEAM.
16. [`c033_branching_test.exs`](test/catena/c033_branching_test.exs) —
   Bool-pattern dispatch as the conditional, guarded fallthrough,
   commitment traces, `M001` unchanged, and the statement-form
   absence.
17. [`c035_equality_test.exs`](test/catena/c035_equality_test.exs) —
   the comparable-set classifier with signed-zero ordering, tuple and
   constructor-value equality on both targets, `EQN001` exclusions,
   monomorphism, and the guard split.
18. [`c034_recursion_test.exs`](test/catena/c034_recursion_test.exs) —
   non-tail recursion at 10,000 depth on BEAM, the stepper's
   budget-exhaustion divergence witness, tail termination, the
   `CND004` regression, and the bounded-regime matrix.

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
