# Catena Language Tour

Catena is a strict functional language for the BEAM VM. Its design combines
type inference, algebraic data, exhaustive pattern matching, approachable
composition operations, and explicit effect handling. The aim is to give
ordinary programmers useful guarantees without requiring category-theory
terminology as a prerequisite.

This tour is the best starting point for understanding the language model. It
is not a replacement for the specification.

## Before you start

Catena is currently an executable language-design prototype, not yet a
source-language distribution:

- the compiler accepts a versioned JSON AST rather than Catena source text;
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

Catena's initial model has seven connected parts:

1. **Functions and inference** — ordinary code receives principal
   Hindley–Milner types where possible; advanced features require explicit
   annotations.
2. **Data and patterns** — nominal algebraic data types are consumed through
   ordered, exhaustively checked pattern matches.
3. **Clause conditions** — a deliberately small, pure, total condition
   language can refine clause selection without hiding arbitrary execution.
4. **Shared behavior** — coherent traits expose operations such as `map`,
   `combine`, and `and_then` under names that describe what programmers do.
5. **Effects and handlers** — `uses`, `request`, and `handle` make nonlocal
   behavior explicit and select handlers through lexical capability identity.
6. **Specifications and governance** — optional typed rules and exact examples
   become strict, artifact-bound package gates once a project adopts them.
7. **BEAM execution** — verified typed core lowers to Erlang Abstract Format,
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

## Algebraic data and patterns

Types have nominal identity: two declarations with the same shape are still
different types. A declaration can expose its constructors or keep them
abstract across a module boundary.

The canonical datatype notation is:

```catena
type Option A =
  | None
  | Some A

type DeliveryStatus =
  | Queued
  | InTransit { tracking_id: TrackingId }
```

Construction qualifies the constructor:

```catena
Option.Some(7)
DeliveryStatus.InTransit { tracking_id: id }
```

Pattern matching is ordered, but the compiler also checks usefulness and
exhaustiveness. Missing alternatives receive concrete witnesses such as
`Option.Some(_)`; unreachable alternatives are rejected rather than silently
retained.

The initial pattern language includes wildcards, binders, integer and Boolean
literals, tuples, constructors, `as` patterns, and `or` patterns. It does not
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

Instances are globally coherent: import order and local preference cannot
silently change which behavior is selected. Trait evidence is checked during
compilation, specialized to direct calls, and erased from the resulting BEAM
code. Laws are explicit promises or test evidence; they do not authorize
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

Initial handlers are deep: resuming reinstalls the same handler around the
remaining computation. A clause may discard its resumption or invoke it once,
using the dedicated form:

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

Catena 0.6 explores an optional assurance layer without making organizational
governance a prerequisite for ordinary programs. A rule names a typed,
effect-free verification definition and attaches it to a resolved language
subject. Exact examples invoke that checker under a fixed deterministic
budget. Compiler evidence, signed external attestations, and explicit
assumptions retain different meanings.

Once a package names a governance bundle, every matching package, module,
action, output, interface, and profile policy is combined additively. A local
`build` may be permitted while `publish` or `activate` remains blocked.
Approvals and lifecycle transitions bind exact claim, policy, evidence, and
artifact digests; signatures prove who signed canonical bytes, not that the
statement is mathematically true.

All 0.6 specification and governance material is build-time only. Verification
definitions disappear before Erlang Abstract Format lowering, and the package
sidecar records what was checked and which exact BEAM/interface bytes were
admitted. The compiler emits a canonical signing payload for an external
Ed25519 signer and never reads a private key.

This is a candidate semantic JSON contract, not final Catena source
punctuation. Read the
[candidate specification and governance chapters](https://github.com/pcharbon70/catena-research/tree/main/60-specification/specifications-and-governance)
for the exact formats, policy algebra, lifecycle, diagnostics, and promotion
gate.

## From Catena to BEAM

The compiler path is:

```text
future Catena source parser
        ↓
versioned JSON AST (the current input)
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

Catena does not yet provide a source parser, formatter, REPL, end-user package
manager, complete standard library, language-level concurrency model, resource
scope semantics, exception boundary, foreign-term validation, or finalized
surface grammar. The compiler is valuable today as a deterministic executable
model of the completed specification slices—not yet as a general-purpose
language toolchain.
