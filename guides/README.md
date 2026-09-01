# Catena Guides

These guides explain the executable Catena language model, its assurance
protocol, and the bootstrap compiler. They complement rather than replace the
[normative language specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification).

Catena does not yet have an ergonomic source lexer or parser. Revisions 0.1.9
through 0.1.22 provide the strict source envelope, standalone Unicode name
rules, layout classification, abstract comment/documentation pipeline,
atomic literal scanner, and numeric literal meaning that later stages will
consume. Code in a
`catena` fence is **illustrative source notation**: it teaches the selected
language meaning but does not freeze punctuation or every keyword. Commands,
JSON-AST examples, and the separately identified exact 0.1.8 kernel
S-expressions are executable against the current compiler.

The reader-facing guides use Catena's current behavior-first vocabulary:
`variant`, `payload`, `match`, `Mapper`, `uses`, `request`, `promises`, and
similar words describe what a programmer does. Formal and compiler-internal
terms remain available where precision requires them, especially in the
developer path. Each guide marks vocabulary that is proposed but not yet
accepted by the source parser or implemented semantic slice.

## Choose a path

```mermaid
flowchart TD
    Start[Start here] --> Tour[Language Tour]
    Tour --> First[Getting Started]
    First --> Text[Source Text]
    Text --> Names[Identifiers]
    Names --> Layout[Whitespace and Layout]
    Layout --> Comments[Comments and Documentation]
    Comments --> Literals[Literals]
    Literals --> Editions[Editions and Previews]
    Editions --> Data[Variant Types]
    Data --> Match[Pattern Matching]
    Match --> Traits[Traits and Composition]
    Traits --> Effects[Effects and Handlers]
    Effects --> Kernel[Formal Semantic Kernel]
    Kernel --> Specs[Specifications]
    Specs --> Gov[Governance]
    Gov --> Beam[Catena and BEAM]

    Gov --> Ops[Governance Operations]

    Start --> Dev[Compiler Architecture]
    Dev --> Concepts[Kernel Developer Concepts]
    Concepts --> IR[Intermediate Representations]
    IR --> Tests[Diagnostics and Testing]
    Tests --> Feature[Adding a Language Feature]
    Feature --> Lifecycle[Versioning and Feature Lifecycle]
    Lifecycle --> Contrib[Contributing]
```

## User path

1. [Language Tour](../LANGUAGE-TOUR.md) — a compact overview of the complete
   language direction.
2. [Getting Started](getting-started.md) — install the toolchain, understand a
   source-first example, and run the executable model.
3. [Source Text](language/source-text.md) — validate UTF-8, newline handling,
   normalization preservation, and original-byte locations at revision 0.1.9.
4. [Identifiers and Qualified Names](language/identifiers.md) — validate
   Unicode 17 names, NFC, security profiles, keywords, qualification, and
   confusable warnings at revision 0.1.10.
5. [Whitespace, Separators, and Line Continuation](language/whitespace-and-layout.md)
   — resolve non-semantic indentation, hard separators, and grammar-aware soft
   lines over lexer-supplied events at revision 0.1.11.
6. [Comments and Documentation Comments](language/comments-and-documentation-comments.md)
   — scan slash comments, classify nested comment line breaks, and attach
   normalized outer documentation at revision 0.1.12.
7. [Literals](language/literals.md) — scan Boolean, numeric, text, character,
   and byte literals with exact decoding and provenance at revision 0.1.13,
   then elaborate numeric meanings at revision 0.1.14.
8. [Operators](language/operators.md) — tokenize complete files into the
   0.1.15 whole-source stream and resolve operator expressions over the
   fixed precedence ladder.
9. [Files and Modules](language/files.md) — resolve `.cat` file units with
   at-most-one module, basename verification, and generated markers at
   revision 0.1.16.
10. [Namespaces](language/namespaces.md) — resolve names through
   per-category namespaces with shadowing and precedence at revision
   0.1.17.
11. [Imports and Exports](language/imports.md) — validate exports and
   imports against digest-bound export sets with unused-import warnings
   at revision 0.1.18.
12. [Abstraction](language/abstraction.md) — confirm the binary authority
   vocabulary, the stable-layout exclusion, and the smart-constructor
   invariant idiom at revision 0.1.19.
13. [Module Cycles](language/module-cycles.md) — compile dependency cycles
   as strongly-connected components with signature regimes and joint
   digests at revision 0.1.20.
14. [Packages](language/packages.md) — declare dependencies, resolve
   versions, generate and replay `catena.lock`, and identify packages by
   bundle digest at revision 0.1.21.
15. [Prelude](language/prelude.md) — select a prelude, understand its
   ordinary precedence and the zero-implicit-names guarantee at revision
   0.1.22.
16. [Entry Points](language/entry-points.md) — declare effect-closed
   entries, derive libraries, and launch an entry whose returned value is
   the shutdown result at revision 0.1.23.
17. [API and ABI Compatibility](language/api-compatibility.md) —
   classify interface changes under the strict matrix, validate SemVer
   claims, and rely on the declared behavior and ABI absences at
   revision 0.1.24.
18. [Values and Evaluation](language/values.md) — the closed ten-form
   value grammar with Float, uniform first-classness, the strictness
   invariant with its edition-record gate, and value-or-trap terminals
   at revision 0.1.25.
19. [Evaluation Order](language/evaluation-order.md) — the closed
   ordered-forms table with typed-core completions, the future-form
   entry rule, and trace-observable order at revision 0.1.26.
20. [Bindings and Sequencing](language/bindings.md) — non-recursive
   local bindings, silent shadowing, definitions-only recursion, the
   sequencing idiom, and the deny-able `BS001` warning at revision
   0.1.27.
21. [Functions and Calls](language/functions.md) — the semantic-unary
   curried model, free partial application, lexical immutable capture,
   let-bound local functions, and proper tail calls at revision
   0.1.28.
22. [Branching](language/branching.md) — match as the single branch
   form, the conditional sugar promise, the consolidated rules, and
   the statement-form absence at revision 0.1.29.
23. [Equality and Ordering](language/equality.md) — the closed
   comparable set with structural recursion, bit-exact float equality,
   monomorphic comparison, and the `EQN001` exclusion at revision
   0.1.30.
24. [Recursion and Termination](language/recursion.md) — unrestricted
   program recursion with divergence as non-termination, the cited
   separation table, and the G038 entry rule at revision 0.1.31.
25. [Failure Taxonomy](language/failure.md) — the single `trap`
   outcome with kinded reasons, the six-category mapping, and the
   per-producer entry rule at revision 0.1.32.
26. [Resource Observability](language/observability.md) — the six-way
   non-observability classification, semantic identity, the
   two-clause identity rule, and the finalization gate at revision
   0.1.33.
27. [Compile-Time Evaluation](language/compile-time.md) — the
   absence-plus-gate stance, derivations as generation, and the cited
   restriction table at revision 0.1.34.
28. [Built-In Data Model](language/data-model.md) — the twelve-way
   classification, Text/Character/Bytes elaboration, and
   content-based comparability at revision 0.1.35.
29. [Structural Records](language/records.md) — the seven-operation
   table, kernel rows verbatim, and semantic maps at revision
   0.1.36.
30. [Collections](language/collections.md) — construction as constructor
   application, miss as value, and the complexity exclusion at revision
   0.1.37.
31. [Pattern Contexts](language/pattern-contexts.md) — the three
   refutability classes, per-context rules and reservations, and the
   programmable-pattern exclusion at revision 0.1.38.
32. [List Comprehensions](language/list-comprehensions.md) — the
   dormant elaboration boundary for `for ... yield` at revision
   0.1.39: qualifier trees, the fused worker chain, and the `LCP`
   families.
33. [Numeric Relationships](language/numeric-relationships.md) —
   closed-set instantiation over `{Int, Float}` at revision
   0.1.40: same-type operands, no dispatch, division to G105.
34. [Aliases and Newtypes](language/aliases-and-newtypes.md) — the
   alias exclusion, the abstract-export opaque routing, and the
   newtype as the nominal single-field ADT at revision 0.1.41.
35. [Name Resolution](language/name-resolution.md) — the
   type-independence invariant with the five-way classification
   and the evidence-selection carve-out at revision 0.1.42.
8. [Editions, Revisions, and Previews](language/editions-and-previews.md) —
   pin one exact language contract, inspect retained revisions, understand
   named feature lifecycle, and read migration diagnostics.
9. [Variant Types and Structured Data](language/algebraic-data-types.md) —
   model a domain with nominal variants, explicit payloads, and controlled
   module boundaries.
10. [Pattern Matching](language/pattern-matching.md) — consume data with
   ordered, exhaustive clauses and safe conditions.
11. [Traits and Composition](language/traits-and-composition.md) — use shared
   behavior such as `map`, `map2`, and `and_then` without requiring category
   theory terminology.
12. [Effects and Handlers](language/effects-and-handlers.md) — declare external
   abilities, request them through lexical capabilities, and interpret them
   with deep affine handlers.
13. [Formal Semantic Kernel](language/formal-semantic-kernel.md) — run the exact
   0.1.8 conformance input and understand structural rows, the small-step
   machine, typed actors, fixed layouts, and normative conformance evidence.
14. [Specifications](language/specifications.md) — attach typed rules and exact
   examples to named language subjects.
15. [Governance](language/governance.md) — understand policy, evidence,
   approval, lifecycle, and protected package actions.
16. [Catena and BEAM](language/catena-and-beam.md) — understand Abstract Format
   lowering, module interfaces, companion modules, and the current
   interoperability boundary.

## Operator path

- [Governance Operations](operations/governance-operations.md) — establish an
  offline trust root, operate build/publish/activate gates, sign externally,
  verify assurance manifests, rotate authority, revoke credentials, and use
  recovery.

The compiler verifies governance documents but does not manage private keys or
provide an organizational workflow service. Operators remain responsible for
secure key generation, custody, approval collection, and transport.

## Compiler developer path

- [Compiler Architecture](development/compiler-architecture.md) — pipeline,
  modules, trust boundaries, and non-negotiable invariants.
- [Semantic-kernel Developer Guides](../docs/guides/developer/README.md) — a
  detailed concept-by-concept account of the kernel, S-expression reader,
  parser, type checker, reference machine, stepper/explorer, OTP lowering, and
  compiler, including their contracts and relationships.
- [Intermediate Representations](development/intermediate-representations.md)
  — JSON AST, decoded AST, typed core, interfaces, Erlang Abstract Format, and
  assurance records.
- [Diagnostics and Testing](development/diagnostics-and-testing.md) — stable
  error families and the layered conformance strategy.
- [Compiler Conformance Profile](../CONFORMANCE.md) — the bootstrap release,
  supported revisions, selected optional paths, recommendation dispositions,
  presentation latitude, and published implementation limits.
- [Adding a Language Feature](development/adding-a-language-feature.md) — the
  end-to-end specification, implementation, verification, and documentation
  workflow.
- [Versioning and Feature Lifecycle](development/versioning-and-feature-lifecycle.md)
  — exact semantic selection, retained formats, previews, migration records,
  signature domains, compatibility tests, and the C008 promotion gate.
- [Contributing](../CONTRIBUTING.md) — repository setup, change discipline,
  review expectations, and pull-request checklist.

Developer guides deliberately show both sides of the vocabulary boundary—for
example, public `variant` and `implementation` alongside internal constructor
identity and instance evidence—so implementation terminology never leaks into
the beginner path by accident.

## Authority and status

The research repository's
[Specification Authority](https://github.com/pcharbon70/catena-research/blob/main/SPECIFICATION-AUTHORITY.md)
defines which documents control. Only an applicable `status: normative`
specification chapter defines the language. A version number does not select a
winner unless normative text explicitly states applicability or replacement.
The companion
[Conformance Vocabulary](https://github.com/pcharbon70/catena-research/blob/main/CONFORMANCE-VOCABULARY.md)
defines canonical requirement words and behavior classes. The local
[compiler profile](../CONFORMANCE.md) discloses implementation choices and
limits against those rules; it remains evidence rather than authority.

Conformance requirements, executable reference paths, compiler tests,
immutable implementation records, and compiler behavior are evidence against
that specification. They do not amend it, resolve its silence, or outrank one
another. Guides, the language tour, and exploratory research are explanatory.

When artifacts disagree, cite the normative document and heading, suspend the
affected conformance claim, and repair the non-normative artifact. If the
normative chapters themselves conflict or remain ambiguous, resolve the
language text explicitly before changing compiler behavior. A guide bug does
not amend the language; add a regression test when the same misunderstanding
could also appear in the compiler.

## Versioned implementation slices

| Version | Implemented slice |
| --- | --- |
| 0.1.1 | principal type inference and annotation-directed advanced checking |
| 0.1.2 | nominal algebraic data, patterns, coverage, folds, and interfaces |
| 0.1.3 | safe clause conditions and condition-aware coverage |
| 0.1.4 | coherent traits, structural derivation, specialization, and erasure |
| 0.1.5 | lexical effects, deep handlers, affine resumptions, and effect-directed CPS |
| 0.1.6 | typed specifications, offline governance, assurance artifacts, and total erasure |
| 0.1.7 | editions, exact revisions, previews, migration records, and selection-bound artifacts |
| 0.1.8 | exact formal semantic kernel, independent verification, and typed local actors |
| 0.1.9 | strict UTF-8 source-text envelope, newline normalization, and original-byte scalar spans |
| 0.1.10 | Unicode 17 identifiers, NFC spelling, secure scripts, keywords, qualification, and confusable diagnostics |
| 0.1.11 | non-semantic indentation, hard separators, abstract continuation, and lossless layout events |
| 0.1.12 | slash comments, nested block comments, layout integration, and outer documentation attachment |
| 0.1.13 | atomic Boolean, numeric, text, character, and byte literal spelling, decoding, and provenance |
| 0.1.14 | numeric literal meaning: monomorphic `Int` and finite binary64 `Float`, correct rounding, static overflow invalidity, negation |
| 0.1.15 | operators and punctuation: closed inventory, maximal munch, C015 capabilities and frames, fixed ladder, pipe, transactional rejection |
| 0.1.16 | files and modules: `.cat` extension, at-most-one module with basename verification, no-module files, generated markers |
| 0.1.17 | namespaces: per-category namespaces with spelling classes, silent shadowing, type variables, local-over-imported precedence |
| 0.1.18 | imports and exports: private-by-default exports with transparency modes, qualification-plus-list admission, declared exclusions, `IMP001` warnings |
| 0.1.19 | abstraction boundaries: binary authority vocabulary, no stable layout, smart-constructor invariant idiom |
| 0.1.20 | module dependency cycles: SCC admission, signature regimes, joint digests, inversion alternative |
| 0.1.21 | packages: manifest dependencies, SemVer exact/caret/tilde, single-version resolution, `catena.lock`, bundle digests |
| 0.1.22 | prelude: opt-in manifest selection, ordinary-origin precedence, absent/null opt-out, zero-implicit-names guarantee |
| 0.1.23 | entry points: named zero-argument effect-closed entries, derived libraries, one launch marker, invocation-only launch with return-is-shutdown |
| 0.1.24 | API compatibility: strict interface diff matrix, SemVer claim validation, declared behavior and BEAM ABI absences |
| 0.1.25 | values and evaluation: closed ten-form value grammar with Float, uniform first-classness, strictness invariant with edition-record gate, value-or-trap terminals |
| 0.1.26 | evaluation order: closed ordered-forms table with typed-core completions, future-form entry rule, trace-observable order with reference/BEAM agreement |
| 0.1.27 | bindings and sequencing: non-recursive local lets, silent innermost shadowing, definitions-only recursion, sequencing idiom, deny-able `BS001` with `_`-prefix exemption |
| 0.1.28 | functions and calls: semantic-unary currying, free partial application, lexical immutable capture, let-bound local functions, elevated proper-tail-call guarantee |
| 0.1.29 | branching: match-only dispatch, conditional sugar promise, consolidated rules, statement forms declared absent |
| 0.1.30 | equality and ordering: closed comparable set with structural recursion, bit-exact float equality (`-0.0 != 0.0`), monomorphic comparison, `EQN001` exclusion |
| 0.1.31 | recursion and termination: unrestricted program recursion, divergence as non-termination, cited separation table, G038 entry rule |
| 0.1.32 | runtime failure taxonomy: single `trap(reason)` outcome, kinded reasons, six-category mapping, per-producer entry rule, kernel-verbatim observability |
| 0.1.33 | resource observability: six-way non-observability classification, semantic identity, two-clause identity rule, gated finalization absence |
| 0.1.34 | compile-time evaluation: constants never execute, attributes and macros absent, derivations as gated-free generation, cited restriction table |
| 0.1.35 | built-in data model: twelve-way classification, Text/Character/Bytes elaborated from scanned literals, collections as library territory, references excluded |
| 0.1.36 | structural records and variants: seven-operation table, closed literals with duplicate-label rejection, type-position tails, semantic maps |
| 0.1.37 | collection construction and update: six-topic decision, miss as typed failure as a value, complexity excluded from the language layer |
| 0.1.38 | pattern contexts: three context classes, irrefutable-only default, public-receive reservation, exception-clause and programmable-pattern exclusions |
| 0.1.39 | list comprehensions: for-yield surface contract, qualifier rules, sequential execution, dormant qualifier-tree elaboration to a fused worker chain |
| 0.1.40 | numeric relationships: closed-set instantiation over {Int, Float}, same-type arithmetic, no dispatch, division and remainder to G105 |
| 0.1.41 | aliases and newtypes: transparent aliases excluded with arrival conditions, opaque = abstract export, newtype = nominal single-field ADT, explicit-only deriving |
| 0.1.42 | name resolution: type-independent invariant, five-way classification, evidence selection carved out from resolution |

Versions 0.1.1 through 0.1.42 identify completed normative revision boundaries;
their accepted frontend formats remain explicit rather than implicitly
cumulative.
Version 0.1.7 implements the normative C008 edition and lifecycle contract;
its immutable promotion evidence is recorded in the research archive. Edition `0.1` is the
end-user compatibility track; each exact three-component revision selects a
specific cumulative contract. The compiler package has its own release
version, so `mix.exs` remains `0.1.0`. Retired two-component slice identifiers
are rejected rather than normalized because they also occurred in digests and
signed protocol domains.
