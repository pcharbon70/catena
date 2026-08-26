# Catena Bootstrap Compiler Conformance Profile

This document is the versioned conformance disclosure for the current Catena
bootstrap compiler. It describes the implementation; it does not define or
amend the language.

The applicable normative chapters remain in the
[Catena research specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification).
The research repository's
[Specification Authority](https://github.com/pcharbon70/catena-research/blob/main/SPECIFICATION-AUTHORITY.md)
defines which documents control, and its
[Conformance Vocabulary](https://github.com/pcharbon70/catena-research/blob/main/CONFORMANCE-VOCABULARY.md)
defines requirement words, behavior classes, and profile obligations. The
[Implementation Limits and Portability policy](https://github.com/pcharbon70/catena-research/blob/main/IMPLEMENTATION-LIMITS.md)
defines portable minima, limit reporting, and exhaustion outcomes. If this
profile and an applicable controlling document disagree, the controlling
document governs and the affected conformance claim is suspended.

## Profile identity

| Field | Value |
| --- | --- |
| Profile format | `1` |
| Machine-readable profile | `catena conformance-info` (`catena-conformance-info`, version `1`) |
| Implementation | Catena Elixir bootstrap compiler |
| Compiler release | `0.1.0` |
| Bootstrap toolchain | Elixir `1.20.2-otp-29` on Erlang/OTP `29.0.4` |
| Runtime target | BEAM through OTP 29 Erlang Abstract Format |
| Edition | `0.1` |
| Supported exact language revisions | Normative `0.1.1` through `0.1.22` |
| Source boundary | Versioned JSON AST for `0.1.1`–`0.1.7`; exact kernel S-expression for `0.1.8`; strict source-text envelope for `0.1.9`–`0.1.22`; standalone identifiers for `0.1.10`; layout over lexer-supplied events for exact `0.1.11`; comment scanning and documentation attachment over supplied events for exact `0.1.12`; atomic literal scanning for exact `0.1.13`; numeric literal elaboration for exact `0.1.14`; whole-source tokenization and operator-expression parsing for exact `0.1.15`; file-unit resolution for exact `0.1.16`; namespace resolution for exact `0.1.17`; import/export validation and unused-import analysis for exact `0.1.18`; abstraction-boundary exclusions for exact `0.1.19`; SCC grouping and joint digests for exact `0.1.20`; dependency resolution, lockfiles, and bundle digests for exact `0.1.21`; the prelude origin for exact `0.1.22` |
| Implementation-defined choices | None |
| Vendor extensions | None |

C009 and C012 remain repository-governance milestones and did not consume a
language revision. Normative C010 uses `0.1.8` without changing compiler release
`0.1.0`. It adds a separate kernel frontend, 0.1.8 interface and compile
metadata, public kernel CLI commands, and fixed kernel BEAM representations;
the retained JSON, package, governance, and historical signature formats are
unchanged. Its explicitly authorized immutable compiler identity and
post-commit evidence are recorded in the
[C010 conformance journal](https://github.com/pcharbon70/catena-research/blob/main/50-journal/2026-08-06-c010-formal-semantic-kernel.md).
C012 centralizes the already-published bootstrap budgets, establishes portable
floors for source and artifact dimensions, and adds deterministic
machine-readable disclosure without changing language revision or compiler
release.

Normative C013 uses `0.1.9` for strict UTF-8 source-text decoding. It adds
`Catena.SourceText`, `Catena.decode_source_text/2`, `catena check-source-text`,
and `SRC001`–`SRC003`. This revision emits no module interface or BEAM artifact
and does not change the retained JSON or exact-kernel frontends.

Normative C014 uses `0.1.10` for standalone identifiers and qualified names.
The compiler uses vendored Unicode 17.0.0 data for XID, NFC, the General
Security Profile, Highly Restrictive script checks, and confusable skeletons.
It adds `Catena.parse_identifier/2`, `Catena.parse_qualified_name/2`,
`Catena.audit_identifiers/2`, `catena check-identifiers`, and `IDN001`–`IDN007`.
It emits no persisted interface or BEAM artifact and does not widen the JSON
or exact-kernel frontend versions.

Normative C015 uses `0.1.11` for whitespace, separators, and line
continuation. It adds `Catena.Layout`, `Catena.resolve_layout/2`, lossless
`soft`/`separator`/`blank` LF classifications, and `LAY001`–`LAY003`. The
engine consumes abstract events supplied by a future lexer; it defines no
whole-source CLI and does not guess comments, literals, or concrete token
capabilities. It emits no persisted interface or BEAM artifact and does not
widen the JSON, exact-kernel, interface, artifact, or signed-format versions.

Normative C016 uses `0.1.12` for comments and outer documentation comments.
It adds `Catena.Comment`, `Catena.scan_comment/2`, `Catena.resolve_comments/2`,
nested block scanning, lossless comment-internal LF classification, normalized
documentation attachments, and `CMT001`, `CMT002`, and `DOC001`. The APIs
consume lexer- and parser-supplied boundaries; they define no whole-source CLI,
Markdown rendering, or doctest execution. This source-only revision does not
widen the retained or persisted format versions.

Normative C017 uses `0.1.13` for atomic literal spelling, decoding, and source
provenance. It adds `Catena.Literal`, `Catena.scan_literal/2`, `LIT001`–`LIT003`,
and activates `LIM004`. It does not extend the retained JSON, kernel,
interface, artifact, or signed-format sets and does not claim a whole lexer,
parser, or runtime literal integration.

Normative C018 uses `0.1.14` for numeric literal semantics. It adds
`Catena.Numeric`, `Catena.elaborate_numeric_literal/2`, `NUM001`, and
activates `LIM005`. Integer literals elaborate to exact mathematical `Int`
values; decimal literals elaborate to finite binary64 `Float` values through
one correctly rounded exact conversion, with static invalidity for decimals
that round beyond the largest finite magnitude. Literal scanning stays pinned
to exact `0.1.13`, and this source-only revision does not widen the retained
JSON, kernel, interface, artifact, or signed-format sets.

Normative C019 uses `0.1.15` for operators and punctuation. It adds
`Catena.Tokenizer`, `Catena.Operator`, `Catena.tokenize_source/2`,
`Catena.parse_operator_expression/1`, and `OPR001`–`OPR002`. The closed
semantic-mapped token inventory is matched by maximal munch against every
C014–C018 atom; concrete C015 capability and delimiter-frame assignments
make multiline parenthesized and bracketed contents soft and brace contents
newline-separated; the fixed precedence ladder rejects comparison chains,
places prefix `-` and `!` above the binary levels, and binds `|>`
left-associatively as the loosest operator. Reserved symbol spellings are
rejected transactionally and no recovery exists. Every predecessor API
retains its exact 0.1.10 through 0.1.14 selection and default.

Normative C020 uses `0.1.16` for the file-to-module relationship. It adds
`Catena.FileUnit`, `Catena.resolve_file_unit/4`, and `FIL001`–`FIL005`. A
`.cat` file contains at most one module declared by parser-supplied events
whose ASCII uppercase-initial name must equal the file basename; empty and
comment-only files are valid no-module units; generated files are
recognized by one exact first-line `// catena:generated by <tool>` marker
that is inert anywhere else. The concrete module-header syntax remains
future grammar work, and every predecessor API retains its exact 0.1.10
through 0.1.15 selection and default.

Normative C021 uses `0.1.17` for namespaces and shadowing. It adds
`Catena.Namespace`, `Catena.build_namespace_environment/2`,
`Catena.resolve_name/2`, and `NSP001`–`NSP005`. Names live in
per-category namespaces with the hard spelling-class partition; shadowing
is innermost-wins, silent, and cross-category-safe; type variables scope
per quantifier and may shadow type names; local declarations beat imports
while two-origin unqualified collisions are rejected with every origin
named; governed identities never participate in program resolution; and a
qualified reference is exactly two segments. Import syntax remains future
work, and every predecessor API retains its exact 0.1.10 through 0.1.16
selection and default.

Normative C022 uses `0.1.18` for imports and exports. The namespace
environment builder consumes export events (categories, spelling classes,
and type transparency modes), provided-module events carrying
digest-bound export sets, and import-module events admitting
qualification plus an explicit possibly-empty unqualified name list. It
adds `EXP001` for exports of undeclared names, `IMP002`/`IMP003` for
invalid imports, the declared exclusions of wildcards, hiding, renaming,
aliases, and re-exports, and `Catena.Namespace.check_unused_imports/2`
whose deny-able `IMP001` warnings never affect acceptance. The concrete
`use`/`export` punctuation remains future grammar work.

Normative C023 uses `0.1.19` for abstraction boundaries. It confirms the
transparent/abstract pair as the complete constructor-authority vocabulary,
admits no stable-layout form on any frontend (both-layout conformance stays
mandatory and `L001` unchanged), and sanctions the abstract-type-plus
validating-constructor-plus-observer invariant idiom whose abstract
constructors stay unconstructible and unmatchable through digest-bound
interfaces. No accepted input, diagnostic family, or persisted format
changes.

Normative C024 uses `0.1.20` for module dependency cycles. The namespace
environment builder partitions provide/import event graphs into
strongly-connected components, rejects intra-component digest presentation
and signature gaps as `CYC001` at the closing transaction, and records
membership; `Catena.compile_scc/2` compiles all members of a component
together — each member checked and compiled against its companions'
provisional interfaces built from declared types, then cross-verified —
and yields the members' binaries and interfaces plus one deterministic
joint digest, invariant to member order and layout. Intra-component
imports carry no digests; cross-component imports stay digest-bound
exactly as C022 fixed them.

Normative C025 uses `0.1.21` for package identity and dependency
resolution. `Catena.Package.Deps` vendors the SemVer 2.0.0 grammar and
precedence, the exact/caret/tilde requirement operators with Cargo-style
0.x semantics and Hex's pre-release operand restriction, single-version
highest-satisfying order-independent resolution with `PKG003` requirer
lists, `PKG002` package-graph cycle rejection, byte-deterministic
generated `catena.lock` records that replay as exact pins with `PKG005`
stale/tamper separation, and registry-neutral SHA-256 bundle digests
binding manifest semantics plus member and C024 component digests. The
0.1.7 manifest decoder accepts an optional `dependencies` object; the
engine fetches and signs nothing.

Normative C026 uses `0.1.22` for the prelude. A manifest's optional
`prelude` object names one package and one requirement; when present, the
resolved package's exports enter scope as an ordinary import-class origin
— locals win, prelude-import collisions reject as `NSP004` naming both
origins, and absent or `null` means no prelude origin at all. The
namespace environment builder accepts a `prelude:` option; the manifest
decoder validates the field (`PRE001` on malformed shapes); and
`Catena.Package.Deps` resolves and locks the prelude selection as an
ordinary dependency. Edition 0.1 guarantees zero implicit names; a future
default prelude requires an explicit lifecycle record.

Normative C027 uses `0.1.23` for entry points. A manifest's optional
`entries` array declares named, zero-argument, total, effect-closed
exports with a recorded result spelling and at most one `launch: true`
marker (`ENT001` on malformed shapes at decode and on unknown, ambiguous,
non-zero-arity, non-closed, or result-mismatched declarations at package
validation). A package with zero declared entries is a library, derived,
with absent, `null`, and `[]` equivalent. `Catena.Entry.launch/2`
invokes one declared entry to completion under the unchanged kernel
semantics — introducing no scope and spawning no process — reporting
`completed` with the entry's returned value as the shutdown result, or
`ENT002` for undeclared names and `ENT003` with the trap identity for
failures. Compilation roots, supervision, and tooling are unchanged and
remain future work.

Normative C028 uses `0.1.24` for API compatibility. `Catena.Package
Compat.diff/2` classifies the ordered diff of two decoded semantic
interfaces under the strict matrix — export, datatype, trait, instance,
effect-family, and handler removals, renames, and signature changes and
effect-row widening are breaking (`CMP002` on malformed input, `CMP003`
on unclassifiable drift); additions and row narrowing are minor;
representation and digest changes never break alone. `diff_entries/2`
classifies manifest entry sets (additions minor; removals and result
changes breaking). `validate_claim/3` enforces version claims — a
breaking diff requires major at 1.0.0+ and minor below it, with `CMP001`
for under-claims. Behavior compatibility and any BEAM ABI, wire, or
serialization contract are declared absences: the deterministic kernel
is the behavior contract, and compiled binaries are deterministic
outputs, not compatibility surfaces.

Normative C029 uses `0.1.25` for values and evaluation. Values are the
closed ten-form grammar — the kernel's integer, Boolean, Unit, tuple,
closure, constructor-value, record, injection, and opaque
process-handle forms plus Float — uniformly first-class with no tiers;
evidence, handler declarations, capability names, resumptions, traps,
effect rows, and signatures are never values. The strictness invariant
holds that every subexpression evaluates at most once, to a value or a
terminal trap, before use, with the kernel's `and`/`or` right-operand
skips as the only named exceptions and every future lazy or
multi-evaluation form gated behind an edition record. `Catena.Values`
is the total classifier over typed-core and kernel terms; the slice is
definitional and adds zero new diagnostic families.

Normative C030 uses `0.1.26` for evaluation order. One closed
ordered-forms table fixes when every existing compound evaluates: the
kernel's list elevated verbatim, plus the typed-core completions —
curried multi-argument application as repeated unary left-to-right,
trait-call subject then arguments, handler installation before body,
annotate transparency. Order is observable semantics: a conforming
implementation's effect-request trace equals the declared order's
trace, with reference-evaluator and compiled-BEAM traces agreeing per
program (the C005 dual-agreement pattern, generalized). A future-form
entry rule keeps collections and interpolation unordered until their
own slices declare entries; the `and`/`or` skips remain the only
exceptions under C029's edition-record gate. The slice is definitional:
no new public API and zero new diagnostic families.

Normative C031 uses `0.1.27` for bindings and sequencing. Local `let`
bindings are strictly non-recursive (a self-referential right-hand side
is `T001` unbound), scope is sequential-lexical with silent
innermost-wins shadowing of any in-scope name, recursion is
definitions-only with C024's SCC as mutual recursion's home, an unused
binding stays valid with its right-hand side's effects preserved, and
the let idiom (`let _ = e1; e2`) is the normative sequencing form.
`Catena.Bindings` emits the deny-able `BS001` warning for
non-underscore-prefixed binders that never occur in their body;
denial through the manifest promotes it to an error.

`catena conformance-info` writes one JSON object to standard output. The
document reports implementation identity, supported revisions, declared
choices and extensions, recommendation dispositions, bounded presentations,
every executable limit, and runtime-capacity constraints. Its key order is not
semantic; decoded content and values are deterministic for a compiler build.

## Implementation-scoped permissions

These settings disclose paths permitted by normative `MAY` clauses. They are
not implementation-defined choices: each path is already bounded by the
specification, explicitly selected where observable, or irrelevant to program
semantics.

| Governing rule | Bootstrap setting |
| --- | --- |
| [Type-System Overview — Two guarantee profiles](https://github.com/pcharbon70/catena-research/blob/main/60-specification/type-system/type-system-overview.md#two-guarantee-profiles) | Export signatures remain mandatory. The compiler does not offer its inferred private principal type as a candidate export-signature suggestion. |
| [Edition Selection — Standalone and interactive selection](https://github.com/pcharbon70/catena-research/blob/main/60-specification/editions-and-feature-lifecycle/edition-selection-and-applicability.md#standalone-and-interactive-selection) | Legacy JSON AST and manifest formats infer their historical edition/revision selection and emit the required `EDN002` advisory. |
| [Interfaces and Representation — Deterministic module interface](https://github.com/pcharbon70/catena-research/blob/main/60-specification/data-and-patterns/interfaces-and-representation.md#deterministic-module-interface) | Interface consumption is enabled for checking and compilation. Checking alone does not write an interface. |
| [Construction and Pattern Typing — Construction](https://github.com/pcharbon70/catena-research/blob/main/60-specification/data-and-patterns/construction-and-pattern-typing.md#construction) | Compact ADT layout is the default. Uniform layout remains an explicit `--layout uniform` selection; both paths preserve the required values and order. |
| [GADT Patterns — Typed-core evidence](https://github.com/pcharbon70/catena-research/blob/main/60-specification/data-and-patterns/gadt-and-existential-patterns.md#typed-core-evidence) | Coverage analysis uses locally verified GADT equalities to exclude impossible constructors. |
| [Derived Folds — Generated evidence](https://github.com/pcharbon70/catena-research/blob/main/60-specification/data-and-patterns/derived-folds.md#generated-evidence) | Generated folds use the verified ordinary lowering path rather than a separate direct-fold optimization. |
| [Lifecycle BEAM metadata](https://github.com/pcharbon70/catena-research/blob/main/60-specification/editions-and-feature-lifecycle/interfaces-artifacts-and-governance.md#beam-metadata-and-erasure) and [claim summaries](https://github.com/pcharbon70/catena-research/blob/main/60-specification/specifications-and-governance/artifacts-erasure-and-cli.md#interface-boundary) | Selection metadata is emitted in the non-executable BEAM compile-information chunk, and non-runtime claim summaries are emitted in module interfaces. |

The compiler exposes explicit native/ordinary condition lowering and
compact/uniform ADT layout controls for differential evidence. These are named
selections with common required observations, not hidden variability.

## Recommendation dispositions

The current normative corpus contains five substantive `SHOULD` clauses.
Format 1 records each implementation disposition and its follow-up instead of
silently treating a recommendation as either mandatory or irrelevant.

| Recommendation | Current disposition | Rationale and follow-up |
| --- | --- | --- |
| [Secondary diagnostic spans](https://github.com/pcharbon70/catena-research/blob/main/60-specification/type-system/diagnostics-and-conformance.md#diagnostic-contract) | Partially implemented | Every source-derived 0.1.8 syntax or static diagnostic has a primary source span. Standalone malformed-interface and forged-core results have no source form. Related secondary spans remain absent, and retained JSON inputs still carry stable paths. Tracked by P117. |
| [Task-facing “clause condition” wording](https://github.com/pcharbon70/catena-research/blob/main/60-specification/clause-conditions/diagnostics-and-conformance.md#stable-diagnostics) | Current wording deviation | Some compiler details still use implementation-facing condition/guard terms. Public wording cleanup is tracked by P117. |
| [Shared pattern matrices](https://github.com/pcharbon70/catena-research/blob/main/60-specification/data-and-patterns/match-semantics-and-coverage.md#usefulness-model) | Current performance deviation | The implementation may rebuild equivalent matrices. Required usefulness and coverage results are unchanged; sharing work is tracked by G138. |
| [Original Catena source locations](https://github.com/pcharbon70/catena-research/blob/main/60-specification/type-system/typed-core-elaboration.md#beam-only-backend-boundary) | Implemented for the kernel and source/layout/comment/literal boundaries; unavailable for JSON | The 0.1.8 parser preserves spans through verified core and Abstract Format annotations. The 0.1.9 decoder maps every logical scalar to original bytes, and the 0.1.11–0.1.13 event and literal engines preserve those spans, but no ergonomic parser yet carries them into typed core. Retained JSON revisions still have paths. Tracked by P117 and the remaining ergonomic-source gaps. |
| [Stale-preview removal edit](https://github.com/pcharbon70/catena-research/blob/main/60-specification/editions-and-feature-lifecycle/feature-lifecycle-and-compatibility.md#preview-selection) | Not implemented | The compiler diagnoses stale preview selection but does not suggest the semantics-preserving removal edit. Tracked by P125. |

No deviation in this table is permitted to change acceptance, safety,
evaluation order, runtime values or effects, stable diagnostic identity, or
artifact identity.

## Bounded unspecified presentation

The compiler uses the following unspecified presentation latitude:

- fresh type-variable spelling and equivalent constraint order may vary, but
  inferred schemes remain alpha-equivalent and typed core, stable diagnostic
  identity, and artifact identity remain unchanged; and
- diagnostic prose and optional technical-detail ordering may improve while
  the diagnostic ID, severity, stable path, meaning-bearing details, ordered
  fixes, and repair remain unchanged.

No unspecified presentation choice may affect input acceptance, safety,
runtime values, evaluation order, effects, governance decisions, signature
domains, or artifact bytes. Programmer-facing messages continue to lead with
approachable Catena words such as `variant`, `match`, `condition`, `effect`,
`request`, and `approval`. Formal conformance classes belong in specifications,
contributor documentation, and optional technical detail rather than routine
diagnostic headlines.

## Published implementation limits and analysis bounds

Each refusal limit below uses its distinct stable outcome. Condition fact
analysis instead has a conservative evidence cutoff: exhaustion returns
`unknown` and cannot establish exhaustiveness or redundancy. Neither kind of
exhaustion is a semantic counterexample or authorization for arbitrary
behavior.

| Concern | Published bound | Classification and exhaustion behavior |
| --- | --- | --- |
| Source callable arity | 253 explicit arguments | Portable implementation limit: `LIM001`. Effectful kernel workers may add two hidden arguments and reach OTP arity 255. |
| Integer literal magnitude | 4,096 decimal digits, excluding a leading minus sign | Portable implementation limit: `LIM002` |
| Decoded text or byte literal payload | 65,536 bytes | Portable implementation limit: `LIM004` |
| Decimal literal component digits | 4,096 digits across integral, fractional, and exponent components | Portable implementation limit: `LIM005` |
| Generated BEAM module | 1,048,576 bytes per module | Portable implementation limit: `LIM003` |
| Pattern usefulness and coverage | 20,000 analysis steps | Implementation limit: `M004` |
| Condition normalization and transitive inlining | 20,000 nodes/steps | Implementation limit: `CND007` |
| Condition fact analysis | 20,000 formula nodes and 20,000 branch-analysis steps | Conservative evidence cutoff: `unknown`; structural coverage remains authoritative and retains `M004` for its own limit |
| Trait resolution | 20,000 solver steps | Implementation limit: `TRT008` |
| Package specialization | 20,000 specialization steps | Implementation limit: `TRT007` |
| Specification example evaluation | 20,000 semantic steps per example | Implementation limit: `EVD003` |
| Governance policy evaluation | 20,000 policy steps | Implementation limit: `GOV002` and a denied decision |
| Kernel S-expression parsing | 20,000 syntax nodes and nesting depth 1,024 | Implementation limit: `SYN003`; no successful output |
| Kernel reference execution | 20,000 small steps by default | Evidence bound: `budget_exhausted`; not a source rejection |
| Kernel schedule exploration | 20,000 transitions and 20,000 distinct configurations | Evidence bound: `exhausted`; inconclusive rather than a semantic counterexample |

Refusal diagnostics carry `limit_id`, `minimum_supported`, `configured`,
`observed`, and `unit` details. The implementation's executable registry is
`Catena.ImplementationLimits`; production checks and `conformance-info` read
the same values so documentation cannot silently become a second configuration
source. Evidence cutoffs remain distinct: exhausting one produces `unknown`,
`budget_exhausted`, or `exhausted`, never a source-limit diagnostic.

Runtime mailbox capacity is deployment-defined rather than a compiler
message-count limit. C012 forbids capacity handling from silently reordering
messages from one sender, retargeting sends, or dropping messages addressed to
a live target. Concrete quotas, process failure, and backpressure remain owned
by G068 and G129.

## Undefined behavior and runtime failure

This implementation claims no undefined behavior. Invalid or malformed input
is rejected with no successful output publication at the affected
transactional boundary. Resource exhaustion uses the published limit outcomes
and analysis cutoff above. Specified dynamic failures—such as consuming an
affine resumption more than once or evaluating 0.1.8 `trap reason`—trap
explicitly; they do not open an arbitrary-behavior escape. A process trap is
local to that process. Sending to a dead target succeeds with Unit and drops
the message, as specified; it is not undefined behavior.

Specification silence is reported as a specification defect rather than
filled by compiler precedent. Tests, reference evaluators, guides, and this
profile remain evidence only.

## Maintaining this profile

Update this file whenever a compiler release, target, supported revision,
vendor extension, implementation-defined choice, recommendation disposition,
bounded presentation choice, or implementation limit changes. A semantic
change still requires an applicable normative revision; a profile edit alone
cannot introduce one.
