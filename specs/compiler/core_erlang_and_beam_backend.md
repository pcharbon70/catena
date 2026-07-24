# Core Erlang And BEAM Backend

## Status

Promoted target: accepted architecture with Phases 1 through 4 implemented:
backend safety, validated-unit/declaration-disposition enforcement, local and
higher-order call resolution, and exhaustive pure expression, pattern, and
data lowering. Later runtime, linkage, and public-artifact phases remain
partial or planned.

The current repository proves a working source-to-BEAM vertical slice for
representative transforms, arithmetic, algebraic data constructors, and
constructor-pattern dispatch. Local, forward, self-recursive, mutually
recursive, higher-order, and constructor calls are proven through executable
source programs. The same evidence now covers primitive pure operators,
collections, records, field access, parser-native patterns, guards, aliases,
or-patterns, representation round trips, and fail-closed type erasure. The
repository does not yet satisfy the complete backend contract in this spec.

## Purpose

This spec defines when a Catena source-language construct may be described as
BEAM-supported. It covers the boundary from a validated Catena compilation
unit through Core Erlang lowering, BEAM binary generation, module loading, and
observable execution.

It distinguishes:

- static constructs that are intentionally erased
- runtime constructs with a defined BEAM representation
- runtime constructs whose lowering is still partial
- frontend constructs that must be rejected until backend support exists

## Design Anchors

- [ADR-0005: Fail-Closed, Semantics-Preserving BEAM Backend](../adr/ADR-0005-fail-closed-semantics-preserving-beam-backend.md)
- [Compiler Contract](../contracts/compiler_contract.md)
- [Testing And Quality Contract](../contracts/testing_and_quality_contract.md)
- [Core Compiler Pipeline](core_compiler_pipeline.md)
- [BEAM Backend Feature Ledger](beam_backend_feature_ledger.md)
- [Pattern Matching Engine](pattern_matching_engine.md)
- [Effect Runtime](../runtime/effect_runtime.md)
- [Backend Hardening Implementation Plan](../planning/backend-hardening/README.md)
- `src/compiler/catena_compile.erl`
- `src/compiler/catena_compilation_unit.erl`
- `src/compiler/codegen/catena_declaration_disposition.erl`
- `src/compiler/codegen/catena_call_resolution.erl`
- `src/compiler/codegen/catena_codegen_lower.erl`
- `src/compiler/codegen/catena_codegen_erase.erl`
- `src/compiler/codegen/catena_codegen_module.erl`
- `src/compiler/codegen/catena_codegen_expr.erl`
- `src/compiler/codegen/catena_codegen_pattern.erl`
- `src/compiler/codegen/catena_effect_codegen.erl`
- `test/compiler/integration/catena_core_pipeline_tests.erl`
- `test/compiler/integration/catena_backend_hardening_phase1_tests.erl`
- `test/compiler/integration/catena_backend_hardening_phase2_tests.erl`
- `test/compiler/integration/catena_backend_hardening_phase3_tests.erl`
- `test/compiler/integration/catena_backend_hardening_phase4_tests.erl`

## Compilation Boundary

The promoted backend workflow is:

```text
Catena source
  -> lexical and syntax validation
  -> semantic normalization and desugaring
  -> import, kind, type, trait, and effect validation
  -> validated compilation unit
  -> symbol and call resolution
  -> backend AST lowering
  -> representation selection and static erasure
  -> Core Erlang emission and validation
  -> OTP from_core compilation
  -> BEAM binary
```

`compile_string/1,2` remains the typed-module API.
`compile_string_to_core/1,2` and `compile_file_to_core/1,2` remain explicit
Core Erlang APIs. They now share `compile_string_to_unit/1,2`, which constructs
the validated backend authority only after the canonical frontend succeeds.
Backend hardening later adds in-memory BEAM APIs with the same authority:

```erlang
catena_compile:compile_string_to_beam(Source).
catena_compile:compile_string_to_beam(Source, Options).
catena_compile:compile_file_to_beam(Path).
catena_compile:compile_file_to_beam(Path, Options).
```

The success result must identify the module and carry the BEAM binary. The
exact options and diagnostic envelope may evolve, but a success result must
not be returned until OTP accepts the emitted Core Erlang.

## Backend Support Classes

| Class | Meaning | Required compiler behavior |
| --- | --- | --- |
| Supported | Runtime semantics are lowered and executable. | Emit Core Erlang and maintain source-to-BEAM evidence. |
| Static-erased | The construct has no runtime identity after validation. | Record the erasure disposition and omit it only after validation and representation selection. |
| Runtime-lowered | The construct executes through a Catena runtime/library boundary. | Emit an explicit, resolved call and include the runtime dependency in conformance evidence. |
| Deferred | The frontend or research surface exists but the backend contract is incomplete. | Reject application artifact generation with a structured unsupported-construct diagnostic. |

No fifth implicit class exists for placeholders, wildcard substitution, or
silent omission.

## Runtime Representation

| Catena surface | BEAM/Core representation | Current compliance |
| --- | --- | --- |
| Module and transform definitions | Core Erlang module and function definitions | Proven for accepted local pure transforms, including guarded and multi-clause definitions. |
| Primitive literals | Native BEAM terms | Proven for the parser-native pure literal surface. |
| Algebraic data constructors | Tagged tuples `{Constructor, ...}` | Proven for nullary, unary, and higher-arity constructors with exact arity validation. |
| Lists and tuples | Native BEAM lists and tuples | Proven through construction, operations, and pattern round trips. |
| Structural records | BEAM maps keyed by field atoms | Proven for construction, exact-key pattern matching, field access, and missing-key behavior. |
| Lambdas and higher-order values | Core Erlang functions and `apply` | Proven for lambda parameters, let-bound functions, returned functions, and named transforms used as values. |
| Local, forward, and recursive transform calls | Resolved Core Erlang function references | Proven for direct, forward, self-recursive, and mutually recursive calls. |
| Imported transform calls | Resolved Core Erlang module calls | Deferred pending executable module linkage. |
| Pattern matching and guards | Core Erlang cases and clauses | Proven for every promoted parser-native pattern, recursive or-pattern expansion, bindings, pure guard conjunction, and source clause order. |
| Type declarations and annotations | Static-erased after validation | Explicit, exhaustive, and fail-closed after disposition and representation selection. |
| Effect declarations | Static-erased metadata | Explicit after operation metadata is retained; runtime effect execution remains a Phase 5 concern. |
| `perform` and `handle` | Explicit calls to the Catena effect runtime with context passing | Partial: lowering and runtime paths exist; promoted source-to-BEAM coverage is incomplete. |
| Traits and instances | Resolved runtime dictionaries | Deferred: validation and dictionary integration are incomplete. |
| Test and property declarations | Explicit testing artifacts or runner registrations | Deferred: declarations are currently omitted from application emission. |
| Actor/process surface | Explicit BEAM process/runtime operations | Outside the accepted frontend/backend surface until source-language integration exists. |

This table records the current baseline, not permission to keep partial
behavior indefinitely. A row moves to supported only when its acceptance
criteria and executable evidence are satisfied.

## Call Resolution

Call resolution happens before Core Erlang expression emission.

### Local Transform Calls

A call to a known local transform must resolve to its module-local identity and
arity. Resolution must not depend on source declaration order and must support:

- calls to earlier declarations
- forward calls
- self-recursion
- mutual recursion
- unambiguous name and arity validation; the current language rejects
  overloaded local callable names

### Imported Calls

An imported call must carry the resolved source module, runtime module
identity, transform name, and arity. Type-environment import success alone is
not sufficient for executable linkage.

### Higher-Order Calls

Only a value whose expression type is callable may lower as a closure
application. A bare source identifier must not be assumed to be a closure when
it resolves to a top-level or imported transform.

### Constructors And Trait Methods

Constructor applications and trait method calls require their own resolved
identities. Trait methods must not be lowered as ordinary local calls unless
resolution has selected a concrete implementation with equivalent semantics.

## Declaration Disposition

Before module emission, each declaration receives one disposition:

| Declaration | Required disposition |
| --- | --- |
| Implemented transform | Lower to a Core Erlang function. |
| Signature without implementation | Static-erased only when no executable export or call requires it; otherwise reject as missing implementation. |
| Type declaration | Static-erased after constructor representation and pattern metadata are fixed. |
| Effect declaration | Static-erased after operation validation and runtime call metadata are fixed. |
| Trait declaration | Static-erased only after dispatch metadata is fixed. |
| Instance declaration | Lower to a valid dictionary/dispatch artifact or reject. |
| Import declaration | Lower into resolved linkage metadata; it is not a runtime function. |
| Test/property declaration | Lower through the testing artifact contract or reject for application emission. |

Unknown declarations are always rejected.

Phase 2 implements this boundary with an explicit disposition pass. Implemented
transforms lower; type and effect declarations erase only after their
constructor/operation metadata is retained; unused non-exported signatures may
erase; exported signatures without implementations fail; and imports, traits,
instances, tests, properties, and unknown declarations remain explicit
unsupported dispositions until their later runtime/linkage contracts exist.

## Diagnostics

Backend failures must retain the source stage, construct, and location whenever
available. Stable diagnostic categories include:

- `unsupported_backend_construct`
- `unresolved_call`
- `ambiguous_call`
- `arity_mismatch`
- `missing_transform_implementation`
- `invalid_declaration_disposition`
- `core_validation_failed`
- `beam_compilation_failed`

Generated placeholder terms are not diagnostics.

## Acceptance Criteria

### AC-BEAM-001 Validated Backend Authority

Public Core Erlang and BEAM APIs must run the complete validated frontend and
must not generate an application artifact after a semantic, import, kind,
type, trait, or effect error.

### AC-BEAM-002 Resolved Calls

Local, forward, self-recursive, mutually recursive, imported, constructor, and
higher-order calls must be classified before emission. Every supported call
must produce valid Core Erlang with the correct target and arity; unresolved or
ambiguous calls must produce structured errors.

### AC-BEAM-003 Exhaustive Fail-Closed Lowering

Every normalized declaration, expression, pattern, and operator reaching the
backend must be lowered, intentionally erased, runtime-lowered, or rejected.
Unknown expressions must not become placeholder values, patterns must not
degrade to wildcards, and runtime-bearing declarations must not disappear.

### AC-BEAM-004 Stable Runtime Representations

Supported values and operations must follow the representation table in this
spec. Representation changes that affect interoperability or observable
semantics require an ADR update and corresponding executable evidence.

### AC-BEAM-005 Declaration Disposition

Module emission must classify every declaration before filtering. Static
erasure must happen only after validation and representation selection.
Deferred runtime-bearing declarations must fail application artifact
generation with a source-oriented diagnostic.

### AC-BEAM-006 Core And BEAM Artifact Validation

A successful BEAM API result requires emitted Core Erlang to pass OTP
`from_core` compilation. Core lint or BEAM compiler errors must be returned as
structured compiler errors rather than crashes or successful placeholder
artifacts.

### AC-BEAM-007 Source-Oriented Backend Diagnostics

Backend errors must identify the failing construct and retain its Catena source
location when one exists. Diagnostics must not expose only generated Core
Erlang names when a source identity is available.

### AC-BEAM-008 Executable Feature Conformance

Every row promoted to supported must have at least one positive source-to-BEAM
execution test. Every deferred or invalid path that previously risked silent
approximation must have negative evidence showing that artifact generation is
rejected.

### AC-BEAM-009 Current Compliance Is Explicit

Documentation and release/status claims must distinguish the proven
source-to-BEAM subset from lowering-only, partial, and deferred surfaces. A
green code-generation unit test or successful Core term construction alone
does not promote a Catena feature to BEAM-supported status.

## Conformance Evidence Strategy

`SCN-011` owns the executable BEAM backend contract. Phase 1 evidence includes
the arithmetic and constructor-pattern vertical slice plus the dedicated
fail-closed backend suite. Phase 2 adds validated-unit construction, frontend
failure exclusion, declaration classification, representation-before-erasure,
and missing/deferred-runtime rejection evidence. Phase 3 adds direct, forward,
self-recursive, mutually recursive, closure, named-function-value, and
constructor-arity execution and rejection evidence. Phase 4 adds primitive
pure expressions and operators, collections, records, field access, every
promoted parser-native pattern, guard and clause behavior, representation
round trips, exhaustive erasure, and negative fail-closed paths. Later phases
extend that suite to cover:

- local, forward, recursive, imported, and higher-order calls
- basic effect performance and handling
- trait dispatch when promoted
- explicit rejection of deferred tests, properties, imports, traits, or actor
  constructs until their runtime contracts are implemented
- Core validation failures and source-oriented diagnostics

## Out Of Scope

- backend optimization and specialization
- application packaging and release assembly
- native-code generation outside the BEAM toolchain
- a full module/package system design
- actor syntax design
- preserving static Catena types as runtime BEAM values
