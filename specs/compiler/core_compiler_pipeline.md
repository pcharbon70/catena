# Core Compiler Pipeline

## Status

Promoted status: implemented through typed-module compilation, explicit
type-validated source-to-Core Erlang APIs, and public validated in-memory BEAM
APIs for source strings, files, and dependency-ordered source sets. Every
backend handoff is a validated compilation unit.

## Purpose

This spec promotes the compiler pipeline that Catena currently ships in code.
It covers the typed-module API and the public path from source text through
frontend validation to Core Erlang and OTP-accepted BEAM output.

## Design Anchors

- [Catena Design Baseline](../design.md)
- [Current Status](../planning/current_status.md)
- [Compiler Contract](../contracts/compiler_contract.md)
- [Core Erlang And BEAM Backend](core_erlang_and_beam_backend.md)
- [ADR-0005: Fail-Closed, Semantics-Preserving BEAM Backend](../adr/ADR-0005-fail-closed-semantics-preserving-beam-backend.md)
- [ADR-0006: First-Class Resumptions Through Selective CPS](../adr/ADR-0006-first-class-resumptions-and-selective-cps.md)
- `src/compiler/catena_compile.erl`
- `src/compiler/catena_module_loader.erl`
- `src/compiler/semantic/*`
- `src/compiler/codegen/*`
- `test/compiler/integration/catena_pipeline_integration_tests.erl`
- `test/compiler/integration/catena_core_pipeline_tests.erl`
- `test/compiler/codegen/*`

## Current Promoted Surface

- `catena_compile:compile_string/1,2` and `compile_file/1` are the canonical top-level compiler entry points.
- The public compile path currently returns `{ok, {typed_module, Name, TypedDecls, Env}}` or a stage-specific error.
- `compile_string_to_core/1,2` and `compile_file_to_core/1,2` reuse the same
  frontend and type-checking path, then return a Core Erlang module.
- `compile_string_to_beam/1,2`, `compile_file_to_beam/1,2`, and
  `compile_source_set_to_beam/1,2` reuse the validated unit, require explicit
  Core validation and OTP `from_core` compilation, and return stable versioned
  in-memory artifacts without internal compilation-unit state.
- `compile_string_to_unit/1,2` is the maintained compiler-internal handoff for
  artifact backends. It retains normalized source, typed declarations, the
  effective environment, imports, exports, options, validation evidence,
  symbols, the complete module-local callable inventory, locations,
  authoritative control modes, validated selective-CPS IR, and declaration
  dispositions.
- Import processing publishes versioned module interfaces and resolves open,
  qualified, aliased, selective, and dotted imports to executable symbols and
  artifact dependencies. The compiler-internal closed-source-set path orders
  dependencies and emits remote calls and closures from those bindings.
- `catena_codegen_lower` is the explicit canonical-AST-to-backend boundary.
- local transform and constructor identities are predeclared before
  expression emission, so direct, forward, recursive, and higher-order calls
  do not depend on declaration order
- The typed-module and Core APIs share one frontend-success assembly path.
  Production Core generation accepts the resulting validated unit; direct raw
  AST generation remains a documented low-level codegen test helper.

## Implemented Control-IR Boundary

ADR-0006's compiler boundary now runs after validated typing, effect, trait,
import, and call analysis and before ordinary Core Erlang expression
emission. `catena_control_mode` classifies callables and regions as direct or
resumable; `catena_selective_cps` lowers the graph into `catena_control_ir`;
`catena_control_abi` defines entries, closures, final continuations, and
bridges; and `catena_control_validate` must produce a retained passing report
before declaration projection.

The boundary is specified in
[Delimited Resumption Architecture](delimited_resumption_architecture.md).
It is compiler IR, not runtime promotion evidence. Explicit resumptions remain
fail-closed at Core emission until the deep one-shot runtime and Core lowering
phases consume this graph.

## Acceptance Criteria

### AC-CPIPE-001 Stage-Oriented Compilation

Catena source must flow through the following promoted stages in order:

1. lexical analysis
2. parsing
3. semantic analysis and desugaring
4. kind and higher-kinded-type validation
5. type/effect checking

Each stage must fail with its own error family rather than collapsing everything into a generic compiler failure.

### AC-CPIPE-002 Typed Module Boundary

The canonical success artifact of the current top-level compiler API is a typed module carrying:

- module identity
- typed declarations
- the merged type environment used for later compilation or interactive work

The typed-module API remains the canonical interactive/compiler-analysis
boundary. Callers that need a backend artifact use the explicit Core or BEAM
APIs.

### AC-CPIPE-003 Executable Import Resolution

The promoted module boundary for the current compiler is:

- imports are resolved through `catena_module_loader`
- default search paths include the Catena standard library and current working directory
- only exported symbols are visible from imported modules
- local definitions shadow imported definitions
- application artifacts resolve imported transforms by versioned interface,
  source/runtime module identity, kind, name, and arity
- dependency-ordered source sets reject missing modules and unsupported cycles
  before artifact emission

This executable linkage boundary does not imply that the full planned Phase 4
package/module system, separate compilation workflow, or release packaging is
complete.

### AC-CPIPE-004 Semantic Normalization Before Typing

Semantic normalization must remain part of the promoted compiler design. In practice this means:

- declaration grouping and structural validation happen before typing
- do-notation and related surface sugar are lowered before type checking
- later compiler phases can rely on normalized AST forms rather than re-implementing surface syntax rules

### AC-CPIPE-005 Separate But Real Codegen Surface

Core Erlang generation modules under `src/compiler/codegen/` are part of the
promoted compiler design. The explicit Core API requires:

- module generation from canonical analyzed Catena module ASTs
- expression and pattern lowering support
- type erasure support where needed
- preservation of parser-native transform exports
- successful Core-to-BEAM validation for supported source programs

### AC-CPIPE-006 Honest Scope

Any compiler-facing status or design document must describe the current pipeline as:

- materially implemented through typed modules
- able to emit validated Core Erlang through an explicit public API
- able to compile source strings, files, and closed module sets to accepted
  in-memory BEAM artifacts through public APIs
- still short of on-disk emission, packaging, and a polished executable build
  workflow

This criterion exists to keep the promoted spec aligned with the actual code instead of the aspirational roadmap alone.

### AC-CPIPE-007 Fail-Closed Backend Handoff

The public Core Erlang and BEAM APIs must hand the backend a validated
compilation unit with the source, symbol, arity, type, declaration-disposition,
callable-resolution, and location information required by
[Core Erlang And BEAM Backend](core_erlang_and_beam_backend.md). Passing type
validation and then lowering an unrelated or insufficiently described AST is
not the completed backend boundary.

## Out Of Scope

- complete package/module system semantics
- backend optimization passes
- distribution-layer compilation concerns
