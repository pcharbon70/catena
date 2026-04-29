# Core Compiler Pipeline

## Status

Promoted status: implemented through typed-module compilation, with code-generation components present and tested as separate compiler surfaces.

## Purpose

This spec promotes the compiler pipeline that Catena currently ships in code. It covers the implemented path from source text through typed module output, plus the existing Core Erlang generation components that sit just beyond the public `catena_compile` API.

## Design Anchors

- [Catena Design Baseline](../design.md)
- [Current Status](../planning/current_status.md)
- [Compiler Contract](../contracts/compiler_contract.md)
- `src/compiler/catena_compile.erl`
- `src/compiler/catena_module_loader.erl`
- `src/compiler/semantic/*`
- `src/compiler/codegen/*`
- `test/compiler/integration/catena_pipeline_integration_tests.erl`
- `test/compiler/codegen/*`

## Current Promoted Surface

- `catena_compile:compile_string/1,2` and `compile_file/1` are the canonical top-level compiler entry points.
- The public compile path currently returns `{ok, {typed_module, Name, TypedDecls, Env}}` or a stage-specific error.
- Import processing is intentionally minimal: Catena supports loading modules from the standard library and current project search paths, then merging exported environments into the local type environment.
- Code generation to Core Erlang exists in dedicated modules and tests, even though the top-level compile API is not yet a single "emit artifact" command.

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

This spec does not require the public pipeline to emit BEAM artifacts directly today.

### AC-CPIPE-003 Minimal Import Resolution

The promoted module boundary for the current compiler is:

- imports are resolved through `catena_module_loader`
- default search paths include the Catena standard library and current working directory
- only exported symbols are visible from imported modules
- local definitions shadow imported definitions

This is sufficient for current stdlib integration and does not imply that the full planned Phase 4 module system is complete.

### AC-CPIPE-004 Semantic Normalization Before Typing

Semantic normalization must remain part of the promoted compiler design. In practice this means:

- declaration grouping and structural validation happen before typing
- do-notation and related surface sugar are lowered before type checking
- later compiler phases can rely on normalized AST forms rather than re-implementing surface syntax rules

### AC-CPIPE-005 Separate But Real Codegen Surface

Core Erlang generation modules under `src/compiler/codegen/` are part of the promoted compiler design, even though they are not yet the default `catena_compile` output boundary. The spec requires:

- module generation from Catena module ASTs
- expression and pattern lowering support
- type erasure support where needed
- dedicated tests for generated Core Erlang structure

### AC-CPIPE-006 Honest Scope

Any compiler-facing status or design document must describe the current pipeline as:

- materially implemented through typed modules
- supported by tested code-generation components
- still short of a single polished artifact-emission workflow

This criterion exists to keep the promoted spec aligned with the actual code instead of the aspirational roadmap alone.

## Out Of Scope

- complete package/module system semantics
- backend optimization passes
- distribution-layer compilation concerns
