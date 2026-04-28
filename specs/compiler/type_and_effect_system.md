# Type And Effect System

## Status

Promoted status: implemented for the current compiler core, with trait resolution, higher-kinded type validation, explicit effect tracking, and the repo's implemented algebraic-effects row-polymorphism surfaces in place.

## Design Anchors

- [Current Status](../planning/current_status.md)
- [Compiler Contract](../contracts/compiler_contract.md)
- [Standard Library Surface](../stdlib/standard_library_surface.md)
- `src/compiler/types/*`
- `src/compiler/effects/*`
- `src/compiler/validation/*`
- `test/compiler/types/*`
- `test/compiler/effects/*`
- `test/compiler/semantic/catena_hkt_validation_tests.erl`

## Current Promoted Surface

- Hindley-Milner style inference is the compiler's current type-theoretic core.
- Trait constraints and instance resolution are first-class parts of typing, not add-on post-processing.
- Higher-kinded type usage is validated before declaration typing proceeds.
- Effect typing now spans both concrete effect sets and the implemented effect-row machinery used by the algebraic-effects track, including handler removal and row-variable generalization/instantiation helpers.
- Typed handlers, operation signatures, and higher-order effect support are present in the compiler/effects layers and validated by dedicated tests.
- Standard-library validation is already part of the type-system story, not a separate future concern.

## Acceptance Criteria

### AC-TES-001 Algorithm W Core

The promoted type system must provide:

- expression inference
- pattern inference
- unification with occurs checks
- polymorphic schemes and generalization where supported by the current architecture
- accumulated error reporting instead of only fail-fast behavior

This requirement is anchored in `catena_infer`, `catena_infer_expr`, `catena_infer_pattern`, `catena_infer_unify`, and the surrounding type-environment modules.

### AC-TES-002 Trait And Constraint Resolution

Trait constraints are part of the current compiler contract. The promoted implementation must continue to support:

- trait hierarchies
- trait method lookup
- instance resolution
- coherence checks
- built-in and standard-library-backed instance databases

If a constraint cannot be satisfied, the type system must fail with trait/constraint-specific diagnostics rather than silently erasing the obligation.

### AC-TES-003 Kinds And HKT Validation

Type checking of module declarations must remain gated by kind construction and higher-kinded type validation. This spec requires:

- a kind environment built from declarations
- explicit validation of higher-kinded usage before full typing
- clear separation between kind errors and ordinary type errors

### AC-TES-004 Effect Rows And Handlers

The promoted effect system for the current repo is:

- effect information is normalized across concrete sets and implemented effect rows
- functions carry effect information in their types
- operation signatures can carry effect information and row variables where the compiler/effects layers expose them
- performs introduce effects
- handlers remove handled effects
- guard expressions must be pure
- effect compatibility uses the implemented equality/subset, row operations, and effect-constraint solving surfaces rather than silently approximating obligations

This criterion reflects what the code implements today across `src/compiler/types/*` and `src/compiler/effects/*`, while avoiding claims that every experimental note is already a stable surface-language feature.

### AC-TES-005 Standard Library Validation Matters

The type/effect system is not complete unless it can validate the implemented standard-library surface. Promoted completion therefore includes:

- compilation of stdlib modules through the current typing pipeline
- trait-instance resolution against stdlib abstractions
- higher-kinded validation where stdlib abstractions require it
- support for the currently implemented operator and do-notation desugar targets

### AC-TES-006 Deferred Scope Stays Deferred

The following remain explicitly deferred and must not be implied as complete by this spec:

- full front-end exposure of every internal algebraic-effects capability
- the final ergonomics and optimization story for the entire effect system
- the final law-verification story for all abstractions
- the broader actor/distribution phases that build on top of the effect machinery

## Reconciled Note

Phase 1.5 is partially complete in the planning checklist but materially further along in the code and later summaries. The original proof-of-concept "Phase 6 effect completion" wording is now historical planning context rather than the best canonical description of the current repo. This spec follows the reconciled status recorded in `specs/planning/current_status.md`.
