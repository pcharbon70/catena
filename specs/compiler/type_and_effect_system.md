# Type And Effect System

## Status

Promoted status: implemented for the proof-of-concept core, with trait resolution, higher-kinded type validation, and monomorphic effect tracking in place. Effect polymorphism remains deferred.

## Design Anchors

- [Proof-of-concept Phase 1 plan](../../notes/planning/proof-of-concept/phase-01.md)
- [Section 1.5 standard library validation review](../../notes/reviews/section-1.5-stdlib-validation-review.md)
- [Trait instance resolution summary](../../notes/summaries/section-1.5.2-trait-instance-resolution.md)
- [HKT validation summary](../../notes/summaries/section-1.5.3-hkt-validation.md)
- [Do-notation summary](../../notes/summaries/section-1.5.5-do-notation.md)
- [Effect integration summary](../../notes/summaries/section-1.5.6-effect-integration.md)
- `src/compiler/types/*`
- `test/compiler/types/*`
- `test/compiler/semantic/catena_hkt_validation_tests.erl`

## Current Promoted Surface

- Hindley-Milner style inference is the compiler's current type-theoretic core.
- Trait constraints and instance resolution are first-class parts of typing, not add-on post-processing.
- Higher-kinded type usage is validated before declaration typing proceeds.
- Effect tracking is concrete and monomorphic for the proof-of-concept: Catena tracks explicit effect sets, but does not yet expose full effect-row polymorphism.
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

### AC-TES-004 Monomorphic Effect Tracking

The promoted effect system for the current repo is:

- effect sets are concrete and normalized
- functions carry effect information in their types
- performs introduce effects
- handlers remove handled effects
- guard expressions must be pure
- effect compatibility in the proof-of-concept is equality/subset based on concrete sets, not general row-polymorphic solving

This criterion reflects what the code implements today and prevents the specs from skipping ahead to Phase 6 semantics.

### AC-TES-005 Standard Library Validation Matters

The type/effect system is not complete unless it can validate the implemented standard-library surface. Promoted completion therefore includes:

- compilation of stdlib modules through the current typing pipeline
- trait-instance resolution against stdlib abstractions
- higher-kinded validation where stdlib abstractions require it
- support for the currently implemented operator and do-notation desugar targets

### AC-TES-006 Deferred Scope Stays Deferred

The following remain explicitly deferred and must not be implied as complete by this spec:

- full effect polymorphism
- generalized effect-row solving
- the final law-verification story for all abstractions
- the complete Phase 6 effect-system roadmap

## Reconciled Note

Phase 1.5 is partially complete in the planning checklist but materially further along in the code and later summaries. This spec follows the reconciled status recorded in `specs/planning/current_status.md`.
