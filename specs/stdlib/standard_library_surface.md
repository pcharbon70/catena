# Standard Library Surface

## Status

Promoted status: materially implemented and compiler-relevant today, with Phase 1.5 partially complete but further along than the raw planning checklist indicates.

## Design Anchors

- [Standard library overview](../../notes/guides/standard-library-overview.md)
- [Minimal core keywords](../../notes/features/minimal-core-keywords.md)
- [Minimal core keywords summary](../../notes/summaries/minimal-core-keywords.md)
- [Basic module imports](../../notes/features/basic-module-imports.md)
- [Standard library validation review](../../notes/reviews/section-1.5-stdlib-validation-review.md)
- [Section 1.5.1 summary](../../notes/summaries/section-1.5.1-stdlib-compilation-complete.md)
- [Section 1.5.2 summary](../../notes/summaries/section-1.5.2-trait-instance-resolution.md)
- [Section 1.5.3 summary](../../notes/summaries/section-1.5.3-hkt-validation.md)
- [Section 1.5.5 summary](../../notes/summaries/section-1.5.5-do-notation.md)
- [Section 1.5.6 summary](../../notes/summaries/section-1.5.6-effect-integration.md)
- `lib/catena/stdlib/prelude.cat`
- `lib/catena/stdlib/test.cat`
- `lib/catena/stdlib/laws.cat`
- `lib/catena/stdlib/effect/*`

## Current Promoted Surface

- The standard library is part of Catena's implemented language surface, not just future packaging work.
- Category-theoretic abstractions are intentionally expressed in library code and traits before becoming compiler intrinsics.
- The `Prelude`, `Test`, `Laws`, and effect modules are the main promoted stdlib surfaces today.
- Minimal import support exists largely to unblock stdlib use and validation.

## Acceptance Criteria

### AC-STDLIB-001 Library-First Authority

Catena's promoted language design is library-first for abstractions such as mapping, application, chaining, pipelines, testing, and laws. The compiler may support these abstractions, but the canonical semantic surface should live in library modules and traits whenever the current implementation allows it.

### AC-STDLIB-002 Canonical Modules

The current promoted standard-library boundary includes at least:

- `Prelude`
- `Test`
- `Laws`
- effect modules under `lib/catena/stdlib/effect/`

These modules are part of the working compiler/runtime story and must be treated as first-class language surface.

### AC-STDLIB-003 Desugaring Targets Library Concepts

Implemented syntax sugar such as do-notation and operator aliases must lower toward the standard library's semantic vocabulary rather than bypassing it with unrelated hidden semantics. In the current repo this especially applies to:

- `chain` and `pure`-oriented do-notation lowering
- operator forms corresponding to mapping, application, bind, and composition

### AC-STDLIB-004 Reconciled Phase 1.5 Status

The promoted current stdlib-validation status is:

- 1.5.1 compilation: complete
- 1.5.2 trait instance resolution: complete
- 1.5.3 HKT validation: complete
- 1.5.4 law verification through the test module: partial
- 1.5.5 do-notation desugaring: implemented
- 1.5.6 effect integration with Kleisli arrows: mostly implemented
- 1.5.7 operator desugaring: implemented

This reconciled status supersedes the stale raw checklist until the planning note is updated.

### AC-STDLIB-005 Importable And Compiler-Visible

The standard library must remain importable through the current module-loader path so compiler tests, interactive use, and future module work can rely on a stable library boundary.

### AC-STDLIB-006 Minimal Core Means Minimal Core

Changes to Catena's syntax or compiler core should prefer extending the library surface over introducing new hard-coded keywords or special forms, unless a compelling implementation reason is captured in a new ADR.

## Out Of Scope

- versioned package distribution
- a complete future ecosystem story beyond the current repo stdlib
