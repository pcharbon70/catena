# Standard Library Surface

## Status

Promoted status: materially implemented and compiler-relevant today, with Phase 1.5 partially complete but further along than the raw planning checklist indicates.

## Design Anchors

- [ADR-0002: Minimal Core And Library-First Surface](../adr/ADR-0002-minimal-core-and-library-first-surface.md)
- [Current Status](../planning/current_status.md)
- [Flow Core Surface](flow_core_surface.md)
- `lib/catena/stdlib/prelude.cat`
- `lib/catena/stdlib/test.cat`
- `lib/catena/stdlib/laws.cat`
- `lib/catena/stdlib/effect/*`

## Current Promoted Surface

- The standard library is part of Catena's implemented language surface, not just future packaging work.
- Category-theoretic abstractions are intentionally expressed in library code and traits before becoming compiler intrinsics.
- The `Prelude`, `Test`, `Laws`, and effect modules are the main promoted stdlib surfaces today.
- The pure `System`/`Flow` core is now part of that promoted stdlib surface and is specified in [flow_core_surface.md](flow_core_surface.md).
- Minimal import support exists largely to unblock stdlib use and validation.
- Law verification is currently staged: the repo already has pure law definitions in `Laws` and `Test.verify` as the current wrapper surface, but generic property-based law verification remains future work.

## Acceptance Criteria

### AC-STDLIB-001 Library-First Authority

Catena's promoted language design is library-first for abstractions such as mapping, application, chaining, pipelines, testing, and laws. The compiler may support these abstractions, but the canonical semantic surface should live in library modules and traits whenever the current implementation allows it.

This now explicitly includes the implemented pure Flow core (`System`, `Flow`, and the current desugared Flow operators) rather than leaving Flow solely in planning documents.

### AC-STDLIB-002 Canonical Modules

The current promoted standard-library boundary includes at least:

- `Prelude`
- `Test`
- `Laws`
- the currently implemented pure Flow core within `Prelude` and `Laws`
- effect modules under `lib/catena/stdlib/effect/`

These modules are part of the working compiler/runtime story and must be treated as first-class language surface.

### AC-STDLIB-003 Desugaring Targets Library Concepts

Implemented syntax sugar such as do-notation and operator aliases must lower toward the standard library's semantic vocabulary rather than bypassing it with unrelated hidden semantics. In the current repo this especially applies to:

- `chain` and `pure`-oriented do-notation lowering
- operator forms corresponding to mapping, application, bind, and composition
- pure Flow operators `>>>`, `<<<`, `***`, and `&&&`

### AC-STDLIB-004 Reconciled Phase 1.5 Status

The promoted current stdlib-validation status is:

- 1.5.1 compilation: complete
- 1.5.2 trait instance resolution: complete
- 1.5.3 HKT validation: complete
- 1.5.4 law verification through the test module: implemented for concrete suites
- 1.5.5 do-notation desugaring: implemented
- 1.5.6 effect integration with Kleisli arrows: mostly implemented
- 1.5.7 operator desugaring: implemented

This reconciled status supersedes the stale raw checklist until the planning note is updated.

For `1.5.4`, "partial" specifically means:

- pure law transforms exist in `Laws`
- `Test.verify` is the intended current wrapper surface
- structural tests validate those definitions
- executable concrete suites and generic generator-backed law suites are still staged follow-on work

### AC-STDLIB-005 Importable And Compiler-Visible

The standard library must remain importable through the current module-loader path so compiler tests, interactive use, and future module work can rely on a stable library boundary.

### AC-STDLIB-006 Minimal Core Means Minimal Core

Changes to Catena's syntax or compiler core should prefer extending the library surface over introducing new hard-coded keywords or special forms, unless a compelling implementation reason is captured in a new ADR.

### AC-STDLIB-007 Staged Law Verification

The promoted law-verification path for Catena is staged:

1. express laws as pure standard-library definitions
2. execute concrete reusable suites through `Test.verify`
3. migrate generic trait-law verification onto the internal property-testing framework

Until later stages land, Catena MUST NOT claim that law verification is already fully generic or universally property-based.

## Out Of Scope

- versioned package distribution
- a complete future ecosystem story beyond the current repo stdlib
