# Flow Core Surface

## Status

Promoted status: partially implemented as a library-first standard-library surface. The pure Flow core is now present in `Prelude`, `Laws`, compiler kind validation, and operator parsing/desugaring, while instances, choice/apply extensions, and later Flow libraries remain planned.

## Design Anchors

- [ADR-0004: Flow as pragmatic name for Arrow](../adr/ADR-0004-flow-as-arrow.md)
- [Flow Phase 1 plan](../planning/flow/phase-01-flow-core-trait-and-system-foundation.md)
- [Standard library surface](standard_library_surface.md)
- `lib/catena/stdlib/prelude.cat`
- `lib/catena/stdlib/laws.cat`
- `src/compiler/semantic/catena_kind.erl`
- `src/compiler/lexer/catena_lexer.xrl`
- `src/compiler/parser/catena_parser.yrl`
- `src/compiler/semantic/catena_desugar.erl`
- `test/compiler/semantic/catena_hkt_validation_tests.erl`
- `test/compiler/integration/catena_stdlib_compilation_tests.erl`
- `test/compiler/integration/catena_stdlib_integration_tests.erl`
- `test/compiler/integration/catena_stdlib_laws_tests.erl`
- `test/compiler/parser/catena_parser_negative_tests.erl`

## Current Promoted Surface

- `Prelude` now exports `System` and `Flow` as real standard-library traits rather than plan-only vocabulary.
- The current pure Flow core surface is:
  - `System`: `id`, `compose`
  - `Flow`: `lift`, `first`, `parallel`, `split`
- The compiler front end now recognizes the pure Flow operator family `>>>`, `<<<`, `***`, and `&&&`.
- Those operators lower toward library vocabulary rather than hidden compiler-only semantics:
  - `>>>` and `<<<` desugar to `compose`
  - `***` desugars to `parallel`
  - `&&&` desugars to `split`
- The compiler's kind-validation surface now supports the higher-arity trait usage needed for `System arr` and `Flow arr`.
- `Laws` now contains a real pure Flow/core-category law surface rather than only older mapper/pipeline laws.
- The promoted implemented boundary stops at the pure core. The repo does not yet promote:
  - `FlowChoice`
  - `FlowApply`
  - core Flow instances
  - flow-specific utility modules
  - stream/circuit Flow libraries
  - a settled effectful Flow interpretation beyond the pure-core design constraints

## Acceptance Criteria

### AC-FLOW-001 Library-First Core

Catena's promoted Flow core is a standard-library surface first. `System` and `Flow` belong in `Prelude`, and compiler support for Flow operators or kinds MUST continue to serve that library-owned vocabulary rather than replace it with unrelated intrinsic semantics.

### AC-FLOW-002 Pure Core Trait Shape

The current promoted trait surface is:

- `System` with `id : arr a a` and `compose : arr b c -> arr a b -> arr a c`
- `Flow` extending `System`
- `Flow` with `lift : (a -> b) -> arr a b`
- `Flow` with `first : arr a b -> arr (a, c) (b, c)`
- `Flow` with the currently promoted derived/core operations `parallel` and `split`

Any claim that `FlowChoice`, `FlowApply`, or other later Flow traits are already canonical would exceed the current implemented surface.

### AC-FLOW-003 Operator Surface

The promoted pure Flow operator surface is limited to:

- `>>>`
- `<<<`
- `***`
- `&&&`

These operators MUST parse and desugar to the corresponding library vocabulary already present in the current repo.

### AC-FLOW-004 Higher-Arity Trait Support

The promoted compiler surface for Flow includes the higher-arity kind-validation support needed for binary-kinded `arr` usage in `System` and `Flow`. The canonical status of the pure Flow core therefore depends not only on stdlib text existing, but also on the compiler validating that trait shape in its current HKT surface.

### AC-FLOW-005 Law Surface

The current promoted Flow/core-category law surface in `Laws` includes:

- `systemLeftIdentityLaw`
- `systemRightIdentityLaw`
- `systemAssociativityLaw`
- `flowLiftIdentityLaw`
- `flowLiftCompositionLaw`
- `flowFirstLiftLaw`
- `flowFirstCompositionLaw`
- `flowFirstProjectionLaw`
- `flowFirstNaturalityLaw`
- `flowFirstAssociativityLaw`

These laws are part of the implemented structural library surface today even though generic instance-backed law execution remains a wider staged concern.

### AC-FLOW-006 Reconciled Scope Boundary

The promoted current status for Flow is:

- pure Flow core: materially implemented
- function and other stdlib instances: not yet promoted as implemented
- FlowChoice/FlowApply: planned
- utility/stream/circuit/example phases: planned

This boundary exists so the specs do not collapse "foundational Flow surface now exists" into the false claim that the full 8-phase track is already done.

## Out Of Scope

- claiming that the full Flow roadmap is implemented
- claiming that FlowChoice or FlowApply are already part of the canonical current surface
- claiming that Flow instances for functions, `Maybe`, `Either`, `Result`, or `List` are already present
- claiming effect-sensitive Flow optimization rules are already implemented beyond the current pure-core boundaries
