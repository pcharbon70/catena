# Core Erlang And BEAM Backend Hardening

**Description:** This roadmap implements the fail-closed,
semantics-preserving backend accepted by
[ADR-0005](../../adr/ADR-0005-fail-closed-semantics-preserving-beam-backend.md)
and specified by the
[Core Erlang and BEAM backend contract](../../compiler/core_erlang_and_beam_backend.md).
It closes the gap between the Catena surface accepted by the frontend and the
subset that can currently reach executable BEAM without unresolved calls,
placeholder values, wildcard approximation, or silently omitted runtime
behavior.

## Status Rules

- Planned work uses unchecked phase, section, task, and subtask boxes.
- A task is complete only when its focused tests and backend behavior are
  reviewable in the repository.
- A section is complete only when its implementation and tests preserve all
  earlier phase gates.
- Every phase ends with a numbered integration-test section.
- A phase is complete only when its integration-test section passes together
  with `make check-specs` and the complete active EUnit suite.
- A Core Erlang term that has not passed OTP `from_core` compilation is not a
  successful executable backend artifact.
- A code-generation unit test does not promote a source construct to
  BEAM-supported status without source-to-BEAM execution evidence.

## Starting Baseline

The roadmap starts from the backend analysis and promoted specs recorded on
2026-07-24:

- simple exported transforms and arithmetic compile from Catena source to
  loadable BEAM
- nullary and unary algebraic data constructors and multi-clause constructor
  matching compile and execute
- a basic handled effect can compile and execute through the Catena runtime
- calls between named transforms can fail Core validation with an unbound
  variable
- unknown expressions can become placeholder error tuples
- unknown or misplaced patterns can become wildcards
- runtime-bearing declarations can be omitted without an explicit disposition
- executable import linkage, trait dictionary dispatch, and application-level
  test/property artifacts are incomplete
- the public compiler API stops at typed modules or Core Erlang rather than an
  accepted in-memory BEAM result

## Implementation Principles

1. Fail closed before expanding the promoted supported surface.
2. Preserve the typed frontend as the authority for every backend artifact.
3. Resolve symbols and arities before emitting Core Erlang calls.
4. Classify every declaration, expression, pattern, and operator explicitly.
5. Erase static information only after runtime representation decisions.
6. Keep runtime-backed behavior behind explicit, resolved module calls.
7. Add positive execution evidence and negative rejection evidence together.
8. Keep each phase independently mergeable and leave later-phase failures
   visible rather than approximating them.

## Phases

- [x] [Phase 1: Backend Safety Baseline And Fail-Closed Diagnostics](phase-01-backend-safety-baseline-and-fail-closed-diagnostics.md)
- [x] [Phase 2: Validated Compilation Unit And Declaration Disposition](phase-02-validated-compilation-unit-and-declaration-disposition.md)
- [x] [Phase 3: Local And Higher-Order Call Resolution](phase-03-local-and-higher-order-call-resolution.md)
- [x] [Phase 4: Exhaustive Expression, Pattern, And Data Lowering](phase-04-exhaustive-expression-pattern-and-data-lowering.md)
- [x] [Phase 5: Effect And Runtime-Backed Semantics](phase-05-effect-and-runtime-backed-semantics.md)
- [x] [Phase 6: Module Linkage, Imported Calls, And Trait Dispatch](phase-06-module-linkage-imported-calls-and-trait-dispatch.md)
- [ ] [Phase 7: Public BEAM API And Conformance Enforcement](phase-07-public-beam-api-and-conformance-enforcement.md)

## Requirement And Scenario Traceability

| Roadmap area | Primary requirements | Scenarios |
| --- | --- | --- |
| Validated backend authority | `REQ-COMP-008` | `SCN-002`, `SCN-003`, `SCN-011` |
| Call and linkage resolution | `REQ-COMP-009` | `SCN-003`, `SCN-011` |
| Fail-closed exhaustive lowering | `REQ-COMP-010` | `SCN-003`, `SCN-006`, `SCN-011` |
| Runtime semantic preservation | `REQ-COMP-011` | `SCN-003`, `SCN-004`, `SCN-011` |
| Public BEAM artifacts | `REQ-COMP-012` | `SCN-003`, `SCN-011` |
| Source-oriented diagnostics | `REQ-COMP-013` | `SCN-001`, `SCN-003`, `SCN-011` |
| Executable feature evidence | `REQ-TEST-008` | `SCN-011` |

## Roadmap Completion Gate

- [ ] Every frontend construct has a documented backend support class
- [ ] Named local, forward, recursive, higher-order, and imported calls resolve
      correctly
- [ ] Unknown expressions, patterns, declarations, and operators fail closed
- [ ] Static erasure and runtime lowering are explicit and ordered
- [ ] Promoted effect and trait behavior executes through accepted runtime
      representations
- [ ] Public source-to-BEAM APIs return only OTP-accepted binaries
- [ ] Backend errors preserve source identity and location
- [ ] `SCN-011` has positive and negative evidence for the complete promoted
      backend surface
- [ ] `make check-specs`, `make conformance`, and the complete active suite pass
- [ ] Compiler, tooling, and current-status specs record the verified final
      boundary
