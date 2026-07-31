# First-Class Delimited Resumptions

**Description:** This roadmap implements the source language, type system,
selective-CPS compiler path, runtime representation, BEAM lowering, and
promotion evidence accepted by
[ADR-0006](../../adr/ADR-0006-first-class-resumptions-and-selective-cps.md)
and specified by the
[Delimited Resumption Architecture](../../compiler/delimited_resumption_architecture.md).
It closes the gap between Catena's current request/response effect handlers
and true handler-visible resumptions that execute the remainder of a compiled
Catena computation.

## Status Rules

- Planned work uses unchecked phase, section, task, and subtask boxes.
- Existing `catena_resumption`, deep/shallow, one-shot/multi-shot, and handler
  helper modules are inputs to the work, not evidence that a phase is complete.
- A task is complete only when its implementation, focused tests, source
  origins, and failure behavior are reviewable in the repository.
- A section is complete only when its implementation preserves every earlier
  phase gate.
- Every phase ends with a numbered integration-test section.
- A phase is complete only when its integration-test section passes with
  `make check-specs` and the complete active EUnit suite.
- A resumption marker or identity closure is not executable continuation
  evidence.
- A Core Erlang term that has not passed OTP `from_core` compilation is not a
  successful executable resumption artifact.
- Shallow or multi-shot helper tests do not promote shallow or multi-shot
  source semantics without accepted syntax, typing, runtime policy, and
  source-to-BEAM evidence.

## Starting Baseline

The roadmap starts from the reconciled boundary recorded when ADR-0006 was
accepted:

- functions carry concrete effects or effect rows in their types;
- `perform` introduces effects and handlers remove handled effects;
- generated code passes explicit effect contexts;
- source handlers execute through a request/response handler-process runtime;
- existing operation cases have no source-visible resumption binder;
- `catena_resumption:capture_continuation/0` wraps a direct-style
  `{resumed, Value}` marker rather than the remainder of a source computation;
- internal modules model deep/shallow and one-shot/multi-shot concepts, but
  the source-to-BEAM path does not provide their true continuation semantics;
- the fail-closed backend and public in-memory BEAM API are complete through
  the currently promoted non-resumable effect boundary;
- the parser has 37 documented shift/reduce conflicts before `with` and
  `resume` grammar work begins.

## Implementation Principles

1. Preserve explicit effect contexts as handler-lookup authority.
2. Reify continuations in the compiler rather than claiming to capture an
   ordinary Erlang stack.
3. Keep public `Resumption` values distinct from compiler continuations.
4. Preserve existing value handlers through specified tail auto-resume.
5. Use effect-directed selective CPS rather than mandatory whole-program CPS.
6. Execute resumed source computations on their originating BEAM process.
7. Make deep and one-shot the initial defaults and promotion boundary.
8. Treat shallow and multi-shot as separately gated semantic extensions.
9. Preserve source origins through every synthetic binder, continuation,
   bridge, delimiter, and runtime diagnostic.
10. Fail closed whenever the compiler cannot prove or represent the selected
    control behavior.

## Phases

- [x] [Phase 1: Operational Semantics, Feature Ledger, And Reference Oracle](phase-01-operational-semantics-feature-ledger-and-reference-oracle.md)
- [x] [Phase 2: `with`/`resume` Syntax, AST, And Semantic Normalization](phase-02-with-resume-syntax-ast-and-semantic-normalization.md)
- [x] [Phase 3: First-Class `Resumption` Kinds, Types, And Effects](phase-03-first-class-resumption-kinds-types-and-effects.md)
- [x] [Phase 4: Control-Mode Analysis And Selective CPS IR](phase-04-control-mode-analysis-and-selective-cps-ir.md)
- [x] [Phase 5: Deep One-Shot Runtime And Resumption Ownership](phase-05-deep-one-shot-runtime-and-resumption-ownership.md)
- [x] [Phase 6: Core Erlang, BEAM, And Call-Graph Integration](phase-06-core-erlang-beam-and-call-graph-integration.md)
- [x] [Phase 7: Shallow Handlers And Multi-Shot Resumptions](phase-07-shallow-handlers-and-multi-shot-resumptions.md)
- [x] [Phase 8: Tooling, Optimization, Conformance, And Promotion](phase-08-tooling-optimization-conformance-and-promotion.md)

## Dependency Graph

```text
Phase 1: semantics and oracle
    -> Phase 2: source syntax and normalized AST
        -> Phase 3: kinds, types, and effects
            -> Phase 4: control analysis and CPS IR
                -> Phase 5: deep one-shot runtime
                    -> Phase 6: Core Erlang and loaded BEAM
                        -> Phase 7: shallow and multi-shot
                            -> Phase 8: tooling and promotion
```

Phase 8 may begin non-promoting tooling experiments earlier, but its
conformance and status work depends on all behavior it advertises.

## Requirement And Scenario Traceability

The roadmap refines accepted architecture and now promotes the Phase 8
boundary through dedicated compiler, runtime, observability, testing, and
tooling requirements plus `SCN-012` executable evidence.

| Roadmap area | Current requirements | Current scenarios |
| --- | --- | --- |
| Syntax and normalization | `REQ-COMP-001`, `REQ-COMP-002` | `SCN-001` |
| Resumption typing and effects | `REQ-COMP-003`, `REQ-COMP-004`, `REQ-COMP-014` | `SCN-002`, `SCN-012` |
| Validated CPS/backend authority | `REQ-COMP-008`, `REQ-COMP-010`, `REQ-COMP-011`, `REQ-COMP-015` | `SCN-003`, `SCN-011`, `SCN-012` |
| Explicit runtime contexts and lifecycle | `REQ-RT-001`, `REQ-RT-002`, `REQ-RT-003`, `REQ-RT-010` | `SCN-004`, `SCN-012` |
| Compiler-backed interactive sessions | `REQ-RT-011` | `SCN-005`, `SCN-012` |
| Source-oriented diagnostics | `REQ-COMP-013`, `REQ-OBS-007` | `SCN-001`, `SCN-009`, `SCN-011`, `SCN-012` |
| Executable promotion evidence | `REQ-TEST-009` | `SCN-009`, `SCN-011`, `SCN-012` |

## Roadmap Completion Gate

- [x] `with` binds a typed first-class resumption in source operation cases
- [x] `resume(k, value)` executes the real delimited remainder of the source
      computation
- [x] Existing value handlers preserve their observable behavior through
      tail auto-resume
- [x] `Resumption OneShot a b e` is retained through validated typing and
      lowering
- [x] Direct/resumable classification and all calling-convention bridges are
      explicit and validated
- [x] Deep one-shot resumptions are opaque, process-affine, and
      deterministically consumed
- [x] Local, recursive, imported, higher-order, trait-dispatched, and
      effect-polymorphic call graphs execute correctly
- [x] Shallow and multi-shot behavior passes its distinct Phase 7 surface,
      static-safety, runtime-policy, and loaded-BEAM gates
- [x] Source-origin diagnostics cover compile-time and runtime control
      failures
- [x] Public source-to-BEAM APIs return only OTP-accepted artifacts
- [x] Dedicated conformance evidence covers positive and negative resumption
      semantics
- [ ] Modified modules meet the repository coverage target
- [x] `make check-specs`, `make conformance`, and the complete active suite
      pass
- [x] Current-status and component specs distinguish implemented, partial,
      and deferred resumption behavior accurately

The Phase 8 gate executed coverage rather than treating it as implicitly
green. Focused modified-module coverage remains below 90% for seven of eight
modules, so the coverage item stays unchecked as explicit quality debt. This
does not broaden the semantics promoted by the green source-to-BEAM,
conformance, and complete-suite gates.
