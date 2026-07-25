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
- [ ] [Phase 2: `with`/`resume` Syntax, AST, And Semantic Normalization](phase-02-with-resume-syntax-ast-and-semantic-normalization.md)
- [ ] [Phase 3: First-Class `Resumption` Kinds, Types, And Effects](phase-03-first-class-resumption-kinds-types-and-effects.md)
- [ ] [Phase 4: Control-Mode Analysis And Selective CPS IR](phase-04-control-mode-analysis-and-selective-cps-ir.md)
- [ ] [Phase 5: Deep One-Shot Runtime And Resumption Ownership](phase-05-deep-one-shot-runtime-and-resumption-ownership.md)
- [ ] [Phase 6: Core Erlang, BEAM, And Call-Graph Integration](phase-06-core-erlang-beam-and-call-graph-integration.md)
- [ ] [Phase 7: Shallow Handlers And Multi-Shot Resumptions](phase-07-shallow-handlers-and-multi-shot-resumptions.md)
- [ ] [Phase 8: Tooling, Optimization, Conformance, And Promotion](phase-08-tooling-optimization-conformance-and-promotion.md)

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

The roadmap initially refines accepted architecture without claiming new
promoted requirements. Contract and scenario additions occur in Phase 8 after
executable evidence exists.

| Roadmap area | Current requirements | Current scenarios |
| --- | --- | --- |
| Syntax and normalization | `REQ-COMP-001`, `REQ-COMP-002` | `SCN-001` |
| Resumption typing and effects | `REQ-COMP-003`, `REQ-COMP-004` | `SCN-002` |
| Validated CPS/backend authority | `REQ-COMP-008`, `REQ-COMP-010`, `REQ-COMP-011` | `SCN-003`, `SCN-011` |
| Explicit runtime contexts and lifecycle | `REQ-RT-001`, `REQ-RT-002`, `REQ-RT-003` | `SCN-004` |
| Source-oriented diagnostics | `REQ-COMP-013`, `REQ-OBS-*` | `SCN-001`, `SCN-009`, `SCN-011` |
| Executable promotion evidence | `REQ-TEST-*` | `SCN-009`, `SCN-011` |

## Roadmap Completion Gate

- [ ] `with` binds a typed first-class resumption in source operation cases
- [ ] `resume(k, value)` executes the real delimited remainder of the source
      computation
- [ ] Existing value handlers preserve their observable behavior through
      tail auto-resume
- [ ] `Resumption OneShot a b e` is retained through validated typing and
      lowering
- [ ] Direct/resumable classification and all calling-convention bridges are
      explicit and validated
- [ ] Deep one-shot resumptions are opaque, process-affine, and
      deterministically consumed
- [ ] Local, recursive, imported, higher-order, trait-dispatched, and
      effect-polymorphic call graphs execute correctly
- [ ] Shallow and multi-shot behavior remains rejected until its Phase 7
      surface and safety gates pass
- [ ] Source-origin diagnostics cover compile-time and runtime control
      failures
- [ ] Public source-to-BEAM APIs return only OTP-accepted artifacts
- [ ] Dedicated conformance evidence covers positive and negative resumption
      semantics
- [ ] Modified modules meet the repository coverage target
- [ ] `make check-specs`, `make conformance`, and the complete active suite
      pass
- [ ] Current-status and component specs distinguish implemented, partial,
      and deferred resumption behavior accurately
