# Effect Runtime

## Status

Promoted status: compiler-aligned explicit-context runtime with handler
processes, nested scope support, configurable operation timeouts, synchronous
handler cleanup, and builtin `IO` and `Process` handlers. Selective-CPS source
constructs execute through local deep or shallow handler frames and opaque
process-affine one-shot resumptions from Catena source through loaded BEAM.
Multi-shot runtime authority remains deferred to Phase 7 Section 7.3.

## Design Anchors

- [ADR-0003: Explicit Effect Context Runtime](../adr/ADR-0003-explicit-effect-context-runtime.md)
- [ADR-0006: First-Class Resumptions Through Selective CPS](../adr/ADR-0006-first-class-resumptions-and-selective-cps.md)
- [Current Status](../planning/current_status.md)
- [Runtime Contract](../contracts/runtime_contract.md)
- `src/compiler/runtime/catena_effect_runtime.erl`
- `src/compiler/runtime/catena_resumption_runtime.erl`
- `src/compiler/effects/catena_effect_system.erl`
- `src/compiler/types/catena_effect_resolution.erl`
- `src/compiler/codegen/catena_effect_codegen.erl`
- `test/compiler/runtime/catena_effect_runtime_tests.erl`
- `test/compiler/runtime/catena_effect_context_resumption_tests.erl`
- `test/compiler/runtime/catena_resumption_lifecycle_tests.erl`
- `test/compiler/integration/catena_delimited_resumption_phase5_tests.erl`
- `test/integration/catena_effect_integration_tests.erl`
- `test/compiler/integration/catena_backend_hardening_phase5_tests.erl`

## Current Promoted Surface

- Catena's current effect runtime is explicitly context-passing, not process-dictionary based.
- Effect handlers are implemented as BEAM processes that receive perform messages and reply with results.
- Nested handler scopes are part of the design, so child contexts can shadow or extend parent handlers.
- Generated effectful transforms keep their public source arity and delegate to
  private context-aware entries. Calls between effectful transforms reuse the
  current context instead of creating nested top-level runtimes.
- Effect declarations and handler clauses are validated against stable
  operation identities before Core Erlang emission. Handler patterns use the
  ordinary lossless pattern compiler.
- Effectful artifacts declare the exact version and features of
  `catena_effect_runtime` plus the version 1 `catena_effect_system` contract;
  artifact preparation rejects targets that cannot supply those contracts.
- The runtime timeout defaults to 5,000 milliseconds and may be configured
  through `catena_effect_system` for an execution boundary.
- Builtin effect support exists today for `IO` and `Process`.
- The builtin `Process` handler exposes `spawn`, `spawn_link`, `send`, `self`,
  `link`, `unlink`, `monitor`, `demonitor`, `whereis`, `register`,
  `is_process_alive`, and `trap_exit`.
- Higher-level algebraic-effects orchestration exists in the compiler/effects
  tree as a separate Erlang-facing facade. It uses process-local handler scopes
  for direct component execution and does not replace the explicit-context
  generated-code boundary.
- Selective-CPS code may install same-process `local_resumable` frames,
  perform through `perform_cps/5`, and invoke real compiler-shaped
  continuations through opaque depth-aware one-shot runtime handles.
- Resumption authority uses explicit registry state, frame leases, owner and
  provider monitors, cooperative deadlines, and source-oriented failures; it
  does not use the process dictionary or execute continuations on providers.

## Implemented Runtime Evolution

ADR-0006 preserves explicit contexts as the authority for handler lookup and
adds the implemented runtime distinction:

- resumable source handler frames execute on the computation's originating
  BEAM process so a continuation preserves `self`, mailbox ownership, and
  process-local failure behavior
- process-backed builtin or external providers may continue to calculate
  operation results, but they do not execute the captured continuation
- opaque, process-affine `Resumption` values carry a compiler-reified CPS
  continuation, delimiter, captured context, kind, ownership, and consumption
  authority

Phase 4 supplies the validated selective-CPS graph and calling convention.
Phase 5 supplies the ownership registry, deep one-shot invocation, retained
frame leases, lifecycle cleanup, and stable runtime diagnostics. Phase 6
lowers the validated graph into calls to this ABI and proves source-to-loaded
BEAM execution. Phase 7 Section 7.2 adds shallow parent-context restoration,
depth-aware handler specifications, and versioned artifact mode contracts.
The current handler-process lifecycle acceptance criteria remain
authoritative for the promoted request/response path.

## Acceptance Criteria

### AC-ERT-001 Explicit Context Authority

The promoted generated-code runtime model for effect execution is explicit
context passing. Process-local handler lookup in the higher-level
`catena_effects` orchestration facade is an internal/component execution
surface, not a replacement runtime authority, unless a later ADR supersedes
this boundary.

### AC-ERT-002 Handler Lifecycle

`with_handlers/3` must remain responsible for:

- spawning handler processes
- constructing a child effect context
- executing a body inside that context
- synchronously cleaning up the spawned handlers after normal results,
  handler failures, unhandled operations, and timeouts

This lifecycle behavior is part of the runtime contract, not an incidental implementation detail.

### AC-ERT-003 Perform Resolution

`perform/4` must resolve operations by:

- walking the current context and its parents for a handler
- delegating to builtin handlers when no explicit handler exists for supported builtin effects
- failing loudly when no handler is available

### AC-ERT-004 Safety Boundaries

The promoted runtime includes the current lightweight safety boundaries already present in code, including:

- handler response timeout behavior, including the configurable
  `effect_timeout` execution option
- basic process-count limits for process-oriented operations
- file/path and size protections in builtin IO operations

These are part of Catena's current runtime design and should remain documented until a stronger operational model replaces them.

### AC-ERT-005 Language Alignment

The effect runtime is only promoted as correct when it lines up with the compiler's effect model:

- effect names used in runtime dispatch correspond to the language's effect declarations
- performed operations resolve to declared names, arities, argument types, and
  result types before lowering
- handlers compose with nested scopes
- handler cases cover every declared operation exactly once with the declared
  arity
- handler removal/resolution semantics remain compatible with the type/effect system's current explicit effect tracking, including the implemented row-polymorphic/algebraic-effects surfaces

## Out Of Scope

- source-language actor declarations and compilation; the implemented local
  Erlang actor toolkit is specified separately in
  [Actor Runtime](actor_runtime.md)
- distributed effect handling
- multi-shot branch authority, state policy, and resource budgets, which are
  owned by Phase 7 Section 7.3
- the full future runtime story beyond the proof-of-concept
