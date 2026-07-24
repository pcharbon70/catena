# Effect Runtime

## Status

Promoted status: implemented as an explicit-context runtime with handler processes, nested scope support, and builtin `IO` and `Process` handlers.

## Design Anchors

- [ADR-0003: Explicit Effect Context Runtime](../adr/ADR-0003-explicit-effect-context-runtime.md)
- [Current Status](../planning/current_status.md)
- [Runtime Contract](../contracts/runtime_contract.md)
- `src/compiler/runtime/catena_effect_runtime.erl`
- `test/compiler/runtime/catena_effect_runtime_tests.erl`
- `test/integration/catena_effect_integration_tests.erl`

## Current Promoted Surface

- Catena's current effect runtime is explicitly context-passing, not process-dictionary based.
- Effect handlers are implemented as BEAM processes that receive perform messages and reply with results.
- Nested handler scopes are part of the design, so child contexts can shadow or extend parent handlers.
- Builtin effect support exists today for `IO` and `Process`.
- The builtin `Process` handler exposes `spawn`, `spawn_link`, `send`, `self`,
  `link`, `unlink`, `monitor`, `demonitor`, `whereis`, `register`,
  `is_process_alive`, and `trap_exit`.
- Higher-level algebraic-effects orchestration exists in the compiler/effects
  tree as a separate Erlang-facing facade. It uses process-local handler scopes
  for direct component execution and does not replace the explicit-context
  generated-code boundary.

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
- cleaning up the spawned handlers after execution

This lifecycle behavior is part of the runtime contract, not an incidental implementation detail.

### AC-ERT-003 Perform Resolution

`perform/4` must resolve operations by:

- walking the current context and its parents for a handler
- delegating to builtin handlers when no explicit handler exists for supported builtin effects
- failing loudly when no handler is available

### AC-ERT-004 Safety Boundaries

The promoted runtime includes the current lightweight safety boundaries already present in code, including:

- handler response timeout behavior
- basic process-count limits for process-oriented operations
- file/path and size protections in builtin IO operations

These are part of Catena's current runtime design and should remain documented until a stronger operational model replaces them.

### AC-ERT-005 Language Alignment

The effect runtime is only promoted as correct when it lines up with the compiler's effect model:

- effect names used in runtime dispatch correspond to the language's effect declarations
- handlers compose with nested scopes
- handler removal/resolution semantics remain compatible with the type/effect system's current explicit effect tracking, including the implemented row-polymorphic/algebraic-effects surfaces

## Out Of Scope

- source-language actor declarations and compilation; the implemented local
  Erlang actor toolkit is specified separately in
  [Actor Runtime](actor_runtime.md)
- distributed effect handling
- the full future runtime story beyond the proof-of-concept
