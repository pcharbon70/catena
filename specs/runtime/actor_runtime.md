# Actor Runtime

## Status

Promoted status: implemented as a local Erlang runtime toolkit for process
primitives, stateful actors, GenServer-style callbacks, minimal one-for-one
supervision, process registration, publish/subscribe, and event broadcasting.
This is not yet a complete source-language actor model.

## Design Anchors

- [Current Status](../planning/current_status.md)
- [Runtime Contract](../contracts/runtime_contract.md)
- [Effect Runtime](effect_runtime.md)
- `src/runtime/catena_process.erl`
- `src/runtime/catena_actor.erl`
- `src/runtime/catena_gen_server.erl`
- `src/runtime/catena_supervisor.erl`
- `src/runtime/catena_registry.erl`
- `src/runtime/catena_pubsub.erl`
- `src/runtime/catena_event_broadcaster.erl`
- `test/runtime/catena_process_tests.erl`
- `test/runtime/catena_actor_tests.erl`
- `test/runtime/catena_actor_integration_tests.erl`

## Current Promoted Surface

### Process Façade

`catena_process` provides BEAM-native process creation, asynchronous send,
native OTP `gen_server:call` delegation, links, monitors, local name
registration, dynamic-predicate receive, process liveness, trap-exit control,
and exit operations.

The façade normalizes lifecycle mutations to `ok`, but preserves native BEAM
semantics where they carry meaning:

- sending to a PID is asynchronous and returns `ok` even if that PID has
  already exited
- sending to an unresolved local name raises `{no_process, Name}`
- links and monitors belong to the calling process
- registered names disappear automatically when their process exits
- dynamic-predicate receive restores rejected messages before returning

### Stateful Actor And GenServer-Style Components

`catena_actor` implements a local callback-driven actor loop with:

- initialization
- synchronous calls
- asynchronous casts
- raw info messages
- private state transitions
- explicit replies and termination

`catena_gen_server` implements a separate GenServer-style local protocol. It
uses Catena's own message envelopes and is not the native OTP `gen_server`
wire protocol used by `catena_process:call`.

### Supervision

`catena_supervisor` provides the currently tested local subset:

- child startup and shutdown
- child listing and counts
- explicit restart, terminate, and delete operations
- automatic restart of an abnormally exiting child
- local one-for-one behavior

This surface is a proof-of-concept supervisor. It does not promote the full OTP
strategy, restart-intensity, shutdown, or code-upgrade contract.

### Registration And Fan-Out

The local runtime also includes:

- `catena_registry`: monitored key/PID registration with metadata and automatic
  cleanup
- `catena_pubsub`: local topic subscriptions, wildcard matching, publication,
  and monitored subscriber cleanup
- `catena_event_broadcaster`: local listener fan-out, optional filters, and
  monitored cleanup

## Acceptance Criteria

### AC-ACTOR-001 BEAM Process Semantics

The process façade must retain BEAM's asynchronous messaging and signal
ownership semantics while presenting stable Catena-facing lifecycle results.

### AC-ACTOR-002 Actor State Isolation

Actor and GenServer-style callbacks must process one message at a time and
carry state explicitly from one callback result to the next.

### AC-ACTOR-003 Local Lifecycle Cleanup

Registries, pub/sub services, and broadcasters must monitor registered
processes and remove entries when those processes terminate.

### AC-ACTOR-004 Supervision Scope

The promoted supervisor contract is limited to the local behavior exercised by
the focused suite. Strategy completeness and production OTP parity must not be
implied.

### AC-ACTOR-005 Process Effect Boundaries

The three runtime-facing Process surfaces have distinct current roles:

- generated code uses the explicit-context `catena_effect_runtime`
- the standard Catena Process effect declares `spawn`, `send`, and `self`
- the REPL directly executes those three standard operations through
  `catena_process`

The Erlang actor modules are not themselves evidence that source-language
actors have been parsed, typed, lowered, or compiled.

## Out Of Scope

- Catena actor declaration syntax and typed message protocols
- compiling source actors to the Erlang runtime components
- actors as first-class algebraic-effect handlers
- a Process Workflow/Monad instance and actor do-notation
- full OTP supervisor strategies and restart intensity
- GenStage, state-machine, and task libraries
- global/via registration and distributed actors
- remote spawning, network partition handling, and distribution benchmarks
- production performance or memory-overhead guarantees
