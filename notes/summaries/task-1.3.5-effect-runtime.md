# Task 1.3.5: Effect Runtime System - Summary

**Date**: 2025-11-20
**Branch**: `feature/task-1.3.5-effect-runtime`
**Status**: Complete

---

## Overview

Implemented a process-based effect runtime system leveraging BEAM's lightweight processes. Each try/with handler spawns a handler process that receives perform messages, executes handler operations, and sends results back. The implementation includes builtin IO and Process effects for the PoC.

---

## Implementation Details

### 1.3.5.1: Handler Process Spawning

Handler processes are spawned for each effect in a try/with block.

**Handler Spawning**:
```erlang
spawn_handler(Effect, Operations) ->
    CallerPid = self(),
    Pid = spawn_link(fun() ->
        handler_loop(Effect, Operations, CallerPid)
    end),
    register_handler(Effect, Pid),
    {Effect, Pid}.
```

**Handler Registration**:
- Uses process dictionary for O(1) lookup
- Key format: `{catena_effect_handler, Effect}`
- Automatic cleanup on try/with completion

**Handler Loop**:
```erlang
handler_loop(Effect, Operations, _CallerPid) ->
    receive
        {perform, Effect, Operation, Args, ReplyPid} ->
            case lists:keyfind(Operation, 1, Operations) of
                {Operation, HandlerFun} ->
                    Result = apply(HandlerFun, Args),
                    ReplyPid ! {effect_result, Result};
                false ->
                    ReplyPid ! {effect_error, {unknown_operation, Operation}}
            end,
            handler_loop(Effect, Operations, _CallerPid);
        stop ->
            ok
    end.
```

### 1.3.5.2: Perform Operation Compilation

Perform operations send messages to handler processes and await results.

```erlang
perform(Effect, Operation, Args) ->
    case get_handler(Effect) of
        undefined ->
            perform_builtin(Effect, Operation, Args);
        HandlerPid ->
            HandlerPid ! {perform, Effect, Operation, Args, self()},
            receive
                {effect_result, Value} -> Value;
                {effect_error, Reason} ->
                    erlang:error({effect_error, Effect, Operation, Reason})
            after ?EFFECT_TIMEOUT ->
                erlang:error({effect_timeout, Effect, Operation})
            end
    end.
```

**With Handlers Pattern**:
```erlang
with_handlers(HandlerSpecs, BodyFun) ->
    HandlerPids = spawn_handlers(HandlerSpecs),
    try
        BodyFun()
    after
        cleanup_handlers(HandlerPids)
    end.
```

### 1.3.5.3: Effect Message Protocol

Bidirectional message protocol using tagged tuples.

**Message Types**:

| Direction | Message Format | Description |
|-----------|---------------|-------------|
| Request | `{perform, Effect, Operation, Args, ReplyPid}` | Perform operation request |
| Success | `{effect_result, Value}` | Successful result |
| Error | `{effect_error, Reason}` | Error response |
| Control | `stop` | Stop handler process |

**Timeout Handling**:
- Default timeout: 5 seconds (`?EFFECT_TIMEOUT`)
- Raises `{effect_timeout, Effect, Operation}` on timeout

### 1.3.5.4: Builtin IO Effect Handler

The IO effect provides basic file and console operations.

**IO Operations**:

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `print` | `Text -> Unit` | Print text to stdout |
| `println` | `Text -> Unit` | Print text with newline |
| `readFile` | `Path -> Binary` | Read file contents |
| `writeFile` | `Path, Content -> Unit` | Write file contents |
| `getLine` | `() -> Binary` | Read line from stdin |

**Implementation**:
```erlang
io_handler() ->
    {'IO', [
        {print, fun io_print/1},
        {println, fun io_println/1},
        {readFile, fun io_read_file/1},
        {writeFile, fun io_write_file/2},
        {getLine, fun io_get_line/0}
    ]}.
```

### Process Effect Handler

The Process effect provides basic process operations.

**Process Operations**:

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `spawn` | `Fun -> Pid` | Spawn a new process |
| `send` | `Pid, Msg -> Unit` | Send message to process |
| `self` | `() -> Pid` | Get current process pid |

---

## New File

### `src/compiler/runtime/catena_effect_runtime.erl` (~280 lines)

**Main API**:
- `perform/3` - Perform an effect operation
- `with_handlers/2` - Execute body with effect handlers

**Handler Management**:
- `register_handler/2` - Register handler process
- `unregister_handler/1` - Unregister handler
- `get_handler/1` - Get handler pid

**Builtin Effects**:
- `io_handler/0` - Get IO handler specification
- `process_handler/0` - Get Process handler specification

### `test/compiler/runtime/catena_effect_runtime_tests.erl` (~490 lines)

**Test Coverage**: 30 tests across 8 test groups

- Handler spawning tests (5 tests)
- Perform operation tests (5 tests)
- Message protocol tests (3 tests)
- Builtin IO tests (5 tests)
- Builtin Process tests (4 tests)
- Integration tests (4 tests)
- Error handling tests (3 tests)
- Utility tests (1 test)

---

## Test Results

- **New tests**: 30 passed
- **No regressions** in existing test suites

---

## Example Usage

### Basic Effect Handling

```erlang
%% Define custom handler
Handlers = [
    {'Math', [
        {double, fun(X) -> X * 2 end},
        {add, fun(A, B) -> A + B end}
    ]}
],

%% Use handlers
Result = catena_effect_runtime:with_handlers(Handlers, fun() ->
    V1 = catena_effect_runtime:perform('Math', double, [21]),
    V2 = catena_effect_runtime:perform('Math', add, [V1, 8]),
    V2
end).
%% Result = 50
```

### Using Builtin IO

```erlang
%% IO operations use builtin handler automatically
catena_effect_runtime:perform('IO', println, [<<"Hello, Effects!">>]),
Content = catena_effect_runtime:perform('IO', readFile, ["/tmp/data.txt"]),
catena_effect_runtime:perform('IO', writeFile, ["/tmp/output.txt", Content]).
```

### Nested Handlers

```erlang
OuterHandlers = [{'Outer', [{get, fun() -> outer end}]}],
InnerHandlers = [{'Inner', [{get, fun() -> inner end}]}],

catena_effect_runtime:with_handlers(OuterHandlers, fun() ->
    OuterVal = catena_effect_runtime:perform('Outer', get, []),

    %% Nested scope
    catena_effect_runtime:with_handlers(InnerHandlers, fun() ->
        InnerVal = catena_effect_runtime:perform('Inner', get, []),
        %% Outer handler still accessible
        catena_effect_runtime:perform('Outer', get, [])
    end)
end).
```

### Overriding Builtin Effects

```erlang
%% Override IO with custom implementation
CustomIO = [
    {'IO', [
        {print, fun(Text) ->
            log_to_file(Text),
            ok
        end}
    ]}
],

catena_effect_runtime:with_handlers(CustomIO, fun() ->
    %% Uses custom print instead of builtin
    catena_effect_runtime:perform('IO', print, [<<"logged">>])
end).
```

---

## Architecture Notes

### Process-Based Design

The effect runtime leverages BEAM's strengths:

1. **Lightweight Processes**: Handler processes are cheap to spawn
2. **Message Passing**: Natural fit for effect request/response
3. **Process Isolation**: Handlers are isolated from main computation
4. **Link-Based Cleanup**: `spawn_link` ensures cleanup on errors

### Handler Lookup Strategy

Handler lookup uses the process dictionary for O(1) access:

```erlang
%% Store handler
put({catena_effect_handler, Effect}, Pid)

%% Lookup handler
get({catena_effect_handler, Effect})
```

This allows nested handlers to shadow outer handlers while maintaining fast access.

### Builtin Effect Fallback

When no handler is registered, the runtime falls back to builtin effects:

```erlang
case get_handler(Effect) of
    undefined -> perform_builtin(Effect, Operation, Args);
    HandlerPid -> %% ... send to handler
end.
```

This allows programs to use IO and Process effects without explicit handlers.

### Integration with Code Generation

The runtime integrates with `catena_codegen_expr.erl`:

- `translate_perform` generates `catena_effect_runtime:perform(Effect, Op, Args)`
- `translate_try_with` generates `catena_effect_runtime:with_handlers(Specs, BodyFun)`

---

## Success Criteria

- 1.3.5.1 - Handler process spawning with operation implementations and registration
- 1.3.5.2 - Perform operation compilation with message send/receive and timeout
- 1.3.5.3 - Effect message protocol with tagged tuples for bidirectional communication
- 1.3.5.4 - Builtin IO effect handler with readFile, writeFile, print operations

---

## Files Created

- `src/compiler/runtime/catena_effect_runtime.erl`
- `test/compiler/runtime/catena_effect_runtime_tests.erl`

---

## Future Enhancements

The current implementation is suitable for the PoC. Future improvements could include:

1. **Effect Composition**: Combinators for composing effect handlers
2. **Resumable Handlers**: Support for multi-shot continuations
3. **Effect Inference**: Automatic effect type inference from handlers
4. **Optimization**: Handler inlining for known effects
5. **Monitoring**: Effect execution tracing and profiling

The effect runtime completes the core code generation pipeline, enabling execution of effectful Catena programs on the BEAM VM with proper isolation and handler semantics.
