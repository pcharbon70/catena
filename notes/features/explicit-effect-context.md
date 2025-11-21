# Feature: Explicit Effect Context Passing

**Branch**: `feature/explicit-effect-context`
**Date**: 2025-11-21
**Status**: In Progress

---

## Problem Statement

The current effect runtime uses the process dictionary for handler lookup, which creates several issues:

1. **Implicit State**: Handler context is invisible - can't see what handlers are active from code or function signatures
2. **Cross-Process Issues**: Spawned processes don't inherit the process dictionary, so effects fail in child processes
3. **No Proper Nesting**: Handler nesting doesn't properly restore outer handlers after inner scope exits
4. **Testing Difficulty**: Hard to test in isolation because you need to set up process dictionary state
5. **No Explicit Dependencies**: Functions that perform effects don't show in their signature that they depend on handler context

## Solution Overview

Replace process dictionary storage with explicit context passing:

- Define an `effect_context()` type as a map containing handlers and parent reference
- All effectful functions receive context as first parameter
- Context is threaded through call chains
- Child contexts inherit from parents, enabling proper nesting
- Spawned processes receive context explicitly

### Key Design Decisions

1. **Context Structure**: Map with `handlers` (effect → pid) and `parent` (for nesting)
2. **API Change**: `perform/3` becomes `perform/4` with context first
3. **Backward Compatibility**: Keep builtin effects working without explicit handlers
4. **Code Generation**: Update `catena_codegen_expr.erl` to thread context in generated code

---

## Technical Details

### Files to Modify

1. **`src/compiler/runtime/catena_effect_runtime.erl`**
   - Add type definitions for `effect_context()`
   - Add `empty_context/0`, `new_context/0`
   - Change `perform/3` to `perform/4` (context first)
   - Change `with_handlers/2` to `with_handlers/3` (context first)
   - Remove process dictionary usage (`register_handler`, `unregister_handler`, `get_handler`)
   - Add context-based handler lookup with parent chain walking

2. **`src/compiler/codegen/catena_codegen_expr.erl`**
   - Update `compile_perform/3` to pass context
   - Update effect-related code generation

3. **`test/compiler/runtime/catena_effect_runtime_tests.erl`**
   - Update all tests to use new context-based API
   - Add tests for cross-process context passing
   - Add tests for nested handler contexts

### New Type Definitions

```erlang
-type effect_context() :: #{
    handlers := #{atom() => pid()},
    parent := effect_context() | undefined
}.
```

### New API

```erlang
%% Context creation
-spec empty_context() -> effect_context().
-spec new_context() -> effect_context().

%% Main operations (context first)
-spec perform(effect_context(), atom(), atom(), list()) -> term().
-spec with_handlers(effect_context(), list(), fun((effect_context()) -> T)) -> T.

%% Removed (no longer needed)
%% register_handler/2
%% unregister_handler/1
%% get_handler/1
```

---

## Success Criteria

1. All existing effect runtime tests pass with new API
2. Cross-process effect performance works (context passed to spawned processes)
3. Nested handlers work correctly (inner shadows outer, outer restored after)
4. No process dictionary usage in effect runtime
5. Code generation produces context-threading code
6. Tests demonstrate explicit context benefits (easy mocking)

---

## Implementation Plan

### Step 1: Update Effect Runtime Module ✅
- [x] Add type definitions
- [x] Add `empty_context/0` and `new_context/0`
- [x] Change `perform/3` to `perform/4`
- [x] Change `with_handlers/2` to `with_handlers/3`
- [x] Update `spawn_handlers` to not use process dictionary
- [x] Update handler lookup to walk parent chain
- [x] Remove `register_handler/2`, `unregister_handler/1`, `get_handler/1` from exports
- [x] Keep backward-compatible builtin effect handling

### Step 2: Update Tests ✅
- [x] Update all existing tests to use new API
- [x] Add test for empty context creation
- [x] Add test for context with handlers
- [x] Add test for nested contexts (parent chain lookup)
- [x] Add test for cross-process context passing
- [x] Add test for handler shadowing in nested contexts
- [x] Add test for easy mocking with explicit context

### Step 3: Update Code Generator ✅
- [x] Update `catena_codegen_expr.erl` to generate context-threading code
- [x] Update perform compilation to pass context

### Step 4: Verify and Document ⬜
- [x] Run full test suite
- [ ] Create summary document
- [ ] Request commit permission

---

## Current Status

### What Works
- Feature branch created: `feature/explicit-effect-context`
- Effect runtime updated with explicit context passing
- All 38 effect runtime tests pass
- Code generator updated to thread context
- All 28 codegen expr tests pass

### What's Implemented

1. **New API**:
   - `empty_context/0` - Create empty effect context
   - `new_context/0` - Alias for empty_context
   - `perform/4` - Perform effect with context (was perform/3)
   - `with_handlers/3` - Execute with handlers and context (was with_handlers/2)

2. **Removed from exports**:
   - `register_handler/2` (no longer needed)
   - `unregister_handler/1` (no longer needed)
   - `get_handler/1` (now internal)

3. **Code Generator Changes**:
   - `translate_perform` generates `perform(__catena_ctx__, Effect, Op, Args)`
   - `translate_try_with` generates `with_handlers(__catena_ctx__, Specs, fun(__catena_ctx__) -> Body end)`

### How to Run
```bash
make compile
make test
# Or specific tests:
rebar3 eunit --module=catena_effect_runtime_tests
rebar3 eunit --module=catena_codegen_expr_tests
```

---

## Notes/Considerations

### Edge Cases
- Builtin effects (IO, Process) should work without explicit handler registration
- Empty context should fall through to builtins
- Handler cleanup must still happen in `after` clause

### Future Improvements
- Could add context validation/debugging helpers
- Could add context pretty-printing for error messages
- Consider Reader monad sugar in Catena surface syntax

### Risks
- API change affects all code using effect runtime
- Must update code generator to match
