# Task 1.2.5: Effect-Specific Error Messages - Summary

**Date**: 2025-11-20
**Branch**: `feature/task-1.2.5-effect-error-messages`
**Status**: Complete

---

## Overview

Implemented comprehensive effect-specific error messages for the Catena compiler's type system. These errors help developers understand which effects are unhandled, which handler operations are missing or mismatched, and where effects were introduced and propagated.

---

## Implementation Details

### New Error Types Added

Five new error types were added to `catena_type_error.erl`:

1. **`unhandled_effect/3`** - Reports when a function performs an effect not declared in its type signature
   - Shows effect name and function name
   - Includes location where effect was introduced (perform site)
   - Suggests adding effect annotation or try/with handler

2. **`handler_missing_operation/3`** - Reports missing handler for an effect operation
   - Shows effect and operation names
   - Includes handler location
   - Suggests adding handler case

3. **`handler_arity_mismatch/5`** - Reports incorrect parameter count in handler
   - Shows expected vs actual argument counts
   - Includes location of the handler case

4. **`effect_annotation_mismatch/3`** - Reports declared vs inferred effect mismatch
   - Shows both declared and inferred effect sets
   - Identifies missing and extra effects

5. **`effect_context_chain/2`** - Shows effect propagation chain
   - Lists function call chain with locations
   - Helps identify where to handle the effect

### Files Modified

- **`src/compiler/types/catena_type_error.erl`** (lines 276-358, 391-400, 491-558)
  - Added error type definitions
  - Added format_error clauses with detailed messages
  - Added helper function `format_effect_chain/1`
  - Added constructor functions with documentation

### New Test File

- **`test/compiler/types/catena_effect_error_tests.erl`** (246 lines)
  - 17 tests covering all new error types
  - Tests for constructors, formatting, and edge cases
  - Integration tests for error formatting

---

## Example Error Messages

### Unhandled Effect

```
Unhandled effect 'io' in function 'process_file'
Effect introduced at: 10:5

The function performs an operation that requires the io effect,
but this effect is not declared in the function's type signature.

To fix this, either:
  1. Add the effect to the function signature: / {io}
  2. Handle the effect with a try/with block
```

### Handler Missing Operation

```
Missing handler for operation 'FileIO.read'
Handler at: 10:5

The handler for effect 'FileIO' does not handle the 'read' operation.
All operations of an effect must be handled.

Add a handler case:
  | read(args) -> result
```

### Handler Arity Mismatch

```
Handler arity mismatch for 'Console.print'
At: 10:5

  Expected: 1 arguments
  Got:      2 arguments

The handler must accept the same number of arguments as the operation declaration.
```

### Effect Annotation Mismatch

```
Effect annotation mismatch in function 'my_func'
  Declared: {io}
  Inferred: {file, io}
  Missing from annotation: [file]

The effect annotation does not match the effects actually used by the function.
```

### Effect Context Chain

```
Effect 'io' propagation trace:
  1. read_file at 10:1
  2. process_data at 20:1
  3. main at 30:1

The effect was introduced and propagated through the following call chain.
Consider handling the effect at an appropriate point in this chain.
```

---

## Test Results

- **New tests**: 17 passed
- **Full test suite**: 397 passed, 6 failed (pre-existing issues)
- **No regressions** introduced by these changes

---

## Integration Notes

The error constructors are exported from `catena_type_error` and can be used by:

- Effect inference (`catena_infer_effect.erl`) when detecting unhandled effects
- Handler verification (`catena_handler_verify.erl`) for handler mismatches
- Type checking when validating effect annotations

### Usage Example

```erlang
%% Create an unhandled effect error
Error = catena_type_error:unhandled_effect(io, my_function, Location),
Msg = catena_type_error:format_error(Error).

%% Create a handler missing operation error
Error = catena_type_error:handler_missing_operation('FileIO', read, HandlerLoc),
Msg = catena_type_error:format_error(Error).

%% Create an effect context chain
Chain = [{perform_io, Loc1}, {caller, Loc2}, {main, Loc3}],
Error = catena_type_error:effect_context_chain(io, Chain),
Msg = catena_type_error:format_error(Error).
```

---

## Future Integration

These error types are ready for integration with:

1. **Effect inference** - Generate `unhandled_effect` errors when effects aren't declared
2. **Handler verification** - Generate `handler_missing_operation` and `handler_arity_mismatch` errors
3. **Type annotation checking** - Generate `effect_annotation_mismatch` errors
4. **Call graph analysis** - Generate `effect_context_chain` for detailed error context

The integration will happen as part of tasks 1.2.6 (Trait Constraint System) and the effect system completion in Phase 6.

---

## Success Criteria

✅ 1.2.5.1 - Unhandled effect errors with perform site location
✅ 1.2.5.2 - Handler mismatch errors for arities and missing operations
✅ 1.2.5.3 - Effect annotation mismatch errors
✅ 1.2.5.4 - Effect context explanation showing propagation chain

---

## Commits

- Initial implementation of effect-specific error types and tests
