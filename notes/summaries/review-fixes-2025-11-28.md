# Review Fixes Summary - 2025-11-28

**Branch**: `feature/review-fixes`
**Status**: Complete

---

## Overview

This document summarizes the fixes applied to address the concerns and suggested improvements from the Phase 1.5 Standard Library Validation code review.

## Changes Made

### 1. Integration Tests for Stdlib Operations (Concern #2)

**File**: `test/compiler/integration/catena_stdlib_integration_tests.erl` (NEW - 288 lines)

Created comprehensive integration tests covering:

- **Erlang Prelude Runtime Tests** (11 tests)
  - `map (fn x -> x + 1) [1, 2, 3]` produces `[2, 3, 4]`
  - `Some 5 >>= (fn x -> Some (x + 1))` produces `Some 6`
  - `None >>= f` produces `None`
  - `[1, 2] <> [3, 4]` produces `[1, 2, 3, 4]`
  - `chain`, `compose`, `filter`, `fold` operations

- **Do-Notation Simulation Tests** (3 tests)
  - Bind chain simulation for Maybe
  - None short-circuits computation
  - List comprehension via bind

- **Import and Constructor Tests** (5 tests)
  - `import Prelude` and use `Some`, `None`, `Left`, `Right`, `Ok`, `Err`, `LT`, `EQ`, `GT`

- **Operator Desugaring Verification** (3 tests)
  - `<>` desugars to `combine`
  - `===` desugars to `equals`
  - `>=>` desugars to `kleisli`

- **Kleisli Composition Tests** (1 test)
  - Kleisli composition of Maybe functions

**Parser Updates** (`src/compiler/parser/catena_parser.yrl`):
- Added terminals: `fmap`, `ap`, `bind`, `mappend`, `kleisli`
- Added precedence declarations for category theory operators
- Added expression rules for each operator

### 2. Desugar Error Handling Fix (Concern #3)

**File**: `src/compiler/semantic/catena_desugar.erl`

Changed `desugar_stmts/3` depth limit from returning `{error, ...}` to throwing an exception:

```erlang
%% Before
desugar_stmts(_, Loc, Depth) when Depth > ?MAX_DO_DEPTH ->
    {error, {do_nesting_exceeded, Depth, Loc}};

%% After
desugar_stmts(_, Loc, Depth) when Depth > ?MAX_DO_DEPTH ->
    error({do_nesting_exceeded, Depth, Loc});
```

**Rationale**: Error tuples could be embedded in AST nodes where callers might not check them. Using `error/1` ensures immediate termination and proper propagation.

**Test Update** (`test/compiler/semantic/catena_desugar_properties.erl`):
- Updated `prop_depth_limit_enforced/0` to catch exception instead of matching error tuple

### 3. Test Helper Consolidation (Concern #4)

**File**: `test/compiler/integration/catena_test_helpers.erl`

Added shared `loc/0` helper:

```erlang
-spec loc() -> {location, integer(), integer()}.
loc() ->
    {location, 1, 1}.
```

**Updated Files** (removed duplicate `loc/0` definitions):
- `test/compiler/types/catena_effect_kleisli_tests.erl`
- `test/compiler/semantic/catena_hkt_validation_tests.erl`
- `test/compiler/semantic/catena_desugar_properties.erl`
- `test/compiler/codegen/catena_codegen_module_tests.erl`

Each file now imports: `-import(catena_test_helpers, [loc/0]).`

### 4. AST Builder Refactoring (Concern #5)

**File**: `test/compiler/integration/catena_e2e_tests.erl`

Refactored 78 lines of repetitive `build_codegen_ast/2` functions into parameterized builders:

```erlang
%% Before: 10 separate functions with repeated Loc = {1, 1} pattern
build_codegen_ast(_Source, test_arith) ->
    Loc = {1, 1},
    {module, test_arith, [{add, 2}, {multiply, 2}], [], [
        {transform, add, [{pat_var, x, Loc}, {pat_var, y, Loc}],
         {binary_op, '+', {var, x, Loc}, {var, y, Loc}, Loc}, Loc},
        ...
    ], Loc};

%% After: Concise DSL-style builders
build_codegen_ast(_Source, test_arith) ->
    build_module(test_arith, [
        {add, [x, y], binary_op('+', x, y)},
        {multiply, [x, y], binary_op('*', x, y)}
    ]);

%% Shared helpers
build_module(ModuleName, TransformSpecs) ->
    Exports = [{Name, length(Params)} || {Name, Params, _} <- TransformSpecs],
    Decls = [build_transform(Name, Params, Body) || {Name, Params, Body} <- TransformSpecs],
    {module, ModuleName, Exports, [], Decls, loc()}.

var(Name) when is_atom(Name) -> {var, Name, loc()}.
lit(N) when is_integer(N) -> {literal, integer, N, loc()}.
binary_op(Op, Left, Right) -> ...
```

---

## Suggestions Implemented

### 5. Architecture: Move add_effect_to_state/2

**File**: `src/compiler/types/catena_infer_expr.erl`

Removed local `add_effect_to_state/2` function and now uses the existing `catena_infer_state:add_effect/2` which provides the same functionality. This improves encapsulation by keeping all state operations in the state module.

### 6. Architecture: Effect Scope Tracking

**File**: `src/compiler/types/catena_infer_state.erl`

Added effect scope stack for Phase 6 preparation:

```erlang
%% New record fields
effect_scope_stack :: [catena_infer_effect:effect_set()],

%% New functions
push_effect_scope/1  - Push current effects and start fresh
pop_effect_scope/1   - Pop and merge with current effects
get_effect_scope_depth/1 - Get scope depth
```

This prepares the infrastructure for effect polymorphism where effects need to be tracked at different scopes (e.g., inside handle blocks).

### 7. Security: Expression Depth Counter

**File**: `src/compiler/types/catena_infer_state.erl`

Added expression depth tracking with limit:

```erlang
-define(MAX_EXPR_DEPTH, 1000).

%% New record field
expr_depth :: non_neg_integer()

%% New functions
get_expr_depth/1  - Get current depth
inc_expr_depth/1  - Increment (entering nested expr)
dec_expr_depth/1  - Decrement (exiting nested expr)
check_expr_depth/1 - Throws error if > MAX_EXPR_DEPTH
```

This is a defense-in-depth measure to prevent stack overflow from deeply nested expressions.

### 8. Security: Max Effect Set Size Limit

**File**: `src/compiler/types/catena_infer_effect.erl`

Added size limit check to `union/2`:

```erlang
-define(MAX_EFFECT_SET_SIZE, 100).

%% Updated union/2 to check size
union({effect_set, E1}, {effect_set, E2}) ->
    Result = normalize({effect_set, E1 ++ E2}),
    check_size(Result),
    Result.

%% New function
check_size/1 - Throws error if > MAX_EFFECT_SET_SIZE
```

This prevents pathological cases where effect sets grow unboundedly.

### 9. Testing: Negative Test Cases for Traits

**File**: `test/compiler/types/catena_trait_negative_tests.erl` (NEW - 280 lines)

Created comprehensive negative tests covering:

- **Missing Instance Tests** (3 tests)
  - Non-existent trait resolution fails
  - Type without instance fails
  - Wrong arity type argument handling

- **Type Mismatch Tests** (2 tests)
  - Method type inference with wrong argument types
  - Constraint accumulation for later solving

- **Circular Dependency Tests** (2 tests)
  - Self-referential constraints don't cause infinite loop
  - Mutual dependencies terminate

- **Overlapping Instance Tests** (2 tests)
  - More specific instance preference
  - Instance uniqueness enforcement

- **Invalid Constraint Tests** (3 tests)
  - Empty type list rejection
  - Wrong arity rejection
  - Malformed type handling

- **Edge Case Tests** (3 tests)
  - Empty instance database errors
  - Unknown method name handling
  - is_trait_method returns false for unknown methods

## Test Results

All 2447 tests pass:
- `catena_stdlib_integration_tests`: 23 tests
- `catena_desugar_properties`: 18 tests
- `catena_effect_kleisli_tests`: 27 tests
- `catena_hkt_validation_tests`: 25 tests
- `catena_codegen_module_tests`: 22 tests
- `catena_e2e_tests`: 12 tests
- `catena_infer_state_tests`: 27 tests
- `catena_trait_negative_tests`: 15 tests (NEW)

## Files Changed Summary

| File | Change Type | Lines Changed |
|------|-------------|---------------|
| `catena_stdlib_integration_tests.erl` | NEW | +288 |
| `catena_trait_negative_tests.erl` | NEW | +280 |
| `catena_parser.yrl` | MODIFIED | +20 |
| `catena_desugar.erl` | MODIFIED | +5 |
| `catena_desugar_properties.erl` | MODIFIED | +7, -8 |
| `catena_test_helpers.erl` | MODIFIED | +12 |
| `catena_effect_kleisli_tests.erl` | MODIFIED | +2, -7 |
| `catena_hkt_validation_tests.erl` | MODIFIED | +3, -5 |
| `catena_codegen_module_tests.erl` | MODIFIED | +2, -4 |
| `catena_e2e_tests.erl` | MODIFIED | +35, -78 |
| `catena_infer_expr.erl` | MODIFIED | -12 |
| `catena_infer_state.erl` | MODIFIED | +70 |
| `catena_infer_effect.erl` | MODIFIED | +15 |
| `catena_effect_infer.erl` | NEW | +414 |

### 10. Architecture: Dedicated Effect Inference Module

**File**: `src/compiler/types/catena_effect_infer.erl` (NEW - 414 lines)

Created a comprehensive effect inference module that serves as the main entry point for effect inference during type checking. The module provides:

**Main Inference Entry Points**:
- `infer_effects/2`, `infer_effects/3` - Walk AST and accumulate effects

**Expression-Specific Inference**:
- `infer_perform/4` - Handle `perform Effect.operation(args)` expressions
- `infer_handle/4` - Handle effect handler blocks with scope management
- `infer_application/4` - Propagate function effects

**Effect Propagation**:
- `propagate_effects/2` - Add function effects to current context
- `merge_branch_effects/2` - Union effects from conditional branches

**Effect Resolution**:
- `resolve_handler/3` - Remove handled effects from effect set
- `remove_effect/2` - Remove single effect

**Utilities**:
- `extract_function_effects/1` - Get effects from function type
- `build_function_type/3` - Construct typed function with effects
- `effects_of/1` - Get effects from state
- `with_effect_scope/2` - Execute in isolated effect scope

This module orchestrates effect inference and prepares the infrastructure for Phase 6 effect polymorphism.

## Remaining Items from Review

The following items remain:

- Property-based law tests (blocked by module system maturity)
