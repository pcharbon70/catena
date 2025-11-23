# Section 1.5.2 - Trait Instance Resolution

## Summary

Completed Section 1.5.2 which validates that trait instances resolve correctly during type checking. The prelude's 17 instances (for Maybe, Either, List) now resolve properly using the instance resolution infrastructure.

## Completed Tasks

### 1.5.2.1 Resolve Mapper instance for Maybe ✅
- Test: `resolve_mapper_maybe_test`
- Resolves `Mapper` constraint for `{tcon, 'Maybe'}` successfully

### 1.5.2.2 Resolve Mapper instance for List ✅
- Test: `resolve_mapper_list_test`
- Resolves `Mapper` constraint for `{tcon, 'List'}` successfully

### 1.5.2.3 Resolve constrained instances ✅
- Tests: `resolve_comparable_maybe_a_test`, `resolve_comparable_list_a_test`
- Resolves parameterized instances like `Comparable (Maybe a)`
- Unification works: `Comparable (Maybe Int)` matches instance `Comparable (Maybe a)` with substitution `a → Int`
- Also works for `Either e` instances via `resolve_mapper_either_test`

### 1.5.2.4 Detect and report missing instances ✅
- Test: `resolve_missing_instance_test`
- Returns `{error, no_instance}` when no matching instance exists
- Example: `Mapper Int` correctly fails since Int is not a functor

### 1.5.2.5 Verify trait hierarchy resolution ✅
- Tests: `resolve_pipeline_maybe_test`, `resolve_applicator_maybe_test`, `resolve_chainable_maybe_test`, `resolve_pipeline_list_test`
- All traits in hierarchy resolve correctly:
  - Pipeline for Maybe/List
  - Applicator for Maybe/List
  - Chainable for Maybe/List
  - Mapper for Maybe/List/Either

## Changes Made

### `src/compiler/types/catena_instance.erl`

Added `build_instance_db/1` function to build instance database from AST declarations:

```erlang
-spec build_instance_db([term()]) -> instance_db().
build_instance_db(Declarations) ->
    lists:foldl(
        fun(Decl, DB) ->
            case Decl of
                {instance_decl, TraitName, TypeArgs, _Constraints, _Methods, Loc} ->
                    InternalTypes = [ast_type_to_internal(T) || T <- TypeArgs],
                    Instance = make_instance(TraitName, InternalTypes, Loc),
                    add_instance(Instance, DB);
                _ ->
                    DB
            end
        end,
        empty_instance_db(),
        Declarations
    ).
```

Added `ast_type_to_internal/1` helper to convert AST types to internal representation:
- Converts `{type_con, Name, Loc}` to `{tcon, Name}`
- Converts `{type_var, Name, Loc}` to `{tvar, erlang:phash2(Name, 1000000)}`
  - Uses hash to convert atom names to integer IDs for unification
- Converts `{type_app, Con, Args, Loc}` to `{tapp, ...}` recursively

### `test/compiler/integration/catena_stdlib_tests.erl`

Added 10 new tests for instance resolution:

1. `load_prelude_instances/0` - Helper to load prelude and build instance DB
2. `resolve_mapper_maybe_test/0` - Test 1.5.2.1
3. `resolve_mapper_list_test/0` - Test 1.5.2.2
4. `resolve_comparable_maybe_a_test/0` - Test 1.5.2.3
5. `resolve_comparable_list_a_test/0` - Test 1.5.2.3
6. `resolve_missing_instance_test/0` - Test 1.5.2.4
7. `resolve_pipeline_maybe_test/0` - Test 1.5.2.5
8. `resolve_applicator_maybe_test/0` - Test 1.5.2.5
9. `resolve_chainable_maybe_test/0` - Test 1.5.2.5
10. `resolve_pipeline_list_test/0` - Test 1.5.2.5
11. `resolve_mapper_either_test/0` - Test constrained instance

## Test Results

### catena_stdlib_tests: 33/34 pass
- All 10 new instance resolution tests pass
- Only failure: `parse_trait_default_impl_test` (pre-existing parser limitation)

## Technical Notes

### Type Variable Conversion
The key insight was that AST type variables use atom names (`a`, `e`) but the internal type system uses integer IDs for unification. The `ast_type_to_internal` function converts atoms to integers using `erlang:phash2/2` for consistent hashing.

### Instance Resolution Flow
1. `build_instance_db/1` extracts `instance_decl` nodes from AST
2. Converts AST types to internal representation
3. `resolve_constraint/2` finds matching instances by trait name and arity
4. `unify_instance/2` checks if instance types unify with constraint types
5. Returns `{ok, Instance, Substitution}` or `{error, no_instance}`

### Prelude Instances
The prelude defines 17 instances:
- Maybe: Mapper, Applicator, Chainable, Pipeline, Comparable (5)
- Either: Mapper, Applicator, Chainable, Pipeline (4)
- List: Mapper, Applicator, Chainable, Pipeline, Foldable, Comparable, Combiner, Accumulator (8)

## Branch

`feature/section-1.5.2-trait-instance-resolution`

## Files Modified

- `src/compiler/types/catena_instance.erl` - Added `build_instance_db/1` and `ast_type_to_internal/1`
- `test/compiler/integration/catena_stdlib_tests.erl` - Added 10 instance resolution tests
