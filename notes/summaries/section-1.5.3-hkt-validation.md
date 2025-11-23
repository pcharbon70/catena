# Section 1.5.3 - Higher-Kinded Type Validation

## Summary

Implemented kind checking for higher-kinded types (HKTs) to validate that type constructors are used correctly in traits and instances. The kind system ensures that types like `Mapper f` require `f` to have kind `Type -> Type`.

## Completed Tasks

### 1.5.3.1 Validate kind checking for Mapper trait ✅
- Analyzes how trait type parameters are used in method signatures
- Infers that `f` in `map : (a -> b) -> f a -> f b` has kind `Type -> Type`
- Tests: `kind_check_mapper_trait_test`, `kind_check_pipeline_trait_test`, `kind_check_comparable_trait_test`

### 1.5.3.2 Validate kind inference for instance declarations ✅
- Infers kinds from type constructor declarations (arity determines kind)
- `Maybe : Type -> Type` (1 parameter)
- `Either : Type -> Type -> Type` (2 parameters)
- Test: `kind_infer_maybe_instance_test`

### 1.5.3.3 Validate partially applied type constructors ✅
- `Either String : Type -> Type` (partial application)
- `Maybe Int : Type` (full application)
- Tests: `kind_infer_either_partial_test`, `kind_infer_maybe_int_test`

### 1.5.3.4 Report kind errors ✅
- Detects over-application: `Int Int` fails with `{error, {over_applied, star, 1, _}}`
- Test: `kind_error_over_applied_test`

## Implementation

### New Module: `src/compiler/types/catena_kind.erl`

Complete kind system implementation with:

#### Kind Representation
```erlang
-type kind() :: star | {arrow, kind(), kind()}.
```

- `star` - The kind of concrete types (Type)
- `{arrow, K1, K2}` - The kind of type constructors (K1 -> K2)

#### Key Functions

1. **`check_trait_kind/1`** - Infers kinds for trait type parameters
   - Analyzes method signatures to determine how type variables are used
   - If applied (e.g., `f a`), infers `Type -> Type`
   - Otherwise infers `Type`

2. **`infer_type_kind/2`** - Infers kind of a type expression
   - Handles `type_con`, `type_var`, `type_app`
   - Computes partial application results

3. **`validate_hkt/2`** - Validates all instances in a module
   - Builds trait kind map
   - Checks each instance type argument against expected kind

4. **`build_kind_env/1`** - Builds kind environment from declarations
   - Includes built-in types (Int, Bool, Maybe, Either, etc.)
   - Adds declared types with kinds based on arity

5. **`format_kind/1`** - Pretty prints kinds for error messages

#### Built-in Type Kinds
```erlang
#{
    'Int' => star,
    'Bool' => star,
    'String' => star,
    'Maybe' => {arrow, star, star},
    'List' => {arrow, star, star},
    'Either' => {arrow, star, {arrow, star, star}}
}
```

### Tests Added

8 new tests in `catena_stdlib_tests.erl`:

1. `kind_check_mapper_trait_test` - Mapper `f` : Type -> Type
2. `kind_check_pipeline_trait_test` - Pipeline `m` : Type -> Type
3. `kind_check_comparable_trait_test` - Comparable `a` : Type
4. `kind_infer_maybe_instance_test` - Maybe : Type -> Type
5. `kind_infer_either_partial_test` - Either String : Type -> Type
6. `kind_infer_maybe_int_test` - Maybe Int : Type
7. `kind_error_over_applied_test` - Int Int : error
8. `validate_hkt_prelude_test` - All prelude instances pass

## Test Results

### catena_stdlib_tests: 41/42 pass
- All 8 new HKT tests pass
- Only failure: `parse_trait_default_impl_test` (pre-existing parser limitation)

## Technical Notes

### Kind Inference Algorithm

1. For trait parameters:
   - Scan method signatures for type applications
   - If `f` appears in `f a`, infer `f : Type -> Type`
   - Otherwise `f : Type`

2. For type expressions:
   - Base types have kind from environment
   - Application `f a` where `f : K1 -> K2` gives kind `K2`
   - Multiple applications are applied successively

3. For validation:
   - Build map of trait name → parameter kinds
   - For each instance, check type args against expected kinds
   - Report mismatches with location info

### Error Types

- `{kind_mismatch, Expected, Actual}` - Instance type has wrong kind
- `{over_applied, Kind, N, Loc}` - Applied too many arguments
- `{cannot_apply, Kind, N, Loc}` - Cannot apply non-arrow kind

## Branch

`feature/section-1.5.3-hkt-validation`

## Files Created/Modified

- `src/compiler/types/catena_kind.erl` - New kind checking module (320 lines)
- `test/compiler/integration/catena_stdlib_tests.erl` - Added 8 HKT tests
