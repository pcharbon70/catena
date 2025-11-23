# Extend and Constrain Keywords

**Date**: 2025-11-23

## Summary

Added `extend` and `constrain` keywords to improve readability of trait inheritance and type constraints. Previously these used symbolic syntax (`:` for extends, `=>` for constraints). Now they use explicit keywords for better developer experience.

## Syntax Changes

### Trait Inheritance

**Before (using colon):**
```catena
trait Ord a : Eq a where
  compare : a -> a -> Ordering
end
```

**After (using extend):**
```catena
trait Ord a extend Eq a where
  compare : a -> a -> Ordering
end
```

### Type Constraints

**Before (using double arrow):**
```catena
transform sort : Ord a => List a -> List a
```

**After (using constrain):**
```catena
transform sort : List a -> List a constrain Ord a
```

### Constraints with Effects

The `constrain` keyword comes after the effect annotation:
```catena
transform readAndSort : List a -> List a / {IO} constrain Ord a
```

### Multiple Constraints

Multiple constraints are comma-separated:
```catena
transform compare : a -> a -> Ordering constrain Eq a, Show a
```

## Implementation Details

### Lexer Changes

Added two new keywords in `src/compiler/lexer/catena_lexer.xrl`:
```erlang
%% Trait system keywords
extend : {token, {extend, TokenLine}}.
constrain : {token, {constrain, TokenLine}}.
```

### Parser Changes

1. **Added terminals** in `src/compiler/parser/catena_parser.yrl`:
   ```erlang
   %% Trait system keywords
   extend constrain
   ```

2. **Updated trait extends syntax** from `:` to `extend`:
   ```erlang
   maybe_trait_extends -> extend trait_extends_list : '$2'.
   maybe_trait_extends -> '$empty' : undefined.
   ```

3. **Added constrain production** to type expressions:
   ```erlang
   type_expr -> type_expr constrain type_constraints :
       {constrained_type, '$3', '$1', extract_location('$2')}.

   type_constraints -> trait_constraint :
       ['$1'].
   type_constraints -> trait_constraint comma type_constraints :
       ['$1' | '$3'].
   ```

4. **Removed old `=>` constraint syntax** from transform signatures and trait members (constraints now part of type_expr).

### AST Representation

Constrained types are represented as:
```erlang
{constrained_type, Constraints, Type, Location}
```

Where `Constraints` is a list of `trait_constraint` records.

## Tests Added

4 new tests in `test/compiler/parser/catena_parser_trait_tests.erl`:

1. `parse_transform_valid_constrain_test` - Basic constrain
2. `parse_transform_valid_multiple_constrain_test` - Multiple constraints
3. `parse_transform_valid_effects_and_constrain_test` - Effects with constrain
4. `parse_trait_method_valid_constrain_test` - Trait method with constrain

All new tests pass.

## Parser Conflicts

Shift/reduce conflicts increased from 17 to 37 due to the new `constrain` production. These resolve correctly via shift (parser continues building the constrained type when it sees `constrain`).

## Design Rationale

1. **Readability**: `extend` and `constrain` are more readable than `:` and `=>`
2. **Consistency**: Both use explicit keywords rather than symbolic operators
3. **Clarity**: `constrain` after the type reads as "type constrained by" which matches natural language
4. **PoC Focus**: The proof-of-concept emphasizes readable word-based syntax over symbolic operators

## Instance Constraints

Note: Instance constraints still use `=>` syntax:
```catena
instance Eq a => Eq (List a) where
  ...
end
```

This reads as "given Eq a, we can have Eq (List a)" which is semantically different from type constraints. This syntax may be updated in a future iteration.

## Files Modified

- `src/compiler/lexer/catena_lexer.xrl` - Added `extend` and `constrain` keywords
- `src/compiler/parser/catena_parser.yrl` - Updated grammar for new keywords
- `test/compiler/parser/catena_parser_trait_tests.erl` - Updated existing extends tests, added constrain tests

## Known Issues

Some pre-existing tests in `catena_parser_trait_tests.erl` fail due to record matching issues (tests use `#trait_decl{}` but parser returns tuples). This is unrelated to the keyword changes.
