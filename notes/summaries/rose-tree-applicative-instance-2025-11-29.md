# Rose Tree Applicative Instance - Implementation Summary

**Date**: 2025-11-29
**Branch**: `feature/rose-tree-applicative-instance`
**Status**: Complete
**Phase**: Property Testing Framework - Phase 1, Section 1.1.4

---

## Overview

Implemented the applicative functor instance for rose trees, enabling combination of multiple generated values with interleaved shrinking. The applicative provides `pure`, `ap`, and `liftA2` operations that are essential for combining generators in property-based testing while preserving good shrinking behavior.

## Applicative Concept

An applicative functor extends a functor with the ability to apply wrapped functions to wrapped values:

- **pure**: Lift a value into the applicative context (creates a singleton tree)
- **ap**: Apply a tree of functions to a tree of values with interleaved shrinking
- **liftA2**: Lift a binary function to work on two trees

The key insight for rose trees is **interleaved shrinking**: when combining two trees, we shrink the left tree first, then the right. This ensures systematic exploration of the shrink space during test failure minimization.

## Implementation Details

### Functions Added to `catena_tree.erl`

| Function | Type Signature | Description |
|----------|---------------|-------------|
| `pure/1` | `A -> tree(A)` | Lift a value into a singleton tree |
| `ap/2` | `tree(A -> B), tree(A) -> tree(B)` | Apply function tree to value tree |
| `liftA2/3` | `(A, B -> C), tree(A), tree(B) -> tree(C)` | Lift binary function over two trees |

### Interleaved Shrinking Strategy

The `ap/2` function implements interleaved shrinking:

```erlang
ap(TreeF, TreeX) ->
    F = root(TreeF),
    X = root(TreeX),
    #tree{
        root = F(X),
        children = fun() ->
            %% Left shrinks first, then right shrinks
            LeftShrinks = [ap(ShrunkF, TreeX) || ShrunkF <- children(TreeF)],
            RightShrinks = [ap(TreeF, ShrunkX) || ShrunkX <- children(TreeX)],
            LeftShrinks ++ RightShrinks
        end
    }.
```

This ordering ensures:
1. Shrinking the function (left) is tried before shrinking the argument (right)
2. All shrink possibilities are systematically explored
3. Laziness is preserved - shrinks are only computed when accessed

### liftA2 via Currying

The `liftA2` function is derived from `ap` and `map`:

```erlang
liftA2(F, TreeA, TreeB) when is_function(F, 2) ->
    ap(map(fun(A) -> fun(B) -> F(A, B) end end, TreeA), TreeB).
```

This curries the binary function and applies it in two stages.

## Applicative Laws Verified

All four applicative functor laws are tested:

1. **Identity**: `ap(pure(id), v) == v`
2. **Homomorphism**: `ap(pure(f), pure(x)) == pure(f(x))`
3. **Interchange**: `ap(u, pure(y)) == ap(pure(fun(f) -> f(y) end), u)`
4. **Composition**: `ap(ap(ap(pure(compose), u), v), w) == ap(u, ap(v, w))`

Additionally verified:
- **Map/Ap equivalence**: `map(f, x) == ap(pure(f), x)`

## Test Coverage

Added 23 new tests for applicative operations:

### pure/1 Tests (3 tests)
- `pure_creates_singleton_test` - Creates tree with no children
- `pure_equivalent_to_singleton_test` - Equivalent to singleton/1
- `pure_with_function_value_test` - Can lift functions as values

### ap/2 Tests (8 tests)
- `ap_applies_function_to_value_test` - Basic application
- `ap_with_no_shrinks_test` - Singletons produce singleton
- `ap_interleaved_shrinking_test` - Left shrinks before right
- `ap_left_shrinks_before_right_test` - Ordering verification
- `ap_only_fn_shrinks_test` - Only function has shrinks
- `ap_only_val_shrinks_test` - Only value has shrinks
- `ap_nested_shrinks_test` - Multiple shrink levels
- `ap_preserves_laziness_test` - Children not evaluated eagerly

### liftA2/3 Tests (4 tests)
- `liftA2_combines_two_trees_test` - Basic combination
- `liftA2_with_shrinks_test` - Shrinking preserved
- `liftA2_tuple_construction_test` - Build tuples from trees
- `liftA2_three_args_via_composition_test` - Chain for 3+ args

### Applicative Law Tests (5 tests)
- `applicative_law_identity_test`
- `applicative_law_homomorphism_test`
- `applicative_law_interchange_test`
- `applicative_law_composition_test`
- `applicative_map_ap_equivalence_test`

### Edge Case Tests (3 tests)
- `ap_deep_shrink_tree_test` - Deep shrink trees (5 levels)
- `ap_many_shrinks_test` - Wide shrink trees (10 shrinks each)
- `liftA2_commutative_operation_test` - Verifies shrink order differs

## Test Count

- **Previous**: 59 tests
- **New applicative tests**: 23
- **Total**: 82 tests

## Files Changed

| File | Lines Changed | Status |
|------|---------------|--------|
| `src/proptest/catena_tree.erl` | +36 | Modified |
| `test/proptest/catena_tree_tests.erl` | +332 | Modified |

## API Reference

```erlang
%% Section 1.1.4: Applicative Instance
-spec pure(A) -> tree(A).
-spec ap(tree(fun((A) -> B)), tree(A)) -> tree(B).
-spec liftA2(fun((A, B) -> C), tree(A), tree(B)) -> tree(C).
```

## Use Cases for Applicative Operations

### Combining Generators

```erlang
%% Generate a point from two integer trees
PointGen = catena_tree:liftA2(
    fun(X, Y) -> {point, X, Y} end,
    IntTreeX,
    IntTreeY
).
```

### Building Records

```erlang
%% Apply a constructor to multiple generated fields
pure(fun(Name, Age) -> #{name => Name, age => Age} end)
|> ap(NameTree)
|> ap(AgeTree).
```

## Next Steps

The remaining tasks for rose tree implementation:
- **Section 1.1.5**: Monad instance (`bind`, `flatten`)

## References

- Planning document: `notes/planning/property-testing/phase-01.md`
- Previous work: `notes/summaries/rose-tree-functor-edge-cases-2025-11-29.md`
- Applicative functor laws: https://wiki.haskell.org/Typeclassopedia#Applicative
