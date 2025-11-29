# Rose Tree Monad Instance - Implementation Summary

**Date**: 2025-11-29
**Branch**: `feature/rose-tree-monad-instance`
**Status**: Complete
**Phase**: Property Testing Framework - Phase 1, Section 1.1.5

---

## Overview

Implemented the monad instance for rose trees, enabling dependent generation where the second tree depends on the value produced by the first. The monad provides `return`, `bind`, and `flatten` operations essential for property-based testing scenarios that require sequential dependencies.

## Monad Concept

A monad extends an applicative functor with the ability to sequence computations where later computations depend on earlier results:

- **return**: Lift a value into the monadic context (alias for `pure`/`singleton`)
- **bind**: Thread a value through a tree-producing function (flatMap/>>=)
- **flatten**: Collapse nested trees into a single tree (join)

## Shrinking Trade-off

**IMPORTANT**: Monadic bind has suboptimal shrinking for dependent values. When shrinking the first value, the dependent values do NOT automatically re-shrink to match.

Consider this example:
```erlang
%% Generate a list of length N where N comes from the first tree
TreeN = catena_tree:tree(3, fun() -> [catena_tree:singleton(1)] end),
Result = catena_tree:bind(TreeN, fun(N) ->
    catena_tree:singleton(lists:duplicate(N, x))
end).
```

When N shrinks from 3 to 1, we get `[x]` instead of `[x, x, x]`, but the shrinking doesn't coordinate - we just re-run the function on the shrunk N.

**Recommendation**: Use Applicative (`ap`, `liftA2`) when values are independent, as it provides better shrinking behavior. Use `bind` only when you need true dependency.

## Implementation Details

### Functions Added to `catena_tree.erl`

| Function | Type Signature | Description |
|----------|---------------|-------------|
| `return/1` | `A -> tree(A)` | Lift a value into a singleton tree |
| `bind/2` | `tree(A), (A -> tree(B)) -> tree(B)` | Thread value through tree-producing function |
| `flatten/1` | `tree(tree(A)) -> tree(A)` | Collapse nested trees |

### Shrinking Strategy for bind

For `bind(TreeA, F)`:
1. Compute root: Extract A from TreeA, compute F(A), extract root of F(A)
2. Inner shrinks: Children from F(A) come first
3. Outer shrinks: Re-run F on each shrunk value of A

```erlang
bind(TreeA, F) when is_function(F, 1) ->
    A = root(TreeA),
    TreeB = F(A),
    B = root(TreeB),
    #tree{
        root = B,
        children = fun() ->
            InnerShrinks = children(TreeB),
            OuterShrinks = [bind(ShrunkA, F) || ShrunkA <- children(TreeA)],
            InnerShrinks ++ OuterShrinks
        end
    }.
```

### Shrinking Strategy for flatten

For `flatten(TreeOfTrees)`:
1. Root: Extract inner tree from outer root, extract root from that
2. Inner shrinks: Shrinks from the inner tree come first
3. Outer shrinks: Flatten each outer child

### Key Relationships

The following equivalences are verified by tests:
- `flatten(t) == bind(t, fun(x) -> x end)`
- `bind(t, f) == flatten(map(f, t))`

## Monad Laws Verified

All three monad laws are tested:

1. **Left Identity**: `bind(return(a), f) == f(a)`
2. **Right Identity**: `bind(m, return) == m`
3. **Associativity**: `bind(bind(m, f), g) == bind(m, fun(x) -> bind(f(x), g) end)`

## Test Coverage

Added 25 new tests for monad operations:

### return/1 Tests (3 tests)
- `return_creates_singleton_test` - Creates tree with no children
- `return_equivalent_to_pure_test` - Same as pure/1
- `return_equivalent_to_singleton_test` - Same as singleton/1

### bind/2 Tests (9 tests)
- `bind_applies_function_to_root_test` - Basic application
- `bind_with_no_shrinks_test` - Singletons produce singleton
- `bind_preserves_inner_shrinks_test` - Shrinks from F(A) preserved
- `bind_preserves_outer_shrinks_test` - Shrinks from A re-run through F
- `bind_interleaves_inner_and_outer_shrinks_test` - Inner first, then outer
- `bind_dependent_generation_test` - Dependent value generation
- `bind_chaining_test` - Multiple binds in sequence
- `bind_preserves_laziness_test` - Children not evaluated eagerly

### flatten/1 Tests (5 tests)
- `flatten_nested_singletons_test` - Basic flattening
- `flatten_inner_shrinks_test` - Inner tree shrinks preserved
- `flatten_outer_shrinks_test` - Outer tree shrinks preserved
- `flatten_both_shrinks_test` - Both shrink sources combined
- `flatten_deeply_nested_test` - Triple nesting requires double flatten

### Monad Law Tests (4 tests)
- `monad_law_left_identity_test`
- `monad_law_right_identity_test`
- `monad_law_associativity_test`
- `monad_law_associativity_deep_test`

### Equivalence Tests (2 tests)
- `flatten_equals_bind_identity_test` - flatten == bind with identity
- `bind_equals_flatten_map_test` - bind == flatten . map

### Edge Case Tests (3 tests)
- `bind_deep_shrink_tree_test` - Deep shrink trees (5 levels)
- `bind_wide_shrink_tree_test` - Wide shrink trees (10 shrinks)
- `bind_type_changing_test` - Type-changing bind (int -> string)

## Test Count

- **Previous**: 82 tests
- **New monad tests**: 25
- **Total**: 107 tests

## Files Changed

| File | Lines Changed | Status |
|------|---------------|--------|
| `src/proptest/catena_tree.erl` | +135 | Modified |
| `test/proptest/catena_tree_tests.erl` | +336 | Modified |

## API Reference

```erlang
%% Section 1.1.5: Monad Instance
-spec return(A) -> tree(A).
-spec bind(tree(A), fun((A) -> tree(B))) -> tree(B).
-spec flatten(tree(tree(A))) -> tree(A).
```

## Use Cases for Monadic Operations

### Dependent Generation

```erlang
%% Generate a list of length N
catena_tree:bind(IntTree, fun(N) ->
    catena_tree:singleton(lists:duplicate(N, element))
end).
```

### Chaining Computations

```erlang
%% Chain multiple dependent operations
catena_tree:bind(TreeA, fun(A) ->
    catena_tree:bind(TreeB(A), fun(B) ->
        catena_tree:return({A, B, compute(A, B)})
    end)
end).
```

## Section 1.1 Completion Status

With the monad instance complete, all rose tree tasks from Section 1.1 are finished:

| Section | Task | Status |
|---------|------|--------|
| 1.1.1 | Rose Tree Type Definition | Complete |
| 1.1.2 | Comonadic Operations | Complete |
| 1.1.3 | Functor Instance | Complete |
| 1.1.4 | Applicative Instance | Complete |
| 1.1.5 | Monad Instance | Complete |

## Next Steps

Section 1.1 is now complete. The next phase is:
- **Section 1.2**: Generator Type and Seed Management

## References

- Planning document: `notes/planning/property-testing/phase-01.md`
- Previous work: `notes/summaries/rose-tree-applicative-instance-2025-11-29.md`
- Monad laws: https://wiki.haskell.org/Monad_laws
