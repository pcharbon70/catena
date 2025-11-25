# Phase 3.3 Exhaustiveness and Redundancy Checking - November 25, 2025

## Overview

This session implemented Section 3.3 of Phase 3 (Pattern Matching Engine) from the proof-of-concept plan. The implementation adds static analysis for pattern matching to ensure patterns are complete (exhaustiveness) and contain no unreachable code (redundancy).

## Files Created

### Source Code
- `src/compiler/semantic/catena_pattern_check.erl` (675 lines)
  - Exhaustiveness analysis
  - Redundancy detection
  - Usefulness algorithm (Maranget)
  - Warning generation
  - SMT integration stubs for guards

### Tests
- `test/compiler/semantic/catena_pattern_check_tests.erl` (500 lines)
  - 42 unit tests covering all functionality

## Implementation Details

### 3.3.1 Exhaustiveness Analysis

Uses the "usefulness" algorithm from Luc Maranget's paper "Warnings for pattern matching" (2007).

**Key insight**: A pattern is "useful" if it matches some value not matched by previous patterns. The algorithm:
1. Build a pattern matrix from all patterns
2. For each potential witness pattern, check if it's useful against the matrix
3. Useful witnesses represent missing cases

**Supported types**:
- ADTs with constructors: `{adt, Name, [{CtorName, Arity}, ...]}`
- Booleans: `{bool}`
- Lists: `{list, ElemType}`
- Tuples: `{tuple, ElemTypes}`
- Infinite types: `{int}`, `{float}`, `{string}`

**API**:
```erlang
check_exhaustiveness(PatternMatrix, TypeInfo) -> boolean()
find_missing_patterns(PatternMatrix, TypeInfo) -> [pattern()]
```

### 3.3.2 Redundancy Detection

A pattern is redundant if it's not useful against the patterns before it.

**Algorithm**:
1. For each pattern in order, check if it's useful against preceding patterns
2. If not useful, mark as redundant
3. Return list of `{Index, Pattern}` for all redundant patterns

**API**:
```erlang
check_redundancy(PatternMatrix, TypeInfo) -> boolean()
find_redundant_patterns(PatternMatrix, TypeInfo) -> [{Index, Pattern}]
```

### 3.3.3 Warning Generation

Two warning types:
- `non_exhaustive`: Missing patterns with examples
- `redundant_pattern`: Unreachable patterns with explanation

**Warning format**:
```erlang
{warning, Type, Data, Location}
```

**Formatting functions**:
```erlang
format_missing_warning([Pattern], Loc) -> binary()
format_redundancy_warning(Index, Pattern, Loc) -> binary()
```

Warnings can be selectively enabled/disabled via options:
```erlang
check_match(Patterns, TypeInfo, #{warnings => [non_exhaustive]})
```

### 3.3.4 SMT Integration for Guards

Provides infrastructure for SMT-based guard analysis (placeholder for full implementation):

**API**:
```erlang
check_guard_exhaustiveness([{Pattern, Guard}], TypeInfo) ->
    exhaustive | non_exhaustive | unknown
```

**SMT translation** (stubbed):
```erlang
guard_to_smtlib(Guard) -> iolist()  % SMT-LIB format
```

Currently returns `unknown` when patterns aren't exhaustive without guards, as full SMT integration would require an external solver like Z3.

### Core Algorithm: Usefulness

The `is_useful/2` function is the heart of the algorithm:

```erlang
is_useful(Matrix, Row) ->
    %% Empty matrix - any pattern is useful
    %% Empty row - base case
    %% Otherwise:
    %%   - If row starts with wildcard: useful_wildcard()
    %%   - If row starts with constructor: specialize and recurse
```

**Matrix operations**:
- `specialize(Matrix, Ctor)`: Keep rows matching constructor, expand args
- `default_matrix(Matrix)`: Keep rows with wildcard in first column

**Handling infinite types**:
Wildcards are always useful against finite sets of literals (integers, floats, strings) because there are infinitely many other values.

## API Summary

### Main Entry Point
```erlang
check_match(Patterns, TypeInfo) -> #{
    exhaustive := boolean(),
    missing := [pattern()],
    redundant := [{non_neg_integer(), pattern()}],
    warnings := [warning()]
}
```

### Pattern Subsumption
```erlang
pattern_subsumes(P1, P2) -> boolean()  % P1 covers all values of P2
patterns_overlap(P1, P2) -> boolean()  % P1 and P2 share some values
```

## Supported Pattern Types

- Variables: `{pat_var, Name, Loc}`
- Wildcards: `{pat_wildcard, Loc}`
- Literals: `{pat_literal, Value, Type, Loc}`
- Constructors: `{pat_constructor, Name, Args, Loc}`
- Tuples: `{pat_tuple, Elements, Loc}`
- Lists: `{pat_list, Elements, Loc}`
- Cons: `{pat_cons, Head, Tail, Loc}`
- As-patterns: `{pat_as, Name, Inner, Loc}`
- Or-patterns: `{pat_or, Alternatives, Loc}`

## Test Results

- All 42 new unit tests pass
- All 2244 existing tests pass (up from 2202)

## Git Branch

Branch: `feature/phase-3.3-exhaustiveness-checking`

## Usage Examples

### Check Boolean Exhaustiveness
```erlang
Patterns = [{pat_literal, true, bool, Loc}],
Result = catena_pattern_check:check_match(Patterns, {bool}),
%% Result: #{exhaustive => false, missing => [{pat_literal, false, bool, _}], ...}
```

### Check ADT Coverage
```erlang
MaybeType = {adt, 'Maybe', [{'None', 0}, {'Some', 1}]},
Patterns = [
    {pat_constructor, 'None', [], Loc},
    {pat_constructor, 'Some', [{pat_wildcard, Loc}], Loc}
],
Result = catena_pattern_check:check_match(Patterns, MaybeType),
%% Result: #{exhaustive => true, missing => [], redundant => [], ...}
```

### Detect Redundancy
```erlang
Patterns = [
    {pat_wildcard, Loc},
    {pat_literal, true, bool, Loc}  % Unreachable!
],
Result = catena_pattern_check:check_match(Patterns, {bool}),
%% Result: #{redundant => [{1, {pat_literal, true, bool, _}}], ...}
```

## Notes for Future Development

1. **Full SMT integration**: Would require spawning Z3 process and parsing results

2. **Polymorphic types**: Current implementation assumes monomorphic types; polymorphic exhaustiveness would need type variable tracking

3. **Guard analysis**: Currently conservative; could be improved with abstract interpretation

4. **Performance**: For very large pattern sets (1000+), consider caching intermediate results

5. **Integration**: Should be called from type checker after pattern type inference
