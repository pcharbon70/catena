# Phase 3.2 Decision Tree Pattern Compilation - November 25, 2025

## Overview

This session implemented Section 3.2 of Phase 3 (Pattern Matching Engine) from the proof-of-concept plan. The implementation adds efficient decision tree-based pattern matching compilation using the Maranget algorithm.

## Files Created

### Source Code
- `src/compiler/codegen/catena_pattern_decision_tree.erl` (771 lines)
  - Decision tree data structure
  - Tree construction algorithm
  - Column selection heuristics
  - Matrix specialization operations
  - Code generation to Core Erlang
  - Failure compilation

### Tests
- `test/compiler/codegen/catena_pattern_decision_tree_tests.erl` (555 lines)
  - 36 unit tests covering all functionality

## Implementation Details

### 3.2.1 Decision Tree Generation

#### 3.2.1.1 Data Structure
The decision tree uses three main node types:
- `{leaf, Index, Guards, Action}` - Successfully matched clause
- `{switch, ColumnInfo, Branches, Default}` - Test node with branches
- `{fail, SourceLoc}` - Match failure

Pattern rows are represented as records:
```erlang
-record(pattern_row, {
    patterns :: [pattern()],
    guard :: term(),
    action :: term(),
    index :: non_neg_integer()
}).
```

#### 3.2.1.2 Tree Construction Algorithm
Follows Maranget's algorithm:
1. Build pattern matrix from clauses
2. If matrix is empty, return failure
3. If first row matches (all wildcards), return leaf (with guard handling)
4. Select best column to test using heuristics
5. Split matrix by constructors at that column
6. Recursively build subtrees for each constructor
7. Build default branch for wildcards

#### 3.2.1.3 Occurrence Analysis
The `analyze_patterns/1` function identifies:
- Constructor distribution per column
- Wildcard counts
- Sharing opportunities (rows with identical actions)

#### 3.2.1.4 Tree Balancing
Implicit in column selection - choosing columns with more constructors creates more balanced trees.

### 3.2.2 Code Generation Optimization

#### Core Erlang Generation
The `tree_to_core/3` function generates:
- `cerl:c_case/2` for switch nodes
- Pattern matching clauses with bound variables
- Guard evaluation with fallback handling

#### Branch Handling
- Constructor branches compile to specific patterns
- Tuples: `cerl:c_tuple/1`
- Lists: `cerl:c_cons/2`, `cerl:c_nil/0`
- Literals: `cerl:c_int/1`, `cerl:c_atom/1`, etc.

### 3.2.3 Test Ordering Heuristics

Column selection uses cost/benefit analysis:

**Benefit calculation** (`column_benefit/2`):
- Proportion of non-wildcard patterns at a column
- Higher = more discrimination power

**Cost calculation** (`column_cost/2`):
- Wildcards: 0.0 (free)
- Literals: 1.0
- Constructors: 1.0 + 0.5 * arity
- Tuples: 0.5 + 0.5 * arity
- Lists: 1.0-1.5

**Score** = benefit / cost (with threshold for near-zero cost)

### 3.2.4 Failure Compilation

Two modes supported:
1. **Strict mode** (default): Generate `erlang:error({match_error, ...})`
2. **Lenient mode**: Return a default value on failure

Error information includes:
- Source location (line/column)
- Match context

## API Summary

```erlang
%% Main API
compile(Clauses, Scrutinees, State) -> {CoreExpr, NewState}
compile_with_opts(Clauses, Scrutinees, State, Opts) -> {CoreExpr, NewState}

%% Tree Construction
build_tree(PatternMatrix, Opts) -> TreeNode
pattern_matrix_from_clauses(Clauses) -> PatternMatrix

%% Analysis
analyze_patterns(PatternMatrix) -> #{...}
select_column(PatternMatrix, Opts) -> ColumnIndex

%% Testing exports
specialize_matrix(Matrix, ColIdx, Constructor) -> SpecializedMatrix
default_matrix(Matrix, ColIdx) -> DefaultMatrix
is_complete_signature(Constructors, Type) -> boolean()
```

## Supported Pattern Types

- Variables (`{pat_var, Name, Loc}`)
- Wildcards (`{pat_wildcard, Loc}`)
- Literals (`{pat_literal, Value, Type, Loc}`)
- Constructors (`{pat_constructor, Name, Args, Loc}`)
- Tuples (`{pat_tuple, Elements, Loc}`)
- Lists (`{pat_list, Elements, Loc}`)
- Cons (`{pat_cons, Head, Tail, Loc}`)
- As-patterns (`{pat_as, Name, Pattern, Loc}`)
- Or-patterns (`{pat_or, Alternatives, Loc}`)
- Records (`{pat_record, Fields, Loc}`)

## Test Results

- All 36 new unit tests pass
- All 2202 existing tests pass
- Total test count increased from 2166 to 2202

## Git Branch

Branch: `feature/phase-3.2-decision-trees`

## Notes for Future Development

1. **Integration**: The new module can replace the simplified decision tree in `catena_codegen_pattern.erl` (lines 266-359)

2. **Optimization opportunities**:
   - Share identical subtrees (detected by `find_sharing/1`)
   - Use BEAM's `select_val` instruction for large literal switches
   - Peephole optimization for redundant tests

3. **Type information**: Complete signature detection would benefit from type info during compilation (currently assumes incomplete for unknown types)

4. **Guard optimization**: Complex guards could be hoisted or shared when possible
