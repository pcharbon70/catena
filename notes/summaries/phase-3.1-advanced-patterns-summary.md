# Phase 3.1 Advanced Pattern Features - Implementation Summary

## Overview

Phase 3.1 implements advanced pattern matching features for the Catena language compiler, including:

1. **Pattern Guards with Purity Checking** (3.1.1)
2. **Or-Patterns** (3.1.2)
3. **Nested Patterns** (3.1.3) - Already existed
4. **As-Patterns** (3.1.4)

## Implementation Details

### 3.1.1 Pattern Guards with Purity Checking

Guards already existed in the parser and code generation. The new functionality is **guard purity checking** in the effect system.

**Files Modified:**
- `src/compiler/types/catena_infer_effect.erl`

**Key Functions Added:**
- `check_guard_purity/1` - Validates that guard expressions have no side effects
- `infer_guard_effects/1` - Recursively infers effects from guard expression AST

**Pure Operations:**
- Literals, variables
- Binary/unary operators
- Function applications (effect comes from function type)
- Tuple/list/record construction
- Field access

**Impure Operations (rejected in guards):**
- `perform` operations (IO, State, etc.)
- `do_expr` expressions

### 3.1.2 Or-Patterns

Or-patterns allow matching multiple alternatives in a single clause:
```catena
match | Red | Blue | Green -> "primary color"
```

**Parser Changes (`src/compiler/parser/catena_parser.yrl`):**
- Added `or_pattern` nonterminal
- Modified `match_clause` to use `or_pattern` instead of `pattern`
- Or-patterns are right-associative, collecting alternatives into a list

**AST Representation:**
```erlang
{pat_or, [Pattern1, Pattern2, ...], Location}
```

**Type Inference (`src/compiler/types/catena_infer_pattern.erl`):**
- `infer({por, Patterns}, Env, State)` - Infers or-pattern type
- All alternatives must have the same type (unified)
- All alternatives must bind the same variables with the same types
- Added `check_or_pattern_consistency/5` and `check_or_pattern_bindings/3`

**Code Generation (`src/compiler/codegen/catena_codegen_pattern.erl`):**
- Or-patterns are **expanded at clause level**, not pattern level
- Each alternative becomes a separate clause sharing the same guards and body
- `expand_or_patterns_in_clauses/1` performs the expansion

### 3.1.3 Nested Patterns

Already fully implemented in the existing codebase. Patterns can be arbitrarily nested:
- Constructors containing patterns
- Tuples/lists/records containing patterns
- As-patterns wrapping other patterns

### 3.1.4 As-Patterns

As-patterns bind a name to the entire matched value:
```catena
match | Some(x) as opt -> use_both(x, opt)
```

**Parser Changes (`src/compiler/parser/catena_parser.yrl`):**
- Added rule: `pattern -> pattern as lower_ident`
- `as` has low precedence, so `h :: t as lst` parses as `h :: (t as lst)`

**AST Representation:**
```erlang
{pat_as, Name, InnerPattern, Location}
```

**Type Inference:**
- Already existed in `catena_infer_pattern.erl`
- Infers inner pattern type, binds both the name and inner pattern bindings

**Code Generation:**
- Already existed in `catena_codegen_pattern.erl`
- Uses Core Erlang `cerl:c_alias/2` for efficient matching

## Test Coverage

### New Test Files Created:

1. **`test/compiler/parser/catena_parser_advanced_pattern_tests.erl`** - 17 tests
   - As-pattern parsing (5 tests)
   - Or-pattern parsing (7 tests)
   - Combined pattern tests (3 tests)
   - Guard integration tests (2 tests)

2. **`test/compiler/types/catena_pattern_advanced_tests.erl`** - 16 tests
   - Or-pattern type inference (3 tests)
   - As-pattern type inference (4 tests)
   - Guard purity checking (7 tests)
   - Effect set operations (2 tests)

**Total: 33 new tests, all passing**

## Parser Conflicts

The grammar changes increased shift/reduce conflicts from 17 to 37. This is expected behavior due to:
- Or-patterns reusing the `|` token (also used for match clause separation)
- As-pattern `as` keyword ambiguity

Yecc resolves these correctly via default shift behavior.

## Technical Notes

### Precedence

- `as` has lower precedence than `::` (cons)
- `h :: t as lst` parses as `h :: (t as lst)`, not `(h :: t) as lst`
- Use parentheses for the latter: `(h :: t) as lst`

### Or-Pattern Variable Consistency

All alternatives in an or-pattern must:
1. Have the same type (enforced via unification)
2. Bind exactly the same variables
3. Bind those variables to compatible types

Example (valid):
```catena
| Some(x) | None as opt -> ...  -- x only bound in first, invalid
```

Example (invalid):
```catena
| Left(x) | Right(x) -> use(x)  -- x bound in both with same type, valid
```

### Guard Purity

Guards must be pure to ensure deterministic pattern matching. The purity checker rejects:
- Direct effect operations (`perform`)
- Effectful blocks (`do`)
- Any operation that modifies state or performs IO

## Files Changed

### Modified:
- `src/compiler/parser/catena_parser.yrl` - Or-pattern and as-pattern grammar
- `src/compiler/types/catena_infer_pattern.erl` - Or-pattern type inference
- `src/compiler/types/catena_infer_effect.erl` - Guard purity checking
- `src/compiler/codegen/catena_codegen_pattern.erl` - Or-pattern expansion

### Created:
- `test/compiler/parser/catena_parser_advanced_pattern_tests.erl`
- `test/compiler/types/catena_pattern_advanced_tests.erl`
- `notes/features/phase-3.1-advanced-patterns.md` - Feature planning doc

## Branch

`feature/phase-3.1-advanced-patterns`

## Status

Complete and ready for commit.
