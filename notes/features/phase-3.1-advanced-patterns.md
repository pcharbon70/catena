# Phase 3.1 Advanced Pattern Features Implementation

## Status: IN PROGRESS

## Overview

Section 3.1 implements advanced pattern matching features: guards with purity checking, or-patterns, nested patterns, and as-patterns. These features dramatically increase pattern expressiveness while maintaining type safety.

## Current State Analysis

### What Already Exists

**Parser (catena_parser.yrl):**
- Basic guard support with `when` keyword (lines 604-616)
- Pattern types: var, wildcard, constructor, literal, list, tuple, record, cons
- No or-patterns (`|` in patterns)
- No as-patterns (`pattern as name`)

**Pattern Inference (catena_infer_pattern.erl):**
- Already supports: wildcard, literal, variable, tuple, record, variant, as-pattern
- As-pattern inference exists at lines 88-98

**Pattern Codegen (catena_codegen_pattern.erl):**
- Already supports: var, wildcard, literal, constructor, list, cons, tuple, as, record
- Guard compilation exists (lines 191-228)
- As-pattern compilation exists at lines 154-159

**Tests (catena_parser_pattern_tests.erl):**
- 41 tests covering basic patterns and simple guards
- Tests for nested patterns exist (Section 6)
- No tests for or-patterns or as-patterns

### What Needs Implementation

| Feature | Parser | Inference | Codegen | Tests |
|---------|--------|-----------|---------|-------|
| 3.1.1 Pattern Guards | ✅ Exists | ❌ Need purity check | ✅ Exists | ⚠️ Need purity tests |
| 3.1.2 Or-Patterns | ❌ Missing | ❌ Missing | ❌ Missing | ❌ Missing |
| 3.1.3 Nested Patterns | ✅ Exists | ✅ Exists | ✅ Exists | ✅ Exists |
| 3.1.4 As-Patterns | ❌ Parser missing | ✅ Exists | ✅ Exists | ❌ Missing |

## Implementation Plan

### 3.1.1 Pattern Guards with Purity Checking
- [x] 3.1.1.1 Guard syntax parsing - **ALREADY EXISTS**
- [x] 3.1.1.2 Guard evaluation in pattern compiler - **ALREADY EXISTS**
- [x] 3.1.1.3 Guard failure handling - **ALREADY EXISTS**
- [ ] 3.1.1.4 Effect checking for guard purity

### 3.1.2 Or-Patterns
- [ ] 3.1.2.1 Implement or-pattern syntax parsing (`pattern1 | pattern2`)
- [ ] 3.1.2.2 Implement variable consistency checking
- [ ] 3.1.2.3 Implement or-pattern compilation
- [ ] 3.1.2.4 Implement or-pattern pretty-printing

### 3.1.3 Nested Patterns
- [x] 3.1.3.1 Nested pattern parsing - **ALREADY EXISTS**
- [x] 3.1.3.2 Nested pattern type checking - **ALREADY EXISTS**
- [x] 3.1.3.3 Nested pattern compilation - **ALREADY EXISTS**
- [ ] 3.1.3.4 Optimizations for deeply nested patterns (defer to 3.2)

### 3.1.4 As-Patterns
- [ ] 3.1.4.1 Implement as-pattern syntax parsing (`pattern as identifier`)
- [x] 3.1.4.2 As-pattern variable binding - **EXISTS in inference**
- [x] 3.1.4.3 As-pattern type inference - **EXISTS**
- [x] 3.1.4.4 As-pattern compilation - **EXISTS in codegen**

### Unit Tests
- [ ] Test pattern guards with various boolean conditions
- [ ] Test guard purity checking (reject effectful guards)
- [ ] Test or-patterns with multiple alternatives
- [ ] Test as-patterns binding both whole values and parts
- [ ] Test nested patterns (expand existing tests)

## Technical Details

### Or-Pattern AST Node
```erlang
{pat_or, [Pattern], Location}
```

### As-Pattern AST Node
```erlang
{pat_as, Name, Pattern, Location}
```

### Guard Purity Check
Guards must have empty effect set. Implementation in `catena_infer_effect.erl`:
```erlang
check_guard_purity(Guard, Env, State) ->
    {_Type, Effects, State1} = catena_infer:infer_expr(Guard, Env, State),
    case Effects of
        {effects, []} -> {ok, State1};
        {effects, _} -> {error, {impure_guard, Guard, Effects}}
    end.
```

## Files to Modify

1. `src/compiler/parser/catena_parser.yrl` - Add or-pattern and as-pattern syntax
2. `src/compiler/types/catena_infer_pattern.erl` - Add or-pattern inference
3. `src/compiler/codegen/catena_codegen_pattern.erl` - Add or-pattern compilation
4. `src/compiler/types/catena_infer.erl` - Add guard purity checking
5. `test/compiler/parser/catena_parser_pattern_tests.erl` - Add new tests
6. NEW: `test/compiler/types/catena_pattern_advanced_tests.erl` - Advanced pattern tests

## Success Criteria

1. Or-patterns parse correctly: `| Red | Green | Blue -> "color"`
2. As-patterns parse correctly: `| Cons x xs as list -> ...`
3. Guard purity is enforced (effectful guards rejected)
4. All existing pattern tests continue to pass
5. Variable consistency in or-patterns is checked
6. 85% test coverage on new code
