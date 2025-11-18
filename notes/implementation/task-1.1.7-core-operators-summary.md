# Task 1.1.7: Type-Level Equality Operators

**Date**: 2025-11-15
**Status**: ✅ Complete (Refactored)
**Branch**: `feature/task-1.1.7-core-operators`

---

## Overview

Implemented type-level equality operators (`===`, `!==`) in the Catena language grammar. Originally included 8 category theory operators, but refactored to keep only fundamental type-level equality operators.

**Rationale for Simplification**:
- Minimize core language surface area for PoC phase
- Category theory operators (`<$>`, `<*>`, `<>`, `>>=`, etc.) will be implemented as standard library functions
- Type-level equality is fundamental and distinct from value equality (`==`, `/=`)
- Keeps language beginner-friendly while maintaining category theory foundations for library

**Implementation Time**: ~5 hours (initial + refactoring)
**Tests Passing**:
- 41/41 trait tests (no regressions)
- 4/4 operator tests (type-level equality coverage)
**Parser Conflicts**: Stable at 17 shift/reduce (no new conflicts)

---

## Operators Implemented

### Type-Level Equality (Setoid)

| Operator | Token | Precedence | Associativity | Purpose |
|----------|-------|------------|---------------|---------|
| `===` | `setoid_eq` | 300 | nonassoc | Type-level equality |
| `!==` | `setoid_neq` | 300 | nonassoc | Type-level inequality |

**Example**:
```catena
trait Setoid a where
  eq : a -> a -> Bool
end

instance Setoid Bool where
  flow eq x y = x === y
end
```

**Why Keep These?**:
- Distinct from value equality (`==`) - structural vs. definitional equality
- Non-associative prevents chaining (`x === y === z` is invalid)
- Fundamental to type system operations
- Used for type-level comparisons in trait resolution

---

## Operators Removed (For Standard Library)

The following operators were removed from the language grammar and will be implemented as standard library functions in future phases:

### Category Theory Operators (Removed)

| Operator | Token | Will Become | Library Trait |
|----------|-------|-------------|---------------|
| `<$>` | `fmap` | `fmap` function | Functor |
| `<*>` | `ap` | `ap` function | Applicative |
| `<>` | `concat` | `append` function | Semigroup |

### Monadic Operators (Removed)

| Operator | Token | Will Become | Library Trait |
|----------|-------|-------------|---------------|
| `>>=` | `bind` | `bind` function | Monad |
| `>>` | `then_op` | `then` function | Monad |
| `=<<` | `bind_flip` | `bindFlip` function | Monad |
| `>=>` | `kleisli_lr` | `composeKleisli` function | Monad |
| `<=<` | `kleisli_rl` | `composeKleisliRev` function | Monad |

**Migration Strategy**:
When standard library is implemented, these will be available as:
```catena
-- Instead of: result <$> computation
-- Use: fmap result computation
-- Or: computation |> fmap result

-- Instead of: m >>= f >>= g
-- Use: m |> bind f |> bind g
```

---

## Complete Precedence Table

From lowest to highest precedence:

| Level | Operators | Associativity | Description |
|-------|-----------|---------------|-------------|
| 100 | `->` | right | Type-level function arrow |
| 160 | `\|>` | right | Pipe operator |
| 300 | `==`, `/=`, `===`, `!==` | nonassoc | Equality |
| 310 | `<`, `>`, `<=`, `>=` | nonassoc | Comparison |
| 400 | `+`, `-` | left | Addition, subtraction |
| 500 | `*`, `/` | left | Multiplication, division |
| 600 | `.` | left | Record field access |

---

## Files Modified

### 1. Lexer (`src/compiler/lexer/catena_lexer.xrl`)

**Changes**: Added 2 type-level equality operators

```erlang
%% Type-level equality operators
=== : {token, {setoid_eq, TokenLine}}.
!== : {token, {setoid_neq, TokenLine}}.
```

**Removed**: 8 category theory operator tokens (`<$>`, `<*>`, `<>`, `>>=`, `>>`, `=<<`, `>=>`, `<=<`)

### 2. Parser (`src/compiler/parser/catena_parser.yrl`)

**Terminals Added**:
```erlang
setoid_eq setoid_neq
```

**Precedence Rules**:
```erlang
Nonassoc 300 eq neq setoid_eq setoid_neq.  %% == /= === !==
```

**Grammar Rules**:
```erlang
expr -> expr setoid_eq expr :
    {binary_op, setoid_eq, '$1', '$3', extract_location('$2')}.

expr -> expr setoid_neq expr :
    {binary_op, setoid_neq, '$1', '$3', extract_location('$2')}.
```

**Removed**:
- 8 terminal declarations
- 4 precedence rules
- 8 expression grammar rules for category theory operators

### 3. Test Suite (`test/compiler/parser/catena_parser_operator_tests.erl`)

**Test Coverage**: 4 tests

1. **Setoid Equality Tests** (3 tests):
   - `parse_setoid_eq_in_trait_test` - Trait with `===` operator
   - `parse_instance_with_setoid_operators_test` - Instance using `===`
   - `parse_instance_with_setoid_neq_test` - Instance using `!==`

2. **Comprehensive Test** (1 test):
   - `parse_all_operators_comprehensive_test` - Both operators tokenize correctly

**Removed**: 13 tests for category theory operators

---

## Testing

### Operator Tests

**New Test Suite** (`catena_parser_operator_tests`): 4/4 tests passing ✅

All tests verify operators work correctly in realistic trait/instance contexts.

### Regression Tests

**Existing Test Suites** (all passing ✅):
- `catena_parser_trait_tests`: 41/41 tests passing
- No regressions introduced
- Parser compiles without new conflicts

### Verification

**Setoid operators work**:
```bash
$ erl -noshell -pa src/compiler/lexer -eval '
{ok, T1, _} = catena_lexer:string("x === y"),
io:format("=== : ~p~n", [T1])
' -s init stop

=== : [{lower_ident,1,"x"},{setoid_eq,1},{lower_ident,1,"y"}]
```

**Removed operators correctly fail**:
```bash
$ erl -noshell -pa src/compiler/lexer -eval '
Result = catena_lexer:string("x <> y"),
io:format("Concat test: ~p~n", [Result])
' -s init stop

Concat test: {ok,[{lower_ident,1,"x"},{lt,1},{gt,1},{lower_ident,1,"y"}],1}
```

The `<>` is now tokenized as separate `<` and `>` tokens, as expected.

---

## Build Process

### Lexer Generation
```bash
$ erlc -W0 -o src/compiler/lexer src/compiler/lexer/catena_lexer.xrl
```

### Parser Generation
```bash
$ ./scripts/build_parser.sh
✓ Parser generated successfully: src/compiler/parser/catena_parser.erl
Warning: conflicts: 17 shift/reduce, 0 reduce/reduce
```

### Compilation
```bash
$ erlc -W0 -o src/compiler/lexer src/compiler/lexer/catena_lexer.erl
$ erlc -W0 -o src/compiler/parser src/compiler/parser/catena_parser.erl
$ erlc -W0 -o src/compiler src/compiler/catena_compiler_utils.erl
```

---

## Success Criteria

### ✅ Functional Requirements

1. **Setoid Equality**:
   - [x] `===` and `!==` operators parse correctly
   - [x] Non-associative precedence enforced
   - [x] Distinct from value equality `==`, `/=`

2. **Language Simplification**:
   - [x] Category theory operators removed from grammar
   - [x] Parser complexity reduced
   - [x] Minimal core language achieved

### ✅ Quality Requirements

1. **Testing**:
   - [x] Lexer verification complete
   - [x] Parser verification complete
   - [x] No regressions in existing tests (41/41 passing)
   - [x] Operator test suite (4/4 passing)

2. **Parser Quality**:
   - [x] Parser compiles successfully
   - [x] No new conflicts introduced
   - [x] No reduce/reduce conflicts

3. **Code Quality**:
   - [x] Follows project style guidelines
   - [x] Lexer rules properly ordered
   - [x] Parser rules consistent with existing code

---

## Design Rationale

### Why Remove Category Theory Operators?

**Original Decision** (Task 1.1.7 initial implementation):
- Add 8 operators for category theory (`<$>`, `<*>`, `<>`, `>>=`, `>>`, `=<<`, `>=>`, `<=<`)
- Goal: Haskell-style symbolic operators for experts

**Revised Decision** (After ultrathink analysis):
- Keep only type-level equality operators (`===`, `!==`)
- Move category theory operators to standard library

**Rationale**:

1. **Language Complexity**: Every built-in operator increases parser complexity and maintenance burden
2. **PoC Goals**: Phase 1 focuses on core language infrastructure, not library features
3. **Catena Philosophy**: "Shapes" and "Flows" terminology emphasizes accessibility - `>=>` is cryptic
4. **BEAM Precedent**: Erlang has zero category theory operators and thrives
5. **Flexibility**: Library functions can be changed; language operators are permanent
6. **Composition**: Existing `|>` pipe operator handles composition well

### Why Keep `===` and `!==`?

1. **Fundamental Operation**: Type-level equality is core to type systems
2. **Distinct Semantics**: Different from value equality (`==`) - structural vs. definitional
3. **Non-Associative**: Prevents confusing chains, enforces explicit comparisons
4. **Type System Integration**: Used in trait resolution and type inference

---

## Future Work

### When Implementing Standard Library (Phase 6-7)

**Category Theory Traits**:
```catena
trait Functor f where
  fmap : (a -> b) -> f a -> f b
end

trait Applicative f extends Functor f where
  pure : a -> f a
  ap : f (a -> b) -> f a -> f b
end

trait Semigroup a where
  append : a -> a -> a
end

trait Monad m extends Applicative m where
  bind : m a -> (a -> m b) -> m b
end
```

**Usage with Pipe Operator**:
```catena
-- Functor: map function over container
Just 5 |> fmap succ |> fmap (* 2)

-- Monad: sequential composition
getUserInput
  |> bind validateInput
  |> bind processData
  |> bind saveToDatabase
```

**Optional: Syntactic Sugar** (later phase):
```catena
-- Could desugar operators to function calls
f <$> x  ==>  fmap f x
m >>= f  ==>  bind m f
x <> y   ==>  append x y
```

---

## Conclusion

Task 1.1.7 refactored to **minimal, principled design**. Type-level equality operators (`===`, `!==`) provide fundamental type system functionality while keeping core language simple and beginner-friendly.

Category theory abstractions remain central to Catena design but are implemented as library features rather than built-in operators. This aligns with:
- PoC phase goals (core infrastructure)
- Catena philosophy (accessible functional programming)
- BEAM ecosystem conventions (minimal syntax, powerful libraries)

The foundation is laid for comprehensive standard library implementation in future phases.
