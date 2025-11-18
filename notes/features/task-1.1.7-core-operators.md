# Feature Plan: Task 1.1.7 - Core Operators

**Status**: 🔵 In Progress
**Branch**: `feature/task-1.1.7-core-operators`
**Started**: 2025-11-15

---

## Problem Statement

Catena needs dual notation for category theory operators - readable keywords for beginners and concise symbolic operators for experts. Currently missing:
- Setoid operators for type-level equality (`===`, `!==`)
- Category theory composition operators (`<>`, `<$>`, `<*>`)
- Monadic sequencing operators (`>>=`, `>>`, `=<<`, `>=>`, `<=<`)

**Success Criteria**:
- All operators parse with correct precedence and associativity
- Full test coverage (100% operator coverage)
- Parser conflicts remain stable (≤17 shift/reduce)
- No breaking changes to existing functionality

---

## Implementation Checklist

### ✅ Planning Phase
- [x] Create feature branch
- [x] Generate comprehensive implementation plan
- [x] Document operator precedence rules

### 🔵 Implementation Phase

#### Step 1: Setoid Equality Operators
- [ ] Add `===` and `!==` to lexer
- [ ] Add terminals to parser
- [ ] Define precedence (infix 4, non-associative)
- [ ] Add expression grammar rules
- [ ] Test basic functionality

#### Step 2: Category Theory Operators
- [ ] Add `<$>` (Functor map) to lexer
- [ ] Add `<*>` (Applicative apply) to lexer
- [ ] Verify `<>` (Semigroup append) exists
- [ ] Add terminals to parser
- [ ] Define precedence (infixl 4 for <$>, <*>; infixl 6 for <>)
- [ ] Add expression grammar rules
- [ ] Test functionality

#### Step 3: Monadic Operators
- [ ] Verify `>>=` exists, add `>>` to lexer
- [ ] Add `=<<`, `>=>`, `<=<` to lexer
- [ ] Add terminals to parser
- [ ] Define precedence (infixl 1 for bind/seq, infixr 1 for Kleisli)
- [ ] Add expression grammar rules
- [ ] Test functionality

#### Step 4: Comprehensive Testing
- [ ] Create operator test suite (28+ tests)
- [ ] Test precedence rules (6 tests)
- [ ] Test associativity (left, right, non-assoc)
- [ ] Integration tests with trait system
- [ ] Regression tests (existing suites)

#### Step 5: Documentation
- [ ] Create implementation summary
- [ ] Document precedence table
- [ ] Add operator examples
- [ ] Update task checklist in phase-01.md

---

## Operator Reference

### Setoid Equality
| Operator | Token | Precedence | Assoc | Description |
|----------|-------|------------|-------|-------------|
| `===` | `setoid_eq` | 300 | nonassoc | Type-level equality |
| `!==` | `setoid_neq` | 300 | nonassoc | Type-level inequality |

### Category Theory
| Operator | Token | Precedence | Assoc | Description |
|----------|-------|------------|-------|-------------|
| `<>` | `concat` | 350 | right | Semigroup append |
| `<$>` | `fmap` | 370 | left | Functor map |
| `<*>` | `ap` | 370 | left | Applicative apply |

### Monadic
| Operator | Token | Precedence | Assoc | Description |
|----------|-------|------------|-------|-------------|
| `>>=` | `bind` | 150 | left | Monadic bind |
| `>>` | `then_op` | 151 | left | Sequence/then |
| `=<<` | `bind_flip` | 150 | left | Flipped bind |
| `>=>` | `kleisli_lr` | 152 | right | Kleisli left-to-right |
| `<=<` | `kleisli_rl` | 153 | right | Kleisli right-to-left |

---

## Current Status

**Last Updated**: 2025-11-15 18:15

**What Works**:
- ✅ All 8 operators added to lexer
- ✅ All operators added to parser terminals
- ✅ Precedence and associativity defined
- ✅ Expression grammar rules implemented
- ✅ Lexer compiles and tokenizes correctly
- ✅ Parser compiles without new conflicts
- ✅ No regressions (41/41 trait tests passing)
- ✅ Manual verification complete

**What's Next**:
- Create comprehensive operator test suite
- Integration tests with trait system
- Final documentation and commit

**How to Test**:
```bash
# Build lexer and parser
./scripts/build_parser.sh

# Run tests
erlc -I src/compiler/parser -pa src/compiler -pa src/compiler/parser -o test/compiler/parser test/compiler/parser/catena_parser_operator_tests.erl
erl -noshell -pa src/compiler -pa src/compiler/parser -pa test/compiler/parser -eval "eunit:test(catena_parser_operator_tests, [verbose])" -s init stop
```

---

## Files Modified

- `src/compiler/lexer/catena_lexer.xrl` - Add operator tokens
- `src/compiler/parser/catena_parser.yrl` - Add terminals, precedence, grammar rules
- `test/compiler/parser/catena_parser_operator_tests.erl` - New test suite

---

## Notes

- Following Haskell precedence conventions for operator compatibility
- Maintaining parser conflicts at ≤17 shift/reduce
- Using DRY principle for test helpers
- All operators encode category theory concepts
