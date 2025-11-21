# Minimal Core Keywords Feature Summary

## Overview

This feature reduces the Catena language from 26+ keywords to a minimal core of ~12 keywords that require compiler support, moving other concepts to the standard library.

## Changes Made

### Lexer Updates (`src/compiler/lexer/catena_lexer.xrl`)

**Removed Keywords:**
- `do` - Desugar from `>>=` (Pipeline bind)
- `if`, `then`, `else` - Desugar to `match` on Bool
- `extends` - Use `:` syntax in trait definition
- `try`, `with` - Replaced by `handle` keyword
- `supervisor` - Library convention

**Added Keywords:**
- `handle` - Effect handling
- `process` - Actor message handlers

**Retained Core Keywords (12):**
- `type`, `transform`, `let`, `match`
- `trait`, `instance`
- `effect`, `perform`, `handle`
- `actor`, `process`, `module`

**Syntax Keywords:**
- `in`, `end`, `where`, `when`, `as`, `forall`, `operation`

### Parser Updates (`src/compiler/parser/catena_parser.yrl`)

1. **Trait Declaration Syntax**
   - Uses `where...end` syntax for consistency
   - Uses `:` instead of `extends` for inheritance
   - Example: `trait Orderable a : Comparable a where ... end`

2. **Instance Declaration Syntax**
   - Uses `where...end` syntax with `transform` for methods
   - Example: `instance Mapper List where transform map f xs = ... end`

3. **Effect Handling**
   - Uses `handle expr { handlers }` instead of `try expr with handlers end`

4. **Removed if/then/else**
   - Library functions handle conditional logic via `match`

### Standard Library Structure

Created `lib/catena/stdlib/` with:

1. **`prelude.catena`** - Core traits and types:
   - Comparable (Setoid)
   - Orderable (Ord)
   - Combiner (Semigroup)
   - Accumulator (Monoid)
   - Mapper (Functor)
   - Applicator (Applicative)
   - Chainable (Chain)
   - Pipeline (Monad)
   - Extractor (Comonad)
   - Foldable, Traversable
   - Core types: Ordering, Maybe, Either, Result
   - Instances for Maybe, Either, List

2. **`test.catena`** - Testing framework:
   - `unit` - Unit tests
   - `property` - Property-based tests
   - `assert`, `assertEqual`, `assertMatch`
   - `verify` - Law verification
   - `suite` - Test organization
   - `benchmark` - Performance testing

3. **`effect/io.catena`** - IO effect:
   - `print`, `println`, `readLine`
   - `readFile`, `writeFile`

4. **`effect/state.catena`** - State effect:
   - `get`, `put`, `modify`

5. **`effect/error.catena`** - Error effect:
   - `raise`, `catch`

### Language Specification Updates

Updated `notes/research/language_overview.md`:
- Documented 12 core keywords with examples
- Listed syntax keywords
- Explained removed keywords and their replacements
- Updated all trait examples to use `where...end` syntax
- Updated all instance examples to use `transform` and `end`

## Benefits

1. **Smaller core language** - Easier to understand and maintain
2. **Library-defined concepts** - Testing, conditionals defined in library
3. **Consistent syntax** - `:` for trait inheritance matches type annotations
4. **Composable** - `handle` expression is more composable than `try/with`

## Test Status

- Lexer tests: Passing
- Parser trait tests: 41/41 passing
- Pre-existing type system test failures unrelated to this feature

## Files Changed

- `src/compiler/lexer/catena_lexer.xrl`
- `src/compiler/parser/catena_parser.yrl`
- `test/catena_lexer_tests.erl`
- `test/compiler/parser/catena_parser_trait_tests.erl`
- `notes/research/language_overview.md`
- `notes/features/minimal-core-keywords.md` (planning)
- `lib/catena/stdlib/prelude.catena` (new)
- `lib/catena/stdlib/test.catena` (new)
- `lib/catena/stdlib/effect/io.catena` (new)
- `lib/catena/stdlib/effect/state.catena` (new)
- `lib/catena/stdlib/effect/error.catena` (new)

## Branch

`feature/minimal-core-keywords`

## Future Work

1. Implement `if/then/else` desugaring to `match`
2. Implement `do` notation desugaring to `>>=`
3. Add more standard library modules (data types, utilities)
4. Complete actor/process syntax support
5. Add attribute support for `@doc`, `@operator`
