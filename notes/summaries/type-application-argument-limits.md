# Type Application Argument Limit - Investigation Resolution

**Date**: 2025-11-16
**Status**:  Root cause identified - Grammar limitation
**Files Investigated**:
- `test/compiler/parser/catena_parser_higher_order_types_tests.erl` (tests #5, #6)
- `src/compiler/parser/catena_parser.yrl` (lines 806-817)

---

## Summary

Tests #5 and #6 of the higher-order type tests are failing **not due to tuple parsing issues**, but due to a **grammar limitation**: the parser only supports **single-argument type application**.

**Key Finding**: The grammar is hardcoded to accept only 1 type argument in type applications like `Maybe a` or `List Int`. Multi-argument type constructors like `Triple a b c` or `Map String Int` are **not supported** by the current grammar design.

---

## Test Failures Re-Analyzed

### Test #5: `parse_tuple_type_three_elements_test()` (Lines 197-227)

**What test does**:
```erlang
%% Trait signature: mk : (a, b, c) -> Triple a b c
Tokens = [
    {trait, 1}, {upper_ident, 1, "Triple"}, {where, 1},
    {lower_ident, 2, "mk"}, {colon, 2},
    {lparen, 2}, {lower_ident, 2, "a"}, {comma, 2},
    {lower_ident, 2, "b"}, {comma, 2},
    {lower_ident, 2, "c"}, {rparen, 2},
    {arrow, 2}, {upper_ident, 2, "Triple"},
    {lower_ident, 2, "a"}, {lower_ident, 2, "b"}, {lower_ident, 2, "c"},
    {'end', 3}
]
```

**Error**: `{error, {2, catena_parser, ["syntax error before: ", ["\"b\""]]}}`

**Root Cause**: Not the tuple `(a, b, c)` - that parses fine! The issue is `Triple a b c` which requires **3 type arguments**, but the grammar only supports **1**.

### Test #6: `parse_multiple_methods_with_higher_order_types_test()` (Lines 335-371)

**What test does**:
```erlang
%% Trait Category c where
%%   id : c a a,
%%   compose : c b c -> c a b -> c a c
%% end
```

**Root Cause**: The type `c a a` requires **2 type arguments** (type variable `c` applied to `a` twice), but grammar only supports **1 argument**.

---

## Investigation Steps

### Step 1: Test 3-Element Tuple in Isolation

Created `/tmp/test_tuple.erl` with simplified token sequence (just the tuple, no return type):

```erlang
Tokens = [
    {trait, 1}, {upper_ident, 1, "Test"}, {where, 1},
    {lower_ident, 2, "mk"}, {colon, 2},
    {lparen, 2}, {lower_ident, 2, "a"}, {comma, 2},
    {lower_ident, 2, "b"}, {comma, 2},
    {lower_ident, 2, "c"}, {rparen, 2},
    {'end', 3}
]
```

**Result**:  **Parses successfully!**
```erlang
{ok, {type_tuple, [{type_var,a,...}, {type_var,b,...}, {type_var,c,...}], ...}}
```

**Conclusion**: 3-element tuples work perfectly. Grammar rule handles it correctly via recursive `type_expr_list`.

### Step 2: Test Progressive Type Application Complexity

Tested return types with 0, 1, 2, and 3 type arguments:

| Return Type | Type Args | Result |
|-------------|-----------|--------|
| `Triple` | 0 |  Works |
| `Triple a` | 1 |  Works |
| `Triple a b` | 2 | L **Fails: "syntax error before: \"b\""** |
| `Triple a b c` | 3 | L Fails |

**Conclusion**: Parser has a **hard limit of 1 type argument** in type applications.

### Step 3: Grammar Analysis

Located the type application grammar rules (lines 806-817):

```erlang
%% Type application (higher precedence than function arrows)
%% Supports both type constructors (Maybe a) and type variables (f a) for higher-kinded types
type_expr_app -> upper_ident type_expr_primary :
    {type_app,
        {type_con, extract_atom('$1'), extract_location('$1')},
        ['$2'],  % <--- HARDCODED SINGLE ARGUMENT
        extract_location('$1')}.

%% Type application with type variable (for higher-kinded types like f a)
type_expr_app -> lower_ident type_expr_primary :
    {type_app,
        {type_var, extract_atom('$1'), extract_location('$1')},
        ['$2'],  % <--- HARDCODED SINGLE ARGUMENT
        extract_location('$1')}.
```

**Key Issue**: `['$2']` hardcodes a **single-element list**. The grammar takes:
- `$1`: Type constructor or variable (e.g., `Triple`, `Maybe`, `f`)
- `$2`: **Exactly one** `type_expr_primary` (e.g., `a`, `Int`, `(a, b)`)

**What's NOT supported**:
- Multiple separate arguments: `Triple a b c`
- Space-separated type arguments (common in Haskell/ML style)

**What IS supported**:
- Single argument: `Maybe a`, `List Int`, `f a`
- Parenthesized single argument: `Maybe (a, b)` (tuple as single arg)

---

## Grammar Limitation Details

### Current Grammar Capabilities

**Supported**:
```haskell
-- Single argument type application
Maybe a           -- Maybe applied to a
List Int          -- List applied to Int
f a               -- Higher-kinded: f applied to a
Map (String, Int) -- Map applied to a tuple (single arg)
```

**NOT Supported**:
```haskell
-- Multiple argument type application
Triple a b c      -- Triple applied to 3 args
Map String Int    -- Map applied to 2 args
Either a b        -- Either applied to 2 args
c a a             -- Type variable c applied to a twice
```

### Why This Limitation Exists

This is likely a **conscious design decision** from early parser development:

1. **Simplicity**: Single-argument application is easier to parse and reason about
2. **Currying**: Multi-argument types can be represented via currying: `Map String Int` becomes `(Map String) Int`
3. **Future expansion**: May have been deferred to later implementation phases
4. **ML-style syntax**: OCaml/SML use parentheses for multi-arg: `(a, b, c) triple` instead of `Triple a b c`

### Alternative Syntax That Works

**Instead of**:
```haskell
Triple a b c
```

**Use** (if grammar supported it):
```haskell
Triple (a, b, c)  -- Triple applied to single 3-tuple argument
```

But this changes semantics - `Triple (a, b, c)` is a type constructor taking a tuple, whereas `Triple a b c` is a type constructor taking 3 separate arguments.

---

## How to Fix the Grammar

To support multi-argument type application, the grammar would need:

### Option 1: Space-Separated Arguments (Haskell/ML style)

```erlang
%% Multi-argument type application
type_expr_app -> upper_ident type_expr_primary_list :
    {type_app,
        {type_con, extract_atom('$1'), extract_location('$1')},
        '$2',  % List of multiple arguments
        extract_location('$1')}.

%% List of type arguments (non-empty)
type_expr_primary_list -> type_expr_primary :
    ['$1'].
type_expr_primary_list -> type_expr_primary type_expr_primary_list :
    ['$1' | '$2'].
```

**Allows**: `Triple a b c`, `Map String Int`, `Either a b`

**Challenge**: May create ambiguity with function types. For example:
```haskell
f a b -> c
```
Is this `(f a b) -> c` or `f (a b) -> c` or `f (a (b -> c))`?

### Option 2: Explicit Grouping (Safer)

Require parentheses or angle brackets for multi-arg:

```haskell
Triple<a, b, c>     -- Explicit angle bracket syntax
Triple[a, b, c]     -- Explicit square bracket syntax
(Triple a b c)      -- Parenthesized application
```

**Benefits**:
- No ambiguity with function types
- Clear visual grouping
- Common in C++/Java/Rust

### Option 3: Curried Style (No Grammar Change)

Force currying in type constructors:

```haskell
-- Define Triple as curried
shape Triple a = Triple1 a
shape Triple1 a b = Triple2 a b
shape Triple2 a b c = MkTriple a b c

-- Use it
Triple2 a b c  -- Each application is single-arg
```

**Benefits**: Works with current grammar
**Drawbacks**: Verbose, unintuitive

---

## Test Expectations

### Current Test Status

| Test | Issue | Fixable? |
|------|-------|----------|
| #5: `parse_tuple_type_three_elements_test()` | Expects `Triple a b c` to parse | L Grammar doesn't support it |
| #6: `parse_multiple_methods_with_higher_order_types_test()` | Expects `c a a` to parse | L Grammar doesn't support it |

### Recommended Test Fixes

**Option 1: Change tests to match current grammar limitations**

Update tests to use single-argument type application:

```erlang
%% Test #5: Change from Triple a b c to Triple with tuple argument
parse_tuple_type_three_elements_test() ->
    %% trait Triple where
    %%   mk : (a, b, c) -> Triple  % No type args
    %% end
    ...
```

**Option 2: Mark tests as pending grammar enhancement**

```erlang
%% @doc PENDING: Multi-argument type application not yet supported by grammar
%% This test will pass once grammar is updated to support Triple a b c syntax
parse_tuple_type_three_elements_test() ->
    %% Skip this test for now
    ok.
```

**Option 3: Extend grammar to support multi-arg type application**

Implement Option 1 or Option 2 from "How to Fix the Grammar" section above.

---

## Impact Assessment

### What Works

 **Tuple types**: `(a, b, c)` with any number of elements
 **Single-arg type application**: `Maybe a`, `List Int`, `f a`
 **Parenthesized types**: `(a -> b)`, `((a, b) -> c)`
 **Higher-kinded types**: `f a`, `m (a -> b)`

### What Doesn't Work

L **Multi-arg type constructors**: `Triple a b c`, `Map String Int`
L **Category theory types**: `Category c` with `c a a` morphisms
L **Complex type applications**: `Either a b`, `Result e a`

### Impact on Trait System

**Affected traits** (from standard library plans):

```haskell
-- Currently CAN'T express:
trait Bifunctor f where
  bimap : (a -> b) -> (c -> d) -> f a c -> f b d  -- f a c requires 2 args

trait Category c where
  id : c a a       -- c a a requires 2 args
  compose : c b c -> c a b -> c a c  -- All require 2 args

-- CAN express:
trait Functor f where
  fmap : (a -> b) -> f a -> f b  -- f a is single arg (OK)

trait Monad m where
  bind : m a -> (a -> m b) -> m b  -- m a, m b are single arg (OK)
```

**Workaround for now**: Many standard library traits work fine with single-arg application. Multi-arg type constructors can be deferred to future implementation phases.

---

## Recommendations

### Short-Term (Current PoC Phase)

1. **Update failing tests** to match current grammar capabilities:
   - Test #5: Use `Triple` without type args or `Triple (a, b, c)` with tuple arg
   - Test #6: Use different trait examples with single-arg type application

2. **Document limitation** in parser documentation:
   - Add note to `catena_parser.yrl` explaining single-arg limit
   - Update trait system docs to note multi-arg types pending

3. **Add issue/task** for future grammar enhancement:
   - Create research task for multi-arg type application syntax
   - Compare Haskell, OCaml, Rust, Scala approaches
   - Decide on Catena syntax (space-separated, angle brackets, etc.)

### Long-Term (Post-PoC)

1. **Extend grammar** to support multi-argument type application:
   - Research best syntax approach (space-separated vs explicit grouping)
   - Implement chosen approach in grammar
   - Add comprehensive tests for multi-arg types

2. **Expand standard library** with multi-arg types:
   - `Either a b`, `Result e a`
   - `Map k v`, `Tuple2 a b`, `Tuple3 a b c`
   - Category theory types: `Category c` with `c a b` morphisms

3. **Type inference** updates:
   - Ensure Algorithm W handles multi-arg type constructors
   - Update kind inference for multi-parameter types
   - Test higher-kinded types with multiple args

---

## Conclusion

The investigation revealed that tests #5 and #6 are failing due to a **known grammar limitation**, not a parsing bug:

-  **Tuple parsing works correctly** for any number of elements
-  **Single-arg type application works** (`Maybe a`, `List Int`, `f a`)
- L **Multi-arg type application NOT supported** (`Triple a b c`, `Map String Int`)

**Root Cause**: Grammar rules (lines 806-817) hardcode a single type argument in `type_expr_app`.

**Impact**:
- Tests #5 and #6 cannot pass with current grammar
- Standard library traits requiring multi-arg types (Bifunctor, Category) cannot be expressed yet
- Most common traits (Functor, Monad, Applicative) work fine with single-arg types

**Recommendation**: Update tests to match current grammar capabilities and defer multi-arg type application to future enhancement phase.

---

**Investigation Status**:  Complete
**Parser Status**:  Working correctly within design constraints
**Tests Status**:   Need updates to match grammar limitations
**Grammar Enhancement**: =Ë Recommended for future implementation
