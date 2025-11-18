# Catena Standard Library Overview

**Note**: This document describes the planned standard library for Catena. These features will be implemented in later phases (Phase 6-7) after the core language infrastructure is complete.

---

## Introduction

The Catena standard library provides category theory abstractions as library functions and traits. While the core language includes only minimal operators (type-level equality `===` and `!==`), the standard library offers rich compositional abstractions through traits and their implementations.

**Design Philosophy**:
- **Library-based**: Operators defined as library functions, not built into grammar
- **Pipe-friendly**: All functions work naturally with `|>` pipe operator
- **Lawful**: All traits come with mathematical laws verified through property testing
- **Beginner-accessible**: Keyword equivalents for all symbolic operators

---

## Core Abstractions

### Setoid (Type-Level Equality)

**Purpose**: Structural equality distinct from value equality

```catena
trait Setoid a where
  equals : a -> a -> Bool
  not_equals : a -> a -> Bool
  not_equals x y = not (equals x y)

-- Operators (built into language)
operator (===) = equals
operator (!==) = not_equals
```

**Usage**:
```catena
-- With operators
x === y
x !== y

-- With functions
equals x y
not_equals x y

-- With pipe
x |> equals y
```

**Laws**:
- Reflexivity: `x === x`
- Symmetry: `x === y` implies `y === x`
- Transitivity: `x === y` and `y === z` implies `x === z`

### Ord (Ordering)

**Purpose**: Total ordering on types

```catena
trait Ord a extends Setoid a where
  compare : a -> a -> Ordering

  -- Derived operations
  less_than : a -> a -> Bool
  less_than x y = compare x y == LT

  less_than_or_equal : a -> a -> Bool
  greater_than : a -> a -> Bool
  greater_than_or_equal : a -> a -> Bool

shape Ordering = LT | EQ | GT
```

**Usage**:
```catena
compare x y
less_than x y
x |> less_than y
```

**Note**: Comparison operators (`<`, `>`, `<=`, `>=`) are built into the language for convenience.

### Semigroup (Associative Append)

**Purpose**: Associative binary operation

```catena
trait Semigroup a where
  append : a -> a -> Bool

-- Library function (not built-in operator)
flow (<>) : Semigroup a => a -> a -> a
flow (<>) = append
```

**Usage**:
```catena
-- With function
append "hello" " world"
append [1, 2] [3, 4]

-- With pipe
"hello" |> append " world"
[1, 2] |> append [3, 4]

-- With library operator (if imported)
"hello" <> " world"
```

**Laws**:
- Associativity: `(x <> y) <> z === x <> (y <> z)`

### Monoid (Identity Element)

**Purpose**: Semigroup with identity element

```catena
trait Monoid a extends Semigroup a where
  empty : a

-- Common instances
instance Monoid Text where
  empty = ""
  append = Text.concat

instance Monoid (List a) where
  empty = []
  append = List.concat
```

**Laws**:
- Left identity: `empty <> x === x`
- Right identity: `x <> empty === x`
- Associativity: inherited from Semigroup

---

## Functor Hierarchy

### Functor (Mappable Containers)

**Purpose**: Map functions over structures while preserving shape

```catena
trait Functor (f : Type -> Type) where
  fmap : (a -> b) -> f a -> f b

-- Library operators (not built-in)
flow (<$>) : Functor f => (a -> b) -> f a -> f b
flow (<$>) = fmap

flow (<$) : Functor f => a -> f b -> f a
flow (<$) x = fmap (const x)

flow ($>) : Functor f => f a -> b -> f b
flow ($>) fa x = fmap (const x) fa
```

**Usage**:
```catena
-- With function name
fmap succ (Some 5)          -- Some 6
fmap (* 2) [1, 2, 3]        -- [2, 4, 6]

-- With pipe operator
Some 5 |> fmap succ         -- Some 6
[1, 2, 3] |> fmap (* 2)     -- [2, 4, 6]

-- With library operator (if imported)
succ <$> Some 5             -- Some 6
(* 2) <$> [1, 2, 3]         -- [2, 4, 6]

-- Replace contents
5 <$ Some 10                -- Some 5
```

**Laws**:
- Identity: `fmap identity === identity`
- Composition: `fmap (h . g) === fmap h . fmap g`

**Common Instances**:
```catena
instance Functor Option where
  fmap f None = None
  fmap f (Some x) = Some (f x)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Functor (Result e) where
  fmap f (Ok x) = Ok (f x)
  fmap f (Error e) = Error e
```

### Applicative (Function Application in Context)

**Purpose**: Apply wrapped functions to wrapped values

```catena
trait Applicative (f : Type -> Type) extends Functor f where
  pure : a -> f a
  apply : f (a -> b) -> f a -> f b

-- Library operators
flow (<*>) : Applicative f => f (a -> b) -> f a -> f b
flow (<*>) = apply

flow (<*) : Applicative f => f a -> f b -> f a
flow (<*) fa fb = const <$> fa <*> fb

flow (*>) : Applicative f => f a -> f b -> f b
flow (*>) fa fb = flip const <$> fa <*> fb
```

**Usage**:
```catena
-- With functions
pure 5                           -- Some 5, [5], etc.
apply (Some succ) (Some 5)       -- Some 6

-- With pipe
Some 5 |> apply (Some succ)      -- Some 6

-- With library operators
pure (+) <*> Some 3 <*> Some 5   -- Some 8
pure (\x y z -> x + y + z) <*> Some 1 <*> Some 2 <*> Some 3  -- Some 6

-- Sequence, keeping left/right
Some 1 <* Some 2                 -- Some 1
Some 1 *> Some 2                 -- Some 2
```

**Laws**:
- Identity: `pure identity <*> v === v`
- Composition: `pure compose <*> u <*> v <*> w === u <*> (v <*> w)`
- Homomorphism: `pure f <*> pure x === pure (f x)`
- Interchange: `u <*> pure y === pure (\f -> f y) <*> u`

**Common Instances**:
```catena
instance Applicative Option where
  pure x = Some x
  apply None _ = None
  apply _ None = None
  apply (Some f) (Some x) = Some (f x)

instance Applicative List where
  pure x = [x]
  apply fs xs = [f x | f <- fs, x <- xs]
```

### Monad (Sequential Composition)

**Purpose**: Chain computations that produce wrapped values

```catena
trait Monad (m : Type -> Type) extends Applicative m where
  bind : m a -> (a -> m b) -> m b

  -- Default implementation of Applicative.apply
  apply mf mx = bind mf (\f -> bind mx (\x -> pure (f x)))

-- Library operators
flow (>>=) : Monad m => m a -> (a -> m b) -> m b
flow (>>=) = bind

flow (>>) : Monad m => m a -> m b -> m b
flow (>>) ma mb = ma >>= (\_ -> mb)

flow (=<<) : Monad m => (a -> m b) -> m a -> m b
flow (=<<) = flip bind

-- Kleisli composition
flow (>=>) : Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
flow (>=>) f g = \x -> f x >>= g

flow (<=<) : Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
flow (<=<) = flip (>=>)
```

**Usage**:
```catena
-- With bind function
bind getUserInput validateInput
bind (bind getUserInput validateInput) saveToDatabase

-- With pipe operator (recommended)
getUserInput
  |> bind validateInput
  |> bind processData
  |> bind saveToDatabase

-- With library operators
getUserInput >>= validateInput >>= processData >>= saveToDatabase

-- Sequence (ignore left result)
putStrLn "Hello" >> putStrLn "World"

-- Flipped bind (function first)
validateInput =<< getUserInput

-- Kleisli composition
flow processAndSave : Input -> IO ()
flow processAndSave = validateInput >=> processData >=> saveToDatabase
```

**Laws**:
- Left identity: `pure x >>= f === f x`
- Right identity: `m >>= pure === m`
- Associativity: `(m >>= f) >>= g === m >>= (\x -> f x >>= g)`

**Common Instances**:
```catena
instance Monad Option where
  bind None _ = None
  bind (Some x) f = f x

instance Monad List where
  bind xs f = concat (fmap f xs)

instance Monad (Result e) where
  bind (Error e) _ = Error e
  bind (Ok x) f = f x
```

---

## Built-in Functions

### Core Functions

```catena
-- Identity morphism (fundamental to all categories)
flow identity : a -> a
flow identity x = x

-- Constant function
flow const : a -> b -> a
flow const x _ = x

-- Function composition
flow compose : (b -> c) -> (a -> b) -> (a -> c)
flow compose g f = \x -> g (f x)

flow (.) : (b -> c) -> (a -> b) -> (a -> c)
flow (.) = compose

-- Flip argument order
flow flip : (a -> b -> c) -> (b -> a -> c)
flow flip f = \y x -> f x y
```

### Applicative/Monad Utilities

```catena
-- Lift pure value into context
flow return : Monad m => a -> m a
flow return = pure

-- Monadic map (same as fmap for Monad)
flow liftM : Monad m => (a -> b) -> m a -> m b
flow liftM = fmap

-- Lift binary function
flow liftM2 : Monad m => (a -> b -> c) -> m a -> m b -> m c
flow liftM2 f ma mb = do
  a <- ma
  b <- mb
  return (f a b)

-- Sequence list of monadic actions
flow sequence : Monad m => List (m a) -> m (List a)
flow sequence = match
  | [] -> pure []
  | (m:ms) -> do
    x <- m
    xs <- sequence ms
    return (x:xs)
  end

-- Map with monadic function and collect results
flow mapM : Monad m => (a -> m b) -> List a -> m (List b)
flow mapM f xs = sequence (fmap f xs)

-- Map with monadic function, discard results
flow mapM_ : Monad m => (a -> m b) -> List a -> m ()
flow mapM_ f xs = sequence_ (fmap f xs)
  where
    sequence_ ms = foldl (>>) (pure ()) ms

-- Filter with monadic predicate
flow filterM : Monad m => (a -> m Bool) -> List a -> m (List a)
flow filterM p = match
  | [] -> pure []
  | (x:xs) -> do
    keep <- p x
    rest <- filterM p xs
    return (if keep then x:rest else rest)
  end
```

---

## Operator Reference Tables

### Equality & Ordering

| Description | Keyword | Operator | Location |
|-------------|---------|----------|----------|
| Type-level equality | `equals` | `===` | **Language** |
| Type-level inequality | `not_equals` | `!==` | **Language** |
| Less than | `less_than` | `<` | Language |
| Less than or equal | `less_than_or_equal` | `<=` | Language |
| Greater than | `greater_than` | `>` | Language |
| Greater than or equal | `greater_than_or_equal` | `>=` | Language |

### Algebraic Structures

| Description | Keyword | Library Operator | Precedence |
|-------------|---------|------------------|------------|
| Associative append | `append` | `<>` | right 6 |

### Functor, Applicative, Monad

| Description | Keyword | Library Operator | Precedence |
|-------------|---------|------------------|------------|
| Functor map | `fmap` or `map` | `<$>` | left 4 |
| Replace left | N/A | `<$` | left 4 |
| Replace right | N/A | `$>` | left 4 |
| Applicative apply | `apply` | `<*>` | left 4 |
| Sequence, keep left | N/A | `<*` | left 4 |
| Sequence, keep right | N/A | `*>` | left 4 |
| Monadic bind | `bind` | `>>=` | left 1 |
| Sequence (ignore left) | N/A | `>>` | left 1 |
| Flipped bind | `flip bind` | `=<<` | right 1 |

### Composition Operators

| Description | Keyword | Library Operator | Precedence |
|-------------|---------|------------------|------------|
| Kleisli composition (L→R) | N/A | `>=>` | right 1 |
| Kleisli composition (R→L) | N/A | `<=<` | right 1 |
| Function composition (L→R) | `compose` (flipped) | `>>>` | right 9 |
| Function composition (R→L) | `compose` | `<<<` | right 9 |
| Function composition (dot) | `compose` | `.` | right 9 |

### Arrow Operators (Advanced)

| Description | Keyword | Library Operator |
|-------------|---------|------------------|
| Parallel composition on pairs | `parallel` | `***` |
| Fanout (duplicate input) | `fanout` | `&&&` |

### Comonad Operators (Advanced)

| Description | Keyword | Library Operator |
|-------------|---------|------------------|
| Extend (L→R) | `extend` (flipped) | `=>>` |
| Extend (R→L) | `extend` | `<<=` |

---

## Usage Patterns

### Pattern 1: Pipe-First (Recommended)

```catena
-- Computation flows left to right
getUserInput
  |> fmap validate
  |> bind processData
  |> fmap formatOutput
  |> bind saveToDatabase

-- List operations
[1, 2, 3, 4, 5]
  |> fmap (* 2)
  |> filter (> 5)
  |> foldl (+) 0
```

### Pattern 2: Function Composition

```catena
-- Build reusable pipelines
flow processUser : UserInput -> IO ()
flow processUser =
  validate
    >=> processData
    >=> saveToDatabase

-- Use it
getUserInput |> bind processUser
```

### Pattern 3: Library Operators (Optional)

```catena
-- For Haskell familiarity (if operators imported)
(* 2) <$> Some 5                    -- Some 10
pure (+) <*> Some 3 <*> Some 5      -- Some 8
getUserInput >>= validate >>= save  -- Monadic chain
```

---

## Example: Working with Option

```catena
-- Basic Functor usage
Some 5 |> fmap (* 2)          -- Some 10
None |> fmap (* 2)            -- None

-- Applicative for multiple arguments
flow add3 : Natural -> Natural -> Natural -> Natural
flow add3 x y z = x + y + z

pure add3 <*> Some 1 <*> Some 2 <*> Some 3  -- Some 6
pure add3 <*> Some 1 <*> None <*> Some 3    -- None

-- Monadic chaining
flow safeDivide : Natural -> Natural -> Option Natural
flow safeDivide _ 0 = None
flow safeDivide x y = Some (x / y)

flow compute : Natural -> Natural -> Natural -> Option Natural
flow compute x y z =
  safeDivide x y
    |> bind (\result1 -> safeDivide result1 z)

-- Or with pipe all the way
flow compute : Natural -> Natural -> Natural -> Option Natural
flow compute x y z =
  safeDivide x y
    |> bind (flip safeDivide z)
```

---

## Example: Working with Lists

```catena
-- Functor: transform each element
[1, 2, 3] |> fmap (* 2)                    -- [2, 4, 6]

-- Applicative: cartesian product of functions and values
[(+1), (*2)] <*> [1, 2, 3]                 -- [2, 3, 4, 2, 4, 6]

-- Monad: flatMap
[[1, 2], [3, 4]] |> bind identity           -- [1, 2, 3, 4]

[1, 2, 3] |> bind (\x -> [x, x * 10])      -- [1, 10, 2, 20, 3, 30]

-- List comprehension equivalent
flow pairs : List Natural -> List (Natural, Natural)
flow pairs xs = do
  x <- xs
  y <- xs
  return (x, y)

pairs [1, 2]  -- [(1,1), (1,2), (2,1), (2,2)]
```

---

## Example: Error Handling with Result

```catena
shape Result e a = Ok a | Error e

instance Functor (Result e) where
  fmap f (Ok x) = Ok (f x)
  fmap f (Error e) = Error e

instance Monad (Result e) where
  pure = Ok
  bind (Ok x) f = f x
  bind (Error e) _ = Error e

-- Usage
flow processRequest : Request -> Result Error Response
flow processRequest request =
  parseRequest request
    |> bind validateRequest
    |> bind authenticateUser
    |> bind authorizeAction
    |> bind executeAction
    |> fmap formatResponse

-- Early return on first error
parseRequest request    -- Result Error ParsedRequest
  |> bind validateRequest  -- Result Error ValidRequest
  |> bind authenticateUser -- Result Error AuthenticatedRequest
  -- If any step returns Error, chain stops
```

---

## Natural Transformations

Natural transformations convert between functors while preserving structure:

```catena
-- List to Option: take first element
natural ListToOption : List ~> Option where
  transform : forall a. List a -> Option a
  transform [] = None
  transform (x:_) = Some x

  -- Naturality law (verified by property testing)
  law naturality:
    forall f : (a -> b), xs : List a ->
      transform (fmap f xs) === fmap f (transform xs)

-- Option to List: wrap in singleton or empty
natural OptionToList : Option ~> List where
  transform : forall a. Option a -> List a
  transform None = []
  transform (Some x) = [x]

-- Usage
[1, 2, 3] |> transform @ListToOption        -- Some 1
[] |> transform @ListToMaybe               -- None

Some 5 |> transform @OptionToList           -- [5]
None |> transform @MaybeToList             -- []
```

---

## Implementation Notes

### For Standard Library Implementers

1. **Precedence Levels**: Follow Haskell conventions for operator compatibility
2. **Efficiency**: Specialize common cases (e.g., List.fmap uses tail recursion)
3. **Law Checking**: All instances must pass property-based law verification
4. **Documentation**: Every function must have examples and laws documented
5. **Testing**: Property tests verify laws, unit tests verify edge cases

### Library Organization

```
std/
  category/
    functor.catena      -- Functor trait and instances
    applicative.catena  -- Applicative trait and instances
    monad.catena        -- Monad trait and instances
  data/
    list.catena         -- List type and instances
    option.catena       -- Option type and instances
    result.catena       -- Result type and instances
  prelude.catena        -- Auto-imported basics
```

---

## Migration from Built-in Operators

For code that used built-in operators (pre-refactoring):

```catena
-- Old (when operators were built-in)
result <$> computation
m >>= f >>= g
"hello" <> " world"

-- New (library-based, pipe-first style)
computation |> fmap result
m |> bind f |> bind g
"hello" |> append " world"

-- Alternative (import library operators)
import Std.Operators ((<$>), (>>=), (<>))

result <$> computation      -- Works if imported
m >>= f >>= g               -- Works if imported
"hello" <> " world"         -- Works if imported
```

---

## Conclusion

The Catena standard library provides powerful category theory abstractions while maintaining beginner accessibility through:

1. **Pipe-first design**: All functions work naturally with `|>` pipe
2. **Keyword equivalents**: Every operator has a named function
3. **Library-based**: Not baked into language grammar
4. **Lawful**: Mathematical properties verified through testing
5. **Gradual learning**: Start with functions, adopt operators when comfortable

The library will be implemented in Phases 6-7, after core language infrastructure is complete.
