# Category Theory Library Plan

## Overview

This planning document outlines the implementation of a comprehensive category theory library for Catena that exposes traditional mathematical terminology alongside the pragmatic names. The library provides `Functor`, `Monad`, `Applicative`, and other category theory abstractions as aliases to the core pragmatic traits (`Mapper`, `Pipeline`, `Applicator`), allowing developers familiar with category theory to use conventional terminology.

**Total Duration**: 7.5 weeks

## Phase Navigation

- **Phase 1**: Core Abstractions (Weeks 1-2)
- **Phase 2**: Applicative and Monad (Weeks 3-4)
- **Phase 3**: Comonad and Arrows (Weeks 5-6.5)
- **Phase 4**: Foldable and Traversable (Weeks 6.5-7)
- **Phase 5**: Operators and Laws (Weeks 7-7.5)

---

# Phase 1: Core Abstractions

**Duration**: 2 weeks (Weeks 1-2)

This phase establishes the foundational algebraic structures and the functor hierarchy. These form the basis upon which all other abstractions are built, providing the essential building blocks of category theory.

## 1.1 Algebraic Structures
- [ ] **Section 1.1 Complete**

Algebraic structures provide the foundation for combining and accumulating values. We implement the classic hierarchy from Setoid through Group, each with clearly documented laws that implementations must satisfy. These structures are fundamental to functional programming and appear throughout the standard library.

### 1.1.1 Setoid (Equality)
- [ ] **Task 1.1.1 Complete**

Setoid defines types with a custom equality relation. Unlike structural equality, Setoid equality can be defined semantically (e.g., two sets are equal if they contain the same elements regardless of order). This is the foundation for all comparison operations.

- [ ] 1.1.1.1 Define `Setoid` trait as alias to `Comparable` with `equals : a -> a -> Bool` (success: `Setoid` can be used interchangeably with `Comparable`)
- [ ] 1.1.1.2 Implement `notEquals` as default method using `not (equals x y)`
- [ ] 1.1.1.3 Define `===` operator mapping to `equals` function
- [ ] 1.1.1.4 Define `!==` operator mapping to `notEquals` function
- [ ] 1.1.1.5 Document Setoid laws: reflexivity (`x === x`), symmetry (`x === y` implies `y === x`), transitivity

### 1.1.2 Semigroup
- [ ] **Task 1.1.2 Complete**

Semigroup defines types with an associative binary operation. This is the minimal structure needed for combining values, appearing in string concatenation, list append, and numeric addition.

- [ ] 1.1.2.1 Define `Semigroup` trait as alias to `Combiner` with `append : a -> a -> a` (success: `Semigroup` can be used interchangeably with `Combiner`)
- [ ] 1.1.2.2 Define `<>` operator mapping to `append` function
- [ ] 1.1.2.3 Implement `sconcat` for non-empty list concatenation using `append`
- [ ] 1.1.2.4 Implement `stimes` for repeated application of `append`
- [ ] 1.1.2.5 Document Semigroup law: associativity (`(x <> y) <> z === x <> (y <> z)`)

### 1.1.3 Monoid
- [ ] **Task 1.1.3 Complete**

Monoid extends Semigroup with an identity element. This enables folding empty structures and provides a "zero" value for accumulation. Monoids are ubiquitous in functional programming.

- [ ] 1.1.3.1 Define `Monoid` trait as alias to `Accumulator` extending `Semigroup` with `mempty : a` (success: `Monoid` can be used interchangeably with `Accumulator`)
- [ ] 1.1.3.2 Implement `mconcat` as fold over list using `mempty` and `append`
- [ ] 1.1.3.3 Document Monoid laws: left identity (`mempty <> x === x`), right identity (`x <> mempty === x`)
- [ ] 1.1.3.4 Provide standard instances for `String`, `List`, `Natural` (under addition), `Natural` (under multiplication)

### 1.1.4 Group
- [ ] **Task 1.1.4 Complete**

Group extends Monoid with inverses, allowing subtraction and negation. While less common than Monoid, Groups appear in numeric types and transformations.

- [ ] 1.1.4.1 Define `Group` trait extending `Monoid` with `invert : a -> a`
- [ ] 1.1.4.2 Implement `minus` as `x <> (invert y)`
- [ ] 1.1.4.3 Document Group laws: left inverse (`(invert x) <> x === mempty`), right inverse (`x <> (invert x) === mempty`)
- [ ] 1.1.4.4 Provide instance for `Integer` (integers under addition)

### Unit Tests - Section 1.1
- [ ] **Unit Tests 1.1 Complete**
- [ ] Test Setoid laws (reflexivity, symmetry, transitivity) for all instances
- [ ] Test Semigroup associativity law with property-based testing
- [ ] Test Monoid identity laws for all instances
- [ ] Test Group inverse laws for Integer instance
- [ ] Test operator definitions (`===`, `!==`, `<>`) produce correct results
- [ ] Test `mconcat` correctly folds empty and non-empty lists

## 1.2 Functor Hierarchy
- [ ] **Section 1.2 Complete**

The functor hierarchy provides structure-preserving transformations. Functors map functions over containers while preserving their structure. This section implements the core functor types: covariant, contravariant, and bifunctors.

### 1.2.1 Functor
- [ ] **Task 1.2.1 Complete**

Functor is the fundamental abstraction for mapping functions over structures. It allows transforming the contents of a container without changing its shape. This is one of the most commonly used abstractions in functional programming.

- [ ] 1.2.1.1 Define `Functor` trait as alias to `Mapper` with `fmap : (a -> b) -> f a -> f b` (success: `Functor` can be used interchangeably with `Mapper`)
- [ ] 1.2.1.2 Define `<$>` operator as infix version of `fmap`
- [ ] 1.2.1.3 Implement `<$` (replace all with constant): `x <$ fa = fmap (const x) fa`
- [ ] 1.2.1.4 Implement `$>` (flipped replace): `fa $> x = fmap (const x) fa`
- [ ] 1.2.1.5 Implement `void` to replace contents with Unit: `void fa = () <$ fa`
- [ ] 1.2.1.6 Document Functor laws: identity (`fmap id x === x`), composition (`fmap (f . g) x === fmap f (fmap g x)`)

### 1.2.2 Contravariant
- [ ] **Task 1.2.2 Complete**

Contravariant functors reverse the direction of the mapping. While covariant functors transform outputs, contravariant functors transform inputs. They appear in predicates, comparators, and serializers.

- [ ] 1.2.2.1 Define `Contravariant` trait with `contramap : (b -> a) -> f a -> f b`
- [ ] 1.2.2.2 Define `>$<` operator as infix version of `contramap`
- [ ] 1.2.2.3 Implement `>$` (contravariant replace): `x >$ fa = contramap (const x) fa`
- [ ] 1.2.2.4 Document Contravariant laws: identity (`contramap id x === x`), composition (`contramap (f . g) x === contramap g (contramap f x)`)
- [ ] 1.2.2.5 Provide instance for `Predicate a = Predicate (a -> Bool)`

### 1.2.3 Bifunctor
- [ ] **Task 1.2.3 Complete**

Bifunctors are functors over two type parameters, allowing mapping over both simultaneously. They appear in tuple types and Either/Result types.

- [ ] 1.2.3.1 Define `Bifunctor` trait with `bimap : (a -> c) -> (b -> d) -> f a b -> f c d`
- [ ] 1.2.3.2 Implement `first : (a -> c) -> f a b -> f c b` as `bimap f id`
- [ ] 1.2.3.3 Implement `second : (b -> d) -> f a b -> f a d` as `bimap id g`
- [ ] 1.2.3.4 Document Bifunctor laws: identity (`bimap id id x === x`), composition
- [ ] 1.2.3.5 Provide instances for `Tuple`, `Either`, `Result`

### 1.2.4 Profunctor
- [ ] **Task 1.2.4 Complete**

Profunctors are bifunctors that are contravariant in the first argument and covariant in the second. They generalize functions and appear in optics (lenses, prisms).

- [ ] 1.2.4.1 Define `Profunctor` trait with `dimap : (c -> a) -> (b -> d) -> p a b -> p c d`
- [ ] 1.2.4.2 Implement `lmap : (c -> a) -> p a b -> p c b` as `dimap f id`
- [ ] 1.2.4.3 Implement `rmap : (b -> d) -> p a b -> p a d` as `dimap id g`
- [ ] 1.2.4.4 Document Profunctor laws: identity, composition
- [ ] 1.2.4.5 Provide instance for function type `(->)`

### Unit Tests - Section 1.2
- [ ] **Unit Tests 1.2 Complete**
- [ ] Test Functor laws (identity, composition) for Maybe, List, Either with property-based testing
- [ ] Test Contravariant laws for Predicate type
- [ ] Test Bifunctor laws for Tuple and Either
- [ ] Test Profunctor laws for function type
- [ ] Test all operators (`<$>`, `<$`, `$>`, `>$<`, `>$`) produce correct results
- [ ] Test `void` correctly replaces contents with Unit

---

# Phase 2: Applicative and Monad

**Duration**: 2 weeks (Weeks 3-4)

This phase implements the applicative functor and monad hierarchies, which are central to effectful and sequential computation in functional programming. These abstractions enable composing computations that may fail, have side effects, or require sequencing.

## 2.1 Applicative Functors
- [ ] **Section 2.1 Complete**

Applicative functors extend functors with the ability to apply functions within a context. They enable combining multiple independent computations and are more powerful than functors but less powerful than monads.

### 2.1.1 Applicative
- [ ] **Task 2.1.1 Complete**

Applicative provides `pure` to lift values into a context and `<*>` to apply functions within a context. This enables parallel/independent combination of effectful computations.

- [ ] 2.1.1.1 Define `Applicative` trait as alias to `Applicator` extending `Functor` with `pure : a -> f a` and `ap : f (a -> b) -> f a -> f b` (success: `Applicative` can be used interchangeably with `Applicator`)
- [ ] 2.1.1.2 Define `<*>` operator mapping to `ap` function
- [ ] 2.1.1.3 Implement `<*` (sequence, keep left): `fa <* fb = pure const <*> fa <*> fb`
- [ ] 2.1.1.4 Implement `*>` (sequence, keep right): `fa *> fb = pure (flip const) <*> fa <*> fb`
- [ ] 2.1.1.5 Implement `liftA2 : (a -> b -> c) -> f a -> f b -> f c`
- [ ] 2.1.1.6 Implement `liftA3` for three arguments
- [ ] 2.1.1.7 Document Applicative laws: identity, composition, homomorphism, interchange

### 2.1.2 Alternative
- [ ] **Task 2.1.2 Complete**

Alternative extends Applicative with choice and failure. It provides a monoid over applicative functors, enabling parsing with alternatives and computations that can fail and recover.

- [ ] 2.1.2.1 Define `Alternative` trait extending `Applicative` with `empty : f a` and `alt : f a -> f a -> f a`
- [ ] 2.1.2.2 Define `<|>` operator mapping to `alt` function
- [ ] 2.1.2.3 Implement `some : f a -> f (List a)` (one or more)
- [ ] 2.1.2.4 Implement `many : f a -> f (List a)` (zero or more)
- [ ] 2.1.2.5 Implement `optional : f a -> f (Maybe a)`
- [ ] 2.1.2.6 Document Alternative laws: monoid laws for `empty` and `<|>`, left/right distributivity
- [ ] 2.1.2.7 Provide instances for `Maybe`, `List`

### 2.1.3 Applicative Operators
- [ ] **Task 2.1.3 Complete**

Define the complete set of applicative operators with correct precedence and associativity for ergonomic use.

- [ ] 2.1.3.1 Define operator precedence: `<$>` and `<*>` left-associative at same level
- [ ] 2.1.3.2 Define `<**>` as flipped `<*>`: `fa <**> ff = ff <*> fa`
- [ ] 2.1.3.3 Implement `forever : f a -> f b` (repeat indefinitely)
- [ ] 2.1.3.4 Implement `when : Bool -> f Unit -> f Unit` (conditional execution)
- [ ] 2.1.3.5 Implement `unless : Bool -> f Unit -> f Unit` (negated conditional)

### Unit Tests - Section 2.1
- [ ] **Unit Tests 2.1 Complete**
- [ ] Test Applicative laws for Maybe, List, Either with property-based testing
- [ ] Test Alternative laws for Maybe and List
- [ ] Test `liftA2`, `liftA3` correctly combine computations
- [ ] Test `some`, `many`, `optional` for parsers
- [ ] Test all operators produce correct results with proper associativity
- [ ] Test `when`, `unless` conditional execution

## 2.2 Monad
- [ ] **Section 2.2 Complete**

Monad extends applicative with sequential composition where later computations can depend on earlier results. This is the foundation for effectful programming, enabling error handling, state, IO, and more.

### 2.2.1 Monad
- [ ] **Task 2.2.1 Complete**

Monad provides `bind` (>>=) for sequential composition and `return` (same as `pure`). It enables the full power of effectful computation with dependent sequencing.

- [ ] 2.2.1.1 Define `Monad` trait as alias to `Pipeline` extending `Applicative` with `bind : m a -> (a -> m b) -> m b` (success: `Monad` can be used interchangeably with `Pipeline`)
- [ ] 2.2.1.2 Define `>>=` operator mapping to `bind` function
- [ ] 2.2.1.3 Define `>>` operator for sequencing ignoring result: `ma >> mb = ma >>= const mb`
- [ ] 2.2.1.4 Define `=<<` as flipped bind: `f =<< ma = ma >>= f`
- [ ] 2.2.1.5 Implement `return` as alias for `pure`
- [ ] 2.2.1.6 Implement `join : m (m a) -> m a` as `ma >>= id`
- [ ] 2.2.1.7 Document Monad laws: left identity (`return a >>= f === f a`), right identity (`ma >>= return === ma`), associativity

### 2.2.2 MonadPlus and MonadFail
- [ ] **Task 2.2.2 Complete**

MonadPlus combines Monad with Alternative for monads with choice. MonadFail provides explicit failure handling for pattern match failures in do-notation.

- [ ] 2.2.2.1 Define `MonadPlus` trait extending `Monad` and `Alternative` (no new methods)
- [ ] 2.2.2.2 Implement `mfilter : (a -> Bool) -> m a -> m a` for MonadPlus
- [ ] 2.2.2.3 Implement `guard : Bool -> m Unit` for MonadPlus
- [ ] 2.2.2.4 Define `MonadFail` trait with `fail : String -> m a`
- [ ] 2.2.2.5 Document MonadPlus laws: left zero (`mzero >>= f === mzero`), left distribution
- [ ] 2.2.2.6 Provide instances for `Maybe`, `List`

### 2.2.3 Monad Transformers
- [ ] **Task 2.2.3 Complete**

Monad transformers allow stacking monadic effects. They wrap one monad inside another, combining their capabilities (e.g., MaybeT for optional results in IO).

- [ ] 2.2.3.1 Define `MonadTrans` trait with `lift : m a -> t m a`
- [ ] 2.2.3.2 Implement `MaybeT m a` transformer
- [ ] 2.2.3.3 Implement `EitherT e m a` transformer (also called ExceptT)
- [ ] 2.2.3.4 Implement `ReaderT r m a` transformer
- [ ] 2.2.3.5 Implement `StateT s m a` transformer
- [ ] 2.2.3.6 Document transformer laws and lifting requirements

### 2.2.4 Kleisli Composition
- [ ] **Task 2.2.4 Complete**

Kleisli arrows are functions that return monadic values. Kleisli composition allows composing these effectful functions directly.

- [ ] 2.2.4.1 Define `>=>` operator (left-to-right Kleisli): `(f >=> g) x = f x >>= g`
- [ ] 2.2.4.2 Define `<=<` operator (right-to-left Kleisli): `(g <=< f) x = f x >>= g`
- [ ] 2.2.4.3 Implement `Kleisli` newtype wrapper: `Kleisli m a b = Kleisli (a -> m b)`
- [ ] 2.2.4.4 Provide `Category` instance for `Kleisli m`
- [ ] 2.2.4.5 Document Kleisli composition laws (category laws)

### Unit Tests - Section 2.2
- [ ] **Unit Tests 2.2 Complete**
- [ ] Test Monad laws for Maybe, List, Either, Result with property-based testing
- [ ] Test MonadPlus laws for Maybe and List
- [ ] Test MonadFail for pattern match failure handling
- [ ] Test monad transformers correctly compose effects
- [ ] Test `lift` correctly lifts inner monad operations
- [ ] Test Kleisli composition satisfies category laws
- [ ] Test all operators (`>>=`, `>>`, `=<<`, `>=>`, `<=<`) produce correct results

---

# Phase 3: Comonad and Arrows

**Duration**: 1.5 weeks (Weeks 5-6.5)

This phase implements comonads (the dual of monads) and the arrow abstraction. Comonads are useful for streaming, cellular automata, and context-dependent computation. Arrows generalize functions and provide a way to work with computation pipelines.

## 3.1 Comonad
- [ ] **Section 3.1 Complete**

Comonads are the categorical dual of monads. While monads inject values into a context and sequence computations, comonads extract values from a context and extend computations. They're used in dataflow programming, streaming, and UI frameworks.

### 3.1.1 Comonad
- [ ] **Task 3.1.1 Complete**

Comonad provides `extract` to get a value from context and `extend` to apply a context-consuming function to all positions in the structure.

- [ ] 3.1.1.1 Define `Comonad` trait as alias to `Extractor` extending `Functor` with `extract : w a -> a` and `extend : (w a -> b) -> w a -> w b` (success: `Comonad` can be used interchangeably with `Extractor`)
- [ ] 3.1.1.2 Implement `duplicate : w a -> w (w a)` as `extend id`
- [ ] 3.1.1.3 Define `=>>` operator (left-to-right extend): `wa =>> f = extend f wa`
- [ ] 3.1.1.4 Define `<<=` operator (right-to-left extend): `f <<= wa = extend f wa`
- [ ] 3.1.1.5 Document Comonad laws: left identity (`extend extract === id`), right identity (`extract . extend f === f`), associativity

### 3.1.2 Common Comonads
- [ ] **Task 3.1.2 Complete**

Implement standard comonad instances that demonstrate the abstraction's utility: Store for memoization, Env for context, and Traced for logging.

- [ ] 3.1.2.1 Implement `Identity` comonad (trivial comonad)
- [ ] 3.1.2.2 Implement `NonEmpty` comonad (non-empty list with focus on head)
- [ ] 3.1.2.3 Implement `Store s a` comonad (position-indexed structure): `Store (s -> a, s)`
- [ ] 3.1.2.4 Implement `Env e a` comonad (value with environment): `Env (e, a)`
- [ ] 3.1.2.5 Implement `Traced m a` comonad (value with monoidal trace): `Traced (m -> a)`
- [ ] 3.1.2.6 Provide accessor functions: `pos`, `peek` for Store; `ask` for Env; `trace` for Traced

### Unit Tests - Section 3.1
- [ ] **Unit Tests 3.1 Complete**
- [ ] Test Comonad laws for all instances with property-based testing
- [ ] Test `extract` correctly retrieves the focus value
- [ ] Test `extend` correctly applies function at all positions
- [ ] Test `duplicate` creates correct nested structure
- [ ] Test Store comonad for memoization patterns
- [ ] Test Env comonad for context-dependent computation

## 3.2 Category and Arrow
- [ ] **Section 3.2 Complete**

Arrows generalize functions to computations with a more structured interface. They're useful when you need more control than monads provide, particularly for circuits, parsers, and stream processors.

### 3.2.1 Category
- [ ] **Task 3.2.1 Complete**

Category defines composition and identity, generalizing function composition to arbitrary arrow-like structures.

- [ ] 3.2.1.1 Define `Category` trait with `id : cat a a` and `compose : cat b c -> cat a b -> cat a c`
- [ ] 3.2.1.2 Define `>>>` operator (left-to-right): `f >>> g = compose g f`
- [ ] 3.2.1.3 Define `<<<` operator (right-to-left): `g <<< f = compose g f`
- [ ] 3.2.1.4 Document Category laws: left identity, right identity, associativity
- [ ] 3.2.1.5 Provide instance for function type `(->)`

### 3.2.2 Arrow
- [ ] **Task 3.2.2 Complete**

Arrow extends Category with the ability to lift functions and work with pairs. This enables parallel composition and product structures.

- [ ] 3.2.2.1 Define `Arrow` trait extending `Category` with `arr : (a -> b) -> arr a b` and `first : arr a b -> arr (a, c) (b, c)`
- [ ] 3.2.2.2 Implement `second : arr a b -> arr (c, a) (c, b)` using `first` and `arr swap`
- [ ] 3.2.2.3 Define `***` operator (parallel): `f *** g = first f >>> arr swap >>> first g >>> arr swap`
- [ ] 3.2.2.4 Define `&&&` operator (fanout): `f &&& g = arr dup >>> (f *** g)` where `dup x = (x, x)`
- [ ] 3.2.2.5 Document Arrow laws involving `arr`, `first`, `>>>`, `***`
- [ ] 3.2.2.6 Provide instance for function type

### 3.2.3 ArrowChoice and ArrowApply
- [ ] **Task 3.2.3 Complete**

ArrowChoice adds branching (sum types) to arrows. ArrowApply allows applying arrows within the arrow computation, equivalent to monads in expressive power.

- [ ] 3.2.3.1 Define `ArrowChoice` extending `Arrow` with `left : arr a b -> arr (Either a c) (Either b c)`
- [ ] 3.2.3.2 Implement `right : arr a b -> arr (Either c a) (Either c b)`
- [ ] 3.2.3.3 Define `+++` operator (sum parallel): `f +++ g` handles both branches
- [ ] 3.2.3.4 Define `|||` operator (fanin): `f ||| g` merges branches
- [ ] 3.2.3.5 Define `ArrowApply` with `app : arr (arr a b, a) b`
- [ ] 3.2.3.6 Document ArrowChoice and ArrowApply laws

### 3.2.4 Arrow Utilities
- [ ] **Task 3.2.4 Complete**

Implement utility functions for working with arrows in common patterns.

- [ ] 3.2.4.1 Implement `returnA : arr a a` as `arr id`
- [ ] 3.2.4.2 Implement `(^>>) : (a -> b) -> arr b c -> arr a c` (pre-composition with function)
- [ ] 3.2.4.3 Implement `(>>^) : arr a b -> (b -> c) -> arr a c` (post-composition with function)
- [ ] 3.2.4.4 Implement `(<<^) : arr b c -> (a -> b) -> arr a c` (reverse pre-composition)
- [ ] 3.2.4.5 Implement `(^<<) : (b -> c) -> arr a b -> arr a c` (reverse post-composition)

### Unit Tests - Section 3.2
- [ ] **Unit Tests 3.2 Complete**
- [ ] Test Category laws for function and Kleisli instances
- [ ] Test Arrow laws with property-based testing
- [ ] Test `***` and `&&&` correctly handle pairs
- [ ] Test ArrowChoice correctly routes Either values
- [ ] Test all operators have correct precedence and associativity
- [ ] Test utility functions for arrow composition patterns

---

# Phase 4: Foldable and Traversable

**Duration**: 0.5 weeks (Weeks 6.5-7)

This phase implements foldable and traversable, which provide generic ways to reduce structures to values and to traverse structures with effects.

## 4.1 Foldable
- [ ] **Section 4.1 Complete**

Foldable abstracts the ability to reduce a structure to a single value. It generalizes list folding to any container type.

### 4.1.1 Foldable Trait
- [ ] **Task 4.1.1 Complete**

Define the Foldable trait with multiple fold variants for different use cases.

- [ ] 4.1.1.1 Define `Foldable` trait with `foldr : (a -> b -> b) -> b -> t a -> b` (minimal definition)
- [ ] 4.1.1.2 Implement `foldl : (b -> a -> b) -> b -> t a -> b`
- [ ] 4.1.1.3 Implement `foldMap : Monoid m => (a -> m) -> t a -> m`
- [ ] 4.1.1.4 Implement `fold : Monoid m => t m -> m` as `foldMap id`
- [ ] 4.1.1.5 Document Foldable laws relating `foldr`, `foldl`, `foldMap`

### 4.1.2 Foldable Utilities
- [ ] **Task 4.1.2 Complete**

Implement common operations derived from Foldable for convenience.

- [ ] 4.1.2.1 Implement `toList : t a -> List a`
- [ ] 4.1.2.2 Implement `null : t a -> Bool` (check if empty)
- [ ] 4.1.2.3 Implement `length : t a -> Natural`
- [ ] 4.1.2.4 Implement `elem : Setoid a => a -> t a -> Bool`
- [ ] 4.1.2.5 Implement `maximum`, `minimum : Orderable a => t a -> Maybe a`
- [ ] 4.1.2.6 Implement `sum`, `product : Monoid a => t a -> a`
- [ ] 4.1.2.7 Implement `any`, `all : (a -> Bool) -> t a -> Bool`
- [ ] 4.1.2.8 Implement `find : (a -> Bool) -> t a -> Maybe a`
- [ ] 4.1.2.9 Provide instances for `List`, `Maybe`, `Either`, `Tuple`

### Unit Tests - Section 4.1
- [ ] **Unit Tests 4.1 Complete**
- [ ] Test Foldable laws for all instances
- [ ] Test `foldr` and `foldl` produce correct results
- [ ] Test `foldMap` respects monoid operations
- [ ] Test all utility functions for various container types
- [ ] Test edge cases: empty containers, single elements

## 4.2 Traversable
- [ ] **Section 4.2 Complete**

Traversable extends Foldable with the ability to traverse a structure while accumulating effects. It combines mapping with effect accumulation.

### 4.2.1 Traversable Trait
- [ ] **Task 4.2.1 Complete**

Define Traversable for structures that can be traversed with applicative effects.

- [ ] 4.2.1.1 Define `Traversable` trait extending `Functor` and `Foldable` with `traverse : Applicative f => (a -> f b) -> t a -> f (t b)`
- [ ] 4.2.1.2 Implement `sequenceA : Applicative f => t (f a) -> f (t a)` as `traverse id`
- [ ] 4.2.1.3 Implement `mapM : Monad m => (a -> m b) -> t a -> m (t b)` (traverse for monads)
- [ ] 4.2.1.4 Implement `sequence : Monad m => t (m a) -> m (t a)` (sequenceA for monads)
- [ ] 4.2.1.5 Document Traversable laws: naturality, identity, composition

### 4.2.2 Traversable Utilities
- [ ] **Task 4.2.2 Complete**

Implement common traversal patterns and utilities.

- [ ] 4.2.2.1 Implement `for : Applicative f => t a -> (a -> f b) -> f (t b)` (flipped traverse)
- [ ] 4.2.2.2 Implement `forM : Monad m => t a -> (a -> m b) -> m (t b)` (flipped mapM)
- [ ] 4.2.2.3 Implement `mapAccumL : (s -> a -> (s, b)) -> s -> t a -> (s, t b)`
- [ ] 4.2.2.4 Implement `mapAccumR : (s -> a -> (s, b)) -> s -> t a -> (s, t b)`
- [ ] 4.2.2.5 Provide instances for `List`, `Maybe`, `Either`, `Tuple`

### Unit Tests - Section 4.2
- [ ] **Unit Tests 4.2 Complete**
- [ ] Test Traversable laws for all instances with property-based testing
- [ ] Test `traverse` correctly sequences effects
- [ ] Test `sequenceA` inverts nested applicatives
- [ ] Test `mapAccumL` and `mapAccumR` maintain state correctly
- [ ] Test traversal over Maybe, Either, List with various applicatives

---

# Phase 5: Operators and Laws

**Duration**: 0.5 weeks (Weeks 7-7.5)

This phase consolidates all operators with proper precedence and provides comprehensive law documentation with property-based test templates.

## 5.1 Operator Definitions
- [ ] **Section 5.1 Complete**

Define all category theory operators in a central module with consistent precedence and associativity.

### 5.1.1 Complete Operator Suite
- [ ] **Task 5.1.1 Complete**

Consolidate all operators defined throughout the library into a comprehensive reference.

- [ ] 5.1.1.1 Create `Category.Operators` module exporting all operators
- [ ] 5.1.1.2 Document Setoid operators: `===`, `!==`
- [ ] 5.1.1.3 Document Semigroup operator: `<>`
- [ ] 5.1.1.4 Document Functor operators: `<$>`, `<$`, `$>`, `>$<`, `>$`
- [ ] 5.1.1.5 Document Applicative operators: `<*>`, `<*`, `*>`, `<**>`, `<|>`
- [ ] 5.1.1.6 Document Monad operators: `>>=`, `>>`, `=<<`, `>=>`, `<=<`
- [ ] 5.1.1.7 Document Arrow operators: `>>>`, `<<<`, `***`, `&&&`, `+++`, `|||`
- [ ] 5.1.1.8 Document Comonad operators: `=>>`, `<<=`

### 5.1.2 Operator Precedence
- [ ] **Task 5.1.2 Complete**

Define consistent precedence levels ensuring intuitive parsing without excessive parentheses.

- [ ] 5.1.2.1 Define precedence table from lowest to highest binding
- [ ] 5.1.2.2 Set composition operators (`>>>`, `<<<`, `.`) at high precedence
- [ ] 5.1.2.3 Set application operators (`<*>`, `<$>`) below composition
- [ ] 5.1.2.4 Set monad operators (`>>=`, `>>`) at lower precedence for sequencing
- [ ] 5.1.2.5 Set comparison operators (`===`, `!==`) at lowest precedence
- [ ] 5.1.2.6 Document associativity: left for application, right for composition and bind

### Unit Tests - Section 5.1
- [ ] **Unit Tests 5.1 Complete**
- [ ] Test operator precedence parses expressions correctly
- [ ] Test associativity produces expected results
- [ ] Test all operators are exported from central module
- [ ] Test operators work correctly with type inference

## 5.2 Law Documentation
- [ ] **Section 5.2 Complete**

Provide comprehensive documentation of all algebraic laws with property-based test templates.

### 5.2.1 Law Reference
- [ ] **Task 5.2.1 Complete**

Create a comprehensive reference of all laws for each abstraction.

- [ ] 5.2.1.1 Document Setoid laws with examples
- [ ] 5.2.1.2 Document Semigroup and Monoid laws with examples
- [ ] 5.2.1.3 Document Functor laws with examples
- [ ] 5.2.1.4 Document Applicative laws with examples
- [ ] 5.2.1.5 Document Monad laws with examples
- [ ] 5.2.1.6 Document Comonad laws with examples
- [ ] 5.2.1.7 Document Category and Arrow laws with examples
- [ ] 5.2.1.8 Document Foldable and Traversable laws with examples

### 5.2.2 Property-Based Test Templates
- [ ] **Task 5.2.2 Complete**

Provide reusable test templates for verifying law compliance of new instances.

- [ ] 5.2.2.1 Create `checkSetoidLaws` test generator
- [ ] 5.2.2.2 Create `checkSemigroupLaws`, `checkMonoidLaws` test generators
- [ ] 5.2.2.3 Create `checkFunctorLaws` test generator
- [ ] 5.2.2.4 Create `checkApplicativeLaws` test generator
- [ ] 5.2.2.5 Create `checkMonadLaws` test generator
- [ ] 5.2.2.6 Create `checkComonadLaws` test generator
- [ ] 5.2.2.7 Create `checkTraversableLaws` test generator
- [ ] 5.2.2.8 Document how to use test generators for custom instances

### Unit Tests - Section 5.2
- [ ] **Unit Tests 5.2 Complete**
- [ ] Test law documentation examples compile and run correctly
- [ ] Test property generators produce valid test cases
- [ ] Test all built-in instances pass their respective law checks
- [ ] Test error messages when laws are violated are clear

---

## Success Criteria

1. **Algebraic Structures**: Setoid, Semigroup, Monoid, and Group traits defined with all laws documented and tested
2. **Functor Hierarchy**: Functor, Contravariant, Bifunctor, and Profunctor implemented with correct instances
3. **Applicative**: Applicative and Alternative traits with all operators and utility functions
4. **Monad**: Monad, MonadPlus, MonadFail, and transformers with Kleisli composition
5. **Comonad**: Comonad trait with Store, Env, and Traced instances
6. **Arrows**: Category, Arrow, ArrowChoice, and ArrowApply with all operators
7. **Foldable/Traversable**: Both traits with comprehensive utility functions
8. **Operators**: All operators defined with consistent precedence and associativity
9. **Laws**: Complete law documentation with property-based test templates
10. **Interoperability**: All category theory names work as aliases to pragmatic traits

## Provides Foundation

This library establishes the infrastructure for:
- **Advanced Type System Features**: Higher-kinded types, type families
- **Effect System Integration**: Monad transformers for algebraic effects
- **Optimization Passes**: Functor fusion, deforestation, rewrite rules
- **Optics Library**: Lenses and prisms built on Profunctor
- **Parser Combinators**: Built on Applicative and Alternative
- **Streaming Libraries**: Built on Comonad and Arrow
- **Formal Verification**: Law-based reasoning and equational proofs

## Key Outputs

- **8 library files** in `lib/catena/stdlib/category/`:
  - `core.cat` - Trait aliases and re-exports
  - `functor.cat` - Functor hierarchy
  - `applicative.cat` - Applicative and Alternative
  - `monad.cat` - Monad and transformers
  - `comonad.cat` - Comonad instances
  - `arrow.cat` - Category and Arrow
  - `foldable.cat` - Foldable and Traversable
  - `operators.cat` - All operators
- **Comprehensive operator suite** with 30+ operators
- **Law documentation** for all abstractions
- **Property-based test templates** for verifying new instances
- **Examples** demonstrating each abstraction's usage
