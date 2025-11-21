# Catena Language Guide

## Language Keywords

### Core Keywords (12 keywords requiring compiler support)

| Keyword | Purpose | Example |
|---------|---------|---------|
| **type** | Define data types | `type User = {name: Text, age: Natural}` |
| **transform** | Define pure functions | `transform greet : User -> Text` |
| **let** | Local bindings | `let x = 5 in x + 1` |
| **match** | Pattern matching | `match x \| Some v -> v \| None -> 0` |
| **trait** | Define type classes | `trait Mapper f where ... end` |
| **instance** | Implement traits | `instance Mapper List where ... end` |
| **effect** | Define algebraic effects | `effect FileIO { ... } end` |
| **perform** | Use effects | `perform FileIO.read(path)` |
| **handle** | Handle effects | `handle expr { ... }` |
| **actor** | Define actors | `actor Counter = {...}` |
| **process** | Actor message handlers | `process handle : Message -> State -> ...` |
| **module** | Define modules | `module Collections` |

### Syntax Keywords

| Keyword | Purpose | Example |
|---------|---------|---------|
| **in** | let expression body | `let x = 5 in x + 1` |
| **end** | Block terminator | `instance ... end`, `match ... end` |
| **where** | Instance body | `instance Trait Type where ... end` |
| **when** | Guards | `transform f x when x > 0 = ...` |
| **as** | Pattern alias | `match list \| (h :: t) as whole -> ...` |
| **forall** | Explicit polymorphism | `forall a. a -> a` |
| **operation** | Effect operations | `operation read : String -> String` |

### Removed Keywords (Library or Desugaring)

The following keywords have been removed from the core language:

- **do** - Desugar to `>>=` (Pipeline bind)
- **if/then/else** - Desugar to `match` on Bool
- **extends** - Use `:` syntax in trait definition
- **try/with** - Replaced by `handle` keyword
- **supervisor** - Library convention
- **test/property** - Use `Test` module from stdlib
- **laws** - Define in documentation or separate verification

## Introduction

Catena is a new functional programming language for the BEAM virtual machine that fundamentally grounds itself in category theory principles. Unlike traditional functional languages that add categorical concepts as libraries, Catena makes category theory the foundation from which all language features emerge naturally. This creates a unique synthesis where mathematical rigor meets practical distributed systems programming.

To make category theory accessible, Catena uses **pragmatic terminology** alongside traditional mathematical names. Where mathematicians say "morphism", we say **Transform**. Where they say "object", we say **Type**. This developer-friendly naming makes the powerful abstractions of category theory intuitive for everyday programming.

**Note**: This guide focuses on core language features. For information about standard library abstractions (Mapper/Functor, Pipeline/Monad, operators, etc.), see the [Standard Library Guide](../guides/standard-library-overview.md).

## Core Philosophy

In Catena, **everything is a System (category)**. Programs are compositions of Transforms (morphisms), data is represented through immutable Types (objects), and side effects are handled through algebraic effect handlers. The language treats the BEAM's actor model as a natural categorical structure, making distributed programming both type-safe and mathematically sound.

## Fundamental Concepts

### 1. Types (Objects in Category Theory)

**Types** define data structures as objects in a System. They are immutable by construction and support algebraic data types. In category theory, these are called *Objects* - the fundamental entities that Transforms operate between.

```catena
-- Product types (records)
type User = {
  name: Text,
  age: Natural,
  email: Email
} deriving [Eq, Show, Doc]

-- Sum types (variants) - Choice (Coproduct) in CT
type Result a b = Ok a | Error b

-- Recursive types
type List a = Nil | Cons a (List a)

-- Type aliases
type CustomerId = Natural
type Money = Decimal
```

### 2. Transforms (Morphisms in Category Theory)

**Transforms** are pure functions that represent morphisms between Types. They compose naturally and preserve categorical properties. In category theory, *Morphisms* are the arrows that connect objects.

```catena
-- Basic transform definition
transform greet : User -> Text
transform greet user = Text.concat "Hello, " user.name

-- Composition using pipe operator
transform processUser : User -> Result User ValidationError
transform processUser =
  validate |> normalize |> store

-- Type class constraints with Mapper (Functor)
transform double : List Natural -> List Natural
transform double xs = List.map (* 2) xs

-- Polymorphic transforms with constraints
transform total : Accumulator a => List a -> a
transform total xs = List.fold combine empty xs
```

### 3. Trait System (Type Classes)

Traits define type classes with laws that implementations must satisfy. This is the foundation for all category theory abstractions in Catena.

```catena
-- Define a Comparable trait (Setoid in category theory)
trait Comparable a where
  equals : a -> a -> Bool

  -- Default implementations
  not_equals : a -> a -> Bool
  not_equals x y = not (equals x y)
end

-- Define operators for traits
operator (===) = equals
operator (!==) = not_equals

-- Trait hierarchies
trait Orderable a : Comparable a where
  compare : a -> a -> Ordering

  less_than : a -> a -> Bool
  less_than x y = compare x y == LT
end

-- Mapper trait (Functor in category theory)
trait Mapper (f : Type -> Type) where
  map : (a -> b) -> f a -> f b
end

-- Applicator trait (Applicative Functor in category theory)
trait Applicator (f : Type -> Type) : Mapper f where
  pure : a -> f a
  apply : f (a -> b) -> f a -> f b
end

-- Chainable trait (Chain/Bind in category theory)
trait Chainable (m : Type -> Type) : Mapper m where
  chain : (a -> m b) -> m a -> m b
end

-- Pipeline trait (Monad in category theory)
trait Pipeline (m : Type -> Type) : Applicator m, Chainable m where
  -- Combines pure and chain from parent traits
end

-- Extractor trait (Comonad in category theory)
trait Extractor (w : Type -> Type) : Mapper w where
  extract : w a -> a
  extend : (w a -> b) -> w a -> w b
end

-- Trait laws (verified by property testing)
laws Mapper f where
  property "identity" =
    forall x : f a ->
      map identity x === x

  property "composition" =
    forall x : f a, g : (a -> b), h : (b -> c) ->
      map (h . g) x === (map h . map g) x
end
```

### 4. Instances (Trait Implementations)

Instances provide concrete implementations of traits for specific types.

```catena
-- Simple instance
instance Comparable Natural where
  transform equals x y = x == y
end

-- Instance with constraints
instance Comparable a => Comparable (List a) where
  transform equals Nil Nil = True
  transform equals (Cons x xs) (Cons y ys) = x === y && xs === ys
  transform equals _ _ = False
end

-- Instance for Mapper
instance Mapper List where
  transform map f xs = List.map f xs
end

-- Instance for Applicator
instance Applicator List where
  transform pure x = [x]
  transform apply fs xs = List.flat_map (\f -> map f xs) fs
end

-- Instance for Pipeline (Monad)
instance Pipeline Maybe where
  transform pure x = Some x
  transform apply mf mx = match mf, mx
    | None, _ -> None
    | _, None -> None
    | Some f, Some x -> Some (f x)
  end
  transform chain f mx = match mx
    | None -> None
    | Some x -> f x
  end
end

-- Instance for Extractor (Comonad)
instance Extractor NonEmptyList where
  transform extract (NEL x _) = x
  transform extend f w@(NEL x xs) = NEL (f w) (extend_list f xs x [])
end
```

### 5. Systems (Categories/Modules)

Systems organize code into categorical structures with explicit imports and exports.

```catena
-- System definition
system Collections = {
  -- Export types (Objects)
  export type List a
  export type Set a
  export type Map k v

  -- Export transforms (Morphisms)
  export transform map : (a -> b) -> List a -> List b
  export transform filter : (a -> Bool) -> List a -> List a
  export transform fold : (a -> b -> b) -> b -> List a -> b

  -- Internal helpers (not exported)
  transform partition_helper : (a -> Bool) -> List a -> (List a, List a)
  transform partition_helper pred xs = ...
}

-- Import from systems
import Collections (List, map, filter)

-- Qualified imports
import qualified Data.Natural as Nat
```

### 6. Effects and Handlers

Side effects are managed through algebraic effect handlers, making them explicit in types. Effects are essentially **EffectfulTransforms** (Kleisli arrows in category theory).

```catena
-- Define an effect
effect FileIO = {
  read : Path -> String,
  write : Path -> String -> Unit
}

-- Use effects with perform keyword
transform loadConfig : Path -> Config / {FileIO}
transform loadConfig path = do
  content <- perform FileIO.read(path)
  return parseConfig(content)

-- Handle effects
handle loadConfig("app.toml") with
  FileIO.read(p) -> readFromDisk(p)
  FileIO.write(p, c) -> writeToDisk(p, c)
end

-- Pipeline-based effects
trait PipelineIO (m : Type -> Type) : Pipeline m where
  liftIO : IO a -> m a
```

### 7. Actors and Processes

BEAM processes are first-class with categorical structure. Actors form their own System where message passing represents Transforms.

```catena
actor Counter = {
  type State = { count: Natural }
  type Message = Increment | Decrement | Get

  transform init : Unit -> State
  transform init () = { count: 0 }

  -- Use 'process' keyword for message handlers
  process handle : Message -> State -> (State, Maybe Reply)
  process handle msg state = match msg
    | Increment -> ({ count: state.count + 1 }, None)
    | Decrement -> ({ count: state.count - 1 }, None)
    | Get -> (state, Some state.count)
  end
}

-- Supervisor definition
supervisor CounterSupervisor = {
  strategy: one_for_one,
  intensity: 10,
  period: 60,

  children: [
    {
      id: counter,
      start: Counter.start_link,
      restart: permanent,
      shutdown: 5000
    }
  ]
}
```

### 8. Adapters (Natural Transformations)

Adapters provide structure-preserving conversions between type constructors, representing natural transformations in category theory.

```catena
-- Adapter definition
adapter listToMaybe : List ~> Maybe where
  adapt [] = None
  adapt (x::_) = Some x

-- Using adapters
transform firstElement : List a -> Maybe a
transform firstElement xs = adapt listToMaybe xs

-- Adapter laws
laws adapter listToMaybe where
  property "naturality" =
    forall f : (a -> b), xs : List a ->
      map f (adapt xs) === adapt (map f xs)
```

### 9. Pattern Matching

Advanced pattern matching with categorical foundations.

```catena
transform process : Message -> Response
transform process = match
  -- View patterns
  | parse_json -> Ok(data) -> handle_data(data)

  -- Or-patterns (Choice/Coproduct decomposition)
  | Error(e) | Failure(e) -> handle_error(e)

  -- Pattern guards with bindings
  | Request(data) when validate(data) -> Valid(v) ->
    process_valid(v)

  -- Pattern synonyms
  | Success(result) -> return_success(result)
end
```

### 10. Documentation as First-Class

Documentation is mandatory and introspectable.

```catena
doc "Calculates the distance between two points"
doc params {
  p1: "First point in 2D space",
  p2: "Second point in 2D space"
}
doc returns "Euclidean distance as Float"
doc examples """
  distance({x: 0, y: 0}, {x: 3, y: 4}) == 5.0
"""
transform distance : Point -> Point -> Float
transform distance p1 p2 =
  sqrt((p2.x - p1.x)^2 + (p2.y - p1.y)^2)

-- Documentation for trait instances
doc """
  Comparable instance for List ensures structural equality:
  - Reflexivity: xs === xs
  - Symmetry: xs === ys implies ys === xs
  - Transitivity: xs === ys and ys === zs implies xs === zs
"""
instance Comparable a => Comparable (List a) where
  equals = List.equal_by equals
```

### 11. Testing as Language Primitive

Tests verify that implementations satisfy categorical laws.

```catena
-- Unit tests
test "distance calculation" =
  let origin = {x: 0, y: 0}
  let point = {x: 3, y: 4}
  assert distance(origin, point) == 5.0

-- Property-based testing
property "distance is symmetric" =
  forall p1 p2 : Point ->
    distance(p1, p2) === distance(p2, p1)

-- Law verification for pragmatic traits
test "List satisfies Mapper laws" =
  verify laws Mapper for List

test "Maybe is a valid Pipeline" =
  verify laws Pipeline for Maybe

test "Natural forms an Accumulator" =
  verify laws Accumulator for Natural

-- Test suites for organization
suite "Collection Operations" where
  test "map preserves length" =
    forall xs : List Natural ->
      List.length (List.map (+1) xs) == List.length xs

  property "reverse is involutive" =
    forall xs : List a ->
      List.reverse (List.reverse xs) === xs

-- Benchmarks
benchmark "fold performance" = {
  baseline: {
    "left fold": measure -> List.fold_left combine empty largeList,
    "right fold": measure -> List.fold_right combine empty largeList
  },

  requirements: {
    "fold under 100ms": time < 100.ms,
    "memory under 10MB": heap_growth < 10.mb
  }
}
```

### 12. Type System Features

Advanced types with categorical grounding.

```catena
-- Row polymorphism
transform getX : {x: Float | ρ} -> Float
transform getX record = record.x

-- Type families
type family Append xs ys where
  Append [] ys = ys
  Append (x:xs) ys = x : Append xs ys

-- Existential types
type AbstractSet a = exists r. {
  rep: r,
  empty: r,
  insert: a -> r -> r,
  member: a -> r -> Bool
}

-- Higher-kinded types
trait DualMapper (p : Type -> Type -> Type) where
  bimap : (a -> c) -> (b -> d) -> p a b -> p c d

-- Multiple constraints using pragmatic names
transform traverse : (Traversable t, Applicator f) =>
  (a -> f b) -> t a -> f (t b)

-- Bundle (Product) and Choice (Coproduct) types
type Bundle a b = Bundle a b  -- Product in CT
type Choice a b = Left a | Right b  -- Coproduct in CT
```

### 13. Immutability and Updates

All data is immutable with structured update syntax.

```catena
-- Update with 'with' keyword
transform birthday : User -> User
transform birthday user =
  user with { age = user.age + 1 }

-- Optics for nested updates (lenses)
transform updateCity : Text -> Person -> Person
transform updateCity newCity =
  Person.address.city.set newCity
```

## Complete Syntax Examples

### Session Types for Protocol Safety

Session types define communication protocols as Systems where each state is a Type and transitions are Transforms.

```catena
-- Define a session protocol
session TwoPhaseCommit =
  !prepare -> ?vote ->
  choice {
    commit -> !finalize -> end,
    abort -> !rollback -> end
  }

-- Implement session
transform coordinator : Session[TwoPhaseCommit] -> Result
transform coordinator session = do
  send session Prepare
  vote <- receive session
  match vote
    | Ready ->
      select session commit
      send session Finalize
      Ok
    | NotReady ->
      select session abort
      send session Rollback
      Aborted
  end
```

### DSL Creation

DSLs are embedded Systems with their own categorical structure.

```catena
-- Define a DSL
dsl MatrixDSL : System where
  -- Grammar for Types
  grammar Matrix where
    syntax "[[ _ ]]" : Matrix where
      [[ a, b; c, d ]] = Matrix 2 2 [[a,b], [c,d]]

  -- Operations (Transforms in the DSL)
  operator (×) = matrix_multiply
  operator (ᵀ) = transpose [postfix]

  -- Laws (categorical properties)
  law transpose_multiply:
    (A × B)ᵀ == Bᵀ × Aᵀ
```

## Memory Diagram

```mermaid
graph TB
    subgraph "Category Theory Foundation"
        CT[System Type System]
        CT --> Types[Types (Objects)]
        CT --> Transforms[Transforms (Morphisms)]
        CT --> Traits[Traits/Type Classes]
        CT --> Mappers[Mappers (Functors)]
        CT --> Adapters[Adapters (Natural Transformations)]
        CT --> Pipelines[Pipelines (Monads)]
    end

    subgraph "BEAM Runtime"
        VM[BEAM VM]
        VM --> Processes[Lightweight Processes]
        VM --> Messages[Message Passing]
        VM --> Supervision[Supervision Trees]
        VM --> HotReload[Hot Code Reloading]
    end

    subgraph "Type System"
        TS[Advanced Types]
        TS --> Row[Row Polymorphism]
        TS --> PV[Polymorphic Variants]
        TS --> TF[Type Families]
        TS --> ET[Existential Types]
        TS --> HKT[Higher-Kinded Types]
        TS --> Bundle[Bundle (Product)]
        TS --> Choice[Choice (Coproduct)]
    end

    subgraph "Effects"
        EFF[Algebraic Effects]
        EFF --> Handlers[Effect Handlers]
        EFF --> Pure[Pure Core]
        EFF --> EffTransforms[EffectfulTransforms (Kleisli)]
        EFF --> IO[I/O Effects]
        EFF --> Conc[Concurrency Effects]
    end

    subgraph "Testing"
        TEST[Testing Framework]
        TEST --> Units[Unit Tests]
        TEST --> Props[Property Tests]
        TEST --> Laws[Law Verification]
        TEST --> Bench[Benchmarks]
    end

    Types --> Processes
    Transforms --> Messages
    Mappers --> Supervision
    Adapters --> HotReload
    Traits --> CT
    Pipelines --> EFF

    TS --> CT
    EFF --> VM
    TEST --> Traits
```

## Key Language Features Summary

### Category Theory Integration (Pragmatic Names)
- **Types and Transforms**: Data structures and functions as Objects and Morphisms
- **Systems**: Categories organizing code structure
- **Traits and Instances**: Type classes with lawful abstractions
- **Mappers (Functors)**: Structure-preserving transformations
- **Pipelines (Monads)**: Composable effectful computations
- **Adapters (Natural Transformations)**: Structure-preserving conversions
- **Extractors (Comonads)**: Dual of Pipelines, extract from context
- **Bundles and Choices**: Products and Coproducts
- **Accumulators (Monoids)**: Types with identity and combination

### Type System
- **Trait System**: General abstraction mechanism with `trait`, `instance`, `:` (inheritance)
- **Higher-Kinded Types**: Type constructors with kind annotations
- **Type Class Constraints**: Using pragmatic names (Comparable, Mapper, Pipeline)
- **Row Polymorphism**: Extensible records and variants
- **Polymorphic Variants**: Open sum types
- **Type Families**: Type-level computation
- **Existential Types**: Abstract data types
- **Session Types**: Protocol-safe communication

### Effects and Purity
- **Algebraic Effects**: First-class effect definitions
- **Effect Handlers**: Flexible effect interpretation
- **EffectfulTransforms (Kleisli Arrows)**: Computations with effects
- **Pure by Default**: Explicit effect annotations
- **Effect Polymorphism**: Generic over effects
- **PipelineIO**: Effect lifting into Pipelines

### Concurrency and Distribution
- **Actor Model**: Type-safe processes with `actor` keyword
- **Process Handlers**: Message handling with `process` keyword
- **Supervision Trees**: Categorical fault tolerance
- **Hot Code Reloading**: Version migration
- **Distributed Types**: Location-transparent computation

### Developer Experience
- **Mandatory Documentation**: Machine-checkable docs with `doc`
- **First-Class Testing**: Tests as language primitives
- **Property Testing**: Automated law verification
- **Pattern Matching**: Advanced patterns with `match`
- **DSL Creation**: Embedded Systems with custom syntax
- **Pragmatic Terminology**: Developer-friendly names for CT concepts

## Pragmatic Terminology Summary

| Pragmatic Name | Category Theory Term | Description |
|----------------|---------------------|-------------|
| **Type** | Object | Data structures and types |
| **Transform** | Morphism | Pure functions between Types |
| **System** | Category | Collection of Types and Transforms |
| **Comparable** | Setoid | Types with custom equality |
| **Combiner** | Semigroup | Associative binary operation |
| **Accumulator** | Monoid | Combiner with identity element |
| **Mapper** | Functor | Lifts Transforms to contexts |
| **Applicator** | Applicative | Mapper with pure and apply |
| **Pipeline** | Monad | Chainable effectful computations |
| **EffectfulTransform** | Kleisli Arrow | Transform that produces effects |
| **PipelineLayer** | Monad Transformer | Stacks pipeline capabilities |
| **Extractor** | Comonad | Extract values from context |
| **Adapter** | Natural Transformation | Convert between type constructors |
| **Bundle** | Product | Composite of two Types |
| **Choice** | Coproduct | One of many possibilities |
| **DualMapper** | Bifunctor | Maps two type parameters |

## Conclusion

Catena represents a unique synthesis of mathematical rigor and practical distributed systems programming. By using **pragmatic terminology** (Types, Transforms, Pipelines) alongside traditional category theory terms, it makes powerful mathematical abstractions accessible to all developers. The language enables you to write code that is simultaneously:

- **Mathematically Sound**: Verified by categorical laws through property testing
- **Intuitively Named**: Using developer-friendly terms like Pipeline and Mapper
- **Practically Useful**: Running on battle-tested BEAM
- **Type Safe**: Catching errors at compile time with advanced type system
- **Concurrent**: Leveraging actors with dedicated `process` handlers
- **Testable**: With built-in testing, property verification, and benchmarking
- **Maintainable**: Through mandatory documentation and clear effects
- **Composable**: Via trait system and Transform composition

The online store example demonstrates how these features work together with pragmatic terminology to create a robust, scalable, and maintainable system that leverages the best of functional programming, category theory, and the BEAM runtime.