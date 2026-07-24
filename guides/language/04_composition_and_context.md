# 4. Composition and computational context

So far, composition has meant feeding an ordinary value from one pure
transform into the next. That works until a step may have no value, may fail,
or may produce several values. Those situations add a **context** around the
result:

- `Maybe a` means there may be an `a`.
- `Result a e` means there is either an `a` or an error `e`.
- `List a` means there may be many `a` values.

The context is not incidental packaging. It changes how computations connect.
Catena's standard library describes those connection rules with traits such as
`Mapper`, `Applicator`, `Chainable`, and `Pipeline`.

Parcel Relay now needs validation. A shipping quote should not be computed
from a zero weight or an unsupported route. Instead of throwing an untyped
exception, validation will return a value that explicitly represents success
or failure.

## Higher-order transforms

A higher-order transform receives or returns another transform. It lets a
reusable piece of code describe *where* behavior belongs while its caller
supplies *what* the behavior is.

This simple helper applies an adjustment to a quote:

```catena
transform adjust_quote : (Int -> Int) -> Int -> Int
transform adjust_quote adjustment quote =
  adjustment quote
```

The first pair of parentheses belongs to the type:
`(Int -> Int)` is one argument, itself a transform. We can pass a named
transform:

```catena
transform add_insurance amount = amount + 3

transform insured_total total =
  adjust_quote add_insurance total
```

Or use a lambda for a one-off rule:

```catena
transform weekend_total : Int -> Int
transform weekend_total total =
  adjust_quote (fn amount -> amount + 2) total
```

Higher-order transforms are the foundation of `map`, `apply`, and `chain`.

## Mapping preserves the context

Mapping applies a pure transform to values inside a context without changing
the context's shape.

For a `Maybe`, `None` stays `None` and `Some value` becomes
`Some (f value)`. For a list, each element is transformed and the list shape
is preserved. For a `Result`, an error stays an error and a successful value
is transformed.

With the `Prelude` imported, Parcel Relay can add insurance only when a quote
exists:

```catena
import Prelude

transform add_insurance amount = amount + 3

transform insure_optional_quote quote =
  map add_insurance quote
```

The symbolic form uses `<$>`:

```catena
transform insure_optional_quote : Maybe Int -> Maybe Int
transform insure_optional_quote quote =
  add_insurance <$> quote
```

Both forms mean `map add_insurance quote`. The operator does not unwrap a
`Maybe`, invent a missing value, or discard a list. It changes values while
preserving structure.

This is the practical meaning of `Mapper`—called *Functor* in category theory.

## Modeling validation with `Result`

Define errors as ordinary data:

```catena
type QuoteError =
  MissingWeight
  | UnsupportedZone
```

The prelude's `Result a e` has two constructors: `Ok a` and `Err e`.
Validation can now state its failure in the return type:

```catena
transform validate_weight : Int -> Result Int QuoteError
transform validate_weight 0 = Err MissingWeight
transform validate_weight weight = Ok weight
```

The zone check follows the same shape:

```catena
transform validate_zone : Zone -> Result Zone QuoteError
transform validate_zone International = Err UnsupportedZone
transform validate_zone zone = Ok zone
```

No hidden exception channel is involved. A caller has to account for `Ok` and
`Err`, either with pattern matching or with contextual composition.

## Chaining dependent steps

Mapping is enough when the supplied transform returns a plain value. Validation
steps return another `Result`, so mapping them would produce a nested shape
such as `Result (Result Zone QuoteError) QuoteError`.

**Chaining** flattens that situation. It runs the next transform only for a
successful value and propagates the existing context otherwise.

In Catena's prelude, `chain` takes the next transform first and the contextual
value second:

```catena
transform validated_fee : Int -> Result Int QuoteError
transform validated_fee weight =
  chain
    (fn valid_weight -> Ok (5 + valid_weight + valid_weight))
    (validate_weight weight)
```

The `>>=` operator writes the contextual value first:

```catena
transform validated_fee : Int -> Result Int QuoteError
transform validated_fee weight =
  validate_weight weight
  >>= fn valid_weight -> Ok (5 + valid_weight + valid_weight)
```

If the weight is `0`, `validate_weight` returns `Err MissingWeight` and the
lambda is skipped. For any other weight, the lambda receives the unwrapped
value and produces the next `Result`.

The crucial difference is:

```text
map   : (a -> b)   -> context a -> context b
chain : (a -> context b) -> context a -> context b
```

Use `map` when the next step cannot introduce a new contextual failure. Use
`chain` when the next step itself returns that context.

## `do` notation

Several dependent steps can become visually noisy as nested `chain` calls.
`do` notation is surface syntax for the same structure. A block uses braces
and semicolons:

```catena
transform validate_parcel : Int -> Zone -> Result Int QuoteError
transform validate_parcel weight zone =
  do {
    valid_weight <- validate_weight weight;
    valid_zone <- validate_zone zone;
    Ok (5 + valid_weight + valid_weight + zone_fee valid_zone)
  }
```

A binding such as:

```text
valid_weight <- validate_weight weight
```

means “chain this contextual result; if it succeeds, bind its value and
continue.” The final expression is already in the context, so it is returned
as the block's result.

The compiler desugars the block into `chain` calls and lambdas before type
checking later normalized forms. `do` does not introduce mutation or an
imperative escape hatch; it is a readable notation for contextual
composition.

A pure local binding inside a block uses `let` without `in`:

```catena
transform quote_with_local : Int -> Result Int QuoteError
transform quote_with_local weight =
  do {
    valid_weight <- validate_weight weight;
    let base = 5;
    Ok (base + valid_weight + valid_weight)
  }
```

## `Pipeline` and its supporting traits

The standard library separates several useful capabilities:

- `Mapper` can transform values while preserving a context.
- `Applicator` can place a value in a context with `pure` and apply independent
  contextual arguments.
- `Chainable` can sequence a step whose next computation depends on the
  previous result.
- `Pipeline` combines `Applicator` and `Chainable` and supplies derived
  operations such as `join` and `sequence`.

These names describe programming capabilities first. Their conventional
category-theory names are Functor, Applicative, Chain, and Monad.

The distinction is useful because a type should promise only the operations
whose laws it can satisfy. Code that only needs `map` should ask for `Mapper`,
not the stronger `Pipeline`.

## Ordinary pipes versus contextual chains

The two operators solve different problems:

```catena
value |> pure_transform
```

passes a plain value to a plain transform.

```catena
contextual_value >>= contextual_transform
```

passes a successful inner value to a transform that returns the same kind of
context.

For Parcel Relay:

```catena
shipping_fee 4 30 |> add_insurance
```

is an ordinary pricing pipeline.

```catena
validate_weight 4 >>= fn weight -> Ok (shipping_fee weight 30)
```

is a failure-aware pipeline.

Choosing the operator follows from the types, not visual preference.

## The current implementation boundary

`Maybe`, `Either`, `Result`, `List`, the standard traits, operator parsing, and
`do` desugaring are implemented language and standard-library surfaces. The
artifact backend executes concrete local and imported trait dictionaries, and
has source-to-BEAM evidence for `<$>`, `<*>`, and `>>=` when a concrete
instance is selected.

The shipped `Prelude` is currently a typed/source library, not a complete
executable BEAM provider: its default `Pipeline.join` refers to an unresolved
`id` implementation. Consequently, the `import Prelude` snippets in this
chapter teach the current language and library semantics but are not complete
standalone artifact examples. Self-contained modules and closed source sets
with concrete dictionaries are executable; application artifact generation
fails closed if an imported provider cannot be linked.

## What to remember

- A context such as `Maybe`, `Result`, or `List` changes how transforms
  compose.
- Higher-order transforms receive or return transforms.
- `map` and `<$>` preserve context while changing contained values.
- `chain` and `>>=` sequence dependent contextual work.
- `do { ... }` is readable syntax that desugars to `chain` and lambdas.
- `|>` composes ordinary values; `>>=` composes contextual results.
- Catena's pragmatic trait names correspond to established algebraic
  abstractions, but you can understand them from their behavior.

As an exercise, add `InvalidDistance` to `QuoteError`, validate distance before
pricing, and express the three-step workflow once with `>>=` and once with
`do`.

Previous: [Types and pattern matching](03_types_and_patterns.md)

Next: [Traits, instances, and laws](05_traits_instances_and_laws.md).
