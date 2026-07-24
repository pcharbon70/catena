# 2. Values and transforms

The first Parcel Relay quote used one number and one arithmetic expression.
Real pricing rules have more pieces: weight, distance, insurance, and local
names for intermediate results. Catena handles these with expressions,
immutable `let` bindings, multi-argument transforms, lambdas, and composition.

The important mental shift is that everything produces a value. A `let`
binding does not create a mutable storage cell, and a transform body does not
need an explicit `return`. The value of the final expression is the result.

## Literal values and expressions

The current grammar includes integer, floating-point, and string literals,
along with lists, tuples, and records. Boolean values such as `true` and
`false` come from the language environment. Arithmetic and comparisons build
larger expressions from those values.

Common operators include:

| Purpose | Operators |
| --- | --- |
| Arithmetic | `+`, `-`, `*`, `/` |
| Comparison | `==`, `/=`, `<`, `>`, `<=`, `>=` |
| Boolean logic | `&&`, `||` |
| Lists | `++`, `::` |
| Forward pipe | `|>` |

Precedence follows the familiar order: multiplication and division bind more
tightly than addition and subtraction. Parentheses make intent explicit when
an expression would otherwise be hard to scan.

Parcel Relay can calculate a fee from both weight and distance:

```catena
transform shipping_fee : Int -> Int -> Int
transform shipping_fee weight distance =
  5 + weight + weight + distance
```

Function application uses whitespace, so a quote for weight `4` and distance
`30` is written:

```catena
shipping_fee 4 30
```

There are no commas or parentheses around ordinary function arguments.
Parentheses are still useful to group a nested expression:

```catena
shipping_fee (2 + 2) 30
```

## Immutable local bindings

Names help explain a calculation. A `let` expression binds a value for the
expression after `in`. The binding cannot be reassigned.

The pricing rule becomes easier to read when each charge has a name:

```catena
transform shipping_fee : Int -> Int -> Int
transform shipping_fee weight distance =
  let weight_charge = weight + weight in
  let distance_charge = distance in
  5 + weight_charge + distance_charge
```

Each `let ... in ...` is itself an expression. The scope of
`weight_charge` is everything after its `in`; the scope of
`distance_charge` begins at the next `in`. The final arithmetic expression is
the transform's result.

This structure makes data dependencies visible. `distance_charge` cannot
secretly modify `weight_charge`, and another caller cannot observe a
half-computed quote.

## Records for related values

A record groups named fields. Record fields are accessed with a dot. At this
stage we can use a structural record type without introducing a named parcel
type yet.

```catena
transform quote : {weight: Int, distance: Int} -> Int
transform quote parcel =
  let weight_charge = parcel.weight + parcel.weight in
  let distance_charge = parcel.distance in
  5 + weight_charge + distance_charge
```

A matching record value looks like this:

```catena
{weight: 4, distance: 30}
```

Structural records are useful for small local shapes. In the next guide we
will give the domain a named algebraic data type so that a parcel cannot be
confused with any unrelated record that happens to have the same fields.

## Small transforms compose better

A long expression can be correct and still be difficult to change. Pure helper
transforms let each pricing rule have one job:

```catena
transform weight_charge weight = weight + weight

transform distance_charge distance = distance

transform subtotal weight distance =
  5 + weight_charge weight + distance_charge distance

transform add_insurance amount = amount + 3
```

The signature tells us which helpers can connect. `subtotal` eventually
produces an `Int`, and `add_insurance` accepts an `Int`, so the output of
the first can flow into the second.

## Forward piping

The forward pipe makes that flow read from left to right:

```catena
transform insured_quote : Int -> Int -> Int
transform insured_quote weight distance =
  subtotal weight distance
  |> add_insurance
```

Conceptually:

```text
value |> next
```

means “send `value` to `next`.” It is equivalent to:

```catena
add_insurance (subtotal weight distance)
```

Piping is most useful when each stage accepts one value. It helps the source
mirror the story a developer tells: calculate the subtotal, then add
insurance.

The pipe, named top-level calls, forward references, recursion, and
higher-order calls are all part of the executable subset. The explicit nested
form and the piped form compile with the same left-to-right meaning.

## Partial application

Because multi-argument transforms are curried, giving only some arguments
produces a new transform. We can define a standard parcel weight without
repeating it:

```catena
transform shipping_fee weight distance =
  5 + weight + weight + distance

transform quote_four_unit_parcel = shipping_fee 4
```

`shipping_fee 4` has the remaining type `Int -> Int`. It waits for a
distance. Partial application is not a special syntax feature; it follows from
the right-associated function type described in the previous guide.

## Lambdas for one-off behavior

A lambda is an unnamed transform. The current surface uses
`fn parameter -> expression`:

```catena
transform add_weekend_fee : Int -> Int
transform add_weekend_fee amount =
  (fn fee -> fee + 2) amount
```

Named transforms are clearer for stable domain rules. Lambdas shine when a
short behavior is passed to a higher-order transform and does not deserve a
module-level name. We will use them heavily when mapping and chaining in the
next chapters.

## Type inference and explicit signatures

Catena's type checker uses Hindley–Milner-style inference. It can infer many
local types from literals, applications, and surrounding constraints. Public
transforms still benefit from signatures because signatures:

- document the intended contract,
- catch accidental changes at the boundary,
- make effect information visible later, and
- give readers an entry point before they inspect the implementation.

During exploration, this is valid language surface:

```catena
transform double value = value * 2
```

For a public Parcel Relay rule, prefer:

```catena
transform double : Int -> Int
transform double value = value * 2
```

## What to remember

- Expressions evaluate to values; the final expression is the result.
- `let name = value in body` introduces an immutable local name.
- Application uses spaces and is left-associative.
- Function arrows associate to the right, enabling partial application.
- `value |> transform` makes compatible stages read left to right.
- Lambdas express short, one-off transforms.
- Type inference reduces noise, while explicit public signatures clarify
  contracts.

As an exercise, add `add_priority_fee : Int -> Int` and build a quote
that applies both insurance and priority fees with pipes.

Previous: [Orientation and your first transform](01_orientation.md)

Next: [Types and pattern matching](03_types_and_patterns.md).
