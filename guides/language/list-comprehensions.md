# List Comprehensions: The Dormant Boundary

Revision `0.1.39` fixes the complete comprehension contract — and its
executable surface is a dormant elaboration boundary, not syntax: no
frozen frontend carries comprehension expressions, so
`Catena.Comprehension.elaborate/1` maps a qualifier tree to a kernel
module until the surface-grammar capstone adopts the tokens.

## The form

```text
for pattern in source qualifier* yield expression
```

| Role | Form | Rule |
| --- | --- | --- |
| Ordinary generator | `pattern in source` | pattern must be total for the element type |
| Filtering generator | `case pattern in source` | mismatch alone skips the element |
| Boolean filter | `when expr` | `expr : Bool`; `false` skips; effects visible |
| Local binding | `let pattern = expr` | pattern must be total (C044's rule) |
| Result | `yield expr` | one, last, produces every element |

Sources are `List A`; results are `List B`; production is eager and
ordered; execution is sequential and depth-first left-to-right.

## The elaborator

```elixir
{:ok, source, advisories} =
  Catena.Comprehension.new(
    module: "C047Map",
    origin: "test://c047/map",
    context: [{"xs", "(List Int)", "(construct Cons 1 (construct Cons 2 (construct Nil)))"}],
    qualifiers: [
      {:generator,
       [pattern: "(bind x)", element_type: "Int", source: "(var xs)",
        binds: [{"x", "Int"}]]}
    ],
    yield: "(add (var x) 1)",
    result_element_type: "Int"
  )
  |> Catena.Comprehension.elaborate()

{:ok, core} = Catena.check_kernel(source)
{:ok, [2, 3], %{root_status: :terminated}} = Catena.Kernel.Stepper.run(core, "main")
```

The generated module declares its own `List` nominal type, one fused
tail-recursive worker per generator depth sharing one accumulator, and
a final ordering pass — no intermediate map/filter lists. Values,
failures, and effect rows agree on the stepper and compiled BEAM.

## Diagnostics

| Family | Meaning |
| --- | --- |
| `LCP001` | name rebound in the same comprehension |
| `LCP002` | filtering pattern that can never match |
| `LCP003` | filtering marker unnecessary (pattern already total) |

Non-total ordinary generators and refutable `let` bindings reuse the
non-exhaustive-match family (`M001`); non-list sources and non-`Bool`
filters reuse the typing families; unused bindings reuse `BS001`.

## What stays outside

Iterators, streams, effectful producers, and generic foldable
sources; lazy production and infinite inputs; parallel traversal;
map, set, binary, and arbitrary `Applicative`/`Monad` targets; zip,
ranges, and neighboring iteration syntax (D059). Token-level
punctuation, layout, and block forms are the surface capstone's.

The normative contract is the research repository's
[List Comprehensions Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/list-comprehensions).
