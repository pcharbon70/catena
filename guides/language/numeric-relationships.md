# Numeric Relationships: The Closed Set

Revision `0.1.40` fixes how `Int` and `Float` relate across
operators: **closed-set instantiation**. The operands unify with
each other and the operator instantiates over exactly `{Int,
Float}` — the numeric runtime types of the data model. No trait
dispatch, no user overloadability, no defaulting, no implicit
coercion, no literal constraints.

## The rule

| Operator class | Operands | Result |
| --- | --- | --- |
| `add`/`subtract`/`multiply` | same-type member of `{Int, Float}` | the operand type |
| `less`/`less_equal`/`greater`/`greater_equal` | same-type member | `Bool` (unchanged) |
| equality | the comparable set (unchanged) | `Bool` |
| negation | member | the operand type (unchanged) |

Mixed `Int`/`Float` operands are ill-typed everywhere — an
operation expecting one accepts neither the other nor its
literals, unchanged from `0.1.14`.

## What changed at 0.1.40

Arithmetic joins ordering and negation over `Float`: the checker's
inference rule for `add`/`subtract`/`multiply` now instantiates
same-type instead of pinning `Int`. The rule is
**correct-but-dormant** — no frozen frontend carries a float type
or literal spelling, so it cannot be reached from source yet. The
witness drives the inference engine directly:

```elixir
alias Catena.Type.{Infer, Scheme}

expression = %{
  tag: :binary, operator: :add, path: nil,
  left: %{tag: :variable, name: "x", path: nil},
  right: %{tag: :variable, name: "x", path: nil}
}

{_typed, :float, _state} =
  Infer.infer(expression, %{"x" => Scheme.mono(:float)},
    %{next: 100, substitution: %{}})
```

The evaluator's arithmetic computes Elixir floats natively; the
rule becomes input-reachable with the first float-bearing frontend.

## What stays outside

- **Division, remainder, and reserved spellings** — the numeric
  library's own revision (G105): checked and decimal arithmetic,
  division-by-zero classification, truncation and remainder signs.
- **Explicit conversions** — `Int`↔`Float` conversions are explicit
  named operations, G105's; nothing converts implicitly.
- **User overloadability** — no trait, instance, or library
  declaration adds meaning to an operator; a future numeric type
  (decimal, bignum) joins the closed set by amending the
  enumeration in its own revision.

The normative contract is the research repository's
[Numeric Relationships Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/numeric-relationships).
