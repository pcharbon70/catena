# Pattern Contexts: Refutability by Context

Every syntactic position that binds by pattern belongs to exactly one
of three classes, and the class fixes what happens on mismatch. The
classes are revision `0.1.38`'s whole contribution: no grammar
changes, no new diagnostics — just the rule that no context anywhere
inherits an implicit runtime match failure.

## The three classes

| Class | Rule |
| --- | --- |
| **Exhaustive** | patterns must cover the scrutinee type; `M001` on a missing witness, `M002` on a useless row |
| **Irrefutable-only** | patterns must be proved total for the bound type, or the form rejects |
| **Explicit-failure** | the construct visibly names its mismatch behavior (for example a filtering generator's mismatch-as-skip) |

Match is the only exhaustive context. Everything else — `let`
binders, function parameters, handler clauses — is irrefutable-only
on arrival, and today all of them are plain names anyway.

## What exists today

```elixir
{:ok, core} = Catena.check_kernel(source)
{:ok, 12, %{root_status: :terminated}} =
  Catena.Kernel.Stepper.run(core, "main")
```

Match programs elaborate, check, and run unchanged; a `let` whose
binder is not a plain value name rejects with `SYN002` ("let binding
must be an unquoted name"); the JSON-AST `let` keeps its `"name"`
binder; handler operation clauses bind plain parameters plus the
resumption binder.

## A refutable destructure has three honest spellings

1. Prove it total (the usefulness relation decides — C045).
2. Make the failure a value: return `Option` or another total type.
3. Make the selection visible: use `match`, or an explicit
   filtering form when comprehensions arrive.

## Reserved and excluded

- **Generators**: ordinary generators require total patterns;
  filtering generators explicitly request mismatch-as-skip. The
  principle is fixed; grammar, effects, and lowering belong to the
  comprehension slices.
- **Public receives**: none exists yet (the receive in the kernel is
  C003's typed lowering harness). On arrival: exhaustive over the
  message type or an explicit total fallback, in its own slice.
- **Exception clauses**: excluded — `trap` is terminal and typed
  failure is a value (C036); no exception mechanism exists or is
  planned.
- **Programmable patterns** (view, synonym, active): excluded;
  any arrival is its own slice stating effects, totality, coverage,
  evaluation count, and cost.

## Current boundary

Match-clause coverage and redundancy remain C045's; `let` structure
remains C031's; handler structure remains C005's; the failure
taxonomy remains C036's; generator grammar remains the comprehension
program's; spellings remain P109's.

The normative contract is the research repository's
[Pattern Contexts Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/pattern-contexts).
