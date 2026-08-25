# Evaluation Order

Catena 0.1.26 defines when every existing compound form evaluates: one
closed table, observable through effect-request traces.

## The ordered-forms table

| Form | Declared order |
| --- | --- |
| Call (curried) | callee, then arguments left-to-right as repeated unary |
| Tuple / record / constructor fields, variant payloads | written order |
| Record update | base, then replacement value |
| Match scrutinee | once, before any clause test |
| Operation / spawn arguments | written order |
| Send | target first, then message |
| Trap | reason before the trap terminal |
| Binary operator | left, then right, each exactly once |
| `and` / `or` | left; right only when not skipped (the only exceptions) |
| `let` | right-hand side to a value, then substitution |
| Sequence | first to a value, then second |
| Trait call | subject first, then arguments |
| Handler | installation, then body |
| Annotate | transparent |

The kernel's list is elevated verbatim; the typed-core completions —
curried application, trait calls, handler installation, annotate — are
the new content. The clause-level fragments keep their homes in
C002/C003/C004/C005.

## Order is observable

For a program whose subexpressions perform distinguishable effect
requests, a conforming implementation's request trace must equal the
table's:

```elixir
{{:ok, _}, reference} =
  Catena.Effect.Runtime.capture_trace(fn ->
    Catena.Reference.Evaluator.run(core, "main")
  end)

{value, beam} =
  Catena.Effect.Runtime.capture_trace(fn -> apply(module, :main, []) end)

reference == beam  # and both equal the declared order
```

This generalizes C004's trait-traversal rule and C005's observable
handler order; implementations keep every unobservable within-step
freedom.

## The entry rule

A compound form not in the table has no declared order until its own
slice says so — collections, interpolation, and every G040 compound
enter with their entry. Any future exception to a declared order
requires the C029 edition-record gate.

## Current boundary

Binding structure remains G031's; arity and currying as typing remain
G032's; branch forms remain G033's; equality remains P035's; the
failure taxonomy remains G036's; collections and interpolation remain
G040's; cancellation mid-evaluation remains G088's; surface syntax
remains P109's.

The normative contract is the research repository's
[Evaluation Order Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/evaluation-order).
