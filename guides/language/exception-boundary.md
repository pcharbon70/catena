# Exception Boundary

Revision `0.1.47` closes G081: exceptions are **several visibly
distinct mechanisms** — and no language exception form.

## The partition

| Class | Mechanism | Caught by |
| --- | --- | --- |
| Typed failure | an ordinary value (`Option`/`Result`-shaped; contents G103's) | ordinary code |
| Exception-style control | **the effect pattern**: a handler declining to resume aborts to its result — visible in the effect row | an enclosing handler |
| Fatal failure | `trap(reason)` — kinded, local, **never catchable** | nothing |

No construct blurs the classes or converts one into another
silently. Converting an abort to a trap is a handler's explicit
choice; converting a trap to a value is impossible.

## The pattern, witnessed

```elixir
# The operation clause binds the resumption and never uses it:
(operation ask (params (value Int)) (resume next) 0)

# main requests ask 99 under Fallback → aborts to 0, not 100.
{:ok, core} = Catena.check_kernel(source)
{:ok, 0, %{root_status: :terminated}} =
  Catena.Kernel.Stepper.run(core, "main")   # BEAM agrees: apply(m, :main, []) == 0
```

This is a library idiom over unchanged C005 semantics — the
statement adds no rule.

## Panics

A programmer panic **is** a `trap` carrying the reserved
assertion/panic kind, entering with its producer under C036's
per-producer gate. No separate construct; no producer ships the
kind yet.

## Routing

Process exits → G084 (C010's local-trap/spared-spawner stands).
Foreign failures → `trap(reason)` at the visible boundary
(G095/G096, C067's rule). Cancellation → G088. Library faults →
G105. Outcome types → G103. A language exception form arrives
only through C044's reopening door: first reopen C036's taxonomy,
then state catch semantics, visibility, and evidence interaction.

The normative contract is the research repository's
[Exception Boundary Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/exception-boundary).
