# Resource Observability

Catena 0.1.33 defines what programs may observe of resources: almost
nothing, by design.

## The six-way classification

| Category | Program observability |
| --- | --- |
| Allocation addresses | none |
| Sharing (record maps, message copy vs alias) | none — semantic identity |
| Garbage collection | none |
| Object identity | process identity only |
| Stack use | completion vs the tail guarantee only |
| Finalization | none — declared absent, gated |

## Semantic identity

```text
Equal values are interchangeable. Copy or share, boxed or unboxed,
moved or pinned — representation never changes meaning.
```

This is what makes `equal` complete (no `eq` beside it), what buys
the compiler its whole freedom budget (sharing, unboxing,
deduplication, CPS, GC movement), and what keeps programs
deterministic and portable.

## The two-clause identity rule

1. **Process identity is the only identity-bearing value**: fresh
   per spawn, observable only through the kernel's handle
   operations (`send`, `self`), never comparable.
2. **Every other value has semantic identity only**: closure
   allocation identity, record sharing, and message copying are
   unobservable.

## Finalization

No destructor, finalizer, or cleanup form exists; any arrival goes
through the resource-scope era (G080s/G084) or the foreign boundary
(G095), each shipping its own semantics.

## Debugging observes the implementation

The rules constrain what *programs* observe; tools — debuggers,
tracers — observe the implementation from outside program semantics,
consuming the deterministic anchors the language provides
(effect-request traces, trap reasons, process identities). G124 owns
the tooling.

## Current boundary

Handle operations beyond the kernel's remain G084's; message-copy
details G085's; resource scopes and cleanup the G080s era's; foreign
finalization G095's.

The normative contract is the research repository's
[Resource Observability Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/resource-observability).
