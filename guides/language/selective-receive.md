# Selective Receive

Revision `0.1.49` corrects the inconsistent starvation claim in the retained
`0.1.46` contract. Historical selections and executable format versions remain
unchanged; public syntax and the routed interfaces remain separate work.

## The rules

1. **Scan order** — a receive attempt scans its mailbox from the
   oldest message toward the newest.
2. **Preservation** — a rejected message remains queued, in
   position; scanning continues.
3. **One-time removal** — the selected message is removed exactly
   once, before its body runs.
4. **Typing** — one explicit closed message type; clauses are
   pattern-typed against it.
5. **Effects** — the receive form itself performs none; clause
   bodies carry their own rows.
6. **Conditions** — the portable native set only; or-pattern
   expansion rejects `CND006` where condition sharing would be
   required.

## Starvation, honestly

A rejected prefix does not prevent selecting a later match. With no matching
message, including an empty mailbox, the receive waits and preserves every
message. A message that never matches can remain queued while later messages
are consumed. No scheduler fairness or bounded waiting time is promised.
Abstract scan work counts candidates and clauses actually examined; the
contract does not require a rescan on every wakeup or promise wall-clock cost.

The completion fixture selects `2` then `1` from `[0, 2, -1, 1, 3]` and
observes `[0, -1, 3]` remaining on both the reference stepper and live BEAM.
The following older fixture demonstrates no-match waiting only:

```elixir
# The preservation witness: both messages stay queued, in order,
# while the receiver waits.
{:quiescent, outcome} = Catena.Kernel.Stepper.run(core, "main")
holder = Enum.find(outcome.processes, &(&1.name == "Holder"))
holder.status    #=> :waiting
holder.mailbox   #=> [Some 0, Some 1]
```

## The routed interfaces

| Connection | Owner | Obligation |
| --- | --- | --- |
| Public syntax | P109 | realize the rule set; the timeout clause is C044's explicit total fallback |
| Timeouts, cancellation | G088 | timeout evaluation order, races, totality, cancellation disposal |
| Typed protocols | G087 | protocol typing composes with the closed-message-type rule |
| Send-side semantics | G085 | everything beyond C010's order-and-content preservation |

The normative contract is the research repository's
[Selective Receive Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/selective-receive-correction).
