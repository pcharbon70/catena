# Selective Receive

Revision `0.1.46` closes P086: the selective-receive rule set is
fixed at the language level, and the four connections that need
other owners are stated as routed interfaces.

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

A receive whose clauses reject a prefix starves while that prefix
stands. Each attempt's scan cost is proportional to its rejected
prefix; a stable prefix is re-examined by every subsequent
attempt. No fairness guarantee beyond scan order is claimed.

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
[Selective Receive Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/selective-receive).
