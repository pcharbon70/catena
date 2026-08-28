# Failure Taxonomy

Catena 0.1.32 defines runtime failure: one outcome, kinded reasons.

## The single outcome

```text
outcome ::= value | trap ( reason )
```

`trap(reason)` is *the* runtime failure outcome. The three-way
partition: values (normal termination), traps (failure), running
(divergence — not failure). No second outcome class exists, and none
may arrive except by amending this contract.

## What a trap does (kernel-verbatim)

- discards its mailbox;
- sends no exit signal;
- affects no spawner;
- is unobservable through Catena handles;
- cannot be intercepted by any handler or match.

On BEAM, a trap raises `{:catena_trap, reason}` — the reason value's
identity is the executable claim, agreeing with the stepper.

## The six categories

| Category | Classification |
| --- | --- |
| Explicit panic/crash | the kernel `trap` expression — the only user-invoked failure |
| Typed failure (`Option`/`Result`) | a **value**, not a failure — G105's types return |
| Arithmetic faults | reserved — enters with the first faulting operator |
| Failed assertions | reserved — enters with the first assert form |
| Foreign exceptions | reserved — G095/G096, a raise maps to `trap(reason)` |
| VM termination | operational, outside program semantics (G084/G092/G121) |

## The entry rule

Every arriving failure producer classifies its failures as
`trap(reason)` in its admitting slice. No producer may add a second
outcome class; no producer may arrive unclassified.

## Current boundary

Library contents remain G105's; foreign calls G095/G096's; process
death and signals G084's; VM termination G092/G121's; cancellation
G088's (distinct from failure); failure-path observability G037's;
spellings P109's.

The normative contract is the research repository's
[Runtime Failure Taxonomy Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/runtime-failure-taxonomy).
