# Recursion and Termination

Catena 0.1.31 defines the recursion stance: program recursion is
unrestricted, and every meta-level evaluator is total-or-bounded.

## The stance

```text
Any named definition may recurse. General recursion may reduce
forever. Divergence is non-termination — never a trap, never
undefined behavior.
```

- Tail recursion keeps the proper-tail-call guarantee — the **only**
  stack promise.
- Non-tail recursion consumes stack without bound and is nevertheless
  legal: a 10,000-deep recursive sum is a conforming program.
- No expression-level totality checking exists or is planned; any
  future termination checker enters as an edition-record-gated
  **opt-in analysis** — a tool that reports, never a rule that
  rejects.

## The separation table

| Meta evaluator | Regime | Home |
| --- | --- | --- |
| Conditions (guards) | acyclic first-order; recursion is `CND004` | C003 |
| Specification examples | fixed 20,000-step pure checker | C006 |
| Laws and samples | bounded law checks and bounded samples | C004 |
| Compile-time evaluation | must ship total-or-bounded | G038 (gated) |

## The entry rule

Any recursive-total fragment — recursive conditions, law evaluators,
compile-time evaluation — enters only through a slice that proves its
totality or fixes its budget **in the same change**. No meta-level
evaluator may arrive unbounded.

## Current boundary

Compile-time evaluation design remains G038's, under the gate; syntax
remains P109's; the failure taxonomy remains G036's (divergence
explicitly outside it); process-loop termination beyond the kernel's
receive clause remains G084's; cancellation remains G088's.

The normative contract is the research repository's
[Recursion and Termination Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/recursion-and-termination).
