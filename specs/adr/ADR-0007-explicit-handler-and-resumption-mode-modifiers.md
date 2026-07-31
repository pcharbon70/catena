# ADR-0007: Explicit Handler And Resumption Mode Modifiers

## Status

Accepted.

## Context

[ADR-0006](ADR-0006-first-class-resumptions-and-selective-cps.md)
fixes the meanings and defaults of deep/shallow handlers and
one-shot/multi-shot resumptions, but deliberately defers their opt-in source
spelling. Phase 7 needs one spelling that identifies the delimiter policy
before handler cases bind `with k` resumptions.

The mode belongs to the handler delimiter, not to an individual `resume`
call. Selecting it at `resume` would allow the same authority to be invoked
under conflicting policies. Selecting it on `with k` would duplicate the
same delimiter decision across every operation case and would leave value
handlers without a clear policy.

## Decision

Catena accepts `shallow` and `multi_shot` as explicit modifiers immediately
after `handle`:

```catena
handle shallow multi_shot computation then {
  Choice {
    choose() with k ->
      let left = resume(k, true)
      in resume(k, false)
  }
}
```

Each modifier is optional:

```catena
handle computation then { ... }              -- deep, one-shot
handle shallow computation then { ... }      -- shallow, one-shot
handle multi_shot computation then { ... }   -- deep, multi-shot
handle shallow multi_shot computation then { ... }
```

The parser accepts either modifier order and the formatter emits the
canonical `shallow multi_shot` order. `shallow` and `multi_shot` are reserved
language words with ordinary identifier-boundary behavior. `with k` remains
unchanged; the selected delimiter mode determines the inferred type and
runtime authority of `k`.

Deep and one-shot remain implicit defaults. Catena does not add redundant
`deep` or `one_shot` modifiers in this decision.

The normalized mode inventory is:

```text
HandlerMode {
  depth: Deep | Shallow,
  kind: OneShot | MultiShot,
  origin: SourceOrigin
}
```

Shallow typing retains the selected handled effect in a resumption's
residual row because the frame is absent during resumed execution.

Multi-shot typing is conservative. A resumption is admissible only when its
residual row is closed and empty. Known residual effects and open rows fail
closed because the compiler cannot prove that external state, PIDs, ports,
mailboxes, provider state, or other capabilities have branch semantics.
This rule can be widened only by a later accepted effect-capability
classification decision.

## Consequences

### Positive

- Mode selection is local, explicit, and attached to the delimiter that owns
  the continuation.
- Existing source remains deep one-shot without migration.
- Operation cases and first-class `Resumption` use retain the established
  `with` and `resume` vocabulary.
- Parser, normalized AST, types, control inventories, module interfaces, and
  artifacts can carry one canonical mode representation.
- Multi-shot cannot silently duplicate unknown external authority.

### Negative

- `shallow` and `multi_shot` are no longer available as ordinary identifiers.
- The initial multi-shot admissibility rule rejects some computations that
  could be safe under a future effect-capability system.
- Shallow effect inference is intentionally conservative when continuation-
  precise effect information is unavailable.

### Neutral

- This decision selects syntax and static policy; runtime context restoration,
  branch execution, and resource budgets remain implementation obligations of
  Phase 7.
- The order written by a user is not preserved by formatting; the semantic
  mode is preserved.

## Alternatives Rejected

### Annotate `resume`

`resume shallow(k, value)` or `resume multi_shot(k, value)` makes invocation
sites appear able to override authority fixed at capture time.

### Annotate Every `with` Binder

`with multi_shot k` repeats a delimiter-wide choice across cases and provides
no natural policy for automatically resumed value handlers.

### Use A Generic Attribute Bag

`handle @[depth: shallow, kind: multi_shot] ...` is extensible but introduces
an attribute surface solely for two fundamental control modes and weakens
grammar-level diagnostics.

### Add Explicit Default Modifiers

Allowing `handle deep one_shot ...` adds spellings without adding behavior and
creates unnecessary normalization and migration cases.

## Related Documents

- [ADR-0003: Explicit Effect Context Runtime](ADR-0003-explicit-effect-context-runtime.md)
- [ADR-0006: First-Class Resumptions Through Effect-Directed Selective CPS](ADR-0006-first-class-resumptions-and-selective-cps.md)
- [Delimited Resumption Architecture](../compiler/delimited_resumption_architecture.md)
- [Phase 7 Plan](../planning/delimited-resumptions/phase-07-shallow-handlers-and-multi-shot-resumptions.md)
