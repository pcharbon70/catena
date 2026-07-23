# Catena Targets

## Current Repo Status

Based on the current `specs/` set, codebase, and test suite, the current promoted status is:

- Proof-of-concept Phase 1 Sections 1.1 through 1.4 are materially implemented.
- Proof-of-concept Phase 1.5 is partially reconciled:
  - 1.5.1, 1.5.2, and 1.5.3 are complete.
  - 1.5.4 is implemented for concrete suites.
  - 1.5.5 is implemented through do-notation parsing/desugaring.
  - 1.5.6 is mostly implemented through effect/Kleisli integration work.
  - 1.5.7 operator desugaring is implemented.
- Phase 2 work exists in code and summaries for the REPL, prelude, testing framework, and integration tests.
- Phase 3 parser-native patterns now reach executable Core Erlang through the
  public typed compiler path. Decision-tree and exhaustiveness/redundancy
  surfaces exist and are tested separately rather than being implied as
  default pipeline stages.
- Phase 4 is not complete, but a minimal import-resolution bridge exists.
- A newer algebraic-effects track is materially implemented through orchestration, row-polymorphism integration, validation, and integration-test surfaces associated with Phases 7 through 14.
- The internal property-testing framework has completed the rose-tree foundation for Phase 1.1, the generator/seed/size layer for Phase 1.2, the categorical generator layer for Phase 1.3, the primitive generator layer for Phase 1.4, and the range layer for Phase 1.5; the next major step is basic shrinking infrastructure.

## Immediate Targets

- reconcile canonical specs and planning state around the implemented compiler/runtime/library surfaces
- consolidate the promoted description of the implemented algebraic-effects machinery so specs, contracts, and planning stop treating it as purely deferred work
- finish the remaining Phase 1.5 library-validation gaps
- decide whether to resume the proof-of-concept track or continue the property-testing framework track next
- resolve the published 122-failure baseline phase by phase while preserving
  complete `rebar3 eunit` discovery
- continue migrating historical PropEr coverage into the internal
  property-testing architecture without hiding active-suite failures

## Medium-Term Targets

- full module-system completion
- actor model integration on top of the existing effect/runtime foundation
- broader front-end coverage and ergonomics for the already-implemented algebraic-effects system
- generator, runner, and law-discipline layers for the internal property-testing framework

## Non-Goals Of This Specs Layer

- replacing code- and test-backed component detail with a second parallel status layer
- pretending that all planning checklists are already reconciled
- freezing experimental research areas that are still clearly exploratory
