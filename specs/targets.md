# Catena Targets

## Current Repo Status

Based on `notes/planning/`, `notes/summaries/`, `notes/reviews/`, and the codebase itself, the current promoted status is:

- Proof-of-concept Phase 1 Sections 1.1 through 1.4 are materially implemented.
- Proof-of-concept Phase 1.5 is partially reconciled:
  - 1.5.1, 1.5.2, and 1.5.3 are complete.
  - 1.5.4 is partial.
  - 1.5.5 is implemented through do-notation parsing/desugaring.
  - 1.5.6 is mostly implemented through effect/Kleisli integration work.
  - 1.5.7 operator desugaring is implemented.
- Phase 2 work exists in code and summaries for the REPL, prelude, testing framework, and integration tests.
- Phase 3 work exists in code and summaries for advanced patterns, decision trees, exhaustiveness/redundancy checking, and pattern integration tests.
- Phase 4 is not complete, but a minimal import-resolution bridge exists.
- The internal property-testing framework has completed the rose-tree foundation for Phase 1.1; the next major step is generator/seed management.

## Immediate Targets

- reconcile canonical specs and planning state around the implemented compiler/runtime/library surfaces
- finish the remaining Phase 1.5 library-validation gaps
- decide whether to resume the proof-of-concept track or continue the property-testing framework track next
- continue migrating historical PropEr coverage into the internal property-testing architecture now that the PropEr compile-path blockage is removed

## Medium-Term Targets

- full module-system completion
- actor model integration on top of the existing effect/runtime foundation
- effect polymorphism and advanced effect-system completion
- generator, runner, and law-discipline layers for the internal property-testing framework

## Non-Goals Of This Specs Layer

- replacing the detailed historical record in `notes/`
- pretending that all planning checklists are already reconciled
- freezing experimental research areas that are still clearly exploratory
