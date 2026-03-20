# Property Testing Framework

## Status

Promoted status: in progress. The rose-tree foundation is implemented and tested, while generator/seed management and fuller runner integration remain the next active step.

## Design Anchors

- [Property testing Phase 1 plan](../../notes/planning/property-testing/phase-01.md)
- [Rose tree completion summary](../../notes/summaries/rose-tree-unit-tests-complete-2025-11-29.md)
- [Phase 2.3 testing summary](../../notes/summaries/phase-2.3-testing-summary.md)
- `src/proptest/catena_tree.erl`
- `test/proptest/catena_tree_tests.erl`
- `src/testing/catena_generators.erl`
- `src/testing/catena_test_runner.erl`

## Current Promoted Surface

- The new property-testing track is real implementation work, not only planning.
- `catena_tree` is the current canonical implemented surface for the new framework.
- The older Phase 2.3 property-test execution path still exists and remains the active runner for first-class `property` declarations.
- The repo is in transition from older/simple generation toward a more principled internal framework with integrated shrinking.

## Acceptance Criteria

### AC-PROP-001 Rose Tree Foundation

The promoted implemented core of the new framework is the rose-tree data structure in `src/proptest/catena_tree.erl`, including:

- lazy child generation
- root and child accessors
- unfold support
- comonad-oriented operations
- functor support
- applicative support
- monad support

This is the current completed unit of work for the new framework.

### AC-PROP-002 Reconciled Project Status

The promoted current status for the property-testing roadmap is:

- Phase 1 Section 1.1 rose-tree foundation: complete
- next step: Phase 1 Section 1.2 generator type and seed management

The planning checklist has not yet been reconciled to that implemented state, so this spec uses the later summary and code as the current authority.

### AC-PROP-003 Transitional Coexistence

Until the new framework is fully wired through the language and tooling, Catena currently has two valid testing-related layers:

- the existing test/property declaration runner in `src/testing/*`
- the new foundational property-testing implementation in `src/proptest/*`

The promoted spec must describe that coexistence honestly instead of pretending the migration is already complete.

### AC-PROP-004 Internal Replacement Direction

The canonical direction is internal property testing with integrated shrinking, not ongoing dependence on PropEr as Catena's lasting architecture. Migration work should therefore move toward:

- Catena-owned generators
- Catena-owned shrinking
- Catena-owned runner/reporting semantics

### AC-PROP-005 Known Gap Visibility

Documentation and tooling specs must continue to note that the PropEr transition is not yet fully reconciled at the repo-wide test-entry-point level. This prevents the specs from hiding the current workflow gap while the replacement architecture is still landing.

## Out Of Scope

- claiming generator and seed management are already complete
- claiming the new framework fully replaces the older test runner today
