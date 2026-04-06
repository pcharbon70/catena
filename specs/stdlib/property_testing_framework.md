# Property Testing Framework

## Status

Promoted status: in progress. The rose-tree foundation is implemented and tested, while generator/seed management and fuller runner integration remain the next active step.

## Design Anchors

- [Property testing Phase 1 plan](../../notes/planning/property-testing/phase-01.md)
- [Property testing Phase 4 law testing plan](../../notes/planning/property-testing/phase-04.md)
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
- The generic long-term destination for trait-law verification is this internal framework, but that law-testing layer belongs to a later roadmap phase and is not yet implemented.

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

The later reusable law-testing framework remains planned in Property Testing Phase 4 rather than being part of the current implemented surface.

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

Documentation and tooling specs must continue to note the true transition state:

- the default repo-wide `rebar3 eunit` entry point is green again for the active suite
- the migration is still incomplete because historical PropEr suites remain quarantined and their internal replacements are not all implemented yet

This prevents the specs from confusing "default workflow restored" with "migration finished."

### AC-PROP-006 Law-Testing Destination

Generic trait-law verification SHOULD converge on the internal property-testing framework rather than remaining permanently split between ad hoc concrete suites and external tooling.

Promoted staging:

- near-term: concrete executable suites may still use `Laws + Test.verify`
- mid-term: generator and runner foundations become available
- later: reusable law/disciplines/suite generation land on top of the internal framework

## Out Of Scope

- claiming generator and seed management are already complete
- claiming the new framework fully replaces the older test runner today
- claiming generic law-test disciplines are already implemented today
