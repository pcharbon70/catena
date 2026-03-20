# Property Testing Framework

## Status

Promoted status: in progress. The rose-tree foundation, generator/seed/size layer, categorical generator instances, primitive combinators, range types, and basic shrinking infrastructure are implemented and tested, while runner integration and Phase 1 integration coverage remain active follow-on work.

## Design Anchors

- [Property testing Phase 1 plan](../../notes/planning/property-testing/phase-01.md)
- [Property testing Phase 4 law testing plan](../../notes/planning/property-testing/phase-04.md)
- [Rose tree completion summary](../../notes/summaries/rose-tree-unit-tests-complete-2025-11-29.md)
- [Generator type and seed management summary](../../notes/summaries/generator-type-and-seed-management-2026-03-20.md)
- [Generator categorical instances summary](../../notes/summaries/generator-categorical-instances-2026-03-20.md)
- [Primitive combinators summary](../../notes/summaries/primitive-combinators-2026-03-20.md)
- [Range types summary](../../notes/summaries/range-types-2026-03-20.md)
- [Basic shrinking infrastructure summary](../../notes/summaries/basic-shrinking-infrastructure-2026-03-20.md)
- [Phase 2.3 testing summary](../../notes/summaries/phase-2.3-testing-summary.md)
- `src/proptest/catena_tree.erl`
- `src/proptest/catena_range.erl`
- `src/proptest/catena_shrink.erl`
- `test/proptest/catena_tree_tests.erl`
- `test/proptest/catena_range_tests.erl`
- `test/proptest/catena_shrink_tests.erl`
- `src/proptest/catena_gen.erl`
- `test/proptest/catena_gen_tests.erl`
- `src/testing/catena_generators.erl`
- `src/testing/catena_test_runner.erl`

## Current Promoted Surface

- The new property-testing track is real implementation work, not only planning.
- `catena_tree`, `catena_range`, `catena_shrink`, and `catena_gen` are the current canonical implemented surfaces for the new framework.
- `catena_gen` now includes the functor/applicative/monad/alternative-style layer needed to compose generators before the primitive-combinator phase.
- `catena_gen` now also includes primitive constant/element/bool/int/filter/sample utilities with first-class `Range` integration for integer generation.
- `catena_range` now owns the promoted range abstraction, including origins, size-scaled bounds, and linear/exponential constructors.
- `catena_shrink` now owns the promoted shrink/search layer, including reusable shrink sequences, depth-first minimal-case search, and configurable shrink limits.
- `catena_gen` now also exposes custom shrinking hooks so generators can replace, disable, or transform their shrink descendants when integrated shrinking is not enough.
- The older Phase 2.3 property-test execution path still exists and remains the active runner for first-class `property` declarations.
- Concrete law suites now execute on the current `Laws + Test.verify + src/testing/*` path for known instances while the generic framework is still under construction.
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

### AC-PROP-002 Generator And Seed Foundation

The promoted implemented next layer of the framework is the generator/seed/size
surface in `src/proptest/catena_gen.erl`, including:

- explicit generator values wrapping `(Size, Seed) -> Tree`
- deterministic `seed_from_int/1`
- non-deterministic `seed_new/0`
- `seed_next/1` stepping
- `seed_split/1` for deterministic independent sub-streams
- `sized/1`, `resize/2`, and `scale/2` for explicit size control

This establishes the planned Section 1.2 execution substrate for the later
generator-instance and primitive-combinator work.

### AC-PROP-003 Generator Categorical Instances

The promoted implemented next layer of the framework is the compositional
generator API in `src/proptest/catena_gen.erl`, including:

- `gen_map/2` and multi-argument mapping helpers
- `gen_pure/1` and `gen_ap/2`
- `gen_bind/2` and `gen_flatten/1`
- `gen_empty/0`, `gen_alt/2`, `gen_one_of/1`, and `gen_frequency/1`

This gives the property-testing track the planned Section 1.3 categorical base
that later primitive generators and law-testing surfaces will build on.

### AC-PROP-004 Primitive Combinators

The promoted implemented next layer of the framework is the primitive generator
surface in `src/proptest/catena_gen.erl`, including:

- `constant/1`, `element/1`, and `elements/1`
- `gen_bool/0` and `gen_bool/1`
- `gen_int/0`, `gen_int/1`, `gen_pos_int/0`, `gen_neg_int/0`, and `gen_nat/0`
- `gen_filter/2` and `gen_such_that/2`
- `sample/1`, `sample/2`, `print_tree/1`, and `shrinks/1`

This gives the property-testing track a usable primitive surface for the
non-range primitives and the higher-level debug/filter helpers.

### AC-PROP-005 Range Types

The promoted implemented next layer of the framework is the first-class range
surface in `src/proptest/catena_range.erl`, together with its integration into
`src/proptest/catena_gen.erl`, including:

- `range_bounds/2` and `range_origin/1`
- `range_constant/1`
- `range_linear/2` and `range_linear_from/3`
- `range_exponential/2` and `range_exponential_from/3`
- range-backed `gen_int/1`
- compatibility `gen_int_range/2`

This gives the property-testing track the planned Section 1.5 range semantics:
explicit shrink targets, size-scaled bounds, and a canonical replacement for
the earlier tuple-bound integer surface.

### AC-PROP-006 Basic Shrinking Infrastructure

The promoted implemented next layer of the framework is the shrink/search
surface in `src/proptest/catena_shrink.erl`, together with custom-shrinking
hooks in `src/proptest/catena_gen.erl`, including:

- `shrink_towards/2`
- `shrink_binary/2`
- `shrink_list/1`
- `shrink_halves/1`
- `find_minimal/2`
- configurable `find_minimal/3`
- `with_shrink/2`
- `no_shrink/1`
- `shrink_map/2`

This gives the property-testing track the planned Section 1.6 execution layer:
reusable shrink strategies, deterministic minimal-counterexample search, and a
generator escape hatch for domain-specific shrinking behavior.

### AC-PROP-007 Reconciled Project Status

The promoted current status for the property-testing roadmap is:

- Phase 1 Section 1.1 rose-tree foundation: complete
- Phase 1 Section 1.2 generator type and seed management: complete
- Phase 1 Section 1.3 categorical instances for generators: complete
- Phase 1 Section 1.4 primitive combinators: complete
- Phase 1 Section 1.5 range types: complete
- Phase 1 Section 1.6 basic shrinking infrastructure: complete
- next step: Phase 1 Section 1.7 integration tests

The planning checklist is now reconciled through Section 1.6.

The later reusable law-testing framework remains planned in Property Testing Phase 4 rather than being part of the current implemented surface.

### AC-PROP-008 Transitional Coexistence

Until the new framework is fully wired through the language and tooling, Catena currently has two valid testing-related layers:

- the existing test/property declaration runner in `src/testing/*`
- the new foundational property-testing implementation in `src/proptest/*`

The promoted spec must describe that coexistence honestly instead of pretending the migration is already complete.

### AC-PROP-009 Internal Replacement Direction

The canonical direction is internal property testing with integrated shrinking, not ongoing dependence on PropEr as Catena's lasting architecture. Migration work should therefore move toward:

- Catena-owned generators
- Catena-owned shrinking
- Catena-owned runner/reporting semantics

### AC-PROP-010 Known Gap Visibility

Documentation and tooling specs must continue to note the true transition state:

- the default repo-wide `rebar3 eunit` entry point is green again for the active suite
- the migration is still incomplete because historical PropEr suites remain quarantined and their internal replacements are not all implemented yet

This prevents the specs from confusing "default workflow restored" with "migration finished."

### AC-PROP-011 Law-Testing Destination

Generic trait-law verification SHOULD converge on the internal property-testing framework rather than remaining permanently split between ad hoc concrete suites and external tooling.

Promoted staging:

- near-term: concrete executable suites already use `Laws + Test.verify`
- mid-term: generator and runner foundations become available
- later: reusable law/disciplines/suite generation land on top of the internal framework

## Out Of Scope

- claiming the runner/reporting and Phase 1 integration-test layers are already complete
- claiming the new framework fully replaces the older test runner today
- claiming generic law-test disciplines are already implemented today
