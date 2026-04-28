# Property Testing Framework

## Status

Promoted status: materially implemented for the maintained repo workflow. The internal property-testing engine now spans rose trees, generators, ranges, the runner, reporting, first-class Catena property surfaces, and generic law-suite execution for known stdlib-backed instances. The remaining PropEr boundary is historical/documentary rather than part of the active execution path.

## Design Anchors

- [Current Status](../planning/current_status.md)
- [Law Verification Staged Plan](../planning/law_verification_staged_plan.md)
- [Testing And Quality Contract](../contracts/testing_and_quality_contract.md)
- `src/proptest/catena_tree.erl`
- `src/proptest/catena_range.erl`
- `test/proptest/catena_tree_tests.erl`
- `test/proptest/catena_range_tests.erl`
- `src/proptest/catena_gen.erl`
- `test/proptest/catena_gen_tests.erl`
- `src/proptest/catena_runner.erl`
- `src/proptest/catena_report.erl`
- `lib/catena/stdlib/gen.cat`
- `lib/catena/stdlib/test.cat`
- `src/testing/catena_first_class_property_adapter.erl`
- `src/testing/catena_property_adapter.erl`
- `src/testing/catena_stdlib_law_bridge.erl`
- `src/testing/catena_test_runner.erl`

## Current Promoted Surface

- The new property-testing track is real implementation work, not only planning.
- `catena_tree`, `catena_range`, and `catena_gen` are the current canonical implemented surfaces for the new framework.
- `catena_gen` now includes the functor/applicative/monad/alternative-style layer needed to compose generators before the primitive-combinator phase.
- `catena_gen` now also includes primitive constant/element/bool/int/filter/sample utilities with first-class `Range` integration for integer generation.
- `catena_range` now owns the promoted range abstraction, including origins, size-scaled bounds, and linear/exponential constructors.
- `catena_runner` is now the maintained execution engine for active properties, including deterministic seeds, shrinking, discard tracking, and richer result metadata.
- `catena_report` is now part of the promoted property-testing surface for failure formatting and CI-friendly structured output.
- `lib/catena/stdlib/test.cat` now exposes a real first-class property surface through `Test.prop`, property configuration helpers, and `forAll` / `implies` / `discard`.
- `lib/catena/stdlib/gen.cat` now exposes the initial Catena-facing generator surface for bool/int/text families and first combinators such as `oneOf`, `map`, `flatMap`, and `filter`.
- `src/testing/catena_first_class_property_adapter.erl` is now the promoted bridge from first-class stdlib property/generator values into `src/proptest/*`.
- `src/testing/catena_property_adapter.erl` now routes legacy `property_decl` syntax into the same internal engine rather than maintaining a separate handwritten property loop.
- `src/testing/catena_test_runner.erl` is now the maintained compatibility and formatting layer over that internal engine for declaration-based tests, first-class properties, suites, and known-instance law checks.
- `src/testing/catena_stdlib_law_bridge.erl` now bridges the stdlib law surface into the internal trait-law/disciplines framework for known instances such as `Maybe`, `Either`, `List`, and `Int`.
- Rich property/law results now preserve seeds, discard counts, shrink information, counterexamples, and label/output metadata in the active runner path.
- The historical PropEr quarantine has effectively collapsed to documentation-only material under `test_legacy/proper/`; there are no runnable PropEr suites left in the maintained path.

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

### AC-PROP-006 Reconciled Project Status

The promoted current status for the property-testing roadmap is:

- Phase 1 Section 1.1 rose-tree foundation: complete
- Phase 1 Section 1.2 generator type and seed management: complete
- Phase 1 Section 1.3 categorical instances for generators: complete
- Phase 1 Section 1.4 primitive combinators: complete
- Phase 1 Section 1.5 range types: complete
- runner/reporting integration: materially implemented
- Catena-facing property API and generator bridge: materially implemented
- known-instance law bridging: materially implemented

The active property/law engine is no longer limited to the original foundations.

The remaining work is now mostly advanced engine breadth, workflow polish, and canonical documentation rather than the absence of a working property/law execution stack.

### AC-PROP-007 Transitional Coexistence

Until the new framework is fully wired through the language and tooling, Catena currently has two valid testing-related layers:

- the compatibility/front-end layer in `src/testing/*`
- the internal execution/reporting engine in `src/proptest/*`

The promoted spec must describe that coexistence honestly: `src/testing/*` still exists, but it no longer owns a separate property-testing engine.

### AC-PROP-008 Internal Replacement Direction

The canonical direction is internal property testing with integrated shrinking, not ongoing dependence on PropEr as Catena's lasting architecture. Migration work should therefore move toward:

- Catena-owned generators
- Catena-owned shrinking
- Catena-owned runner/reporting semantics

### AC-PROP-009 Known Gap Visibility

Documentation and tooling specs must continue to note the true transition state:

- the default repo-wide `rebar3 eunit` entry point is green again for the active suite
- the historical PropEr area no longer contains runnable suites, only documentary artifacts
- the remaining gaps are advanced internal-framework breadth and workflow ergonomics, not active dependence on PropEr in the default path

This prevents the specs from confusing "default workflow restored" with "nothing remains to improve."

### AC-PROP-010 Law-Testing Destination

Generic trait-law verification SHOULD converge on the internal property-testing framework rather than remaining permanently split between ad hoc concrete suites and external tooling.

Promoted staging:

- near-term: concrete executable suites may still use `Laws + Test.verify`
- current implemented middle layer: known-instance generator-backed law suites execute through `catena_stdlib_law_bridge`
- later: broader derive/REPL sugar and more automatic law plumbing land on top of the same internal framework

## Out Of Scope

- claiming every advanced proptest subsystem is finished and production-polished
- claiming `src/testing/*` disappears entirely even though it still serves as a compatibility/front-end layer
- inventing a second property/law architecture alongside the internal framework
