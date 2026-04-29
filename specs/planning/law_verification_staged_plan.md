# Law Verification Staged Plan

## Purpose

This document promotes Catena's staged plan for trait-law verification.

It exists because the repository currently has:

- real law definitions in the standard library
- real test wrappers for law checks
- structural tests proving those surfaces compile and normalize correctly
- an unfinished path from those stdlib-native surfaces to fully reconciled executable law verification, even though the internal Erlang/proptest framework has moved materially further ahead

The goal is to describe the intended path without overstating what is already executable.

## Current Baseline

Catena's current baseline for law verification is:

- `lib/catena/stdlib/laws.cat` defines pure law transforms for `Mapper`, `Pipeline`, `Comparable`, and `Combiner`
- `lib/catena/stdlib/test.cat` defines `verify` as the current standard-library wrapper for turning a Bool-producing law check into a test value
- `test/compiler/integration/catena_stdlib_laws_tests.erl` verifies the `Laws` module parses, exports the expected transforms, and preserves the intended law arities and AST shape
- `src/proptest/catena_laws.erl`, `catena_trait_laws.erl`, `catena_discipline.erl`, and `catena_law_tests.erl` provide a generic Erlang-side law framework
- `test/proptest/catena_trait_laws_tests.erl` and `test/proptest/catena_law_integration_tests.erl` exercise that internal framework end-to-end
- full execution of reusable imported law suites is still partial
- generic generator-backed law verification now exists in the internal Erlang/proptest framework, but the stdlib/Catena-native execution path is still incomplete

This creates two overlapping realities:

- the stdlib/Catena-native law path is still only partially executable
- the internal Erlang/proptest law framework is materially further along

Promoted interpretation:

- Stage 1 law definition work exists
- Stage 2 remains partial on the stdlib-native path
- Stage 3 and Stage 4 are materially implemented in the internal Erlang/proptest framework
- Stage 5 remains future ergonomics and workflow integration work

## Stage 1: Structural Law Definition

Status: implemented

The first stage establishes the law surface itself:

- define laws as pure Catena transforms in `Laws`
- keep law semantics library-first rather than compiler-intrinsic
- express law checks in terms of existing trait vocabulary such as `map`, `chain`, `pure`, `combine`, and `equals`
- validate that those law definitions parse and normalize correctly

This stage proves the language and stdlib can represent the laws, but it does not yet prove generic instance compliance by execution across many values.

## Stage 2: Executable Concrete Law Suites

Status: partial and still the clearest unfinished step on the stdlib-native path

The second stage makes today's law surface executable for concrete known instances before the generic property-testing framework is complete.

### Deliverables

- run `Laws` checks through `Test.verify` for concrete fixtures such as `Maybe`, `Either`, `List`, and simple comparable/combiner instances
- compile and load `Prelude`, `Test`, and `Laws` together through the current compiler/module path
- add executable integration tests that confirm concrete law suites pass or fail as expected
- report failures through the current testing surface with law-oriented names and messages

### Constraints

- this stage should not depend on generic generators or automatic derivation
- fixture sets may be finite and hand-authored
- the focus is proving executable law-suite plumbing, not proving universal correctness

### Success Criteria

- at least one reusable law suite executes end-to-end against known stdlib-backed instances
- a deliberately broken fixture or intentionally invalid instance can be shown to fail
- the tests run through a maintained repo test path rather than ad hoc one-off scripts

## Stage 3: Generator And Runner Foundation

Status: materially implemented in the internal Erlang/proptest framework

The third stage builds the internal foundation required for generic law verification:

- Property Testing Phase 1.2 Generator Type and Seed Management
- Property Testing Phase 1.3 categorical instances for generators
- Property Testing Phase 1.4 primitive combinators
- runner/reporting integration that can execute internal properties cleanly

This stage is where Catena moves from finite fixture checking toward data-driven law checking with integrated shrinking.

Current promoted reading:

- the generator/runner/reporting foundation now exists in `src/proptest/*`
- the remaining work is less about foundation and more about bridging that foundation back into the stdlib-native law-verification story cleanly

## Stage 4: Generic Law Specifications And Disciplines

Status: materially implemented on the Erlang/proptest side

This stage aligns with the Phase 4 property-testing track and turns law verification into a reusable framework.

### Deliverables

- `Law` values with name, description, and parameterized property functions
- equality abstractions for structural, approximate, or domain-specific equality
- law sets for Functor, Applicative, Monad, Semigroup, Monoid, Setoid, and ordering traits
- discipline packaging that groups laws and auxiliary generators
- explicit Erlang-side helpers such as `catena_laws:test_suite/2` before any macro/derive sugar exists

### Success Criteria

- the same law package can be instantiated for multiple types
- failures produce law-aware reports and counterexamples
- generic law suites no longer depend on hand-authored concrete fixture lists

Current promoted reading:

- this stage exists today in the internal framework, even though the older phase markdown remains largely unchecked
- the missing work is broader integration and canonical documentation, not the complete absence of a generic law framework

## Stage 5: Ergonomic Derivation And Workflow Integration

Status: partial/follow-on

Once Catena grows the right macro/metadata capabilities, law verification can become more ergonomic.

### Candidate Features

- `derive_law_tests` or equivalent Catena-native sugar
- REPL helpers such as `check_laws/2`
- grouped law output in the test runner
- CI/reporting that shows which traits and laws were exercised

This stage is explicitly downstream of the generic framework, not a prerequisite for it.

Current promoted reading:

- explicit function-based helpers now exist before macro/derive sugar
- the broader ergonomic workflow story remains incomplete

## Recommended Execution Order

1. Maintain the green default repo test path while continuing the PropEr migration.
2. Complete Stage 2 by making current `Laws + Test.verify` executable for concrete suites.
3. Reconcile the staged-law docs with the already-implemented `src/proptest/*` framework rather than pretending Stages 3 and 4 are still absent.
4. Bridge the stdlib-native law path and the internal law framework more explicitly where that improves contributor understanding and test coverage.
5. Add broader ergonomic derivation only after the underlying framework and integration boundaries are solid.

## What This Plan Avoids

This plan intentionally avoids two bad outcomes:

- claiming law verification is already complete just because the law definitions compile
- blocking all useful progress on law execution until the entire future property-testing roadmap is finished

Catena should instead ship law verification in layers: first representable, then executable for concrete suites, then generic and property-based.
