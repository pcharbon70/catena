# Law Verification Staged Plan

## Purpose

This document promotes Catena's staged plan for trait-law verification.

It exists because the repository currently has:

- real law definitions in the standard library
- real test wrappers for law checks
- structural tests proving those surfaces compile and normalize correctly
- executable concrete law suites
- a known-instance bridge from stdlib generic checks into the internal Erlang/proptest framework

The goal is to describe the implemented layers and preserve the boundary
between current known-instance execution and future automatic derivation.

## Current Baseline

Catena's current baseline for law verification is:

- `lib/catena/stdlib/laws.cat` defines pure law transforms for `Mapper`, `Pipeline`, `Comparable`, and `Combiner`
- `lib/catena/stdlib/test.cat` defines concrete `verify` checks plus generic `verifyTrait` and `verifyTraits` values with iteration and seed configuration
- `test/compiler/integration/catena_stdlib_laws_tests.erl` verifies structure and executes concrete, generic, configured, and deliberately failing suites
- `src/proptest/catena_laws.erl`, `catena_trait_laws.erl`, `catena_discipline.erl`, and `catena_law_tests.erl` provide a generic Erlang-side law framework
- `test/proptest/catena_trait_laws_tests.erl` and `test/proptest/catena_law_integration_tests.erl` exercise that internal framework end-to-end
- `src/testing/catena_stdlib_law_bridge.erl` maps Catena trait vocabulary and known `Maybe`, `Either`, `List`, and `Int` instances into that framework
- `src/testing/catena_test_runner.erl` executes the resulting law checks and preserves law-aware failure details

This creates one layered execution path:

- concrete fixtures execute Catena law transforms directly
- known-instance generic checks execute generator-backed internal laws
- both report through the maintained test runner

Promoted interpretation:

- Stage 1 law definition is implemented
- Stage 2 concrete stdlib execution is implemented
- Stage 3 and Stage 4 are materially implemented and connected at the known-instance bridge
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

Status: implemented for current concrete fixtures

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

Current evidence:

- `Maybe`, `Either`, `List`, applicative, accumulator, and orderable fixtures execute
- a deliberately broken `Maybe` fixture reports a law failure
- imported `Prelude`, `Test`, and `Laws` declarations execute through the maintained compiler/runtime test path

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
- stdlib generic checks now reach that foundation through `catena_stdlib_law_bridge`

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

- this stage exists in the internal framework and is callable from stdlib generic law values for supported known instances
- the missing work is automatic instance discovery, broader instance coverage, and ergonomic source-language derivation

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
- generic stdlib checks support iteration and seed configuration
- the broader ergonomic workflow story remains incomplete

## Recommended Execution Order

1. Maintain the green default repo test path and the focused property/law gate.
2. Extend the known-instance bridge only with explicit generators, adapters, and focused law evidence.
3. Improve grouped reporting and contributor-facing workflow ergonomics on the maintained runner.
4. Add automatic instance discovery, source-language derivation, or REPL sugar only after the required reflection and macro capabilities exist.

## What This Plan Avoids

This plan intentionally avoids two bad outcomes:

- claiming law verification is already complete just because the law definitions compile
- blocking all useful progress on law execution until the entire future property-testing roadmap is finished

Catena ships law verification in layers: representable, executable for concrete
suites, then generic and property-based for explicit known instances. Automatic
derivation remains a later layer rather than an implicit claim of the current
bridge.
