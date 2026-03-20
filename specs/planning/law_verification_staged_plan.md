# Law Verification Staged Plan

## Purpose

This document promotes Catena's staged plan for trait-law verification.

It exists because the repository currently has:

- real law definitions in the standard library
- real test wrappers for law checks
- structural tests proving those surfaces compile and normalize correctly
- an implemented concrete-suite execution path
- a still-unfinished path from those surfaces to generic property-based law verification

The goal is to describe the intended path without overstating what is already executable.

## Current Baseline

Catena's current baseline for law verification is:

- `lib/catena/stdlib/laws.cat` defines pure law transforms for `Mapper`, `Pipeline`, `Comparable`, and `Combiner`
- `lib/catena/stdlib/test.cat` defines `verify` as the current standard-library wrapper for turning a Bool-producing law check into a test value
- `test/compiler/integration/catena_stdlib_laws_tests.erl` verifies the `Laws` module parses, exports the expected transforms, and preserves the intended law arities and AST shape
- concrete imported law suites now execute end to end for `Maybe`, `Either`, and `List`
- deliberately broken fixtures are proven to fail through the same maintained test path
- generic generator-backed law verification is not yet implemented

Promoted interpretation:

- Stage 1 law definition work exists
- Stage 2 executable concrete suites are implemented
- Stage 3 and later generality work remain to be done

## Stage 1: Structural Law Definition

Status: implemented

The first stage establishes the law surface itself:

- define laws as pure Catena transforms in `Laws`
- keep law semantics library-first rather than compiler-intrinsic
- express law checks in terms of existing trait vocabulary such as `map`, `chain`, `pure`, `combine`, and `equals`
- validate that those law definitions parse and normalize correctly

This stage proves the language and stdlib can represent the laws, but it does not yet prove generic instance compliance by execution across many values.

## Stage 2: Executable Concrete Law Suites

Status: implemented

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

Current promoted outcome:

- reusable suites execute against `Maybe`, `Either`, and `List`
- failing fixtures are covered by active tests
- the repo-wide maintained test path exercises this surface

## Stage 3: Generator And Runner Foundation

Status: in progress

The third stage builds the internal foundation required for generic law verification:

- Property Testing Phase 1.2 Generator Type and Seed Management
- Property Testing Phase 1.3 categorical instances for generators
- Property Testing Phase 1.4 primitive combinators
- runner/reporting integration that can execute internal properties cleanly

This stage is where Catena moves from finite fixture checking toward data-driven law checking with integrated shrinking.

Current promoted progress inside this stage:

- Property Testing Phase 1.2 Generator Type and Seed Management: implemented
- Property Testing Phase 1.3 categorical instances for generators: next
- Property Testing Phase 1.4 primitive combinators: later in this same stage
- runner/reporting integration for internal properties: still pending

### Immediate Dependencies

- preserve the now-green default `rebar3 eunit` path while migrating historical PropEr coverage into internal replacements
- continue the `src/proptest/*` track beyond `catena_tree`
- decide whether the older `src/testing/*` runner is wrapped, replaced, or bridged during migration

## Stage 4: Generic Law Specifications And Disciplines

Status: planned in the property-testing roadmap

This stage aligns with `notes/planning/property-testing/phase-04.md` and turns law verification into a reusable framework.

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

## Stage 5: Ergonomic Derivation And Workflow Integration

Status: later follow-on

Once Catena grows the right macro/metadata capabilities, law verification can become more ergonomic.

### Candidate Features

- `derive_law_tests` or equivalent Catena-native sugar
- REPL helpers such as `check_laws/2`
- grouped law output in the test runner
- CI/reporting that shows which traits and laws were exercised

This stage is explicitly downstream of the generic framework, not a prerequisite for it.

## Recommended Execution Order

1. Maintain the green default repo test path while continuing the PropEr migration.
2. Continue Property Testing Phase 1.3 and later Stage 3 generator/runner work now that Phase 1.2 is complete.
3. Implement the generic law-specification and discipline framework from the property-testing Phase 4 plan.
4. Add ergonomic derivation only after the underlying framework is solid.

## What This Plan Avoids

This plan intentionally avoids two bad outcomes:

- claiming law verification is already complete just because the law definitions compile
- blocking all useful progress on law execution until the entire future property-testing roadmap is finished

Catena should instead ship law verification in layers: first representable, then executable for concrete suites, then generic and property-based.
