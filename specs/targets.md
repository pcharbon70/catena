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
- The internal property-testing framework is materially implemented from
  generators and shrinking through runners, reporting, laws, stateful/BEAM
  helpers, and explicit advanced helpers. Placeholder-backed advanced paths
  and automatic source-language integration remain.
- The complete maintained EUnit suite is green.
- Promoted requirements, scenarios, component acceptance criteria, ADRs,
  paths, and links are checked by executable governance. Scenario evidence has
  a focused manifest-driven runner, and the complete verification contract is
  enforced in CI.

## Immediate Targets

- Execute the
  [Dialyzer remediation roadmap](planning/dialyzer-remediation/README.md) to
  replace the visible 777-warning inventory with an enforced zero-warning
  boundary.
- Decide whether the next implementation track resumes the proof-of-concept
  module system, advances source-language actor integration, or deepens the
  already-implemented property-testing framework.
- Finish placeholder-backed parallel, concurrency, distribution, and OTP
  property-testing paths.
- Extend executable scenario evidence when promoted behavior changes.
- Continue migrating useful historical PropEr intent into the internal
  property-testing architecture without reintroducing a second engine.

## Medium-Term Targets

- full module-system completion
- actor model integration on top of the existing effect/runtime foundation
- broader front-end coverage and ergonomics for the already-implemented algebraic-effects system
- automatic reflection/derivation and broader workflow ergonomics for the
  internal property-testing framework

## Non-Goals Of This Specs Layer

- replacing code- and test-backed component detail with a second parallel status layer
- treating historical planning checklists as more authoritative than
  executable code, tests, and promoted status
- freezing experimental research areas that are still clearly exploratory
