# Current Status

This document reconciles Catena's current status from:

- the current `specs/` set
- the current codebase and test tree
- preserved implementation-facing documentation that has already been promoted into `specs/`

It exists because some planning checklists are stale relative to later implementation summaries and code.

## Track Summary

| Track | Current promoted status |
| --- | --- |
| Proof-of-concept | Implemented through Phases 1 to 3, with Phase 4 partial/minimal and later phases still planned or historically superseded. |
| Algebraic-effects | Phase 7 handler/resumption foundations and Phase 8 equation/law surfaces are materially implemented; later effect-system surfaces exist with uneven maturity, and Phase 9 handler-depth work is the next active milestone. |
| Property testing | Phases 1 to 4 are materially implemented in `src/proptest`; Phases 5 and 6 are substantial but partial; Phase 7 remains planned. |
| Law verification | Structural stdlib law definition is implemented; stdlib-native execution remains partial; the internal proptest law framework is materially ahead of the staged plan. |
| Language revamp migration | Completed and now historical. |
| Flow | The pure Flow core is materially implemented in stdlib/compiler/test surfaces; later phases remain planned. |
| Standalone category-theory library plan | Historical only; integrated into the PoC planning lineage rather than active as a separate track. |

## Proof-Of-Concept Track

### Phase 1: Core Language Infrastructure

Current promoted status:

- Section 1.1: complete
- Section 1.2: complete
- Section 1.3: complete
- Section 1.4: complete
- Section 1.5: partial but materially advanced beyond the raw checklist

Reconciled Section 1.5 status:

- 1.5.1 Standard Library Compilation: complete
- 1.5.2 Trait Instance Resolution: complete
- 1.5.3 Higher-Kinded Type Validation: complete
- 1.5.4 Law Verification via Test Module: partial
  - pure law definitions in `Laws` exist
  - structural tests for those law surfaces exist
  - executable concrete law suites and generic generator-backed law verification remain staged follow-on work
- 1.5.5 Do-Notation Desugaring: implemented
- 1.5.6 Effect Integration with Kleisli Arrows: mostly implemented
- 1.5.7 Operator Desugaring: implemented

### Phase 2: REPL And Basic Runtime

Implementation summaries and code indicate:

- Phase 2.1 REPL: implemented
- Phase 2.2 Prelude/runtime bindings: implemented
- Phase 2.3 Testing framework: implemented
- Phase 2.4 Integration tests: implemented

The older Phase 2 proof-of-concept checklist was not reconciled to that later implementation state.

### Phase 3: Pattern Matching Engine

Implementation summaries and code indicate:

- Phase 3.1 advanced patterns: implemented
- Phase 3.2 decision trees: implemented
- Phase 3.3 exhaustiveness/redundancy checking: implemented
- Phase 3.4 integration tests: implemented

The older Phase 3 proof-of-concept checklist was not reconciled to that later implementation state.

### Phase 4 And Beyond

- Phase 4 module system: not complete
- current repo does include minimal import resolution and exported-environment wiring
- Phase 5 actor model integration: planned
- the original proof-of-concept "Phase 6 effect completion" label is no longer the best description of the current effect-system implementation
- Phase 7 distribution layer: planned/research-backed, not implemented

## Algebraic-Effects Track

The repo now contains a newer algebraic-effects track that materially overtakes the older "effect completion is deferred" phrasing from the proof-of-concept plan.

Current promoted status:

- Phase 7 handler/resumption foundations are materially implemented in `src/compiler/effects/catena_handler.erl`, `catena_resumption.erl`, `catena_perform.erl`, and their focused tests
- Phase 8 equation and algebraic-law surfaces are materially implemented in `src/compiler/effects/catena_equations.erl`, `catena_equation_prover.erl`, `catena_equation_rewrite.erl`, `catena_effect_system.erl`, and dedicated effect-system optimization/verification tests
- row-polymorphism integration, typed handlers, operation signatures, higher-order effects, and Phase 14 validation/orchestration surfaces are present in code and tests, but those later milestones are not yet reconciled as a single uniformly finished front-end track
- the promoted repo status is therefore "implemented algebraic-effects machinery with uneven language-surface rollout", not "effect polymorphism entirely deferred"
- the next active milestone on this track is Phase 9 deep-versus-shallow handler semantics, which already has scaffold modules/tests in-tree but still needs section-by-section reconciliation

Important caveat:

- some of the newer effect machinery is better represented in internal compiler/runtime modules and focused tests than in the fully surfaced end-user syntax, so implementation maturity is ahead of total front-end polish

## Property-Testing Track

The property-testing planning documents are no longer aligned with the current repo in a simple "plan ahead of code" direction. Phase 1 and Phase 2 notes mark work complete beyond the older promoted status, while Phases 3 through 6 still show unchecked markdown checklists even though the repo now contains substantial `src/proptest/*` implementations and tests for those areas.

Current promoted status:

- Phase 1 core generator work is materially complete through Sections 1.1 to 1.7
- Phase 2 standard generators are materially complete through the implemented `catena_stdgen` surface and integration tests
- Phase 3 property DSL, runner, reporting, shrinking integration, and seed reproducibility are materially implemented in `src/proptest/catena_property.erl`, `catena_runner.erl`, `catena_report.erl`, and related tests
- Phase 4 law-testing infrastructure is materially implemented on the Erlang/proptest side through `catena_laws`, `catena_trait_laws`, `catena_discipline`, `catena_law_tests`, and integration tests
- Phase 5 stateful testing is substantial but partial: state-machine definition, command generation, symbolic/concrete execution, and integration surfaces exist, but some parallel-execution paths remain placeholder-backed
- Phase 6 BEAM integration is substantial but partial: process, message, concurrency, distribution, and OTP testing surfaces exist, but several distribution/concurrency/OTP paths remain placeholder-backed or simplified
- Phase 7 advanced features remain planned rather than implemented
- the current concrete property-testing surface spans `src/proptest/catena_tree.erl`, `catena_gen.erl`, `catena_shrink.erl`, `catena_stdgen.erl`, `catena_property.erl`, `catena_runner.erl`, `catena_report.erl`, `catena_laws.erl`, `catena_trait_laws.erl`, `catena_discipline.erl`, `catena_statem.erl`, `catena_process.erl`, `catena_message.erl`, `catena_concurrency.erl`, `catena_distribution.erl`, and `catena_otp.erl`

Important caveats:

- the Phase 1 and Phase 2 note checklists are ahead of the old promoted summary and now mark completion through Section 1.7 and Phase 2.6 integration tests
- the Phase 3 through Phase 6 note checklists remain largely unchecked despite real implementations and tests, so this document is the reconciled read
- placeholder-backed paths still exist in parts of the stateful/concurrency/distribution surfaces, so "implemented" here does not mean every branch is production-polished
- the legacy `src/testing/*` property-testing surface still exists, but `src/proptest/*` is the newer and broader active direction

Next clear steps on this track:

- finish the placeholder-backed parallel/concurrency/distribution paths in Phases 5 and 6
- reconcile the property-testing phase markdown files themselves with the now-implemented Erlang/proptest surfaces
- decide whether the next active push is Phase 7 advanced features or deeper stabilization/integration of the already-implemented framework

## Law-Verification Track

The staged law-verification plan is now conservative relative to the repo's newer internal property-testing framework.

Current promoted status:

- Stage 1 structural stdlib law definition is implemented
- Stage 2 stdlib-native executable law suites remain partial and are still the clearest unfinished step on the standard-library path
- Stage 3 generator/runner foundation is materially implemented in `src/proptest/*`
- Stage 4 generic law specifications, trait-law definitions, discipline packaging, and law-test generation helpers are materially implemented on the Erlang/proptest side
- Stage 5 ergonomic derivation and workflow integration remains partial/future: function-based helpers exist, but the broader macro/derive/REPL/CI ergonomics are not fully realized

Important caveat:

- this track now has two partially overlapping realities: the stdlib/Catena-native law-execution story is still incomplete, while the internal Erlang/proptest law framework is materially further along

## Language-Revamp Migration Track

Current promoted status:

- the five-phase language revamp migration plan is complete
- the migration is historical rather than active
- its main remaining value is documentary rather than operational

## Flow Track

Current promoted status:

- the 8-phase Flow plan exists under `specs/planning/flow/`
- the accepted naming decision in ADR-0004 exists and the pure Flow core is no longer only plan text
- `Prelude` now exports the pure `System` and `Flow` traits, with the current core operations `id`, `compose`, `lift`, `first`, `parallel`, and `split`
- the compiler now supports the foundational pure Flow surface needed by that stdlib layer:
  - higher-arity trait-kind validation for `arr`
  - `>>>`, `<<<`, `***`, and `&&&` parsing/desugaring
  - structural `System` and `Flow` law definitions in `Laws`
- the current promoted status is therefore "pure Flow core materially implemented, later Flow track still planned", not "Flow entirely absent from the repo"

Important caveat:

- the implemented Flow surface is still only the pure core; function instances, `FlowChoice`, `FlowApply`, utilities, stream/circuit work, and broader examples/docs remain planned rather than implemented

## Historical Integrated Plans

Current promoted status:

- the standalone category-theory library plan is historical only
- it was integrated into the PoC planning lineage and should not be treated as an active separate execution track

## Current Quality State

The default `rebar3 eunit` entry point is clean again after quarantining the legacy PropEr-based property suites outside the active `test/` compile tree and fixing the remaining active-suite regressions that surfaced afterward.

Promoted interpretation:

- the repo-wide `rebar3 eunit` path is green for active repo surfaces
- the internal property-testing transition is still underway
- historical PropEr suites remain preserved under `test_legacy/proper/` as migration targets rather than active default tests
