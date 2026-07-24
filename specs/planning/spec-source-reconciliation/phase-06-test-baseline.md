# Phase 6 Test Baseline

## Purpose

This baseline records the complete active EUnit result after runtime transform
scope, stdlib law execution, and unique collection generation were reconciled.
It supersedes the Phase 5 baseline as the current quality snapshot and records
the first fully green complete-suite boundary in this reconciliation roadmap.

## Verification

The baseline was verified on 2026-07-24 with Erlang/OTP 28, ERTS 16.2, and
rebar3 3.27.0.

| Command | Result |
| --- | --- |
| `make compile` | Pass; all active source modules compile and module names are unique |
| `rebar3 dialyzer` | Non-green; 777 repository-wide warnings remain outside the Phase 6 gate |
| Focused Phase 6 modules | 1,091 passed, 0 failed, 0 skipped |
| `make test` final run 1 | 4,829 passed, 0 failed, 0 skipped |
| `make test` final run 2 | 4,829 passed, 0 failed, 0 skipped |

The active suite contains 4,829 tests. The two consecutive final
complete-suite runs produced identical green totals. Eight consecutive
recorded repetitions of the 53-module Phase 6 gate also produced identical
green totals.

## Phase 6 Integration Evidence

The focused gate selects every active `*_tests.erl` module under
`test/proptest/` and `test/testing/`, the property/law source modules that
carry inline EUnit coverage, and the compiled stdlib law integration suite:

| Focused group | Modules | Verified boundary |
| --- | ---: | --- |
| `src/proptest` inline suites | 8 | Discipline, laws, reporting, runner, stdgen, and trait-law internals |
| `test/proptest` | 36 | Generators, shrinking, properties, reporting, laws, stateful/BEAM integration, and advanced helpers |
| `test/testing` | 8 | Compatibility adapters, first-class values, convergence, runner formatting, and workflows |
| `catena_stdlib_laws_tests` | 1 | Parsed, concrete, generic, configured, and failing stdlib law suites |
| **Total** | **53** | **1,091 tests pass** |

## Resolved Phase 6 Inventory

Every failure assigned to Phase 6 in the Phase 5 baseline is absent from the
two final complete-suite results:

| Resolved module | Phase 5 observation | Phase 6 result |
| --- | ---: | --- |
| `catena_stdlib_laws_tests` | 2 deterministic failures | Pass |
| `catena_stdgen_collection_tests` | 1 observed intermittent failure | Pass |
| **Total** | **3 observations** | **Resolved** |

The complete suite grew from 4,824 to 4,829 tests through one forward-reference
runtime regression and four unique-collection cardinality/exhaustion
regressions.

## Runtime And Law Boundary

`catena_test_runner:build_runtime_env/2` now builds constructors and callable
transforms in separate passes. Transform closures evaluate against the complete
callable scope, so imported stdlib transforms can refer to later declarations
without depending on source order.

That closes the compiled path from `Test.verifyTrait` and
`Test.verifyTraits`, including iteration and seed configuration, through
`catena_stdlib_law_bridge` and the internal property engine. Supported known
instances execute their generator-backed laws, while unsupported trait
requests produce explicit failed test results.

## Unique Collection Boundary

Map and set roots now resample duplicate keys or elements with deterministic
seed progression until the selected cardinality is reached. Each missing
unique value has a bounded retry budget. A generator whose domain cannot
satisfy the requested cardinality raises
`{generator_failed, {unique_collection_exhausted, Kind, Size}}` rather than
hanging or returning an undersized root.

The focused collection tests verify exact cardinality across 250 deterministic
seeds for maps and 250 for sets, plus explicit unsatisfiable-domain behavior.
Existing shrinking behavior remains allowed to reduce collection size after
root generation.

## Remaining Scope

There is no remaining active EUnit failure assigned to Phase 6. The following
work remains visible beyond this phase:

- 777 repository-wide Dialyzer warnings
- placeholder-backed or simplified branches in some advanced stateful,
  concurrency, distribution, and OTP property-testing helpers
- automatic source-language reflection, derivation macros, and broader
  REPL/CI law ergonomics
- executable spec-conformance and governance automation assigned to Phase 7

## Interpretation

Catena now has one maintained executable property/law path: Catena-facing and
legacy declaration surfaces adapt into the internal generator, shrinking,
runner, reporting, and discipline framework. The compatibility layer remains
real, but it no longer represents a separate property engine.

The green default suite proves the maintained repository boundary. It does not
claim that every advanced property-testing helper is production-complete or
that static analysis is green.
