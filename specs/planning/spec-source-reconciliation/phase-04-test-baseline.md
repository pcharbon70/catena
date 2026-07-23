# Phase 4 Test Baseline

## Purpose

This baseline records the complete active EUnit result after the public effect
execution contract, handler/resumption orchestration, effect-type helpers, and
algebraic-effects validation were reconciled. It supersedes the Phase 3
baseline as the current quality snapshot while retaining all later-phase
failures as visible work.

## Verification

The baseline was verified on 2026-07-23 with Erlang/OTP 28, ERTS 16.2, and
rebar3 3.27.0.

| Command | Result |
| --- | --- |
| `make compile` | Pass; all active source modules compile |
| `rebar3 dialyzer` | Non-green; 781 repository-wide warnings remain outside the Phase 4 gate |
| Focused Phase 4 modules | 244 passed, 0 failed, 0 skipped |
| `make test` run 1 | 4,801 passed, 17 failed, 0 skipped |
| `make test` run 2 | 4,801 passed, 17 failed, 0 skipped |

The active suite contains 4,818 tests. The two complete-suite runs produced
the same failing-test and failing-module inventories.

## Phase 4 Integration Evidence

The focused gate covers the public effect library, orchestration, continuation
wrappers, type helpers, validation, the explicit-context runtime, and
end-to-end effect scenarios:

| Focused module | Tests | Verified boundary |
| --- | ---: | --- |
| `catena_effects_tests` | 26 | Public builtin and custom effect execution |
| `catena_unified_effects_tests` | 42 | Unified lifecycle, runners, diagnostics, and type helpers |
| `catena_system_integration_tests` | 21 | Cross-component orchestration scenarios |
| `catena_effects_integration_tests` | 24 | Executable effects with optimizer/distribution descriptors |
| `catena_effect_validation_tests` | 5 | Aggregate theoretical/property/conformance validation |
| `catena_effect_system_tests` | 40 | System configuration, scopes, handlers, and optimization |
| `catena_effect_runtime_tests` | 38 | Explicit-context runtime and builtin IO/Process handlers |
| `catena_effect_integration_tests` | 25 | Runtime end-to-end effect behavior |
| `catena_algebraic_effects_integration_tests` | 5 | Algebraic-effects validation and runtime scenarios |
| `catena_continuation_kind_integration_tests` | 18 | Public one-shot and multi-shot wrapper behavior |
| **Total** | **244** | **All pass** |

## Resolved Phase 4 Inventory

All failures assigned to Phase 4 in the Phase 3 baseline are absent from the
revised complete-suite result:

| Resolved module | Phase 3 failures | Phase 4 result |
| --- | ---: | --- |
| `catena_effects_tests` | 26 | Pass |
| `catena_unified_effects_tests` | 19 | Pass |
| `catena_system_integration_tests` | 14 | Pass |
| `catena_effects_integration_tests` | 10 | Pass |
| `catena_effect_validation_tests` | 2 | Pass |
| `catena_effect_system_tests` | 2 | Pass |
| `catena_effect_runtime_tests` | 1 | Pass |
| `catena_effect_integration_tests` | 1 | Pass |
| `catena_algebraic_effects_integration_tests` | 1 | Pass |
| **Total** | **76** | **Resolved** |

The fresh Phase 4 starting audit reported 75 failures because
`catena_effect_integration_tests` was already green after Phase 3. The
complete-suite gate also exposed two stale continuation-wrapper assertions;
those now match the public facade's unwrapped resume result.

## Remaining Failure Inventory

| Later phase | Failing module | Failures |
| --- | --- | ---: |
| Phase 5: Runtime, REPL, and Actor Boundaries | `catena_process_tests` | 15 |
| Phase 6: Property and Law Status Reconciliation | `catena_stdlib_laws_tests` | 2 |
|  | **Total** | **17** |

## Ownership Summary

| Later phase | Failure count | Reconciliation focus |
| --- | ---: | --- |
| Phase 5 | 15 | Runtime process primitive contracts |
| Phase 6 | 2 | Standard-library law bridge behavior |
| **Total** | **17** |  |

## Interpretation

The public effect facade now executes operations through installed handlers;
its builtin State, Reader, Writer, and Error runners no longer expose the
historical descriptor-constructor behavior. Raw `{effect, ...}` descriptors
remain valid inputs for optimizer and distribution components.

Generated Catena code continues to use the explicit-context
`catena_effect_runtime` boundary. The higher-level Erlang orchestration facade
uses process-local scopes and opaque resumption wrappers for component and
integration work. Its current resume marker supplies operation results, but it
does not capture a true delimited Erlang continuation; that remains a
compiler/CPS concern.

The complete active suite remains intentionally non-green because Phases 5
and 6 own the 17 failures above. Those failures remain active and must not be
hidden by filtering or permissive assertions.
