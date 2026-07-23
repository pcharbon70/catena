# Phase 2 Test Baseline

## Purpose

This baseline records the complete active EUnit result after standard-library
compilation, higher-kinded validation, parser-native imports/exports, and
canonical frontend fixtures were reconciled. It supersedes the Phase 1
baseline as the current quality snapshot while preserving later-phase failures
as visible work.

## Verification

The baseline was verified on 2026-07-23 with Erlang/OTP 28, ERTS 16.2, and
rebar3 3.27.0.

| Command | Result |
| --- | --- |
| `make compile` | Pass; all active source modules compile |
| Focused Phase 2 modules | 170 passed, 0 failed, 0 skipped |
| `make test` run 1 | 4,706 passed, 93 failed, 0 skipped |
| `make test` run 2 | 4,706 passed, 93 failed, 0 skipped |

The active suite contains 4,799 tests. The two complete-suite runs produced
the same failing-test and failing-module inventories.

## Resolved Phase 2 Inventory

All 29 failures assigned to Phase 2 in the Phase 1 baseline are resolved:

| Resolved module | Phase 1 failures | Phase 2 result |
| --- | ---: | --- |
| `catena_stdlib_compilation_tests` | 10 | Pass |
| `catena_hkt_validation_tests` | 8 | Pass |
| `catena_stdlib_integration_tests` | 5 | Pass |
| `catena_import_tests` | 4 | Pass |
| `catena_name_resolve_tests` | 1 | Pass |
| `catena_compile_tests` | 1 | Pass |
| **Total** | **29** | **Resolved** |

Nine regression tests were added for kind evidence, constructor patterns,
partial higher-kinded application, substitution normalization, import
selection/qualification, and parser-native name resolution. This moves the
complete-suite total from 4,790 to 4,799 tests and the passing count from
4,668 to 4,706.

## Remaining Failure Inventory

| Later phase | Failing module | Failures |
| --- | --- | ---: |
| Phase 4: Type and Effect Integration Boundary | `catena_effects_tests` | 26 |
| Phase 4: Type and Effect Integration Boundary | `catena_unified_effects_tests` | 19 |
| Phase 4: Type and Effect Integration Boundary | `catena_system_integration_tests` | 14 |
| Phase 4: Type and Effect Integration Boundary | `catena_effects_integration_tests` | 10 |
| Phase 4: Type and Effect Integration Boundary | `catena_effect_validation_tests` | 2 |
| Phase 4: Type and Effect Integration Boundary | `catena_effect_system_tests` | 2 |
| Phase 4: Type and Effect Integration Boundary | `catena_effect_runtime_tests` | 1 |
| Phase 4: Type and Effect Integration Boundary | `catena_effect_integration_tests` | 1 |
| Phase 4: Type and Effect Integration Boundary | `catena_algebraic_effects_integration_tests` | 1 |
| Phase 5: Runtime, REPL, and Actor Boundaries | `catena_process_tests` | 15 |
| Phase 6: Property and Law Status Reconciliation | `catena_stdlib_laws_tests` | 2 |
|  | **Total** | **93** |

## Ownership Summary

| Later phase | Failure count | Reconciliation focus |
| --- | ---: | --- |
| Phase 4 | 76 | Legacy effect APIs versus current orchestration/runtime semantics |
| Phase 5 | 15 | Runtime process primitive contracts |
| Phase 6 | 2 | Standard-library law bridge behavior |
| **Total** | **93** |  |

Phase 3 still has no uniquely assigned baseline failure. Its compiler,
code-generation, and pattern-integration work remains a separate validation
phase rather than being inferred complete from this inventory.

## Interpretation

The standard-library and frontend boundary assigned to Phase 2 is green. The
repository-wide suite remains intentionally non-green because later
reconciliation phases still own the 93 failures above; those failures remain
active and must not be hidden by filtering or permissive assertions.
