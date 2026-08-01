# Phase 1 Test Baseline

## Purpose

This baseline records the complete active EUnit result after repository module
names, build entry points, generated sources, and stateful test fixtures were
reconciled. It is an honest starting point for later phases, not a green-suite
claim.

## Verification

The baseline was verified on 2026-07-23 with Erlang/OTP 28, ERTS 16.2, and
rebar3 3.27.0.

| Command | Result |
| --- | --- |
| `make compile` | Pass; all active source modules compile |
| `make test` run 1 | 4,668 passed, 122 failed, 0 skipped |
| `make test` run 2 | 4,668 passed, 122 failed, 0 skipped |

The active suite contains 4,790 tests. The two full-suite runs produced the
same failing-module inventory.

## Remaining Failure Inventory

| Later phase | Failing module | Failures |
| --- | --- | ---: |
| Phase 2: Standard Library and Frontend Validation | `catena_stdlib_compilation_tests` | 10 |
| Phase 2: Standard Library and Frontend Validation | `catena_hkt_validation_tests` | 8 |
| Phase 2: Standard Library and Frontend Validation | `catena_stdlib_integration_tests` | 5 |
| Phase 2: Standard Library and Frontend Validation | `catena_import_tests` | 4 |
| Phase 2: Standard Library and Frontend Validation | `catena_name_resolve_tests` | 1 |
| Phase 2: Standard Library and Frontend Validation | `catena_compile_tests` | 1 |
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
|  | **Total** | **122** |

## Ownership Summary

| Later phase | Failure count | Reconciliation focus |
| --- | ---: | --- |
| Phase 2 | 29 | Parser, imports, HKT validation, and stdlib compilation |
| Phase 4 | 76 | Legacy effect APIs versus current orchestration/runtime semantics |
| Phase 5 | 15 | Runtime process primitive contracts |
| Phase 6 | 2 | Standard-library law bridge behavior |
| **Total** | **122** |  |

Phase 3 has no uniquely assigned baseline failure. Its compiler, codegen, and
pattern integration work remains part of the roadmap and will be validated
against the now-canonical suite when that phase starts.

## Interpretation

The canonical build path is operational and deterministic, but the repository
is not test-green. Focused suites that already pass remain useful; the failed
families above are active reconciliation work and must not be omitted or
converted into permissive assertions.
