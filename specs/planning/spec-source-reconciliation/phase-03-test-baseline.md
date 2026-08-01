# Phase 3 Test Baseline

## Purpose

This baseline records the complete active EUnit result after canonical
frontend-to-codegen lowering, compiler-enforced pattern contracts, and the
public source-to-Core pipeline were reconciled. It supersedes the Phase 2
baseline as the current quality snapshot while retaining all later-phase
failures as visible work.

## Verification

The baseline was verified on 2026-07-23 with Erlang/OTP 28, ERTS 16.2, and
rebar3 3.27.0.

| Command | Result |
| --- | --- |
| `make compile` | Pass; all active source modules compile |
| Focused Phase 3 modules | 397 passed, 0 failed, 0 skipped |
| `make test` run 1 | 4,725 passed, 93 failed, 0 skipped |
| `make test` run 2 | 4,725 passed, 93 failed, 0 skipped |

The active suite contains 4,818 tests. The two complete-suite runs produced
the same failing-test and failing-module inventories.

## Phase 3 Integration Evidence

Phase 3 had no uniquely assigned failure in the Phase 2 baseline. Its
completion required new executable evidence across boundaries that the older
component tests exercised only with manually constructed ASTs.

| New conformance module | Tests | Verified boundary |
| --- | ---: | --- |
| `catena_codegen_lower_tests` | 4 | Parser-native AST to valid Core Erlang |
| `catena_pattern_contract_tests` | 7 | Semantic pattern contracts and advanced lowering |
| `catena_core_pipeline_tests` | 8 | Typed source/file input to executable Core Erlang |
| **Total** | **19** | **All pass** |

The focused Phase 3 gate also includes the existing compiler pipeline,
codegen, parser pattern, type pattern, static pattern-analysis, decision-tree,
and pattern-integration modules. Together, those 15 modules pass all 397
tests.

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

## Interpretation

The canonical compiler path now reaches executable Core Erlang after semantic,
kind, import, and type validation. Pattern guard purity, transform clause
arity, and or-pattern bindings are enforced on that path. Exhaustiveness and
redundancy analyzers and the decision-tree builder remain separately tested
compiler surfaces; the default Core lowering currently emits binding-safe Core
case clauses directly.

The complete active suite remains intentionally non-green because later
reconciliation phases still own the 93 failures above. Those failures remain
active and must not be hidden by filtering or permissive assertions.
