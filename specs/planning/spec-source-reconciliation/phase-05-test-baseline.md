# Phase 5 Test Baseline

## Purpose

This baseline records the complete active EUnit result after the BEAM process
façade, local actor/OTP-style runtime toolkit, and direct REPL Process-effect
boundary were reconciled. It supersedes the Phase 4 baseline as the current
quality snapshot while retaining all property/law failures as visible Phase 6
work.

## Verification

The baseline was verified on 2026-07-24 with Erlang/OTP 28, ERTS 16.2, and
rebar3 3.27.0.

| Command | Result |
| --- | --- |
| `make compile` | Pass; all active source modules compile |
| `rebar3 dialyzer` | Non-green; 777 repository-wide warnings remain outside the Phase 5 gate |
| Focused Phase 5 modules | 302 passed, 0 failed, 0 skipped |
| `make test` final run 1 | 4,822 passed, 2 failed, 0 skipped |
| `make test` final run 2 | 4,822 passed, 2 failed, 0 skipped |

The active suite contains 4,824 tests. The two consecutive final complete-suite
runs produced the same failing-test and failing-module inventories.

## Phase 5 Integration Evidence

The focused gate covers BEAM process primitives, actors and OTP-style
components, local registration/fan-out services, interactive state and
commands, direct effect execution, and complete REPL workflows:

| Focused module | Tests | Verified boundary |
| --- | ---: | --- |
| `catena_process_tests` | 26 | Spawn, messaging, native OTP calls, links, monitors, names, receive, and exits |
| `catena_actor_tests` | 21 | Actor lifecycle, calls, casts, state, replies, and concurrency |
| `catena_gen_server_tests` | 21 | GenServer-style local callback protocol |
| `catena_supervisor_tests` | 14 | Minimal local one-for-one child lifecycle |
| `catena_registry_tests` | 19 | Local registration, metadata, lookup, and cleanup |
| `catena_pubsub_tests` | 14 | Topic publication, wildcards, and subscriber cleanup |
| `catena_event_broadcaster_tests` | 13 | Listener fan-out, filters, and cleanup |
| `catena_actor_integration_tests` | 8 | Cross-component local actor scenarios |
| `catena_repl_tests` | 36 | Parsing, commands, evaluation, state, and recovery |
| `catena_repl_completion_tests` | 12 | Command completion |
| `catena_repl_effects_tests` | 35 | Direct builtins and Process spawn/send/self execution |
| `catena_repl_history_tests` | 21 | In-memory and persisted history behavior |
| `catena_repl_workflow_tests` | 26 | Complete interactive workflows |
| `catena_repl_programs_tests` | 36 | Representative evaluated programs |
| **Total** | **302** | **All pass** |

## Resolved Phase 5 Inventory

Every failure assigned to Phase 5 in the Phase 4 baseline is absent from the
two final complete-suite results:

| Resolved module | Phase 4 failures | Phase 5 result |
| --- | ---: | --- |
| `catena_process_tests` | 15 | Pass |
| **Total** | **15** | **Resolved** |

The process suite now has 26 passing tests after adding native OTP call
coverage and correcting invalid assumptions about dead-PID sends, link
ownership, monitor results, name cleanup, dynamic receive, and exit signals.

## Remaining Deterministic Failure Inventory

| Later phase | Failing module | Failures |
| --- | --- | ---: |
| Phase 6: Property and Law Status Reconciliation | `catena_stdlib_laws_tests` | 2 |
|  | **Total** | **2** |

The two deterministic failures are:

- `generic_bridge_law_suite_executes_test`
- `generic_bridge_reports_explicit_errors_test`

## Observed Phase 6 Flake

A preliminary complete-suite run reported 4,821 passing and 3 failing because
`catena_stdgen_collection_tests:gen_map_of_with_range_test` failed once. The
generated list met the requested entry-count range, but duplicate keys reduced
the resulting map below its asserted minimum size. The test passed in both
consecutive final runs.

This is retained as Phase 6 work because it belongs to collection-generator
semantics and reproducibility, not the runtime/REPL/actor boundary. It is not
included in the deterministic two-failure inventory above.

## Interpretation

`catena_process` now presents stable Catena-facing lifecycle results without
hiding native BEAM messaging and signal semantics. Its dynamic-predicate
receive restores rejected messages, and native OTP calls pass requests through
without protocol double-wrapping.

The actor, GenServer-style, supervisor, registry, pub/sub, and broadcaster
modules are promoted as a tested local Erlang runtime toolkit. Their presence
does not imply that Catena actor declarations, typed protocols, actors as
effect handlers, full OTP strategies, or distributed actors are implemented.

The REPL's direct effect evaluator now recognizes canonical effect names and
executes all three operations declared by the standard Process effect:
`spawn`, `send`, and `self`. Generated code continues to use the separate
explicit-context effect runtime.

The complete active suite remains intentionally non-green because Phase 6 owns
the two deterministic law-bridge failures and the observed collection-generator
flake. None of that work is hidden by filtering or permissive assertions.
