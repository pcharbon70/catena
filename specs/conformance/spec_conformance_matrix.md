# Spec Conformance Matrix

This matrix ties Catena's requirement families and promoted component specs to stable scenarios.

## Requirement Family Coverage

| Requirement Family | Source Contract | Covered By Scenarios |
| --- | --- | --- |
| `REQ-COMP-*` | `specs/contracts/compiler_contract.md` | `SCN-001`, `SCN-002`, `SCN-003`, `SCN-006`, `SCN-007`, `SCN-011`, `SCN-012` |
| `REQ-RT-*` | `specs/contracts/runtime_contract.md` | `SCN-003`, `SCN-004`, `SCN-005`, `SCN-007`, `SCN-012` |
| `REQ-STDLIB-*` | `specs/contracts/stdlib_contract.md` | `SCN-002`, `SCN-004`, `SCN-007`, `SCN-008`, `SCN-010` |
| `REQ-TEST-*` | `specs/contracts/testing_and_quality_contract.md` | `SCN-007`, `SCN-008`, `SCN-009`, `SCN-010`, `SCN-011`, `SCN-012` |
| `REQ-OBS-*` | `specs/contracts/observability_contract.md` | `SCN-001`, `SCN-005`, `SCN-007`, `SCN-009`, `SCN-012` |

## Component AC Coverage

| Component Spec | REQ Mapping | SCN Mapping |
| --- | --- | --- |
| `specs/compiler/core_compiler_pipeline.md` | `REQ-COMP-*`, `REQ-OBS-*` | `SCN-001`, `SCN-002`, `SCN-003`, `SCN-011` |
| `specs/compiler/core_erlang_and_beam_backend.md` | `REQ-COMP-*`, `REQ-TEST-*` | `SCN-003`, `SCN-006`, `SCN-011` |
| `specs/compiler/type_and_effect_system.md` | `REQ-COMP-*`, `REQ-STDLIB-*` | `SCN-002`, `SCN-007` |
| `specs/compiler/pattern_matching_engine.md` | `REQ-COMP-*`, `REQ-OBS-*` | `SCN-001`, `SCN-006` |
| `specs/compiler/delimited_resumption_architecture.md` | `REQ-COMP-*`, `REQ-RT-*`, `REQ-OBS-*`, `REQ-TEST-*` | `SCN-012` |
| `specs/runtime/effect_runtime.md` | `REQ-RT-*`, `REQ-STDLIB-*`, `REQ-OBS-*` | `SCN-003`, `SCN-004`, `SCN-007`, `SCN-012` |
| `specs/runtime/actor_runtime.md` | `REQ-RT-*`, `REQ-OBS-*` | `SCN-004`, `SCN-005`, `SCN-009` |
| `specs/runtime/repl_runtime.md` | `REQ-RT-*`, `REQ-OBS-*` | `SCN-005`, `SCN-007`, `SCN-012` |
| `specs/stdlib/standard_library_surface.md` | `REQ-STDLIB-*`, `REQ-COMP-*` | `SCN-002`, `SCN-003`, `SCN-007`, `SCN-010` |
| `specs/stdlib/flow_core_surface.md` | `REQ-STDLIB-*`, `REQ-COMP-*` | `SCN-002`, `SCN-003`, `SCN-007`, `SCN-010` |
| `specs/stdlib/property_testing_framework.md` | `REQ-STDLIB-*`, `REQ-TEST-*` | `SCN-008`, `SCN-009`, `SCN-010` |
| `specs/tooling/build_and_test_tooling.md` | `REQ-TEST-*`, `REQ-OBS-*` | `SCN-001`, `SCN-009`, `SCN-012` |
