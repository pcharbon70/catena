# Requirements Catalog

This catalog is the canonical inventory of Catena's promoted `REQ-*` requirement families.

It complements the contract files by giving contributors a single place to answer:

- which requirement families currently exist
- which contract owns each family
- which component specs and scenarios are expected to cover them

## Requirement Families

| Requirement Family | Source Contract | Primary Scope | Component Coverage | Scenario Coverage |
| --- | --- | --- | --- | --- |
| `REQ-COMP-*` | [compiler_contract.md](compiler_contract.md) | compiler pipeline, semantic normalization, typing, effects, fail-closed Core Erlang/BEAM backend, call resolution, imports, selective CPS and proof-gated control optimization | [../compiler/core_compiler_pipeline.md](../compiler/core_compiler_pipeline.md), [../compiler/core_erlang_and_beam_backend.md](../compiler/core_erlang_and_beam_backend.md), [../compiler/type_and_effect_system.md](../compiler/type_and_effect_system.md), [../compiler/pattern_matching_engine.md](../compiler/pattern_matching_engine.md), [../compiler/delimited_resumption_architecture.md](../compiler/delimited_resumption_architecture.md) | `SCN-001`, `SCN-002`, `SCN-003`, `SCN-006`, `SCN-007`, `SCN-011`, `SCN-012` |
| `REQ-RT-*` | [runtime_contract.md](runtime_contract.md) | explicit effect runtime, handler execution, resumption authority, REPL/runtime behavior | [../runtime/effect_runtime.md](../runtime/effect_runtime.md), [../runtime/repl_runtime.md](../runtime/repl_runtime.md) | `SCN-003`, `SCN-004`, `SCN-005`, `SCN-007`, `SCN-012` |
| `REQ-STDLIB-*` | [stdlib_contract.md](stdlib_contract.md) | library-first language surface, stdlib modules, desugaring targets, property-testing direction | [../stdlib/standard_library_surface.md](../stdlib/standard_library_surface.md), [../stdlib/flow_core_surface.md](../stdlib/flow_core_surface.md), [../stdlib/property_testing_framework.md](../stdlib/property_testing_framework.md), [../compiler/type_and_effect_system.md](../compiler/type_and_effect_system.md) | `SCN-002`, `SCN-004`, `SCN-007`, `SCN-008`, `SCN-010` |
| `REQ-TEST-*` | [testing_and_quality_contract.md](testing_and_quality_contract.md) | build generation, test organization, workflow quality, migration boundaries, executable governance, backend and resumption conformance | [../tooling/build_and_test_tooling.md](../tooling/build_and_test_tooling.md), [../stdlib/property_testing_framework.md](../stdlib/property_testing_framework.md), [../compiler/core_erlang_and_beam_backend.md](../compiler/core_erlang_and_beam_backend.md), [../compiler/delimited_resumption_architecture.md](../compiler/delimited_resumption_architecture.md) | `SCN-007`, `SCN-008`, `SCN-009`, `SCN-010`, `SCN-011`, `SCN-012` |
| `REQ-OBS-*` | [observability_contract.md](observability_contract.md) | status traceability, reconciled documentation, safe resumption/control diagnostics, executable evidence mapping | [../compiler/core_compiler_pipeline.md](../compiler/core_compiler_pipeline.md), [../compiler/pattern_matching_engine.md](../compiler/pattern_matching_engine.md), [../compiler/delimited_resumption_architecture.md](../compiler/delimited_resumption_architecture.md), [../runtime/repl_runtime.md](../runtime/repl_runtime.md), [../tooling/build_and_test_tooling.md](../tooling/build_and_test_tooling.md) | `SCN-001`, `SCN-005`, `SCN-007`, `SCN-009`, `SCN-012` |

## Catalog Rules

1. Add a new row here whenever a new `REQ-*` family is introduced.
2. Keep this catalog aligned with the owning contract file and [../conformance/spec_conformance_matrix.md](../conformance/spec_conformance_matrix.md).
3. Do not list ad hoc one-off requirement IDs here unless they belong to a stable family promoted by a contract.
4. When a requirement family materially changes scope, update both this catalog and the affected component specs in the same change set.
