# Specs Index

This `specs/` tree is the canonical architecture-and-governance layer for the repository. It provides the promoted view of what Catena currently is, how its main parts fit together, and where the project is actually up to.

The structure follows the layered model used by `../epic/jido_os/specs`, adapted to a language/compiler project:

- baseline architecture docs describe the current system shape
- contracts define normative `REQ-*` families
- ADRs capture architectural tie-breakers already implicit in the repo
- conformance maps requirements to stable scenarios
- planning captures the reconciled current status
- component specs describe the implemented compiler, runtime, library, and tooling surfaces

## How The Pieces Fit Together

1. Baseline docs define Catena's architecture, boundaries, and current targets.
2. Contracts define what the major surfaces are expected to guarantee.
3. ADRs record the design choices that shape multiple parts of the repo.
4. Conformance gives each requirement family scenario-level coverage.
5. Domain/component specs map the actual codebase and current implementation state onto that architecture.

## Getting Started

- [getting-started.md](getting-started.md)

## Baseline Architecture

- [design.md](design.md)
- [topology.md](topology.md)
- [boundaries.md](boundaries.md)
- [control_planes.md](control_planes.md)
- [services-and-libraries.md](services-and-libraries.md)
- [targets.md](targets.md)
- [specs-governance-and-compliance-guide.md](specs-governance-and-compliance-guide.md)

## Contracts

- [contracts/README.md](contracts/README.md)
- [contracts/requirements_catalog.md](contracts/requirements_catalog.md)
- [contracts/control_plane_ownership_matrix.md](contracts/control_plane_ownership_matrix.md)
- [contracts/compiler_contract.md](contracts/compiler_contract.md)
- [contracts/runtime_contract.md](contracts/runtime_contract.md)
- [contracts/stdlib_contract.md](contracts/stdlib_contract.md)
- [contracts/testing_and_quality_contract.md](contracts/testing_and_quality_contract.md)
- [contracts/observability_contract.md](contracts/observability_contract.md)

## ADRs

- [adr/README.md](adr/README.md)
- [adr/adr_catalog.md](adr/adr_catalog.md)
- [adr/ADR-0001-control-plane-authority.md](adr/ADR-0001-control-plane-authority.md)
- [adr/ADR-0002-minimal-core-and-library-first-surface.md](adr/ADR-0002-minimal-core-and-library-first-surface.md)
- [adr/ADR-0003-explicit-effect-context-runtime.md](adr/ADR-0003-explicit-effect-context-runtime.md)
- [adr/ADR-0004-flow-as-arrow.md](adr/ADR-0004-flow-as-arrow.md)

## Conformance

- [conformance/scenario_catalog.md](conformance/scenario_catalog.md)
- [conformance/spec_conformance_matrix.md](conformance/spec_conformance_matrix.md)

## Planning And Operations

- [planning/README.md](planning/README.md)
- [planning/current_status.md](planning/current_status.md)
- [planning/law_verification_staged_plan.md](planning/law_verification_staged_plan.md)
- [operations/README.md](operations/README.md)

## Research

- [research/README.md](research/README.md)

## Domain Indexes

- [compiler/README.md](compiler/README.md)
- [runtime/README.md](runtime/README.md)
- [stdlib/README.md](stdlib/README.md)
- [tooling/README.md](tooling/README.md)
