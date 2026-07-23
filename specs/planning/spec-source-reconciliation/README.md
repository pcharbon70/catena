# Spec-Source Reconciliation

This roadmap aligns Catena's promoted specifications with executable repository
evidence. Work proceeds sequentially: stabilize the build and test baseline,
repair promoted compiler and runtime integrations, then add executable
conformance checks that keep the two views synchronized.

## Status Rules

- Planned work uses unchecked task and section boxes.
- A task is complete only after its implementation and focused validation pass.
- A section is complete only after its tasks pass and the relevant status
  documentation records the verified result.
- A phase is complete only after its integration gate passes.

## Phases

- [x] [Phase 1: Build and Test Baseline](phase-01-build-and-test-baseline.md)
- [x] [Phase 2: Standard Library and Frontend Validation](phase-02-standard-library-and-frontend-validation.md)
- Phase 3: Compiler, Codegen, and Pattern Integration
- Phase 4: Type and Effect Integration Boundary
- Phase 5: Runtime, REPL, and Actor Boundaries
- Phase 6: Property and Law Status Reconciliation
- Phase 7: Executable Conformance and Governance
