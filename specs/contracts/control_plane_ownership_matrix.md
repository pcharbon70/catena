# Control Plane Ownership Matrix

This matrix provides the current ownership map for Catena's promoted architecture.

| Surface | Primary Plane | Canonical Spec |
| --- | --- | --- |
| Core language semantics and surface evolution | Language Plane | `specs/design.md` |
| Compiler pipeline and static analysis behavior | Compiler Plane | `specs/contracts/compiler_contract.md` |
| Effect execution and REPL runtime behavior | Runtime Plane | `specs/contracts/runtime_contract.md` |
| Prelude, laws, testing, and property-testing library surfaces | Library Plane | `specs/contracts/stdlib_contract.md` |
| Build, test, regression, and transition-quality workflow | Quality Plane | `specs/contracts/testing_and_quality_contract.md` |
| Diagnostics, traceability, and status reconciliation | Quality Plane | `specs/contracts/observability_contract.md` |
| Cross-plane authority tie-breaks | Shared | `specs/adr/ADR-0001-control-plane-authority.md` |
