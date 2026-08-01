# Catena Control Planes

This document names the primary decision planes that shape the current Catena repository.

## Primary Planes

### Language Plane

Owns the language-facing design:

- core syntax
- semantic intent
- research-driven feature direction
- long-term surface evolution

### Compiler Plane

Owns the implementation of static language processing:

- parsing and AST construction
- desugaring and semantic validation
- kind, type, trait, and effect analysis
- pattern compilation and code generation

### Runtime Plane

Owns execution semantics that the compiler targets:

- effect runtime behavior
- REPL runtime behavior
- runtime bindings and BEAM-facing execution helpers

### Library Plane

Owns abstractions that should exist as library surfaces rather than compiler magic:

- category-theory traits and instances
- laws and testing helpers
- effect modules
- internal property-testing library work

### Quality Plane

Owns the delivery mechanics that keep the repo operable:

- build hooks
- test organization
- regression and hardening expectations
- traceability and current-status reconciliation

## Tie-Breaker

When ownership is ambiguous, use this order:

1. [contracts/control_plane_ownership_matrix.md](contracts/control_plane_ownership_matrix.md)
2. [adr/ADR-0001-control-plane-authority.md](adr/ADR-0001-control-plane-authority.md)
3. the domain/component specs for the affected surface
