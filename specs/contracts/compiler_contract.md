# Compiler Contract

This contract defines the `REQ-COMP-*` family for Catena's compiler surfaces.

## Requirements

- `REQ-COMP-001`: The compiler MUST accept Catena source through a documented stage pipeline and either produce structured IR or typed diagnostics.
- `REQ-COMP-002`: The compiler MUST include semantic validation and desugaring as explicit stages, rather than burying those transformations inside unrelated passes.
- `REQ-COMP-003`: The compiler MUST provide Hindley-Milner-style type inference with trait constraints, kind checking for implemented higher-kinded usage, and explicit effect tracking across both the proof-of-concept core and the implemented algebraic-effects row surfaces.
- `REQ-COMP-004`: Where the compiler exposes algebraic-effects machinery, it MUST preserve handler removal, effect constraints, and implemented row-polymorphic behavior rather than silently erasing or approximating effect obligations.
- `REQ-COMP-005`: The compiler MUST lower valid Catena modules toward Core Erlang and BEAM-compilable output through documented code-generation surfaces.
- `REQ-COMP-006`: Implemented pattern-matching features MUST be reflected consistently across parsing, typing, static analysis, and code generation.
- `REQ-COMP-007`: Partial module-system features that exist ahead of Phase 4 completion MUST remain explicitly scoped as minimal import support rather than being treated as full module-system completion.
