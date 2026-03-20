# Compiler Contract

This contract defines the `REQ-COMP-*` family for Catena's compiler surfaces.

## Requirements

- `REQ-COMP-001`: The compiler MUST accept Catena source through a documented stage pipeline and either produce structured IR or typed diagnostics.
- `REQ-COMP-002`: The compiler MUST include semantic validation and desugaring as explicit stages, rather than burying those transformations inside unrelated passes.
- `REQ-COMP-003`: The compiler MUST provide Hindley-Milner-style type inference with trait constraints, kind checking for implemented higher-kinded usage, and explicit effect-set tracking for the proof-of-concept scope.
- `REQ-COMP-004`: The proof-of-concept compiler MUST treat effect polymorphism as deferred work rather than silently approximating it.
- `REQ-COMP-005`: The compiler MUST lower valid Catena modules toward Core Erlang and BEAM-compilable output through documented code-generation surfaces.
- `REQ-COMP-006`: Implemented pattern-matching features MUST be reflected consistently across parsing, typing, static analysis, and code generation.
- `REQ-COMP-007`: Partial module-system features that exist ahead of Phase 4 completion MUST remain explicitly scoped as minimal import support rather than being treated as full module-system completion.
