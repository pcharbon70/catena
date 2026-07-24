# Compiler Contract

This contract defines the `REQ-COMP-*` family for Catena's compiler surfaces.

## Requirements

- `REQ-COMP-001`: The compiler MUST accept Catena source through a documented stage pipeline and either produce structured IR or typed diagnostics.
- `REQ-COMP-002`: The compiler MUST include semantic validation and desugaring as explicit stages, rather than burying those transformations inside unrelated passes.
- `REQ-COMP-003`: The compiler MUST provide Hindley-Milner-style type inference with trait constraints, kind checking for implemented higher-kinded usage, and explicit effect tracking across both the proof-of-concept core and the implemented algebraic-effects row surfaces.
- `REQ-COMP-004`: Where the compiler exposes algebraic-effects machinery, it MUST preserve handler removal, effect constraints, and implemented row-polymorphic behavior rather than silently erasing or approximating effect obligations.
- `REQ-COMP-005`: The compiler MUST lower valid Catena modules to
  BEAM-compilable Core Erlang through documented public APIs that run semantic,
  kind, import, and type validation before code generation.
- `REQ-COMP-006`: Implemented pattern-matching features MUST be reflected consistently across parsing, typing, static analysis, and code generation.
- `REQ-COMP-007`: Partial module-system features that exist ahead of Phase 4 completion MUST remain explicitly scoped as minimal import support rather than being treated as full module-system completion.
- `REQ-COMP-008`: Public Core Erlang and BEAM artifact generation MUST run only after lexical, syntactic, semantic, import, kind, type, trait, and effect validation, and the backend input MUST retain the symbol, type, disposition, and source-location information needed for sound lowering.
- `REQ-COMP-009`: Backend call resolution MUST distinguish local transforms, imported transforms, higher-order values, constructors, and trait methods; supported forward references, self-recursion, and mutual recursion MUST produce valid targets with resolved arities.
- `REQ-COMP-010`: Every declaration, expression, pattern, and operator reaching code generation MUST be explicitly lowered, intentionally static-erased, runtime-lowered, or rejected; unknown constructs MUST NOT become placeholder values, wildcard patterns, arbitrary BIF calls, or silently omitted runtime behavior.
- `REQ-COMP-011`: The backend MUST preserve the documented BEAM representation and observable semantics of every promoted supported construct, including explicit runtime boundaries for effects and trait dispatch.
- `REQ-COMP-012`: The compiler MUST expose a validated in-memory source-to-BEAM API whose success result is produced only after OTP accepts the emitted Core Erlang with `from_core`; Core and BEAM diagnostics MUST be returned as structured compiler failures.
- `REQ-COMP-013`: Backend diagnostics MUST identify unresolved, ambiguous, unsupported, invalidly erased, Core-invalid, and BEAM-invalid constructs with Catena source identity and location whenever available.
