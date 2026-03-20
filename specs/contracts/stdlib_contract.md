# Standard Library Contract

This contract defines the `REQ-STDLIB-*` family for Catena's standard-library and library-first language surfaces.

## Requirements

- `REQ-STDLIB-001`: Category-theory abstractions SHOULD be expressed as library surfaces whenever they do not require compiler-enforced syntax or typing hooks.
- `REQ-STDLIB-002`: The prelude MUST define the core traits, types, and baseline instances that the current compiler/runtime work depends on.
- `REQ-STDLIB-003`: Test and law-verification surfaces MUST remain expressible as Catena library modules, even when their execution or verification depth is still partial.
- `REQ-STDLIB-004`: Standard-library effect modules MUST be representable in Catena syntax and align with the compiler's current effect and module capabilities.
- `REQ-STDLIB-005`: Syntax sugar such as operators or do-notation MAY be implemented in the compiler, but their semantic destination SHOULD remain library-defined where possible.
- `REQ-STDLIB-006`: The in-repo property-testing framework SHOULD replace long-term external law-verification dependencies, but transition gaps MUST be explicit.
