# Services And Libraries

Catena is not a service platform in the same way `jido_os` is, but it does have stable reusable subsystems and library surfaces that behave like first-class internal services.

## Current Reusable Layers

### Compiler Subsystems

- `catena_compile` as the pipeline orchestrator
- parser and lexer surfaces
- semantic/desugar surfaces
- type, trait, and effect analysis modules
- code generation surfaces

### Runtime Subsystems

- `catena_effect_runtime` as the process-based effect execution boundary
- `catena_repl` as the interactive entry point

### Library Surfaces

- `lib/catena/stdlib/prelude.cat`
- `lib/catena/stdlib/test.cat`
- `lib/catena/stdlib/laws.cat`
- `lib/catena/stdlib/effect/*.cat`
- Erlang runtime bindings in `src/stdlib` and `src/testing`
- rose-tree property-testing foundation in `src/proptest`

### Tooling Surfaces

- build scripts that regenerate generated sources
- EUnit and integration test layout
- documentation and planning workflow around `notes/`

## Current Design Bias

Catena prefers:

- library-defined abstractions over compiler special cases
- explicit runtime boundaries over hidden process-global behavior
- layered promotion from research notes to canonical specs

That bias should guide future component specs and new subsystem design.
