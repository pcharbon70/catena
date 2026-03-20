# Catena Topology

## Repository-Level Topology

Catena currently behaves like a language-toolchain repository with four major domains:

- `compiler`: `src/compiler/*`
- `runtime`: `src/compiler/runtime`, `src/repl`
- `library`: `lib/catena/stdlib`, `src/stdlib`, `src/testing`, `src/proptest`
- `tooling`: `scripts/`, `Makefile`, `rebar.config`, and the test tree

## Compiler And Runtime Flow

```text
Catena source
  -> lexer (catena_lexer)
  -> parser (catena_parser)
  -> semantic analysis + desugaring
  -> kind checking
  -> type/effect inference
  -> Core Erlang generation
  -> BEAM compilation/loading
  -> runtime/effect execution and REPL-facing use
```

## Current Concrete Surfaces

### Compiler Domain

- AST utilities and pretty-printing
- parser wrapper and source-location handling
- semantic passes including do-notation/operator desugaring
- type, trait, instance, coherence, and effect modules
- code generation modules including pattern compilation and decision trees
- minimal import processing through `catena_module_loader` and `catena_compile`

### Runtime Domain

- explicit-context effect runtime
- builtin IO and Process handlers
- REPL loop, command handling, prelude loading, and typed inspection

### Library Domain

- Catena-side stdlib modules: `Prelude`, `Test`, `Laws`, effect modules
- Erlang-side runtime prelude helpers
- basic testing framework support
- internal rose-tree property-testing foundation

### Tooling Domain

- build hooks that regenerate lexer and parser
- subsystem-organized EUnit tests
- integration tests for compiler, REPL, stdlib, runtime, and proptest work

## Current Boundary Of The Module System

The repository has a basic import loader and exported-environment wiring, but not the full Phase 4 module system. The canonical topology should treat current import resolution as a minimal bridge rather than evidence that the full module system is complete.
