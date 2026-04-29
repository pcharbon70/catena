# Catena Design Baseline

Catena is a category theory-inspired functional programming language targeting the BEAM VM.

The research corpus under `specs/research/` covers a much larger design space than the codebase currently implements. This document records the current promoted design baseline that is both visible in the notes and materially reflected in the repository.

## Stable Design Themes

### Language Shape

- Catena is a functional language with `type`, `transform`, `trait`, `instance`, `effect`, `perform`, `handle`, and `module` as core surface constructs.
- Category-theoretic abstractions are primarily library-defined rather than compiler-baked.
- Syntax and semantics are oriented around composition, explicit structure, and strong algebraic reasoning.

### Compiler Direction

- the implementation is written in Erlang
- lexer and parser generation use `leex` and `yecc`
- the compiler pipeline is `lexer -> parser -> semantic/desugar -> kind checking -> type/effect inference -> code generation`
- code generation targets Core Erlang and then BEAM compilation

### Type And Effect Direction

- the current proof-of-concept uses Hindley-Milner-style inference
- trait constraints and higher-kinded type validation are part of the current design
- effects are tracked explicitly in function types
- the repo now includes implemented algebraic-effects machinery in the compiler/type layers, including row-polymorphism-oriented effect operations and validation surfaces

### Runtime Direction

- effect execution is process-based and BEAM-native
- explicit effect context passing is the preferred runtime model
- an interactive REPL exists and reuses the compiler pipeline for inspection and evaluation-oriented workflows

### Library Direction

- the standard library defines the category-theory surface in Catena syntax
- `Prelude`, `Test`, `Laws`, and effect modules are part of the current library surface
- an internal property-testing framework is being built inside the repo, with rose-tree, generator, and range foundations implemented in Erlang

## Current Implemented Baseline

The repository currently contains real code and tests for:

- Phase 1 compiler infrastructure through code generation and effect runtime
- substantial standard-library validation work
- REPL, prelude, and testing framework work associated with Phase 2
- advanced pattern features, decision trees, and exhaustiveness/redundancy checking associated with Phase 3
- a newer algebraic-effects track through Phase 14-style orchestration, validation, and integration-test surfaces
- an implemented property-testing foundation spanning `src/proptest/catena_tree.erl`, `src/proptest/catena_gen.erl`, and `src/proptest/catena_range.erl`

## Still Planned Or Partial

- full Phase 4 module system beyond basic imports
- actor model integration as a first-class Catena surface
- full language-surface consolidation and long-tail ergonomics for the newer algebraic-effects machinery
- the remaining generator, runner, and law-testing layers of the internal property-testing framework
