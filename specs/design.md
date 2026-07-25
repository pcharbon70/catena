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
- the promoted backend direction is fail-closed: every construct is lowered,
  intentionally erased, runtime-lowered, or rejected before artifact success

### Type And Effect Direction

- the current proof-of-concept uses Hindley-Milner-style inference
- trait constraints and higher-kinded type validation are part of the current design
- effects are tracked explicitly in function types
- the repo now includes implemented algebraic-effects machinery in the compiler/type layers, including row-polymorphism-oriented effect operations and validation surfaces
- [ADR-0006](adr/ADR-0006-first-class-resumptions-and-selective-cps.md)
  accepts first-class `Resumption` values, explicit `with`/`resume` control,
  and effect-directed selective CPS as the path to true delimited handlers;
  the syntax, AST, and normalization frontend is implemented, while typing,
  selective CPS, and executable explicit resumptions remain planned

### Runtime Direction

- current effect execution is process-based and BEAM-native
- explicit effect context passing is the preferred runtime model
- future resumable source handlers execute compiler-reified continuations on
  the originating BEAM process while retaining explicit contexts as
  handler-lookup authority
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
- true delimited source-level resumptions through the accepted selective-CPS
  architecture
- complete source-to-BEAM semantic coverage, including resolved named calls,
  exhaustive lowering, and a public validated BEAM artifact API
- the remaining generator, runner, and law-testing layers of the internal property-testing framework
