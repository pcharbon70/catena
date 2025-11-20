# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Catena is a category theory-inspired functional programming language for the BEAM VM. It's currently in early development with a proof-of-concept compiler implementation focusing on Phase 1 (Core Language Infrastructure). The project uses Erlang for the compiler implementation, with leex/yecc for lexer/parser generation.

**Etymology**: Catena (Latin) means "chain" or "series of connected links" - reflecting the language's emphasis on composition where functions, types, and effects are chained together through category-theoretic principles.

## Build System

### Quick Start Commands

```bash
# Build lexer and parser (automatically runs before compile)
./scripts/build.sh

# Compile all source modules
make compile

# Run all tests
make test

# Run tests with coverage reporting
make coverage

# View coverage summary
make coverage-report

# Clean build artifacts
make clean

# Using rebar3 (runs build.sh automatically via pre-hooks)
rebar3 compile
rebar3 eunit
```

### Build Architecture

The build system has a two-stage process:

1. **Generated Code** (via leex/yecc):
   - `src/compiler/lexer/catena_lexer.xrl` → generates `catena_lexer.erl`
   - `src/compiler/parser/catena_parser.yrl` → generates `catena_parser.erl`
   - These are excluded from git (see `.gitignore`) and regenerated on each build

2. **Compilation** (via erlc or rebar3):
   - Source files expected in `src/compiler/` hierarchy (currently only test files exist)
   - Test files in `test/compiler/` organized by component

The build scripts (`scripts/build.sh`, `scripts/build_lexer.sh`, `scripts/build_parser.sh`) include timestamp checking and only rebuild when source files have changed.

## Test Organization

Tests are organized by compiler component:

- `test/compiler/lexer/` - Lexer tests including property-based tests
- `test/compiler/parser/` - Parser tests for syntax validation, AST construction, error handling
- `test/compiler/types/` - Type system tests (type inference, unification, constraints, effects)
- `test/compiler/error/` - Error reporting and formatting tests
- `test/compiler/ast/` - AST utilities and traversal tests
- `test/compiler/integration/` - End-to-end integration tests

### Running Specific Tests

```bash
# Run specific test module via eunit
rebar3 eunit --module=catena_parser_effect_tests

# Run property-based tests with PropEr
rebar3 proper -m catena_parser_properties
rebar3 proper -m catena_lexer_properties

# Run property-based test with specific count
rebar3 proper -m catena_parser_properties -n 1000
```

### Test Coverage

- Target: ≥90% coverage on modified modules
- Current: ~86% overall for compiler modules
- HTML reports generated in `_build/test/cover/` after running `make coverage`
- Generated modules (lexer/parser) are excluded from coverage metrics

## Module Naming Convention

All modules follow the `catena_*` naming pattern:

- **Type System**: `catena_types`, `catena_type_subst`, `catena_type_env`, `catena_type_scheme`, `catena_infer`, etc.
- **Parser/Lexer**: `catena_parser`, `catena_lexer` (generated)
- **Tests**: Mirror source module names with `_tests` suffix (e.g., `catena_types_tests.erl`)
- **Properties**: Property-based tests use `_properties` suffix (e.g., `catena_parser_properties.erl`)

## Compiler Architecture

The Catena compiler follows a traditional multi-pass architecture:

### Phase 1: Lexer and Parser (✓ Complete)
- **Lexer** (leex): Tokenizes source code including keywords (`type`, `transform`, `effect`, `trait`), operators (`|>`, `>>=`, `<$>`, `<*>`), and literals
- **Parser** (yecc): Builds AST from tokens, handles operator precedence, supports effect syntax, trait system declarations
- **Error Recovery**: Panic-mode recovery for syntax errors with helpful messages

### Phase 2: Type System (In Progress)
- **Type Representation** (`catena_types`): Core type structures including type variables, constructors, functions, records, effects
- **Type Inference** (`catena_infer`): Hindley-Milner type inference with effect tracking
  - Modular inference: `catena_infer_expr`, `catena_infer_pattern`, `catena_infer_effect`
  - Unification: `catena_infer_unify` with occurs check
  - State management: `catena_infer_state` for fresh variable generation and constraint accumulation
- **Constraint Solving** (`catena_constraint`): Type class constraints and trait instance resolution
- **Effect System**: Algebraic effects with monomorphic effect tracking (polymorphism deferred to Phase 6)

### Key Type System Concepts

- **Effect Tracking**: Functions are typed as `a -> b / {Effect1, Effect2}` where the effect set tracks side effects
- **Trait System**: Category theory abstractions (Functor, Monad, etc.) implemented as traits with principled resolution
- **Row Types**: Extensible records with row polymorphism for effect sets
- **Constraint Solving**: Two-phase approach separating constraint generation from solving

## Implementation Phases

The project follows a 6-phase proof-of-concept plan (see `notes/planning/proof-of-concept/`):

- **Phase 1** (6.5 weeks, ✓ Complete): Lexer, Parser, Type System, Effect Runtime, Code Generation
- **Phase 2** (3.5 weeks): REPL and Basic Runtime
- **Phase 3** (3.5 weeks): Pattern Matching Engine
- **Phase 4** (2.5 weeks): Module System
- **Phase 5** (3.5 weeks): Actor Model Integration
- **Phase 6** (5 weeks): Effect System Completion (polymorphism, optimizations)

Current focus is on completing Phase 1 type system implementation.

## Language Syntax Examples

```catena
-- Type (algebraic data type)
type User = {
  name: Text,
  age: Natural,
  email: Email
} derives [Eq, Show, Mappable]

-- Transform (pure function)
transform greet : User -> Text
transform greet user = "Hello, " <> user.name

-- Effect declaration
effect FileIO {
  operation read(path: String): String
  operation write(path: String, content: String): Unit
}

-- Function with effects
transform process_file : String -> String / {FileIO}
transform process_file path =
  let content = perform FileIO.read(path)
  in content |> process |> validate

-- Trait declaration
trait Functor f where
  fmap : (a -> b) -> f a -> f b

-- Instance declaration
instance Functor Maybe where
  fmap f = match
    | None -> None
    | Some x -> Some (f x)
  end
```

## Key Operators

Catena uses dual notation - keywords and symbolic operators:

- **Equality**: `===`, `!==` (Setoid)
- **Composition**: `|>` (pipe), `>>=` (bind), `>=>` (Kleisli composition)
- **Functor**: `<$>` (fmap) or `map`
- **Applicative**: `<*>` (apply)
- **Semigroup**: `<>` (append)

## Documentation Structure

- `notes/planning/` - Project planning and phase documentation
- `notes/research/` - Language design research (modules, effects, type system, etc.)
- `notes/implementation/` - Implementation notes and decisions
- `notes/guides/` - Developer guides (test coverage, naming conventions, etc.)
- `notes/summaries/` - Session summaries and completed work
- `notes/reviews/` - Code review notes

## PropEr Property-Based Testing

The project uses PropEr for property-based testing:

- Generators defined in `*_properties.erl` modules
- Test properties with invariants (roundtrip parsing, never crashes, valid AST generation)
- Configure test runs via `rebar.config` (`{numtests, 100}`, `{max_size, 20}`)
- Run with `rebar3 proper -m module_name`

## Important Notes

- **No src/ directory yet**: Currently only test files exist. Source files will be added as Phase 1 completes.
- **Generated files**: Never edit `catena_lexer.erl` or `catena_parser.erl` directly - modify `.xrl` and `.yrl` sources.
- **Module references**: When renaming affects module calls, update both file names and `-module()` declarations.
- **Build dependency**: Parser depends on lexer, so lexer must be built first (handled by `scripts/build.sh`).
- **Coverage exclusions**: Generated modules and demo scripts are excluded from coverage (see `rebar.config`).
