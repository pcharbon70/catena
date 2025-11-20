# Catena

> A category theory-inspired functional programming language for the BEAM VM

## Etymology

**Catena** (Latin): literally "chain" or "series of connected links."

Figuratively, a catena represents an unbroken sequence of related elements, each linked to form a coherent whole. In logic and mathematics, it describes a chain of reasoning where each step follows necessarily from the last. This reflects the language's emphasis on composition—where functions, types, and effects are chained together through category-theoretic principles to build reliable, fault-tolerant systems.

## Overview

Catena is a research project exploring the design of a programming language that unifies category theory's mathematical rigor with the BEAM VM's legendary fault tolerance and concurrency capabilities. The language aims to make abstract mathematical concepts practical and accessible while leveraging the proven strengths of Erlang's runtime.

## Core Philosophy

Catena reimagines functional programming by making category theory concepts first-class language features rather than academic curiosities:

- **Shapes** instead of types - intuitive terminology for algebraic data types
- **Flows** instead of functions - emphasizing data transformation and composition
- **Composition as foundation** - the pipe operator `|>` and Kleisli composition `>>=` as primary abstractions
- **Immutability everywhere** - all data structures are immutable by default with persistent data structures
- **Actor model integration** - seamless blend of pure functional programming with BEAM's process model

## Key Features

### Category Theory Made Practical

- **Functors, Applicatives, and Monads** - simplified as "Mappable" and "Chainable" traits
- **Natural transformations** - first-class support for transforming between functors
- **Explicit effect system** - computational effects tracked through monads
- **Categorical laws** - property-based testing for algebraic properties

### BEAM-First Design

- **Process modules** - stateful computations as OTP processes
- **Supervision trees** - declarative fault tolerance
- **Hot code reloading** - leveraging BEAM's capabilities with pure function guarantees
- **Hex.pm integration** - seamless interoperability with Erlang/Elixir ecosystem

### Advanced Module System

- **ML-style parameterized modules (functors)** - inspired by OCaml and Standard ML
- **Three-level visibility control** - private, export, and public export
- **Signature-based abstraction** - interfaces separate from implementations
- **Category-as-module** - modules literally form categories with types as objects

### Developer Experience

- **Mandatory documentation** - first-class language construct with runtime introspection
- **Built-in testing** - property-based testing, doctests, and categorical law verification
- **Time-travel debugging** - leveraging immutability for reversible execution
- **FFI with safety** - category theory-based foreign function interface

### Standard Library: Category Theory Foundations

- **17 core abstractions** - Setoid, Ord, Semigroup, Monoid, Semigroupoid, Category, Functor, Apply, Applicative, Chain, Monad, Foldable, Traversable, Bifunctor, Extend, Comonad, Arrow
- **Dual notation system** - every operator has a readable keyword equivalent (`map`/`<$>`, `append`/`<>`, `bind`/`>>=`)
- **Beginner-friendly explanations** - complex mathematical concepts made accessible
- **Law verification** - automated property testing ensures implementations satisfy algebraic laws
- **Trait-based design** - general abstraction mechanism with `trait`, `instance`, `extends` keywords
- **Hierarchical organization** - Algebraic Structures → Categorical Structures → Functorial Hierarchy → Data Operations

### Distribution Layer: Partisan Integration

- **Scalable distribution** - replace Distributed Erlang's O(n²) full-mesh with pluggable topologies
- **Category-theoretic foundations** - network topologies as categories, membership operations as functors, messages as natural transformations
- **Named channels** - eliminate head-of-line blocking with multiple logical connections (12.5-38x throughput improvements)
- **Massive scale** - HyParView protocol enables 2,000+ node clusters vs Distributed Erlang's 60-200 node limit
- **Production-proven** - powers 30M+ daily GPS transmissions across 300K vehicles
- **Pluggable topologies** - full-mesh, peer-to-peer (HyParView), client-server, or custom overlays
- **Effect system integration** - topology selection and channel management through algebraic effects
- **Session types** - protocol-safe distributed communication with compile-time guarantees

## Project Status

⚠️ **This is a research project in early design phase**

Currently, the repository contains extensive research and design documentation exploring various language features. A proof-of-concept compiler is under active development.

## Building

The Catena compiler uses leex (lexer) and yecc (parser) to generate Erlang modules from grammar definitions.

### Quick Start

```bash
# Build lexer and parser
./scripts/build.sh

# Build individual components
./scripts/build_lexer.sh   # Generate lexer from .xrl
./scripts/build_parser.sh  # Generate parser from .yrl
```

### Using rebar3

The build scripts are automatically invoked as pre-compile hooks:

```bash
rebar3 compile  # Automatically builds lexer and parser, then compiles
rebar3 eunit    # Run unit tests
```

### Build Scripts

- **`scripts/build.sh`** - Master build script (builds lexer + parser)
- **`scripts/build_lexer.sh`** - Generates `src/compiler/lexer/catena_lexer_gen.erl` from `catena_lexer.xrl`
- **`scripts/build_parser.sh`** - Generates `src/compiler/parser/catena_parser.erl` from `catena_parser.yrl`

All scripts include timestamp checking and only rebuild when source files have changed.

## Documentation

Comprehensive research documents are available in [`notes/research/`](notes/research/):

- **[1.01 - Original Idea](notes/research/1.01-original_idea/1.01.1-original_idea.md)** - Core design philosophy
- **[1.02 - Modules](notes/research/1.02-modules/1.02.1-modules.md)** - Advanced module system design
- **[1.03 - Immutability](notes/research/1.03-immutability/1.03.1-immutability.md)** - Immutability as foundation
- **[1.04 - Documentation](notes/research/1.04-documentation/1.04.1-documentation.md)** - Documentation as language construct
- **[1.05 - Packages](notes/research/1.05-packages/1.05.1-packages.md)** - Ecosystem integration
- **[1.06 - Testing](notes/research/1.06-testing/1.06.1-testing.md)** - Built-in testing framework
- **[1.07 - Error Handling](notes/research/1.07-error-handling/1.07.1-error-handling.md)** - Type systems meet "let it crash"
- **[1.08 - Advanced Concurrency](notes/research/1.08-advanced-concurrency/1.08.1-advanced-concurrency.md)** - Session types and choreographic programming
- **[1.09 - Reflection and Introspection](notes/research/1.09-reflection-and-introspection/1.09.1-reflection-and-introspection.md)** - Runtime reflection
- **[1.10 - Pattern Matching](notes/research/1.10-pattern-matching/1.10.1-pattern-matching.md)** - Advanced pattern features
- **[1.11 - Domain-Specific Languages](notes/research/1.11-domain-specific-langage/1.11.1-domain-specific-langage.md)** - First-class DSL support
- **[1.12 - Build System](notes/research/1.12-build-system/1.12.1-build-system.md)** - Compilation pipeline
- **[1.13 - Foreign Function Interface](notes/research/1.13-foreign-function-interface/1.13.1-foreign-function-interface.md)** - Safe native code interop
- **[1.14 - Development Tools](notes/research/1.14-development-tools/1.14.1-development-tools.md)** - Debugging and tooling
- **[1.15 - Advanced Type System](notes/research/1.15-advanced-type-system/1.15.1-advanced-type-system.md)** - Category theory-based type system
- **[1.16 - Performance Optimization Directives](notes/research/1.16-performance-optimization-directives/1.16.1-performance-optimization-directives.md)** - BEAM-specific optimizations
- **[1.17 - Side Effects Design](notes/research/1.17-side-effects-design/1.17.1-side-effects-design.md)** - Algebraic effects and handlers
- **[1.18 - Library Updates](notes/research/1.18-library-updates/1.18.1-library-updates.md)** - Standard library evolution
- **[1.19 - Security](notes/research/1.19-security/1.19.1-preventing-rce.md)** - Security architecture and RCE prevention
- **[1.20 - Standard Library](notes/research/1.20-standard-library/1.20.1-standard-library.md)** - Category theory foundations (Setoid, Functor, Monad, etc.)
- **[1.21 - Distribution Layer](notes/research/1.21-distribution-layer/1.21.2-partisan.md)** - Partisan integration and scalable distribution

See the [research directory README](notes/research/README.md) for a detailed overview and recommended reading order.

## Example Code

```catena
-- Define a type (algebraic data type)
type User = {
  name: Text,
  age: Natural,
  email: Email
} derives [Eq, Show, Mappable]

-- Define a transform (pure function)
transform greet : User -> Text
transform greet user = "Hello, " <> user.name

-- Composition with the pipe operator
transform process_user : User -> Result Success Error
transform process_user =
  validate
  |> process
  |> persist

-- Actor as immutable state machine
actor Counter = {
  type State = { count: Natural }
  type Message = Increment | Decrement | Get

  transform handle : Message -> State -> (State, Maybe Reply)
  transform handle = match
    | Increment, s -> ({ count: s.count + 1 }, None)
    | Decrement, s -> ({ count: s.count - 1 }, None)
    | Get, s       -> (s, Some s.count)
}
```

## Goals

1. **Demonstrate that category theory can be practical** - Make abstract mathematics accessible to working programmers
2. **Unify functional and distributed programming** - Show that pure functions and actor models complement rather than conflict
3. **Leverage existing ecosystems** - Build on the BEAM's 30+ years of production success rather than starting from scratch
4. **Make correctness convenient** - Design where the easiest path is also the correct path

## Non-Goals

- Replace Erlang or Elixir for existing projects
- Support every category theory concept (focus on practical subset)
- Create yet another general-purpose language without a clear niche
- Ignore performance or developer experience for theoretical purity

## Inspiration

Catena draws inspiration from:

- **Category theory** - Mathematical foundations for composition and abstraction
- **ML family** (OCaml, Standard ML, Haskell) - Module systems and type systems
- **BEAM languages** (Erlang, Elixir) - Concurrency and fault tolerance
- **Idris/Agda** - Dependent types and visibility control
- **Rust** - Safety guarantees and modern ergonomics

## Development

### Building and Testing

The project uses a Makefile for common development tasks:

```bash
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
```

### Test Coverage

Test coverage reporting is integrated using Erlang's built-in `cover` tool. After running `make coverage`, HTML reports are generated in `_build/test/cover/` showing line-by-line coverage for each module.

**Current Coverage:** ~86% overall for compiler modules (248 tests, all passing)

See [Test Coverage Guide](notes/guides/test-coverage-guide.md) for detailed information on:
- Interpreting coverage results
- Improving test coverage
- Coverage targets and best practices

## Contributing

This project is currently in the research and early implementation phase. A **proof-of-concept compiler is under active development** following a structured 17-week implementation plan.

### Implementation Roadmap

The proof-of-concept is divided into 6 phases spanning approximately 4.25 months:

| Phase | Duration | Focus Area | Document |
|-------|----------|------------|----------|
| **Phase 1** | 6.5 weeks | Core Language Infrastructure | [phase-01.md](notes/planning/proof-of-concept/phase-01.md) |
| | | Lexer, Parser, Type-and-Effect System, Effect Runtime, Code Generation | |
| **Phase 2** | 3.5 weeks | REPL and Basic Runtime | [phase-02.md](notes/planning/proof-of-concept/phase-02.md) |
| | | Effect-Aware REPL, Prelude, Builtin Effects (IO, Process) | |
| **Phase 3** | 3.5 weeks | Pattern Matching Engine | [phase-03.md](notes/planning/proof-of-concept/phase-03.md) |
| | | Advanced Patterns, Pure Guards, Decision Trees, Exhaustiveness | |
| **Phase 4** | 2.5 weeks | Module System | [phase-04.md](notes/planning/proof-of-concept/phase-04.md) |
| | | Modules, Effect Propagation, Interface Files, Separate Compilation | |
| **Phase 5** | 3.5 weeks | Actor Model Integration | [phase-05.md](notes/planning/proof-of-concept/phase-05.md) |
| | | Actor-Effect Unification, Supervision, Fault Tolerance | |
| **Phase 6** | 5 weeks | Effect System Completion (Post-PoC) | [phase-06-effect-completion.md](notes/planning/proof-of-concept/phase-06-effect-completion.md) |
| | | Effect Polymorphism, Effect Library, Optimizations, Advanced Features | |

**Quick Links:**
- [📋 Proof-of-Concept Overview](notes/planning/proof-of-concept/proof-of-concept.md) - High-level goals and architecture
- [🧭 Phase Navigation](notes/planning/proof-of-concept/phase-navigation.md) - Detailed phase descriptions and dependencies
- [📁 Planning Directory](notes/planning/README.md) - Complete planning documentation

### How to Contribute

Contributions are welcome in several forms:

- **Research Feedback** - Review and provide feedback on design documents in `notes/research/`
- **Additional Research** - Explore new topics related to category theory, BEAM integration, or language design
- **Implementation** - Contribute to the proof-of-concept compiler (see phase documents for current progress)
- **Testing** - Help develop test cases and property-based testing strategies
- **Documentation** - Improve existing documentation or add examples

When contributing code:
1. Write tests for new functionality
2. Run `make coverage` to verify test coverage
3. Aim for ≥90% coverage on modified modules
4. Include coverage stats in pull request descriptions

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
