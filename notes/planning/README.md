# Catena Planning Documents

This directory contains planning and implementation documents for the Catena programming language project. These documents outline concrete implementation strategies, timelines, and technical approaches for bringing Catena from design to reality.

## Purpose

While the research documents in `notes/research/` explore theoretical foundations and design decisions, the planning documents here focus on:

- **Implementation roadmaps** - Step-by-step plans for building language features
- **Proof-of-concept designs** - Minimal viable implementations to validate concepts
- **Technical specifications** - Detailed implementation strategies for specific components
- **Project timelines** - Estimated phases and milestones for development

## Planning Documents

### Proof of Concept
**File**: `proof-of-concept/proof-of-concept.md`

A comprehensive plan for building a minimal proof-of-concept implementation of Catena. Includes:

**Phase 1: Core Language Infrastructure (Weeks 1-3)**
- Lexer and parser implementation using leex/yecc
- Hindley-Milner type inference with Algorithm W
- Core Erlang code generation for BEAM compilation
- Basic shape (ADT) and flow (function) support

**Phase 2: REPL and Basic Runtime (Weeks 4-5)**
- Interactive REPL with type inspection
- Expression evaluation and definition persistence
- Standard prelude with essential types and functions
- Multi-line input and error recovery

**Phase 3: Essential Language Features (Weeks 6-8)**
- Enhanced pattern matching (guards, nested patterns, as-patterns)
- Let bindings and where clauses
- Basic module system with imports/exports
- Immutability primitives and update syntax

**Phase 4: Category Theory Integration (Weeks 9-11)**
- Functor, Applicative, Monad typeclasses
- Natural transformations as first-class values
- Effect system foundations
- Compositional programming primitives

**Phase 5: BEAM Integration (Weeks 12-14)**
- Actor model support with process spawning
- Message passing primitives
- Basic supervision primitives
- Interoperability with Erlang/Elixir

**Implementation Technology**:
- Written in Elixir/Erlang to leverage existing BEAM infrastructure
- Uses leex/yecc or nimble_parsec for parsing
- Compiles to Core Erlang for BEAM execution
- Integrates with OTP for runtime features

## Status

The planning documents represent the current implementation strategy for building Catena. As the project evolves, these documents will be updated to reflect lessons learned and adjusted priorities.

## Contributing

When adding new planning documents:
- Focus on concrete, actionable implementation details
- Include code examples and technical specifications
- Provide clear success criteria and deliverables
- Consider dependencies on other components
- Estimate effort and identify potential risks

## Relationship to Research Documents

The planning documents build directly on concepts explored in the research documents:

- Research defines **what** Catena should be
- Planning defines **how** to build it
- Research explores possibilities and tradeoffs
- Planning commits to specific implementation approaches

Together, these documents provide a complete picture from theoretical foundations to practical implementation.
