# Catena Proof-of-Concept - Phase Navigation

## All Phases

### [Phase 1: Core Language Infrastructure](phase-01.md) (Weeks 1-6.5)
**Foundation of the compiler and minimal viable effect system**

Establishes the complete compilation pipeline from source code to BEAM bytecode. Implements lexer/parser for Catena syntax including effect declarations, Hindley-Milner type-and-effect inference with categorical extensions, Core Erlang code generation with process-based effect runtime, and minimal viable algebraic effects. This phase provides the essential infrastructure that all subsequent phases build upon, including the effect system foundation.

**Key Deliverables:**
- Lexer and parser producing ASTs including effect syntax
- Type-and-effect inference engine with Algorithm W
- Minimal viable effect system (monomorphic, IO/Process effects only)
- Process-based effect runtime leveraging BEAM
- Core Erlang code generator with effect compilation
- Basic pattern compilation
- Error reporting infrastructure for types and effects

---

### [Phase 2: REPL and Basic Runtime](phase-02.md) (Weeks 7-10.5)
**Interactive development and standard library with effect support**

Creates an effect-aware interactive REPL for rapid prototyping and establishes the standard prelude with foundational types, functions, and builtin effects. Developers can evaluate effectful expressions, inspect type-and-effect signatures, and load modules interactively. The prelude provides essential category-theoretic abstractions (Functor, Monad), core data structures (List, Maybe, Result), and builtin IO/Process effect definitions.

**Key Deliverables:**
- Effect-aware REPL with effectful expression evaluation
- Command system (:type showing effect sets, :load, :browse)
- Pretty printing for values, types, and effect sets
- Effect execution in REPL with automatic handler provision
- Standard prelude with core types and builtin effects
- Functor and Monad traits with instances
- Essential list operations
- Builtin IO and Process effect handlers

---

### [Phase 3: Pattern Matching Engine](phase-03.md) (Weeks 11-14.5)
**Advanced pattern matching with effect-aware guards**

Implements sophisticated pattern matching with guards (pure only in PoC), or-patterns, nested patterns, and as-patterns. The pattern compiler generates efficient decision trees, performs exhaustiveness checking, and detects redundant patterns. Guards are checked for purity (empty effect set) with effectful guards deferred to Phase 6.

**Key Deliverables:**
- Guards (pure only), or-patterns, nested patterns, as-patterns
- Guard purity checking (effect-free guards)
- Decision tree compilation
- Exhaustiveness checking with examples
- Redundancy detection
- Optimized code generation
- SMT integration for guard analysis

---

### [Phase 4: Module System](phase-04.md) (Weeks 15-17)
**Code organization with effect propagation**

Establishes hierarchical module organization with visibility control, qualified and selective imports, and separate compilation. Module interfaces include type-and-effect signatures enabling cross-module effect checking. Effects propagate correctly across module boundaries. The system supports incremental builds, detects circular dependencies, and enables interoperability with Erlang code.

**Key Deliverables:**
- Module declaration and export system with effect signatures
- Qualified and selective imports
- Name resolution with shadowing
- Circular dependency detection
- Interface files with type-and-effect signatures
- Cross-module effect propagation
- Incremental and parallel compilation
- Standard library organization with effects

---

### [Phase 5: Actor Model Integration](phase-05.md) (Weeks 18-21.5)
**Concurrency through actor-effect unification**

Unifies actors with the Process effect system, demonstrating actors as effect handlers for stateful concurrent computation. Actor handlers use Process effect operations (spawn, send, receive) within handler logic. Supervision trees provide fault tolerance through declarative restart strategies. This completes the proof-of-concept by demonstrating category theory, algebraic effects, and BEAM concurrency working together.

**Key Deliverables:**
- Actor definition syntax as Process effect handlers
- State types and message protocols
- Effectful message handlers with Process effect tracking
- Process primitives as effect operations
- Actor-effect unification demonstration
- Supervision trees and restart strategies
- Communication patterns (sync/async) with effect tracking
- Fault tolerance through effect boundaries

---

### [Phase 6: Effect System Completion](phase-06-effect-completion.md) (Weeks 22-26)
**Advanced effect features (Post-PoC)**

Completes the effect system with effect polymorphism, expanded effect library, optimizations, and advanced features deferred from the PoC. Implements effect variables and constraints enabling generic effectful code. Adds State, Reader, Writer, Async, and Error effects to standard library. Optimizes effect runtime to <5% overhead through fusion, inlining, and static resolution. Enables delimited continuations, scoped effects, and effectful guards.

**Key Deliverables:**
- Effect polymorphism with effect variables and constraints
- Complete standard effect library (State, Reader, Writer, Async, Error)
- Effect optimizations (fusion, inlining, static resolution)
- Delimited continuations for control flow effects
- Scoped effects for resource safety
- Effectful pattern guards
- Effect debugging and profiling tools
- Production-ready effect system

---

## Quick Links

**By Development Stage:**
- [Foundation + Effects](phase-01.md) → [Development Tools + Effect REPL](phase-02.md) → [Language Features](phase-03.md) → [Organization + Effect Propagation](phase-04.md) → [Actor-Effect Unification](phase-05.md) → [Effect Completion](phase-06-effect-completion.md)

**By Topic:**
- **Compilation + Effects**: [Phase 1](phase-01.md) - Lexer, Parser, Type-and-Effect System, Effect Runtime, Code Generation
- **Developer Experience**: [Phase 2](phase-02.md) - Effect-aware REPL, Prelude, Builtin Effects
- **Language Features**: [Phase 3](phase-03.md) - Pattern Matching, Pure Guards
- **Architecture**: [Phase 4](phase-04.md) - Modules, Imports, Effect Propagation, Interface Files
- **Concurrency**: [Phase 5](phase-05.md) - Actors as Effect Handlers, Supervision, Process Effect
- **Advanced Effects**: [Phase 6](phase-06-effect-completion.md) - Effect Polymorphism, Effect Library, Optimizations

**Back to:**
- [Proof-of-Concept Overview](proof-of-concept.md)
- [Planning Directory](../README.md)

---

## Phase Dependencies

```
Phase 1 (Foundation + Minimal Viable Effects)
   ↓
Phase 2 (REPL + Prelude + Builtin Effects) ─┐
   ↓                                         │
Phase 3 (Patterns + Pure Guards)             │← Uses effect-aware REPL
   ↓                                         │
Phase 4 (Modules + Effect Propagation)       │← Needs prelude with effects
   ↓                                         │
Phase 5 (Actors + Effect Unification)        │← Builds on all, unifies with effects
   ↓                                         │
Phase 6 (Effect System Completion)           │← Post-PoC, builds on effect foundation
   └──────────────────────────────────────────┘
```

Each phase builds incrementally on previous work. Phase 1 establishes minimal viable effects that grow through Phases 2-5, culminating in full effect system in Phase 6.

---

## Progress Tracking

Use checkboxes in each phase document to track progress:
- `- [ ]` = Not started
- `- [x]` = Completed

Each phase includes:
- Section-level completion tracking
- Task-level completion tracking
- Subtask-level completion tracking
- Unit tests per section
- Integration tests per phase

---

## Implementation Timeline

### Proof-of-Concept (12 weeks)

| Phase | Duration | Week Range | Status |
|-------|----------|------------|--------|
| Phase 1 | 6.5 weeks | Weeks 1-6.5 | ❌ Not Started |
| Phase 2 | 3.5 weeks | Weeks 7-10.5 | ❌ Not Started |
| Phase 3 | 3.5 weeks | Weeks 11-14.5 | ❌ Not Started |
| Phase 4 | 2.5 weeks | Weeks 15-17 | ❌ Not Started |
| Phase 5 | 3.5 weeks | Weeks 18-21.5 | ❌ Not Started |
| **PoC Total** | **~12 weeks** | **3 months** | **0% Complete** |

### Post-PoC Completion (5 weeks)

| Phase | Duration | Week Range | Status |
|-------|----------|------------|--------|
| Phase 6 | 5 weeks | Weeks 22-26 | ❌ Not Started |
| **Total All Phases** | **~17 weeks** | **4.25 months** | **0% Complete** |

**Note**: Timeline is approximate. Phase 6 is post-PoC and implements advanced effect features deferred from the initial demonstration. Phases 1-5 constitute the complete proof-of-concept demonstrating minimal viable algebraic effects on BEAM.

---

## Getting Started

1. Begin with [Phase 1: Core Language Infrastructure](phase-01.md)
2. Follow the phases in sequence for dependency management
3. Mark checkboxes as you complete tasks
4. Run tests after each section
5. Integration tests validate phase completion

## Support Documents

- [Proof-of-Concept Overview](proof-of-concept.md) - High-level goals and architecture
- [Planning README](../README.md) - Planning directory overview
- [Research Documents](../../research/README.md) - Theoretical foundations
