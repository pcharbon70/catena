# Flow Implementation Plan

**Description:** This plan implements the Flow abstraction (pragmatic name for Arrow from category theory) in Catena. Flow provides a generalization of functions that enables structured computation with parallel composition, fanout, and choice operations.

This plan predates the repo's newer algebraic-effects track. It should now be read against Catena's current effect baseline: explicit effect tracking, implemented row-polymorphism surfaces, typed handlers, operation signatures, and higher-order effect machinery. Flow remains a library-level structured-computation abstraction, but its effectful interpretation and optimization story must now respect the algebraic-effects model rather than assuming only the older Kleisli/effect-set framing.

**Total Duration:** 4 weeks (8 phases)

**Status:** In progress. The pure Flow core foundation is now materially implemented, while the remainder of the 8-phase track remains planned.

**References:**
- [ADR-0004: Flow as Pragmatic Name for Arrow](../../adr/ADR-0004-flow-as-arrow.md)
- [Standard Library Surface](../../stdlib/standard_library_surface.md)

---

## Phase Overview

| Phase | Title | Duration | Status |
|-------|-------|----------|--------|
| 1 | [Flow Core Trait and System Foundation](./phase-01-flow-core-trait-and-system-foundation.md) | 3 days | Partially implemented |
| 2 | [Flow Instances for Core Types](./phase-02-flow-instances-for-core-types.md) | 3 days | Planned |
| 3 | [FlowChoice and FlowApply](./phase-03-flow-choice-and-flow-apply.md) | 4 days | Planned |
| 4 | [Flow Utilities and Combinators](./phase-04-flow-utilities-and-combinators.md) | 3 days | Planned |
| 5 | [Flow Processors and Stream Processing](./phase-05-flow-processors-and-stream-processing.md) | 4 days | Planned |
| 6 | [Circuit and Signal Flow Processing](./phase-06-circuit-and-signal-flow-processing.md) | 4 days | Planned |
| 7 | [Flow Optimization and Laws](./phase-07-flow-optimization-and-laws.md) | 4 days | Planned |
| 8 | [Documentation and Examples](./phase-08-documentation-and-examples.md) | 3 days | Planned |

---

## Quick Reference

Current implemented subset:

- `System` and `Flow` traits exist in `Prelude`
- the current pure-core operations `id`, `compose`, `lift`, `first`, `parallel`, and `split` exist
- the pure Flow operators `>>>`, `<<<`, `***`, and `&&&` parse and desugar
- pure `System` and `Flow` laws now exist in `Laws`
- the rest of this plan remains a forward plan rather than completed repo surface

### Flow Traits

| Pragmatic Name | Category Theory | Key Operations |
|----------------|-----------------|----------------|
| System | Category | `id`, `compose` |
| Flow | Arrow | `lift`, `first` |
| FlowChoice | ArrowChoice | `left`, `right` |
| FlowApply | ArrowApply | `app` |

### Flow Operators

| Operator | Description | Type Signature |
|----------|-------------|----------------|
| `>>>` | Left-to-right composition | `arr b c -> arr a b -> arr a c` |
| `<<<` | Right-to-left composition | `arr b c -> arr a b -> arr a c` |
| `***` | Parallel composition | `arr a b -> arr c d -> arr (a, c) (b, d)` |
| `&&&` | Fanout/split | `arr a b -> arr a c -> arr (a, (b, c))` |
| `+++` | Choice | `arr a b -> arr c d -> arr (Either a c) (Either b d)` |
| `\|\|\|` | Merge | `arr a c -> arr b c -> arr (Either a b) c` |

---

## Deliverables Summary

### New Modules
- `lib/catena/stdlib/prelude.cat` — Updated with System, Flow, FlowChoice, FlowApply traits
- `lib/catena/stdlib/laws.cat` — Updated with System and Flow laws
- `lib/catena/stdlib/stream.cat` — Stream processing with Flow
- `lib/catena/stdlib/circuit.cat` — Circuit simulation with Flow
- `lib/catena/stdlib/flow.cat` — Flow utilities and combinators
- `lib/catena/stdlib/parser.cat` — Parser combinators using Flow

### Test Modules
- `test/compiler/stdlib/catena_system_tests.erl`
- `test/compiler/stdlib/catena_flow_tests.erl`
- `test/compiler/stdlib/catena_flow_choice_tests.erl`
- `test/compiler/stdlib/catena_flow_apply_tests.erl`
- `test/compiler/stdlib/catena_stream_tests.erl`
- `test/compiler/stdlib/catena_circuit_tests.erl`
- `test/compiler/stdlib/catena_flow_integration_tests.erl`
- `test/compiler/stdlib/catena_flow_law_tests.erl`
- `test/compiler/stdlib/catena_flow_optimization_tests.erl`

### Documentation
- `docs/flow-reference.md` — Complete Flow reference
- `docs/flow-tutorial.md` — Flow tutorial
- `docs/flow-patterns.md` — Flow patterns guide
- `docs/flow-vs-pipeline-and-effects.md` — Flow vs Pipeline vs algebraic effects comparison
- `examples/flow/` — Flow example programs

---

## Success Criteria

1. **System Trait Implemented** — Category trait with proper laws
2. **Flow Trait Implemented** — Arrow trait with all core operations
3. **FlowChoice Implemented** — Choice/branching for sum types
4. **FlowApply Implemented** — Dynamic Flow application
5. **Core Instances Working** — Function, Maybe, Either, Result, List
6. **Stream Processing** — Flow-based stream processing library
7. **Circuit Simulation** — Flow-based circuit examples
8. **Laws Verified** — All Flow laws verified for all instances
9. **Optimizations Working** — Flow fusion, specialization, inlining, and effect-sensitive safety boundaries
10. **Documentation Complete** — Reference, tutorial, patterns, and explicit guidance on Flow vs Pipeline vs algebraic effects

---

## Prerequisites

Before implementing Flow, the following must be complete:

1. Higher-kinded type support in type checker (Phase 1.5.3)
2. Trait system with inheritance (Phase 1.5.2)
3. Operator desugaring (Phase 1.5.7)
4. The current effect-system baseline, including the implemented algebraic-effects row/handler surfaces, not just the older Kleisli integration wording
5. Law verification infrastructure (Phase 1.5.4)

---

## Flow And Algebraic Effects

The Flow track should be influenced by the current algebraic-effects implementation in the following ways:

- the pure Flow core (`System`, `Flow`, `FlowChoice`, `FlowApply`) remains a library-design concern and does not require compiler intrinsics just because algebraic effects now exist
- any effectful Flow interpretation must line up with the current type/effect system, including explicit effect information, handler removal, and implemented row-polymorphic/effect-constraint surfaces
- Flow-related optimizations must preserve effectful semantics and must not reorder computations across handler boundaries or effect-sensitive choice points without a justified law
- documentation must distinguish clearly between:
  - `Flow` for static/parallel structure
  - `Pipeline` for sequential/dependent composition
  - algebraic effects for effect declaration, handling, and resumption semantics
- examples involving effects should be written against the current algebraic-effects model rather than assuming only the older `EffectfulTransform`/Kleisli framing

---

## Dependencies and Interactions

Flow interacts with the following existing components:

| Component | Interaction | Notes |
|-----------|------------|-------|
| **Pipeline (Monad)** | Complementary | Flow for parallel/static, Pipeline for sequential/dependent |
| **EffectfulTransform (Kleisli)** | Related but not sufficient | Historical/older effectful framing; Flow planning now also needs to account for the newer algebraic-effects surfaces |
| **Mapper (Functor)** | Foundation | Flow extends System, which uses Mapper concepts |
| **Applicator** | Related | Applicative and Arrow have overlapping capabilities |
| **Effect System** | Integration | Flow can be used within effectful computations, but Flow/effect interop should be specified against the current algebraic-effects model |
| **Type System** | Foundation | Requires higher-kinded type support |
