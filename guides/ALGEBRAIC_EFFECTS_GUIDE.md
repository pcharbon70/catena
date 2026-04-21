# Algebraic Effects Guide

## Status

Catena's algebraic-effects track now has an end-to-end implementation across
the compiler, runtime, validation, and integration-test layers.

Two surfaces coexist today:

- The internal/compiler surface includes resumptions, row-polymorphic effect
  rows, equations, deep and shallow handlers, one-shot and multi-shot
  continuations, and higher-order effect machinery.
- The active parser surface is narrower. If you need syntax covered by the
  frontend tests today, follow `perform Effect.operation(...)` and the
  `handle Expr then { Effect { op(args) -> body } }` form exercised in
  `catena_parser_effect_tests.erl`.

Treat this guide as the Phase 14 architecture reference for the newer
algebraic-effects track.

## Architecture

The main implementation layers are:

- Orchestration: `src/compiler/effects/catena_effect_system.erl`
- Runtime: `src/compiler/runtime/catena_effect_runtime.erl`
- Code generation: `src/compiler/codegen/catena_effect_codegen.erl`
- Type integration: `src/compiler/types/catena_effect_synthesis.erl`,
  `src/compiler/types/catena_effect_constraints.erl`, and
  `src/compiler/types/catena_row_poly_integration.erl`
- Equations and laws: `src/compiler/effects/catena_algebraic_laws.erl`,
  `src/compiler/effects/catena_equation_spec.erl`, and
  `src/compiler/effects/catena_equation_apply.erl`
- Higher-order effects: `src/compiler/effects/catena_op_signatures.erl`,
  `src/compiler/effects/catena_ho_effects.erl`,
  `src/compiler/effects/catena_hefty.erl`, and
  `src/compiler/effects/catena_ho_execution.erl`
- Validation: `src/compiler/validation/catena_effect_validation.erl`

## Handler And Resumption Model

The runtime model is explicit-context based.

1. `perform` resolves an operation against the current effect context.
2. `with_runtime` and `with_handlers` create child scopes with handler
   bindings.
3. A handler can return immediately, resume the captured continuation, or
   short-circuit the continuation.
4. Deep and shallow handlers control whether nested operations are intercepted
   recursively or only at the direct scope.
5. Continuation modules distinguish one-shot from multi-shot resumptions.

The important practical consequence is that effectful execution should run
under `catena_effect_system:with_runtime/1,2`, not through hidden
process-local state.

## Equations And Laws

Phase 14 connects algebraic laws to optimizer behavior instead of treating
laws as comments or separate theory.

The normal workflow is:

1. define or load equations from `catena_algebraic_laws`
2. validate them with `catena_equation_spec`
3. apply rewrites with `catena_equation_apply:rewrite/2,3`

This allows effect-aware normalization and optimization passes to share the
same equation language used for theoretical validation.

## Row Polymorphism

The older proof-of-concept tracked only closed effect sets. The algebraic
effects track extends that with rows.

Rows let the compiler express:

- concrete handled effects
- residual "rest of the effects" information
- handler removal that preserves unhandled effects
- constraints over required effects without forcing a fully closed set

The main row-polymorphism modules are:

- `catena_row_types`
- `catena_row_operations`
- `catena_row_unify`
- `catena_row_inference`
- `catena_row_poly_integration`

## Higher-Order Effects

Higher-order operations accept effectful callbacks or computations.

Catena models them with:

- operation signatures carrying row variables
- effectful parameter descriptors
- hefty trees and handlers for interpretation
- execution helpers that preserve callback effects

Use this layer for scoped resources, iteration with effectful callbacks,
effectful recovery functions, and other operations whose arguments are not
plain pure values.

## Validation

Use the Phase 14 validation entry point after non-trivial effect-system
changes:

```erlang
catena_effect_validation:validate().
```

This validates:

- theoretical invariants
- property-level checks
- conformance across runtime, type, and codegen boundaries

## Examples

See `examples/effects/` for:

- state threading
- error recovery
- logging
- async and process orchestration
- resource scoping

Each example is marked as either current parser-compatible core syntax or
full Phase 14 model syntax.

## Practical Guidance

- Use builtin `IO` and `Process` handlers when you need runtime-backed effects
  now.
- Wrap generated or manual runtime execution with
  `catena_effect_system:with_runtime/1,2`.
- Use `catena_effect_validation:validate/0` before merging phase-level
  runtime or type changes.
- Treat `specs/compiler/type_and_effect_system.md` and
  `specs/runtime/effect_runtime.md` as the promoted proof-of-concept baseline.
  This guide documents the newer algebraic-effects track that now extends
  beyond that baseline.
