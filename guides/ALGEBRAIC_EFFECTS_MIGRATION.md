# Algebraic Effects Migration Guide

## Scope

This guide is for moving compiler integrations and examples from the older
tracked-effects model to the Phase 14 algebraic-effects system.

The older model assumed:

- concrete effect sets only
- equality or subset checks over closed sets
- simpler handler removal semantics
- no first-class equation-driven optimization
- no higher-order effect signatures

The Phase 14 system adds:

- explicit runtime orchestration through `catena_effect_system`
- row-polymorphic effect rows
- handler and resumption semantics
- equation- and law-driven rewrites
- higher-order effect signatures and hefty execution

## Breaking Changes

- Runtime setup is explicit. Effectful execution should happen under
  `catena_effect_system:with_runtime/1,2` or
  `catena_effect_runtime:with_handlers/3`.
- Hidden handler lookup is not the model anymore. The runtime passes effect
  context explicitly.
- Effect typing no longer assumes every useful program can be described by a
  closed `{Effect1, Effect2}` set.
- Handling an effect removes the handled effect and preserves the residual
  row.
- Effectful callbacks need higher-order signatures, not plain first-order
  function types.
- Optimization may rewrite effect terms using validated equation sets.

## Migration Patterns

### 1. Runtime Entry Points

Before, effect execution often assumed ambient handler state.

After, start an explicit runtime scope:

```erlang
Result = catena_effect_system:with_runtime(fun(Ctx) ->
    catena_effect_runtime:with_handlers(Ctx, Handlers, BodyFun)
end).
```

### 2. Closed Effect Sets To Rows

Before:

```catena
transform load : Path -> Text / {IO, Error}
```

After, conceptually:

```catena
transform load : Path -> Text / {IO, Error | rho}
```

Use `catena_row_types`, `catena_row_operations`, and
`catena_row_poly_integration` for compiler-side handling. When source syntax
needs to stay within today's frontend, keep the closed-set spelling in source
and let the row-aware compiler modules preserve residual effects underneath.

### 3. Handler Typing

Before, the checker removed a handled effect from a closed set and stopped.

After, propagate the residual row and solve row constraints with
`catena_effect_constraints`.

### 4. Higher-Order Operations

Before, effectful callbacks had to be approximated as pure or handled
out-of-band.

After, represent them with `catena_ho_effects:effectful_param/3` and
`catena_op_signatures:op_sig/...` so callback effects remain visible.

### 5. Optimization And Laws

Before, optimizations were mostly syntactic.

After, register validated equations and call:

```erlang
catena_equation_apply:rewrite(Expr, EqSet, normal).
```

### 6. Validation

Before, verification relied mostly on isolated unit tests.

After, add a phase-level validation pass:

```erlang
catena_effect_validation:validate().
```

## Source Migration Examples

### Simple Tracked Effect

Before:

```catena
transform greet : String -> Unit / {IO}
transform greet name =
  perform IO.println ("Hello, " ++ name)
```

After, keep the same `perform` surface but:

- run the program under an explicit runtime
- allow the compiler to preserve extra effects through rows instead of forcing
  a fully closed effect set everywhere

### Handler-Based Recovery

Before, `Error` was treated as a closed effect that the checker removed
manually.

After, model recovery as a handler that consumes `Error` and leaves the rest
of the effect row intact.

### Effectful Callbacks

Before, operations such as `catch`, iteration, or resource scoping had to
approximate effectful callbacks as plain functions.

After, give those callbacks higher-order signatures so their effects stay in
scope.

## Checklist

- Replace ambient runtime assumptions with
  `catena_effect_system:with_runtime/1,2`.
- Move effect-set reasoning to the `catena_row_*` helpers when you touch
  compiler internals.
- Use `catena_effect_constraints` for residual-effect solving.
- Represent effectful callbacks with `catena_ho_effects` and
  `catena_op_signatures`.
- Register algebraic laws through `catena_equation_spec` before applying
  rewrites.
- Run `catena_effect_validation:validate/0` before merging phase-level
  changes.

## Examples

See `examples/effects/` for migration-oriented sample programs:

- `state_counter.catena`
- `error_recovery.catena`
- `logging_pipeline.catena`
- `async_worker.catena`
- `resource_scope.catena`
