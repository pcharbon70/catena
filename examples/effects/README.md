# Algebraic Effects Examples

This directory contains the Phase 14 examples for Catena's algebraic-effects
track.

## Surface Note

Some examples use the full handler and resumption notation described in
`guides/ALGEBRAIC_EFFECTS_GUIDE.md`. That notation matches the internal
runtime and type-system model, but parts of it still lead the parser surface
covered by `catena_parser_effect_tests.erl`.

When you need syntax proven in the frontend today, prefer:

- `perform Effect.operation(...)`
- `handle Expr then { Effect { op(args) -> body } }`

## Files

- `state_counter.catena` uses the full Phase 14 model to show resumptions and
  state threading.
- `error_recovery.catena` uses the full Phase 14 model to show handler-based
  recovery.
- `logging_pipeline.catena` stays close to the current parser-compatible core
  syntax.
- `async_worker.catena` stays close to the current parser-compatible core
  syntax while using builtin process effects.
- `resource_scope.catena` uses the full Phase 14 model to show a higher-order
  resource effect.
