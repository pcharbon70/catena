# ADR-0003: Explicit Effect Context Runtime

## Status

Accepted

## Context

The effect runtime originally relied on process-dictionary style handler state. The notes under `notes/features/explicit-effect-context.md` and its summary describe why that model was a poor fit for nesting, spawned processes, and testability.

## Decision

Catena's effect runtime will use explicit effect-context passing as the canonical runtime model:

- effectful operations receive context explicitly
- nested handlers create child contexts with parent lookup
- builtin effects remain available through explicit runtime boundaries rather than hidden global registration
- code generation should thread effect context through generated runtime calls

## Consequences

- effect dependencies are more visible and composable
- spawned processes can receive effect context explicitly
- tests can mock handler context without process-global setup
- runtime entry points must create and thread an initial context deliberately
