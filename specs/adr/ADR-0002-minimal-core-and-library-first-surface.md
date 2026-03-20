# ADR-0002: Minimal Core And Library-First Surface

## Status

Accepted

## Context

The notes and implementation work show a repeated design preference: Catena should keep the compiler core relatively small while expressing rich algebraic abstractions in the standard library. The `notes/features/minimal-core-keywords.md` workstream and the current `Prelude`, `Test`, and `Laws` modules all push in that direction.

## Decision

Catena will prefer a minimal compiler-supported surface and a library-first semantic layer:

- core syntactic forms such as `type`, `transform`, `trait`, `instance`, `effect`, `perform`, `handle`, and `module` remain compiler-facing
- category-theory abstractions, law surfaces, and most testing semantics live in library code
- syntax sugar such as operators and do-notation may be compiler-supported, but should desugar toward library-defined meanings where possible

## Consequences

- the compiler stays smaller and easier to reason about
- the standard library becomes a first-class part of the language story
- some roadmap items are partly implemented in both compiler and library layers and must be documented together
- future additions should justify why they belong in the compiler rather than the library
