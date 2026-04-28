# Catena Boundaries

## Canonical Boundaries

### Language Design Boundary

Owns Catena syntax, semantics, vocabulary, and long-range design research.

Primary sources:

- `specs/research/`
- `specs/planning/`
- promoted decisions in `specs/adr/`

### Compiler Boundary

Owns the translation from Catena source to typed IR and BEAM-targetable output.

This includes:

- lexing and parsing
- AST shaping and desugaring
- kind, type, trait, and effect analysis
- pattern compilation and code generation
- limited import processing that is already part of the implemented pipeline

### Runtime Boundary

Owns execution-time behavior that is not just static compilation:

- effect runtime behavior
- handler execution semantics
- REPL interaction model
- runtime bindings that support interactive workflows

### Standard Library Boundary

Owns library-defined language surfaces:

- prelude traits, types, and instances
- testing and law modules
- Catena-defined effect modules
- the in-repo property-testing framework workstream

### Tooling And Quality Boundary

Owns the mechanics used to build, test, and validate the repository:

- build scripts and `rebar3` integration
- test layout and quality workflow
- security/resource-limit hardening in build/test surfaces
- status reconciliation between code, tests, and specs

## Promotion Rule

`specs/` should define the stable line between:

- what Catena is aiming for
- what the repository already implements
- what is still explicitly deferred
