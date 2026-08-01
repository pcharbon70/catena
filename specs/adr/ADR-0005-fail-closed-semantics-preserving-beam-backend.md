# ADR-0005: Fail-Closed, Semantics-Preserving BEAM Backend

## Status

Accepted

## Context

Catena has a real source-to-Core Erlang path and executable integration tests,
but the frontend currently accepts a larger language surface than the backend
can faithfully preserve.

The current backend can generate and execute representative transforms,
arithmetic, algebraic data constructors, and constructor pattern matches.
However, several gaps prevent a successful code-generation result from being a
general semantic-preservation guarantee:

- named calls between top-level transforms are emitted as unbound Core Erlang
  variables rather than resolved local function references
- imports contribute to type environments without a complete executable
  linkage model
- unknown expressions can become runtime `{error, unknown_expression, ...}`
  values
- unknown or misplaced patterns can become wildcards
- complex binding patterns can be approximated as `_`
- runtime-bearing declarations can be filtered from module emission without an
  explicit supported, erased, or rejected disposition
- trait and instance dictionary lowering is not integrated end to end
- test and property declarations parse but are not emitted as executable test
  artifacts
- the public compiler API returns Core Erlang but does not expose a complete
  in-memory BEAM artifact API

These behaviors blur four materially different outcomes:

1. a construct is supported and its behavior is preserved
2. a construct is static metadata and is intentionally erased
3. a construct requires an explicit runtime lowering
4. a construct is not supported by the backend

BEAM itself is not the limiting factor. Catena values and behavior can be
represented with tagged tuples, maps, closures, Core Erlang cases, remote
calls, processes, messages, and explicit runtime libraries. The missing
boundary is a compiler policy that makes unsupported or unresolved semantics a
compile-time error.

## Decision

Catena's production BEAM backend will be fail-closed and
semantics-preserving.

### Validated Compilation Unit

The public backend path will consume a validated compilation unit produced
after lexical, syntactic, semantic, import, kind, type, trait, and effect
validation. That unit will retain:

- the normalized source AST needed for lowering
- typed declaration results and the effective type environment
- local and imported symbol identities, kinds, and arities
- source locations for backend diagnostics
- the runtime representation or erasure disposition of each declaration

Low-level code-generation helpers may continue to accept backend AST terms for
focused unit tests, but they are not a safe public compilation boundary and
must not be presented as one.

### Explicit Call Resolution

Before expression emission, every call target will be classified as exactly
one of:

- a local transform reference, emitted with its resolved name and arity
- an imported transform reference, emitted as a resolved module call
- a higher-order value, emitted as a closure application
- a constructor, emitted using the constructor representation
- an unresolved or ambiguous target, rejected with a structured diagnostic

This resolution must support forward references, mutual recursion, and
self-recursion without depending on declaration order.

### Exhaustive Backend Disposition

Every normalized declaration, expression, pattern, and operator that reaches
the backend will have an explicit disposition:

- `lowered`
- `erased_static`
- `runtime_lowered`
- `unsupported`

`unsupported` is a compile-time error. It must never be implemented by:

- embedding a placeholder error value in generated code
- replacing a pattern or binding with a wildcard
- silently omitting a runtime-bearing declaration
- treating an unknown operator as an arbitrary Erlang BIF

Static constructs such as type declarations may be erased only after every
runtime representation decision that depends on them has been completed.

### Runtime Representation Contract

The backend will document and test stable representation choices:

- algebraic data constructors use tagged tuples
- lists and tuples use their native BEAM representations
- structural records use maps until another representation is accepted by a
  later ADR
- closures use BEAM functions
- pattern matching uses Core Erlang clauses and cases
- effects use explicit Catena runtime calls and context passing
- trait dispatch uses resolved dictionaries or another explicitly accepted
  representation

Test and property declarations are not ordinary application functions. They
must either lower through an explicit testing artifact/runner contract or be
rejected by application-module compilation until that contract exists.

### Artifact Validation And Public API

The compiler will expose an in-memory source-to-BEAM API in addition to its
typed-module and Core Erlang APIs. A successful BEAM result requires:

1. the complete validated frontend path
2. exhaustive backend lowering
3. Core Erlang validation
4. OTP compilation with `from_core`
5. structured propagation of Core or BEAM diagnostics

Writing artifacts to disk, packaging applications, and release assembly remain
separate tooling concerns.

### Executable Conformance

Every construct promoted as BEAM-supported will have source-level executable
evidence that performs:

```text
Catena source
  -> validated compilation unit
  -> Core Erlang
  -> BEAM binary
  -> loaded module
  -> observable result
```

Negative evidence will verify that unsupported, unresolved, or invalid
constructs fail before artifact emission. Code-generation unit tests remain
useful, but they do not by themselves promote a source-language feature to
BEAM-supported status.

## Consequences

### Positive Consequences

- A successful BEAM compilation becomes a meaningful semantic guarantee.
- Unsupported frontend/backend combinations fail close to their source.
- Recursion, local calls, imports, and higher-order calls gain one explicit
  resolution model.
- Static erasure is distinguished from missing runtime implementation.
- Backend completeness becomes reviewable feature by feature.
- New language features must define their runtime representation and
  executable evidence before being advertised as BEAM-supported.

### Negative Consequences

- Some programs that currently return placeholder values or compile after
  silent omission will become compile-time errors.
- The compiler needs a validated-unit or equivalent backend-input structure
  rather than passing an analyzed AST alone.
- Existing backend unit tests that bypass validation will need to be clearly
  scoped or migrated.
- The dedicated source-to-BEAM conformance suite adds an ongoing maintenance
  obligation whenever the promoted feature ledger changes.

### Neutral Consequences

- Catena remains uniformly represented on BEAM; this decision does not require
  monomorphization.
- Type declarations and most type annotations remain erased by design.
- This decision does not select a backend optimization strategy.
- This decision does not make the full module, actor, testing, or property
  systems immediately complete.

## Migration Rules

1. Replace unknown-expression and unknown-pattern fallbacks with structured
   backend errors.
2. Introduce symbol and arity resolution before Core Erlang emission.
3. Make local and recursive transform calls produce valid Core Erlang function
   references.
4. Classify declarations as lowered, static-erased, runtime-lowered, or
   rejected.
5. Add the in-memory source-to-BEAM API and preserve the existing typed-module
   and Core Erlang APIs.
6. Grow executable conformance from the current arithmetic and constructor
   baseline until every promoted backend feature has positive and negative
   evidence.

## Implementation Status

The seven-phase backend-hardening roadmap completed on 2026-07-24. The
validated compilation unit, explicit symbol/disposition boundaries,
exhaustive fail-closed lowering, runtime-backed effects, executable
imports/traits, versioned public BEAM artifacts, source-origin diagnostics,
and dedicated `SCN-011` conformance evidence implement the decision.

Deferred test/property artifacts, source-language actor/process integration,
on-disk output, packaging, release assembly, and optimization remain outside
the promoted backend boundary. Where a deferred construct reaches application
artifact generation, it is rejected rather than approximated.

## Related Decisions

- [ADR-0001: Control-Plane Authority](ADR-0001-control-plane-authority.md)
- [ADR-0002: Minimal Core And Library-First Surface](ADR-0002-minimal-core-and-library-first-surface.md)
- [ADR-0003: Explicit Effect Context Runtime](ADR-0003-explicit-effect-context-runtime.md)

## References

- [Core Compiler Pipeline](../compiler/core_compiler_pipeline.md)
- [Core Erlang And BEAM Backend](../compiler/core_erlang_and_beam_backend.md)
- [Backend Hardening Implementation Plan](../planning/backend-hardening/README.md)
- [Compiler Contract](../contracts/compiler_contract.md)
- [Testing And Quality Contract](../contracts/testing_and_quality_contract.md)
- [Spec Conformance Matrix](../conformance/spec_conformance_matrix.md)
