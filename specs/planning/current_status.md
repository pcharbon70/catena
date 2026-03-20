# Current Status

This document reconciles Catena's current status from:

- `notes/planning/`
- `notes/summaries/`
- `notes/reviews/`
- the current codebase and test tree

It exists because some planning checklists are stale relative to later implementation summaries and code.

## Proof-Of-Concept Track

### Phase 1: Core Language Infrastructure

Current promoted status:

- Section 1.1: complete
- Section 1.2: complete
- Section 1.3: complete
- Section 1.4: complete
- Section 1.5: partial but materially advanced beyond the raw checklist

Reconciled Section 1.5 status:

- 1.5.1 Standard Library Compilation: complete
- 1.5.2 Trait Instance Resolution: complete
- 1.5.3 Higher-Kinded Type Validation: complete
- 1.5.4 Law Verification via Test Module: implemented for concrete suites
  - pure law definitions in `Laws` exist
  - structural tests for those law surfaces exist
  - concrete executable suites now run end to end through imported `Prelude`, `Test`, and `Laws` surfaces for `Maybe`, `Either`, and `List`
  - intentionally broken fixtures are proven to fail through the active test path
  - generic generator-backed law verification remains later staged work
- 1.5.5 Do-Notation Desugaring: implemented
- 1.5.6 Effect Integration with Kleisli Arrows: mostly implemented
- 1.5.7 Operator Desugaring: implemented

### Phase 2: REPL And Basic Runtime

Implementation summaries and code indicate:

- Phase 2.1 REPL: implemented
- Phase 2.2 Prelude/runtime bindings: implemented
- Phase 2.3 Testing framework: implemented
- Phase 2.4 Integration tests: implemented

The phase checklist in `notes/planning/proof-of-concept/phase-02.md` has not been reconciled to that later implementation state.

### Phase 3: Pattern Matching Engine

Implementation summaries and code indicate:

- Phase 3.1 advanced patterns: implemented
- Phase 3.2 decision trees: implemented
- Phase 3.3 exhaustiveness/redundancy checking: implemented
- Phase 3.4 integration tests: implemented

The phase checklist in `notes/planning/proof-of-concept/phase-03.md` has not been reconciled to that later implementation state.

### Phase 4 And Beyond

- Phase 4 module system: not complete
- current repo does include minimal import resolution and exported-environment wiring
- Phase 5 actor model integration: planned
- Phase 6 effect completion: planned
- Phase 7 distribution layer: planned/research-backed, not implemented

## Property-Testing Track

The property-testing planning documents are newer than the implementation and are also stale relative to the latest code.

Current promoted status:

- Phase 1 Section 1.1 rose-tree foundation: implemented
- Phase 1 Section 1.2 generator type and seed management: implemented
- Phase 1 Section 1.3 generator categorical instances: implemented
- Phase 1 Section 1.4 primitive combinators: implemented
- Phase 1 Section 1.5 range types: implemented
- `src/proptest/catena_tree.erl`, `src/proptest/catena_range.erl`, and `src/proptest/catena_gen.erl` provide the current concrete surface
- generator execution is deterministic for explicit seeds, size-aware by construction, and now supports primitive bool/int/element/filter/sample surfaces with first-class range-backed integer generation
- fixed integer bounds remain available explicitly through `gen_int_range/2` as compatibility glue
- the planning checklist is now reconciled through Section 1.5
- property-based law verification for the framework itself is intentionally deferred until the framework can test itself

Next clear step on this track:

- Phase 1 Section 1.6 Basic Shrinking Infrastructure

Longer-term destination on this track:

- Property Testing Phase 4 law testing provides the generic reusable destination for trait-law verification once generators and runner integration are mature

Immediate consequence of the completed concrete law-suite stage:

- the next law-verification step is no longer “make `Laws + Test.verify` executable”
- the next law-verification step is Property Testing Phase 1.6 and the broader Stage 3 generator/runner foundation

## Current Quality State

The default `rebar3 eunit` entry point is clean again after quarantining the legacy PropEr-based property suites outside the active `test/` compile tree and fixing the remaining active-suite regressions that surfaced afterward.

Promoted interpretation:

- the repo-wide `rebar3 eunit` path is green for active repo surfaces
- the internal property-testing transition is still underway
- historical PropEr suites remain preserved under `test_legacy/proper/` as migration targets rather than active default tests
