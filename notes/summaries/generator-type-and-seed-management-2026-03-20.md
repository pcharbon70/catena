# Generator Type And Seed Management - Section 1.2 Complete

**Date**: 2026-03-20
**Branch**: `codex/proptest-generator-stage`
**Status**: Complete
**Phase**: Property Testing Framework - Phase 1, Section 1.2

---

## Overview

Implemented the `Generator` abstraction and the deterministic seed/size
machinery that sits directly above Catena's rose-tree foundation.

The new surface lives in `src/proptest/catena_gen.erl` and establishes the
planned `Generator<A> = (Size, Seed) -> Tree<A>` model in an explicit Erlang
wrapper. It gives the property-testing track a real execution substrate for the
next phases: generator instances, primitive combinators, and runner
integration.

## Implemented Surface

### Generator Core

Added:

- `new/1` to construct a generator from a `(Size, Seed) -> Tree` function
- `run/3` to execute a generator with an explicit size and seed

### Seed Management

Added:

- `seed_new/0` for non-deterministic seed creation from runtime entropy
- `seed_from_int/1` for deterministic reproducibility
- `seed_split/1` for deterministic independent sub-streams
- `seed_next/1` for stepping a seed and extracting a pseudo-random word

Implementation note:

- the seed logic uses a small SplitMix64-style stepping function so deterministic
  behavior does not depend on OTP's internal `rand` implementation details

### Size Management

Added:

- `sized/1` for size-aware generator construction
- `resize/2` for overriding the size seen by a generator
- `scale/2` for transforming size in recursive/composed scenarios

Documented semantics:

- size `0` is the simplest generation context
- larger sizes allow more complex generation strategies
- size control is explicit rather than implicit global state

## Test Coverage

Added `test/proptest/catena_gen_tests.erl` covering:

- generator execution returning rose trees
- deterministic same-seed behavior
- distinct results for distinct seeds
- deterministic `seed_from_int/1`
- deterministic `seed_split/1`
- distinct streams after splitting
- `sized/1` receiving the current size
- `resize/2` overriding size
- `scale/2` transforming size
- `seed_new/0` producing usable runtime seeds

## Planning Reconciliation

Updated `notes/planning/property-testing/phase-01.md` to reconcile:

- Section `1.1` as complete in the planning checklist
- Section `1.2` as complete in the planning checklist

The next implementation step on the property-testing track is now:

- **Section 1.3: Categorical Instances for Generators**

## Why This Matters

This change moves Catena's internal property-testing framework from a single
foundational rose-tree data structure to an actual generator execution model.
That unlocks the next layer of work:

- functor/applicative/monad structure for generators
- primitive combinators
- future generic law verification on internal generators rather than ad hoc
  finite fixture sets

## Files Changed

| File | Change |
|------|--------|
| `src/proptest/catena_gen.erl` | New generator/seed/size module |
| `test/proptest/catena_gen_tests.erl` | New Section 1.2 EUnit coverage |
| `notes/planning/property-testing/phase-01.md` | Reconciled 1.1 and 1.2 checklist state |

## Next Step

- **Section 1.3: Categorical Instances for Generators**
