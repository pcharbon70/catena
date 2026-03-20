# Generator Categorical Instances - Section 1.3 Complete

**Date**: 2026-03-20
**Branch**: `codex/proptest-generator-instances`
**Status**: Complete
**Phase**: Property Testing Framework - Phase 1, Section 1.3

---

## Overview

Implemented the categorical instance layer for Catena generators on top of the
existing rose-tree and generator/seed foundation.

The core work landed in `src/proptest/catena_gen.erl`, extending the generator
surface from a minimal execution wrapper into a compositional API with functor,
applicative, monad, and alternative-style choice operators.

## Implemented Surface

### Functor

Added:

- `gen_map/2`
- `gen_map2/3`
- `gen_map3/4`
- `gen_map4/5`

These lift pure transformations over generator-produced rose trees while
preserving shrinking.

### Applicative

Added:

- `gen_pure/1`
- `gen_ap/2`

Implementation note:

- `gen_ap/2` documents and uses split seeds for independent function/value
  branches before combining the resulting trees with `catena_tree:ap/2`

### Monad

Added:

- `gen_bind/2`
- `gen_flatten/1`

Documented caveat:

- dependent generation inherits the usual shrinking trade-off where shrinking
  the first value re-runs the dependent generator rather than discovering a
  separately coordinated shrink space

### Alternative-Style Choice

Added:

- `gen_empty/0`
- `gen_alt/2`
- `gen_one_of/1`
- `gen_frequency/1`

This gives the property-testing layer a basic failure/choice surface ahead of
the later primitive-combinator phase.

## Test Coverage

Expanded `test/proptest/catena_gen_tests.erl` to cover:

- functor mapping and multi-argument helpers
- applicative composition over split-seed branches
- monadic dependent generation and flattening
- alternative/choice generators
- representative functor/applicative/monad law checks using concrete examples

## Planning Reconciliation

Updated `notes/planning/property-testing/phase-01.md` to reconcile:

- Section `1.3` as complete

The next implementation step on the property-testing track is now:

- **Section 1.4: Primitive Combinators**

## Why This Matters

This change gives Catena an actual compositional generator API rather than only
the substrate for one. That unlocks the next layer of work:

- primitive generators such as constants, elements, booleans, and integers
- higher-level structural combinators built on a real categorical base
- eventual generator-backed law verification on top of the same internal
  framework

## Files Changed

| File | Change |
|------|--------|
| `src/proptest/catena_gen.erl` | Added generator functor/applicative/monad/alternative operations |
| `test/proptest/catena_gen_tests.erl` | Added Section 1.3 EUnit coverage |
| `notes/planning/property-testing/phase-01.md` | Reconciled Section 1.3 checklist state |

## Next Step

- **Section 1.4: Primitive Combinators**
