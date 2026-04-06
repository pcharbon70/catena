# Primitive Combinators - Section 1.4 Complete

**Date**: 2026-03-20
**Branch**: `codex/proptest-primitive-combinators`
**Status**: Complete
**Phase**: Property Testing Framework - Phase 1, Section 1.4

---

## Overview

Implemented the first concrete generator primitives on top of Catena's
generator/seed foundation and the categorical generator layer.

This stage turns `src/proptest/catena_gen.erl` from a compositional core into a
usable primitive-generation surface with basic values, filtering, and debugging
helpers.

## Implemented Surface

### Constant And Element Generators

Added:

- `constant/1`
- `element/1`
- `elements/1`

`element/1` includes shrinking toward earlier list values, so choice generators
already participate in the integrated shrink tree rather than behaving like
opaque random picks.

### Boolean Generator

Added:

- `gen_bool/0`
- `gen_bool/1`

Booleans shrink toward `false`, and the arity-1 form supports explicit
probability bias toward `true`.

### Integer Generators

Added:

- `gen_int/0`
- `gen_int/1`
- `gen_pos_int/0`
- `gen_neg_int/0`
- `gen_nat/0`

Implementation note:

- this stage uses explicit integer bounds and size-derived defaults as the
  current integer-generation surface
- first-class `Range` types remain the next planned step in Section `1.5`

Shrinking moves toward zero when it is in bounds, or toward the nearest allowed
origin when zero is outside the current bounds.

### Filter And Debug Utilities

Added:

- `gen_filter/2`
- `gen_such_that/2`
- `sample/1`
- `sample/2`
- `print_tree/1`
- `shrinks/1`

`gen_filter/2` prunes or promotes shrink candidates so returned shrink trees
continue satisfying the predicate rather than leaking invalid values.

## Test Coverage

Expanded `test/proptest/catena_gen_tests.erl` to cover:

- constant generation
- element selection and shrinking
- boolean generation and shrink behavior
- integer bounds and shrink origins
- positive/negative/natural helper generators
- filter semantics
- sampling and debug helpers

## Planning Reconciliation

Updated `notes/planning/property-testing/phase-01.md` to reconcile:

- Section `1.4` as complete
- the transitional integer-bounds interpretation before Section `1.5` range
  types

The next implementation step on the property-testing track is now:

- **Section 1.5: Range Types**

## Why This Matters

This change is the bridge between a generator framework and an actually usable
property-testing library. We now have:

- compositional generators
- concrete primitive values to compose
- shrink-aware filtering
- simple sampling/debugging tools for inspecting generator behavior

That sets up the next stage cleanly: making integer and later collection/string
generation use first-class range semantics instead of transitional explicit
bounds.

## Files Changed

| File | Change |
|------|--------|
| `src/proptest/catena_gen.erl` | Added primitive combinators, filter helpers, and debug utilities |
| `test/proptest/catena_gen_tests.erl` | Added Section 1.4 EUnit coverage |
| `notes/planning/property-testing/phase-01.md` | Reconciled Section 1.4 checklist state |

## Next Step

- **Section 1.5: Range Types**
