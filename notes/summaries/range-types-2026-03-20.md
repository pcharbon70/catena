# Range Types - Section 1.5 Complete

**Date**: 2026-03-20
**Branch**: `codex/proptest-range-types`
**Status**: Complete
**Phase**: Property Testing Framework - Phase 1, Section 1.5

---

## Overview

Implemented the first-class range layer for Catena's internal property-testing
framework.

This stage promotes range values into their own module, updates integer
generation to consume those range values directly, and preserves fixed-bound
compatibility with an explicit helper.

## Implemented Surface

### First-Class Range Values

Added `src/proptest/catena_range.erl` with:

- `range_bounds/2`
- `range_origin/1`
- `range_constant/1`
- `range_linear/2`
- `range_linear_from/3`
- `range_exponential/2`
- `range_exponential_from/3`

Each range carries:

- an origin value
- absolute min/max bounds
- a scaling strategy

Computed bounds now move toward the origin at small sizes and widen toward the
full range as size approaches Catena's current typical max size semantics.

### Integer Generation Integration

Updated `src/proptest/catena_gen.erl` so:

- `gen_int/1` now accepts first-class range values
- shrinking follows the range origin automatically
- `gen_int_range/2` preserves the old fixed-bound workflow explicitly
- `gen_int/0`, `gen_pos_int/0`, `gen_neg_int/0`, and `gen_nat/0` now sit on top
  of range-backed generation

This removes the transitional "tuple bounds as the main API" design from the
promoted surface while keeping compatibility available where it still helps.

## Test Coverage

Added `test/proptest/catena_range_tests.erl` covering:

- constant ranges remaining stable across sizes
- linear scaling around default and explicit origins
- exponential scaling behavior
- origin containment within computed bounds

Expanded `test/proptest/catena_gen_tests.erl` to cover:

- `gen_int/1` with first-class ranges
- shrinking toward explicit range origins
- `gen_int_range/2` compatibility behavior

## Planning Reconciliation

Updated `notes/planning/property-testing/phase-01.md` to reconcile:

- Section `1.5` as complete
- the new canonical integer-generation surface as range-backed
- the next implementation step as **Section 1.6: Basic Shrinking Infrastructure**

## Why This Matters

This stage turns ranges from planning vocabulary into executable generator
infrastructure. That gives Catena:

- predictable size-sensitive bounds
- explicit shrink targets
- a reusable abstraction the later list/text/collection generators can share
- a better foundation for generic property-based law verification

## Files Changed

| File | Change |
|------|--------|
| `src/proptest/catena_range.erl` | New first-class range module |
| `src/proptest/catena_gen.erl` | Integrated range-backed integer generation |
| `test/proptest/catena_range_tests.erl` | Added Section 1.5 EUnit coverage |
| `test/proptest/catena_gen_tests.erl` | Updated integer-generator coverage for ranges |
| `notes/planning/property-testing/phase-01.md` | Reconciled Section 1.5 checklist state |

## Next Step

- **Section 1.6: Basic Shrinking Infrastructure**
