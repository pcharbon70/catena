# File Extension Change: .catena → .cat

## Summary

Changed the file extension for Catena source files from `.catena` to `.cat` throughout the codebase for brevity.

## Changes Made

### Renamed Files

6 stdlib files renamed:
- `lib/catena/stdlib/prelude.catena` → `prelude.cat`
- `lib/catena/stdlib/test.catena` → `test.cat`
- `lib/catena/stdlib/prelude_minimal.catena` → `prelude_minimal.cat`
- `lib/catena/stdlib/effect/io.catena` → `io.cat`
- `lib/catena/stdlib/effect/state.catena` → `state.cat`
- `lib/catena/stdlib/effect/error.catena` → `error.cat`

### Updated References

Updated `.catena` to `.cat` in 47 files across:
- Source code (`src/`)
- Test files (`test/`)
- Documentation (`notes/`, `docs/`)

Key files updated:
- `src/compiler/parser/catena_parse.erl` - code comments
- `src/compiler/types/catena_constraint.erl` - example comments
- `test/compiler/test_helpers.erl` - temp file paths
- `test/compiler/catena_compile_tests.erl` - stdlib file paths
- `test/compiler/integration/catena_stdlib_tests.erl` - stdlib file paths
- Multiple test files with test fixture filenames

## Test Results

All relevant tests pass:
- `catena_compile_tests`: 10/10 pass
- `catena_semantic_tests`: 12/12 pass
- `catena_stdlib_tests`: 18/19 pass (1 pre-existing parser issue)

## Rationale

- `.cat` is shorter and easier to type
- Maintains clear association with the Catena language
- Common convention for language file extensions to be 3-4 characters

## Branch

`feature/change-extension-to-cat`

## Files Modified

### Renamed (6 files)
- All `.catena` files in `lib/catena/stdlib/`

### Updated (47 files)
- Source files in `src/compiler/`
- Test files in `test/compiler/`
- Documentation in `notes/` and `docs/`
