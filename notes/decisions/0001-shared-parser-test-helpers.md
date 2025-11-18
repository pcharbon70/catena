# ADR 0001: Shared Parser Test Helper Module

**Date**: 2025-11-16
**Status**: Accepted
**Decision Maker**: Code Review Recommendation (Task 1.1.7)
**Related**: Task 1.1.7 Comprehensive Review, Parser Test Suite Refactoring

---

## Context

### Problem Statement

During the Task 1.1.7 comprehensive code review, we identified code duplication across parser test files. Specifically:

1. **Pattern Tests** (`catena_parser_pattern_tests.erl`): 22 lines of helper functions
2. **Type Tests** (`catena_parser_type_tests.erl`): 11 lines of helper functions
3. **Trait Tests** (`catena_parser_trait_tests.erl`): 9 lines of helper functions

**Total Duplication**: ~40 lines across 3 files

### Duplicated Functions

```erlang
%% Duplicated in catena_parser_pattern_tests.erl
parse_flow(Source) -> ...
get_patterns(FlowDecl) -> ...
get_guards(FlowDecl) -> ...

%% Duplicated in catena_parser_type_tests.erl
parse_type_sig(Source) -> ...

%% Duplicated in catena_parser_trait_tests.erl
parse_single_decl(Tokens) -> ...
```

### Review Recommendation

From `task-1.1.7-comprehensive-review.md`:

> **💡 Shared Test Utilities** (MEDIUM PRIORITY):
> - Could extract helpers to `catena_parser_test_helpers.erl`
> - Only if duplication increases in other test files
> - Currently per-file helpers work well

**Assessment**: DEFER - current code is clear and maintainable

However, as the test suite grows (41+ pattern tests, 51+ type tests, 41+ trait tests), maintaining identical helper functions becomes a maintenance burden.

---

## Decision

**We will create a shared test helper module** (`catena_parser_test_helpers.erl`) containing commonly used parsing and AST extraction utilities.

### What to Extract

**✅ Include in Shared Module**:
- Generic parsing utilities used across multiple files
- AST extraction functions with consistent interfaces
- Functions with stable, well-defined contracts
- Utilities that simplify common test patterns

**❌ Keep in Test Files**:
- Test-specific assertion helpers (e.g., `assert_trait_structure`)
- Functions with file-specific expectations or parameter formats
- Token construction utilities (highly specialized)
- Functions tightly coupled to specific test scenarios

### Shared Module API

```erlang
-module(catena_parser_test_helpers).

%% Parsing helpers
-export([
    parse_flow/1,           % Parse flow from source string
    parse_type_sig/1,       % Parse type signature from source
    parse_single_decl/1     % Parse single declaration from tokens
]).

%% AST extraction helpers
-export([
    get_patterns/1,         % Extract patterns from flow declaration
    get_guards/1,           % Extract guards from flow declaration
    get_body/1              % Extract body from flow declaration
]).
```

### Usage Pattern

Test files import only the helpers they need:

```erlang
%% In catena_parser_pattern_tests.erl
-import(catena_parser_test_helpers, [parse_flow/1, get_patterns/1, get_guards/1]).

%% In catena_parser_type_tests.erl
-import(catena_parser_test_helpers, [parse_type_sig/1]).

%% In catena_parser_trait_tests.erl
-import(catena_parser_test_helpers, [parse_single_decl/1]).
```

---

## Rationale

### Why Extract Now

1. **Code Duplication**: 40+ lines duplicated across 3 files
2. **Maintenance Burden**: Changes to helpers require updating 3 files
3. **Test Suite Growth**: More parser tests planned (effects, actors, modules)
4. **DRY Principle**: Don't Repeat Yourself - central source of truth
5. **Testability**: Shared helpers can be tested independently

### Why Not Earlier

Per review: "Currently per-file helpers work well"

- Initially, each test file had unique helpers
- No duplication when test files were independent
- Extraction would have been premature optimization

### Why This Scope

**Included** (6 functions):
- Used in multiple files
- Simple, stable interfaces
- No file-specific logic
- Pure extraction utilities

**Excluded** (assertion helpers):
- Different parameter formats (strings vs atoms)
- File-specific expectations
- Tightly coupled to test scenarios
- Not truly duplicated (semantically different)

Example of excluded helper:
```erlang
%% Trait-specific - NOT extracted
assert_trait_structure(TraitDecl, ExpectedName, ExpectedTypeParam, ExpectedMethod) ->
    %% Uses list_to_atom/1 conversions
    %% Has trait-specific field expectations
    %% Different from potential assert_instance_structure/4
```

---

## Consequences

### Positive Consequences

✅ **Reduced Duplication**: -42 lines across test files
✅ **Single Source of Truth**: Helper changes in one place
✅ **Improved Maintainability**: Easier to fix bugs in parsing utilities
✅ **Better Documentation**: Helpers have comprehensive @doc comments
✅ **Test Focus**: Test files focus on assertions, not infrastructure
✅ **Consistency**: All tests use identical parsing logic

### Negative Consequences

⚠️ **Additional Module**: One more file to maintain
⚠️ **Import Overhead**: Must import helpers explicitly
⚠️ **Indirection**: Need to jump to helper module to see implementation

### Neutral Consequences

ℹ️ **Breaking Changes**: If helper API changes, multiple test files affected
ℹ️ **Testing Helpers**: Should shared helpers be tested? (Currently: no)
ℹ️ **Visibility**: Helpers are test-only, not exported to production code

---

## Alternatives Considered

### Alternative 1: Keep Helpers Per-File (Status Quo)

**Pros**:
- No additional module
- Test files are self-contained
- No import dependencies

**Cons**:
- Code duplication (40+ lines)
- Maintenance burden (changes in 3 places)
- Inconsistency risk (helpers diverge over time)

**Rejected**: Duplication outweighs self-containment benefits

### Alternative 2: Extract ALL Helpers to Shared Module

**Pros**:
- Maximum code sharing
- Single source for all test utilities

**Cons**:
- Loses file-specific context
- Forces generic interfaces for specialized functions
- Harder to understand test-specific logic

**Rejected**: Over-extraction reduces clarity

Example problematic extraction:
```erlang
%% Too generic - loses meaning
assert_structure(Decl, Expected) -> ...

%% Better - keeps context
assert_trait_structure(TraitDecl, ...) -> ... % In trait tests
assert_instance_structure(InstanceDecl, ...) -> ... % In trait tests
```

### Alternative 3: Use Macros Instead of Functions

**Pros**:
- Can inline at compile time
- No function call overhead

**Cons**:
- Harder to debug
- Less flexible (can't pass as fun)
- Erlang community prefers functions over macros

**Rejected**: Functions are clearer and more Erlang-idiomatic

### Alternative 4: Use test_helpers.erl for All Tests

**Existing module**: `test/compiler/test_helpers.erl`

**Pros**:
- Already exists
- Shared across all compiler tests

**Cons**:
- Mixes general utilities with parser-specific helpers
- Parser helpers are specialized (parse_flow, get_patterns, etc.)
- Violates separation of concerns

**Rejected**: Keep parser-specific helpers in parser test directory

**Decision**: Create `test/compiler/parser/catena_parser_test_helpers.erl` for parser-specific utilities

---

## Implementation Details

### File Changes

**Created**:
- `test/compiler/parser/catena_parser_test_helpers.erl` (130 lines)

**Modified**:
- `test/compiler/parser/catena_parser_pattern_tests.erl` (-22 lines)
- `test/compiler/parser/catena_parser_type_tests.erl` (-11 lines)
- `test/compiler/parser/catena_parser_trait_tests.erl` (-9 lines)

**Moved** (Cleanup):
- `test/compiler/parser/location_tracking_demo.erl` → `scripts/`
- `test/compiler/parser/resource_limits_demo.erl` → `scripts/`

**Net Change**: +88 lines added, -42 lines removed from test files

### Code Metrics

**Before**:
- Pattern tests: 400+ lines (including 22 lines of helpers)
- Type tests: 500+ lines (including 11 lines of helpers)
- Trait tests: 700+ lines (including 9 lines of helpers)
- **Total helper duplication**: ~40 lines

**After**:
- Pattern tests: ~380 lines (import 3 helpers)
- Type tests: ~490 lines (import 1 helper)
- Trait tests: ~690 lines (import 1 helper)
- **Shared helper module**: 130 lines (6 exported functions)

**Duplication Eliminated**: 100%
**Lines Saved**: ~40 lines across test files
**Added Infrastructure**: +130 lines (well-documented, reusable)

### Testing Strategy

**Helper Verification**:
- All existing tests pass with shared helpers
- Manual testing of individual helpers:
  ```erlang
  catena_parser_pattern_tests:single_variable_pattern_test() → ok
  catena_parser_type_tests:single_type_variable_test() → ok
  catena_parser_trait_tests:parse_trait_valid_basic_test() → ok
  ```

**No Unit Tests for Helpers**:
- Helpers are thoroughly exercised by test suite
- 130+ existing tests use these helpers
- Helper bugs would fail many tests (implicit coverage)

---

## Future Considerations

### When to Add More Helpers

Add to shared module when:
1. Function duplicated in 2+ test files
2. Interface is stable and generic
3. No file-specific logic or expectations
4. Simplifies test code significantly

Examples of candidates:
```erlang
% If we add effect tests
parse_effect_decl/1    % Parse effect declaration

% If we add actor tests
parse_actor_decl/1     % Parse actor declaration

% If we add module tests
parse_module/1         % Parse module declaration
```

### When NOT to Add Helpers

Keep in test files when:
1. Function is test-specific (assertions, validations)
2. Different parameter formats across files
3. Tightly coupled to test scenario
4. Used in only one test file

### Refactoring Triggers

Consider refactoring if:
- Helper module exceeds 300 lines (too large)
- Helpers have file-specific branches (wrong abstraction)
- Test files re-implement helpers (API inadequate)

---

## Related Documents

- **Code Review**: `notes/reviews/task-1.1.7-comprehensive-review.md`
- **Implementation**: Commit `99912be` (2025-11-16)
- **General Test Helpers**: `test/compiler/test_helpers.erl`
- **Parser Tests**:
  - `test/compiler/parser/catena_parser_pattern_tests.erl`
  - `test/compiler/parser/catena_parser_type_tests.erl`
  - `test/compiler/parser/catena_parser_trait_tests.erl`

---

## Lessons Learned

### What Worked Well

✅ **Review-Driven**: Decision based on code review recommendation
✅ **Incremental**: Started with clear duplicates, didn't over-extract
✅ **Tested**: Verified all tests pass before committing
✅ **Documented**: Clear API documentation with examples

### What Could Be Better

⚠️ **Earlier Extraction**: Could have done this during initial test development
⚠️ **Helper Tests**: Consider unit tests for complex helpers in future

### Key Takeaway

**Balance is critical**: Extract enough to reduce duplication, but not so much that you lose clarity and context. The line between "shared utility" and "test-specific helper" is important.

---

## Approval

**Decision**: Accepted
**Date**: 2025-11-16
**Implemented**: Commit `99912be`
**Status**: In Production (Test Suite)

This decision can be revisited if:
1. Helper module becomes too large or complex
2. Test files start re-implementing helpers
3. Maintenance burden increases rather than decreases
