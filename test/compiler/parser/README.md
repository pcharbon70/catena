# Parser Property-Based Tests

## Quick Start

### Prerequisites
```bash
# Ensure PropEr is installed
rebar3 get-deps
rebar3 compile
```

### Run All Tests
```bash
# Run all parser property tests
rebar3 proper -m catena_parser_properties

# Run with verbose output
rebar3 proper -m catena_parser_properties --verbose
```

### Run Specific Property Categories

#### Robustness Tests
```bash
# Parser never crashes
rebar3 proper -m catena_parser_properties -p prop_parser_never_crashes -n 1000

# Malformed tokens handled gracefully
rebar3 proper -m catena_parser_properties -p prop_malformed_tokens_handle_gracefully -n 500
```

#### Grammar Tests
```bash
# Multi-argument type application (new feature)
rebar3 proper -m catena_parser_properties -p prop_multi_arg_type_constructor -n 200

# Category theory morphisms
rebar3 proper -m catena_parser_properties -p prop_category_morphisms

# Type application
rebar3 proper -m catena_parser_properties -p prop_type_application -n 200
```

#### AST Validation
```bash
# Trait parsing produces valid AST
rebar3 proper -m catena_parser_properties -p prop_trait_parse_valid_ast

# Round-trip parsing is idempotent
rebar3 proper -m catena_parser_properties -p prop_parser_invariant_roundtrip
```

## Test Coverage

### AST Structure Properties (4)
- `prop_simple_flow_parses` - Flow declarations
- `prop_simple_shape_parses` - Shape declarations
- `prop_simple_trait_parses` - Trait declarations
- `prop_ast_has_locations` - Location tracking

### Multi-Argument Type Application (2)
- `prop_multi_arg_type_constructor` - 1-5 type arguments
- `prop_category_morphisms` - Category theory types (c a a)

### Robustness Properties (2)
- `prop_parser_never_crashes` - Never crashes on any tokens
- `prop_malformed_tokens_handle_gracefully` - Graceful error handling

### Other Properties (2)
- `prop_parser_invariant_roundtrip` - Deterministic parsing
- `prop_module_structure` - Valid module structure

## Expected Results

**10 properties**, ~1000 test cases total

- Robustness: **100% pass rate**
- Valid grammars: **~95% pass rate** (some invalid combos expected)
- Multi-arg types: **100% pass rate**
- Round-trip: **100% pass rate**

## Property Details

### `prop_multi_arg_type_constructor`
Tests the recently added multi-argument type application feature.

**Generates**: `Triple a`, `Triple a b`, `Triple a b c`, etc. (1-5 args)

**Validates**:
- Parser accepts multi-arg types
- Correct AST structure
- Correct argument count

**Expected**: 100% pass (this feature is now implemented!)

### `prop_category_morphisms`
Tests category theory morphisms like `c a a`.

**Example**: `trait Category c where id : c a a end`

**Validates**:
- Type variable application with multiple args
- Higher-kinded types

**Expected**: 100% pass

### `prop_parser_never_crashes`
Critical robustness property.

**Generates**: Random token sequences (valid and invalid)

**Validates**: Parser returns `{ok, ...}` or `{error, ...}`, never crashes

**Expected**: 100% pass

## Integration with EUnit

Tests are also available via EUnit:

```bash
# Run via EUnit
rebar3 eunit --module=catena_parser_properties
```

EUnit runs 10 properties with 50 test cases each (500 total).

## Debugging

### Shrinking

PropEr automatically shrinks failing test cases:

```
Failed: After 234 test(s).
Shrinking .......(7 time(s))
Minimal failing case:
  Tokens = [{trait, 1}, {upper_ident, 1, "T"}, {'end', 1}]

Property prop_simple_trait_parses failed.
```

This shows the MINIMAL token sequence that triggers the bug.

### Verbose Mode

```bash
rebar3 proper -m catena_parser_properties --verbose
```

Shows each generated test case (useful for understanding what's being tested).
