# Lexer Property-Based Tests

## Quick Start

### Prerequisites
```bash
# Ensure PropEr is installed
rebar3 get-deps
rebar3 compile
```

### Run All Tests
```bash
# Run all lexer property tests (100 cases each)
rebar3 proper -m catena_lexer_properties

# Run with more test cases for thorough fuzzing
rebar3 proper -m catena_lexer_properties -n 1000
```

### Run Security Tests Only
```bash
# Test invalid escape rejection (CRITICAL)
rebar3 proper -m catena_lexer_properties -p prop_invalid_escapes_rejected -n 500

# Test null byte injection blocking
rebar3 proper -m catena_lexer_properties -p prop_null_byte_injection_blocked -n 500

# Test command injection blocking
rebar3 proper -m catena_lexer_properties -p prop_command_injection_blocked -n 500

# Test UTF-8/Unicode validation (CRITICAL)
./test_unicode_props.erl  # Runs all 7 UTF-8/Unicode properties
```

### Run Robustness Tests
```bash
# Test lexer never crashes
rebar3 proper -m catena_lexer_properties -p prop_lexer_never_crashes -n 1000
```

## Test Coverage

### Security Properties (11)

**String Security (3)**:
- `prop_invalid_escapes_rejected` - All invalid escapes rejected
- `prop_null_byte_injection_blocked` - Null byte attacks blocked
- `prop_command_injection_blocked` - Command injection blocked

**Resource Limits (1)**:
- `prop_comment_depth_limits` - Comment nesting depth limited (max 100)

**UTF-8/Unicode Validation (7)**:
- `prop_valid_unicode_codepoints` - Valid Unicode ranges accepted
- `prop_surrogate_codepoints_rejected` - UTF-16 surrogates rejected (CRITICAL)
- `prop_beyond_unicode_rejected` - Out-of-range code points rejected (CRITICAL)
- `prop_negative_codepoints_rejected` - Negative values rejected (CRITICAL)
- `prop_invalid_utf8_rejected` - Invalid UTF-8 sequences rejected (CRITICAL)
- `prop_valid_utf8_accepted` - Valid UTF-8 binaries accepted
- `prop_unicode_in_strings` - Mixed Unicode in strings works

### Valid Input Properties (7)
- `prop_valid_lower_ident` - Lowercase identifiers
- `prop_valid_upper_ident` - Uppercase identifiers
- `prop_valid_integer` - Integer literals
- `prop_valid_float` - Float literals
- `prop_valid_string_with_escapes` - Strings with escapes
- `prop_keywords_recognized` - Keywords
- `prop_safe_characters_in_strings` - Safe string chars

### Robustness Properties (3)
- `prop_lexer_never_crashes` - Never crashes on any input
- `prop_empty_string` - Empty strings work
- `prop_whitespace_ignored` - Whitespace ignored

### Additional Properties (13)
- Identifier length limits
- Scientific notation
- Leading zeros
- Empty string literals
- Multiple escapes
- Comments
- Operators
- Delimiters
- Multiple tokens
- Token boundaries

## Expected Results

All 35 properties should pass with 100% success rate.

Total test cases (default): **3500** (35 properties × 100 cases)

**Security-Critical**: 11 properties (1100 test cases) validate attack prevention
  - 3 string security properties (300 cases)
  - 1 resource limit property (100 cases)
  - 7 UTF-8/Unicode properties (700 cases)

## Security Properties Details

### `prop_comment_depth_limits`
Tests comment nesting depth limits to prevent stack overflow.

**Generates**: Nested comments from 1 to 150 levels deep

**Validates**:
- Comments ≤ 100 levels deep are accepted
- Comments > 100 levels are rejected
- Error includes actual depth and limit

**Expected**: 100% pass (stack overflow protection)

**Example**:
```catena
{- L1 {- L2 {- L3 ... {- L100 -} ... -} -} -}  ✅ OK
{- L1 {- L2 {- L3 ... {- L101 -} ... -} -} -}  ❌ ERROR
```

## Debugging Failed Properties

If a property fails, PropEr will show:
1. The failing test case (shrunk to minimal example)
2. The property that failed
3. The reason for failure

Example:
```
Failed: After 42 test(s).
Shrinking .....(5 time(s))
{"test\\x00string"}

Property prop_invalid_escapes_rejected failed with:
  Expected: {error, _, _}
  Got: {ok, ...}
```

This means the lexer accepted `\x00` (SECURITY BUG!)

## UTF-8/Unicode Security Properties

### `prop_surrogate_codepoints_rejected`
Tests that UTF-16 surrogate code points are rejected (security-critical).

**Generates**: All surrogate values (U+D800-U+DFFF, 2048 possible values)

**Validates**:
- All surrogates rejected with `invalid_unicode` error
- Error includes position, code point, and reason
- No crashes or exceptions

**Expected**: 100% pass (prevents UTF-16/UTF-8 conversion attacks)

**Why Critical**: Surrogates can cause crashes in codec libraries, bypass security checks

**Example**:
```erlang
catena_lexer:tokenize([16#D800])  ✅ REJECTED
% → {error, {0, catena_lexer, {invalid_unicode, 1, 55296, "UTF-16 surrogate code point"}}}
```

### `prop_invalid_utf8_rejected`
Tests that all invalid UTF-8 byte sequences are rejected (security-critical).

**Generates**: 7 types of invalid UTF-8
- Overlong encodings (e.g., `<<0xC0, 0xAF>>` for `/`)
- Incomplete multi-byte sequences (e.g., `<<0xC3>>`)
- Invalid start bytes (e.g., `<<0xFE>>`, `<<0xFF>>`)
- Standalone continuation bytes (e.g., `<<0x80>>`)

**Validates**:
- All rejected with `invalid_utf8` error
- Clear error messages
- No buffer overruns or crashes

**Expected**: 100% pass (prevents overlong encoding attacks)

**Why Critical**: Overlong encodings can bypass security filters

**Example**:
```erlang
catena_lexer:tokenize(<<16#C0, 16#AF>>)  ✅ REJECTED (overlong "/" encoding)
% → {error, {0, catena_lexer, {invalid_utf8, "Invalid UTF-8 byte sequence"}}}
```

### `prop_valid_unicode_codepoints`
Tests that all valid Unicode ranges are accepted.

**Generates**: Code points from 3 valid ranges
- Basic Multilingual Plane: U+0000-U+D7FF
- Private Use Area: U+E000-U+FFFF
- Supplementary Planes: U+10000-U+10FFFF (emoji, rare scripts)

**Validates**:
- All accepted in string literals
- Proper token generation
- Correct value preservation

**Expected**: 100% pass (correctness test)

**Example**:
```erlang
catena_lexer:tokenize([34, 16#1F30D, 34])  ✅ ACCEPTED (🌍 emoji)
% → {ok, [{string, 1, [127757]}]}
```

### Running UTF-8/Unicode Tests

```bash
# Run all 7 properties (700 test cases)
./test_unicode_props.erl

# Example output:
# Running 7 Unicode/UTF-8 property tests (100 cases each)...
#
#   prop_valid_unicode_codepoints            PASS
#   prop_surrogate_codepoints_rejected       PASS
#   prop_beyond_unicode_rejected             PASS
#   prop_negative_codepoints_rejected        PASS
#   prop_invalid_utf8_rejected               PASS
#   prop_valid_utf8_accepted                 PASS
#   prop_unicode_in_strings                  PASS
#
# ========================================
# Results: 7/7 properties passed
# ========================================
```

For detailed documentation, see: `notes/implementation/task-1.1.11-utf8-unicode-validation.md`
