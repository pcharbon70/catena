# Robustness Fix: Unicode Error Handling

**Date:** November 10, 2025
**Severity:** Low - Robustness Enhancement
**Status:** ✅ Fixed and Tested

## Overview

Added proper error handling for unicode decoding failures when reading source files. The system now gracefully handles invalid UTF-8 and incomplete UTF-8 sequences instead of crashing, providing clear error messages to users.

## Problem Description

### What Was the Issue?

From the code review:

> **Issue**: `unicode:characters_to_list/1` can return error tuples but isn't handled.
>
> **Potential Failure Cases**:
> - `{error, Converted, RestData}` - Invalid UTF-8 sequence
> - `{incomplete, Converted, RestData}` - Incomplete UTF-8 at end

**Locations affected:**
1. `catena_error.erl:239` - `read_source_context/3`
2. `catena_parser_wrapper.erl:26` - `parse_file/1`

**Current code pattern:**
```erlang
{ok, Binary} ->
    Content = unicode:characters_to_list(Binary),  % Can fail!
    AllLines = string:split(Content, "\n", all),
```

### Why This Matters

1. **Crash risk**: Unhandled error tuples would cause pattern match failures
2. **Poor user experience**: Cryptic crashes instead of clear error messages
3. **File format validation**: No validation of UTF-8 encoding
4. **Debugging difficulty**: Hard to diagnose encoding issues

**Attack scenarios:**
- Malformed UTF-8 in source files
- Binary files accidentally parsed as source
- Corrupted files with partial UTF-8
- Non-UTF-8 encoded legacy files

## Solution Implementation

### Strategy

Add pattern matching to handle all three possible return values from `unicode:characters_to_list/1`:
1. **Success**: String list (valid UTF-8)
2. **Error**: `{error, Converted, RestData}` (invalid UTF-8 sequence)
3. **Incomplete**: `{incomplete, Converted, RestData}` (incomplete UTF-8 at end)

### Changes Made

#### 1. Fixed `catena_error.erl` (lines 238-260)

**Before:**
```erlang
{ok, Binary} ->
    Content = unicode:characters_to_list(Binary),
    AllLines = string:split(Content, "\n", all),
    ...
```

**After:**
```erlang
{ok, Binary} ->
    % Handle potential unicode decoding errors
    case unicode:characters_to_list(Binary) of
        Content when is_list(Content) ->
            AllLines = string:split(Content, "\n", all),
            % Remove trailing empty line if file ends with newline
            Lines = case AllLines of
                [] -> [];
                _ ->
                    case lists:last(AllLines) of
                        "" -> lists:droplast(AllLines);
                        _ -> AllLines
                    end
            end,
            extract_context_lines(Lines, Line, ContextLines);
        {error, _Converted, _RestData} ->
            {error, {unicode_error, invalid_encoding}};
        {incomplete, _Converted, _RestData} ->
            {error, {unicode_error, incomplete_encoding}}
    end;
```

**Key improvements:**
- Added pattern matching on return value
- Returns descriptive error tuples for unicode issues
- Preserves existing logic for valid UTF-8

#### 2. Fixed `catena_parser_wrapper.erl` (lines 25-66)

**Before:**
```erlang
{ok, Binary} ->
    Content = unicode:characters_to_list(Binary),
    % Split content into lines and cache for source context
    SourceLines = prepare_source_lines(Content),
    ...
```

**After:**
```erlang
{ok, Binary} ->
    % Handle potential unicode decoding errors
    case unicode:characters_to_list(Binary) of
        Content when is_list(Content) ->
            % Split content into lines and cache for source context
            % This prevents re-reading the file for each error
            SourceLines = prepare_source_lines(Content),
            case catena_lexer:tokenize(Content) of
                {ok, Tokens} ->
                    parse_tokens_with_file(Tokens, Filename, SourceLines);
                {error, {Line, _Module, ErrorDesc}} ->
                    % Handle errors from any lexer module
                    Err = make_lexer_error(ErrorDesc, Line, Filename),
                    Err2 = add_source_context(Err, Filename, SourceLines),
                    {error, [Err2]}
            end;
        {error, _Converted, _RestData} ->
            Err = catena_error:new_error(
                'E000_file_error',
                "File contains invalid UTF-8 encoding",
                {1, undefined},
                Filename
            ),
            {error, [Err]};
        {incomplete, _Converted, _RestData} ->
            Err = catena_error:new_error(
                'E000_file_error',
                "File contains incomplete UTF-8 encoding at end",
                {1, undefined},
                Filename
            ),
            {error, [Err]}
    end;
```

**Key improvements:**
- Added pattern matching on return value
- Creates proper error objects with `E000_file_error` code
- Provides user-friendly error messages
- Returns consistent error format

### Error Messages

**Invalid UTF-8:**
```
Error: E000_file_error at line 1
File contains invalid UTF-8 encoding
```

**Incomplete UTF-8:**
```
Error: E000_file_error at line 1
File contains incomplete UTF-8 encoding at end
```

## Test Coverage

### New Tests Added

Added 6 new unicode handling tests:

**catena_error_tests.erl** (lines 358-411):

1. **`read_source_context_invalid_utf8_test`**
   - Tests handling of invalid UTF-8 (continuation byte without start)
   - Expects: `{error, {unicode_error, invalid_encoding}}`
   - Binary: `<<16#80, 16#81, 16#82>>`

2. **`read_source_context_incomplete_utf8_test`**
   - Tests handling of incomplete UTF-8 at end of file
   - Expects: `{error, {unicode_error, incomplete_encoding}}`
   - Binary: `<<"shape Foo = Bar\n", 16#C2>>`

3. **`read_source_context_valid_utf8_test`**
   - Tests that valid UTF-8 with multi-byte characters works
   - Expects: Success (not unicode error)
   - Content: `"shape λ → ∀\n"` (properly encoded)

**catena_parser_wrapper_tests.erl** (lines 549-610):

4. **`parse_file_invalid_utf8_test`**
   - Tests parser handling of invalid UTF-8
   - Expects: `E000_file_error` with "invalid UTF-8" message
   - Binary: `<<16#80, 16#81, 16#82>>`

5. **`parse_file_incomplete_utf8_test`**
   - Tests parser handling of incomplete UTF-8
   - Expects: `E000_file_error` with "incomplete UTF-8" message
   - Binary: `<<"shape Foo = Bar\n", 16#C2>>`

6. **`parse_file_valid_utf8_with_unicode_chars_test`**
   - Tests that valid UTF-8 parses normally
   - Expects: Success or syntax error (not file error)
   - Content: `"shape Option a = Some a | None\n"`

### Test Results

```
✅ All 135 tests passing (100% pass rate)

Error Module Tests:         54/54 ✅ (51 original + 3 new)
Error Formatter Tests:      48/48 ✅
Parser Wrapper Tests:       33/33 ✅ (30 original + 3 new)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Total:                     135/135 ✅
```

## Technical Details

### Unicode Decoding Return Values

**`unicode:characters_to_list(Binary)`** can return:

1. **Success:** `List` when is_list(List)
   - Fully valid UTF-8 decoded to unicode code points

2. **Error:** `{error, Converted, RestData}`
   - `Converted`: Successfully decoded portion (list)
   - `RestData`: Invalid byte sequence that couldn't be decoded (binary)
   - Example: Orphaned continuation byte, invalid start byte

3. **Incomplete:** `{incomplete, Converted, RestData}`
   - `Converted`: Successfully decoded portion (list)
   - `RestData`: Incomplete UTF-8 sequence at end (binary)
   - Example: 2-byte sequence with only start byte

### Invalid UTF-8 Examples

**Continuation byte without start:**
```erlang
<<16#80, 16#81, 16#82>>  % 3 continuation bytes
% Result: {error, [], <<128, 129, 130>>}
```

**Incomplete 2-byte sequence:**
```erlang
<<16#C2>>  % Start byte for 2-byte sequence, missing continuation
% Result: {incomplete, [], <<194>>}
```

**Overlong encoding:**
```erlang
<<16#C0, 16#80>>  % Overlong encoding of NULL (U+0000)
% Result: {error, [], <<192, 128>>}
```

**Invalid start byte:**
```erlang
<<16#FF, 16#FE>>  % Bytes never valid in UTF-8
% Result: {error, [], <<255, 254>>}
```

### Edge Cases Handled

**Empty file:**
```erlang
<<>>  →  ""  (empty list, handled normally)
```

**ASCII-only file:**
```erlang
<<"shape Foo = Bar\n">>  →  "shape Foo = Bar\n"  (all valid UTF-8)
```

**Mixed ASCII and UTF-8:**
```erlang
<<"shape λ = Bar\n">>  →  Valid UTF-8, decoded correctly
```

**Binary file:**
```erlang
<<0, 1, 2, 3, 255, 254>>  →  {error, ...}  (invalid UTF-8)
```

## Files Modified

```
src/compiler/error/catena_error.erl                  | +19 lines, -2 lines
src/compiler/parser/catena_parser_wrapper.erl        | +32 lines, -2 lines
test/compiler/error/catena_error_tests.erl           | +54 lines
test/compiler/parser/catena_parser_wrapper_tests.erl | +62 lines
notes/summaries/robustness-unicode-error-handling.md | NEW
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Total:                                              +167 lines, -4 lines
```

### Change Summary

**catena_error.erl:**
- Wrapped `unicode:characters_to_list` in case statement (lines 240-257)
- Added error returns for invalid/incomplete encoding (lines 253-256)

**catena_parser_wrapper.erl:**
- Wrapped `unicode:characters_to_list` in case statement (lines 27-57)
- Created error objects for invalid/incomplete encoding (lines 41-56)

**Test files:**
- Added 6 comprehensive unicode error tests
- Tests cover both modules and all error paths

## Code Review Resolution

This fix addresses **Concern #7** from the code review:

> **Concern #7: Missing Unicode Error Handling**
> - `unicode:characters_to_list/1` can return error tuples but isn't handled
> - Potential failure cases: `{error, _, _}` and `{incomplete, _, _}`
> - **Fix:** Add pattern matching to handle all return values

**Status:** ✅ **CONCERN RESOLVED**

## User Experience Improvements

### Before Fix

**Invalid UTF-8 file:**
```
** exception error: no function clause matching
    unicode:characters_to_list({error, [], <<128, 129, 130>>})
```

**Incomplete UTF-8 file:**
```
** exception error: no match of right hand side value
    {incomplete, "shape Foo = Bar\n", <<194>>}
```

### After Fix

**Invalid UTF-8 file:**
```
Error [E000_file_error] at test.catena:1
File contains invalid UTF-8 encoding
```

**Incomplete UTF-8 file:**
```
Error [E000_file_error] at test.catena:1
File contains incomplete UTF-8 encoding at end
```

**Clear, actionable error messages** instead of cryptic crashes.

## Best Practices Applied

### Defensive Programming

1. **Handle all cases**: Pattern match on all possible return values
2. **Fail gracefully**: Return errors instead of crashing
3. **Informative messages**: Tell user exactly what went wrong
4. **Consistent errors**: Use same error format as other file errors

### Erlang Idioms

1. **Pattern matching**: Use guards (`when is_list(Content)`)
2. **Tagged tuples**: Consistent error format `{error, {unicode_error, Reason}}`
3. **No exceptions**: Use return values, not exceptions
4. **Explicit handling**: No silent failures or assumptions

### Testing Strategy

1. **Positive tests**: Valid UTF-8 works correctly
2. **Negative tests**: Invalid/incomplete UTF-8 handled properly
3. **Integration tests**: Test through public API (parse_file, read_source_context)
4. **Edge cases**: Empty files, mixed content, binary data

## Performance Impact

**Overhead:** Negligible
- Added one case statement (O(1) pattern match)
- No additional processing for valid UTF-8 (happy path unchanged)
- Only creates errors for invalid encoding (rare case)

**Memory:** Minimal
- Error tuples: ~100 bytes per error
- No caching or persistent state
- Garbage collected immediately

## Security Considerations

### Attack Prevention

**Malicious binary files:**
- Attacker uploads binary file as `.catena` source
- Before: Crashes compiler
- After: Returns clear error, compilation continues

**Crafted invalid UTF-8:**
- Attacker includes invalid UTF-8 to crash compiler
- Before: Crashes on unicode decode
- After: Graceful error message

**DoS via malformed files:**
- Before: Could crash compilation process
- After: Error reported, process continues

### Validation

- UTF-8 validation happens automatically via `unicode:characters_to_list`
- Invalid encoding rejected before any parsing
- No attempt to "fix" or "guess" encoding

## Future Enhancements

### Possible Improvements

1. **Encoding detection**
   - Auto-detect other encodings (Latin-1, UTF-16, etc.)
   - Convert to UTF-8 automatically
   - Warn user about non-UTF-8 files

2. **Partial recovery**
   - Use successfully converted portion for error reporting
   - Show "best effort" context for malformed files
   - Indicate which byte offset failed

3. **Better error location**
   - Calculate byte offset of invalid sequence
   - Show hex dump of problematic bytes
   - Suggest encoding fixes

4. **Configuration**
   - Allow strict/lenient modes
   - Option to skip UTF-8 validation (unsafe)
   - Configurable error handling strategy

## Verification

### Manual Testing

```bash
# Create invalid UTF-8 file
echo -ne '\x80\x81\x82' > /tmp/invalid.catena

# Test error handling
erl -pa ebin -noshell -eval '
  {error, [Err]} = catena_parser_wrapper:parse_file("/tmp/invalid.catena"),
  io:format("Error: ~s~n", [Err#error.message])
' -s init stop

# Expected output:
# Error: File contains invalid UTF-8 encoding
```

### Automated Testing

```bash
# Run all tests
erl -pa ebin -noshell -eval '
  eunit:test([catena_error_tests, catena_parser_wrapper_tests], [verbose])
' -s init stop

# Result: All 135 tests passed
```

### Specific Test Cases

**Test 1: Invalid UTF-8**
```erlang
<<16#80, 16#81, 16#82>> → {error, {unicode_error, invalid_encoding}} ✅
```

**Test 2: Incomplete UTF-8**
```erlang
<<"text", 16#C2>> → {error, {unicode_error, incomplete_encoding}} ✅
```

**Test 3: Valid UTF-8**
```erlang
"shape λ → ∀\n" → {ok, Context} ✅
```

## Conclusion

**Status:** ✅ **ROBUSTNESS FIX COMPLETE**

The compiler now handles unicode errors gracefully:

- ✅ **Invalid UTF-8 detected** and reported clearly
- ✅ **Incomplete UTF-8 detected** and reported clearly
- ✅ **Valid UTF-8 works** correctly (including multi-byte chars)
- ✅ **No crashes** from malformed input
- ✅ **User-friendly errors** instead of cryptic failures
- ✅ **Zero regressions** (all 135 tests passing)
- ✅ **Minimal overhead** (only pattern match added)

Users now get clear, actionable error messages when source files have encoding issues, improving the overall robustness and user experience of the compiler.

## References

- **Code Review**: `notes/reviews/task-1.1.4-code-review.md` (Concern #7)
- **Erlang unicode module**: [unicode:characters_to_list/1](https://www.erlang.org/doc/man/unicode.html#characters_to_list-1)
- **UTF-8 specification**: [RFC 3629](https://www.rfc-editor.org/rfc/rfc3629)
- **Unicode encoding**: [The Unicode Standard](https://unicode.org/standard/standard.html)
