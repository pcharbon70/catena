# Task 1.1.4: Error Recovery and Reporting - Feature Plan

**Status**: Planning → Implementation
**Created**: 2025-11-10
**Phase**: Phase 1 - Core Language Infrastructure
**Section**: 1.1 - Lexer and Parser
**Dependencies**: Tasks 1.1.1 (Lexer), 1.1.2 (Parser), 1.1.3 (AST) - All Complete

---

## 1. Problem Statement

### Current Limitations

The Catena compiler currently has **minimal error reporting capabilities**:

- **No error recovery**: Parser stops at first syntax error
- **Basic error messages**: Only what yecc provides by default (e.g., "syntax error before: token")
- **No source context**: Missing code snippets showing error location
- **No color coding**: Plain text output makes errors hard to spot
- **No helpful suggestions**: Users must figure out fixes themselves
- **Single error reporting**: Can't see multiple issues in one compilation pass

### Impact on Developer Experience

Poor error messages significantly harm productivity:

1. **Slow debugging**: Developers fix one error, recompile, discover next error (repeat)
2. **Confusion**: Vague messages like "syntax error" don't explain the problem
3. **Frustration**: No visual highlighting makes errors hard to locate in source
4. **Learning curve**: Beginners can't understand what they did wrong
5. **Time waste**: Multiple compile cycles for multiple errors

### What Good Error Messages Provide

Industry-standard compilers (Rust, Elm, TypeScript) show us that excellent error messages should:

- **Show exact location**: Line and column numbers with visual indicators
- **Provide context**: Display surrounding source code with highlighting
- **Explain the problem**: Clear description of what went wrong
- **Suggest solutions**: Actionable advice on how to fix it
- **Use color**: Visual hierarchy helps focus attention
- **Report multiple errors**: Find as many issues as possible in one pass

**Example of Target Quality** (Rust-style):

```
error: expected `end` but found end of file
  --> example.catena:5:10
   |
 3 | flow factorial n = match n
 4 |   | 0 -> 1
 5 |   | n -> n * factorial (n - 1)
   |          ^^^^^^^^^^^^^^^^^^^^^^ missing `end` keyword
   |
help: try adding `end` to close the match expression
```

---

## 2. Solution Overview

### High-Level Approach

We'll implement a **comprehensive error recovery and reporting system** with four main components:

1. **Error Data Structures** - Structured error representation
2. **Source Context Extraction** - Read and highlight source code
3. **Error Formatting** - Pretty-print with ANSI colors and suggestions
4. **Parser Error Recovery** - Continue parsing after errors (panic mode)

### Key Design Principles

1. **Accumulate, don't abort**: Collect multiple errors before reporting
2. **Context is king**: Always show source code around the error
3. **Be helpful**: Provide actionable suggestions, not just complaints
4. **Visual hierarchy**: Use color to guide the eye
5. **Performance acceptable**: Error path can be slower than success path
6. **Fail gracefully**: Never crash on malformed input

### Integration Points

**Parser Integration** (yecc):
- Use yecc's `Erlang code.` section to add error recovery hooks
- Catch parse errors and add to error accumulator
- Implement panic mode by inserting dummy tokens

**Lexer Integration**:
- Lexer already provides line numbers in tokens
- Add column tracking if not present
- Report lexical errors (invalid characters, unterminated strings)

**Error Reporter Module**:
- New module: `catena_error.erl`
- Central error formatting and display
- Reusable across all compiler phases

---

## 3. Technical Details

### File Structure

```
src/compiler/error/
├── catena_error.erl          # Main error reporting module (NEW)
└── catena_error_formatter.erl # ANSI formatting utilities (NEW)

src/compiler/parser/
├── catena_parser.yrl          # Add error recovery (MODIFY)
└── catena_parser.erl          # Generated, will include our hooks

src/compiler/lexer/
└── catena_lexer.xrl           # Column tracking (MODIFY if needed)

test/compiler/error/
├── catena_error_tests.erl           # Error structure tests (NEW)
└── catena_error_formatter_tests.erl # Formatting tests (NEW)

test/compiler/parser/
└── catena_parser_error_tests.erl    # Integration tests (NEW)
```

### Error Data Structure

```erlang
-record(error, {
    severity :: error | warning | note,
    code :: atom(),              % e.g., 'E001_syntax_error'
    message :: string(),          % Primary error message
    file :: string() | undefined, % Source file path
    line :: pos_integer(),        % Line number (1-indexed)
    column :: pos_integer() | undefined, % Column number (1-indexed)
    source_line :: string() | undefined, % The actual source line
    context_before :: [string()], % Lines before error (for context)
    context_after :: [string()],  % Lines after error (for context)
    suggestion :: string() | undefined, % How to fix it
    related :: [#error{}]         % Related errors/notes
}).

-type error() :: #error{}.
-type error_list() :: [error()].
```

### Dependencies

**Erlang/OTP Modules**:
- `file` - Read source files for context
- `io` / `io_lib` - Output formatting
- `unicode` - Proper string handling
- `lists` - List manipulation
- `string` - String operations

**ANSI Color Codes**:
- We'll implement ANSI escape sequences for terminal colors
- Detect color support (check `TERM` environment variable)
- Graceful fallback to plain text if colors not supported

**Yecc Error Handling**:
- yecc provides `'$undefined'` for error recovery
- We can insert tokens to continue parsing
- Error productions catch common mistakes

---

## 4. Architecture

### Component Diagram

```
┌─────────────┐
│   Lexer     │
│ (tokens)    │
└──────┬──────┘
       │ tokens with locations
       ↓
┌─────────────┐     errors      ┌─────────────────┐
│   Parser    │ ← ← ← ← ← ← ← ← │ Error           │
│ (yecc)      │                  │ Accumulator     │
└──────┬──────┘                  └────────┬────────┘
       │ AST or errors                    │
       ↓                                  │
┌─────────────┐                           │
│ Compiler    │                           │
│ Frontend    │                           │
└──────┬──────┘                           │
       │                                  │
       │ on errors                        │
       └─────────────────→───────────────→┘
                                          ↓
                                  ┌───────────────┐
                                  │ Error         │
                                  │ Formatter     │
                                  └───────┬───────┘
                                          │
                                          ↓
                                  ┌───────────────┐
                                  │ Terminal      │
                                  │ Output        │
                                  └───────────────┘
```

### Error Flow

1. **Detection**: Lexer or parser encounters invalid input
2. **Collection**: Error added to accumulator (with location)
3. **Recovery** (parser only): Attempt to continue parsing
4. **Context**: Read source file to get code snippets
5. **Format**: Apply ANSI colors, layout error message
6. **Display**: Print to stderr with proper formatting

### Panic Mode Recovery Strategy

When the parser encounters an error:

1. **Detect**: yecc calls error production or returns `{error, ...}`
2. **Record**: Add error to accumulator with current token location
3. **Synchronize**: Skip tokens until we reach a synchronization point
4. **Resume**: Continue parsing from synchronization point

**Synchronization Points** (where we can safely resume):
- `end` keyword (closes blocks)
- Declaration boundaries (`shape`, `flow`, `effect`)
- Statement terminators (`;`, newline in some contexts)
- Closing delimiters (`}`, `]`, `)`)

---

## 5. Success Criteria

### Functional Requirements

✅ **Multiple Error Reporting**:
- Can report at least 3-5 errors in one compilation pass
- Errors don't corrupt parser state for subsequent declarations

✅ **Source Context Display**:
- Shows 2 lines before and after error location
- Highlights error line with color (red)
- Indicates exact column with caret (^) or wavy underline

✅ **Color Output**:
- Red for errors
- Yellow/orange for warnings
- Blue/cyan for notes/hints
- Dim/gray for context lines
- Bold for important text

✅ **Helpful Suggestions**:
- At least 10 common error patterns with suggestions
- Examples: missing `end`, unmatched delimiters, typos in keywords

✅ **Clean Formatting**:
- File:line:column format for IDE integration
- Code snippets with line numbers
- Proper unicode handling for non-ASCII source

### Quality Requirements

✅ **All Tests Passing**:
- Unit tests for error data structures
- Unit tests for ANSI formatting
- Integration tests for parser error recovery
- At least 20+ test cases covering common errors

✅ **Performance**:
- Error reporting adds < 100ms overhead
- Source file reading cached (don't re-read for each error)

✅ **Robustness**:
- Never crashes on malformed input
- Handles edge cases (empty files, very long lines, binary data)
- Graceful degradation if source file unavailable

---

## 6. Implementation Plan

### Step 1: Error Data Structures (2-3 hours)

**Goal**: Define error representation and basic API

**Tasks**:
- Create `src/compiler/error/catena_error.erl`
- Define `#error{}` record with all fields
- Implement `new_error/4`, `add_suggestion/2`, `add_context/3`
- Implement error accumulator functions
- Write unit tests (10-15 tests)

**Files**:
- `src/compiler/error/catena_error.erl` (NEW, ~200 lines)
- `test/compiler/error/catena_error_tests.erl` (NEW, ~300 lines)

**Testing Approach**:
```erlang
% Test creating errors
new_error_test() ->
    Err = catena_error:new_error(error, 'E001', "Syntax error", {1, 5}),
    ?assertEqual(error, Err#error.severity),
    ?assertEqual('E001', Err#error.code).

% Test adding suggestions
add_suggestion_test() ->
    Err = catena_error:new_error(error, 'E001', "Missing end", {10, 1}),
    Err2 = catena_error:add_suggestion(Err, "Add 'end' to close match"),
    ?assertEqual("Add 'end' to close match", Err2#error.suggestion).
```

**Success**: Can create error records and manipulate them

---

### Step 2: Source Context Extraction (2-3 hours)

**Goal**: Read source files and extract code snippets around errors

**Tasks**:
- Implement `read_source_context/3` - gets lines around error
- Handle edge cases (file not found, line out of bounds, binary content)
- Cache source file contents (don't re-read for each error)
- Write unit tests (10-15 tests)

**Files**:
- `src/compiler/error/catena_error.erl` (MODIFY, add ~100 lines)
- `test/compiler/error/catena_error_tests.erl` (MODIFY, add tests)

**API Design**:
```erlang
-spec read_source_context(File, Line, ContextLines) -> {ok, Context} | {error, Reason}
    when File :: string(),
         Line :: pos_integer(),
         ContextLines :: pos_integer(),
         Context :: #{
           before => [string()],
           error_line => string(),
           after => [string()]
         }.
```

**Testing Approach**:
```erlang
% Create test file
setup() ->
    TestFile = "/tmp/test.catena",
    Content = "line 1\nline 2\nline 3\nline 4\nline 5\n",
    file:write_file(TestFile, Content),
    TestFile.

% Test context extraction
read_context_test() ->
    File = setup(),
    {ok, Context} = catena_error:read_source_context(File, 3, 1),
    ?assertEqual(["line 2"], maps:get(before, Context)),
    ?assertEqual("line 3", maps:get(error_line, Context)),
    ?assertEqual(["line 4"], maps:get(after, Context)).
```

**Success**: Can extract source context for any line number

---

### Step 3: ANSI Color Formatting (3-4 hours)

**Goal**: Implement colored terminal output with proper formatting

**Tasks**:
- Create `src/compiler/error/catena_error_formatter.erl`
- Implement ANSI escape code functions
- Detect color support (check `TERM` environment)
- Create error message templates
- Format code snippets with line numbers and highlights
- Write unit tests (15-20 tests)

**Files**:
- `src/compiler/error/catena_error_formatter.erl` (NEW, ~300 lines)
- `test/compiler/error/catena_error_formatter_tests.erl` (NEW, ~400 lines)

**API Design**:
```erlang
-spec format_error(Error) -> iolist() when Error :: error().
-spec format_error_list(Errors) -> iolist() when Errors :: [error()].
-spec supports_color() -> boolean().
```

**ANSI Color Functions**:
```erlang
red(Text) -> ["\e[31m", Text, "\e[0m"].
yellow(Text) -> ["\e[33m", Text, "\e[0m"].
blue(Text) -> ["\e[34m", Text, "\e[0m"].
bold(Text) -> ["\e[1m", Text, "\e[0m"].
dim(Text) -> ["\e[2m", Text, "\e[0m"].
```

**Error Message Template**:
```
error[E001]: expected `end` but found end of file
  --> example.catena:5:10
   |
 3 | flow factorial n = match n
 4 |   | 0 -> 1
 5 |   | n -> n * factorial (n - 1)
   |          ^^^^^^^^^^^^^^^^^^^^^^ missing `end` keyword
   |
help: try adding `end` to close the match expression
```

**Testing Approach**:
```erlang
% Test ANSI codes
red_text_test() ->
    Result = catena_error_formatter:red("error"),
    ?assertEqual("\e[31merror\e[0m", lists:flatten(Result)).

% Test error formatting
format_simple_error_test() ->
    Err = catena_error:new_error(error, 'E001', "Syntax error", {5, 10}),
    Formatted = catena_error_formatter:format_error(Err),
    ?assert(string:find(Formatted, "error[E001]") /= nomatch),
    ?assert(string:find(Formatted, "5:10") /= nomatch).
```

**Success**: Can format errors with colors and proper layout

---

### Step 4: Parser Error Recovery (4-5 hours)

**Goal**: Modify parser to continue after syntax errors

**Tasks**:
- Add error productions to `catena_parser.yrl`
- Implement synchronization point logic
- Integrate error accumulator
- Add yecc `Erlang code.` section with recovery functions
- Write parser error tests (15-20 tests)

**Files**:
- `src/compiler/parser/catena_parser.yrl` (MODIFY, add ~150 lines)
- `src/compiler/parser/catena_parser.erl` (REGENERATE)
- `test/compiler/parser/catena_parser_error_tests.erl` (NEW, ~500 lines)

**Yecc Error Productions**:
```erlang
% Error recovery for shape declarations
shape_decl -> shape upper_ident equals error :
    {error_shape, unwrap_value('$2'), line('$1')}.

% Error recovery for flow declarations
flow_decl -> flow lower_ident equals error :
    {error_flow, unwrap_value('$2'), line('$1')}.

% Error recovery for expressions (missing end)
expr -> match expr of match_clauses error :
    {error_match, '$2', '$4', line('$1')}.
```

**Erlang Code Section** (in catena_parser.yrl):
```erlang
Erlang code.

-export([parse_with_recovery/1]).

parse_with_recovery(Tokens) ->
    case parse(Tokens) of
        {ok, AST} -> {ok, AST, []};
        {error, {Line, _Module, Message}} ->
            % Collect error
            Error = make_parse_error(Line, Message),
            % Attempt recovery
            case try_recovery(Tokens, Line) of
                {ok, AST, More Errors} -> {ok, AST, [Error | MoreErrors]};
                {error, _} -> {error, [Error]}
            end
    end.

try_recovery(Tokens, ErrorLine) ->
    % Skip to next synchronization point
    SyncTokens = skip_to_sync_point(Tokens, ErrorLine),
    parse_with_recovery(SyncTokens).

skip_to_sync_point([], _) -> [];
skip_to_sync_point([{TokenType, _} = Token | Rest], ErrorLine) ->
    case is_sync_point(TokenType) of
        true -> [Token | Rest];
        false -> skip_to_sync_point(Rest, ErrorLine)
    end.

is_sync_point('end') -> true;
is_sync_point(shape) -> true;
is_sync_point(flow) -> true;
is_sync_point(effect) -> true;
is_sync_point(rbrace) -> true;
is_sync_point(_) -> false.
```

**Testing Approach**:
```erlang
% Test missing 'end' keyword
missing_end_test() ->
    Tokens = [
        {flow, 1}, {lower_ident, 1, foo}, {equals, 1},
        {match, 1}, {lower_ident, 1, x}, {of, 1},
        {integer, 2, 1}, {arrow, 2}, {integer, 2, 42}
        % Missing {end, 2}
    ],
    {ok, AST, Errors} = catena_parser:parse_with_recovery(Tokens),
    ?assertEqual(1, length(Errors)),
    [Err] = Errors,
    ?assert(string:find(Err#error.message, "end") /= nomatch).

% Test multiple errors in one pass
multiple_errors_test() ->
    Tokens = [
        % Shape with missing equals
        {shape, 1}, {upper_ident, 1, 'Bool'}, {pipe, 1}, {upper_ident, 1, 'True'},
        % Flow with missing end
        {flow, 3}, {lower_ident, 3, foo}, {equals, 3}, {match, 3}, {lower_ident, 3, x}
    ],
    {ok, AST, Errors} = catena_parser:parse_with_recovery(Tokens),
    ?assert(length(Errors) >= 2).
```

**Success**: Parser can recover from errors and report multiple issues

---

### Step 5: Helpful Suggestions (2-3 hours)

**Goal**: Pattern match common errors and provide actionable suggestions

**Tasks**:
- Implement error pattern matching in `catena_error.erl`
- Create suggestion database for 10+ common errors
- Add suggestion to errors during formatting
- Write tests for each suggestion pattern

**Files**:
- `src/compiler/error/catena_error.erl` (MODIFY, add ~150 lines)
- `test/compiler/error/catena_error_tests.erl` (MODIFY, add tests)

**Common Error Patterns**:

1. **Missing `end` keyword**
   - Pattern: "syntax error" + last token is expression
   - Suggestion: "Add 'end' to close the [match/if/do/let] expression"

2. **Unmatched opening delimiter**
   - Pattern: "unexpected end of file" + unclosed `{`, `[`, `(`
   - Suggestion: "Add closing [} / ] / )] to match opening delimiter at line X"

3. **Typo in keyword**
   - Pattern: identifier similar to keyword (edit distance)
   - Suggestion: "Did you mean '[correct keyword]'?"

4. **Missing `=` in declaration**
   - Pattern: "syntax error" after shape/flow name
   - Suggestion: "Add '=' after declaration name"

5. **Missing pattern arrow `->`**
   - Pattern: "syntax error" between pattern and expression
   - Suggestion: "Add '->' between pattern and expression"

6. **Duplicate declaration name**
   - Pattern: Name already defined
   - Suggestion: "Rename this declaration or remove the previous one at line X"

7. **Missing pipe in ADT**
   - Pattern: Constructor not separated by `|`
   - Suggestion: "Separate constructors with '|'"

8. **Missing type annotation colon**
   - Pattern: "syntax error" before type
   - Suggestion: "Add ':' before type annotation"

9. **Unterminated string**
   - Pattern: Lexer error on string
   - Suggestion: "Add closing quote to terminate string"

10. **Invalid operator sequence**
    - Pattern: Two operators in a row
    - Suggestion: "Remove extra operator or add operand"

**Implementation**:
```erlang
-spec add_helpful_suggestion(Error) -> Error when Error :: error().
add_helpful_suggestion(#error{message = Message} = Err) ->
    Suggestion = case detect_error_pattern(Message, Err) of
        {missing_end, Context} ->
            io_lib:format("Add 'end' to close the ~s expression", [Context]);
        {typo_keyword, {Got, Expected}} ->
            io_lib:format("Did you mean '~s'?", [Expected]);
        {unmatched_delimiter, {Delim, Line}} ->
            io_lib:format("Add closing '~s' to match opening at line ~p", [Delim, Line]);
        unknown ->
            undefined
    end,
    Err#error{suggestion = Suggestion}.

detect_error_pattern(Message, #error{line = Line, source_line = Source}) ->
    % Pattern matching logic here
    case Message of
        _ when string:find(Message, "syntax error") /= nomatch ->
            % Check for missing end
            check_missing_end(Source);
        _ when string:find(Message, "unexpected") /= nomatch ->
            check_unmatched_delimiters(Source, Line);
        _ ->
            check_typos(Message)
    end.
```

**Testing Approach**:
```erlang
missing_end_suggestion_test() ->
    Err = catena_error:new_error(
        error, 'E001',
        "syntax error before: <end of file>",
        {10, 1}
    ),
    Err2 = catena_error:add_helpful_suggestion(Err),
    ?assert(string:find(Err2#error.suggestion, "end") /= nomatch).
```

**Success**: Common errors get helpful, actionable suggestions

---

### Step 6: Integration Testing (2-3 hours)

**Goal**: End-to-end tests of complete error reporting pipeline

**Tasks**:
- Create test Catena source files with intentional errors
- Test full compilation with error reporting
- Verify error messages match expected format
- Test error recovery finds multiple issues
- Verify color output when appropriate

**Files**:
- `test/compiler/integration/error_reporting_integration_tests.erl` (NEW, ~600 lines)
- `test/fixtures/error_*.catena` (NEW, multiple test files)

**Test Scenarios**:

1. **Single syntax error**
   - Input: Catena file with one missing `end`
   - Expected: One error with suggestion, source context, colors

2. **Multiple errors in one file**
   - Input: File with 3-5 different errors
   - Expected: All errors reported, source context for each

3. **Lexer error**
   - Input: File with invalid character or unterminated string
   - Expected: Lexer error with context

4. **Parser error recovery**
   - Input: File with error in first function, valid function after
   - Expected: Error reported, parser continues, second function parsed

5. **Complex nested error**
   - Input: Deeply nested expression with error
   - Expected: Clear indication of error location within nesting

6. **Empty file edge case**
   - Input: Empty file
   - Expected: Appropriate error or warning

7. **Very long line**
   - Input: Source with 500+ character line
   - Expected: Truncated context, doesn't overflow terminal

**Testing Approach**:
```erlang
% Test file with single error
single_error_integration_test() ->
    Source = "flow foo = match x of\n  1 -> 42\n",
    % Missing 'end'

    {error, Errors} = compile_source(Source),
    ?assertEqual(1, length(Errors)),

    [Err] = Errors,
    ?assertEqual(error, Err#error.severity),
    ?assert(string:find(Err#error.message, "end") /= nomatch),
    ?assert(Err#error.suggestion /= undefined).

% Test multiple errors found
multiple_errors_integration_test() ->
    Source =
        "shape Bool = True False\n"  % Missing '|'
        "flow foo = match x of\n"    % Missing 'end'
        "  1 -> 42\n",

    {error, Errors} = compile_source(Source),
    ?assert(length(Errors) >= 2).
```

**Success**: Full compilation pipeline with error reporting works end-to-end

---

## 7. Testing Strategy

### Unit Tests

**Error Module Tests** (~15 tests):
- Creating error records
- Adding suggestions
- Adding context
- Error accumulation
- Edge cases (undefined values, empty lists)

**Formatter Module Tests** (~20 tests):
- ANSI color codes
- Color support detection
- Error message formatting
- Code snippet formatting with line numbers
- Column highlighting
- Multiple error formatting
- Unicode handling

**Parser Error Tests** (~20 tests):
- Each error production rule
- Synchronization point detection
- Recovery from each error type
- Multiple errors in one parse
- Error location tracking

### Integration Tests (~10 tests):
- Full compilation with errors
- Multiple files with errors
- Lexer + parser errors together
- Color output verification
- Performance (error reporting overhead)

### Total: ~65 comprehensive tests

---

## 8. Notes and Considerations

### ANSI Color Support

**Detection Strategy**:
```erlang
supports_color() ->
    case os:getenv("TERM") of
        false -> false;  % No TERM variable
        "dumb" -> false; % Dumb terminal
        _ ->
            % Check NO_COLOR environment variable
            case os:getenv("NO_COLOR") of
                false -> true;  % Colors supported
                _ -> false      % NO_COLOR set, disable colors
            end
    end.
```

**Fallback**: Always have plain text version without ANSI codes

### Internationalization (Future)

Currently English-only messages, but structure allows future i18n:
- Error codes (E001, W002) are language-independent
- Message templates can be replaced with translations
- Defer full i18n to later phase

### Performance Considerations

- **Source file caching**: Read once, cache in memory for multiple errors
- **Lazy context extraction**: Only read context when formatting, not during collection
- **Limit context size**: Max 5 lines before/after to avoid huge output
- **Truncate long lines**: Cap at 120 characters to prevent terminal overflow

**Acceptable Performance Target**:
- Error reporting adds < 100ms for up to 10 errors
- Source file reading < 50ms per file
- Formatting < 10ms per error

### Terminal Compatibility

**Testing On**:
- bash / zsh (Linux/Mac)
- PowerShell / cmd (Windows)
- VS Code integrated terminal
- IntelliJ IDEA terminal

**Handle Edge Cases**:
- Terminal without color support
- Very narrow terminals (< 80 columns)
- Redirected output (pipes, files)

### Future Enhancements

**Not in this task** (defer to future):
1. **IDE integration** - LSP error reporting format
2. **JSON output mode** - Machine-readable errors
3. **Error quickfix** - Automatic fixes for simple errors
4. **Error statistics** - Most common errors over time
5. **Custom error handlers** - Plugin system for error processing
6. **Detailed error docs** - Web page for each error code with examples
7. **Warning levels** - Configurable strictness

---

## 9. Implementation Checklist

### ☐ Step 1: Error Data Structures (2-3 hours)
- [ ] Create `catena_error.erl` module
- [ ] Define `#error{}` record
- [ ] Implement `new_error/4`
- [ ] Implement `add_suggestion/2`
- [ ] Implement `add_context/3`
- [ ] Implement error accumulator functions
- [ ] Write 15 unit tests
- [ ] All tests passing

### ☐ Step 2: Source Context Extraction (2-3 hours)
- [ ] Implement `read_source_context/3`
- [ ] Handle file not found
- [ ] Handle line out of bounds
- [ ] Implement file caching
- [ ] Write 15 unit tests
- [ ] All tests passing

### ☐ Step 3: ANSI Color Formatting (3-4 hours)
- [ ] Create `catena_error_formatter.erl` module
- [ ] Implement ANSI color functions
- [ ] Implement `supports_color/0`
- [ ] Implement `format_error/1`
- [ ] Implement `format_error_list/1`
- [ ] Create error message template
- [ ] Format code snippets with highlighting
- [ ] Write 20 unit tests
- [ ] All tests passing

### ☐ Step 4: Parser Error Recovery (4-5 hours)
- [ ] Add error productions to `catena_parser.yrl`
- [ ] Implement `parse_with_recovery/1`
- [ ] Implement `skip_to_sync_point/2`
- [ ] Implement `is_sync_point/1`
- [ ] Add error accumulator integration
- [ ] Regenerate parser
- [ ] Write 20 parser error tests
- [ ] All tests passing

### ☐ Step 5: Helpful Suggestions (2-3 hours)
- [ ] Implement `add_helpful_suggestion/1`
- [ ] Implement `detect_error_pattern/2`
- [ ] Add 10+ error pattern detectors
- [ ] Write tests for each pattern
- [ ] All tests passing

### ☐ Step 6: Integration Testing (2-3 hours)
- [ ] Create test fixture files
- [ ] Write 10 integration tests
- [ ] Test full compilation pipeline
- [ ] Test multiple error scenarios
- [ ] Test color output
- [ ] All tests passing

### ☐ Documentation
- [ ] Module documentation for `catena_error.erl`
- [ ] Module documentation for `catena_error_formatter.erl`
- [ ] Update parser documentation
- [ ] Create this summary document

---

## 10. Success Metrics

### Quantitative

- ✅ At least **65 tests** written and passing
- ✅ Can report **3-5 errors** in one compilation pass
- ✅ Error reporting adds **< 100ms** overhead
- ✅ **10+ common error patterns** with suggestions
- ✅ **100% test pass rate**

### Qualitative

- ✅ Error messages are **clear and actionable**
- ✅ Source context **visually highlights** the problem
- ✅ Suggestions provide **concrete fixes**
- ✅ Colors make errors **easy to spot**
- ✅ Developer experience is **significantly improved**

---

## 11. Timeline Estimate

**Total: 15-21 hours**

| Step | Task | Hours |
|------|------|-------|
| 1 | Error Data Structures | 2-3 |
| 2 | Source Context Extraction | 2-3 |
| 3 | ANSI Color Formatting | 3-4 |
| 4 | Parser Error Recovery | 4-5 |
| 5 | Helpful Suggestions | 2-3 |
| 6 | Integration Testing | 2-3 |

**Contingency**: Add 20% (3-4 hours) for unexpected issues

**Realistic Total**: 18-25 hours over 3-4 work sessions

---

## 12. Risk Assessment

### Low Risk
- Error data structures (well-understood)
- ANSI color codes (straightforward)
- Source context extraction (file I/O is standard)

### Medium Risk
- Parser error recovery (yecc integration complexity)
- Synchronization point selection (may need iteration)
- Suggestion pattern matching (requires tuning)

### Mitigation Strategies
- Start with simple error recovery, iterate
- Test synchronization extensively
- Use pattern matching for suggestions (easy to extend)
- Keep suggestion logic separate (can improve independently)

---

**Status**: ✅ Planning Complete - Ready to Implement

**Next Step**: Begin Step 1 - Error Data Structures
