# Task 1.1.1: Token Recognition - Feature Planning Document

## Metadata

**Task ID**: 1.1.1
**Task Name**: Token Recognition
**Phase**: Phase 1 - Core Language Infrastructure
**Section**: 1.1 - Lexer and Parser
**Estimated Duration**: 3-4 days
**Dependencies**: None (foundational task)
**Created**: 2025-11-08

---

## Problem Statement

The Catena compiler requires a lexical analyzer (lexer) to transform raw source code text into a stream of tokens that can be consumed by the parser. This is the first step in the compilation pipeline and must accurately recognize all lexical elements of the Catena language including:

- **Keywords**: Reserved words with special meaning (`shape`, `flow`, `match`, `where`, `let`, `in`, `do`, `end`, `actor`, `trait`, etc.)
- **Operators**: Symbolic operators for composition and computation (`|>`, `->`, `:`, `=`, `<>`, `>>=`, `<|>`, `!`, `?`)
- **Delimiters**: Structural punctuation (`{`, `}`, `[`, `]`, `(`, `)`, `|`, `,`, `;`)
- **Literals**: Numeric values (integers, floats, scientific notation) and strings (with escape sequences and multi-line support)
- **Identifiers**: Variable names, type names, module names
- **Comments**: Single-line (`--`) and multi-line (`{- -}`) with proper nesting support
- **Whitespace**: Spaces, tabs, newlines (typically ignored but tracked for source location)

The lexer must:
1. Accurately tokenize all valid Catena source code
2. Track source locations (line and column numbers) for error reporting
3. Handle edge cases (e.g., `->` vs `-` vs `--`, multi-line strings, nested comments)
4. Provide clear error messages for invalid tokens
5. Serve as a foundation for the parser (Task 1.1.2)

**Current State**: No implementation exists. This is the first code to be written for the Catena compiler.

**Target State**: A working lexer implemented using Erlang's leex tool that can tokenize all Catena syntax elements and produce structured token streams ready for parsing.

---

## Solution Overview

We will implement the Catena lexer using **leex**, Erlang's lexical analyzer generator (analogous to lex/flex). Leex uses regular expressions to define token patterns and generates an Erlang module that performs lexical analysis.

### High-Level Approach

1. **Create a leex definition file** (`catena_lexer.xrl`) with three sections:
   - **Definitions**: Character classes and reusable regex patterns
   - **Rules**: Token recognition patterns with associated Erlang code
   - **Erlang code**: Helper functions for token processing

2. **Define character classes** for common patterns:
   - Digits: `[0-9]`
   - Letters: `[a-zA-Z]`
   - Whitespace: `[\s\t\n\r]`
   - Identifier characters: `[a-zA-Z0-9_]`

3. **Implement token recognition rules** for each token category:
   - Keywords: Match specific strings and check against reserved word list
   - Operators: Match multi-character operators first, then single-character
   - Delimiters: Direct character matches
   - Numbers: Regex patterns for integers, floats, scientific notation
   - Strings: Handle quotes, escape sequences, multi-line support
   - Comments: Handle `--` (single-line) and `{- -}` (multi-line with nesting)
   - Identifiers: Match valid identifier patterns and distinguish from keywords

4. **Generate tokens** as Erlang tuples with structure:
   - `{TokenType, LineNumber, TokenValue}` for most tokens
   - `{TokenType, LineNumber}` for fixed tokens (keywords, operators)

5. **Track source locations** using leex's built-in `TokenLine` variable and potentially extending with column tracking

6. **Compile and test** the lexer with comprehensive test cases

### Technology Choice: Leex

**Why Leex?**
- Native Erlang tool, integrates seamlessly with the BEAM ecosystem
- Mature and battle-tested (used for Erlang's own lexer)
- Works naturally with yecc (which we'll use for parsing in Task 1.1.2)
- Regular expression based, making token patterns clear and maintainable
- Generates efficient Erlang code

**Leex File Structure:**
```
Definitions.
[Character class definitions]

Rules.
[Token pattern] : [Erlang code to generate token].

Erlang code.
[Helper functions]
```

---

## Agent Consultations Performed

### Consultation 1: Erlang Leex Documentation Research

**Query**: Searched for "Erlang leex lexical analyzer generator tutorial examples syntax"

**Key Findings**:
- Leex definition files use `.xrl` extension
- Three mandatory sections: Definitions, Rules, Erlang code
- Built-in variables available in rules: `TokenChars`, `TokenLen`, `TokenLine`
- Regular expressions must start at beginning of line with no leading whitespace
- Use `\t` and `\s` for TAB and SPACE in regex patterns
- Compilation: `leex:file("filename.xrl")` generates `.erl` file

### Consultation 2: Leex Example Analysis

**Source**: `erlang_scan.xrl` from rvirding/leex repository

**Key Patterns Identified**:

1. **Character Classes**:
   ```
   O = [0-7]              % Octal digits
   D = [0-9]              % Decimal digits
   H = [0-9a-fA-F]        % Hex digits
   U = [A-Z]              % Uppercase
   L = [a-z]              % Lowercase
   ```

2. **Number Patterns**:
   - Floats: `{D}+\.{D}+((E|e)(\+|-)?{D}+)?`
   - Based integers: `{D}+#{H}+`
   - Decimals: `{D}+`

3. **String Patterns**:
   - Quoted atoms: `'(\\^.|\.|[^'])*'`
   - Strings: `"(\\^.|\.|[^"])*"`

4. **Helper Functions**:
   - Reserved word checking via pattern matching
   - Base conversion for number literals
   - Escape sequence processing

---

## Technical Details

### File Locations

```
catena/
├── src/
│   ├── compiler/
│   │   ├── lexer/
│   │   │   ├── catena_lexer.xrl       # Leex definition file
│   │   │   └── catena_lexer.erl       # Generated lexer module
│   │   └── tokens.hrl                 # Token type definitions (header file)
│   └── ...
└── test/
    └── compiler/
        └── lexer/
            ├── catena_lexer_test.erl   # EUnit tests for lexer
            └── fixtures/               # Test input files
                ├── keywords.catena
                ├── operators.catena
                ├── numbers.catena
                ├── strings.catena
                └── comments.catena
```

### Token Type Definitions

All tokens will be defined as tuples. We'll create a header file (`tokens.hrl`) for documentation:

```erlang
%% Keyword tokens
%% {shape, LineNumber}
%% {flow, LineNumber}
%% {match, LineNumber}
%% {where, LineNumber}
%% {let, LineNumber}
%% {in, LineNumber}
%% {do, LineNumber}
%% {end, LineNumber}
%% {actor, LineNumber}
%% {trait, LineNumber}
%% {effect, LineNumber}
%% {category, LineNumber}
%% {export, LineNumber}
%% {import, LineNumber}
%% {supervisor, LineNumber}
%% {schema, LineNumber}
%% {law, LineNumber}
%% {convert, LineNumber}
%% {spawn, LineNumber}
%% {implements, LineNumber}
%% {derives, LineNumber}
%% {when, LineNumber}
%% {try, LineNumber}
%% {catch, LineNumber}

%% Operator tokens
%% {pipe, LineNumber}              % |>
%% {arrow, LineNumber}             % ->
%% {colon, LineNumber}             % :
%% {equals, LineNumber}            % =
%% {append, LineNumber}            % <>
%% {bind, LineNumber}              % >>=
%% {parallel, LineNumber}          % <|>
%% {send, LineNumber}              % !
%% {receive, LineNumber}           % ?
%% {plus, LineNumber}              % +
%% {minus, LineNumber}             % -
%% {multiply, LineNumber}          % *
%% {divide, LineNumber}            % /
%% {lt, LineNumber}                % <
%% {gt, LineNumber}                % >
%% {lte, LineNumber}               % <=
%% {gte, LineNumber}               % >=
%% {eq, LineNumber}                % ==
%% {neq, LineNumber}               % /=
%% {dot, LineNumber}               % .

%% Delimiter tokens
%% {'{', LineNumber}
%% {'}', LineNumber}
%% {'[', LineNumber}
%% {']', LineNumber}
%% {'(', LineNumber}
%% {')', LineNumber}
%% {'|', LineNumber}
%% {',', LineNumber}
%% {';', LineNumber}

%% Literal tokens
%% {integer, LineNumber, Value}
%% {float, LineNumber, Value}
%% {string, LineNumber, Value}
%% {atom, LineNumber, Value}

%% Identifier tokens
%% {identifier, LineNumber, Name}      % lowercase start (variables, flows)
%% {type_identifier, LineNumber, Name} % uppercase start (types, shapes)

%% Special tokens
%% {eof, LineNumber}
```

### Leex Definition File Structure

**File**: `src/compiler/lexer/catena_lexer.xrl`

```erlang
%% Catena Lexer - Lexical analyzer for Catena language
%% Generated using Erlang leex

Definitions.

%% Character classes
D   = [0-9]
L   = [a-z]
U   = [A-Z]
H   = [0-9a-fA-F]
ALPHA = [a-zA-Z]
ALNUM = [a-zA-Z0-9_]
WS    = [\s\t\r]

%% Number patterns
INT     = {D}+
FLOAT   = {D}+\.{D}+
SCI     = {D}+\.{D}+((E|e)(\+|-)?{D}+)?

%% Identifier patterns
LOWER_ID = {L}{ALNUM}*
UPPER_ID = {U}{ALNUM}*

%% String pattern (basic - will handle escapes in Erlang code)
STRING = "([^"\\]|\\.)*"

Rules.

%% Whitespace (ignored but tracked for line counting)
{WS}+         : skip_token.
\n            : {token, {newline, TokenLine}}.

%% Comments
--.*          : skip_token.
\{-           : {token, {comment_start, TokenLine}}.
-\}           : {token, {comment_end, TokenLine}}.

%% Multi-character operators (MUST come before single-character)
\|>           : {token, {pipe, TokenLine}}.
->            : {token, {arrow, TokenLine}}.
>>=           : {token, {bind, TokenLine}}.
<>            : {token, {append, TokenLine}}.
<\|>          : {token, {parallel, TokenLine}}.
<=            : {token, {lte, TokenLine}}.
>=            : {token, {gte, TokenLine}}.
==            : {token, {eq, TokenLine}}.
/=            : {token, {neq, TokenLine}}.

%% Single-character operators
:             : {token, {colon, TokenLine}}.
=             : {token, {equals, TokenLine}}.
!             : {token, {send, TokenLine}}.
\?            : {token, {receive, TokenLine}}.
\+            : {token, {plus, TokenLine}}.
-             : {token, {minus, TokenLine}}.
\*            : {token, {multiply, TokenLine}}.
/             : {token, {divide, TokenLine}}.
<             : {token, {lt, TokenLine}}.
>             : {token, {gt, TokenLine}}.
\.            : {token, {dot, TokenLine}}.

%% Delimiters
\{            : {token, {'{', TokenLine}}.
\}            : {token, {'}', TokenLine}}.
\[            : {token, {'[', TokenLine}}.
\]            : {token, {']', TokenLine}}.
\(            : {token, {'(', TokenLine}}.
\)            : {token, {')', TokenLine}}.
\|            : {token, {'|', TokenLine}}.
,             : {token, {',', TokenLine}}.
;             : {token, {';', TokenLine}}.

%% Numbers
{SCI}         : {token, {float, TokenLine, list_to_float(TokenChars)}}.
{FLOAT}       : {token, {float, TokenLine, list_to_float(TokenChars)}}.
{INT}         : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.

%% Strings
{STRING}      : {token, {string, TokenLine, process_string(TokenChars, TokenLine)}}.

%% Identifiers and Keywords
{LOWER_ID}    : check_keyword(TokenChars, TokenLine).
{UPPER_ID}    : {token, {type_identifier, TokenLine, list_to_atom(TokenChars)}}.

Erlang code.

%% Reserved keywords
-define(KEYWORDS, [
    shape, flow, match, where, let, in, do, end,
    actor, trait, effect, category, export, import,
    supervisor, schema, law, convert, spawn,
    implements, derives, when, try, catch, return
]).

%% Check if identifier is a keyword
check_keyword(Chars, Line) ->
    Atom = list_to_atom(Chars),
    case lists:member(Atom, ?KEYWORDS) of
        true  -> {token, {Atom, Line}};
        false -> {token, {identifier, Line, Atom}}
    end.

%% Process string literals (remove quotes and handle escapes)
process_string(Chars, Line) ->
    %% Remove leading and trailing quotes
    Stripped = lists:sublist(Chars, 2, length(Chars) - 2),
    %% Process escape sequences
    process_escapes(Stripped, Line).

%% Process escape sequences in strings
process_escapes(String, _Line) ->
    %% TODO: Handle \n, \t, \r, \\, \", \', \xHH, \uHHHH
    %% For now, return as-is (will be implemented in subtask 1.1.1.3)
    list_to_binary(String).
```

### Implementation Steps Breakdown

#### Subtask 1.1.1.1: Keywords, Operators, and Delimiters

**Objective**: Define token types and lexical rules for all Catena keywords, operators, and delimiters.

**Implementation**:

1. **Create project structure**:
   ```bash
   mkdir -p src/compiler/lexer
   mkdir -p test/compiler/lexer/fixtures
   ```

2. **Create token header file** (`src/compiler/tokens.hrl`):
   - Document all token types
   - Include examples for clarity
   - Use consistent naming conventions

3. **Define character classes** in leex file:
   - Basic classes: digits, letters, alphanumeric
   - Whitespace: space, tab, carriage return, newline
   - Special characters for operators

4. **Implement keyword recognition**:
   - Create `?KEYWORDS` macro with complete list:
     ```erlang
     -define(KEYWORDS, [
         shape, flow, match, where, let, in, do, end,
         actor, trait, effect, category, export, import,
         supervisor, schema, law, convert, spawn,
         implements, derives, when, try, catch, return,
         if, then, else, case, of
     ]).
     ```
   - Implement `check_keyword/2` function
   - Test with all keywords

5. **Implement operator recognition**:
   - Order matters: multi-character operators BEFORE single-character
   - Multi-char: `|>`, `->`, `>>=`, `<>`, `<|>`, `<=`, `>=`, `==`, `/=`
   - Single-char: `:`, `=`, `!`, `?`, `+`, `-`, `*`, `/`, `<`, `>`, `.`
   - Escape regex special characters: `\?`, `\.`, `\*`, `\+`, `\|`

6. **Implement delimiter recognition**:
   - All delimiters: `{`, `}`, `[`, `]`, `(`, `)`, `|`, `,`, `;`
   - Escape braces and brackets in regex

**Test Cases**:
- Tokenize each keyword individually
- Tokenize all operators including edge cases (`->` vs `-` vs `--`)
- Tokenize delimiter sequences: `{[()]}`, nested structures
- Verify token types and line numbers are correct

#### Subtask 1.1.1.2: Number Literal Recognition

**Objective**: Implement recognition for integers, floats, and scientific notation.

**Number Formats**:
- **Integers**: `0`, `42`, `1000000`
- **Floats**: `3.14`, `0.001`, `99.99`
- **Scientific notation**: `1.23e10`, `6.022e-23`, `3.0E+8`
- **Edge cases**: `.5` (not valid - require digit before decimal), `1.` (not valid - require digit after decimal)

**Implementation**:

1. **Define number patterns** in Definitions section:
   ```erlang
   D     = [0-9]
   INT   = {D}+
   FLOAT = {D}+\.{D}+
   EXP   = (E|e)(\+|-)?{D}+
   SCI   = {D}+\.{D}+{EXP}
   ```

2. **Add number rules** (order matters - most specific first):
   ```erlang
   {SCI}   : {token, {float, TokenLine, sci_to_float(TokenChars)}}.
   {FLOAT} : {token, {float, TokenLine, list_to_float(TokenChars)}}.
   {INT}   : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
   ```

3. **Implement `sci_to_float/1` helper**:
   ```erlang
   sci_to_float(Chars) ->
       %% Erlang's list_to_float handles scientific notation
       list_to_float(Chars).
   ```

4. **Handle edge cases**:
   - Negative numbers: `-42` is tokenized as `minus` + `integer(42)`
   - Leading zeros: `007` is valid integer (value 7)
   - Very large numbers: may need to use bigint representation

**Test Cases**:
- Integers: `0`, `1`, `42`, `999999999`
- Floats: `0.0`, `3.14159`, `0.001`
- Scientific: `1e10`, `2.5e-3`, `6.022E23`, `1.0e+5`
- Edge cases: `0.0`, `1.0`, negative numbers
- Invalid: `.5`, `1.`, `1e`, `1.2.3` (should error)

#### Subtask 1.1.1.3: String Literal Recognition

**Objective**: Implement string recognition with escape sequences and multi-line support.

**String Features**:
- **Basic strings**: `"hello world"`
- **Escape sequences**: `\n`, `\t`, `\r`, `\\`, `\"`, `\'`
- **Unicode escapes**: `\xHH` (hex), `\uHHHH` (unicode)
- **Multi-line strings**:
  ```catena
  "This is a
   multi-line
   string"
  ```
- **Raw strings** (future): `r"no \n escape"` (not in this task)

**Implementation**:

1. **Basic string pattern** in Definitions:
   ```erlang
   %% Matches strings with escape sequences
   STRING = "([^"\\]|\\(.|\n))*"
   ```

2. **String rule**:
   ```erlang
   {STRING} : {token, {string, TokenLine, process_string(TokenChars, TokenLine)}}.
   ```

3. **Implement `process_string/2`**:
   ```erlang
   process_string(Chars, Line) ->
       %% Remove quotes
       Content = lists:sublist(Chars, 2, length(Chars) - 2),
       %% Process escapes
       Processed = process_escapes(Content, Line),
       %% Return as binary
       list_to_binary(Processed).
   ```

4. **Implement `process_escapes/2`**:
   ```erlang
   process_escapes([], _Line) ->
       [];
   process_escapes([$\\, $n | Rest], Line) ->
       [$\n | process_escapes(Rest, Line)];
   process_escapes([$\\, $t | Rest], Line) ->
       [$\t | process_escapes(Rest, Line)];
   process_escapes([$\\, $r | Rest], Line) ->
       [$\r | process_escapes(Rest, Line)];
   process_escapes([$\\, $\\ | Rest], Line) ->
       [$\\ | process_escapes(Rest, Line)];
   process_escapes([$\\, $\" | Rest], Line) ->
       [$\" | process_escapes(Rest, Line)];
   process_escapes([$\\, $\' | Rest], Line) ->
       [$\' | process_escapes(Rest, Line)];
   process_escapes([$\\, $x, H1, H2 | Rest], Line) ->
       %% Hex escape: \xHH
       Value = erlang:list_to_integer([H1, H2], 16),
       [Value | process_escapes(Rest, Line)];
   process_escapes([$\\, $u, H1, H2, H3, H4 | Rest], Line) ->
       %% Unicode escape: \uHHHH
       Value = erlang:list_to_integer([H1, H2, H3, H4], 16),
       [Value | process_escapes(Rest, Line)];
   process_escapes([$\\, C | _Rest], Line) ->
       %% Invalid escape sequence
       throw({error, {Line, "Invalid escape sequence: \\" ++ [C]}});
   process_escapes([C | Rest], Line) ->
       [C | process_escapes(Rest, Line)].
   ```

5. **Handle multi-line strings**:
   - Update STRING pattern to allow newlines
   - Track line numbers correctly (increment on `\n`)
   - Preserve or normalize indentation (design decision needed)

**Test Cases**:
- Basic strings: `"hello"`, `"world"`, `""`
- With escapes: `"hello\nworld"`, `"tab\there"`, `"quote\"inside"`
- Hex escapes: `"\x41"` (should be "A")
- Unicode escapes: `"\u0041"` (should be "A"), `"\u03BB"` (lambda λ)
- Multi-line: strings spanning multiple lines
- Invalid: unterminated strings, invalid escapes

#### Subtask 1.1.1.4: Comment Recognition

**Objective**: Implement single-line and multi-line comments with proper nesting.

**Comment Types**:
- **Single-line**: `-- This is a comment` (extends to end of line)
- **Multi-line**: `{- This is a multi-line comment -}`
- **Nested multi-line**:
  ```catena
  {- Outer comment
     {- Inner comment -}
     Still in outer comment
  -}
  ```

**Implementation**:

1. **Single-line comments** (simple):
   ```erlang
   Rules.
   --.*\n : skip_token.
   ```
   - Matches `--` followed by any characters until newline
   - `skip_token` means no token is generated

2. **Multi-line comments with nesting** (complex):

   **Challenge**: Leex doesn't support stateful parsing directly, so we need a different approach.

   **Solution**: Use Erlang code to handle nested comments manually.

   ```erlang
   Rules.
   \{- : begin_comment(TokenChars, TokenLine).

   Erlang code.
   begin_comment(_Chars, Line) ->
       {skip_token, {comment_state, 1, Line}}.
   ```

   However, leex doesn't support stateful lexing well. **Better approach**:

   **Use a start condition** (leex supports this):

   ```erlang
   Definitions.
   %% Define start condition for comments

   Rules.
   <INITIAL>\{-        : {begin_comment, TokenLine}.
   <COMMENT>\{-        : {nest_comment, TokenLine}.
   <COMMENT>-\}        : {end_comment, TokenLine}.
   <COMMENT>(.|\n)     : skip_token.
   <COMMENT><<EOF>>    : {error, "Unterminated comment"}.
   ```

   **Actually**, leex has limited start condition support. **Simpler approach**:

   Since this is complex, we'll use a **two-phase approach**:
   - Phase 1: Strip comments as preprocessing step
   - Phase 2: Tokenize cleaned source

   OR

   **Implement comment handling in Erlang code**:

   ```erlang
   Rules.
   \{- : skip_nested_comment(TokenChars, TokenLine, TokenLen).

   Erlang code.
   skip_nested_comment(Chars, Line, Len) ->
       %% Read ahead to find matching -}
       %% Handle nesting by counting {- and -}
       %% Return skip_token when complete
       {skip_token, Length}.
   ```

   **Final decision**: Implement `scan_comment/2` helper that:
   - Takes input stream position
   - Scans forward counting nesting level
   - Returns length of comment
   - Leex rule returns `skip_token`

3. **Implementation sketch**:
   ```erlang
   %% In Erlang code section
   -export([scan_comment/2]).

   scan_comment(Input, StartPos) ->
       scan_comment(Input, StartPos, 1, StartPos).

   scan_comment(Input, Pos, 0, StartPos) ->
       %% Nesting level reached 0, comment complete
       Pos - StartPos;
   scan_comment(Input, Pos, Level, StartPos) when Pos < length(Input) ->
       case {lists:nth(Pos, Input), lists:nth(Pos+1, Input)} of
           {${, $-} ->
               scan_comment(Input, Pos+2, Level+1, StartPos);
           {$-, $}} ->
               scan_comment(Input, Pos+2, Level-1, StartPos);
           _ ->
               scan_comment(Input, Pos+1, Level, StartPos)
       end;
   scan_comment(_Input, _Pos, _Level, _StartPos) ->
       {error, unterminated_comment}.
   ```

**Test Cases**:
- Single-line: `-- comment`, `-- comment\n`, multiple consecutive
- Multi-line: `{- comment -}`, `{- multi\nline -}`
- Nested: `{- outer {- inner -} outer -}`
- Multiple nesting levels: `{- a {- b {- c -} b -} a -}`
- Unterminated: `{- no end` (should error)
- Edge cases: `{--}`, `-{-`, `--{-`

### Compilation and Testing

**Compilation Process**:

1. **Generate lexer from leex file**:
   ```erlang
   leex:file("src/compiler/lexer/catena_lexer.xrl").
   ```
   This produces `src/compiler/lexer/catena_lexer.erl`.

2. **Compile generated Erlang module**:
   ```erlang
   c("src/compiler/lexer/catena_lexer.erl").
   ```

3. **Use the lexer**:
   ```erlang
   {ok, Tokens, _} = catena_lexer:string("shape Maybe a = Some a | None").
   ```

**Testing Framework**: EUnit

**Test Structure**:
```erlang
-module(catena_lexer_test).
-include_lib("eunit/include/eunit.hrl").

%% Test keywords
keywords_test() ->
    {ok, Tokens, _} = catena_lexer:string("shape flow match"),
    ?assertEqual([
        {shape, 1},
        {flow, 1},
        {match, 1}
    ], Tokens).

%% Test operators
operators_test() ->
    {ok, Tokens, _} = catena_lexer:string("|> -> >>="),
    ?assertEqual([
        {pipe, 1},
        {arrow, 1},
        {bind, 1}
    ], Tokens).

%% Test numbers
numbers_test() ->
    {ok, Tokens, _} = catena_lexer:string("42 3.14 1.5e-10"),
    ?assertEqual([
        {integer, 1, 42},
        {float, 1, 3.14},
        {float, 1, 1.5e-10}
    ], Tokens).

%% Test strings
strings_test() ->
    {ok, Tokens, _} = catena_lexer:string("\"hello\" \"world\\n\""),
    ?assertEqual([
        {string, 1, <<"hello">>},
        {string, 1, <<"world\n">>}
    ], Tokens).

%% Test comments
comments_test() ->
    {ok, Tokens, _} = catena_lexer:string("flow -- comment\nshape"),
    ?assertEqual([
        {flow, 1},
        {shape, 2}
    ], Tokens).

%% Test complex example
complex_example_test() ->
    Source = "shape Maybe a = Some a | None",
    {ok, Tokens, _} = catena_lexer:string(Source),
    Expected = [
        {shape, 1},
        {type_identifier, 1, 'Maybe'},
        {identifier, 1, a},
        {equals, 1},
        {type_identifier, 1, 'Some'},
        {identifier, 1, a},
        {'|', 1},
        {type_identifier, 1, 'None'}
    ],
    ?assertEqual(Expected, Tokens).
```

---

## Success Criteria

This task is complete when:

1. **All token types recognized**: The lexer correctly identifies all keywords, operators, delimiters, literals, identifiers, and comments defined in Catena syntax.

2. **Accurate tokenization**: Token types and values match expected results for all test cases.

3. **Source location tracking**: Each token includes accurate line number (column tracking is optional for this task).

4. **Edge cases handled**:
   - Multi-character operators don't conflict with single-character (`->` vs `-`)
   - Comments are properly stripped (including nested multi-line comments)
   - Escape sequences in strings are correctly processed
   - Scientific notation numbers are parsed correctly

5. **Error handling**: Invalid tokens produce clear error messages (e.g., unterminated strings, invalid escape sequences).

6. **Test coverage**: Comprehensive EUnit test suite with:
   - Unit tests for each token category (keywords, operators, numbers, strings, comments)
   - Edge case tests
   - Integration tests with realistic Catena code snippets
   - Minimum 90% code coverage

7. **Documentation**:
   - `tokens.hrl` header file documents all token types
   - Inline comments in `catena_lexer.xrl` explain regex patterns
   - README in `src/compiler/lexer/` explains usage

8. **Integration ready**: Generated token stream is in the format expected by the parser (Task 1.1.2).

---

## Implementation Plan

### Step 1: Project Setup (0.5 days)

**Tasks**:
- Create directory structure: `src/compiler/lexer/`, `test/compiler/lexer/`
- Create `tokens.hrl` header file with all token type definitions
- Create initial `catena_lexer.xrl` file with basic structure
- Set up build configuration for leex in rebar3 or makefile
- Create test fixtures directory with sample `.catena` files

**Deliverables**:
- Empty but structured project
- Build script that compiles leex file
- Initial test file with `?assert(true)` placeholder

### Step 2: Implement Subtask 1.1.1.1 - Keywords, Operators, Delimiters (1 day)

**Tasks**:
1. Define character classes in Definitions section
2. Add rules for all operators (multi-char first, then single-char)
3. Add rules for all delimiters
4. Implement `check_keyword/2` function with `?KEYWORDS` macro
5. Add identifier recognition rules
6. Write unit tests for each category
7. Test with complex examples combining all three

**Deliverables**:
- Working lexer for keywords, operators, delimiters, identifiers
- 15+ unit tests covering all cases
- Test fixture: `fixtures/keywords_operators.catena`

### Step 3: Implement Subtask 1.1.1.2 - Number Literals (0.5 days)

**Tasks**:
1. Define number patterns in Definitions section
2. Add rules for scientific notation, floats, integers (in that order)
3. Implement `sci_to_float/1` if needed (may not be needed)
4. Write unit tests for each number format
5. Test edge cases (very large numbers, very small floats)

**Deliverables**:
- Number recognition working for all formats
- 10+ unit tests
- Test fixture: `fixtures/numbers.catena`

### Step 4: Implement Subtask 1.1.1.3 - String Literals (1 day)

**Tasks**:
1. Define string pattern allowing multi-line and escapes
2. Implement `process_string/2` function
3. Implement `process_escapes/2` function with all escape sequences
4. Handle hex escapes (`\xHH`)
5. Handle unicode escapes (`\uHHHH`)
6. Write unit tests for each escape type
7. Test multi-line strings

**Deliverables**:
- String recognition with full escape support
- 15+ unit tests
- Test fixture: `fixtures/strings.catena`

### Step 5: Implement Subtask 1.1.1.4 - Comments (1 day)

**Tasks**:
1. Add single-line comment rule (simple)
2. Design approach for nested multi-line comments
3. Implement nested comment handling (using Erlang code helper)
4. Write unit tests for single-line comments
5. Write unit tests for multi-line comments
6. Write unit tests for nested comments (multiple levels)
7. Test unterminated comment error handling

**Deliverables**:
- Comment recognition with nesting support
- 10+ unit tests
- Test fixture: `fixtures/comments.catena`

### Step 6: Integration Testing and Documentation (0.5 days)

**Tasks**:
1. Create comprehensive integration test with realistic Catena code:
   ```catena
   -- Example: Maybe type with map function
   shape Maybe a = Some a | None

   flow map : (a -> b) -> Maybe a -> Maybe b
   flow map f = match
     | Some x -> Some (f x)
     | None   -> None
   end
   ```
2. Test lexer on all research document code examples
3. Write README for lexer module
4. Add usage examples to documentation
5. Verify all error messages are clear

**Deliverables**:
- 5+ integration tests with realistic code
- README.md in `src/compiler/lexer/`
- All documentation complete

### Step 7: Review and Refinement (0.5 days)

**Tasks**:
1. Run all tests and achieve 90%+ coverage
2. Review generated token stream format
3. Ensure compatibility with parser expectations (coordinate with Task 1.1.2)
4. Optimize regex patterns if needed
5. Add any missing edge case tests
6. Code review and cleanup

**Deliverables**:
- All tests passing
- Clean, well-documented code
- Ready for parser integration

---

## Notes and Considerations

### Design Decisions

1. **Token format**: Using tuples `{Type, Line}` or `{Type, Line, Value}` for compatibility with yecc parser generator.

2. **Identifier casing**:
   - Lowercase identifiers: `identifier` (variables, functions)
   - Uppercase identifiers: `type_identifier` (types, constructors)
   - This matches Haskell/OCaml conventions

3. **Comment nesting**: Catena supports nested multi-line comments (like Haskell, unlike C). This requires careful implementation.

4. **String encoding**: Using binaries (`<<>>`) for string values for efficiency and consistency with Erlang conventions.

5. **Operator precedence**: Not handled in lexer - that's the parser's job. Lexer just recognizes operators.

### Potential Challenges

1. **Nested comments**: Leex doesn't natively support stateful lexing. Solution: implement nesting logic in Erlang code section.

2. **Multi-line strings**: Need to track line numbers correctly when string spans multiple lines.

3. **Escape sequences**: Must handle both standard escapes and unicode. Need robust error handling for invalid escapes.

4. **Operator conflicts**: `->` vs `-` vs `--`. Solution: order rules correctly (longest match first).

5. **Leex limitations**: Leex is less powerful than modern parser combinators. May need to do some preprocessing or post-processing in Erlang code.

### Future Enhancements (Not in This Task)

1. **Column tracking**: Currently tracking line numbers only. Column numbers would improve error messages.

2. **Raw strings**: Strings without escape processing (`r"..."`).

3. **String interpolation**: `"Hello, ${name}"` (complex, may require parser integration).

4. **Operator overloading metadata**: Tracking fixity and precedence in tokens.

5. **Documentation comments**: Special handling for `{-| ... -}` doc comments.

6. **Lexer directives**: `{-# LANGUAGE ... #-}` style pragmas.

### Dependencies

**This task depends on**:
- Erlang/OTP installation with leex
- Rebar3 or build system setup

**Tasks that depend on this**:
- Task 1.1.2: Grammar Implementation (parser needs token stream from lexer)
- Task 1.1.3: AST Construction (uses token metadata)
- Task 1.1.4: Error Recovery (uses token source locations)

### Testing Strategy

**Test Levels**:
1. **Unit tests**: Each token type in isolation
2. **Integration tests**: Realistic code snippets
3. **Edge case tests**: Boundary conditions, errors
4. **Regression tests**: Once bugs are found, add test to prevent recurrence

**Test Data**:
- Use fixtures for larger test cases
- Include examples from research documents
- Test with intentionally malformed input

**Coverage Goals**:
- 90%+ line coverage
- All regex patterns exercised
- All helper functions tested
- All error paths tested

---

## References

### Catena Language Specification
- Research document 1.01: Original Idea - Core syntax examples
- Research document 1.02: Modules - Module syntax
- Research document 1.10: Pattern Matching - Pattern syntax
- CLAUDE.md: Syntax examples and conventions

### Erlang/Leex Documentation
- [Leex Official Documentation](https://www.erlang.org/doc/apps/parsetools/leex.html)
- [Leex GitHub Repository](https://github.com/rvirding/leex)
- [Leex Example: erlang_scan.xrl](https://github.com/rvirding/leex/blob/master/examples/erlang_scan.xrl)
- [Tutorial: Tokenizing and Parsing with Leex and Yecc](https://andrealeopardi.com/posts/tokenizing-and-parsing-in-elixir-using-leex-and-yecc/)

### Related Phase 1 Tasks
- Task 1.1.2: Grammar Implementation (next task)
- Task 1.1.3: AST Construction
- Task 1.1.4: Error Recovery and Reporting

---

## Appendix: Complete Token List

### Keywords (26 total)
```
shape, flow, match, where, let, in, do, end,
actor, trait, effect, category, export, import,
supervisor, schema, law, convert, spawn,
implements, derives, when, try, catch, return,
if, then, else, case, of
```

### Operators (21 total)
```
|> (pipe)
-> (arrow)
>>=  (bind/Kleisli)
<> (append)
<|> (parallel)
: (colon/type annotation)
= (equals/assignment)
! (send message)
? (receive message)
+ (plus)
- (minus)
* (multiply)
/ (divide)
< (less than)
> (greater than)
<= (less than or equal)
>= (greater than or equal)
== (equal)
/= (not equal)
. (dot/composition)
```

### Delimiters (9 total)
```
{ } (braces)
[ ] (brackets)
( ) (parentheses)
| (pipe/bar)
, (comma)
; (semicolon)
```

### Literals
```
integer: 42, 0, 999999
float: 3.14, 0.001
scientific: 1.5e-10, 6.022e23
string: "hello world", "multi\nline"
```

### Identifiers
```
identifier: lowercase_start (variables, flows)
type_identifier: Uppercase_start (types, constructors)
```

### Comments
```
-- single line
{- multi
   line -}
{- nested {- comments -} -}
```

---

## Revision History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2025-11-08 | Planning Agent | Initial document created |

---

**END OF DOCUMENT**
