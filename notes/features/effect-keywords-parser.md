# Effect Keywords Parser Integration - Feature Planning Document

## Metadata

**Feature ID**: Effect-Keywords-Parser
**Related Task**: 1.1.5 - Effect Syntax Support (Parser Implementation)
**Phase**: Phase 1 - Core Language Infrastructure
**Section**: 1.1 - Lexer and Parser
**Estimated Duration**: 3-4 days
**Dependencies**:
- Task 1.1.1 (Token Recognition) - COMPLETED
- Task 1.1.2 (Grammar Implementation) - COMPLETED (base grammar)
- Effect Keywords Lexer Integration - COMPLETED
**Created**: 2025-11-09

---

## Problem Statement

The Catena language now includes algebraic effects as a core feature integrated throughout Phase 1. The lexer (Task 1.1.1) has been updated to recognize 5 effect keywords (`effect`, `operation`, `perform`, `try`, `with`). However, the parser (Task 1.1.2) cannot yet parse effect-related syntax because the necessary grammar rules are missing from `catena_parser.yrl`.

### Missing Grammar Rules

The parser currently lacks production rules for:

1. **Effect Declarations** - Defining algebraic effects with operations
   ```catena
   effect FileIO {
     operation read(path: String): String
     operation write(path: String, content: String): Unit
   }
   ```

2. **Perform Expressions** - Invoking effectful operations
   ```catena
   perform FileIO.read("/etc/config")
   ```

3. **Try-With Handlers** - Handling effects with pattern matching
   ```catena
   try {
     content = perform FileIO.read(path)
     perform Console.print(content)
   } with FileIO {
     read(p) -> "mock content"
     write(p, c) -> ()
   }
   ```

4. **Effect Annotations** - Specifying effect sets in type signatures
   ```catena
   flow processFile : String -> String / {FileIO, Console}
   ```

### Impact on Compilation Pipeline

Without these grammar rules:
- **Task 1.1.5** (Effect Syntax Support) cannot be completed
- **Section 1.2** (Type System) - Type-and-effect inference blocked (needs effect AST nodes)
- **Section 1.3** (Code Generation) - Cannot generate Core Erlang for effect handlers
- **Section 1.4** (Integration Tests) - Cannot test effectful programs end-to-end

### Current Parser State

The parser (`catena_parser.yrl`) successfully handles:
- Shape declarations (algebraic data types)
- Flow declarations (functions with pattern matching)
- Expression parsing (literals, variables, function application, operators)
- Pattern matching with guards
- Type expressions (including forall quantification)

**What's Missing**: Grammar rules and AST node types for the four effect constructs listed above.

**Target State**: The parser can parse all effect-related syntax specified in Phase 1, generating appropriate AST nodes for subsequent compiler phases.

---

## Solution Overview

We will extend the existing `catena_parser.yrl` yecc grammar and `catena_ast.hrl` AST definitions by adding grammar rules for effect syntax. This follows the established pattern of parser extension and integrates smoothly with existing infrastructure.

### High-Level Approach

1. **Extend AST definitions** (`catena_ast.hrl`) - Add 5 new record types for effect nodes
2. **Add terminals** to parser - Register effect keywords as terminal symbols
3. **Add nonterminals** to parser - Define new syntactic categories for effect constructs
4. **Define grammar rules** - Implement production rules for effect syntax
5. **Implement AST construction** - Build structured trees from effect parses
6. **Extend helper functions** - Update `extract_location/1` for new node types
7. **Comprehensive testing** - Unit and integration tests for all effect syntax

### Why This Approach

- **Consistency**: Follows established patterns in `catena_parser.yrl`
- **Modularity**: Effect syntax is orthogonal to existing constructs
- **No Breaking Changes**: Existing grammar rules remain unchanged
- **Phase 1 Alignment**: Directly implements Task 1.1.5 requirements
- **Extensibility**: Enables future effect system enhancements (Phase 6)

---

## Technical Details

### Architecture Overview

```
Effect Source Code
       ↓
  Lexer (catena_lexer.xrl)
       ↓ [effect tokens]
  Parser (catena_parser.yrl) ← WE ARE HERE
       ↓ [effect AST nodes]
  Type Checker (Section 1.2)
       ↓ [type-and-effect annotations]
  Code Generator (Section 1.3)
       ↓ [Core Erlang with process-based handlers]
  BEAM Bytecode
```

### File Locations

```
catena/
├── src/
│   ├── compiler/
│   │   ├── parser/
│   │   │   ├── catena_ast.hrl         # ADD: 5 new record types
│   │   │   ├── catena_parser.yrl      # MODIFY: Add grammar rules
│   │   │   └── catena_parser.erl      # AUTO-GENERATED: Will be regenerated
├── test/
│   └── compiler/
│       └── parser/
│           ├── catena_parser_simple_tests.erl       # Existing test patterns
│           └── catena_parser_effect_tests.erl       # ADD: New test module
```

### AST Node Definitions

#### 1. Effect Declaration Node

Represents an effect declaration with operations.

```erlang
%% Effect declaration (algebraic effect with operations)
-record(effect_decl, {
    name :: atom(),                           % Effect name (e.g., FileIO)
    operations :: [#effect_operation{}],      % List of operations
    location :: location()
}).

-record(effect_operation, {
    name :: atom(),                           % Operation name (e.g., read)
    params :: [{atom(), type_expr()}],        % Parameter list with types
    return_type :: type_expr(),               % Return type
    location :: location()
}).
```

**Example AST**:
```erlang
% effect FileIO { operation read(path: String): String }
{effect_decl,
  'FileIO',
  [
    {effect_operation,
      read,
      [{'path', {type_con, 'String', {location, 1, 20}}}],
      {type_con, 'String', {location, 1, 35}},
      {location, 1, 13}}
  ],
  {location, 1, 0}}
```

#### 2. Perform Expression Node

Represents invoking an effectful operation.

```erlang
%% Perform expression (effectful operation invocation)
-record(perform_expr, {
    effect :: atom(),                         % Effect name (e.g., FileIO)
    operation :: atom(),                      % Operation name (e.g., read)
    args :: [expr()],                         % Arguments
    location :: location()
}).
```

**Example AST**:
```erlang
% perform FileIO.read(path)
{perform_expr,
  'FileIO',
  read,
  [{var, path, {location, 1, 21}}],
  {location, 1, 0}}
```

#### 3. Try-With Expression Node

Represents an effect handler block with pattern matching on operations.

```erlang
%% Try-with expression (effect handler)
-record(try_with_expr, {
    body :: expr(),                           % Expression to execute
    handlers :: [#handler_clause{}],          % Handler clauses per effect
    location :: location()
}).

-record(handler_clause, {
    effect :: atom(),                         % Effect name being handled
    operation_cases :: [#operation_case{}],   % Pattern match cases per operation
    location :: location()
}).

-record(operation_case, {
    operation :: atom(),                      % Operation name (e.g., read)
    patterns :: [pattern()],                  % Parameter patterns
    guards :: [expr()] | undefined,           % Optional guards
    body :: expr(),                           % Handler implementation
    location :: location()
}).
```

**Example AST**:
```erlang
% try { perform FileIO.read(p) } with FileIO { read(path) -> "content" }
{try_with_expr,
  {perform_expr, 'FileIO', read, [{var, p, {location, 1, 23}}], {location, 1, 6}},
  [
    {handler_clause,
      'FileIO',
      [
        {operation_case,
          read,
          [{pat_var, path, {location, 1, 42}}],
          undefined,
          {literal, "content", string, {location, 1, 51}},
          {location, 1, 37}}
      ],
      {location, 1, 31}}
  ],
  {location, 1, 0}}
```

#### 4. Effect Annotation Node

Represents effect sets in type signatures (using `/` operator).

```erlang
%% Effect annotation (effect set in type signature)
-record(effect_annotation, {
    type :: type_expr(),                      % Return type
    effects :: [atom()],                      % Effect set (e.g., [FileIO, Process])
    location :: location()
}).
```

**Example AST**:
```erlang
% String / {FileIO, Process}
{effect_annotation,
  {type_con, 'String', {location, 1, 0}},
  ['FileIO', 'Process'],
  {location, 1, 7}}
```

#### 5. Updated Declaration Type

Extend the existing `declaration()` type to include effect declarations:

```erlang
% In catena_ast.hrl, update:
-type declaration() :: #shape_decl{} | #flow_decl{} | #trait_decl{} | #effect_decl{}.
```

### Grammar Rules

#### Terminals Section

Add effect keywords to the Terminals section (lines 40-62):

```erlang
Terminals
  %% Keywords
  shape flow match where 'let' 'in' 'do' 'end'
  'if' 'then' 'else' 'case' 'of' 'when'
  module import export exports as qualified private
  trait instance forall
  actor supervisor
  effect operation perform 'try' with  % ADD THIS LINE

  %% Operators
  pipe_right bind arrow double_arrow concat
  eq neq lte gte lt gt
  'or' 'and' cons left_arrow range
  colon equals pipe
  plus minus star slash dot

  %% Delimiters
  lbrace rbrace lbracket rbracket lparen rparen
  comma semicolon underscore

  %% Literals and identifiers
  integer float string
  lower_ident upper_ident
  .
```

#### Nonterminals Section

Add effect-related nonterminals (after line 34):

```erlang
Nonterminals
  catena_module
  declarations declaration
  shape_decl flow_decl effect_decl                              % ADD effect_decl
  type_params type_params_nonempty constructors constructor constructor_fields
  flow_signature flow_clauses flow_clause
  match_clauses match_clause
  pattern_list pattern_list_nonempty pattern tuple_pattern_list
  guards guard
  expr expr_primary expr_app expr_list tuple_expr_list
  record_fields record_field
  record_pattern_fields record_pattern_field
  literal
  type_expr type_expr_primary type_expr_app
  type_list type_record_fields type_record_field
  effect_operations effect_operation                            % ADD THESE
  effect_operation_params effect_operation_param                % ADD THESE
  handler_clauses handler_clause                                % ADD THESE
  operation_cases operation_case                                % ADD THESE
  effect_set effect_names                                       % ADD THESE
  .
```

#### Grammar Production Rules

**1. Effect Declarations** (Task 1.1.5.1)

Add after flow declarations (after line 208):

```erlang
%%----------------------------------------------------------------------------
%% Effect Declarations (Algebraic Effects)
%%----------------------------------------------------------------------------

effect_decl -> effect upper_ident lbrace effect_operations rbrace :
    {effect_decl,
        extract_atom('$2'),
        '$4',
        extract_location('$1')}.

effect_operations -> effect_operation :
    ['$1'].
effect_operations -> effect_operation effect_operations :
    ['$1' | '$2'].

effect_operation -> operation lower_ident lparen effect_operation_params rparen colon type_expr :
    {effect_operation,
        extract_atom('$2'),
        '$4',
        '$7',
        extract_location('$1')}.

effect_operation -> operation lower_ident lparen rparen colon type_expr :
    {effect_operation,
        extract_atom('$2'),
        [],
        '$6',
        extract_location('$1')}.

effect_operation_params -> effect_operation_param :
    ['$1'].
effect_operation_params -> effect_operation_param comma effect_operation_params :
    ['$1' | '$3'].

effect_operation_param -> lower_ident colon type_expr :
    {extract_atom('$1'), '$3'}.
```

**Success Criteria**: Can parse `effect FileIO { operation read(path: String): String }`

**2. Perform Expressions** (Task 1.1.5.2)

Add to expressions section (after line 363):

```erlang
expr_primary -> perform upper_ident dot lower_ident lparen expr_list rparen :
    {perform_expr,
        extract_atom('$2'),
        extract_atom('$4'),
        '$6',
        extract_location('$1')}.

expr_primary -> perform upper_ident dot lower_ident lparen rparen :
    {perform_expr,
        extract_atom('$2'),
        extract_atom('$4'),
        [],
        extract_location('$1')}.
```

**Success Criteria**: Can parse `perform FileIO.read(path)`

**3. Try-With Handlers** (Task 1.1.5.3)

Add to expressions section (after perform expressions):

```erlang
expr_primary -> 'try' lbrace expr rbrace handler_clauses :
    {try_with_expr,
        '$3',
        '$5',
        extract_location('$1')}.

handler_clauses -> handler_clause :
    ['$1'].
handler_clauses -> handler_clause handler_clauses :
    ['$1' | '$2'].

handler_clause -> with upper_ident lbrace operation_cases rbrace :
    {handler_clause,
        extract_atom('$2'),
        '$4',
        extract_location('$1')}.

handler_clause -> with upper_ident lbrace rbrace :
    {handler_clause,
        extract_atom('$2'),
        [],
        extract_location('$1')}.

operation_cases -> operation_case :
    ['$1'].
operation_cases -> operation_case operation_cases :
    ['$1' | '$2'].

operation_case -> lower_ident lparen pattern_list rparen arrow expr :
    {operation_case,
        extract_atom('$1'),
        '$3',
        undefined,
        '$6',
        extract_location('$1')}.

operation_case -> lower_ident lparen rparen arrow expr :
    {operation_case,
        extract_atom('$1'),
        [],
        undefined,
        '$5',
        extract_location('$1')}.

operation_case -> lower_ident lparen pattern_list rparen 'when' guards arrow expr :
    {operation_case,
        extract_atom('$1'),
        '$3',
        '$6',
        '$8',
        extract_location('$1')}.
```

**Success Criteria**: Can parse complete handler blocks with multiple operation cases

**4. Effect Annotations** (Task 1.1.5.4)

Add to type expressions section (after line 444):

```erlang
type_expr -> type_expr slash lbrace effect_set rbrace :
    {effect_annotation,
        '$1',
        '$4',
        extract_location('$2')}.

type_expr -> type_expr slash lbrace rbrace :
    {effect_annotation,
        '$1',
        [],
        extract_location('$2')}.

effect_set -> effect_names :
    '$1'.

effect_names -> upper_ident :
    [extract_atom('$1')].
effect_names -> upper_ident comma effect_names :
    [extract_atom('$1') | '$3'].
```

**Success Criteria**: Can parse `String / {FileIO, Process}` in function signatures

**5. Updated Declaration Rule**

Update the declaration rule to include effect declarations (line 100-101):

```erlang
declaration -> shape_decl : '$1'.
declaration -> flow_decl : '$1'.
declaration -> effect_decl : '$1'.  % ADD THIS LINE
```

### Helper Function Extensions

Add cases to `extract_location/1` function (after line 541):

```erlang
extract_location({effect_decl, _Name, _Operations, Loc}) -> Loc;
extract_location({effect_operation, _Name, _Params, _ReturnType, Loc}) -> Loc;
extract_location({perform_expr, _Effect, _Operation, _Args, Loc}) -> Loc;
extract_location({try_with_expr, _Body, _Handlers, Loc}) -> Loc;
extract_location({handler_clause, _Effect, _Cases, Loc}) -> Loc;
extract_location({operation_case, _Operation, _Patterns, _Guards, _Body, Loc}) -> Loc;
extract_location({effect_annotation, _Type, _Effects, Loc}) -> Loc;
```

### Operator Precedence Considerations

The slash (`/`) operator for effect annotations needs precedence specification. Current precedence table (lines 74-86):

```erlang
Right    100 arrow.           %% Type-level function arrow (right-assoc)
Right    150 pipe_right.      %% |> pipe operator
Right    160 bind.            %% >>= Kleisli composition

Nonassoc 300 eq neq.          %% == /=
Nonassoc 310 lt gt lte gte.   %% < > <= >=
Right    350 concat.          %% <> (right-assoc for strings)

Left     400 plus minus.      %% + -
Left     500 star slash.      %% * /

Left     600 dot.             %% Record field access
```

The slash is already defined at precedence 500 for division. For effect annotations in type signatures, we can reuse this token. The parser will disambiguate based on context:
- In type expressions: `String / {FileIO}` parsed as effect annotation
- In value expressions: `10 / 2` parsed as division

This works because type expressions and value expressions are in separate syntactic categories.

---

## Implementation Plan

### Step 1: Extend AST Definitions (Day 1 Morning - 2 hours)

**Actions**:
1. Open `/home/ducky/code/catena/src/compiler/parser/catena_ast.hrl`
2. Add 5 new record definitions for effect nodes:
   - `effect_decl`
   - `effect_operation`
   - `perform_expr`
   - `try_with_expr`
   - `handler_clause`
   - `operation_case`
   - `effect_annotation`
3. Update `declaration()` type to include `#effect_decl{}`
4. Update `expr()` type to include `#perform_expr{}` and `#try_with_expr{}`
5. Update `type_expr()` type to include `#effect_annotation{}`

**Success Criteria**:
- File compiles without syntax errors
- All new records have proper type specifications
- Location metadata included in all nodes

### Step 2: Add Terminals and Nonterminals (Day 1 Morning - 30 minutes)

**Actions**:
1. Open `/home/ducky/code/catena/src/compiler/parser/catena_parser.yrl`
2. Add effect keywords to Terminals section:
   - `effect operation perform 'try' with`
3. Add effect nonterminals to Nonterminals section:
   - `effect_decl effect_operations effect_operation`
   - `effect_operation_params effect_operation_param`
   - `handler_clauses handler_clause`
   - `operation_cases operation_case`
   - `effect_set effect_names`

**Success Criteria**:
- Parser compiles (yecc accepts syntax)
- No undefined terminal/nonterminal errors

### Step 3: Implement Effect Declaration Grammar (Day 1 Afternoon - 2 hours)

**Actions**:
1. Add effect declaration production rules (after flow declarations)
2. Implement effect operation parsing with parameters
3. Test parsing: `effect FileIO { operation read(path: String): String }`

**Success Criteria**:
- Can parse simple effect with one operation
- Can parse effect with multiple operations
- Can parse operations with 0, 1, or multiple parameters
- AST matches expected structure

### Step 4: Implement Perform Expression Grammar (Day 1 Afternoon - 1 hour)

**Actions**:
1. Add perform expression rules to `expr_primary`
2. Test parsing: `perform FileIO.read(path)`
3. Test with 0, 1, and multiple arguments

**Success Criteria**:
- Can parse perform expressions as primary expressions
- Can use perform in larger expressions (e.g., `let x = perform E.op in x`)
- AST correctly captures effect name, operation name, and arguments

### Step 5: Implement Try-With Handler Grammar (Day 2 Morning - 3 hours)

**Actions**:
1. Add try-with expression rules to `expr_primary`
2. Implement handler clause parsing
3. Implement operation case parsing with patterns and guards
4. Test parsing: `try { perform E.op } with E { op(x) -> x }`
5. Test multiple handlers and multiple operation cases

**Success Criteria**:
- Can parse try-with with single handler
- Can parse multiple handlers (e.g., `with E1 { ... } with E2 { ... }`)
- Can parse multiple operation cases per handler
- Can parse operation cases with guards
- AST correctly represents nested structure

### Step 6: Implement Effect Annotation Grammar (Day 2 Afternoon - 2 hours)

**Actions**:
1. Add effect annotation rules to type expressions
2. Implement effect set parsing
3. Test parsing: `String / {FileIO, Process}`
4. Test in function signatures: `flow f : Int -> String / {IO}`

**Success Criteria**:
- Can parse effect annotations in type expressions
- Can parse empty effect set: `String / {}`
- Can parse single effect: `String / {IO}`
- Can parse multiple effects: `String / {IO, State, Error}`
- Works correctly in function type signatures

### Step 7: Extend Helper Functions (Day 2 Afternoon - 1 hour)

**Actions**:
1. Add cases to `extract_location/1` for all new AST node types
2. Ensure all new nodes work with existing helper functions
3. Test that location tracking works correctly for effect syntax

**Success Criteria**:
- No crashes when extracting locations from effect nodes
- Location information accurate for error reporting
- Works with enhanced location format

### Step 8: Write Unit Tests (Day 3 Morning - 3 hours)

**Actions**:
1. Create `/home/ducky/code/catena/test/compiler/parser/catena_parser_effect_tests.erl`
2. Add unit tests for each grammar rule:
   - Effect declaration parsing (5+ tests)
   - Perform expression parsing (5+ tests)
   - Try-with handler parsing (8+ tests)
   - Effect annotation parsing (5+ tests)
3. Run tests: `./rebar3 eunit --module=catena_parser_effect_tests`

**Test Cases**:

```erlang
-module(catena_parser_effect_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Effect Declaration Tests (Task 1.1.5.1)
%%====================================================================

parse_simple_effect_test() ->
    Tokens = [
        {effect, 1},
        {upper_ident, 1, "Console"},
        {lbrace, 1},
        {operation, 1},
        {lower_ident, 1, "print"},
        {lparen, 1},
        {lower_ident, 1, "msg"},
        {colon, 1},
        {upper_ident, 1, "String"},
        {rparen, 1},
        {colon, 1},
        {upper_ident, 1, "Unit"},
        {rbrace, 1}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    ?assertMatch({module, _, _, _, [_], _}, Result),
    {module, _, _, _, [EffectDecl], _} = Result,
    ?assertMatch({effect_decl, 'Console', [_], _}, EffectDecl),
    {effect_decl, _, [Operation], _} = EffectDecl,
    ?assertMatch({effect_operation, print, [{msg, _}], _, _}, Operation).

parse_effect_multiple_operations_test() ->
    Tokens = [
        {effect, 1},
        {upper_ident, 1, "FileIO"},
        {lbrace, 1},
        {operation, 1},
        {lower_ident, 1, "read"},
        {lparen, 1},
        {lower_ident, 1, "path"},
        {colon, 1},
        {upper_ident, 1, "String"},
        {rparen, 1},
        {colon, 1},
        {upper_ident, 1, "String"},
        {operation, 2},
        {lower_ident, 2, "write"},
        {lparen, 2},
        {lower_ident, 2, "path"},
        {colon, 2},
        {upper_ident, 2, "String"},
        {comma, 2},
        {lower_ident, 2, "content"},
        {colon, 2},
        {upper_ident, 2, "String"},
        {rparen, 2},
        {colon, 2},
        {upper_ident, 2, "Unit"},
        {rbrace, 2}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [EffectDecl], _} = Result,
    {effect_decl, 'FileIO', Operations, _} = EffectDecl,
    ?assertEqual(2, length(Operations)).

parse_effect_no_params_test() ->
    Tokens = [
        {effect, 1},
        {upper_ident, 1, "Random"},
        {lbrace, 1},
        {operation, 1},
        {lower_ident, 1, "get"},
        {lparen, 1},
        {rparen, 1},
        {colon, 1},
        {upper_ident, 1, "Int"},
        {rbrace, 1}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [EffectDecl], _} = Result,
    {effect_decl, 'Random', [Operation], _} = EffectDecl,
    ?assertMatch({effect_operation, get, [], _, _}, Operation).

%%====================================================================
%% Perform Expression Tests (Task 1.1.5.2)
%%====================================================================

parse_perform_simple_test() ->
    Tokens = [
        {flow, 1},
        {lower_ident, 1, "main"},
        {equals, 1},
        {perform, 1},
        {upper_ident, 1, "Console"},
        {dot, 1},
        {lower_ident, 1, "print"},
        {lparen, 1},
        {string, 1, "Hello"},
        {rparen, 1}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {flow_decl, _, _, [{flow_clause, _, _, Expr, _}], _} = FlowDecl,
    ?assertMatch({perform_expr, 'Console', print, [_], _}, Expr).

parse_perform_no_args_test() ->
    Tokens = [
        {flow, 1},
        {lower_ident, 1, "getValue"},
        {equals, 1},
        {perform, 1},
        {upper_ident, 1, "Random"},
        {dot, 1},
        {lower_ident, 1, "get"},
        {lparen, 1},
        {rparen, 1}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {flow_decl, _, _, [{flow_clause, _, _, Expr, _}], _} = FlowDecl,
    ?assertMatch({perform_expr, 'Random', get, [], _}, Expr).

parse_perform_multiple_args_test() ->
    Tokens = [
        {flow, 1},
        {lower_ident, 1, "saveData"},
        {equals, 1},
        {perform, 1},
        {upper_ident, 1, "FileIO"},
        {dot, 1},
        {lower_ident, 1, "write"},
        {lparen, 1},
        {string, 1, "data.txt"},
        {comma, 1},
        {string, 1, "content"},
        {rparen, 1}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {flow_decl, _, _, [{flow_clause, _, _, Expr, _}], _} = FlowDecl,
    ?assertMatch({perform_expr, 'FileIO', write, [_, _], _}, Expr).

%%====================================================================
%% Try-With Handler Tests (Task 1.1.5.3)
%%====================================================================

parse_try_with_simple_test() ->
    Tokens = [
        {flow, 1},
        {lower_ident, 1, "test"},
        {equals, 1},
        {'try', 1},
        {lbrace, 1},
        {integer, 1, 42},
        {rbrace, 1},
        {with, 1},
        {upper_ident, 1, "Console"},
        {lbrace, 1},
        {rbrace, 1}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {flow_decl, _, _, [{flow_clause, _, _, Expr, _}], _} = FlowDecl,
    ?assertMatch({try_with_expr, {literal, 42, integer, _}, [_], _}, Expr).

parse_try_with_single_handler_test() ->
    Tokens = [
        {flow, 1},
        {lower_ident, 1, "test"},
        {equals, 1},
        {'try', 1},
        {lbrace, 1},
        {perform, 1},
        {upper_ident, 1, "Console"},
        {dot, 1},
        {lower_ident, 1, "print"},
        {lparen, 1},
        {string, 1, "Hi"},
        {rparen, 1},
        {rbrace, 1},
        {with, 1},
        {upper_ident, 1, "Console"},
        {lbrace, 1},
        {lower_ident, 1, "print"},
        {lparen, 1},
        {lower_ident, 1, "msg"},
        {rparen, 1},
        {arrow, 1},
        {integer, 1, 0},
        {rbrace, 1}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {flow_decl, _, _, [{flow_clause, _, _, TryExpr, _}], _} = FlowDecl,
    ?assertMatch({try_with_expr, _, [_], _}, TryExpr),
    {try_with_expr, _, [Handler], _} = TryExpr,
    ?assertMatch({handler_clause, 'Console', [_], _}, Handler).

parse_try_with_multiple_operations_test() ->
    Tokens = [
        {flow, 1},
        {lower_ident, 1, "test"},
        {equals, 1},
        {'try', 1},
        {lbrace, 1},
        {integer, 1, 1},
        {rbrace, 1},
        {with, 1},
        {upper_ident, 1, "FileIO"},
        {lbrace, 1},
        {lower_ident, 1, "read"},
        {lparen, 1},
        {lower_ident, 1, "p"},
        {rparen, 1},
        {arrow, 1},
        {string, 1, "data"},
        {lower_ident, 2},
        {lower_ident, 2, "write"},
        {lparen, 2},
        {lower_ident, 2, "p"},
        {comma, 2},
        {lower_ident, 2, "c"},
        {rparen, 2},
        {arrow, 2},
        {integer, 2, 0},
        {rbrace, 2}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {flow_decl, _, _, [{flow_clause, _, _, TryExpr, _}], _} = FlowDecl,
    {try_with_expr, _, [Handler], _} = TryExpr,
    {handler_clause, 'FileIO', Operations, _} = Handler,
    ?assertEqual(2, length(Operations)).

parse_try_with_guards_test() ->
    Tokens = [
        {flow, 1},
        {lower_ident, 1, "test"},
        {equals, 1},
        {'try', 1},
        {lbrace, 1},
        {integer, 1, 1},
        {rbrace, 1},
        {with, 1},
        {upper_ident, 1, "E"},
        {lbrace, 1},
        {lower_ident, 1, "op"},
        {lparen, 1},
        {lower_ident, 1, "x"},
        {rparen, 1},
        {'when', 1},
        {lower_ident, 1, "x"},
        {gt, 1},
        {integer, 1, 0},
        {arrow, 1},
        {integer, 1, 1},
        {rbrace, 1}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {flow_decl, _, _, [{flow_clause, _, _, TryExpr, _}], _} = FlowDecl,
    {try_with_expr, _, [Handler], _} = TryExpr,
    {handler_clause, 'E', [OpCase], _} = Handler,
    ?assertMatch({operation_case, op, [_], [_], _, _}, OpCase).

parse_try_with_multiple_handlers_test() ->
    Tokens = [
        {flow, 1},
        {lower_ident, 1, "test"},
        {equals, 1},
        {'try', 1},
        {lbrace, 1},
        {integer, 1, 1},
        {rbrace, 1},
        {with, 1},
        {upper_ident, 1, "E1"},
        {lbrace, 1},
        {rbrace, 1},
        {with, 2},
        {upper_ident, 2, "E2"},
        {lbrace, 2},
        {rbrace, 2}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {flow_decl, _, _, [{flow_clause, _, _, TryExpr, _}], _} = FlowDecl,
    {try_with_expr, _, Handlers, _} = TryExpr,
    ?assertEqual(2, length(Handlers)).

%%====================================================================
%% Effect Annotation Tests (Task 1.1.5.4)
%%====================================================================

parse_effect_annotation_single_test() ->
    Tokens = [
        {flow, 1},
        {lower_ident, 1, "f"},
        {colon, 1},
        {upper_ident, 1, "String"},
        {slash, 1},
        {lbrace, 1},
        {upper_ident, 1, "IO"},
        {rbrace, 1},
        {flow, 2},
        {lower_ident, 2, "f"},
        {equals, 2},
        {integer, 2, 0}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {flow_decl, f, TypeSig, _, _} = FlowDecl,
    ?assertMatch({effect_annotation, _, ['IO'], _}, TypeSig).

parse_effect_annotation_multiple_test() ->
    Tokens = [
        {flow, 1},
        {lower_ident, 1, "g"},
        {colon, 1},
        {upper_ident, 1, "Int"},
        {slash, 1},
        {lbrace, 1},
        {upper_ident, 1, "IO"},
        {comma, 1},
        {upper_ident, 1, "State"},
        {comma, 1},
        {upper_ident, 1, "Error"},
        {rbrace, 1},
        {flow, 2},
        {lower_ident, 2, "g"},
        {equals, 2},
        {integer, 2, 42}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {flow_decl, g, TypeSig, _, _} = FlowDecl,
    ?assertMatch({effect_annotation, _, ['IO', 'State', 'Error'], _}, TypeSig).

parse_effect_annotation_empty_test() ->
    Tokens = [
        {flow, 1},
        {lower_ident, 1, "pure"},
        {colon, 1},
        {upper_ident, 1, "String"},
        {slash, 1},
        {lbrace, 1},
        {rbrace, 1},
        {flow, 2},
        {lower_ident, 2, "pure"},
        {equals, 2},
        {string, 2, "hello"}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {flow_decl, pure, TypeSig, _, _} = FlowDecl,
    ?assertMatch({effect_annotation, _, [], _}, TypeSig).

parse_effect_annotation_in_function_type_test() ->
    Tokens = [
        {flow, 1},
        {lower_ident, 1, "process"},
        {colon, 1},
        {upper_ident, 1, "String"},
        {arrow, 1},
        {upper_ident, 1, "Int"},
        {slash, 1},
        {lbrace, 1},
        {upper_ident, 1, "FileIO"},
        {rbrace, 1},
        {flow, 2},
        {lower_ident, 2, "process"},
        {lower_ident, 2, "x"},
        {equals, 2},
        {integer, 2, 0}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {flow_decl, process, TypeSig, _, _} = FlowDecl,
    ?assertMatch({type_fun, _, {effect_annotation, _, ['FileIO'], _}, _}, TypeSig).
```

**Success Criteria**:
- All 20+ test cases pass
- Tests cover edge cases (empty lists, multiple items, guards)
- Tests verify AST structure correctness

### Step 9: Write Integration Tests (Day 3 Afternoon - 2 hours)

**Actions**:
1. Add integration tests that combine multiple effect features
2. Test realistic Phase 1 examples from documentation
3. Test complex nested structures

**Integration Test Cases**:

```erlang
%%====================================================================
%% Integration Tests - Complete Effect Programs
%%====================================================================

parse_phase1_effect_declaration_test() ->
    %% From Phase 1 documentation:
    %% effect FileIO { operation read(path: String): String }
    Tokens = [
        {effect, 1},
        {upper_ident, 1, "FileIO"},
        {lbrace, 1},
        {operation, 1},
        {lower_ident, 1, "read"},
        {lparen, 1},
        {lower_ident, 1, "path"},
        {colon, 1},
        {upper_ident, 1, "String"},
        {rparen, 1},
        {colon, 1},
        {upper_ident, 1, "String"},
        {rbrace, 1}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    ?assertMatch({module, _, _, _, [{effect_decl, 'FileIO', [_], _}], _}, Result).

parse_complete_effectful_function_test() ->
    %% Function with effect annotation and perform
    Tokens = [
        {flow, 1},
        {lower_ident, 1, "loadFile"},
        {colon, 1},
        {upper_ident, 1, "String"},
        {arrow, 1},
        {upper_ident, 1, "String"},
        {slash, 1},
        {lbrace, 1},
        {upper_ident, 1, "FileIO"},
        {rbrace, 1},
        {flow, 2},
        {lower_ident, 2, "loadFile"},
        {lower_ident, 2, "path"},
        {equals, 2},
        {perform, 2},
        {upper_ident, 2, "FileIO"},
        {dot, 2},
        {lower_ident, 2, "read"},
        {lparen, 2},
        {lower_ident, 2, "path"},
        {rparen, 2}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    %% Verify type signature with effect annotation
    {flow_decl, loadFile, TypeSig, [Clause], _} = FlowDecl,
    ?assertMatch({type_fun, _, {effect_annotation, _, ['FileIO'], _}, _}, TypeSig),
    %% Verify body uses perform
    {flow_clause, [_], _, Body, _} = Clause,
    ?assertMatch({perform_expr, 'FileIO', read, [_], _}, Body).

parse_complete_handler_program_test() ->
    %% Complete program with effect, perform, and handler
    Tokens = [
        {flow, 1},
        {lower_ident, 1, "main"},
        {equals, 1},
        {'try', 1},
        {lbrace, 1},
        {perform, 1},
        {upper_ident, 1, "Console"},
        {dot, 1},
        {lower_ident, 1, "print"},
        {lparen, 1},
        {string, 1, "Hello"},
        {rparen, 1},
        {rbrace, 1},
        {with, 1},
        {upper_ident, 1, "Console"},
        {lbrace, 1},
        {lower_ident, 1, "print"},
        {lparen, 1},
        {lower_ident, 1, "msg"},
        {rparen, 1},
        {arrow, 1},
        {integer, 1, 0},
        {rbrace, 1}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {flow_decl, main, _, [{flow_clause, _, _, TryExpr, _}], _} = FlowDecl,
    %% Verify try-with structure
    ?assertMatch({try_with_expr, {perform_expr, _, _, _, _}, [_], _}, TryExpr),
    {try_with_expr, _, [Handler], _} = TryExpr,
    {handler_clause, 'Console', [OpCase], _} = Handler,
    ?assertMatch({operation_case, print, [_], undefined, _, _}, OpCase).

parse_nested_effects_test() ->
    %% Try-with inside another try-with
    Tokens = [
        {flow, 1},
        {lower_ident, 1, "nested"},
        {equals, 1},
        {'try', 1},
        {lbrace, 1},
        {'try', 1},
        {lbrace, 1},
        {integer, 1, 1},
        {rbrace, 1},
        {with, 1},
        {upper_ident, 1, "E1"},
        {lbrace, 1},
        {rbrace, 1},
        {rbrace, 1},
        {with, 1},
        {upper_ident, 1, "E2"},
        {lbrace, 1},
        {rbrace, 1}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {flow_decl, _, _, [{flow_clause, _, _, OuterTry, _}], _} = FlowDecl,
    ?assertMatch({try_with_expr, {try_with_expr, _, _, _}, _, _}, OuterTry).

parse_multiple_declarations_with_effects_test() ->
    %% Effect declaration followed by flow using it
    Tokens = [
        {effect, 1},
        {upper_ident, 1, "E"},
        {lbrace, 1},
        {operation, 1},
        {lower_ident, 1, "op"},
        {lparen, 1},
        {rparen, 1},
        {colon, 1},
        {upper_ident, 1, "Unit"},
        {rbrace, 1},
        {flow, 2},
        {lower_ident, 2, "use"},
        {equals, 2},
        {perform, 2},
        {upper_ident, 2, "E"},
        {dot, 2},
        {lower_ident, 2, "op"},
        {lparen, 2},
        {rparen, 2}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [EffectDecl, FlowDecl], _} = Result,
    ?assertMatch({effect_decl, 'E', _, _}, EffectDecl),
    ?assertMatch({flow_decl, use, _, _, _}, FlowDecl).
```

**Success Criteria**:
- All integration tests pass
- Tests verify complete programs with multiple effect features
- Tests match Phase 1 documentation examples

### Step 10: Manual Verification and Regression Testing (Day 4 - 2 hours)

**Actions**:
1. Run full parser test suite: `./rebar3 eunit --dir=test/compiler/parser`
2. Verify all existing tests still pass (regression check)
3. Manual testing in Erlang shell with complex examples
4. Test error cases (malformed syntax should produce parse errors)

**Manual Test Examples**:

```erlang
% In ./rebar3 shell

% Test effect declaration
Tokens1 = [
    {effect, 1}, {upper_ident, 1, "IO"}, {lbrace, 1},
    {operation, 1}, {lower_ident, 1, "print"}, {lparen, 1}, {rparen, 1},
    {colon, 1}, {upper_ident, 1, "Unit"}, {rbrace, 1}
],
catena_parser:parse(Tokens1).

% Test perform expression in flow
Tokens2 = [
    {flow, 1}, {lower_ident, 1, "test"}, {equals, 1},
    {perform, 1}, {upper_ident, 1, "IO"}, {dot, 1},
    {lower_ident, 1, "print"}, {lparen, 1}, {rparen, 1}
],
catena_parser:parse(Tokens2).

% Test complete handler
Tokens3 = [
    {flow, 1}, {lower_ident, 1, "main"}, {equals, 1},
    {'try', 1}, {lbrace, 1}, {integer, 1, 42}, {rbrace, 1},
    {with, 1}, {upper_ident, 1, "E"}, {lbrace, 1}, {rbrace, 1}
],
catena_parser:parse(Tokens3).
```

**Success Criteria**:
- All existing parser tests pass (no regressions)
- All new effect tests pass
- Manual testing confirms correct AST generation
- Parse errors for malformed syntax are clear

### Step 11: Documentation Update (Day 4 - 1 hour)

**Actions**:
1. Update parser documentation with effect syntax support
2. Document new AST node types for type checker developers
3. Note completion of Task 1.1.5
4. Update Phase 1 progress tracking

**Success Criteria**:
- Clear documentation of effect grammar rules
- AST node types documented with examples
- Task 1.1.5 marked complete in Phase 1 planning

---

## Success Criteria

### Functional Criteria

1. **Effect Declaration Parsing** (Task 1.1.5.1)
   - Can parse `effect Name { operation op(...): Type }`
   - Supports 0, 1, or multiple operations
   - Supports 0, 1, or multiple parameters per operation
   - Generates correct `effect_decl` and `effect_operation` AST nodes

2. **Perform Expression Parsing** (Task 1.1.5.2)
   - Can parse `perform Effect.operation(args)`
   - Works as primary expression in larger expressions
   - Supports 0, 1, or multiple arguments
   - Generates correct `perform_expr` AST nodes

3. **Try-With Handler Parsing** (Task 1.1.5.3)
   - Can parse `try { body } with Effect { cases }`
   - Supports multiple handlers per try block
   - Supports multiple operation cases per handler
   - Supports guards in operation cases
   - Generates correct `try_with_expr`, `handler_clause`, and `operation_case` AST nodes

4. **Effect Annotation Parsing** (Task 1.1.5.4)
   - Can parse `Type / {Effect1, Effect2}`
   - Works in function type signatures
   - Supports empty effect sets
   - Generates correct `effect_annotation` AST nodes

### Quality Criteria

5. **Test Coverage**
   - 20+ unit tests covering each grammar rule
   - 5+ integration tests covering realistic programs
   - All tests pass without failures
   - Edge cases covered (empty lists, multiple items, guards, nesting)

6. **Regression Safety**
   - All existing parser tests pass
   - No changes to existing AST node types
   - No changes to existing grammar rules

7. **Code Quality**
   - Follows established yecc patterns
   - Consistent naming conventions
   - Proper location tracking for all nodes
   - Clear and maintainable grammar rules

### Readiness Criteria

8. **Type Checker Readiness** (Section 1.2)
   - Effect AST nodes available for type-and-effect inference
   - Clear AST structure for effect tracking
   - Documentation available for type checker developers

9. **Phase 1 Alignment**
   - Implements all requirements from Task 1.1.5
   - Matches Phase 1 documentation examples
   - Enables Section 1.2 (Type System) work
   - Enables Section 1.3 (Code Generation) work

---

## Risk Assessment

### Risk Level: MEDIUM

This is a moderate-risk implementation due to grammar complexity.

### Risk Factors

**Moderate Risk**:
1. **Grammar Conflicts**: Yecc may report shift/reduce or reduce/reduce conflicts
2. **AST Complexity**: Effect nodes are more complex than existing nodes
3. **Ambiguous Syntax**: `/` operator used in both types and expressions
4. **Testing Scope**: Large number of test cases required

**Low Risk**:
5. **Well-Scoped**: Clear boundaries, no architectural changes
6. **Established Patterns**: Follows existing parser patterns
7. **Reversible**: Changes can be reverted if needed

### Potential Issues and Mitigations

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Shift/reduce conflicts in grammar | Medium | High | Use operator precedence; test incrementally |
| AST node mismatch with type checker | Low | High | Document AST structure; coordinate with Phase 1 |
| Performance degradation | Very Low | Low | Parser is already LALR; minimal impact expected |
| Test maintenance burden | Low | Medium | Well-organized test modules; clear test names |
| Breaking existing tests | Low | High | Run full regression suite after each step |
| Slash operator ambiguity | Medium | Medium | Context-based parsing; separate syntactic categories |

### Mitigation Strategies

1. **Incremental Development**: Implement one grammar rule at a time, testing each
2. **Early Testing**: Run parser compilation after each grammar addition
3. **Conflict Resolution**: Use yecc verbose mode to identify conflicts, resolve with precedence
4. **Comprehensive Testing**: 25+ test cases covering all edge cases
5. **Regression Testing**: Run full test suite after each step

---

## Alternative Approaches Considered

### Alternative 1: Separate Effect Module Type

**Description**: Treat effects as a special type of module rather than a first-class declaration.

**Pros**:
- Could reuse existing module grammar
- Aligns with ML module system

**Cons**:
- More complex semantics (effects aren't modules)
- Phase 1 explicitly defines effects as distinct from modules
- Would require significant research document updates

**Decision**: Rejected - Effects are first-class in Catena design

### Alternative 2: Use Do-Notation Instead of Perform

**Description**: Use monadic do-notation (`do { x <- operation }`) instead of explicit perform.

**Pros**:
- Familiar to Haskell developers
- Implicitly threads effects

**Cons**:
- Phase 1 explicitly uses algebraic effects (not monads)
- Less explicit about effect invocation
- Research document 1.17 chose algebraic effects over monads

**Decision**: Rejected - Phase 1 design uses algebraic effects with explicit perform

### Alternative 3: Effect Annotations Without Slash

**Description**: Use different syntax for effect annotations (e.g., `-> String with {IO}` or `-> String ! IO`).

**Pros**:
- Avoids ambiguity with division operator
- Used by some effect systems (Koka uses `!`)

**Cons**:
- Phase 1 documentation uses `/` syntax
- Research document 1.17 shows examples with `/`
- Slash disambiguation is solvable (type vs value context)

**Decision**: Rejected - Phase 1 design uses `/` syntax

### Chosen Approach: Direct Grammar Extension

**Why**:
- Aligns perfectly with Phase 1 documentation
- Implements research document 1.17 design
- Follows Task 1.1.5 specifications exactly
- Enables type-and-effect system (Section 1.2)
- Minimal complexity, maximum clarity

---

## Dependencies and Blockers

### Dependencies

1. **Task 1.1.1 (Token Recognition)** - COMPLETED
   - Provides working lexer with effect keywords
   - No changes needed

2. **Task 1.1.2 (Grammar Implementation)** - COMPLETED (base grammar)
   - Provides working parser infrastructure
   - Establishes grammar patterns to follow

3. **Effect Keywords Lexer Integration** - COMPLETED
   - Lexer recognizes all 5 effect keywords
   - Token format documented

4. **Phase 1 Documentation** - COMPLETED
   - Defines effect syntax requirements
   - Provides examples for testing

### No Blockers

This implementation has no blockers:
- All prerequisite tasks complete
- No external dependencies
- No pending architectural decisions
- Clear specifications in Phase 1

### Enables Future Work

This implementation enables:

1. **Section 1.2 (Type System)** - Type-and-effect inference
   - Task 1.2.1.5: Effect set representation
   - Task 1.2.2.5: Effect tracking during inference
   - Task 1.2.3.5: Effect handler verification
   - Task 1.2.5: Effect-specific error messages

2. **Section 1.3 (Code Generation)** - Effect compilation
   - Task 1.3.1.5: Perform/try-with translation
   - Task 1.3.5: Effect runtime system

3. **Section 1.4 (Integration Tests)** - Effect system testing
   - Task 1.4.4: End-to-end effectful programs

---

## Future Enhancements

### Short-Term (Phase 1 Completion)

1. **Error Recovery for Effect Syntax**
   - Better error messages for malformed effect declarations
   - Suggestions for common mistakes (e.g., missing operation keyword)

2. **Effect Documentation Comments**
   - Parse doc comments for effects and operations
   - Enable documentation generation

### Medium-Term (Phase 2-5)

3. **Effect Inference** (Phase 2)
   - Infer effect annotations automatically
   - Type checker integration

4. **Standard Library Effects** (Phase 2)
   - Built-in IO, State, Error effects
   - Imported from standard library modules

### Long-Term (Phase 6+)

5. **Effect Polymorphism** (Phase 6)
   - Effect variables in types (e.g., `forall e. Type / e`)
   - Row polymorphism for effects
   - Requires type system extensions

6. **Effect Optimization** (Phase 6)
   - Static effect resolution
   - Inline handlers for performance
   - Requires code generator enhancements

7. **Advanced Effect Features** (Phase 6)
   - Effect subtyping
   - Effect aliases
   - Effect composition operators

---

## Testing Strategy

### Test Organization

```
test/compiler/parser/
├── catena_parser_simple_tests.erl      # Existing tests
├── catena_parser_type_tests.erl        # Existing tests
├── catena_parser_pattern_tests.erl     # Existing tests
└── catena_parser_effect_tests.erl      # NEW: Effect syntax tests
```

### Test Coverage Matrix

| Feature | Unit Tests | Integration Tests | Edge Cases |
|---------|-----------|-------------------|------------|
| Effect declarations | 5 | 2 | Empty operations, multiple operations |
| Perform expressions | 5 | 2 | No args, multiple args, in complex expressions |
| Try-with handlers | 8 | 3 | Empty handlers, guards, nesting, multiple handlers |
| Effect annotations | 5 | 2 | Empty set, multiple effects, in function types |
| **Total** | **23** | **9** | **All covered** |

### Test Execution Plan

1. **Development Testing**: Run tests after each grammar rule addition
   ```bash
   ./rebar3 eunit --module=catena_parser_effect_tests
   ```

2. **Regression Testing**: Run full suite after each step
   ```bash
   ./rebar3 eunit --dir=test/compiler/parser
   ```

3. **Integration Testing**: Test complete programs
   ```bash
   ./rebar3 eunit --module=catena_parser_effect_tests --test=parse_complete_*
   ```

4. **Continuous Integration**: All tests in CI pipeline
   ```bash
   ./rebar3 eunit
   ```

---

## Conclusion

This feature planning document provides a comprehensive blueprint for integrating algebraic effects into the Catena parser. The implementation:

- **Implements Phase 1 Requirements**: Directly addresses Task 1.1.5
- **Enables Type System Work**: Provides AST nodes for Section 1.2
- **Well-Scoped**: Clear boundaries and success criteria
- **Thoroughly Planned**: 11 detailed implementation steps
- **Comprehensive Testing**: 32+ test cases (23 unit + 9 integration)
- **Risk-Managed**: Medium risk with clear mitigation strategies

**Estimated Total Time**: 3-4 days (24-32 hours)

**Recommended Timeline**:
- Day 1: AST definitions, terminals/nonterminals, effect declarations, perform expressions
- Day 2: Try-with handlers, effect annotations, helper functions
- Day 3: Comprehensive unit and integration testing
- Day 4: Regression testing, manual verification, documentation

Upon completion, Task 1.1.5 (Effect Syntax Support) will be complete, enabling:
- Section 1.2 (Core Type System) with type-and-effect inference
- Section 1.3 (Code Generation) with effect runtime compilation
- Section 1.4 (Integration Tests) with end-to-end effectful programs

This moves Phase 1 significantly forward toward the goal of a minimal viable algebraic effect system integrated throughout the compiler pipeline.

---

## References

### Phase 1 Documentation

- **Phase 01 Plan**: `/home/ducky/code/catena/notes/planning/proof-of-concept/phase-01.md`
  - Task 1.1.5: Effect Syntax Support (lines 60-69)
  - Section 1.2: Core Type System with Effect Tracking (lines 82-150)
  - Section 1.3.5: Effect Runtime System (lines 199-208)

### Research Documentation

- **Research 1.17**: `/home/ducky/code/catena/notes/research/1.17-side-effects-design/1.17-side-effects-design.md`
  - Algebraic effects design rationale
  - Effect syntax examples
  - Category theory foundations

### Implementation Files

- **Parser Grammar**: `/home/ducky/code/catena/src/compiler/parser/catena_parser.yrl`
- **AST Definitions**: `/home/ducky/code/catena/src/compiler/parser/catena_ast.hrl`
- **Lexer Definition**: `/home/ducky/code/catena/src/compiler/lexer/catena_lexer.xrl`

### Test Files

- **Parser Tests**: `/home/ducky/code/catena/test/compiler/parser/catena_parser_simple_tests.erl`
- **Effect Tests**: `/home/ducky/code/catena/test/compiler/parser/catena_parser_effect_tests.erl` (NEW)

### Related Features

- **Effect Keywords Lexer**: `/home/ducky/code/catena/notes/features/effect-keywords-lexer.md` (COMPLETED)
- **Task 1.1.1**: Token Recognition (COMPLETED)
- **Task 1.1.2**: Grammar Implementation (IN PROGRESS)

### External References

- Yecc Documentation: Erlang/OTP official docs
- Algebraic Effects Research: Koka, Eff, Frank, Links languages
- Effect Systems: Research document 1.17 references
