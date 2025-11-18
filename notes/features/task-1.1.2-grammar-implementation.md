# Task 1.1.2: Grammar Implementation - Feature Planning Document

## Metadata

**Task ID**: 1.1.2
**Task Name**: Grammar Implementation
**Phase**: Phase 1 - Core Language Infrastructure
**Section**: 1.1 - Lexer and Parser
**Estimated Duration**: 5-7 days
**Dependencies**: Task 1.1.1 (Token Recognition) - COMPLETE
**Created**: 2025-11-08
**Status**: 🟡 IN PROGRESS - Minimal Viable Parser Complete

## Implementation Status (2025-11-08)

### ✅ Completed
- AST record definitions (complete for all language constructs)
- Minimal viable yecc grammar
- Basic parser generation (24KB generated code)
- Test suite for implemented features (7 tests, 100% passing)
- Shape declarations with ADT constructors
- Flow declarations with simple expressions
- Literal expressions (integer, float, string)
- Variable expressions

### 🟡 In Progress
- Full expression grammar with operators
- Pattern matching support
- Type expression parsing
- Operator precedence tables

### ⏳ Not Started
- Match expressions
- Guards in patterns
- Complex type expressions
- Record syntax
- Comprehensive test suite (40+ tests planned)

---

## Problem Statement

The Catena compiler requires a parser to transform the token stream from the lexer into Abstract Syntax Trees (AST). The parser must define the syntactic structure of Catena programs using production rules that handle:

- **Type declarations** (`shape`): Algebraic data types with constructors and record syntax
- **Function definitions** (`flow`): Type signatures and pattern matching clauses
- **Expressions**: Composition operators, function application, let bindings, pattern matching
- **Operator precedence**: Ensuring correct parsing of complex expressions (e.g., function application binds tighter than `|>`)
- **Pattern matching**: Supporting guards, nested patterns, or-patterns
- **Module structure**: Top-level declarations and imports/exports

The grammar must be:
1. **Unambiguous**: Each valid input has exactly one parse tree
2. **Context-free**: Expressible as production rules without context-sensitive constraints
3. **LALR(1) compatible**: Can be parsed by yecc (Erlang's parser generator)
4. **Precedence-correct**: Operator precedence and associativity properly encoded
5. **Extensible**: Easy to add new constructs in future phases

**Current State**: The lexer (Task 1.1.1) produces a token stream. No parser exists yet.

**Target State**: A working parser implemented using yecc that can parse all Catena syntax elements defined in Phase 1 and produce structured ASTs ready for type checking.

---

## Solution Overview

We will implement the Catena parser using **yecc**, Erlang's LALR parser generator (analogous to yacc/bison). Yecc uses context-free grammar production rules with precedence declarations to generate a parser module.

### High-Level Approach

1. **Create a yecc grammar file** (`catena_parser.yrl`) with four sections:
   - **Nonterminals**: Non-terminal symbols (grammar rules)
   - **Terminals**: Terminal symbols (tokens from lexer)
   - **Rootsymbol**: Top-level grammar rule
   - **Rules**: Production rules defining syntax

2. **Define grammar production rules** in BNF-like notation:
   - Module structure and declarations
   - Shape declarations (algebraic data types)
   - Flow definitions (functions with type signatures)
   - Expressions (literals, variables, applications, operators)
   - Patterns (constructors, records, wildcards, guards)
   - Type expressions (type constructors, function types, row types)

3. **Specify operator precedence and associativity**:
   - Use yecc's precedence declarations (`Left`, `Right`, `Nonassoc`)
   - Define relative precedence levels for all operators
   - Ensure correct parsing of ambiguous expressions

4. **Generate AST nodes** as Erlang tuples/records:
   - Each production rule includes Erlang code to construct AST
   - AST nodes preserve source location metadata
   - Consistent AST structure for all constructs

5. **Handle ambiguity and conflicts**:
   - Resolve shift/reduce conflicts with precedence
   - Resolve reduce/reduce conflicts with grammar refactoring
   - Document any remaining conflicts with justification

6. **Compile and test** with comprehensive test cases

### Technology Choice: Yecc

**Why Yecc?**
- Native Erlang tool, integrates with leex and BEAM ecosystem
- Mature and battle-tested (used for Erlang's own parser)
- LALR(1) parsing is efficient and well-understood
- Precedence declarations simplify operator handling
- Generates readable Erlang code for debugging

**Yecc File Structure:**
```
Nonterminals
  [non-terminal symbols]

Terminals
  [terminal symbols from lexer]

Rootsymbol root_symbol.

[Precedence declarations]
Left     100 pipe.
Right    200 arrow.
[...]

Rules.
non_terminal -> production1 : {ast_node, '$1'}.
non_terminal -> production2 : {ast_node, '$1', '$2'}.

Erlang code.
[Helper functions]
```

---

## Research: Catena Syntax Analysis

### Core Syntax Elements from Research Documents

#### 1. Shape Declarations (ADTs)

From `1.01-original_idea.md`:

```catena
-- Sum types (coproducts)
shape Result a b = Ok a | Error b

-- Product types (records)
shape Point = { x: Float, y: Float }

-- Derives clause
shape Point = { x: Float, y: Float }
  derives [Eq, Show, Functor]

-- Recursive types
shape List a = Nil | Cons a (List a)

-- Parametric polymorphism
shape Maybe a = Some a | None
```

**Grammar considerations**:
- Type parameters: `a`, `b`, `c` (lowercase identifiers)
- Constructors: `Ok`, `Error`, `None` (uppercase identifiers)
- Record syntax: `{ field: Type, ... }`
- Or-patterns in constructors: `|` separator
- Optional derives clause

#### 2. Flow Definitions (Functions)

```catena
-- Type signature
flow greet : User -> Text

-- Implementation with single clause
flow greet user = "Hello, " <> user.name

-- Pattern matching with multiple clauses
flow map : (a -> b) -> List a -> List b
flow map f = match
  | Nil -> Nil
  | Cons x xs -> Cons (f x) (map f xs)
end

-- With guards
flow handle : Result User Error -> Response
flow handle = match
  | Ok user when user.age >= 18 -> Success (greet user)
  | Ok user -> MinorUser user
  | Error msg -> Failure msg
end
```

**Grammar considerations**:
- Type signatures: `flow name : Type`
- Implementation: `flow name pattern = expr`
- Match expressions: `match | pattern -> expr end`
- Guards: `when condition`
- Function composition in type signatures: `->`

#### 3. Expressions

```catena
-- Literals
42
3.14
"hello"

-- Variables and identifiers
x
user
count

-- Function application
f x
map f xs
greet user

-- Composition operators
validate |> transform |> persist       -- pipe
parse? >>= validate? >>= store?         -- Kleisli
spawn_all <|> collect_results           -- parallel

-- Let bindings
let x = 42 in x + 1
let counter = spawn Counter.init()
in counter ! Increment

-- Record access
user.name
point.x

-- Record construction
{ x: 1.0, y: 2.0 }
{ name: "Alice", age: 30 }

-- Record update
{ user | age: 31 }

-- If expressions
if condition then expr1 else expr2

-- Do notation (monadic)
do
  x <- action1
  y <- action2
  return (x + y)
end
```

**Grammar considerations**:
- Operator precedence: application > composition > let
- Record syntax: construction, access, update
- Do notation: special syntax for monadic operations
- If-then-else: three-part expression

#### 4. Patterns

From `1.10-pattern-matching.md`:

```catena
-- Constructor patterns
Ok x
Error msg
Cons x xs

-- Record patterns
{ x: x, y: y }
{ name: n, age: a }

-- Wildcard
_

-- Variable binding
x
user

-- As-patterns
list@(Cons x xs)

-- Or-patterns
Ok x | Success x

-- Nested patterns
Cons (Ok x) xs

-- Guards
pattern when condition

-- View patterns (advanced)
parse_int -> Some(n)
```

**Grammar considerations**:
- Recursive pattern structure
- Guards as separate clause
- Or-patterns using `|`
- As-patterns using `@`

#### 5. Type Expressions

From `1.15-advanced-type-system.md`:

```catena
-- Type variables
a
b
t

-- Type constructors
Int
Float
String
List a
Maybe a

-- Function types
a -> b
Int -> String
(a -> b) -> List a -> List b

-- Record types (row polymorphism)
{ x: Float, y: Float }
{ x: Float, y: Float | ρ }

-- Polymorphic variants
[> `Red | `Green | `Blue]
[< `A | `B | `C]

-- Forall quantification
forall a. a -> a
forall a b. (a -> b) -> List a -> List b
```

**Grammar considerations**:
- Type application: `List a`, `Maybe Int`
- Function type arrows: right-associative
- Row types: `| ρ` for row variables
- Variant types: `[>`, `[<` markers
- Forall quantifiers

---

## Technical Details

### File Locations

```
catena/
├── src/
│   ├── compiler/
│   │   ├── parser/
│   │   │   ├── catena_parser.yrl       # Yecc grammar definition
│   │   │   └── catena_parser.erl       # Generated parser module
│   │   ├── ast.hrl                    # AST node definitions
│   │   └── ast.erl                    # AST helper functions
└── test/
    └── compiler/
        └── parser/
            ├── catena_parser_test.erl  # EUnit tests
            └── fixtures/               # Test input files
                ├── shapes.catena
                ├── flows.catena
                ├── expressions.catena
                └── patterns.catena
```

### AST Node Design

**Design Principles**:
1. **Consistent structure**: All nodes are tuples with type tag first
2. **Location tracking**: All nodes include `{line, LineNum}` metadata
3. **Compositional**: Nodes nest naturally
4. **Pattern-matchable**: Easy to traverse with Erlang pattern matching

**AST Node Definitions** (`ast.hrl`):

```erlang
%% Module structure
-record(module, {
    name :: atom(),
    exports :: [export()],
    imports :: [import()],
    declarations :: [declaration()],
    location :: location()
}).

%% Declarations
-type declaration() :: shape_decl() | flow_decl() | trait_decl().

-record(shape_decl, {
    name :: atom(),
    type_params :: [atom()],
    constructors :: [constructor()],
    derives :: [atom()],
    location :: location()
}).

-record(constructor, {
    name :: atom(),
    fields :: [type_expr()],
    location :: location()
}).

-record(flow_decl, {
    name :: atom(),
    type_sig :: type_expr() | undefined,
    clauses :: [flow_clause()],
    location :: location()
}).

-record(flow_clause, {
    patterns :: [pattern()],
    guards :: [expr()] | undefined,
    body :: expr(),
    location :: location()
}).

%% Expressions
-type expr() :: literal() | var() | app() | lambda() | let_expr() | 
                match_expr() | if_expr() | do_expr() | record_expr() |
                binary_op() | unary_op().

-record(literal, {
    value :: integer() | float() | binary(),
    type :: integer | float | string,
    location :: location()
}).

-record(var, {
    name :: atom(),
    location :: location()
}).

-record(app, {
    func :: expr(),
    args :: [expr()],
    location :: location()
}).

-record(lambda, {
    params :: [pattern()],
    body :: expr(),
    location :: location()
}).

-record(let_expr, {
    bindings :: [{pattern(), expr()}],
    body :: expr(),
    location :: location()
}).

-record(match_expr, {
    clauses :: [match_clause()],
    location :: location()
}).

-record(match_clause, {
    pattern :: pattern(),
    guards :: [expr()] | undefined,
    body :: expr(),
    location :: location()
}).

-record(if_expr, {
    condition :: expr(),
    then_branch :: expr(),
    else_branch :: expr(),
    location :: location()
}).

-record(binary_op, {
    op :: atom(),
    left :: expr(),
    right :: expr(),
    location :: location()
}).

-record(record_expr, {
    fields :: [{atom(), expr()}],
    base :: expr() | undefined,  % For record update
    location :: location()
}).

-record(record_access, {
    record :: expr(),
    field :: atom(),
    location :: location()
}).

%% Patterns
-type pattern() :: pat_var() | pat_constructor() | pat_record() | 
                   pat_literal() | pat_wildcard() | pat_as().

-record(pat_var, {
    name :: atom(),
    location :: location()
}).

-record(pat_constructor, {
    name :: atom(),
    args :: [pattern()],
    location :: location()
}).

-record(pat_record, {
    fields :: [{atom(), pattern()}],
    location :: location()
}).

-record(pat_literal, {
    value :: integer() | float() | binary(),
    location :: location()
}).

-record(pat_wildcard, {
    location :: location()
}).

-record(pat_as, {
    name :: atom(),
    pattern :: pattern(),
    location :: location()
}).

%% Type expressions
-type type_expr() :: type_var() | type_con() | type_app() | 
                     type_fun() | type_record() | type_forall().

-record(type_var, {
    name :: atom(),
    location :: location()
}).

-record(type_con, {
    name :: atom(),
    location :: location()
}).

-record(type_app, {
    constructor :: type_expr(),
    args :: [type_expr()],
    location :: location()
}).

-record(type_fun, {
    from :: type_expr(),
    to :: type_expr(),
    location :: location()
}).

-record(type_record, {
    fields :: [{atom(), type_expr()}],
    row_var :: atom() | undefined,
    location :: location()
}).

-record(type_forall, {
    type_vars :: [atom()],
    type :: type_expr(),
    location :: location()
}).

%% Location metadata
-type location() :: {line, pos_integer()}.
```

### Yecc Grammar File Structure

**File**: `src/compiler/parser/catena_parser.yrl`

The grammar file will have the following structure:

```erlang
%% Catena Parser - Parser for Catena language
%% Generated using Erlang yecc

Header "%% -*- erlang -*-".

%% ============================================================================
%% Nonterminals
%% ============================================================================

Nonterminals
  module
  declarations declaration
  shape_decl constructors constructor constructor_fields
  flow_decl flow_signature flow_clauses flow_clause
  type_expr type_expr_app type_expr_atomic type_params
  type_record_fields type_record_field
  expr expr_app expr_atomic expr_list
  let_bindings let_binding
  match_clauses match_clause
  record_fields record_field
  pattern pattern_app pattern_atomic pattern_list
  pattern_record_fields pattern_record_field
  guards guard
  .

%% ============================================================================
%% Terminals (from lexer)
%% ============================================================================

Terminals
  %% Keywords
  shape flow match where let in do end
  actor trait effect category export import
  supervisor schema law convert spawn
  implements derives when try catch return
  if then else case of forall
  
  %% Operators
  pipe arrow colon equals append bind parallel
  send receive
  plus minus multiply divide
  lt gt lte gte eq neq
  dot
  
  %% Delimiters
  '{' '}' '[' ']' '(' ')' '|' ',' ';'
  
  %% Literals and identifiers
  integer float string atom
  identifier type_identifier
  
  %% Special
  eof
  .

%% ============================================================================
%% Rootsymbol
%% ============================================================================

Rootsymbol module.

%% ============================================================================
%% Precedence and Associativity
%% ============================================================================

%% Lowest precedence (binds loosest)
Right    100 arrow.           % Type-level function arrow (right-assoc)
Right    150 pipe.            % |> pipe operator
Right    160 bind.            % >>= Kleisli composition
Left     200 parallel.        % <|> parallel composition

%% Comparison operators
Nonassoc 300 eq neq.          % == /=
Nonassoc 310 lt gt lte gte.   % < > <= >=

%% Arithmetic and append
Left     400 plus minus.      % + -
Left     500 multiply divide. % * /
Right    550 append.          % <> (right-assoc for strings)

%% Application and access (highest precedence)
Left     600 dot.             % Record field access
Left     900 function_application.  % Function application (implicit)

%% ============================================================================
%% Grammar Rules
%% ============================================================================

%% Module structure
module -> declarations : #module{
    name = undefined,  % Will be set from file name
    exports = [],
    imports = [],
    declarations = '$1',
    location = {line, 1}
}.

declarations -> declaration : ['$1'].
declarations -> declaration declarations : ['$1' | '$2'].

declaration -> shape_decl : '$1'.
declaration -> flow_decl : '$1'.

%% Shape declarations
shape_decl -> shape type_identifier type_params equals constructors :
    #shape_decl{
        name = extract_atom('$2'),
        type_params = '$3',
        constructors = '$5',
        derives = [],
        location = extract_location('$1')
    }.

shape_decl -> shape type_identifier type_params equals constructors derives '[' type_list ']' :
    #shape_decl{
        name = extract_atom('$2'),
        type_params = '$3',
        constructors = '$5',
        derives = '$8',
        location = extract_location('$1')
    }.

type_params -> : [].
type_params -> identifier : [extract_atom('$1')].
type_params -> identifier type_params : [extract_atom('$1') | '$2'].

constructors -> constructor : ['$1'].
constructors -> constructor '|' constructors : ['$1' | '$3'].

constructor -> type_identifier : 
    #constructor{
        name = extract_atom('$1'),
        fields = [],
        location = extract_location('$1')
    }.

constructor -> type_identifier constructor_fields :
    #constructor{
        name = extract_atom('$1'),
        fields = '$2',
        location = extract_location('$1')
    }.

constructor -> '{' record_fields '}' :
    #constructor{
        name = record,
        fields = '$2',
        location = extract_location('$1')
    }.

constructor_fields -> type_expr_atomic : ['$1'].
constructor_fields -> type_expr_atomic constructor_fields : ['$1' | '$2'].

%% Flow declarations
flow_decl -> flow_signature flow_clauses :
    #flow_decl{
        name = extract_flow_name('$1'),
        type_sig = extract_flow_type('$1'),
        clauses = '$2',
        location = extract_location('$1')
    }.

flow_signature -> flow identifier colon type_expr :
    {flow_sig, extract_atom('$2'), '$4', extract_location('$1')}.

flow_clauses -> flow_clause : ['$1'].
flow_clauses -> flow_clause flow_clauses : ['$1' | '$2'].

flow_clause -> flow identifier pattern_list equals expr :
    #flow_clause{
        patterns = '$3',
        guards = undefined,
        body = '$5',
        location = extract_location('$1')
    }.

flow_clause -> flow identifier pattern_list when guards equals expr :
    #flow_clause{
        patterns = '$3',
        guards = '$5',
        body = '$7',
        location = extract_location('$1')
    }.

%% Match expressions
flow_clause -> flow identifier equals match match_clauses end :
    #flow_clause{
        patterns = [],
        guards = undefined,
        body = #match_expr{
            clauses = '$5',
            location = extract_location('$4')
        },
        location = extract_location('$1')
    }.

match_clauses -> match_clause : ['$1'].
match_clauses -> match_clause match_clauses : ['$1' | '$2'].

match_clause -> '|' pattern arrow expr :
    #match_clause{
        pattern = '$2',
        guards = undefined,
        body = '$4',
        location = extract_location('$1')
    }.

match_clause -> '|' pattern when guards arrow expr :
    #match_clause{
        pattern = '$2',
        guards = '$4',
        body = '$6',
        location = extract_location('$1')
    }.

guards -> guard : ['$1'].
guards -> guard ',' guards : ['$1' | '$3'].

guard -> expr : '$1'.

%% Type expressions
type_expr -> type_expr_atomic : '$1'.
type_expr -> type_expr arrow type_expr : 
    #type_fun{
        from = '$1',
        to = '$3',
        location = extract_location('$2')
    }.

type_expr -> forall type_params dot type_expr :
    #type_forall{
        type_vars = '$2',
        type = '$4',
        location = extract_location('$1')
    }.

type_expr_app -> type_expr_atomic : '$1'.
type_expr_app -> type_identifier type_expr_atomic :
    #type_app{
        constructor = #type_con{
            name = extract_atom('$1'),
            location = extract_location('$1')
        },
        args = ['$2'],
        location = extract_location('$1')
    }.

type_expr_atomic -> identifier : 
    #type_var{
        name = extract_atom('$1'),
        location = extract_location('$1')
    }.

type_expr_atomic -> type_identifier :
    #type_con{
        name = extract_atom('$1'),
        location = extract_location('$1')
    }.

type_expr_atomic -> '(' type_expr ')' : '$2'.

type_expr_atomic -> '{' type_record_fields '}' :
    #type_record{
        fields = '$2',
        row_var = undefined,
        location = extract_location('$1')
    }.

type_expr_atomic -> '{' type_record_fields '|' identifier '}' :
    #type_record{
        fields = '$2',
        row_var = extract_atom('$4'),
        location = extract_location('$1')
    }.

type_record_fields -> type_record_field : ['$1'].
type_record_fields -> type_record_field ',' type_record_fields : ['$1' | '$3'].

type_record_field -> identifier colon type_expr :
    {extract_atom('$1'), '$3'}.

%% Expressions
expr -> expr_atomic : '$1'.

expr -> let let_bindings in expr :
    #let_expr{
        bindings = '$2',
        body = '$4',
        location = extract_location('$1')
    }.

expr -> if expr then expr else expr :
    #if_expr{
        condition = '$2',
        then_branch = '$4',
        else_branch = '$6',
        location = extract_location('$1')
    }.

expr -> match match_clauses end :
    #match_expr{
        clauses = '$2',
        location = extract_location('$1')
    }.

expr -> expr pipe expr :
    #binary_op{
        op = pipe,
        left = '$1',
        right = '$3',
        location = extract_location('$2')
    }.

expr -> expr bind expr :
    #binary_op{
        op = bind,
        left = '$1',
        right = '$3',
        location = extract_location('$2')
    }.

expr -> expr append expr :
    #binary_op{
        op = append,
        left = '$1',
        right = '$3',
        location = extract_location('$2')
    }.

expr -> expr plus expr :
    #binary_op{
        op = plus,
        left = '$1',
        right = '$3',
        location = extract_location('$2')
    }.

expr -> expr minus expr :
    #binary_op{
        op = minus,
        left = '$1',
        right = '$3',
        location = extract_location('$2')
    }.

expr -> expr multiply expr :
    #binary_op{
        op = multiply,
        left = '$1',
        right = '$3',
        location = extract_location('$2')
    }.

expr -> expr divide expr :
    #binary_op{
        op = divide,
        left = '$1',
        right = '$3',
        location = extract_location('$2')
    }.

expr -> expr eq expr :
    #binary_op{
        op = eq,
        left = '$1',
        right = '$3',
        location = extract_location('$2')
    }.

expr -> expr neq expr :
    #binary_op{
        op = neq,
        left = '$1',
        right = '$3',
        location = extract_location('$2')
    }.

expr -> expr lt expr :
    #binary_op{
        op = lt,
        left = '$1',
        right = '$3',
        location = extract_location('$2')
    }.

expr -> expr gt expr :
    #binary_op{
        op = gt,
        left = '$1',
        right = '$3',
        location = extract_location('$2')
    }.

expr -> expr lte expr :
    #binary_op{
        op = lte,
        left = '$1',
        right = '$3',
        location = extract_location('$2')
    }.

expr -> expr gte expr :
    #binary_op{
        op = gte,
        left = '$1',
        right = '$3',
        location = extract_location('$2')
    }.

expr -> expr dot identifier :
    #record_access{
        record = '$1',
        field = extract_atom('$3'),
        location = extract_location('$2')
    }.

%% Function application (left-associative, implicit)
expr -> expr_app : '$1'.

expr_app -> expr_atomic : '$1'.
expr_app -> expr_app expr_atomic : 
    #app{
        func = '$1',
        args = ['$2'],
        location = extract_location('$1')
    } .

expr_atomic -> integer : 
    #literal{
        value = extract_value('$1'),
        type = integer,
        location = extract_location('$1')
    }.

expr_atomic -> float :
    #literal{
        value = extract_value('$1'),
        type = float,
        location = extract_location('$1')
    }.

expr_atomic -> string :
    #literal{
        value = extract_value('$1'),
        type = string,
        location = extract_location('$1')
    }.

expr_atomic -> identifier :
    #var{
        name = extract_atom('$1'),
        location = extract_location('$1')
    }.

expr_atomic -> type_identifier :
    #var{
        name = extract_atom('$1'),
        location = extract_location('$1')
    }.

expr_atomic -> '(' expr ')' : '$2'.

expr_atomic -> '{' record_fields '}' :
    #record_expr{
        fields = '$2',
        base = undefined,
        location = extract_location('$1')
    }.

expr_atomic -> '{' expr '|' record_fields '}' :
    #record_expr{
        fields = '$4',
        base = '$2',
        location = extract_location('$1')
    }.

record_fields -> record_field : ['$1'].
record_fields -> record_field ',' record_fields : ['$1' | '$3'].

record_field -> identifier colon expr :
    {extract_atom('$1'), '$3'}.

let_bindings -> let_binding : ['$1'].
let_bindings -> let_binding ',' let_bindings : ['$1' | '$3'].

let_binding -> pattern equals expr :
    {'$1', '$3'}.

%% Patterns
pattern -> pattern_atomic : '$1'.

pattern -> type_identifier pattern_list :
    #pat_constructor{
        name = extract_atom('$1'),
        args = '$2',
        location = extract_location('$1')
    }.

pattern -> identifier '@' pattern :
    #pat_as{
        name = extract_atom('$1'),
        pattern = '$3',
        location = extract_location('$1')
    }.

pattern_list -> pattern_atomic : ['$1'].
pattern_list -> pattern_atomic pattern_list : ['$1' | '$2'].

pattern_atomic -> identifier :
    #pat_var{
        name = extract_atom('$1'),
        location = extract_location('$1')
    }.

pattern_atomic -> type_identifier :
    #pat_constructor{
        name = extract_atom('$1'),
        args = [],
        location = extract_location('$1')
    }.

pattern_atomic -> integer :
    #pat_literal{
        value = extract_value('$1'),
        location = extract_location('$1')
    }.

pattern_atomic -> float :
    #pat_literal{
        value = extract_value('$1'),
        location = extract_location('$1')
    }.

pattern_atomic -> string :
    #pat_literal{
        value = extract_value('$1'),
        location = extract_location('$1')
    }.

pattern_atomic -> '_' :
    #pat_wildcard{
        location = extract_location('$1')
    }.

pattern_atomic -> '(' pattern ')' : '$2'.

pattern_atomic -> '{' pattern_record_fields '}' :
    #pat_record{
        fields = '$2',
        location = extract_location('$1')
    }.

pattern_record_fields -> pattern_record_field : ['$1'].
pattern_record_fields -> pattern_record_field ',' pattern_record_fields : ['$1' | '$3'].

pattern_record_field -> identifier colon pattern :
    {extract_atom('$1'), '$3'}.

pattern_record_field -> identifier :
    {extract_atom('$1'), #pat_var{
        name = extract_atom('$1'),
        location = extract_location('$1')
    }}.

%% ============================================================================
%% Erlang Code
%% ============================================================================

Erlang code.

-include("ast.hrl").

%% Extract atom from token
extract_atom({_Token, _Line, Atom}) when is_atom(Atom) -> Atom;
extract_atom({_Token, _Line, List}) when is_list(List) -> list_to_atom(List);
extract_atom({_Token, _Line}) -> undefined.

%% Extract value from token
extract_value({_Token, _Line, Value}) -> Value;
extract_value({_Token, _Line}) -> undefined.

%% Extract location from token
extract_location({_Token, Line}) -> {line, Line};
extract_location({_Token, Line, _Value}) -> {line, Line}.

%% Extract flow name from signature
extract_flow_name({flow_sig, Name, _Type, _Loc}) -> Name.

%% Extract flow type from signature
extract_flow_type({flow_sig, _Name, Type, _Loc}) -> Type.
```

---

## Implementation Steps Breakdown

### Subtask 1.1.2.1: Grammar for Shape Declarations

**Objective**: Define production rules for shape declarations with ADT constructors and record syntax.

**Syntax to Support**:
```catena
shape Maybe a = Some a | None
shape Result a b = Ok a | Error b
shape Point = { x: Float, y: Float }
shape User = { name: Text, age: Natural }
  derives [Eq, Show]
```

**Production Rules**:
```erlang
shape_decl -> shape type_identifier type_params equals constructors
shape_decl -> shape type_identifier type_params equals constructors derives derives_list

type_params -> (empty)
type_params -> identifier type_params

constructors -> constructor
constructors -> constructor '|' constructors

constructor -> type_identifier                      % Nullary: None
constructor -> type_identifier constructor_fields   % With args: Some a
constructor -> '{' record_fields '}'                % Record: { x: Float, ... }

constructor_fields -> type_expr_atomic
constructor_fields -> type_expr_atomic constructor_fields

derives_list -> '[' type_list ']'
type_list -> type_identifier
type_list -> type_identifier ',' type_list
```

**AST Construction**:
- Create `#shape_decl{}` record with all fields
- Parse type parameters as list of atoms
- Parse constructors as list of `#constructor{}` records
- Handle optional derives clause

**Test Cases**:
- Simple sum type: `shape Bool = True | False`
- Parametric type: `shape Maybe a = Some a | None`
- Multi-parameter: `shape Either a b = Left a | Right b`
- Record type: `shape Point = { x: Float, y: Float }`
- With derives: `shape User = { name: Text } derives [Eq]`
- Complex: Multiple constructors with multiple fields

**Implementation Plan**:
1. Add nonterminals: `shape_decl`, `constructors`, `constructor`, etc.
2. Add terminals from lexer: `shape`, `derives`, `type_identifier`, etc.
3. Write production rules with AST construction code
4. Test with simple examples
5. Add derives clause support
6. Test edge cases

### Subtask 1.1.2.2: Grammar for Flow Definitions

**Objective**: Define production rules for flow definitions with type signatures and pattern clauses.

**Syntax to Support**:
```catena
flow greet : User -> Text
flow greet user = "Hello, " <> user.name

flow map : (a -> b) -> List a -> List b
flow map f = match
  | Nil -> Nil
  | Cons x xs -> Cons (f x) (map f xs)
end

flow factorial : Natural -> Natural
flow factorial 0 = 1
flow factorial n = n * factorial (n - 1)
```

**Production Rules**:
```erlang
flow_decl -> flow_signature flow_clauses

flow_signature -> flow identifier colon type_expr

flow_clauses -> flow_clause
flow_clauses -> flow_clause flow_clauses

flow_clause -> flow identifier pattern_list equals expr
flow_clause -> flow identifier pattern_list when guards equals expr
flow_clause -> flow identifier equals match match_clauses end

guards -> guard
guards -> guard ',' guards
```

**AST Construction**:
- Create `#flow_decl{}` with name, type signature, and clauses
- Each clause is `#flow_clause{}` with patterns, guards, body
- Match expressions are `#match_expr{}` with list of `#match_clause{}`

**Test Cases**:
- Type signature only: `flow f : Int -> Int`
- Simple implementation: `flow id x = x`
- Multiple parameters: `flow add x y = x + y`
- Pattern matching: `flow isZero 0 = True | isZero _ = False`
- Match expression: `flow map f = match | Nil -> ... end`
- With guards: `flow abs n when n < 0 = -n`

**Implementation Plan**:
1. Define flow declaration nonterminals
2. Add type signature production
3. Add pattern clause productions
4. Add match expression support
5. Add guards support
6. Test all variations
7. Ensure clause ordering is preserved

### Subtask 1.1.2.3: Grammar for Expressions

**Objective**: Define production rules for expressions including composition, application, and let bindings.

**Syntax to Support**:
```catena
-- Literals
42
3.14
"hello"

-- Variables
x
user

-- Application
f x
map f xs

-- Binary operators
x + y
f |> g
a >>= b

-- Let bindings
let x = 42 in x + 1

-- If expressions
if x > 0 then x else -x

-- Record expressions
{ x: 1, y: 2 }
{ point | x: 3 }
point.x

-- Match expressions
match list
  | Nil -> 0
  | Cons x xs -> 1 + length xs
end
```

**Production Rules**:
```erlang
expr -> expr_atomic
expr -> let let_bindings in expr
expr -> if expr then expr else expr
expr -> match match_clauses end
expr -> expr binary_op expr           % Via precedence
expr -> expr expr_atomic              % Function application
expr -> expr dot identifier           % Record access

expr_atomic -> integer | float | string
expr_atomic -> identifier | type_identifier
expr_atomic -> '(' expr ')'
expr_atomic -> '{' record_fields '}'
expr_atomic -> '{' expr '|' record_fields '}'

let_bindings -> pattern equals expr
let_bindings -> pattern equals expr ',' let_bindings

record_fields -> identifier colon expr
record_fields -> identifier colon expr ',' record_fields
```

**AST Construction**:
- Literals: `#literal{value, type, location}`
- Variables: `#var{name, location}`
- Applications: `#app{func, args, location}`
- Binary ops: `#binary_op{op, left, right, location}`
- Let: `#let_expr{bindings, body, location}`
- If: `#if_expr{condition, then_branch, else_branch, location}`
- Records: `#record_expr{fields, base, location}`
- Access: `#record_access{record, field, location}`

**Test Cases**:
- All literal types
- Variable references
- Simple application: `f x`
- Multi-arg application: `f x y z`
- Infix operators: `x + y`, `f |> g`
- Let binding: `let x = 1 in x`
- Multiple let bindings: `let x = 1, y = 2 in x + y`
- If expression
- Record construction and update
- Record field access
- Nested expressions

**Implementation Plan**:
1. Define expr nonterminals hierarchy
2. Add atomic expressions (literals, vars)
3. Add function application (left-associative)
4. Add binary operators with precedence
5. Add let expressions
6. Add if expressions
7. Add record expressions
8. Test precedence correctness
9. Test complex nested expressions

### Subtask 1.1.2.4: Operator Precedence and Associativity

**Objective**: Define precedence and associativity tables ensuring correct parsing of complex expressions.

**Operator Precedence Design** (from lowest to highest):

```erlang
%% Precedence levels (lower number = lower precedence = binds looser)

Right    100 arrow.           % -> (type and value level)
Right    150 pipe.            % |>
Right    160 bind.            % >>=
Left     200 parallel.        % <|>

Nonassoc 300 eq neq.          % == /=
Nonassoc 310 lt gt lte gte.   % < > <= >=

Left     400 plus minus.      % + -
Left     500 multiply divide. % * /
Right    550 append.          % <>

Left     600 dot.             % . (record access)
Left     900 application.     % function application (implicit)
```

**Rationale**:

1. **Arrow (100)**: Lowest precedence for types
   - `a -> b -> c` parses as `a -> (b -> c)` (right-associative)
   - Allows `f : Int -> Int -> Int` to parse correctly

2. **Pipe (150)**: Next level for composition
   - `f x |> g |> h` parses as `(f x |> g) |> h`
   - Wait, pipe should be left-associative for chaining
   - Actually: `x |> f |> g` means `g (f x)`, so we need to be careful

   **Correction**: Pipe is typically left-associative:
   ```erlang
   Left 150 pipe.  % x |> f |> g = (x |> f) |> g
   ```

3. **Bind (160)**: Kleisli composition
   - `f >>= g >>= h` parses as `f >>= (g >>= h)` (right-associative)
   - Matches monadic bind associativity

4. **Comparison (300-310)**: Non-associative
   - `x < y < z` is a syntax error (as intended)
   - Forces explicit parentheses

5. **Arithmetic (400-500)**: Standard precedence
   - `x + y * z` parses as `x + (y * z)`

6. **Append (550)**: Right-associative
   - `"a" <> "b" <> "c"` parses as `"a" <> ("b" <> "c")`
   - Matches string concatenation semantics

7. **Dot (600)**: Record access
   - `user.address.city` parses left-to-right
   - Binds tighter than arithmetic

8. **Application (900)**: Highest precedence
   - `f x + g y` parses as `(f x) + (g y)`
   - Application binds tightest

**Test Cases for Precedence**:

```catena
%% Application vs operators
f x + g y                    % (f x) + (g y)
f x |> g                     % (f x) |> g
x + y |> f                   % (x + y) |> f

%% Pipe chaining
x |> f |> g |> h             % ((x |> f) |> g) |> h

%% Bind chaining
f >>= g >>= h                % f >>= (g >>= h)

%% Record access
user.address.city            % (user.address).city
f user.name                  % f (user.name)

%% Arithmetic
x + y * z                    % x + (y * z)
x * y + z                    % (x * y) + z

%% Comparison (should error)
x < y < z                    % ERROR: non-associative

%% Mixed operators
f x + g y * h z              % (f x) + ((g y) * (h z))
```

**Implementation Plan**:
1. Add precedence declarations in yecc file header
2. Set precedence for all binary operators
3. Test each precedence level individually
4. Test mixed expressions
5. Compile and check for shift/reduce conflicts
6. Resolve any conflicts with grammar refactoring or precedence adjustment
7. Document final precedence table
8. Create comprehensive test suite

---

## Success Criteria

This task is complete when:

1. **Complete grammar coverage**: Parser handles all Catena syntax elements:
   - Shape declarations (sum types, records, derives)
   - Flow declarations (signatures, patterns, guards, match)
   - All expression forms (literals, application, operators, let, if, records)
   - All pattern forms (variables, constructors, records, wildcards, as-patterns)
   - All type expressions (variables, constructors, functions, records, forall)

2. **Correct precedence**: Operator precedence and associativity produce correct ASTs:
   - `f x + g y` parses as `(f x) + (g y)`
   - `x |> f |> g` chains correctly
   - `a -> b -> c` is right-associative
   - No unexpected precedence conflicts

3. **Unambiguous grammar**: Yecc compiles without shift/reduce or reduce/reduce conflicts, or all conflicts are documented and justified.

4. **Well-formed ASTs**: Generated AST nodes:
   - Have consistent structure
   - Include location metadata
   - Are pattern-matchable
   - Accurately represent source semantics

5. **Comprehensive tests**: Test suite covers:
   - All grammar productions
   - Precedence and associativity
   - Edge cases and corner cases
   - Integration tests with realistic code
   - Minimum 90% code coverage

6. **Error reporting**: Parser produces clear error messages for:
   - Syntax errors
   - Unexpected tokens
   - Incomplete expressions
   - Provides line numbers and context

7. **Integration ready**: Generated ASTs are in the format expected by Task 1.1.3 (AST Construction) and Task 1.2 (Type Inference).

8. **Documentation complete**:
   - Grammar is documented with comments
   - AST node types are fully documented in `ast.hrl`
   - Precedence table is documented
   - Usage examples are provided

---

## Testing Strategy

### Unit Tests

**Test Organization**:
```erlang
-module(catena_parser_test).
-include_lib("eunit/include/eunit.hrl").
-include("ast.hrl").

%% Shape declaration tests
parse_simple_shape_test() ->
    Tokens = tokenize("shape Bool = True | False"),
    {ok, AST} = catena_parser:parse(Tokens),
    ?assertMatch(#module{declarations = [#shape_decl{}]}, AST).

%% Flow declaration tests
parse_simple_flow_test() ->
    Tokens = tokenize("flow id : a -> a\nflow id x = x"),
    {ok, AST} = catena_parser:parse(Tokens),
    ?assertMatch(#module{declarations = [#flow_decl{}]}, AST).

%% Expression tests
parse_application_test() ->
    Tokens = tokenize_expr("f x y"),
    {ok, Expr} = catena_parser:parse_expr(Tokens),
    ?assertMatch(#app{func = #app{func = #var{name = f}, args = [#var{name = x}]}, 
                      args = [#var{name = y}]}, Expr).

%% Precedence tests
parse_precedence_application_plus_test() ->
    Tokens = tokenize_expr("f x + g y"),
    {ok, Expr} = catena_parser:parse_expr(Tokens),
    ?assertMatch(#binary_op{
        op = plus,
        left = #app{func = #var{name = f}, args = [#var{name = x}]},
        right = #app{func = #var{name = g}, args = [#var{name = y}]}
    }, Expr).
```

**Test Coverage Goals**:
- Every production rule exercised
- Every operator precedence level tested
- Every AST node type constructed
- Error cases for malformed input

### Integration Tests

**Realistic Code Examples**:

```catena
-- Test 1: Maybe type with map
shape Maybe a = Some a | None

flow map : (a -> b) -> Maybe a -> Maybe b
flow map f = match
  | Some x -> Some (f x)
  | None -> None
end

-- Test 2: List operations
shape List a = Nil | Cons a (List a)

flow length : List a -> Natural
flow length = match
  | Nil -> 0
  | Cons _ xs -> 1 + length xs
end

-- Test 3: User record
shape User = { name: Text, age: Natural }

flow greet : User -> Text
flow greet user = "Hello, " <> user.name

-- Test 4: Complex expression
flow process : Data -> Result Success Error
flow process data =
  data
  |> validate
  |> transform
  |> persist
```

### Error Handling Tests

**Syntax Error Cases**:
```catena
-- Missing equals
shape Maybe a Some a | None

-- Missing arrow in type
flow f : Int Int

-- Unmatched parentheses
flow g = (x + y

-- Invalid pattern
flow h (x y) = x

-- Missing end
flow i = match | _ -> 0
```

**Expected Behavior**:
- Clear error message indicating problem
- Line number of error
- Suggestion for fix when possible

---

## Potential Challenges and Solutions

### Challenge 1: Shift/Reduce Conflicts

**Problem**: Function application is implicit (no operator), which can create ambiguity:
```catena
f x y       -- Could be: (f x) y  OR  f (x y)
```

**Solution**: 
- Use precedence levels to force left-associativity
- Define `expr_app` nonterminal for application
- Ensure application has higher precedence than all binary operators

### Challenge 2: Record Syntax Ambiguity

**Problem**: Record construction vs record update:
```catena
{ x: 1, y: 2 }           -- Construction
{ point | x: 3 }          -- Update
```

**Solution**:
- Use separate productions
- Check for `|` token to distinguish
- Generate different AST nodes

### Challenge 3: Pattern vs Expression Overlap

**Problem**: Constructors can appear in both patterns and expressions:
```catena
Some x          -- Pattern
Some value      -- Expression
```

**Solution**:
- Maintain separate `pattern` and `expr` nonterminals
- Accept overlap but generate appropriate AST nodes
- Type checker will validate usage

### Challenge 4: Type vs Expression Syntax

**Problem**: Function arrows appear in types and expressions:
```catena
f : Int -> Int      -- Type
f = \x -> x + 1     -- Lambda (future)
```

**Solution**:
- Separate `type_expr` nonterminals from `expr`
- Use context to distinguish (after `:` is type context)
- Different AST nodes prevent confusion

### Challenge 5: Nested Match Expressions

**Problem**: Match inside match:
```catena
flow f = match
  | Some x -> match x
    | 0 -> "zero"
    | _ -> "non-zero"
  end
end
```

**Solution**:
- Ensure `match_clause` body can be any `expr`
- Test nesting thoroughly
- Track nesting level for `end` keyword matching

---

## Notes and Considerations

### Design Decisions

1. **AST Structure**: Using Erlang records for type safety and pattern matching convenience.

2. **Location Tracking**: All AST nodes include `location :: {line, LineNum}` for error reporting.

3. **Precedence Strategy**: Using yecc's precedence declarations rather than grammar factoring for readability.

4. **Implicit Application**: Function application has no explicit operator, relies on juxtaposition and precedence.

5. **Module Structure**: For Phase 1, modules are simple (single file, minimal imports/exports).

### Future Enhancements (Not in This Task)

1. **Column Tracking**: Add column numbers to location metadata.

2. **Module System**: Full import/export with qualified names (Phase 4).

3. **Do Notation**: Monadic do-notation syntax.

4. **Lambda Expressions**: Explicit `\x -> expr` syntax.

5. **List Literals**: `[1, 2, 3]` syntax sugar.

6. **Advanced Patterns**: View patterns, as-patterns, or-patterns.

7. **Type Classes**: Trait definitions and instances.

8. **Effect System**: Effect annotations in types.

### Dependencies

**This task depends on**:
- Task 1.1.1: Token Recognition (COMPLETE) - provides token stream

**Tasks that depend on this**:
- Task 1.1.3: AST Construction - uses AST node definitions
- Task 1.1.4: Error Recovery - uses parser error information
- Task 1.2: Type Inference - traverses AST

---

## References

### Catena Language Specification
- `notes/research/1.01-original_idea/1.01-original_idea.md` - Core syntax
- `notes/research/1.10-pattern-matching/1.10-pattern-matching.md` - Pattern syntax
- `notes/research/1.15-advanced-type-system/1.15-advanced-type-system.md` - Type syntax
- `notes/research/1.02-modules/1.02-modules.md` - Module system
- `CLAUDE.md` - Repository guidance and conventions

### Yecc Documentation
- [Yecc Official Documentation](https://www.erlang.org/doc/apps/parsetools/yecc.html)
- [Yecc User's Guide](https://www.erlang.org/doc/apps/parsetools/yecc_guide.html)
- [Tutorial: Parsing with Yecc](https://andrealeopardi.com/posts/tokenizing-and-parsing-in-elixir-using-leex-and-yecc/)

### Parser Theory
- "Compilers: Principles, Techniques, and Tools" (Dragon Book) - Ch 4: Syntax Analysis
- "Modern Compiler Implementation in ML" - Ch 3: Parsing
- "Engineering a Compiler" - Ch 3: Parsers

### Related Phase 1 Tasks
- Task 1.1.1: Token Recognition (COMPLETE)
- Task 1.1.3: AST Construction (next)
- Task 1.1.4: Error Recovery and Reporting
- Task 1.2: Type Inference

---

## Revision History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2025-11-08 | Planning Agent | Initial document created |

---

**END OF DOCUMENT**
