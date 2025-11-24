# Catena Compiler Pipeline Architecture

This document describes the compilation pipeline for the Catena language. The pipeline transforms source code through several stages, each with specific responsibilities.

## Pipeline Overview

```
Source Code
    │
    ▼
┌─────────────┐
│   Lexer     │  catena_lexer
└─────────────┘
    │ Tokens
    ▼
┌─────────────┐
│   Parser    │  catena_parser
└─────────────┘
    │ AST
    ▼
┌─────────────┐
│  Semantic   │  catena_semantic
│  Analysis   │  catena_desugar
└─────────────┘
    │ Desugared AST
    ▼
┌─────────────┐
│    Kind     │  catena_kind
│  Checking   │
└─────────────┘
    │ Kind-validated AST
    ▼
┌─────────────┐
│    Type     │  catena_infer
│  Checking   │  catena_compile
└─────────────┘
    │ Typed AST
    ▼
┌─────────────┐
│   Code      │  catena_codegen_*
│ Generation  │
└─────────────┘
    │
    ▼
BEAM Bytecode
```

## Stage 1: Lexical Analysis

**Module**: `catena_lexer` (generated from `catena_lexer.xrl`)

**Entry Point**: `catena_lexer:string/1`

**Input**: Source code string

**Output**: `{ok, Tokens, EndLocation}` or `{error, ErrorInfo, EndLocation}`

### Responsibilities

- Tokenize source code into lexemes
- Handle comments (single-line `--` and multi-line `{- -}`)
- Recognize keywords: `type`, `transform`, `effect`, `trait`, `instance`, `match`, `do`, `let`, `fn`, etc.
- Recognize operators: `|>`, `>>=`, `<$>`, `<*>`, `<>`, `===`, `!==`, etc.
- Track source locations for error reporting
- Apply depth limits for nested comments (DoS protection)

### Example

```erlang
{ok, Tokens, _} = catena_lexer:string("transform add x y = x + y\n").
%% Tokens = [{transform,1}, {lower_ident,1,"add"}, {lower_ident,1,"x"}, ...]
```

## Stage 2: Parsing

**Module**: `catena_parser` (generated from `catena_parser.yrl`)

**Entry Point**: `catena_parser:parse/1`

**Input**: Token list from lexer

**Output**: `{ok, AST}` or `{error, {Line, Module, Message}}`

### Responsibilities

- Build Abstract Syntax Tree from tokens
- Enforce grammar rules
- Handle operator precedence
- Produce structured AST nodes for:
  - Module declarations
  - Type declarations
  - Transform declarations (functions)
  - Trait and instance declarations
  - Effect declarations
  - Expressions, patterns, and types

### AST Node Types

- `{module, Name, Exports, Imports, Declarations, Location}`
- `{type_decl, Name, TypeVars, Constructors, Derives, Location}`
- `{transform_decl, Name, TypeSig, Clauses, Location}`
- `{transform_clause, Patterns, Guards, Body, Location}`
- `{lambda, Params, Body, Location}`
- `{app, Function, Args, Location}`
- `{do_expr, Statements, Location}`
- `{match_expr, Scrutinee, Clauses, Location}`

### Example

```erlang
{ok, Tokens, _} = catena_lexer:string("transform id x = x\n"),
{ok, AST} = catena_parser:parse(Tokens).
%% AST = {module, undefined, [], [], [{transform_decl, id, undefined, [...], _}], _}
```

## Stage 3: Semantic Analysis

**Modules**: `catena_semantic`, `catena_desugar`

**Entry Point**: `catena_semantic:analyze/1`

**Input**: Parsed AST

**Output**: `{ok, AnalyzedAST}` or `{error, SemanticError}`

### Responsibilities

#### catena_semantic

- **Transform Grouping**: Merge consecutive transform clauses with the same name
- **Signature Validation**: Ensure signatures are paired with implementations
- **Duplicate Detection**: Detect duplicate type signatures
- **Clause Validation**: Verify clause structure

#### catena_desugar

- **Do-Notation Desugaring**: Convert do-blocks to explicit bind chains

```catena
do { x <- ma; f x }
```
becomes:
```catena
chain(fn x -> f(x))(ma)
```

- **Statement Type Handling**:
  - `do_bind`: `x <- ma` → `chain (fn x -> ...) ma`
  - `do_action`: `action;` → `chain (fn _ -> ...) action`
  - `do_let`: `let x = e;` → `let x = e in ...`
  - `do_return`: `pure e` → `e`

### Example

```erlang
{ok, AST} = catena_parser:parse(Tokens),
{ok, Analyzed} = catena_semantic:analyze(AST).
%% Do-expressions are now desugared to chain/bind calls
```

## Stage 4: Kind Checking

**Module**: `catena_kind`

**Entry Points**: `build_kind_env/1`, `validate_hkt/2`

**Input**: Analyzed AST, Kind environment

**Output**: `{ok, KindEnv}` or `{error, KindErrors}`

### Responsibilities

- **Kind Environment**: Build mapping from type names to their kinds
- **HKT Validation**: Validate higher-kinded type usage
- **Kind Inference**: Infer kinds for type declarations

### Kind System

- `*` (star): Kind of fully-applied types (e.g., `Int`, `Bool`, `List Int`)
- `* -> *`: Kind of type constructors with one parameter (e.g., `List`, `Maybe`)
- `* -> * -> *`: Kind of type constructors with two parameters (e.g., `Either`)
- `(* -> *) -> *`: Higher-kinded types (e.g., type of `Functor`)

### Example

```erlang
KindEnv = catena_kind:build_kind_env(Declarations),
{ok, _} = catena_kind:validate_hkt(Declarations, KindEnv).
```

## Stage 5: Type Checking

**Modules**: `catena_compile`, `catena_infer`, `catena_infer_expr`, `catena_infer_pattern`, `catena_infer_effect`

**Entry Point**: `catena_compile:compile_string/1` (full pipeline) or internal `type_check/1`

**Input**: Kind-validated AST

**Output**: `{ok, TypedModule}` or `{error, TypeError}`

### Responsibilities

#### catena_compile

- **Pipeline Orchestration**: Coordinate all compilation stages
- **Type Environment**: Build initial type environment with builtins and declarations
- **Declaration Processing**: Type check each declaration

#### catena_infer (and sub-modules)

- **Type Inference**: Hindley-Milner type inference with effects
- **Unification**: Unify types during inference (`catena_infer_unify`)
- **Pattern Typing**: Infer types for patterns (`catena_infer_pattern`)
- **Effect Tracking**: Track and validate effects (`catena_infer_effect`)

### Type Environment

Contains type schemes for:
- Built-in operators: `++`, `&&`, `||`, `::`, `[]`
- Type constructors: `None`, `Some`, `Left`, `Right`, etc.
- Transform signatures
- Inferred transform types

### Example

```erlang
{ok, TypedModule} = catena_compile:compile_string(Source).
%% TypedModule = {typed_module, Name, TypedDeclarations, TypeEnv}
```

## Stage 6: Code Generation

**Modules**: `catena_codegen_module`, `catena_codegen_expr`, `catena_codegen_pattern`, `catena_codegen_erase`

**Entry Point**: `catena_codegen_module:generate/1`

**Input**: Typed AST

**Output**: Erlang abstract forms or Core Erlang

### Responsibilities

- **Type Erasure**: Remove type annotations (`catena_codegen_erase`)
- **Expression Translation**: Convert Catena expressions to Erlang (`catena_codegen_expr`)
- **Pattern Translation**: Convert patterns (`catena_codegen_pattern`)
- **Module Generation**: Generate complete Erlang module (`catena_codegen_module`)

## Supporting Modules

### Type System

- `catena_types`: Core type representation
- `catena_type_scheme`: Polymorphic type schemes
- `catena_type_env`: Type environments
- `catena_type_subst`: Type substitutions
- `catena_type_pp`: Type pretty printing

### Traits and Instances

- `catena_trait_hierarchy`: Trait superclass relationships
- `catena_instance`: Instance resolution
- `catena_coherence`: Instance coherence checking
- `catena_constraint`: Constraint solving

### Error Handling

- `catena_error`: Error representation
- `catena_error_formatter`: Error message formatting
- `catena_type_error`: Type error types
- `catena_type_error_formatter`: Type error formatting
- `catena_type_error_explain`: Detailed error explanations

### Utilities

- `catena_ast_pp`: AST pretty printing (for debugging and roundtrip testing)
- `catena_ast_utils`: AST traversal utilities
- `catena_location`: Source location handling
- `catena_config`: Compiler configuration

## Full Pipeline Example

```erlang
%% Source code
Source = "transform double x = x + x\n",

%% Stage 1: Lexing
{ok, Tokens, _} = catena_lexer:string(Source),

%% Stage 2: Parsing
{ok, AST} = catena_parser:parse(Tokens),

%% Stage 3: Semantic Analysis
{ok, Analyzed} = catena_semantic:analyze(AST),

%% Stage 4: Kind Checking (embedded in type_check)
%% Stage 5: Type Checking
{ok, Typed} = catena_compile:compile_string(Source).

%% Or use the unified entry point:
{ok, TypedModule} = catena_compile:compile_string(Source).
```

## Error Handling

Each stage can produce errors that halt compilation:

1. **Lexer Errors**: Invalid tokens, unterminated strings/comments
2. **Parser Errors**: Syntax errors, unexpected tokens
3. **Semantic Errors**: Duplicate signatures, empty transforms
4. **Kind Errors**: Kind mismatches, invalid HKT usage
5. **Type Errors**: Type mismatches, unbound variables, effect violations

Errors include source locations for reporting.

## DoS Protection

The compiler includes protections against malicious or malformed input:

- **Recursion Depth Limits**: Prevent stack overflow from deeply nested structures
- **Comment Nesting Limits**: Prevent resource exhaustion from nested comments
- **Constraint Size Limits**: Limit constraint set size during type inference

See `catena_config` and `catena_type_config` for configuration options.
