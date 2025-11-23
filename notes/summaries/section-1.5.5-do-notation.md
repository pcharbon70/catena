# Section 1.5.5 - Do-Notation Desugaring

## Summary

Implemented do-notation as syntactic sugar for monadic composition. Do-blocks parse to `do_expr` AST nodes and desugar to explicit `chain` (bind) calls. This provides a familiar, readable syntax for sequencing monadic operations while maintaining the Pipeline trait's underlying semantics.

## Completed Tasks

### 1.5.5.1 Parse do-block syntax ✅
- Syntax: `do { x <- ma; y <- mb; pure (x + y) }`
- Tests: `parse_do_block_test`, `parse_do_bind_test`

### 1.5.5.2 Desugar bind ✅
- `x <- ma; rest` becomes `chain (fn x -> rest) ma`
- Test: `desugar_bind_test`

### 1.5.5.3 Desugar sequence ✅
- `ma; rest` becomes `chain (fn _ -> rest) ma`
- Test: `desugar_action_test`

### 1.5.5.4 Desugar let in do ✅
- `let x = e; rest` becomes `let x = e in rest`
- Tests: `parse_do_let_test`, `desugar_let_test`

### 1.5.5.5 Verify desugared code ✅
- Desugared code produces correct AST structure with `chain` calls
- Test: `desugar_complex_do_test`

## Implementation

### Lexer Changes (`src/compiler/lexer/catena_lexer.xrl`)

Added `do` keyword:
```erlang
do : {token, {'do', TokenLine}}.
```

### Parser Changes (`src/compiler/parser/catena_parser.yrl`)

New nonterminals:
- `do_expr` - Top-level do expression
- `do_statements` - List of statements
- `do_statement` - Individual statement (bind, let, action)

Grammar rules:
```
do_expr -> 'do' lbrace do_statements rbrace

do_statements -> expr                            % Final return
do_statements -> do_statement semicolon do_statements

do_statement -> lower_ident left_arrow expr      % Bind: x <- ma
do_statement -> 'let' lower_ident equals expr    % Let: let x = e
do_statement -> expr                             % Action: ma
```

AST node types:
- `{do_expr, Statements, Loc}` - Do block
- `{do_bind, Var, Expr, Loc}` - Bind statement
- `{do_let, Var, Expr, Loc}` - Let statement
- `{do_action, Expr, Loc}` - Action statement
- `{do_return, Expr, Loc}` - Final expression

### Desugaring Module (`src/compiler/semantic/catena_desugar.erl`)

New module with complete AST transformation:

```erlang
-export([desugar/1, desugar_expr/1, desugar_do_expr/1]).

%% Transformation rules:
%% do { x <- ma; rest } => chain (fn x -> rest) ma
%% do { ma; rest }      => chain (fn _ -> rest) ma
%% do { let x = e; rest } => let x = e in rest
%% do { expr }          => expr
```

Key functions:
- `desugar/1` - Desugar entire AST (module or declarations)
- `desugar_expr/1` - Recursively desugar expressions
- `desugar_do_expr/1` - Convert do-block to bind chains

### Tests Added

8 new tests in `catena_stdlib_tests.erl`:

1. `parse_do_block_test` - Basic do-block parsing
2. `parse_do_bind_test` - Multiple bind statements
3. `parse_do_let_test` - Let in do-block
4. `parse_do_action_test` - Action (sequence) in do-block
5. `desugar_bind_test` - Bind desugars to chain
6. `desugar_let_test` - Let desugars to let_expr
7. `desugar_action_test` - Action desugars to chain with wildcard
8. `desugar_complex_do_test` - Complex multi-bind desugaring

## Test Results

### catena_stdlib_tests: 55/56 pass
- All 8 new do-notation tests pass
- Only failure: `parse_trait_default_impl_test` (pre-existing parser limitation)

## Technical Notes

### Desugaring Transformation

The desugaring follows standard Haskell/ML conventions:

```catena
-- Original
do {
  x <- getLine;
  let y = process x;
  print y;
  pure y
}

-- Desugared
chain (fn x ->
  let y = process x in
    chain (fn _ ->
      pure y
    ) (print y)
) getLine
```

### Parser Conflicts

Adding do-notation increased shift/reduce conflicts from 17 to 36. This is expected because `do_statement -> expr` creates ambiguity (any expression can be either an action or the final return). The conflicts resolve correctly via shift preference.

### Pipeline Trait Integration

The desugared code uses `chain` from the Pipeline trait:
```catena
trait Pipeline m : Applicator m where
  chain : (a -> m b) -> m a -> m b
end
```

This ensures do-notation works uniformly for all monadic types (Maybe, List, Either, IO, etc.).

### Usage Example

```catena
-- Monadic computation with Maybe
transform safeDivide x y =
  do {
    result <- validate y;
    let quotient = x / result;
    pure quotient
  }

-- Equivalent desugared code
transform safeDivide x y =
  chain (fn result ->
    let quotient = x / result in
      pure quotient
  ) (validate y)
```

## Branch

`feature/section-1.5.5-error-messages` (branch name was auto-generated, actual feature is do-notation)

## Files Created/Modified

- `src/compiler/lexer/catena_lexer.xrl` - Added `do` keyword
- `src/compiler/parser/catena_parser.yrl` - Added do-block grammar (~50 lines)
- `src/compiler/semantic/catena_desugar.erl` - New desugaring module (142 lines)
- `test/compiler/integration/catena_stdlib_tests.erl` - Added 8 do-notation tests
