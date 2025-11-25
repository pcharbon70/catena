# Phase 2.3 Basic Testing Framework Implementation

## Status: IN PROGRESS

## Problem Statement

Phase 2.3 requires implementing a basic testing framework for Catena that includes:

1. **Unit testing** with `test` keyword and assertion functions
2. **Property-based testing** with `property` keyword and generators
3. **Test reporting** with clear pass/fail status

This enables developers to test their Catena code directly in the language, establishing first-class testing support.

## Current State Analysis

### Existing Infrastructure
- Lexer (`catena_lexer.xrl`) - supports keywords, operators, identifiers
- Parser (`catena_parser.yrl`) - supports declarations, expressions, patterns
- REPL (`catena_repl.erl`) - interactive evaluation environment
- Prelude (`catena_prelude.erl`) - standard library functions
- Type system - HM type inference with effects

### What Needs to Be Added
- `test` keyword to lexer/parser
- `property` keyword to lexer/parser
- `forall` syntax for property quantification (already exists in lexer)
- Test execution framework
- Assertion functions
- Property generators
- Test runner and reporting

## Implementation Plan

### 2.3.1 Test Keyword and Assertions

#### 2.3.1.1 Add `test` Keyword to Lexer and Parser

Lexer changes (`catena_lexer.xrl`):
```erlang
%% Testing keywords
test : {token, {test, TokenLine}}.
```

Parser changes (`catena_parser.yrl`):
```erlang
%% Add to Terminals
test

%% Add to Nonterminals
test_decl

%% Add to declaration
declaration -> test_decl : '$1'.

%% Test declaration: test "name" = expr
test_decl -> test string equals expr :
    {test_decl,
        extract_value('$2'),
        '$4',
        extract_location('$1')}.
```

AST node format:
```erlang
{test_decl, Name :: string(), Body :: expr(), Location}
```

#### 2.3.1.2 Implement Test Execution Framework

Create `src/testing/catena_test_runner.erl`:
- `run_tests/1` - run all tests in a module
- `run_test/1` - run a single test
- `collect_results/1` - aggregate pass/fail results

Test execution flow:
1. Collect all `test_decl` nodes from parsed AST
2. Evaluate each test expression
3. Check if result is `true` (pass) or `false` (fail)
4. Report results

#### 2.3.1.3 Implement Assertion Functions

Add to `catena_prelude.erl`:
```erlang
%% assert : Bool -> Unit
%% Succeeds if condition is true, throws on false
assert(true) -> ok;
assert(false) -> throw({assertion_failed, "Assertion failed"}).

%% assert_equals : a -> a -> Unit
%% Succeeds if values are equal, throws with actual/expected on mismatch
assert_equals(Expected, Actual) when Expected =:= Actual -> ok;
assert_equals(Expected, Actual) ->
    throw({assertion_failed, {expected, Expected, actual, Actual}}).

%% assert_not_equals : a -> a -> Unit
assert_not_equals(X, Y) when X =/= Y -> ok;
assert_not_equals(X, Y) ->
    throw({assertion_failed, {should_not_equal, X, Y}}).
```

#### 2.3.1.4 Implement Test Reporting

Test report format:
```
Running tests...

  ✓ test "addition works"
  ✓ test "list operations"
  ✗ test "failing test"
      Assertion failed: expected 5, actual 6
      at line 42

Results: 2 passed, 1 failed (3 total)
```

### 2.3.2 Property Testing

#### 2.3.2.1 Add `property` Keyword and `forall` Syntax

Lexer changes:
```erlang
property : {token, {property, TokenLine}}.
%% forall already exists in lexer
```

Parser changes:
```erlang
%% Add to Terminals
property

%% Add to Nonterminals
property_decl property_body property_binding property_bindings

%% Property declaration
property_decl -> property string equals property_body :
    {property_decl,
        extract_value('$2'),
        '$4',
        extract_location('$1')}.

%% Property body: forall x : Gen, y : Gen. expr
property_body -> forall property_bindings dot expr :
    {property_forall, '$2', '$4', extract_location('$1')}.

property_bindings -> property_binding :
    ['$1'].
property_bindings -> property_binding comma property_bindings :
    ['$1' | '$3'].

property_binding -> lower_ident colon upper_ident :
    {extract_atom('$1'), extract_atom('$3')}.
```

AST node format:
```erlang
{property_decl, Name :: string(), Body :: property_body(), Location}
{property_forall, Bindings :: [{atom(), atom()}], Expr :: expr(), Location}
```

#### 2.3.2.2 Implement Basic Generators

Create `src/testing/catena_generators.erl`:
```erlang
%% Generator for Bool
gen_bool() -> rand:uniform() < 0.5.

%% Generator for Natural (0-100)
gen_natural() -> rand:uniform(101) - 1.

%% Generator for Text (random strings up to 20 chars)
gen_text() ->
    Len = rand:uniform(21) - 1,
    [rand:uniform(26) + 96 || _ <- lists:seq(1, Len)].

%% Generator for List (random length 0-20, elements from sub-generator)
gen_list(ElemGen) ->
    Len = rand:uniform(21) - 1,
    [ElemGen() || _ <- lists:seq(1, Len)].
```

#### 2.3.2.3 Implement Property Test Runner

Create `src/testing/catena_property_runner.erl`:
```erlang
%% Run property test with N iterations (default 100)
run_property(PropertyDecl, NumTests) ->
    {property_decl, Name, {property_forall, Bindings, Expr, _}, _} = PropertyDecl,
    Results = [run_single_property(Bindings, Expr) || _ <- lists:seq(1, NumTests)],
    case lists:filter(fun({fail, _}) -> true; (_) -> false end, Results) of
        [] -> {pass, Name, NumTests};
        [FirstFailure | _] -> {fail, Name, FirstFailure}
    end.

run_single_property(Bindings, Expr) ->
    %% Generate values for each binding
    Values = [{Name, generate(Gen)} || {Name, Gen} <- Bindings],
    %% Evaluate expression with generated values
    try
        case evaluate_with_bindings(Expr, Values) of
            true -> pass;
            false -> {fail, Values}
        end
    catch
        _:_ -> {fail, Values}
    end.
```

#### 2.3.2.4 Implement Property Failure Reporting

Property failure report format:
```
Property "commutativity of addition" FAILED after 47 tests:

  Counterexample:
    x = 42
    y = 17

  Property: x + y == y + x
  Result: false
```

## Files to Create

| File | Purpose |
|------|---------|
| `src/testing/catena_test_runner.erl` | Unit test execution |
| `src/testing/catena_generators.erl` | Property test generators |
| `src/testing/catena_property_runner.erl` | Property test execution |
| `test/testing/catena_test_runner_tests.erl` | Tests for test runner |
| `test/testing/catena_generators_tests.erl` | Tests for generators |
| `test/testing/catena_property_runner_tests.erl` | Tests for property runner |

## Files to Modify

| File | Changes |
|------|---------|
| `src/compiler/lexer/catena_lexer.xrl` | Add `test`, `property` keywords |
| `src/compiler/parser/catena_parser.yrl` | Add test_decl, property_decl grammar |
| `src/stdlib/catena_prelude.erl` | Add assertion functions |
| `src/repl/catena_repl.erl` | Add `:test` command to run tests |

## Test Cases

### Unit Test Framework Tests
- Parse test declaration correctly
- Run passing test, verify success
- Run failing test, verify failure and message
- Run multiple tests, verify aggregate results
- Test assertion functions work correctly

### Property Test Framework Tests
- Parse property declaration correctly
- Generator produces valid Bool values
- Generator produces valid Natural values (0-100)
- Generator produces valid Text values
- Generator produces valid List values
- Property runner executes correct number of iterations
- Property failure shows counterexample

## Success Criteria

1. ✅ `test` keyword recognized by lexer and parser
2. ✅ Test declarations produce correct AST nodes
3. ✅ `assert` and `assert_equals` functions work
4. ✅ Test runner executes tests and reports results
5. ✅ `property` keyword recognized by lexer and parser
6. ✅ Property declarations with `forall` produce correct AST nodes
7. ✅ Basic generators work for Bool, Natural, Text, List
8. ✅ Property runner executes 100 iterations per property
9. ✅ Property failures show counterexamples
10. ✅ All tests pass
