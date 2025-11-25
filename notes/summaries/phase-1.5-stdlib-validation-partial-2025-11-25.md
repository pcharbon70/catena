# Phase 1.5 Standard Library Validation (Partial) - November 25, 2025

## Overview

This session implemented foundational type inference extensions and desugaring for Phase 1.5 (Standard Library Validation). The work focused on:

1. Extending the type inference system to handle expressions commonly used in the standard library
2. Implementing operator desugaring (Section 1.5.7) for category theory operators
3. Verifying do-notation desugaring (Section 1.5.5) works correctly

## Files Modified

### Source Code
- `src/compiler/types/catena_infer_expr.erl` - Added inference for:
  - Binary operators (arithmetic, comparison, boolean, equality, pipe)
  - List literals `[a, b, c]`
  - Cons expressions `h :: t`
  - Match expressions with pattern clauses
  - Constructor application
  - Alternative literal/identifier AST formats

- `src/compiler/types/catena_type_error.erl` - Added:
  - `unknown_operator/1` error constructor
  - Error formatting for unknown operators

- `src/compiler/semantic/catena_desugar.erl` - Added operator desugaring:
  - `<$>` (fmap) -> `map f x`
  - `<*>` (ap) -> `apply f x`
  - `>>=` (bind) -> `chain f m`
  - `<>` (mappend) -> `combine a b`
  - `===` (setoid_eq) -> `equals a b`
  - `!==` (setoid_neq) -> `not (equals a b)`

### Tests
- `test/compiler/types/catena_stdlib_validation_tests.erl` (NEW - 367 lines)
  - 25 unit tests covering new inference functionality
  - Documents placeholder tests for future work (trait resolution, HKT)

- `test/compiler/semantic/catena_desugar_properties.erl` (EXTENDED)
  - Added `prop_operator_desugaring_never_crashes/0`
  - Added 11 unit tests for operator desugaring

## Implementation Details

### Binary Operator Inference

Added type inference for all binary operators in the parser:

```erlang
%% Arithmetic: +, -, *, /
%% Both operands must unify, result is same type
infer_binary_op(Op, LeftType, RightType, State) when Op =:= plus; ...

%% Comparison: <, >, <=, >=
%% Both operands must unify, result is Bool
infer_binary_op(Op, LeftType, RightType, State) when Op =:= lt; ...

%% Equality: ==, /=, ===, !==
%% Both operands must unify, result is Bool

%% Boolean: and, or
%% Both operands must be Bool, result is Bool

%% List append: ++
%% Both operands must be same list type, result is same

%% Pipe: |>
%% x |> f desugars to f(x), result type from function return
```

### List Literal Inference

```erlang
%% Empty list: polymorphic
[] : List a  where a is fresh type variable

%% Non-empty list
[e1, e2, ..., en] : List T  where all elements unify to T

%% List type representation
{tapp, {tcon, list}, [ElemType]}
```

### Cons Expression Inference

```erlang
%% h :: t : List a  where h : a and t : List a
infer({cons, Head, Tail, _Loc}, ...) ->
    %% Infer head type
    %% Infer tail type
    %% Unify tail with List HeadType
    %% Return List HeadType
```

### Match Expression Inference

```erlang
%% match scrutinee of | p1 -> e1 | ... | pn -> en end : T
%% All branches must return same type T
infer_match(Scrutinee, Clauses, Env, State) ->
    %% Infer scrutinee type
    %% Generate fresh result type
    %% For each clause:
    %%   - Infer pattern type, get bindings
    %%   - Unify pattern type with scrutinee type
    %%   - Extend environment with pattern bindings
    %%   - Infer body type, unify with result type
```

### Operator Desugaring (Section 1.5.7)

Category theory operators are now desugared to trait method calls:

| Operator | Desugars to | Trait |
|----------|-------------|-------|
| `<$>` (fmap) | `map f x` | Mapper |
| `<*>` (ap) | `apply f x` | Applicator |
| `>>=` (bind) | `chain f m` | Chainable |
| `<>` (mappend) | `combine a b` | Combiner |
| `===` (setoid_eq) | `equals a b` | Comparable |
| `!==` (setoid_neq) | `not (equals a b)` | Comparable |

Note: `>>=` swaps operand order: `m >>= f` becomes `chain f m`.

### Do-Notation Desugaring (Section 1.5.5)

Already implemented in `catena_desugar.erl`:

```erlang
%% x <- ma; rest  =>  chain (fn x -> rest) ma
%% ma; rest       =>  chain (fn _ -> rest) ma
%% let x = e; rest => let x = e in rest
```

## Test Results

- All 25 stdlib validation tests pass
- All 18 desugar property/unit tests pass
- 2 pre-existing test failures in `catena_type_subst_occurs_tests` (unrelated)

## What Was Implemented

### Section 1.5.1 - Basic Expression Inference (Partial)
- Binary operator type inference for all operators
- List literal and cons expression type inference
- Match expression type inference with pattern bindings
- Guards in match clauses
- Error handling for type mismatches

### Section 1.5.5 - Do-Notation Desugaring (Complete)
- Previously implemented and verified working
- Depth limit enforcement (MAX_DO_DEPTH = 1000)
- All statement types handled: bind, action, let, return

### Section 1.5.7 - Operator Desugaring (Complete)
- `operator_to_method/1` maps operators to trait methods
- `desugar_operator/4` generates correct AST
- Integrated into `desugar_expr/1` recursive descent
- Nested desugaring works correctly

## What Remains (Deferred to Future Work)

### Section 1.5.2 - Trait Instance Resolution
- Resolve Mapper instance for Maybe/List when type-checking `map f x`
- Resolve constrained instances like `Comparable a => Comparable (List a)`
- Verify trait hierarchy resolution
- Note: `catena_instance.erl` has infrastructure, needs integration

### Section 1.5.3 - Higher-Kinded Type Validation
- Kind checking for `trait Mapper f where map : (a -> b) -> f a -> f b`
- Kind inference for instance declarations
- Partially applied type constructors

### Section 1.5.4 - Law Verification via Test Module
- Property-based testing for trait laws
- Integration with PropEr

### Section 1.5.6 - Effect Integration with Kleisli Arrows
- Kleisli composition `>=>` with effect tracking
- Effect union in composition

## Git Branch

Branch: `feature/phase-1.5-stdlib-validation`

## API Summary

### New Expression Inference Cases

```erlang
%% Binary operators
infer({binary_op, Op, Left, Right, Loc}, Env, State)

%% List literals
infer({list, Elements, Loc}, Env, State)

%% Cons expressions
infer({cons, Head, Tail, Loc}, Env, State)

%% Match expressions
infer({match, Scrutinee, Clauses}, Env, State)
infer({'match', Scrutinee, Clauses, Loc}, Env, State)

%% Constructor application
infer({constructor, Name, Args, Loc}, Env, State)

%% Alternative AST forms
infer({literal, Type, Value, Loc}, Env, State)
infer({identifier, Name, Loc}, Env, State)
```

### Desugaring API

```erlang
%% Main entry point - desugars entire AST
catena_desugar:desugar(AST) -> DesugaredAST

%% Expression desugaring (recursive)
catena_desugar:desugar_expr(Expr) -> DesugaredExpr

%% Do-notation specific
catena_desugar:desugar_do_expr(DoExpr) -> DesugaredExpr

%% Operator desugaring (Section 1.5.7)
catena_desugar:operator_to_method(Op) -> {ok, Method} | not_desugared
catena_desugar:desugar_operator(Method, Left, Right, Loc) -> AppExpr
```

### Type Representations

```erlang
%% List type
{tapp, {tcon, list}, [ElemType]}

%% Function type (with effects)
{tfun, ArgType, ReturnType, {effect_set, Effects}}
```

## Notes for Future Development

1. **Trait Resolution**: The current implementation doesn't resolve trait instances. After desugaring `===` to `equals a b`, the type checker needs to find the `Comparable` instance for the type of `a`.

2. **Kind System**: Higher-kinded type validation requires a kind checking pass that's not yet implemented.

3. **Compiler Pipeline**: The desugaring pass should run between parsing and type checking. Current pipeline:
   - Parse -> AST
   - Desugar -> Core AST (operators and do-notation expanded)
   - Type Check -> Typed AST

4. **Full Stdlib Compilation**: Parsing and type-checking `prelude.cat` requires trait resolution plus module system support.

5. **REPL Integration**: The REPL can now type-check more expressions but still needs prelude function bindings in its environment.
