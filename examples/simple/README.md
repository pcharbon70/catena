# Simple Catena Examples

This directory contains simple Catena programs to test the compiler.

## Files

- `identity.catena` - Basic functional programming primitives (identity, const, compose)

## Compiling

### Prerequisites

Make sure the compiler is built:

```bash
# From the project root
./scripts/build.sh
rebar3 compile
```

### Using the Erlang Shell

Start an Erlang shell with the compiled modules:

```bash
rebar3 shell
```

Then compile a Catena file:

```erlang
%% Read and compile a file
{ok, Result} = catena_compile:compile_file("examples/simple/identity.catena").

%% Or compile a string directly
{ok, Result} = catena_compile:compile_string("transform id x = x\n").
```

### Step-by-Step Compilation

To see each stage of compilation:

```erlang
%% 1. Read the source file
{ok, Binary} = file:read_file("examples/simple/identity.catena").
Source = binary_to_list(Binary).

%% 2. Lexical analysis (tokenization)
{ok, Tokens, _} = catena_lexer:string(Source).
io:format("Tokens: ~p~n", [Tokens]).

%% 3. Parsing (build AST)
{ok, AST} = catena_parser:parse(Tokens).
io:format("AST: ~p~n", [AST]).

%% 4. Semantic analysis (grouping, desugaring)
{ok, Analyzed} = catena_semantic:analyze(AST).
io:format("Analyzed: ~p~n", [Analyzed]).

%% 5. Full compilation (includes type checking)
{ok, TypedModule} = catena_compile:compile_string(Source).
io:format("Typed Module: ~p~n", [TypedModule]).
```

### Expected Output

For `transform id x = x`, the typed module should show:

```erlang
{typed_module, undefined,
  [{typed_transform, id, {tfun, {tvar, 1}, {tvar, 1}, {effect_set, []}}, ...}],
  TypeEnv}
```

This indicates `id` has type `α → α` (a polymorphic identity function) with no effects.

## Current Limitations

The compiler is in Phase 1, so:

- No code generation to BEAM bytecode yet
- No REPL
- Type signatures in source have limited support
- Match expressions need more parser work

The compiler currently produces a typed AST, which can be inspected but not executed.
