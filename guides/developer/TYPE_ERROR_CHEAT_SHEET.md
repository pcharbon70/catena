# Type Error System Cheat Sheet

Quick reference for common usage patterns with the Catena type error system.

## Essential Functions

### Error Formatting

```erlang
% Basic type mismatch
format_type_mismatch(Expected, Actual, Context)
format_type_mismatch(Expected, Actual, Context, Opts)

% General error formatting
format_error(Error)
format_error(Error, Context)

% Specific error types
format_missing_instance(Trait, Types)
format_arity_mismatch(Expected, Actual, FunName, Context)
format_occurs_check(Var, Type, Context)
```

### Educational Content

```erlang
% Get explanations
explain_error(Error)
explain_error(Error, Context)
explain(Error, Context)  % Alias

% Get fix suggestions
suggest_fix(Error, Context)
```

## Verbosity Options

```erlang
Opts = #{verbosity => Verbosity},

Verbosity = terse      % "Expected integer, got string"
         | normal     % Full error with highlighting (default)
         | verbose    % Comprehensive educational output
```

## Type Construction

```erlang
% Basic types
{tcon, integer}
{tcon, string}
{tcon, bool}

% Type variables - uses human-readable names
{tvar, "alpha"}
{tvar, "beta"}
{tvar, input_type}

% Applied types
{tapp, {tcon, list_type}, [ElementType]}     % list<T>
{tapp, {tcon, maybe_type}, [ElementType]}    % maybe<T>

% Function types
{tfun, ParamType, ReturnType, {effect_set, Effects}}

% Records
{trecord, [{field1, Type1}, {field2, Type2}], closed}

% Tuples
{ttuple, [Type1, Type2, Type3]}

% Variant types
{tvariant, [ctor1, ctor2]}
```

## Error Type Patterns

```erlang
% Type mismatches
{unification_error, Expected, Actual}

% Missing trait instances
{unsatisfied_constraint, Trait, Types, no_instance}

% Arity mismatches
{arity_mismatch, ExpectedArgs, ActualArgs, FunName}

% Infinite types (occurs check)
{occurs_check, TypeVar, ProblematicType}

% Unbound variables
{unbound_variable, VarName}

% Ambiguous types
{ambiguous_type, TypeVar}
```

## Context Maps

```erlang
% Basic context
#{expr => Expression}

% Rich context (recommended)
#{
  expr => Expression,
  file => "filename.cat",
  line => 42,
  column => 15,
  function => "my_function"
}
```

## Quick Examples

### Simple Type Mismatch

```erlang
% Setup
Expected = {tcon, integer},
Actual = {tcon, string},
Context = #{},

% Format different ways
terse() ->
  format_type_mismatch(Expected, Actual, Context, #{verbosity => terse}),
  % "Expected integer, got string"

normal() ->
  format_type_mismatch(Expected, Actual, Context),
  % Full error with highlighting

verbose() ->
  format_type_mismatch(Expected, Actual, Context, #{verbosity => verbose}),
  % Comprehensive analysis with suggestions
```

### Complex Container Types

```erlang
% List vs Maybe confusion
ListType = {tapp, {tcon, list_type}, [{tcon, integer}]},
MaybeType = {tapp, {tcon, maybe_type}, [{tcon, string}]},

Error = {unification_error, ListType, MaybeType},

% Get explanation
Explain = catena_type_error_explain:explain_error(Error),
% "Lists and Maybe values are different container types..."

% Get fix suggestions  
Fixes = catena_type_error_explain:suggest_fix(Error, #{}),
% "Consider using 'map' to transform container contents..."
```

### Function Type Issues

```erlang
% Function expected, got value
FunType = {tfun, {tcon, integer}, {tcon, string}, {effect_set, []}},
ValueType = {tcon, integer},

Error = {unification_error, FunType, ValueType},

% Educational explanation
Explain = catena_type_error_explain:explain_error(Error),
% "You're trying to use a non-function value where a function is expected..."

% Actionable fix
Fixes = catena_type_error_explain:suggest_fix(Error, #{}),
% "To fix: Pass a function as the argument, or remove the function call"
```

## Integration Patterns

### IDE/Error Reporting

```erlang
report_error(Error, Context) ->
  % Use terse for space constraints
  TerseOpts = #{verbosity => terse},
  Message = format_type_mismatch(Expected, Actual, Context, TerseOpts),
  ui:show_error(lists:flatten(Message)).
```

### Compiler Integration  

```erlang
compiler_error(Error, Context) ->
  % Use normal for standard output
  Formatted = format_error(Error, Context),
  Explanations = explain_error(Error, Context),
  Complete = [Formatted, "\n\n", Explanations],
  io:format("~s\n", [lists:flatten(Complete)]).
```

### Learning Mode

```erlang
learning_error(Error, Context) ->
  % Use verbose for educational content
  VerboseOpts = #{verbosity => verbose},
  Formatted = format_type_mismatch(Expected, Actual, Context, VerboseOpts),
  Fixes = suggest_fix(Error, Context),
  Educational = [Formatted, "\n\n", Fixes],
  io:format("~s\n", [lists:flatten(Educational)]).
```

## Testing Patterns

```erlang
% Test error formatting
test_format_mismatch() ->
  Expected = {tcon, integer},
  Actual = {tcon, string},
  Result = format_type_mismatch(Expected, Actual, #{}),
  Output = lists:flatten(Result),
  
  ?assert(string:str(Output, "Type mismatch") > 0),
  ?assert(string:str(Output, "Expected: integer") > 0),
  ?assert(string:str(Output, "Got: string") > 0).

% Test explanations
test_explanations() ->
  Error = {unification_error, {tcon, integer}, {tcon, string}},
  Result = catena_type_error_explain:explain_error(Error),
  Output = lists:flatten(Result),
  
  ?assert(string:str(Output, "string where an integer") > 0).
```

## Common Gotchas

1. **Type Construction**: Arguments to `tapp` must be in a list: `[ElementType]`
2. **Maybe Types**: Use `maybe_type` internally, prints as `maybe`
3. **List Types**: Use `list_type` internally, prints as `list`
4. **Context**: Always provide context when available for better messages
5. **Verbosity**: Choose appropriate level for the use case

## Performance Tips

- Cache formatted types when possible
- Use `terse` mode for high-frequency operations
- Lazy-load explanations in UI contexts
- Consider preprocessing common error patterns

This cheat sheet covers the most common usage patterns. See the full Developer Guide for comprehensive documentation.
