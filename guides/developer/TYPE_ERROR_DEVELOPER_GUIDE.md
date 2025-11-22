# Type Error System Developer Guide

This guide explains how to use and integrate the Catena type error formatting system to provide excellent developer experience when encountering type errors.

## Overview

The type error system consists of three main modules that work together:

- **`catena_type_error_formatter`** - Rich error formatting with highlighting and verbosity control
- **`catena_type_error_explain`** - Educational explanations and actionable fix suggestions  
- **`catena_type_pp`** - Human-readable type pretty-printing with intuitive variable names

## Quick Start

### Basic Error Formatting

```erlang
% Simple type mismatch error
Expected = {tcon, integer},
Actual = {tcon, string},
Result = catena_type_error_formatter:format_type_mismatch(Expected, Actual, #{}),
% Output: "Type mismatch:\n  Expected: integer\n  Got:      string\n\nThese types are incompatible..."
```

### Getting Educational Explanations

```erlang
Error = {unification_error, Expected, Actual},
Explanation = catena_type_error_explain:explain_error(Error),
% Output: "You're using an integer where a string is expected..."
```

### Actionable Fix Suggestions

```erlang
Fixes = catena_type_error_explain:suggest_fix(Error, #{}),
% Output: "To fix: Add quotes around the number, or use Integer.to_text to convert"
```

## Integration Patterns

### 1. IDE Integration

Use **terse** mode for space-constrained UI contexts like tooltips:

```erlang
catena_type_error_formatter:format_type_mismatch(
  Expected, Actual, Context, #{verbosity => terse}
)
% Output: "Expected integer, got string"
```

### 2. Compiler Messages

Use **normal** mode for standard compiler output:

```erlang
catena_type_error_formatter:format_error(Error, Context)
```

### 3. Learning Environments

Use **verbose** mode for educational content and detailed debugging:

```erlang
catena_type_error_formatter:format_type_mismatch(
  Expected, Actual, Context, #{verbosity => verbose}
)
% Output: Comprehensive analysis with suggestions and examples
```

## Error Type Reference

### Type Mismatches

Most common errors are type mismatches. The system handles:

- **Basic types**: integer, string, bool, etc.
- **Function types**: parameter and return type mismatches
- **Container types**: list vs maybe, element type mismatches
- **Record types**: missing/extra fields, field type mismatches
- **Tuple types**: size mismatches
- **Variant types**: incompatible variant types

### Trait Instance Errors

When a type doesn't support a required trait:

```erlang
Error = {unsatisfied_constraint, 'Eq', Types, no_instance}
catena_type_error_formatter:format_missing_instance('Eq', Types)
% Output: "Missing instance of trait 'Eq' for type: MyType"
```

### Function Arity Errors

Wrong number of function arguments:

```erlang
Error = {arity_mismatch, 2, 3, "my_func"}
catena_type_error_formatter:format_arity_mismatch(2, 3, "my_func", #{})
% Output: "Function my_func expects 2 arguments but got 3"
```

### Infinite Type Errors

Occurs check failures for recursive types:

```erlang
Error = {occurs_check, {tvar, "alpha"}, {tapp, {tcon, list_type}, [{tvar, "alpha"}]}}
catena_type_error_formatter:format_occurs_check(Var, Type, #{})
% Output: Educational message about infinite types with solutions
```

## Best Practices

### 1. Always Provide Context

```erlang
% Good - with context
format_type_mismatch(Expected, Actual, #{expr => MyExpression})

% Better - with expression context and file position
Context = #{
  expr => MyExpression,
  file => "my_file.cat",
  line => 42,
  column => 15
},
format_type_mismatch(Expected, Actual, Context)
```

### 2. Use Appropriate Verbosity

- **IDE tooltips**: Use `#{verbosity => terse}`
- **Compiler errors**: Use `#{verbosity => normal}` (default)
- **Tutorial/debug modes**: Use `#{verbosity => verbose}`

### 3. Combine Formatting and Explanations

For the best developer experience:

```erlang
% Format the error with highlighting
Formatted = catena_type_error_formatter:format_error(Error, Context),

% Get educational explanation
Explanation = catena_type_error_explain:explain_error(Error, Context),

% Get actionable fixes
Fixes = catena_type_error_explain:suggest_fix(Error, Context),

% Combine for comprehensive output
Output = [Formatted, "\n\n", Explanation, "\n\n", Fixes]
```

## Example: Complete Integration

Here's how to integrate the system into a compiler:

```erlang
-module(my_compiler).
-export([report_type_error/4]).

report_type_error(Error, Context, Verbosity, OutputFormat) ->
  % Configure formatting options
  Opts = case Verbosity of
    quiet -> #{verbosity => terse};
    normal -> #{verbosity => normal};
    verbose -> #{verbosity => verbose}
  end,
  
  % Generate the error message
  FormattedError = catena_type_error_formatter:format_error(Error, Context, Opts),
  
  % Get explanations for educational output
  case OutputFormat of
    educational ->
      Explanation = catena_type_error_explain:explain_error(Error, Context),
      Fixes = catena_type_error_explain:suggest_fix(Error, Context),
      Message = [FormattedError, "\n\n", Explanation, "\n\n", Fixes];
    
    _ ->
      Message = FormattedError
  end,
  
  % Output the final message
  io:format("~s\n", [lists:flatten(Message)]).

% Usage:
% report_type_error(TypeError, #{expr => SomeExpr}, normal, educational)
```

## Troubleshooting

### Common Issues

1. **Unreadable type names**: Ensure you're using `catena_type_pp` for pretty-printing
2. **Missing context**: Always provide expression context when available
3. **Wrong verbosity**: Use `terse` for space constraints, `verbose` for education

### Debug Tips

```erlang
% Test type construction and pretty printing
Type = {tapp, {tcon, list_type}, [{tvar, "alpha"}]},
Pretty = catena_type_pp:pp_type(Type),
% Should output: "list<alpha>"

% Test error formatting with sample data
Error = {unification_error, {tcon, integer}, {tcon, string}},
Formatted = catena_type_error_formatter:format_error(Error, #{}),
io:format("Sample error:\n~s", [lists:flatten(Formatted)])
```

## Extending the System

### Adding New Error Types

1. Define the error type in `catena_type_error.erl`
2. Add formatting in `catena_type_error_formatter.erl`
3. Add explanations in `catena_type_error_explain.erl`
4. Add comprehensive tests

### Custom Verbosity Levels

Extend the verbosity system by modifying the pattern match in `format_type_mismatch/4`:

```erlang
format_type_mismatch(Expected, Actual, Context, Opts) ->
  Verbosity = maps:get(verbosity, Opts, normal),
  case Verbosity of
    my_custom_level ->
      % Your custom formatting logic
  end.
```

This guide should help you effectively integrate and extend the type error system for excellent developer experience.
