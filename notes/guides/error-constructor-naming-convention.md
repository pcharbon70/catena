# Error Constructor Naming Convention

**Date:** 2025-11-16
**Module:** `catena_type_error`
**Status:** Standardized

## Problem

The error constructor functions in `catena_type_error` had inconsistent naming:

### Inconsistent Pattern
```erlang
% Some errors use simple descriptive names
circular_substitution/1         -> {circular_substitution, VarId}
substitution_depth_exceeded/2   -> {substitution_depth_exceeded, Depth, Max}
duplicate_record_fields/1       -> {duplicate_record_fields, Fields}

% Others add a _failure suffix
unification_failure/2           -> {unification_failure, Type1, Type2}
occurs_check_failure/2          -> {occurs_check_failure, VarId, Type}
```

## Preferred Pattern

**Convention:** Error tuple tags should directly describe the problem without redundant `_failure` or `_error` suffixes.

### Rationale

1. **Erlang Convention**: Standard Erlang errors use simple, direct names
   - `badarg`, `badarith`, `function_clause`, `case_clause`
   - Not `badarg_error`, `badarith_failure`

2. **Context is Clear**: The type name `type_error()` and module name `catena_type_error` already indicate these are errors

3. **Consistency**: All error constructors should follow the same pattern

4. **Brevity**: Shorter names are easier to read and type

### Naming Guidelines

1. **For Failed Operations**: Use the operation name directly
   ```erlang
   % GOOD: Names the check that failed
   occurs_check/2 -> {occurs_check, VarId, Type}

   % BAD: Redundant _failure suffix
   occurs_check_failure/2 -> {occurs_check_failure, VarId, Type}
   ```

2. **For Type Mismatches**: Use `_error` suffix only when needed for clarity
   ```erlang
   % GOOD: Clear what went wrong
   unification_error/2 -> {unification_error, Type1, Type2}

   % ACCEPTABLE: But _error suffix is implied
   unification/2 -> {unification, Type1, Type2}

   % BEST: Descriptive without suffix
   type_mismatch/2 -> {type_mismatch, Expected, Actual}
   ```

3. **For Exceeded Limits**: Use `_exceeded` suffix (descriptive, not redundant)
   ```erlang
   % GOOD: Describes what exceeded
   substitution_depth_exceeded/2 -> {substitution_depth_exceeded, Depth, Max}
   type_depth_exceeded/2         -> {type_depth_exceeded, Depth, Max}
   ```

4. **For Structural Problems**: Use noun phrases
   ```erlang
   % GOOD: Describes the structural problem
   duplicate_record_fields/1    -> {duplicate_record_fields, Fields}
   circular_substitution/1      -> {circular_substitution, VarId}
   unbound_variable/1           -> {unbound_variable, VarName}
   ```

## Standardized Names

### Changes Required

| Old Name (Inconsistent) | New Name (Standard) | Reason |
|------------------------|---------------------|--------|
| `unification_failure/2` | `unification_error/2` | Match Erlang convention, remove redundant suffix |
| `occurs_check_failure/2` | `occurs_check/2` | Name after the check that failed |

### All Error Constructors (Standardized)

```erlang
%% Substitution errors - describe the problem directly
circular_substitution/1           % What: circular substitution detected
substitution_depth_exceeded/2     % What: depth limit exceeded
substitution_too_large/2          % What: size limit exceeded

%% Construction errors - structural problems
duplicate_record_fields/1         % What: duplicate fields found
duplicate_variant_constructors/1  % What: duplicate constructors found

%% Unification errors - operation that failed
unification_error/2               % What: unification failed
occurs_check/2                    % What: occurs check failed

%% Type depth errors
type_depth_exceeded/2             % What: depth limit exceeded

%% Environment errors
unbound_variable/1                % What: variable not bound
environment_too_large/2           % What: size limit exceeded

%% Arity errors
arity_mismatch/3                  % What: arity doesn't match

%% Type application errors
invalid_type_application/2        % What: type application is invalid

%% Effect errors
effect_mismatch/2                 % What: effects don't match

%% Field errors
missing_field/2                   % What: field is missing
```

## Pattern Summary

### Good Patterns ✓
- Noun phrases: `circular_substitution`, `unbound_variable`, `missing_field`
- Descriptive adjectives: `duplicate_record_fields`, `invalid_type_application`
- Exceeded limits: `substitution_depth_exceeded`, `type_depth_exceeded`
- Operation names: `occurs_check`, `unification_error`
- Mismatches: `arity_mismatch`, `effect_mismatch`

### Bad Patterns ✗
- Redundant suffixes: `unification_failure`, `occurs_check_failure`
- Verbose: `unification_operation_failed`, `occurs_check_did_not_pass`
- Ambiguous: `error1`, `type_error_2`, `bad_type`

## Benefits

1. **Consistency**: All errors follow the same naming pattern
2. **Readability**: Clear, concise names that describe the problem
3. **Maintainability**: New errors can follow established pattern
4. **Erlang Idiomatic**: Matches Erlang/OTP conventions
5. **Less Typing**: Shorter function names, less verbose error handling

## Examples in Use

### Before (Inconsistent)
```erlang
% Inconsistent naming
case catena_type_subst:unify(T1, T2, Subst) of
    {ok, NewSubst} -> ...;
    {error, {unification_failure, _, _}} -> ...;  % _failure suffix
    {error, {occurs_check_failure, _, _}} -> ...  % _failure suffix
end.
```

### After (Consistent)
```erlang
% Consistent naming
case catena_type_subst:unify(T1, T2, Subst) of
    {ok, NewSubst} -> ...;
    {error, {unification_error, _, _}} -> ...;  % Standard pattern
    {error, {occurs_check, _, _}} -> ...        % Standard pattern
end.
```

## Migration Notes

When refactoring:
1. Update error tuple tags in `type_error()` type spec
2. Update constructor function names and return values
3. Update `format_error/1` pattern matches
4. Update all call sites in type system modules
5. Update tests to use new error names
6. Update documentation and comments

## References

- **Erlang Error Conventions**: `https://www.erlang.org/doc/reference_manual/errors.html`
- **OTP Design Principles**: Error handling patterns in gen_server, gen_statem
- **Module**: `src/compiler/types/catena_type_error.erl`
- **Tests**: `test/compiler/types/catena_type_error_tests.erl`
