%%%
%%% @doc Type Error Formatting
%%%
%%% Provides standardized error types and user-friendly error messages
%%% for type system operations. Integrates with the compiler's error
%%% reporting infrastructure.
%%%
%%% === Error Handling Patterns ===
%%%
%%% The codebase uses three standardized error handling patterns:
%%%
%%% **Pattern 1: Inference Functions (with state threading)**
%%%   - Return: `{ok, Result, State} | {error, Error, State}`
%%%   - Used by: `unify/3`, `infer/3` functions that thread state
%%%   - Example: `catena_infer_unify:unify/3`
%%%
%%% **Pattern 2: Validation Functions (without state)**  
%%%   - Return: `ok | {error, Error}`
%%%   - Used by: Lower-level validation, effect operations
%%%   - Example: `catena_infer_unify:unify_effects/2`
%%%
%%% **Pattern 3: Programming Errors (should never happen)**
%%%   - Return: `error(Error)` (throws exception)
%%%   - Used by: Security violations, invariant violations, programming errors
%%%   - Example: `catena_type_subst:singleton/2` occurs check failures
%%%
%%% **Guideline:** Use Pattern 1 for inference, Pattern 2 for validation,
%%% Pattern 3 only for errors that indicate bugs/security violations.
%%%
%%% @end
%%%
-module(catena_type_error).

-export([
    format_error/1,
    format_error_with_location/2
]).

%% Error constructor functions
-export([
    circular_substitution/1,
    substitution_depth_exceeded/2,
    duplicate_record_fields/1,
    duplicate_variant_constructors/1,
    unification_error/2,
    occurs_check/2,
    type_depth_exceeded/2,
    unbound_variable/1,
    environment_too_large/2,
    substitution_too_large/2,
    arity_mismatch/3,
    duplicate_pattern_binding/3,
    type_var_overflow/2,
    effect_mismatch/2,
    missing_field/2,
    unsatisfied_constraint/3,
    invalid_type_application/2
]).

%%====================================================================
%% Type Definitions
%%====================================================================

%% Type errors that can occur during type checking and inference
-type type_error() ::
    % Substitution errors
    {circular_substitution, catena_types:type_var_id()} |
    {substitution_depth_exceeded, non_neg_integer(), non_neg_integer()} |
    {substitution_too_large, non_neg_integer(), non_neg_integer()} |

    % Construction errors
    {duplicate_record_fields, [atom()]} |
    {duplicate_variant_constructors, [atom()]} |
    {duplicate_pattern_binding, atom(), catena_type_scheme:scheme(), catena_type_scheme:scheme()} |
    {type_var_overflow, non_neg_integer(), non_neg_integer()} |

    % Unification errors
    {unification_error, catena_types:ty(), catena_types:ty()} |
    {occurs_check, catena_types:type_var_id(), catena_types:ty()} |

    % Type depth errors
    {type_depth_exceeded, non_neg_integer(), non_neg_integer()} |

    % Environment errors
    {unbound_variable, atom()} |
    {environment_too_large, non_neg_integer(), non_neg_integer()} |

    % Type application errors
    {arity_mismatch, atom(), non_neg_integer(), non_neg_integer()} |
    {invalid_type_application, catena_types:ty(), [catena_types:ty()]} |

    % Effect errors
    {effect_mismatch, catena_types:effect_set(), catena_types:effect_set()} |

    % Record/field errors
    {missing_field, atom(), catena_types:ty()} |

    % Constraint errors
    {unsatisfied_constraint, catena_constraint:trait_name(), [catena_types:ty()], term()}.
    % {unsatisfied_constraint, TraitName, TypeArgs, Reason}

-export_type([type_error/0]).

%%====================================================================
%% Error Formatting
%%====================================================================

-spec format_error(type_error()) -> string().
%% @doc Format a type error into a human-readable error message
%%
%% Returns a flattened string suitable for display to users.

%% Substitution errors
format_error({circular_substitution, VarId}) ->
    lists:flatten(io_lib:format(
        "Circular type substitution detected: type variable α~p occurs in its own definition~n"
        "This would create an infinite type. Check your type definitions for cycles.",
        [VarId]
    ));

format_error({substitution_depth_exceeded, Depth, Max}) ->
    lists:flatten(io_lib:format(
        "Type substitution depth limit exceeded: ~p levels (maximum: ~p)~n"
        "This may indicate a very complex type or a circular dependency.~n"
        "Consider simplifying your type definitions.",
        [Depth, Max]
    ));

%% Construction errors
format_error({duplicate_record_fields, Fields}) ->
    FieldList = string:join([atom_to_list(F) || F <- Fields], ", "),
    lists:flatten(io_lib:format(
        "Duplicate field names in record type: ~ts~n"
        "Each field name must be unique within a record.",
        [FieldList]
    ));

format_error({duplicate_pattern_binding, VarName, Type1, Type2}) ->
    lists:flatten(io_lib:format(
        "Variable '~p' bound to different types in pattern:~n"
        "  First binding: ~p~n"
        "  Second binding: ~p~n"
        "In pattern matching, the same variable name cannot represent different types.",
        [VarName, Type1, Type2]
    ));

format_error({type_var_overflow, CurrentId, MaxId}) ->
    lists:flatten(io_lib:format(
        "Type variable ID overflow: reached ~p (maximum: ~p)~n"
        "This may indicate infinitely complex types or a potential DoS attack.~n"
        "Consider simplifying your type definitions or increasing the limit.",
        [CurrentId, MaxId]
    ));

format_error({duplicate_variant_constructors, Constructors}) ->
    ConstructorList = string:join([atom_to_list(C) || C <- Constructors], ", "),
    lists:flatten(io_lib:format(
        "Duplicate constructor names in variant type: ~ts~n"
        "Each constructor name must be unique within a variant.",
        [ConstructorList]
    ));

%% Unification errors
format_error({unification_error, Type1, Type2}) ->
    Type1Str = catena_type_pp:pp_type(Type1),
    Type2Str = catena_type_pp:pp_type(Type2),
    lists:flatten(io_lib:format(
        "Type unification failed:~n"
        "  Expected: ~ts~n"
        "  Got:      ~ts~n"
        "These types are incompatible.",
        [Type1Str, Type2Str]
    ));

format_error({occurs_check, VarId, Type}) ->
    TypeStr = catena_type_pp:pp_type(Type),
    lists:flatten(io_lib:format(
        "Occurs check failed: type variable α~p occurs in type ~ts~n"
        "This would create an infinite type. Cannot unify a variable with a type containing itself.",
        [VarId, TypeStr]
    ));

%% Type depth errors
format_error({type_depth_exceeded, Depth, Max}) ->
    lists:flatten(io_lib:format(
        "Type nesting depth exceeded: ~p levels (maximum: ~p)~n"
        "Your type is too deeply nested. Consider breaking it into smaller, named types.",
        [Depth, Max]
    ));

%% Environment errors
format_error({unbound_variable, VarName}) ->
    lists:flatten(io_lib:format(
        "Unbound variable: ~ts~n"
        "This variable is not defined in the current scope.",
        [atom_to_list(VarName)]
    ));

format_error({environment_too_large, Size, Max}) ->
    lists:flatten(io_lib:format(
        "Type environment too large: ~p bindings (maximum: ~p)~n"
        "Consider reducing the number of variables in scope or splitting into smaller functions.",
        [Size, Max]
    ));

format_error({substitution_too_large, Size, Max}) ->
    lists:flatten(io_lib:format(
        "Type substitution too large: ~p mappings (maximum: ~p)~n"
        "This may indicate excessive type complexity. Consider simplifying your types.",
        [Size, Max]
    ));

%% Type application errors
format_error({arity_mismatch, Name, Expected, Actual}) ->
    lists:flatten(io_lib:format(
        "Arity mismatch for type constructor '~s':~n"
        "  Expected: ~p type arguments~n"
        "  Got:      ~p type arguments",
        [atom_to_list(Name), Expected, Actual]
    ));

format_error({invalid_type_application, Constructor, Args}) ->
    ConstructorStr = catena_type_pp:pp_type(Constructor),
    ArgCount = length(Args),
    lists:flatten(io_lib:format(
        "Invalid type application: ~ts cannot be applied to ~p arguments~n"
        "Type constructors must be applied to the correct number of type arguments.",
        [ConstructorStr, ArgCount]
    ));

%% Effect errors
format_error({effect_mismatch, Expected, Actual}) ->
    ExpectedStr = catena_type_pp:pp_effects(Expected),
    ActualStr = catena_type_pp:pp_effects(Actual),
    case {ExpectedStr, ActualStr} of
        {"", ""} ->
            "Effect mismatch (both pure - should not happen)";
        {"", _} ->
            lists:flatten(io_lib:format(
                "Effect mismatch:~n"
                "  Expected: pure function~n"
                "  Got:      function with effects ~s",
                [ActualStr]
            ));
        {_, ""} ->
            lists:flatten(io_lib:format(
                "Effect mismatch:~n"
                "  Expected: function with effects ~ts~n"
                "  Got:      pure function",
                [ExpectedStr]
            ));
        _ ->
            lists:flatten(io_lib:format(
                "Effect mismatch:~n"
                "  Expected: ~ts~n"
                "  Got:      ~ts",
                [ExpectedStr, ActualStr]
            ))
    end;

%% Record/field errors
format_error({missing_field, Field, RecordType}) ->
    RecordStr = catena_type_pp:pp_type(RecordType),
    lists:flatten(io_lib:format(
        "Missing field '~s' in record type ~ts~n"
        "The record does not have a field with this name.",
        [atom_to_list(Field), RecordStr]
    ));

%% Constraint errors
format_error({unsatisfied_constraint, TraitName, TypeArgs, Reason}) ->
    TypeStrs = lists:map(fun catena_type_pp:pp_type/1, TypeArgs),
    ConstraintStr = lists:flatten(
        io_lib:format("~s ~s", [TraitName, lists:join(" ", TypeStrs)])
    ),
    ReasonStr = case Reason of
        no_instance -> "no instance found";
        {ambiguous, _} -> "ambiguous instances (multiple matches)";
        _ -> io_lib:format("~p", [Reason])
    end,
    lists:flatten(io_lib:format(
        "Unsatisfied trait constraint: ~s~n"
        "Reason: ~s~n"
        "Make sure the required trait instance is defined and in scope.",
        [ConstraintStr, ReasonStr]
    ));

%% Catch-all for unknown errors
format_error(Error) ->
    lists:flatten(io_lib:format("Unknown type error: ~p", [Error])).

%%====================================================================
%% Error Constructor Functions
%%====================================================================

%% @doc Create a circular substitution error
-spec circular_substitution(catena_types:type_var_id()) -> type_error().
circular_substitution(VarId) ->
    {circular_substitution, VarId}.

%% @doc Create a substitution depth exceeded error
-spec substitution_depth_exceeded(non_neg_integer(), non_neg_integer()) -> type_error().
substitution_depth_exceeded(Depth, Max) ->
    {substitution_depth_exceeded, Depth, Max}.

%% @doc Create a substitution too large error
-spec substitution_too_large(non_neg_integer(), non_neg_integer()) -> type_error().
substitution_too_large(Size, Max) ->
    {substitution_too_large, Size, Max}.

%% @doc Create a duplicate record fields error
-spec duplicate_record_fields([atom()]) -> type_error().
duplicate_record_fields(Fields) ->
    {duplicate_record_fields, Fields}.

%% @doc Create a duplicate variant constructors error
-spec duplicate_variant_constructors([atom()]) -> type_error().
duplicate_variant_constructors(Constructors) ->
    {duplicate_variant_constructors, Constructors}.

%% @doc Create a unification error
-spec unification_error(catena_types:ty(), catena_types:ty()) -> type_error().
unification_error(Type1, Type2) ->
    {unification_error, Type1, Type2}.

%% @doc Create an occurs check error
-spec occurs_check(catena_types:type_var_id(), catena_types:ty()) -> type_error().
occurs_check(VarId, Type) ->
    {occurs_check, VarId, Type}.

%% @doc Create a type depth exceeded error
-spec type_depth_exceeded(non_neg_integer(), non_neg_integer()) -> type_error().
type_depth_exceeded(Depth, Max) ->
    {type_depth_exceeded, Depth, Max}.

%% @doc Create an unbound variable error
-spec unbound_variable(atom()) -> type_error().
unbound_variable(VarName) ->
    {unbound_variable, VarName}.

%% @doc Create an environment too large error
-spec environment_too_large(non_neg_integer(), non_neg_integer()) -> type_error().
environment_too_large(Size, Max) ->
    {environment_too_large, Size, Max}.

%% @doc Create an arity mismatch error
-spec arity_mismatch(atom(), non_neg_integer(), non_neg_integer()) -> type_error().
arity_mismatch(Name, Expected, Actual) ->
    {arity_mismatch, Name, Expected, Actual}.

%% @doc Create a duplicate pattern binding error
-spec duplicate_pattern_binding(atom(), catena_type_scheme:scheme(), catena_type_scheme:scheme()) -> type_error().
duplicate_pattern_binding(VarName, Type1, Type2) ->
    {duplicate_pattern_binding, VarName, Type1, Type2}.

%% @doc Create a type variable overflow error
-spec type_var_overflow(non_neg_integer(), non_neg_integer()) -> type_error().
type_var_overflow(CurrentId, MaxId) ->
    {type_var_overflow, CurrentId, MaxId}.

%% @doc Create an invalid type application error
-spec invalid_type_application(catena_types:ty(), [catena_types:ty()]) -> type_error().
invalid_type_application(Constructor, Args) ->
    {invalid_type_application, Constructor, Args}.

%% @doc Create an effect mismatch error
-spec effect_mismatch(catena_types:effect_set(), catena_types:effect_set()) -> type_error().
effect_mismatch(Expected, Actual) ->
    {effect_mismatch, Expected, Actual}.

%% @doc Create a missing field error
-spec missing_field(atom(), catena_types:ty()) -> type_error().
missing_field(Field, RecordType) ->
    {missing_field, Field, RecordType}.

%% @doc Create an unsatisfied constraint error
-spec unsatisfied_constraint(catena_constraint:trait_name(), [catena_types:ty()], term()) -> type_error().
unsatisfied_constraint(TraitName, TypeArgs, Reason) ->
    {unsatisfied_constraint, TraitName, TypeArgs, Reason}.

%%====================================================================
%% Location Formatting
%%====================================================================

%% @doc Format a type error with source location information
%%
%% Combines location information with the formatted error message
%% to provide better error reporting with source context.
%%
%% @param Location Source location tuple (from catena_location)
%% @param Error The type error to format
%% @returns Formatted error message with location prefix
-spec format_error_with_location(tuple(), type_error()) -> string().
format_error_with_location(Location, Error) ->
    LocStr = catena_location:format(Location),
    ErrorStr = format_error(Error),
    lists:flatten(io_lib:format("~s: ~ts", [LocStr, ErrorStr])).