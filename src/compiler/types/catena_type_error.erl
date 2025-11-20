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
    invalid_type_application/2,
    %% Effect-specific errors (Task 1.2.5)
    unhandled_effect/3,
    handler_missing_operation/3,
    handler_arity_mismatch/5,
    effect_annotation_mismatch/3,
    effect_context_chain/2
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

    % Effect-specific errors (Task 1.2.5)
    {unhandled_effect, atom(), atom(), catena_location:location()} |  % {EffectName, FunctionName, Location}
    {handler_missing_operation, atom(), atom(), catena_location:location()} |  % {EffectName, OperationName, HandlerLoc}
    {handler_arity_mismatch, atom(), atom(), non_neg_integer(), non_neg_integer(), catena_location:location()} |  % {Effect, Op, Expected, Actual, Loc}
    {effect_annotation_mismatch, atom(), catena_types:effect_set(), catena_types:effect_set()} |  % {FuncName, Declared, Inferred}
    {effect_context_chain, atom(), [{atom(), catena_location:location()}]} |  % {EffectName, [{FuncName, Location}]}

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

%% Effect-specific errors (Task 1.2.5)

format_error({unhandled_effect, EffectName, FunctionName, Location}) ->
    LocStr = catena_location:format(Location),
    lists:flatten(io_lib:format(
        "Unhandled effect '~s' in function '~s'~n"
        "Effect introduced at: ~s~n"
        "~n"
        "The function performs an operation that requires the ~s effect,~n"
        "but this effect is not declared in the function's type signature.~n"
        "~n"
        "To fix this, either:~n"
        "  1. Add the effect to the function signature: / {~s}~n"
        "  2. Handle the effect with a try/with block",
        [atom_to_list(EffectName), atom_to_list(FunctionName), LocStr,
         atom_to_list(EffectName), atom_to_list(EffectName)]
    ));

format_error({handler_missing_operation, EffectName, OperationName, HandlerLoc}) ->
    LocStr = catena_location:format(HandlerLoc),
    lists:flatten(io_lib:format(
        "Missing handler for operation '~s.~s'~n"
        "Handler at: ~s~n"
        "~n"
        "The handler for effect '~s' does not handle the '~s' operation.~n"
        "All operations of an effect must be handled.~n"
        "~n"
        "Add a handler case:~n"
        "  | ~s(args) -> result",
        [atom_to_list(EffectName), atom_to_list(OperationName), LocStr,
         atom_to_list(EffectName), atom_to_list(OperationName),
         atom_to_list(OperationName)]
    ));

format_error({handler_arity_mismatch, EffectName, OperationName, Expected, Actual, Location}) ->
    LocStr = catena_location:format(Location),
    lists:flatten(io_lib:format(
        "Handler arity mismatch for '~s.~s'~n"
        "At: ~s~n"
        "~n"
        "  Expected: ~p arguments~n"
        "  Got:      ~p arguments~n"
        "~n"
        "The handler must accept the same number of arguments as the operation declaration.",
        [atom_to_list(EffectName), atom_to_list(OperationName), LocStr,
         Expected, Actual]
    ));

format_error({effect_annotation_mismatch, FunctionName, Declared, Inferred}) ->
    DeclaredStr = catena_type_pp:pp_effects(Declared),
    InferredStr = catena_type_pp:pp_effects(Inferred),

    %% Determine which effects are extra or missing
    {effect_set, DeclaredEffects} = Declared,
    {effect_set, InferredEffects} = Inferred,
    Missing = InferredEffects -- DeclaredEffects,
    Extra = DeclaredEffects -- InferredEffects,

    MissingStr = case Missing of
        [] -> "";
        _ -> io_lib:format("~n  Missing from annotation: ~p", [Missing])
    end,
    ExtraStr = case Extra of
        [] -> "";
        _ -> io_lib:format("~n  Not used but declared: ~p", [Extra])
    end,

    lists:flatten(io_lib:format(
        "Effect annotation mismatch in function '~s'~n"
        "  Declared: ~ts~n"
        "  Inferred: ~ts~ts~ts~n"
        "~n"
        "The effect annotation does not match the effects actually used by the function.",
        [atom_to_list(FunctionName), DeclaredStr, InferredStr, MissingStr, ExtraStr]
    ));

format_error({effect_context_chain, EffectName, Chain}) ->
    ChainStr = format_effect_chain(Chain),
    lists:flatten(io_lib:format(
        "Effect '~s' propagation trace:~n"
        "~s~n"
        "The effect was introduced and propagated through the following call chain.~n"
        "Consider handling the effect at an appropriate point in this chain.",
        [atom_to_list(EffectName), ChainStr]
    ));

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

%% Helper function to format effect propagation chain
%% Limited to MAX_CHAIN_LENGTH entries to prevent resource exhaustion
-define(MAX_CHAIN_LENGTH, 50).

format_effect_chain(Chain) ->
    format_effect_chain(Chain, 1, []).

format_effect_chain([], _N, Acc) ->
    lists:flatten(lists:reverse(Acc));
format_effect_chain(_Rest, N, Acc) when N > ?MAX_CHAIN_LENGTH ->
    Truncated = io_lib:format("  ... and ~p more entries (truncated)~n", [length(_Rest)]),
    lists:flatten(lists:reverse([Truncated | Acc]));
format_effect_chain([{FuncName, Location} | Rest], N, Acc) ->
    LocStr = catena_location:format(Location),
    Line = io_lib:format("  ~p. ~s at ~s~n", [N, atom_to_list(FuncName), LocStr]),
    format_effect_chain(Rest, N + 1, [Line | Acc]).

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
%% Effect-Specific Error Constructors (Task 1.2.5)
%%====================================================================

%% @doc Create an unhandled effect error
%%
%% Reports when a function performs an effect that is not declared
%% in its type signature. Includes the location where the effect was
%% introduced (the perform site).
%%
%% @param EffectName The name of the unhandled effect
%% @param FunctionName The name of the function with the unhandled effect
%% @param Location The source location where the effect was introduced
-spec unhandled_effect(atom(), atom(), catena_location:location()) -> type_error().
unhandled_effect(EffectName, FunctionName, Location) ->
    {unhandled_effect, EffectName, FunctionName, Location}.

%% @doc Create a handler missing operation error
%%
%% Reports when a try/with handler does not handle all operations
%% of an effect. All operations must be handled for the effect to
%% be properly resolved.
%%
%% @param EffectName The name of the effect
%% @param OperationName The name of the missing operation
%% @param HandlerLocation The source location of the handler
-spec handler_missing_operation(atom(), atom(), catena_location:location()) -> type_error().
handler_missing_operation(EffectName, OperationName, HandlerLocation) ->
    {handler_missing_operation, EffectName, OperationName, HandlerLocation}.

%% @doc Create a handler arity mismatch error
%%
%% Reports when a handler case has the wrong number of parameters
%% for an operation. The handler must accept the same number of
%% arguments as declared in the effect operation.
%%
%% @param EffectName The name of the effect
%% @param OperationName The name of the operation
%% @param Expected The expected number of arguments
%% @param Actual The actual number of arguments in the handler
%% @param Location The source location of the handler case
-spec handler_arity_mismatch(atom(), atom(), non_neg_integer(), non_neg_integer(), catena_location:location()) -> type_error().
handler_arity_mismatch(EffectName, OperationName, Expected, Actual, Location) ->
    {handler_arity_mismatch, EffectName, OperationName, Expected, Actual, Location}.

%% @doc Create an effect annotation mismatch error
%%
%% Reports when the declared effect annotation on a function does not
%% match the effects actually inferred from the function body.
%%
%% @param FunctionName The name of the function
%% @param Declared The declared effect set from the annotation
%% @param Inferred The inferred effect set from the function body
-spec effect_annotation_mismatch(atom(), catena_types:effect_set(), catena_types:effect_set()) -> type_error().
effect_annotation_mismatch(FunctionName, Declared, Inferred) ->
    {effect_annotation_mismatch, FunctionName, Declared, Inferred}.

%% @doc Create an effect context chain error
%%
%% Shows the propagation path of an effect from where it was introduced
%% through the chain of callers. Helps developers understand where the
%% effect came from and where it might need to be handled.
%%
%% @param EffectName The name of the effect being traced
%% @param Chain List of {FunctionName, Location} pairs showing propagation
-spec effect_context_chain(atom(), [{atom(), catena_location:location()}]) -> type_error().
effect_context_chain(EffectName, Chain) ->
    {effect_context_chain, EffectName, Chain}.

%%====================================================================
%% Location Formatting
%%====================================================================

%% @doc Format a type error with source location information
%%
%% Combines location information with the formatted error message
%% to provide better error reporting with source context.
%%
%% @param Location Source location (from catena_location)
%% @param Error The type error to format
%% @returns Formatted error message with location prefix
-spec format_error_with_location(catena_location:location(), type_error()) -> string().
format_error_with_location(Location, Error) ->
    LocStr = catena_location:format(Location),
    ErrorStr = format_error(Error),
    lists:flatten(io_lib:format("~s: ~ts", [LocStr, ErrorStr])).