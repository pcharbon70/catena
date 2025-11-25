%%%-------------------------------------------------------------------
%%% @doc Pattern Type Inference
%%%
%%% This module implements type inference for patterns in pattern matching.
%%% Patterns introduce new bindings in the environment and generate constraints.
%%%
%%% Pattern inference returns:
%%% - The inferred type of the pattern
%%% - Bindings introduced by the pattern (variable names → types)
%%% - Updated inference state
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_infer_pattern).

-export([
    infer/3,
    merge_bindings/2
]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Infer the type of a pattern
%% Returns the pattern's type and bindings it introduces
-spec infer(catena_ast:pattern(), catena_type_env:env(), catena_infer_state:infer_state()) ->
    {catena_types:type(), catena_type_env:env(), catena_infer_state:infer_state()}.

% Pattern: Wildcard
% _ : α  (fresh type variable, no bindings)
infer({pwild}, _Env, State) ->
    {Type, State1} = catena_infer_state:fresh_var(State),
    EmptyBindings = catena_type_env:empty(),
    {Type, EmptyBindings, State1};

% Pattern: Literal
% 42 : Int
% true : Bool
% "hello" : String
infer({plit, Lit}, _Env, State) ->
    Type = catena_infer_utils:literal_type(Lit),
    EmptyBindings = catena_type_env:empty(),
    {Type, EmptyBindings, State};

% Pattern: Variable
% x : α  (fresh type variable, binds x : α)
infer({pvar, Name}, _Env, State) ->
    {Type, State1} = catena_infer_state:fresh_var(State),

    % Create binding for variable
    Scheme = catena_type_scheme:mono(Type),
    Bindings = catena_type_env:extend(catena_type_env:empty(), Name, Scheme),

    {Type, Bindings, State1};

% Pattern: Tuple
% (p1, ..., pn) : (T1, ..., Tn)
% where pi : Ti
infer({ptuple, PatternList}, Env, State) ->
    {Types, Bindings, State1} = infer_patterns(PatternList, Env, State),
    Type = {ttuple, Types},
    {Type, Bindings, State1};

% Pattern: Record
% {field1: p1, ..., fieldn: pn} : {field1: T1, ..., fieldn: Tn | closed}
% where pi : Ti
infer({precord, Fields}, Env, State) ->
    {FieldTypes, Bindings, State1} = infer_record_fields(Fields, Env, State),
    Type = {trecord, FieldTypes, closed},
    {Type, Bindings, State1};

% Pattern: Variant Constructor
% C p1 ... pn : [... | C T1 ... Tn | ...]
% where pi : Ti
% For PoC, we create a fresh variant type with this constructor
infer({pvariant, Constructor, ArgPatterns}, Env, State) ->
    {ArgTypes, Bindings, State1} = infer_patterns(ArgPatterns, Env, State),

    % Create variant type with this constructor
    Type = {tvariant, [{Constructor, ArgTypes}]},

    {Type, Bindings, State1};

% Pattern: As-pattern
% (p as x) : T where p : T
% Binds both the pattern's bindings and x : T
infer({pas, Name, Pattern}, Env, State) ->
    {Type, PatBindings, State1} = infer(Pattern, Env, State),

    % Add binding for the name
    Scheme = catena_type_scheme:mono(Type),
    NameBinding = catena_type_env:extend(catena_type_env:empty(), Name, Scheme),

    % Merge bindings
    CombinedBindings = merge_bindings(NameBinding, PatBindings),

    {Type, CombinedBindings, State1};

% Pattern: Or-pattern
% (p1 | p2 | ... | pn) : T where all pi : T
% All alternatives must bind the same variables with the same types
infer({por, Patterns}, Env, State) ->
    infer_or_pattern(Patterns, Env, State).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Infer types for multiple patterns
%% Collects all bindings from all patterns
-spec infer_patterns([catena_ast:pattern()], catena_type_env:env(), catena_infer_state:infer_state()) ->
    {[catena_types:type()], catena_type_env:env(), catena_infer_state:infer_state()}.
infer_patterns(Patterns, Env, State) ->
    infer_patterns_acc(Patterns, Env, State, [], catena_type_env:empty()).

infer_patterns_acc([], _Env, State, TypesAcc, BindingsAcc) ->
    {lists:reverse(TypesAcc), BindingsAcc, State};
infer_patterns_acc([P | Rest], Env, State, TypesAcc, BindingsAcc) ->
    {Type, Bindings, State1} = infer(P, Env, State),

    % Merge bindings (checking for duplicates)
    CombinedBindings = merge_bindings(BindingsAcc, Bindings),

    infer_patterns_acc(Rest, Env, State1, [Type | TypesAcc], CombinedBindings).

%% @doc Infer types for record pattern fields
-spec infer_record_fields([{atom(), catena_ast:pattern()}], catena_type_env:env(),
                         catena_infer_state:infer_state()) ->
    {[{atom(), catena_types:type()}], catena_type_env:env(), catena_infer_state:infer_state()}.
infer_record_fields(Fields, Env, State) ->
    infer_record_fields_acc(Fields, Env, State, [], catena_type_env:empty()).

infer_record_fields_acc([], _Env, State, FieldsAcc, BindingsAcc) ->
    {lists:reverse(FieldsAcc), BindingsAcc, State};
infer_record_fields_acc([{Label, Pattern} | Rest], Env, State, FieldsAcc, BindingsAcc) ->
    {Type, Bindings, State1} = infer(Pattern, Env, State),

    % Merge bindings
    CombinedBindings = merge_bindings(BindingsAcc, Bindings),

    infer_record_fields_acc(Rest, Env, State1, [{Label, Type} | FieldsAcc], CombinedBindings).

%% @doc Infer type for or-patterns
%% All alternatives must have the same type and bind the same variables
-spec infer_or_pattern([catena_ast:pattern()], catena_type_env:env(), catena_infer_state:infer_state()) ->
    {catena_types:type(), catena_type_env:env(), catena_infer_state:infer_state()}.
infer_or_pattern([], _Env, _State) ->
    error({empty_or_pattern});
infer_or_pattern([Pattern], Env, State) ->
    % Single pattern - just infer it
    infer(Pattern, Env, State);
infer_or_pattern([Pattern | Rest], Env, State) ->
    % Infer first pattern to get expected type and bindings
    {Type, Bindings, State1} = infer(Pattern, Env, State),

    % Infer remaining patterns, checking consistency
    {State2} = check_or_pattern_consistency(Rest, Env, State1, Type, Bindings),

    {Type, Bindings, State2}.

%% @doc Check that all or-pattern alternatives have consistent types and bindings
-spec check_or_pattern_consistency([catena_ast:pattern()], catena_type_env:env(),
                                   catena_infer_state:infer_state(),
                                   catena_types:type(), catena_type_env:env()) ->
    {catena_infer_state:infer_state()}.
check_or_pattern_consistency([], _Env, State, _ExpectedType, _ExpectedBindings) ->
    {State};
check_or_pattern_consistency([Pattern | Rest], Env, State, ExpectedType, ExpectedBindings) ->
    {Type, Bindings, State1} = infer(Pattern, Env, State),

    % Unify type with expected type
    State2 = case catena_infer_unify:unify(Type, ExpectedType, State1) of
        {ok, _Subst, S} -> S;
        {error, Reason, S} ->
            error({or_pattern_type_mismatch, Pattern, Type, ExpectedType, Reason, S})
    end,

    % Check bindings are consistent (same variables, same types)
    check_or_pattern_bindings(Bindings, ExpectedBindings, Pattern),

    check_or_pattern_consistency(Rest, Env, State2, ExpectedType, ExpectedBindings).

%% @doc Check that or-pattern alternative binds same variables as expected
-spec check_or_pattern_bindings(catena_type_env:env(), catena_type_env:env(),
                                catena_ast:pattern()) -> ok.
check_or_pattern_bindings(Bindings, ExpectedBindings, Pattern) ->
    BindingVars = lists:sort(maps:keys(Bindings)),
    ExpectedVars = lists:sort(maps:keys(ExpectedBindings)),

    case BindingVars =:= ExpectedVars of
        true -> ok;
        false ->
            MissingVars = ExpectedVars -- BindingVars,
            ExtraVars = BindingVars -- ExpectedVars,
            error({or_pattern_binding_mismatch, Pattern, MissingVars, ExtraVars})
    end.

%% @doc Merge two binding environments, detecting duplicate bindings
%% In patterns, duplicate variable names are not allowed unless they bind
%% to identical types (which is redundant but not an error)
-spec merge_bindings(catena_type_env:env(), catena_type_env:env()) -> catena_type_env:env().
merge_bindings(Env1, Env2) ->
    % Find common variable names (potential conflicts)
    Vars1 = maps:keys(Env1),
    Vars2 = maps:keys(Env2),
    CommonVars = sets:intersection(sets:from_list(Vars1), sets:from_list(Vars2)),
    
    % Check for type conflicts in common variables
    case sets:to_list(CommonVars) of
        [] ->
            % No conflicts, safe to merge
            catena_type_env:merge(Env1, Env2);
        Conflicts ->
            % Check each common variable for type mismatches
            case check_type_conflicts(Conflicts, Env1, Env2) of
                ok ->
                    % All types match, safe to merge
                    catena_type_env:merge(Env1, Env2);
                {error, {duplicate_binding, Var, Type1, Type2}} ->
                    % Type conflict found, create error
                    error(catena_type_error:duplicate_pattern_binding(Var, Type1, Type2))
            end
    end.

%% @doc Check if any common variables have conflicting types
%% Returns ok if all types match, or {error, Conflict} if conflict found
-spec check_type_conflicts([atom()], catena_type_env:env(), catena_type_env:env()) ->
    ok | {error, {duplicate_binding, atom(), catena_type_scheme:scheme(), catena_type_scheme:scheme()}}.
check_type_conflicts([], _Env1, _Env2) ->
    ok;
check_type_conflicts([Var | Rest], Env1, Env2) ->
    Scheme1 = maps:get(Var, Env1),
    Scheme2 = maps:get(Var, Env2),
    
    case Scheme1 =:= Scheme2 of
        true ->
            % Types match, continue checking
            check_type_conflicts(Rest, Env1, Env2);
        false ->
            % Types conflict, return error
            {error, {duplicate_binding, Var, Scheme1, Scheme2}}
    end.


