%%%-------------------------------------------------------------------
%%% @doc Coherence Checking for Trait Instances
%%%
%%% This module implements coherence checking to ensure that trait
%%% instance resolution is unambiguous. The coherence property guarantees
%%% that for any constraint, at most one instance matches.
%%%
%%% Key Concepts:
%%% - Coherence: No overlapping instances (unique resolution)
%%% - Overlap: Two instances that could match the same constraint
%%% - Orphan Instances: Instances defined outside trait or type module
%%%
%%% Coherence Rules:
%%% 1. No overlapping instances for the same trait
%%% 2. Instance heads must not unify (except for identical instances)
%%% 3. More specific instances are preferred over general ones
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_coherence).

%% Coherence checking
-export([
    check_instance_db/1,
    check_new_instance/2,
    find_overlaps/2,
    check_coverage/2
]).

%% Error reporting
-export([
    format_overlap_error/2,
    format_coverage_error/2
]).

-export_type([coherence_error/0, coverage_error/0]).

%%%===================================================================
%%% Types
%%%===================================================================

%% Coherence error: overlapping instances
-type coherence_error() :: {
    overlap,
    catena_instance:instance(),  % First overlapping instance
    catena_instance:instance(),  % Second overlapping instance
    reason()
}.

%% Coverage error: missing instances
-type coverage_error() :: {
    missing_instance,
    catena_constraint:trait_name(),
    [catena_types:ty()],  % Type parameters that lack instance
    suggestions()
}.

-type reason() :: unifiable_heads | identical_instance.
-type suggestions() :: [string()].

%%%===================================================================
%%% Coherence Checking
%%%===================================================================

%% @doc Check an entire instance database for coherence violations
%%
%% Returns a list of all overlapping instance pairs found.
%% An empty list means the database is coherent.
%%
-spec check_instance_db(catena_instance:instance_db()) ->
    {ok, coherent} | {error, [coherence_error()]}.
check_instance_db(DB) ->
    % Get all traits
    AllTraits = maps:keys(DB),

    % Check each trait's instances for overlaps
    Errors = lists:flatmap(
        fun(Trait) ->
            Instances = catena_instance:get_instances(Trait, DB),
            check_trait_instances(Instances)
        end,
        AllTraits
    ),

    case Errors of
        [] -> {ok, coherent};
        _ -> {error, Errors}
    end.

%% @doc Check if a new instance would violate coherence
%%
%% Before adding an instance to the database, check if it overlaps
%% with any existing instances for the same trait.
%%
-spec check_new_instance(catena_instance:instance(),
                         catena_instance:instance_db()) ->
    {ok, no_overlap} | {error, [coherence_error()]}.
check_new_instance({instance, TraitName, _Args, _Loc} = New, DB) ->
    Existing = catena_instance:get_instances(TraitName, DB),
    Overlaps = find_overlaps(New, Existing),

    case Overlaps of
        [] -> {ok, no_overlap};
        _ ->
            Errors = [{overlap, New, Other, unifiable_heads}
                     || Other <- Overlaps],
            {error, Errors}
    end.

%% @doc Find all instances that overlap with a given instance
%%
%% An instance I1 overlaps with I2 if there exists types T such that
%% both I1 and I2 could be used to satisfy a constraint on T.
%%
%% Example:
%%   I1: Eq α (universal)
%%   I2: Eq Int (specific)
%%   These overlap because when T=Int, both could apply
%%
-spec find_overlaps(catena_instance:instance(), [catena_instance:instance()]) ->
    [catena_instance:instance()].
find_overlaps(Instance, Candidates) ->
    lists:filter(
        fun(Candidate) ->
            Instance =/= Candidate andalso  % Don't compare with self
            catena_instance:check_overlap(Instance, Candidate)
        end,
        Candidates
    ).

%% @doc Check if a set of constraints has complete instance coverage
%%
%% Coverage checking ensures that all required instances exist.
%% This is less strict than coherence - it just checks for missing
%% instances, not overlapping ones.
%%
-spec check_coverage(catena_constraint:constraint_set(),
                    catena_instance:instance_db()) ->
    {ok, complete} | {error, [coverage_error()]}.
check_coverage(Constraints, DB) ->
    Missing = lists:filtermap(
        fun(C) ->
            case catena_instance:has_instance(C, DB) of
                true -> false;
                false ->
                    {trait, Trait, Args, _} = C,
                    Error = {missing_instance, Trait, Args,
                            suggest_instances(Trait, Args)},
                    {true, Error}
            end
        end,
        Constraints
    ),

    case Missing of
        [] -> {ok, complete};
        _ -> {error, Missing}
    end.

%%%===================================================================
%%% Error Formatting
%%%===================================================================

%% @doc Format an overlap error for display
-spec format_overlap_error(catena_instance:instance(),
                          catena_instance:instance()) -> string().
format_overlap_error({instance, Trait, Args1, Loc1},
                    {instance, Trait, Args2, Loc2}) ->
    Args1Str = format_type_list(Args1),
    Args2Str = format_type_list(Args2),
    Loc1Str = format_location(Loc1),
    Loc2Str = format_location(Loc2),

    lists:flatten(io_lib:format(
        "Overlapping instances for trait '~p':~n"
        "  Instance 1: ~p ~ts at ~s~n"
        "  Instance 2: ~p ~ts at ~s~n"
        "  These instances overlap and could cause ambiguous resolution.",
        [Trait, Trait, Args1Str, Loc1Str, Trait, Args2Str, Loc2Str]
    )).

%% @doc Format a coverage error for display
-spec format_coverage_error(catena_constraint:trait_name(),
                           [catena_types:ty()]) -> string().
format_coverage_error(Trait, Args) ->
    ArgsStr = format_type_list(Args),
    lists:flatten(io_lib:format(
        "Missing instance for trait '~p' with types: ~s~n"
        "  No instance found that matches this constraint.",
        [Trait, ArgsStr]
    )).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% Check a list of instances for the same trait for overlaps
-spec check_trait_instances([catena_instance:instance()]) ->
    [coherence_error()].
check_trait_instances(Instances) ->
    check_trait_instances_pairs(Instances, []).

%% Check all pairs of instances for overlaps
-spec check_trait_instances_pairs([catena_instance:instance()],
                                 [coherence_error()]) ->
    [coherence_error()].
check_trait_instances_pairs([], Acc) ->
    lists:reverse(Acc);
check_trait_instances_pairs([I | Rest], Acc) ->
    % Check this instance against all remaining ones
    Overlaps = find_overlaps(I, Rest),
    Errors = [{overlap, I, Other, unifiable_heads} || Other <- Overlaps],
    check_trait_instances_pairs(Rest, Errors ++ Acc).

%% Suggest possible instances for a trait/type combination
-spec suggest_instances(catena_constraint:trait_name(),
                       [catena_types:ty()]) -> suggestions().
suggest_instances(Trait, Args) ->
    % Simple suggestions based on common patterns
    case {Trait, length(Args)} of
        {'Eq', 1} ->
            ["Implement Eq instance for this type",
             "Derive Eq automatically if possible"];
        {'Ord', 1} ->
            ["Implement Ord instance (requires Eq first)",
             "Define comparison function"];
        {'Functor', 1} ->
            ["Implement fmap for this type",
             "Ensure type is a type constructor"];
        _ ->
            [lists:flatten(io_lib:format("Implement ~p instance", [Trait]))]
    end.

%% Format a list of types for display
-spec format_type_list([catena_types:ty()]) -> string().
format_type_list(Types) ->
    TypeStrs = [catena_type_pp:format(T) || T <- Types],
    "[" ++ string:join(TypeStrs, ", ") ++ "]".

%% Format a source location
-spec format_location(catena_constraint:loc()) -> string().
format_location({file, Path, Line, Col}) ->
    lists:flatten(io_lib:format("~s:~p:~p", [Path, Line, Col]));
format_location(unknown) ->
    "unknown location".
