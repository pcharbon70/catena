%%%-------------------------------------------------------------------
%%% @doc Trait Instance Resolution
%%%
%%% This module implements instance resolution for trait constraints.
%%% It searches for trait implementations (instances) and unifies them
%%% with constraints to verify that trait requirements are satisfied.
%%%
%%% Key Concepts:
%%% - Instance: A trait implementation for specific types
%%% - Instance Resolution: Finding instances that match constraints
%%% - Unification: Checking if an instance satisfies a constraint
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_instance).

%% Instance management
-export([
    make_instance/2,
    make_instance/3,
    empty_instance_db/0,
    add_instance/2,
    get_instances/2
]).

%% Instance resolution
-export([
    resolve_constraint/2,
    resolve_constraints/2,
    find_matching_instances/2,
    unify_instance/2,
    %% Enhanced resolution with superclass constraints (Task 1.2.6.2)
    resolve_with_superclasses/3,
    get_superclass_constraints/3
]).

%% Internal functions exported for testing
-ifdef(TEST).
-export([resolve_constraint_depth/3]).
-endif.

%% Instance queries
-export([
    has_instance/2,
    check_overlap/2
]).

-export_type([instance/0, instance_db/0, resolution_result/0]).

%%%===================================================================
%%% Types
%%%===================================================================

%% Trait instance: implementation of a trait for specific type(s)
-type instance() :: {
    instance,
    catena_constraint:trait_name(),  % Trait being implemented
    [catena_types:ty()],              % Type parameters
    catena_constraint:loc()           % Source location
}.

%% Instance database: maps trait names to their instances
-type instance_db() :: #{catena_constraint:trait_name() => [instance()]}.

%% Resolution result
-type resolution_result() ::
    {ok, instance(), catena_type_subst:subst()} |  % Success with unifier
    {error, no_instance} |                         % No matching instance
    {error, {ambiguous, [instance()]}}.            % Multiple matches

%%%===================================================================
%%% Instance Construction
%%%===================================================================

%% @doc Create a trait instance
%%
%% Example:
%%   Instance = catena_instance:make_instance('Functor', [{tcon, list}], Loc)
%%
-spec make_instance(catena_constraint:trait_name(), [catena_types:ty()],
                    catena_constraint:loc()) -> instance().
make_instance(TraitName, TypeArgs, Loc)
  when is_atom(TraitName), is_list(TypeArgs) ->
    {instance, TraitName, TypeArgs, Loc}.

%% @doc Create a trait instance with unknown location
-spec make_instance(catena_constraint:trait_name(), [catena_types:ty()]) ->
    instance().
make_instance(TraitName, TypeArgs) ->
    make_instance(TraitName, TypeArgs, unknown).

%% @doc Create an empty instance database
-spec empty_instance_db() -> instance_db().
empty_instance_db() ->
    #{}.

%% @doc Add an instance to the database
%%
%% Instances are organized by trait name for efficient lookup.
%% Checks size limits to prevent DoS via instance database pollution.
%% Throws error if instance count limit would be exceeded.
%%
-spec add_instance(instance(), instance_db()) -> instance_db().
add_instance({instance, TraitName, _TypeArgs, _Loc} = Instance, DB) ->
    Existing = maps:get(TraitName, DB, []),
    NewCount = length(Existing) + 1,
    case catena_type_config:check_instance_count(TraitName, NewCount) of
        ok ->
            DB#{TraitName => [Instance | Existing]};
        {error, Error} ->
            error(Error)
    end.

%% @doc Get all instances for a trait
-spec get_instances(catena_constraint:trait_name(), instance_db()) ->
    [instance()].
get_instances(TraitName, DB) ->
    maps:get(TraitName, DB, []).

%%%===================================================================
%%% Instance Resolution
%%%===================================================================

%% @doc Resolve a single constraint
%%
%% Attempts to find a unique instance that satisfies the constraint.
%% Returns the instance and a substitution that makes them match.
%%
%% Resolution succeeds if:
%% 1. Exactly one instance matches the constraint
%% 2. The instance head unifies with the constraint
%%
%% Resolution fails if:
%% 1. No instances match (no_instance)
%% 2. Multiple instances match (ambiguous - coherence violation)
%%
-spec resolve_constraint(catena_constraint:constraint(), instance_db()) ->
    resolution_result().
resolve_constraint(Constraint, DB) ->
    resolve_constraint_depth(Constraint, DB, 0).

%% @doc Internal resolve_constraint with recursion depth tracking
%%
%% Tracks depth to prevent infinite loops from circular instance dependencies.
%% Future-proofed for when we add superclass constraint resolution.
-spec resolve_constraint_depth(catena_constraint:constraint(), instance_db(),
                                non_neg_integer()) ->
    resolution_result().
resolve_constraint_depth(_Constraint, _DB, Depth) ->
    % Check depth limit to prevent DoS via infinite recursion
    case catena_type_config:check_resolution_depth(Depth) of
        ok -> ok;
        {error, Error} -> error(Error)
    end,

    % Continue with normal resolution
    resolve_constraint_impl(_Constraint, _DB, Depth).

%% @doc Actual constraint resolution implementation
-spec resolve_constraint_impl(catena_constraint:constraint(), instance_db(),
                               non_neg_integer()) ->
    resolution_result().
resolve_constraint_impl({trait, TraitName, TypeArgs, _Loc}, DB, _Depth) ->
    Instances = get_instances(TraitName, DB),
    Matching = find_matching_instances({trait, TraitName, TypeArgs, unknown},
                                      Instances),
    case Matching of
        [] ->
            {error, no_instance};
        [Instance] ->
            % Unique instance found, unify it
            case unify_instance(Instance, {trait, TraitName, TypeArgs, unknown}) of
                {ok, Subst} ->
                    {ok, Instance, Subst};
                {error, Reason} ->
                    {error, Reason}
            end;
        Multiple ->
            % Ambiguous - multiple instances match
            {error, {ambiguous, Multiple}}
    end.

%% @doc Resolve a constraint with superclass constraint generation (Task 1.2.6.2)
%%
%% When resolving a constraint like `Ord Int`, this function:
%% 1. Finds the matching instance
%% 2. Looks up the trait's superclasses (e.g., Ord extends Eq)
%% 3. Generates superclass constraints (e.g., Eq Int)
%% 4. Returns both the resolution and new constraints to solve
%%
%% TraitDefs is a map of trait_name => {trait_name, [extends], location}
%%
-spec resolve_with_superclasses(
    catena_constraint:constraint(),
    instance_db(),
    catena_trait_hierarchy:trait_defs()
) ->
    {ok, instance(), catena_type_subst:subst(), [catena_constraint:constraint()]} |
    {error, term()}.
resolve_with_superclasses(Constraint, DB, TraitDefs) ->
    case resolve_constraint(Constraint, DB) of
        {ok, Instance, Subst} ->
            %% Generate superclass constraints
            SuperConstraints = get_superclass_constraints(Constraint, Subst, TraitDefs),
            {ok, Instance, Subst, SuperConstraints};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Generate superclass constraints for a resolved constraint
%%
%% Given Constraint = Ord Int and TraitDefs showing Ord extends Eq,
%% returns [Eq Int] as constraints that must also be satisfied.
%%
-spec get_superclass_constraints(
    catena_constraint:constraint(),
    catena_type_subst:subst(),
    catena_trait_hierarchy:trait_defs()
) -> [catena_constraint:constraint()].
get_superclass_constraints({trait, TraitName, TypeArgs, Loc}, Subst, TraitDefs) ->
    %% Get the trait's superclasses
    case catena_trait_hierarchy:get_extends(TraitName, TraitDefs) of
        {ok, Supertraits} ->
            %% For each supertrait, create a constraint with the same type args
            %% Apply the substitution to propagate any type variable bindings
            lists:map(
                fun(SupertraitName) ->
                    %% Apply substitution to type args
                    SubstArgs = [catena_type_subst:apply(Subst, Arg) || Arg <- TypeArgs],
                    catena_constraint:trait_constraint(SupertraitName, SubstArgs, Loc)
                end,
                Supertraits
            );
        {error, not_found} ->
            %% Trait not in definitions (might be builtin), no superclasses
            []
    end.

%% @doc Resolve multiple constraints
%%
%% Attempts to resolve all constraints in the set.
%% Returns:
%% - {ok, Solutions} where Solutions is [{Constraint, Instance, Subst}]
%% - {error, {Constraint, Reason}} for the first failing constraint
%%
-spec resolve_constraints(catena_constraint:constraint_set(), instance_db()) ->
    {ok, [{catena_constraint:constraint(), instance(), catena_type_subst:subst()}]} |
    {error, {catena_constraint:constraint(), term()}}.
resolve_constraints(Constraints, DB) ->
    resolve_constraints_acc(Constraints, DB, []).

%% @doc Find instances that could potentially match a constraint
%%
%% This is a filtering step before unification. An instance "matches"
%% if its trait name is the same and it has the same arity.
%%
-spec find_matching_instances(catena_constraint:constraint(), [instance()]) ->
    [instance()].
find_matching_instances({trait, TraitName, TypeArgs, _}, Instances) ->
    Arity = length(TypeArgs),
    Filtered = lists:filter(
        fun({instance, Name, Args, _}) ->
            Name =:= TraitName andalso length(Args) =:= Arity
        end,
        Instances
    ),
    % Now filter further by checking if unification actually works
    Constraint = {trait, TraitName, TypeArgs, unknown},
    lists:filter(
        fun(Instance) ->
            case unify_instance(Instance, Constraint) of
                {ok, _} -> true;
                {error, _} -> false
            end
        end,
        Filtered
    ).

%% @doc Unify an instance with a constraint
%%
%% Attempts to unify the instance's type parameters with the
%% constraint's type arguments. Returns a substitution if successful.
%%
%% Example:
%%   Instance: Functor [List α]
%%   Constraint: Functor [List Int]
%%   Result: {ok, {α -> Int}}
%%
-spec unify_instance(instance(), catena_constraint:constraint()) ->
    {ok, catena_type_subst:subst()} | {error, term()}.
unify_instance({instance, _TraitName1, InstanceArgs, _Loc},
               {trait, _TraitName2, ConstraintArgs, _}) ->
    % Use the unification algorithm to match instance args with constraint args
    try
        Subst = unify_type_lists(InstanceArgs, ConstraintArgs,
                                catena_type_subst:empty()),
        {ok, Subst}
    catch
        error:Reason ->
            {error, Reason}
    end.

%%%===================================================================
%%% Instance Queries
%%%===================================================================

%% @doc Check if an instance exists for a constraint
%%
%% This is a simpler check than full resolution - just checks
%% if at least one instance could match.
%%
-spec has_instance(catena_constraint:constraint(), instance_db()) -> boolean().
has_instance({trait, TraitName, TypeArgs, _}, DB) ->
    Instances = get_instances(TraitName, DB),
    Matching = find_matching_instances({trait, TraitName, TypeArgs, unknown},
                                      Instances),
    length(Matching) > 0.

%% @doc Check if two instances overlap
%%
%% Two instances overlap if there exists a substitution that makes
%% their heads unify. Overlapping instances violate coherence.
%%
%% Example of overlap:
%%   Instance 1: Eq (List α)
%%   Instance 2: Eq (List Int)
%%   These overlap because α can be Int
%%
-spec check_overlap(instance(), instance()) -> boolean().
check_overlap({instance, Trait1, Args1, _}, {instance, Trait2, Args2, _}) ->
    % Different traits never overlap
    case Trait1 =:= Trait2 of
        false ->
            false;
        true ->
            % Same trait - check if args can unify
            try
                _Subst = unify_type_lists(Args1, Args2, catena_type_subst:empty()),
                true  % Unification succeeded, instances overlap
            catch
                error:_ ->
                    false  % Unification failed, no overlap
            end
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% Resolve constraints accumulator
-spec resolve_constraints_acc(
    catena_constraint:constraint_set(),
    instance_db(),
    [{catena_constraint:constraint(), instance(), catena_type_subst:subst()}]
) ->
    {ok, [{catena_constraint:constraint(), instance(), catena_type_subst:subst()}]} |
    {error, {catena_constraint:constraint(), term()}}.
resolve_constraints_acc([], _DB, Acc) ->
    {ok, lists:reverse(Acc)};
resolve_constraints_acc([C | Rest], DB, Acc) ->
    case resolve_constraint(C, DB) of
        {ok, Instance, Subst} ->
            resolve_constraints_acc(Rest, DB, [{C, Instance, Subst} | Acc]);
        {error, Reason} ->
            {error, {C, Reason}}
    end.

%% Unify two lists of types pairwise
%%
%% This is a helper for instance resolution. It unifies corresponding
%% elements and accumulates the substitution.
%%
-spec unify_type_lists([catena_types:ty()], [catena_types:ty()],
                       catena_type_subst:subst()) ->
    catena_type_subst:subst().
unify_type_lists([], [], Subst) ->
    Subst;
unify_type_lists([T1 | Rest1], [T2 | Rest2], Subst) ->
    % Apply current substitution to both types
    T1_subst = catena_type_subst:apply(Subst, T1),
    T2_subst = catena_type_subst:apply(Subst, T2),

    % Unify the substituted types
    case catena_infer_unify:unify_types(T1_subst, T2_subst) of
        {ok, NewSubst} ->
            % Compose substitutions
            ComposedSubst = catena_type_subst:compose(NewSubst, Subst),
            unify_type_lists(Rest1, Rest2, ComposedSubst);
        {error, Reason} ->
            error(Reason)
    end;
unify_type_lists(_, _, _) ->
    error(arity_mismatch).
