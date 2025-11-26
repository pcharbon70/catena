%%%-------------------------------------------------------------------
%%% @doc Trait Method Resolution for Type Inference
%%%
%%% This module integrates trait instance resolution into type inference.
%%% When type-checking a call to a trait method (like `map`, `chain`,
%%% `combine`), this module:
%%%
%%% 1. Identifies the trait being used based on the method name
%%% 2. Generates a trait constraint from the argument types
%%% 3. Resolves the constraint against the instance database
%%% 4. Returns the resolved method type
%%%
%%% Trait Methods (from prelude.cat):
%%% - map: Mapper f => (a -> b) -> f a -> f b
%%% - apply: Applicator f => f (a -> b) -> f a -> f b
%%% - wrap: Applicator f => a -> f a
%%% - chain: Chainable m => (a -> m b) -> m a -> m b
%%% - combine: Combiner a => a -> a -> a
%%% - equals: Comparable a => a -> a -> Bool
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_trait_resolve).

-export([
    %% Method resolution
    resolve_method_type/4,
    is_trait_method/1,
    method_to_trait/1,

    %% Instance database management
    get_instance_db/0,

    %% Constraint generation
    generate_method_constraint/3
]).

%%%===================================================================
%%% Types
%%%===================================================================

-type method_name() :: map | apply | wrap | chain | combine | equals | not_equals.
-type trait_name() :: 'Mapper' | 'Applicator' | 'Chainable' | 'Combiner' | 'Comparable'.

%%%===================================================================
%%% Instance Database
%%%===================================================================

%% @doc Get the combined instance database (builtins + stdlib)
-spec get_instance_db() -> catena_instance:instance_db().
get_instance_db() ->
    %% Combine builtin and stdlib instances
    Builtin = catena_builtin_instances:builtin_instance_db(),
    Stdlib = catena_stdlib_instances:stdlib_instance_db(),
    merge_instance_dbs(Builtin, Stdlib).

%% @doc Merge two instance databases
-spec merge_instance_dbs(catena_instance:instance_db(), catena_instance:instance_db()) ->
    catena_instance:instance_db().
merge_instance_dbs(DB1, DB2) ->
    maps:fold(
        fun(TraitName, Instances, Acc) ->
            ExistingInstances = maps:get(TraitName, Acc, []),
            Acc#{TraitName => Instances ++ ExistingInstances}
        end,
        DB1,
        DB2
    ).

%%%===================================================================
%%% Method Recognition
%%%===================================================================

%% @doc Check if a name is a trait method
-spec is_trait_method(atom()) -> boolean().
is_trait_method(map) -> true;
is_trait_method(apply) -> true;
is_trait_method(wrap) -> true;
is_trait_method(chain) -> true;
is_trait_method(combine) -> true;
is_trait_method(equals) -> true;
is_trait_method(not_equals) -> true;
is_trait_method('not') -> false;  % Built-in, not a trait method
is_trait_method(_) -> false.

%% @doc Get the trait for a method name
-spec method_to_trait(method_name()) -> trait_name().
method_to_trait(map) -> 'Mapper';
method_to_trait(apply) -> 'Applicator';
method_to_trait(wrap) -> 'Applicator';
method_to_trait(chain) -> 'Chainable';
method_to_trait(combine) -> 'Combiner';
method_to_trait(equals) -> 'Comparable';
method_to_trait(not_equals) -> 'Comparable'.

%%%===================================================================
%%% Method Type Resolution
%%%===================================================================

%% @doc Resolve the type of a trait method call
%%
%% Given a method name and argument types, resolves the instance and
%% returns the specialized method type.
%%
%% Example:
%%   resolve_method_type(map, [FuncType, {tapp, {tcon, 'Maybe'}, [Int]}], DB, State)
%%   => {ok, ResultType, State'}
%%
-spec resolve_method_type(atom(), [catena_types:ty()],
                          catena_instance:instance_db(),
                          catena_infer_state:state()) ->
    {ok, catena_types:ty(), catena_infer_state:state()} |
    {error, term()}.
resolve_method_type(MethodName, ArgTypes, DB, State) ->
    case is_trait_method(MethodName) of
        false ->
            {error, {not_trait_method, MethodName}};
        true ->
            TraitName = method_to_trait(MethodName),
            resolve_for_trait(TraitName, MethodName, ArgTypes, DB, State)
    end.

%% @doc Resolve method for a specific trait
-spec resolve_for_trait(trait_name(), method_name(), [catena_types:ty()],
                        catena_instance:instance_db(),
                        catena_infer_state:state()) ->
    {ok, catena_types:ty(), catena_infer_state:state()} |
    {error, term()}.

%% Mapper.map : (a -> b) -> f a -> f b
resolve_for_trait('Mapper', map, ArgTypes, DB, State) ->
    case ArgTypes of
        [_FuncType, ContainerType] ->
            %% Extract the type constructor from the container
            case extract_type_constructor(ContainerType) of
                {ok, TypeCon} ->
                    %% Generate constraint: Mapper TypeCon
                    Constraint = catena_constraint:trait_constraint('Mapper', [TypeCon], unknown),

                    %% Resolve the constraint
                    case catena_instance:resolve_constraint(Constraint, DB) of
                        {ok, _Instance, _Subst} ->
                            %% Instance found! Generate the result type
                            %% map : (a -> b) -> f a -> f b
                            %% Result is f b where b is the return type of the function
                            {ResultVar, State1} = catena_infer_state:fresh_var(State),
                            ResultType = apply_type_constructor(TypeCon, ResultVar),
                            {ok, ResultType, State1};
                        {error, no_instance} ->
                            {error, {no_instance, 'Mapper', TypeCon}};
                        {error, {ambiguous, Instances}} ->
                            {error, {ambiguous_instance, 'Mapper', TypeCon, Instances}}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            {error, {wrong_arity, map, 2, length(ArgTypes)}}
    end;

%% Chainable.chain : (a -> m b) -> m a -> m b
resolve_for_trait('Chainable', chain, ArgTypes, DB, State) ->
    case ArgTypes of
        [_FuncType, ContainerType] ->
            case extract_type_constructor(ContainerType) of
                {ok, TypeCon} ->
                    Constraint = catena_constraint:trait_constraint('Chainable', [TypeCon], unknown),
                    case catena_instance:resolve_constraint(Constraint, DB) of
                        {ok, _Instance, _Subst} ->
                            {ResultVar, State1} = catena_infer_state:fresh_var(State),
                            ResultType = apply_type_constructor(TypeCon, ResultVar),
                            {ok, ResultType, State1};
                        {error, no_instance} ->
                            {error, {no_instance, 'Chainable', TypeCon}};
                        {error, {ambiguous, Instances}} ->
                            {error, {ambiguous_instance, 'Chainable', TypeCon, Instances}}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            {error, {wrong_arity, chain, 2, length(ArgTypes)}}
    end;

%% Combiner.combine : a -> a -> a
resolve_for_trait('Combiner', combine, ArgTypes, DB, State) ->
    case ArgTypes of
        [Type1, _Type2] ->
            Constraint = catena_constraint:trait_constraint('Combiner', [Type1], unknown),
            case catena_instance:resolve_constraint(Constraint, DB) of
                {ok, _Instance, _Subst} ->
                    %% Result type is the same as input type
                    {ok, Type1, State};
                {error, no_instance} ->
                    {error, {no_instance, 'Combiner', Type1}};
                {error, {ambiguous, Instances}} ->
                    {error, {ambiguous_instance, 'Combiner', Type1, Instances}}
            end;
        _ ->
            {error, {wrong_arity, combine, 2, length(ArgTypes)}}
    end;

%% Comparable.equals : a -> a -> Bool
resolve_for_trait('Comparable', equals, ArgTypes, DB, State) ->
    case ArgTypes of
        [Type1, _Type2] ->
            Constraint = catena_constraint:trait_constraint('Comparable', [Type1], unknown),
            case catena_instance:resolve_constraint(Constraint, DB) of
                {ok, _Instance, _Subst} ->
                    {ok, {tcon, bool}, State};
                {error, no_instance} ->
                    {error, {no_instance, 'Comparable', Type1}};
                {error, {ambiguous, Instances}} ->
                    {error, {ambiguous_instance, 'Comparable', Type1, Instances}}
            end;
        _ ->
            {error, {wrong_arity, equals, 2, length(ArgTypes)}}
    end;

%% Comparable.not_equals (derived from equals)
resolve_for_trait('Comparable', not_equals, ArgTypes, DB, State) ->
    resolve_for_trait('Comparable', equals, ArgTypes, DB, State);

%% Applicator.apply : f (a -> b) -> f a -> f b
resolve_for_trait('Applicator', apply, ArgTypes, DB, State) ->
    case ArgTypes of
        [FuncContainerType, _ArgContainerType] ->
            case extract_type_constructor(FuncContainerType) of
                {ok, TypeCon} ->
                    Constraint = catena_constraint:trait_constraint('Applicator', [TypeCon], unknown),
                    case catena_instance:resolve_constraint(Constraint, DB) of
                        {ok, _Instance, _Subst} ->
                            {ResultVar, State1} = catena_infer_state:fresh_var(State),
                            ResultType = apply_type_constructor(TypeCon, ResultVar),
                            {ok, ResultType, State1};
                        {error, no_instance} ->
                            {error, {no_instance, 'Applicator', TypeCon}};
                        {error, {ambiguous, Instances}} ->
                            {error, {ambiguous_instance, 'Applicator', TypeCon, Instances}}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            {error, {wrong_arity, apply, 2, length(ArgTypes)}}
    end;

%% Applicator.wrap : a -> f a
resolve_for_trait('Applicator', wrap, ArgTypes, DB, State) ->
    case ArgTypes of
        [_ElemType] ->
            %% wrap requires knowing which type constructor to use
            %% This is typically determined by context, not argument type
            %% For now, return a fresh type variable for the container
            {ContainerVar, State1} = catena_infer_state:fresh_var(State),
            {ok, ContainerVar, State1};
        _ ->
            {error, {wrong_arity, wrap, 1, length(ArgTypes)}}
    end;

resolve_for_trait(TraitName, MethodName, _ArgTypes, _DB, _State) ->
    {error, {unknown_method, TraitName, MethodName}}.

%%%===================================================================
%%% Constraint Generation
%%%===================================================================

%% @doc Generate a trait constraint for a method call
-spec generate_method_constraint(atom(), [catena_types:ty()], term()) ->
    {ok, catena_constraint:constraint()} | {error, term()}.
generate_method_constraint(MethodName, ArgTypes, Loc) ->
    case is_trait_method(MethodName) of
        false ->
            {error, {not_trait_method, MethodName}};
        true ->
            TraitName = method_to_trait(MethodName),
            case method_constraint_type(MethodName, ArgTypes) of
                {ok, ConstraintTypes} ->
                    {ok, catena_constraint:trait_constraint(TraitName, ConstraintTypes, Loc)};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc Determine the types to use in the constraint
-spec method_constraint_type(method_name(), [catena_types:ty()]) ->
    {ok, [catena_types:ty()]} | {error, term()}.
method_constraint_type(map, [_FuncType, ContainerType]) ->
    case extract_type_constructor(ContainerType) of
        {ok, TypeCon} -> {ok, [TypeCon]};
        Error -> Error
    end;
method_constraint_type(chain, [_FuncType, ContainerType]) ->
    case extract_type_constructor(ContainerType) of
        {ok, TypeCon} -> {ok, [TypeCon]};
        Error -> Error
    end;
method_constraint_type(apply, [ContainerType, _]) ->
    case extract_type_constructor(ContainerType) of
        {ok, TypeCon} -> {ok, [TypeCon]};
        Error -> Error
    end;
method_constraint_type(wrap, [ElemType]) ->
    %% For wrap, we need the element type
    {ok, [ElemType]};
method_constraint_type(combine, [Type, _]) ->
    {ok, [Type]};
method_constraint_type(equals, [Type, _]) ->
    {ok, [Type]};
method_constraint_type(not_equals, [Type, _]) ->
    {ok, [Type]};
method_constraint_type(Method, Args) ->
    {error, {invalid_args, Method, length(Args)}}.

%%%===================================================================
%%% Type Utilities
%%%===================================================================

%% @doc Extract the type constructor from a container type
%%
%% Examples:
%%   Maybe Int -> Maybe
%%   List String -> list
%%   Either Error Int -> Either Error
-spec extract_type_constructor(catena_types:ty()) ->
    {ok, catena_types:ty()} | {error, term()}.
extract_type_constructor({tapp, TypeCon, _Args}) ->
    {ok, TypeCon};
extract_type_constructor({tcon, Name} = Type) ->
    %% A bare type constructor like 'Maybe' or 'list'
    %% This happens when we have Maybe without type parameters yet
    {ok, Type};
extract_type_constructor({tvar, _} = Var) ->
    %% Type variable - could be any type constructor
    %% Return as-is for unification
    {ok, Var};
extract_type_constructor(Type) ->
    {error, {not_container_type, Type}}.

%% @doc Apply a type constructor to a type argument
%%
%% Examples:
%%   apply_type_constructor(Maybe, Int) -> Maybe Int
%%   apply_type_constructor(list, String) -> List String
-spec apply_type_constructor(catena_types:ty(), catena_types:ty()) -> catena_types:ty().
apply_type_constructor({tcon, _} = TypeCon, ArgType) ->
    {tapp, TypeCon, [ArgType]};
apply_type_constructor({tapp, TypeCon, ExistingArgs}, ArgType) ->
    %% Partially applied - add another argument
    {tapp, TypeCon, ExistingArgs ++ [ArgType]};
apply_type_constructor(TypeCon, ArgType) ->
    %% Generic case
    {tapp, TypeCon, [ArgType]}.
