%%%-------------------------------------------------------------------
%%% @doc Trait Method Type Inference (Task 1.2.6.3)
%%%
%%% Validates that instance method implementations match their trait
%%% declarations. Performs type checking and inference on method
%%% signatures and implementations.
%%%
%%% Key Responsibilities:
%%% - Check method signatures match trait declarations
%%% - Verify method arity is correct
%%% - Check all required methods are implemented
%%% - Detect extra methods not in trait
%%% @end
%%%-------------------------------------------------------------------
-module(catena_trait_methods).

-export([
    %% Trait method validation
    check_trait_methods/1,
    check_instance_methods/3,

    %% Method signature operations
    substitute_method_type/3,
    get_method_signature/2,

    %% Error types
    format_error/1
]).

%%====================================================================
%% Type Definitions
%%====================================================================

%% Trait definition with methods
%% {trait_name, type_params, extends, methods, location}
-type trait_def() :: {
    atom(),           % Trait name
    [atom()],         % Type parameters (e.g., [f] for Functor f)
    [atom()],         % Extended traits
    [method_sig()],   % Method signatures
    term()            % Location
}.

%% Method signature: {method_name, type_expr}
-type method_sig() :: {atom(), catena_types:ty()}.

%% Instance definition with methods
%% {instance, trait_name, type_args, methods, location}
-type instance_def() :: {
    instance,
    atom(),                % Trait name
    [catena_types:ty()],   % Type arguments
    [method_impl()],       % Method implementations
    term()                 % Location
}.

%% Method implementation: {method_name, arity, implementation_type}
-type method_impl() :: {atom(), non_neg_integer(), catena_types:ty()}.

%% Error types for method validation
-type method_error() ::
    {missing_method, atom(), atom()} |          % {TraitName, MethodName}
    {extra_method, atom(), atom()} |            % {InstanceTrait, MethodName}
    {arity_mismatch, atom(), atom(), integer(), integer()} |  % {Trait, Method, Expected, Actual}
    {type_mismatch, atom(), atom(), catena_types:ty(), catena_types:ty()} |  % {Trait, Method, Expected, Actual}
    {invalid_method_signature, atom(), atom()}. % {Trait, Method}

-export_type([trait_def/0, instance_def/0, method_sig/0, method_impl/0, method_error/0]).

%%====================================================================
%% Trait Method Validation
%%====================================================================

%% @doc Check that trait method signatures are well-formed
%%
%% Validates that all methods in a trait have valid type signatures.
-spec check_trait_methods(trait_def()) -> ok | {error, [method_error()]}.
check_trait_methods({TraitName, _TypeParams, _Extends, Methods, _Loc}) ->
    Errors = lists:filtermap(
        fun({MethodName, Type}) ->
            case is_valid_type(Type) of
                true -> false;
                false -> {true, {invalid_method_signature, TraitName, MethodName}}
            end
        end,
        Methods
    ),
    case Errors of
        [] -> ok;
        _ -> {error, Errors}
    end.

%% @doc Check that instance methods match trait declarations
%%
%% Validates:
%% - All required methods are implemented
%% - No extra methods beyond trait definition
%% - Method arities match
%% - Method types are compatible (via unification)
%%
-spec check_instance_methods(instance_def(), trait_def(), catena_type_subst:subst()) ->
    ok | {error, [method_error()]}.
check_instance_methods(
    {instance, _InstTraitName, TypeArgs, ImplMethods, _InstLoc},
    {TraitName, TypeParams, _Extends, TraitMethods, _TraitLoc},
    _BaseSubst
) ->
    %% Build substitution from trait params to instance type args
    %% E.g., f -> List for "instance Functor List"
    ParamSubst = build_param_subst(TypeParams, TypeArgs),

    %% Get implemented method names
    ImplNames = [Name || {Name, _Arity, _Type} <- ImplMethods],

    %% Get required method names from trait
    RequiredNames = [Name || {Name, _Type} <- TraitMethods],

    %% Check for missing methods
    MissingErrors = lists:filtermap(
        fun(Required) ->
            case lists:member(Required, ImplNames) of
                true -> false;
                false -> {true, {missing_method, TraitName, Required}}
            end
        end,
        RequiredNames
    ),

    %% Check for extra methods
    ExtraErrors = lists:filtermap(
        fun(Impl) ->
            case lists:member(Impl, RequiredNames) of
                true -> false;
                false -> {true, {extra_method, TraitName, Impl}}
            end
        end,
        ImplNames
    ),

    %% Check arity and type compatibility for implemented methods
    CompatErrors = check_method_compatibility(ImplMethods, TraitMethods, TraitName, ParamSubst),

    AllErrors = MissingErrors ++ ExtraErrors ++ CompatErrors,
    case AllErrors of
        [] -> ok;
        _ -> {error, AllErrors}
    end.

%%====================================================================
%% Method Signature Operations
%%====================================================================

%% @doc Substitute type parameters in a method signature
%%
%% Given a method signature from a trait (e.g., `(a -> b) -> f a -> f b`)
%% and a substitution (e.g., `f -> List`), produces the specialized type
%% (e.g., `(a -> b) -> List a -> List b`).
%%
-spec substitute_method_type(catena_types:ty(), [atom()], [catena_types:ty()]) ->
    catena_types:ty().
substitute_method_type(MethodType, TypeParams, TypeArgs) ->
    Subst = build_param_subst(TypeParams, TypeArgs),
    apply_param_subst(Subst, MethodType).

%% @doc Get method signature from trait by name
-spec get_method_signature(atom(), [method_sig()]) ->
    {ok, catena_types:ty()} | {error, not_found}.
get_method_signature(MethodName, Methods) ->
    case lists:keyfind(MethodName, 1, Methods) of
        {MethodName, Type} -> {ok, Type};
        false -> {error, not_found}
    end.

%%====================================================================
%% Error Formatting
%%====================================================================

%% @doc Format a method error for display
-spec format_error(method_error()) -> string().
format_error({missing_method, TraitName, MethodName}) ->
    lists:flatten(io_lib:format(
        "Missing method '~s' in instance of trait '~s'~n"
        "All methods declared in the trait must be implemented.",
        [atom_to_list(MethodName), atom_to_list(TraitName)]
    ));
format_error({extra_method, TraitName, MethodName}) ->
    lists:flatten(io_lib:format(
        "Extra method '~s' not declared in trait '~s'~n"
        "Instance methods must match the trait declaration.",
        [atom_to_list(MethodName), atom_to_list(TraitName)]
    ));
format_error({arity_mismatch, TraitName, MethodName, Expected, Actual}) ->
    lists:flatten(io_lib:format(
        "Method '~s.~s' arity mismatch~n"
        "  Expected: ~p arguments~n"
        "  Got:      ~p arguments~n"
        "The implementation must have the same arity as the declaration.",
        [atom_to_list(TraitName), atom_to_list(MethodName), Expected, Actual]
    ));
format_error({type_mismatch, TraitName, MethodName, Expected, Actual}) ->
    ExpectedStr = catena_type_pp:pp_type(Expected),
    ActualStr = catena_type_pp:pp_type(Actual),
    lists:flatten(io_lib:format(
        "Method '~s.~s' type mismatch~n"
        "  Expected: ~s~n"
        "  Got:      ~s~n"
        "The implementation type must be compatible with the declaration.",
        [atom_to_list(TraitName), atom_to_list(MethodName), ExpectedStr, ActualStr]
    ));
format_error({invalid_method_signature, TraitName, MethodName}) ->
    lists:flatten(io_lib:format(
        "Invalid method signature for '~s.~s'~n"
        "The method type expression is malformed.",
        [atom_to_list(TraitName), atom_to_list(MethodName)]
    )).

%%====================================================================
%% Internal Functions
%%====================================================================

%% Build substitution from type parameters to type arguments
build_param_subst(TypeParams, TypeArgs) ->
    lists:zip(TypeParams, TypeArgs).

%% Apply parameter substitution to a type
%% This substitutes type parameter names (atoms) with their values
apply_param_subst(Subst, {tvar, Id}) ->
    %% Type variables are not affected by param substitution
    {tvar, Id};
apply_param_subst(Subst, {tcon, Name}) when is_atom(Name) ->
    %% Check if this is a type parameter
    case lists:keyfind(Name, 1, Subst) of
        {Name, Replacement} -> Replacement;
        false -> {tcon, Name}
    end;
apply_param_subst(Subst, {tapp, Con, Args}) ->
    NewCon = apply_param_subst(Subst, Con),
    NewArgs = [apply_param_subst(Subst, A) || A <- Args],
    {tapp, NewCon, NewArgs};
apply_param_subst(Subst, {fun_type, Param, Return, Effects}) ->
    {fun_type,
     apply_param_subst(Subst, Param),
     apply_param_subst(Subst, Return),
     Effects};
apply_param_subst(Subst, {record, Fields}) ->
    NewFields = [{N, apply_param_subst(Subst, T)} || {N, T} <- Fields],
    {record, NewFields};
apply_param_subst(Subst, {variant, Constructors}) ->
    NewCons = [{N, [apply_param_subst(Subst, T) || T <- Ts]} || {N, Ts} <- Constructors],
    {variant, NewCons};
apply_param_subst(_Subst, Other) ->
    %% Literals and other types pass through unchanged
    Other.

%% Check compatibility of implemented methods against trait declarations
check_method_compatibility(ImplMethods, TraitMethods, TraitName, ParamSubst) ->
    lists:filtermap(
        fun({ImplName, ImplArity, ImplType}) ->
            case get_method_signature(ImplName, TraitMethods) of
                {ok, TraitType} ->
                    %% Apply parameter substitution to trait type
                    SpecializedType = apply_param_subst(ParamSubst, TraitType),
                    %% Check arity
                    TraitArity = count_function_arity(SpecializedType),
                    case ImplArity =:= TraitArity of
                        false ->
                            {true, {arity_mismatch, TraitName, ImplName, TraitArity, ImplArity}};
                        true ->
                            %% Check type compatibility via unification
                            case check_type_compatible(ImplType, SpecializedType) of
                                ok -> false;
                                {error, _} ->
                                    {true, {type_mismatch, TraitName, ImplName, SpecializedType, ImplType}}
                            end
                    end;
                {error, not_found} ->
                    %% Method not in trait - will be caught by extra_method check
                    false
            end
        end,
        ImplMethods
    ).

%% Count function arity (number of arguments)
count_function_arity({fun_type, _Param, Return, _Effects}) ->
    1 + count_function_arity(Return);
count_function_arity(_) ->
    0.

%% Check if two types are compatible via unification
check_type_compatible(ImplType, TraitType) ->
    case catena_infer_unify:unify_types(ImplType, TraitType) of
        {ok, _Subst} -> ok;
        {error, _} = Err -> Err
    end.

%% Check if a type is valid (basic structural check)
is_valid_type({tvar, _}) -> true;
is_valid_type({tcon, Name}) when is_atom(Name) -> true;
is_valid_type({tapp, Con, Args}) ->
    is_valid_type(Con) andalso lists:all(fun is_valid_type/1, Args);
is_valid_type({fun_type, P, R, _E}) ->
    is_valid_type(P) andalso is_valid_type(R);
is_valid_type({record, Fields}) ->
    lists:all(fun({_, T}) -> is_valid_type(T) end, Fields);
is_valid_type({variant, Cons}) ->
    lists:all(fun({_, Ts}) -> lists:all(fun is_valid_type/1, Ts) end, Cons);
is_valid_type(_) -> false.
