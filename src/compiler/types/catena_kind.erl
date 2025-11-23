%%%-------------------------------------------------------------------
%%% @doc Kind Checking for Higher-Kinded Types (Section 1.5.3)
%%%
%%% This module implements kind checking to validate that type
%%% constructors are used correctly in traits and instances.
%%%
%%% Kinds:
%%% - star: Type (kind of concrete types like Int, Bool)
%%% - {arrow, K1, K2}: K1 -> K2 (kind of type constructors)
%%%
%%% Examples:
%%% - Int : Type
%%% - Maybe : Type -> Type
%%% - Either : Type -> Type -> Type
%%% - Either String : Type -> Type
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_kind).

-export([
    %% Kind representation
    star/0,
    arrow/2,

    %% Kind checking
    check_trait_kind/1,
    check_instance_kind/3,
    infer_type_kind/2,

    %% Kind environment
    empty_kind_env/0,
    add_type_kind/3,
    get_type_kind/2,
    build_kind_env/1,

    %% Kind validation
    validate_hkt/2,
    kinds_compatible/2,

    %% Pretty printing
    format_kind/1
]).

-export_type([kind/0, kind_env/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type kind() :: star | {arrow, kind(), kind()}.
-type kind_env() :: #{atom() => kind()}.

%%%===================================================================
%%% Kind Constructors
%%%===================================================================

%% @doc The kind of concrete types
-spec star() -> kind().
star() -> star.

%% @doc Arrow kind (type constructor kind)
-spec arrow(kind(), kind()) -> kind().
arrow(K1, K2) -> {arrow, K1, K2}.

%%%===================================================================
%%% Kind Environment
%%%===================================================================

%% @doc Create empty kind environment
-spec empty_kind_env() -> kind_env().
empty_kind_env() -> #{}.

%% @doc Add type constructor kind to environment
-spec add_type_kind(atom(), kind(), kind_env()) -> kind_env().
add_type_kind(Name, Kind, Env) ->
    Env#{Name => Kind}.

%% @doc Get kind of type constructor
-spec get_type_kind(atom(), kind_env()) -> {ok, kind()} | {error, not_found}.
get_type_kind(Name, Env) ->
    case maps:find(Name, Env) of
        {ok, Kind} -> {ok, Kind};
        error -> {error, not_found}
    end.

%% @doc Build kind environment from declarations
%%
%% Extracts type declarations and assigns kinds based on arity.
%% Also adds built-in types.
-spec build_kind_env([term()]) -> kind_env().
build_kind_env(Declarations) ->
    %% Start with built-in types
    Env0 = builtin_kinds(),

    %% Add kinds for declared types
    lists:foldl(
        fun(Decl, Env) ->
            case Decl of
                {type_decl, Name, TypeVars, _Constructors, _Derives, _Loc} ->
                    Arity = length(TypeVars),
                    Kind = kind_from_arity(Arity),
                    add_type_kind(Name, Kind, Env);
                _ ->
                    Env
            end
        end,
        Env0,
        Declarations
    ).

%% @doc Built-in type kinds
builtin_kinds() ->
    #{
        'Int' => star(),
        'Bool' => star(),
        'String' => star(),
        'Unit' => star(),
        'Natural' => star(),
        'Float' => star(),
        'Char' => star(),
        %% Standard library types
        'Maybe' => arrow(star(), star()),
        'List' => arrow(star(), star()),
        'Either' => arrow(star(), arrow(star(), star())),
        'Ordering' => star()
    }.

%% @doc Create kind from arity
kind_from_arity(0) -> star();
kind_from_arity(N) when N > 0 ->
    arrow(star(), kind_from_arity(N - 1)).

%%%===================================================================
%%% Kind Checking
%%%===================================================================

%% @doc Check kind of trait type parameter
%%
%% Analyzes how the type parameter is used in trait methods to infer its kind.
%% If applied to arguments, it has arrow kind; otherwise star.
-spec check_trait_kind(term()) -> {ok, [{atom(), kind()}]} | {error, term()}.
check_trait_kind({trait_decl, _Name, TypeVars, _Supers, Methods, _Loc}) ->
    %% Infer kinds for each type variable from method signatures
    Kinds = lists:map(
        fun(Var) ->
            Kind = infer_var_kind_from_methods(Var, Methods),
            {Var, Kind}
        end,
        TypeVars
    ),
    {ok, Kinds};
check_trait_kind(_) ->
    {error, not_a_trait}.

%% @doc Infer kind of a type variable from how it's used in methods
infer_var_kind_from_methods(Var, Methods) ->
    %% Check if Var is applied to arguments anywhere in method signatures
    IsApplied = lists:any(
        fun(Method) ->
            case Method of
                {trait_sig, _Name, Type, _Loc} ->
                    is_var_applied(Var, Type);
                _ ->
                    false
            end
        end,
        Methods
    ),
    case IsApplied of
        true -> arrow(star(), star());  % f a -> f has kind Type -> Type
        false -> star()
    end.

%% @doc Check if a variable is used in a type application
is_var_applied(Var, {type_app, {type_var, Var, _}, _, _}) ->
    true;
is_var_applied(Var, {type_app, Con, Args, _}) ->
    is_var_applied(Var, Con) orelse
    lists:any(fun(A) -> is_var_applied(Var, A) end, Args);
is_var_applied(Var, {type_fun, T1, T2, _}) ->
    is_var_applied(Var, T1) orelse is_var_applied(Var, T2);
is_var_applied(_Var, _) ->
    false.

%% @doc Check kind of instance type argument
%%
%% Validates that the instance type argument has the kind expected
%% by the trait parameter.
-spec check_instance_kind(term(), kind(), kind_env()) ->
    {ok, kind()} | {error, term()}.
check_instance_kind(TypeArg, ExpectedKind, Env) ->
    case infer_type_kind(TypeArg, Env) of
        {ok, InferredKind} ->
            case kinds_compatible(InferredKind, ExpectedKind) of
                true -> {ok, InferredKind};
                false -> {error, {kind_mismatch, ExpectedKind, InferredKind}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Infer kind of a type expression
-spec infer_type_kind(term(), kind_env()) -> {ok, kind()} | {error, term()}.
infer_type_kind({type_con, Name, _Loc}, Env) ->
    case get_type_kind(Name, Env) of
        {ok, Kind} -> {ok, Kind};
        {error, not_found} ->
            %% Unknown type, assume kind star
            {ok, star()}
    end;
infer_type_kind({type_var, _Name, _Loc}, _Env) ->
    %% Type variables default to kind star unless applied
    {ok, star()};
infer_type_kind({type_app, Con, Args, Loc}, Env) ->
    %% For f a, if f : K1 -> K2 and a : K1, result is K2
    case infer_type_kind(Con, Env) of
        {ok, Kind} ->
            apply_kind(Kind, length(Args), Loc);
        Error ->
            Error
    end;
infer_type_kind({tcon, Name}, Env) ->
    %% Internal type representation
    case get_type_kind(Name, Env) of
        {ok, Kind} -> {ok, Kind};
        {error, not_found} -> {ok, star()}
    end;
infer_type_kind({tapp, Con, Args}, Env) ->
    case infer_type_kind(Con, Env) of
        {ok, Kind} ->
            apply_kind(Kind, length(Args), unknown);
        Error ->
            Error
    end;
infer_type_kind(_, _Env) ->
    {ok, star()}.

%% @doc Apply kind to arguments
apply_kind(Kind, 0, _Loc) ->
    {ok, Kind};
apply_kind({arrow, _K1, K2}, N, Loc) when N > 0 ->
    apply_kind(K2, N - 1, Loc);
apply_kind(star, N, Loc) when N > 0 ->
    {error, {over_applied, star, N, Loc}};
apply_kind(K, N, Loc) ->
    {error, {cannot_apply, K, N, Loc}}.

%%%===================================================================
%%% Kind Validation
%%%===================================================================

%% @doc Validate HKT usage in instance declarations
%%
%% Checks that each instance type argument has the kind expected
%% by the corresponding trait parameter.
-spec validate_hkt([term()], kind_env()) ->
    {ok, [term()]} | {error, term()}.
validate_hkt(Declarations, Env) ->
    %% Build map of trait names to their parameter kinds
    TraitKinds = build_trait_kind_map(Declarations),

    %% Validate each instance
    Results = lists:map(
        fun(Decl) ->
            case Decl of
                {instance_decl, TraitName, TypeArgs, _Constraints, _Methods, Loc} ->
                    validate_instance_kinds(TraitName, TypeArgs, TraitKinds, Env, Loc);
                _ ->
                    ok
            end
        end,
        Declarations
    ),

    %% Collect errors
    Errors = [E || {error, E} <- Results],
    case Errors of
        [] -> {ok, Declarations};
        _ -> {error, Errors}
    end.

%% @doc Build map of trait names to parameter kinds
build_trait_kind_map(Declarations) ->
    lists:foldl(
        fun(Decl, Map) ->
            case Decl of
                {trait_decl, Name, _TypeVars, _Supers, _Methods, _Loc} = Trait ->
                    case check_trait_kind(Trait) of
                        {ok, ParamKinds} ->
                            Map#{Name => ParamKinds};
                        _ ->
                            Map
                    end;
                _ ->
                    Map
            end
        end,
        #{},
        Declarations
    ).

%% @doc Validate kinds of instance type arguments
validate_instance_kinds(TraitName, TypeArgs, TraitKinds, Env, Loc) ->
    case maps:find(TraitName, TraitKinds) of
        {ok, ParamKinds} ->
            %% Check each type arg against expected kind
            validate_args_kinds(TypeArgs, ParamKinds, Env, Loc);
        error ->
            %% Unknown trait, skip validation
            ok
    end.

validate_args_kinds([], [], _Env, _Loc) ->
    ok;
validate_args_kinds([Arg | Args], [{_Var, ExpectedKind} | Kinds], Env, Loc) ->
    case check_instance_kind(Arg, ExpectedKind, Env) of
        {ok, _} ->
            validate_args_kinds(Args, Kinds, Env, Loc);
        {error, Reason} ->
            {error, {Loc, Reason}}
    end;
validate_args_kinds(_, _, _Env, Loc) ->
    {error, {Loc, arity_mismatch}}.

%% @doc Check if two kinds are compatible
-spec kinds_compatible(kind(), kind()) -> boolean().
kinds_compatible(K, K) -> true;
kinds_compatible({arrow, A1, B1}, {arrow, A2, B2}) ->
    kinds_compatible(A1, A2) andalso kinds_compatible(B1, B2);
kinds_compatible(_, _) -> false.

%%%===================================================================
%%% Pretty Printing
%%%===================================================================

%% @doc Format kind for display
-spec format_kind(kind()) -> string().
format_kind(star) -> "Type";
format_kind({arrow, K1, K2}) ->
    format_kind(K1) ++ " -> " ++ format_kind(K2).
