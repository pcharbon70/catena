%%%-------------------------------------------------------------------
%%% @doc Catena Operation Signatures (Phase 13.1)
%%%
%%% Operation signatures constrain effect polymorphism for individual
%%% operations. They are built on the real row-type surface introduced
%%% by Phase 11 and track row-variable scope explicitly.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_op_signatures).

%% Operation signature constructors
-export([
    op_sig/0,
    op_sig/2,
    op_sig/3,
    op_sig/4
]).

%% Row variable management
-export([
    fresh_row_var/0,
    fresh_row_var/1,
    row_var/1,
    row_var_id/1,
    is_row_var/1
]).

%% Row variable scope tracking
-export([
    new_scope/0,
    enter_scope/1,
    exit_scope/1,
    in_scope/2,
    scope_add_var/2,
    scope_get_vars/1,
    scope_contains_var/2
]).

%% Signature validation
-export([
    is_valid_sig/1,
    is_valid_op_name/1,
    is_valid_params/1,
    is_valid_result/1,
    is_valid_effect_row/1
]).

%% Polymorphism restrictions
-export([
    check_row_var_escape/2,
    signature_restrictions/1,
    required_effects/1,
    forbidden_effects/1,
    satisfies_constraints/2
]).

%% Signature operations
-export([
    substitute_row_var/3,
    generalize_sig/1,
    instantiate_sig/2,
    sig_eq/2
]).

%% Pretty printing
-export([
    format_sig/1,
    format_row_var/1
]).

%% Accessor functions
-export([
    sig_name/1,
    sig_params/1,
    sig_result/1,
    sig_effects/1,
    sig_scope_id/1
]).

-export_type([
    op_sig/0,
    op_signature/0,
    row_var/0,
    row_var_id/0,
    scope/0,
    sig_constraints/0
]).

%%%---------------------------------------------------------------------
%%% Types
%%%---------------------------------------------------------------------

-type row_var_id() :: catena_row_types:row_var_id().
-type row_var() :: catena_row_types:row_var().
-type effect_row() :: catena_row_types:effect_row().
-type type_ref() :: atom() | tuple() | map() | [type_ref()].

-record(op_sig, {
    name :: atom() | undefined,
    params :: [type_ref()],
    result :: type_ref(),
    effects :: effect_row(),
    scope_id :: non_neg_integer() | undefined
}).

-type op_sig() :: #op_sig{}.
-type op_signature() :: op_sig().

-type scope() :: #{
    id => non_neg_integer(),
    parent => scope() | undefined,
    vars => #{row_var_id() => row_var()},
    depth => non_neg_integer()
}.

-type sig_constraints() :: #{
    required => [atom()],
    forbidden => [atom()],
    row_vars => [row_var()]
}.

%%%---------------------------------------------------------------------
%%% Operation Signature Constructors
%%%---------------------------------------------------------------------

-spec op_sig() -> op_sig().
op_sig() ->
    #op_sig{
        name = undefined,
        params = [],
        result = catena_types:tcon('T'),
        effects = catena_row_types:empty_row(),
        scope_id = undefined
    }.

-spec op_sig([type_ref()], type_ref()) -> op_sig().
op_sig(Params, Result) when is_list(Params) ->
    #op_sig{
        name = undefined,
        params = Params,
        result = Result,
        effects = catena_row_types:empty_row(),
        scope_id = undefined
    }.

-spec op_sig([type_ref()], type_ref(), effect_row()) -> op_sig().
op_sig(Params, Result, Effects) when is_list(Params) ->
    #op_sig{
        name = undefined,
        params = Params,
        result = Result,
        effects = normalize_effect_row(Effects),
        scope_id = undefined
    }.

-spec op_sig(atom(), [type_ref()], type_ref(), effect_row()) -> op_sig().
op_sig(Name, Params, Result, Effects) when is_atom(Name), is_list(Params) ->
    #op_sig{
        name = Name,
        params = Params,
        result = Result,
        effects = normalize_effect_row(Effects),
        scope_id = undefined
    }.

%%%---------------------------------------------------------------------
%%% Row Variable Management
%%%---------------------------------------------------------------------

-spec fresh_row_var() -> row_var().
fresh_row_var() ->
    catena_row_types:fresh_row_var().

-spec fresh_row_var(atom()) -> row_var().
fresh_row_var(Name) when is_atom(Name) ->
    (catena_row_types:row_var({row_var, erlang:unique_integer([positive, monotonic])}))#{name => Name}.

-spec row_var(row_var_id()) -> row_var().
row_var(Id) ->
    catena_row_types:row_var(Id).

-spec row_var_id(row_var()) -> row_var_id().
row_var_id(RowVar) ->
    catena_row_types:row_var_id(RowVar).

-spec is_row_var(term()) -> boolean().
is_row_var(Term) ->
    catena_row_types:is_row_var(Term).

%%%---------------------------------------------------------------------
%%% Row Variable Scope Tracking
%%%---------------------------------------------------------------------

-spec new_scope() -> scope().
new_scope() ->
    #{
        id => fresh_scope_id(),
        parent => undefined,
        vars => #{},
        depth => 0
    }.

-spec enter_scope(scope()) -> scope().
enter_scope(Scope) ->
    #{
        id => fresh_scope_id(),
        parent => Scope,
        vars => #{},
        depth => maps:get(depth, Scope, 0) + 1
    }.

-spec exit_scope(scope()) -> scope() | undefined.
exit_scope(#{parent := Parent}) ->
    Parent.

-spec in_scope(row_var(), scope()) -> boolean().
in_scope(RowVar, Scope) when is_map(RowVar), is_map(Scope) ->
    find_scope_for_var(RowVar, Scope) =/= false;
in_scope(_, _) ->
    false.

-spec scope_add_var(scope(), row_var()) -> scope().
scope_add_var(Scope, RowVar) when is_map(Scope), is_map(RowVar) ->
    Vars = maps:get(vars, Scope, #{}),
    Scope#{vars => Vars#{row_var_id(RowVar) => RowVar}}.

-spec scope_get_vars(scope()) -> [row_var()].
scope_get_vars(Scope) when is_map(Scope) ->
    maps:values(gather_scope_vars(Scope, #{}));
scope_get_vars(_) ->
    [].

-spec scope_contains_var(scope(), row_var()) -> boolean().
scope_contains_var(Scope, RowVar) ->
    in_scope(RowVar, Scope).

%%%---------------------------------------------------------------------
%%% Signature Validation
%%%---------------------------------------------------------------------

-spec is_valid_sig(op_sig()) -> boolean().
is_valid_sig(#op_sig{name = Name, params = Params, result = Result, effects = Effects}) ->
    is_valid_name(Name) andalso
    is_valid_params(Params) andalso
    is_valid_result(Result) andalso
    is_valid_effect_row(Effects);
is_valid_sig(_) ->
    false.

-spec is_valid_op_name(term()) -> boolean().
is_valid_op_name(Name) when is_atom(Name) ->
    is_valid_name(Name);
is_valid_op_name(_) ->
    false.

-spec is_valid_params(term()) -> boolean().
is_valid_params(Params) when is_list(Params) ->
    lists:all(fun is_valid_type/1, Params);
is_valid_params(_) ->
    false.

-spec is_valid_result(term()) -> boolean().
is_valid_result(Result) ->
    is_valid_type(Result).

-spec is_valid_effect_row(term()) -> boolean().
is_valid_effect_row(Row) ->
    catena_row_types:is_valid_row(Row).

%%%---------------------------------------------------------------------
%%% Polymorphism Restrictions
%%%---------------------------------------------------------------------

-spec check_row_var_escape(op_sig(), scope()) -> {ok, [row_var()]} | {error, row_var()}.
check_row_var_escape(Sig, Scope) ->
    Vars = collect_sig_row_vars(Sig),
    case lists:dropwhile(fun(RowVar) -> in_scope(RowVar, Scope) end, Vars) of
        [] -> {ok, Vars};
        [Escaped | _] -> {error, Escaped}
    end.

-spec signature_restrictions(op_sig()) -> sig_constraints().
signature_restrictions(Sig) ->
    #{
        required => required_effects(Sig),
        forbidden => forbidden_effects(Sig),
        row_vars => collect_sig_row_vars(Sig)
    }.

-spec required_effects(op_sig()) -> [atom()].
required_effects(#op_sig{effects = Effects}) ->
    maps:get(elements, normalize_effect_row(Effects), []).

-spec forbidden_effects(op_sig()) -> [atom()].
forbidden_effects(_Sig) ->
    [].

-spec satisfies_constraints([atom()] | effect_row(), op_sig()) -> boolean().
satisfies_constraints(Provided, #op_sig{effects = Required}) ->
    ProvidedRow = normalize_effect_input(Provided),
    RequiredRow = normalize_effect_row(Required),
    catena_row_types:row_contains_all(
        ProvidedRow,
        catena_row_types:effect_row(maps:get(elements, RequiredRow, []))
    ).

%%%---------------------------------------------------------------------
%%% Signature Operations
%%%---------------------------------------------------------------------

-spec substitute_row_var(op_sig(), row_var(), effect_row()) -> op_sig().
substitute_row_var(#op_sig{effects = Effects} = Sig, RowVar, Replacement) ->
    NormalizedEffects = normalize_effect_row(Effects),
    case maps:get(row_var, NormalizedEffects, undefined) of
        RowVar ->
            Concrete = catena_row_types:effect_row(maps:get(elements, NormalizedEffects, [])),
            Merged = catena_row_types:row_union(Concrete, normalize_effect_row(Replacement)),
            Sig#op_sig{effects = Merged};
        _ ->
            Sig
    end.

-spec generalize_sig(op_sig()) -> op_sig().
generalize_sig(#op_sig{effects = Effects} = Sig) ->
    Normalized = normalize_effect_row(Effects),
    case maps:get(row_var, Normalized, undefined) of
        undefined ->
            Sig#op_sig{effects = Normalized#{row_var => fresh_row_var()}};
        _ ->
            Sig#op_sig{effects = Normalized}
    end.

-spec instantiate_sig(op_sig(), effect_row()) -> op_sig().
instantiate_sig(#op_sig{effects = Effects} = Sig, Instantiation) ->
    case maps:get(row_var, normalize_effect_row(Effects), undefined) of
        undefined -> Sig;
        RowVar -> substitute_row_var(Sig, RowVar, Instantiation)
    end.

-spec sig_eq(op_sig(), op_sig()) -> boolean().
sig_eq(#op_sig{params = P1, result = R1, effects = E1},
       #op_sig{params = P2, result = R2, effects = E2}) ->
    types_equal(P1, P2) andalso
    types_equal(R1, R2) andalso
    effect_rows_equal(E1, E2);
sig_eq(_, _) ->
    false.

%%%---------------------------------------------------------------------
%%% Pretty Printing
%%%---------------------------------------------------------------------

-spec format_sig(op_sig()) -> binary().
format_sig(#op_sig{name = Name, params = Params, result = Result, effects = Effects}) ->
    NamePart = case Name of
        undefined -> <<"op">>;
        _ -> list_to_binary(atom_to_list(Name))
    end,
    ParamsPart = format_type_list(Params),
    ResultPart = format_type(Result),
    EffectsPart = format_effect_row(Effects),
    <<NamePart/binary, ": ", ParamsPart/binary, " -> ", ResultPart/binary, " / ", EffectsPart/binary>>.

-spec format_row_var(row_var()) -> binary().
format_row_var(#{name := Name}) when is_atom(Name) ->
    list_to_binary(atom_to_list(Name));
format_row_var(#{id := {row_var, Id}}) ->
    list_to_binary(["ρ", integer_to_list(Id)]);
format_row_var(_) ->
    <<"ρ">>.

%%%---------------------------------------------------------------------
%%% Accessor Functions
%%%---------------------------------------------------------------------

-spec sig_name(op_sig()) -> atom() | undefined.
sig_name(#op_sig{name = Name}) ->
    Name.

-spec sig_params(op_sig()) -> [type_ref()].
sig_params(#op_sig{params = Params}) ->
    Params.

-spec sig_result(op_sig()) -> type_ref().
sig_result(#op_sig{result = Result}) ->
    Result.

-spec sig_effects(op_sig()) -> effect_row().
sig_effects(#op_sig{effects = Effects}) ->
    Effects.

-spec sig_scope_id(op_sig()) -> non_neg_integer() | undefined.
sig_scope_id(#op_sig{scope_id = ScopeId}) ->
    ScopeId.

%%%---------------------------------------------------------------------
%%% Internal Helpers
%%%---------------------------------------------------------------------

-spec fresh_scope_id() -> non_neg_integer().
fresh_scope_id() ->
    erlang:unique_integer([positive, monotonic]).

-spec normalize_effect_row(effect_row()) -> effect_row().
normalize_effect_row(Row) ->
    catena_row_types:row_normalize(Row).

-spec normalize_effect_input([atom()] | effect_row()) -> effect_row().
normalize_effect_input(Effects) when is_list(Effects) ->
    catena_row_types:effect_row(Effects);
normalize_effect_input(Row) ->
    normalize_effect_row(Row).

-spec gather_scope_vars(scope(), #{row_var_id() => row_var()}) -> #{row_var_id() => row_var()}.
gather_scope_vars(#{vars := Vars, parent := undefined}, Acc) ->
    maps:merge(Acc, Vars);
gather_scope_vars(#{vars := Vars, parent := Parent}, Acc) ->
    gather_scope_vars(Parent, maps:merge(Acc, Vars)).

-spec find_scope_for_var(row_var(), scope()) -> scope() | false.
find_scope_for_var(RowVar, Scope) ->
    Id = row_var_id(RowVar),
    Vars = maps:get(vars, Scope, #{}),
    case maps:is_key(Id, Vars) of
        true ->
            Scope;
        false ->
            case maps:get(parent, Scope, undefined) of
                undefined -> false;
                Parent -> find_scope_for_var(RowVar, Parent)
            end
    end.

-spec collect_sig_row_vars(op_sig()) -> [row_var()].
collect_sig_row_vars(#op_sig{params = Params, result = Result, effects = Effects}) ->
    dedupe_row_vars(
        collect_row_vars_from_term(Params) ++
        collect_row_vars_from_term(Result) ++
        collect_row_vars_from_term(Effects)
    ).

-spec collect_row_vars_from_term(term()) -> [row_var()].
collect_row_vars_from_term(#{kind := row_var} = RowVar) ->
    [strip_row_var_name(RowVar)];
collect_row_vars_from_term(#{kind := effect_row, row_var := undefined, elements := _}) ->
    [];
collect_row_vars_from_term(#{kind := effect_row, row_var := RowVar, elements := Elements}) ->
    collect_row_vars_from_term(Elements) ++ collect_row_vars_from_term(RowVar);
collect_row_vars_from_term(Map) when is_map(Map) ->
    lists:flatmap(fun collect_row_vars_from_term/1, maps:values(Map));
collect_row_vars_from_term(Tuple) when is_tuple(Tuple) ->
    lists:flatmap(fun collect_row_vars_from_term/1, tuple_to_list(Tuple));
collect_row_vars_from_term(List) when is_list(List) ->
    lists:flatmap(fun collect_row_vars_from_term/1, List);
collect_row_vars_from_term(_) ->
    [].

-spec strip_row_var_name(row_var()) -> row_var().
strip_row_var_name(RowVar) ->
    maps:remove(name, RowVar).

-spec dedupe_row_vars([row_var()]) -> [row_var()].
dedupe_row_vars(Vars) ->
    maps:values(
        lists:foldl(
            fun(Var, Acc) -> Acc#{row_var_id(Var) => Var} end,
            #{},
            Vars
        )
    ).

-spec is_valid_name(atom() | undefined) -> boolean().
is_valid_name(undefined) ->
    true;
is_valid_name(Name) when is_atom(Name) ->
    case atom_to_list(Name) of
        [] -> false;
        [C | _] when C >= $a, C =< $z -> true;
        _ -> false
    end;
is_valid_name(_) ->
    false.

-spec is_valid_type(term()) -> boolean().
is_valid_type(Type) when is_atom(Type); is_map(Type); is_tuple(Type) ->
    true;
is_valid_type(Type) when is_list(Type) ->
    lists:all(fun is_valid_type/1, Type);
is_valid_type(_) ->
    false.

-spec types_equal(type_ref(), type_ref()) -> boolean().
types_equal(T1, T2) when is_map(T1), is_map(T2) ->
    maps:size(T1) =:= maps:size(T2) andalso
    lists:all(
        fun({K, V}) ->
            case maps:find(K, T2) of
                {ok, V2} -> types_equal(V, V2);
                error -> false
            end
        end,
        maps:to_list(T1)
    );
types_equal(T1, T2) when is_tuple(T1), is_tuple(T2) ->
    tuple_size(T1) =:= tuple_size(T2) andalso
    lists:all(
        fun({V1, V2}) -> types_equal(V1, V2) end,
        lists:zip(tuple_to_list(T1), tuple_to_list(T2))
    );
types_equal(T1, T2) when is_list(T1), is_list(T2) ->
    length(T1) =:= length(T2) andalso
    lists:all(fun({V1, V2}) -> types_equal(V1, V2) end, lists:zip(T1, T2));
types_equal(A, B) ->
    A =:= B.

-spec effect_rows_equal(effect_row(), effect_row()) -> boolean().
effect_rows_equal(Row1, Row2) ->
    Normalized1 = normalize_effect_row(Row1),
    Normalized2 = normalize_effect_row(Row2),
    maps:get(elements, Normalized1, []) =:= maps:get(elements, Normalized2, []) andalso
    row_vars_equal(
        maps:get(row_var, Normalized1, undefined),
        maps:get(row_var, Normalized2, undefined)
    ).

-spec row_vars_equal(row_var() | undefined, row_var() | undefined) -> boolean().
row_vars_equal(undefined, undefined) ->
    true;
row_vars_equal(RowVar1, RowVar2) when is_map(RowVar1), is_map(RowVar2) ->
    row_var_id(RowVar1) =:= row_var_id(RowVar2);
row_vars_equal(_, _) ->
    false.

-spec format_type_list([type_ref()]) -> binary().
format_type_list([]) ->
    <<"()">>;
format_type_list(Types) ->
    Joined = lists:join(<<", ">>, [format_type(Type) || Type <- Types]),
    iolist_to_binary(["(", Joined, ")"]).

-spec format_type(type_ref()) -> binary().
format_type(Type) when is_atom(Type) ->
    list_to_binary(atom_to_list(Type));
format_type(Type) when is_map(Type) ->
    case maps:get(kind, Type, undefined) of
        effect_row -> format_effect_row(Type);
        row_var -> format_row_var(Type);
        undefined -> <<"{}">>;
        Kind -> <<(list_to_binary(atom_to_list(Kind)))/binary, "{}">>
    end;
format_type(Type) when is_tuple(Type) ->
    Joined = lists:join(<<", ">>, [format_type(Element) || Element <- tuple_to_list(Type)]),
    iolist_to_binary(["(", Joined, ")"]);
format_type(Type) when is_list(Type) ->
    Joined = lists:join(<<", ">>, [format_type(Element) || Element <- Type]),
    iolist_to_binary(["[", Joined, "]"]);
format_type(_) ->
    <<"?">>.

-spec format_effect_row(effect_row()) -> binary().
format_effect_row(Row) ->
    Normalized = normalize_effect_row(Row),
    Elements = maps:get(elements, Normalized, []),
    RowVar = maps:get(row_var, Normalized, undefined),
    ElementPart =
        case Elements of
            [] -> <<"">>;
            _ -> iolist_to_binary(lists:join(<<", ">>, [list_to_binary(atom_to_list(Element)) || Element <- Elements]))
        end,
    RowVarPart =
        case RowVar of
            undefined -> <<"">>;
            _ -> format_row_var(RowVar)
        end,
    case {ElementPart, RowVarPart} of
        {<<>>, <<>>} -> <<"{}">>;
        {<<>>, _} -> <<"{", RowVarPart/binary, "}">>;
        {_, <<>>} -> <<"{", ElementPart/binary, "}">>;
        {_, _} -> <<"{", ElementPart/binary, " | ", RowVarPart/binary, "}">>
    end.
