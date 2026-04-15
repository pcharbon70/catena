%%%-------------------------------------------------------------------
%%% @doc Catena Operation Signatures (Phase 13.1)
%%%
%%% This module implements operation signatures with row variables for
%%% signature-based effect polymorphism restrictions. Operation signatures
%%% define the contract of effect operations including parameter types,
%%% result types, and effect rows with row variables for polymorphism.
%%%
%%% == Operation Signature Structure ==
%%%
%%% An operation signature consists of:
%%% - Parameter types: list of type references for operation arguments
%%% - Result type: type reference for the operation's return value
%%% - Effect row: extensible effect set that may include row variables
%%% - Row variable scope: tracking for proper scoping of row variables
%%%
%%% == Row Variable Scoping ==
%%%
%%% Row variables (ρ, ε, δ) represent unknown effect sets in polymorphic
%%% operation signatures. They are scoped to specific effect declarations
%%% and cannot escape their scope. This module tracks row variable scopes
%%% and validates that row variables are properly scoped.
%%%
%%% == Polymorphism Restrictions ==
%%%
%%% Operation signatures restrict effect polymorphism by:
%%% 1. Requiring specific effects in the effect row: {Effect | ρ}
%%% 2. Forbidding effects through absence constraints: ρ \ {Effect}
%%% 3. Validating that row variables don't escape their scope
%%% 4. Ensuring signature validity before use in handlers
%%%
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
    row_var/0,
    row_var_id/0,
    scope/0,
    sig_constraints/0
]).

%%%---------------------------------------------------------------------
%%% Types
%%%---------------------------------------------------------------------

%% @doc Row variable identifier - unique ID for each row variable.
-type row_var_id() :: {row_var, non_neg_integer()}.

%% @doc Row variable representing an unknown effect set.
-type row_var() :: #{
    kind => row_var,
    id => row_var_id(),
    name => atom() | undefined
}.

%% @doc Type reference - can be atom, tuple, or map.
-type type_ref() :: atom() | tuple() | map().

%% @doc Effect row with row variables support.
-type effect_row() :: #{
    kind => effect_row,
    elements => [atom()],
    row_var => row_var() | undefined
}.

%% @doc Operation signature with full type information.
-record(op_sig, {
    name :: atom() | undefined,
    params :: [type_ref()],
    result :: type_ref(),
    effects :: effect_row(),
    scope_id :: non_neg_integer() | undefined
}).

-type op_sig() :: #op_sig{}.

%% @doc Row variable scope tracking.
-type scope() :: #{
    id => non_neg_integer(),
    parent => non_neg_integer() | undefined,
    vars => #{row_var_id() => row_var()},
    depth => non_neg_integer()
}.

%% @doc Signature constraints for polymorphism restrictions.
-type sig_constraints() :: #{
    required => [atom()],
    forbidden => [atom()],
    row_vars => [row_var()]
}.

%%%---------------------------------------------------------------------
%%% Operation Signature Constructors
%%%---------------------------------------------------------------------

%% @doc Create an empty operation signature.
-spec op_sig() -> op_sig().
op_sig() ->
    #op_sig{
        name = undefined,
        params = [],
        result = catena_types:tcon('T'),
        effects = empty_effect_row(),
        scope_id = undefined
    }.

%% @doc Create an operation signature with params and result.
-spec op_sig([type_ref()], type_ref()) -> op_sig().
op_sig(Params, Result) when is_list(Params) ->
    #op_sig{
        name = undefined,
        params = Params,
        result = Result,
        effects = empty_effect_row(),
        scope_id = undefined
    }.

%% @doc Create an operation signature with params, result, and effects.
-spec op_sig([type_ref()], type_ref(), effect_row()) -> op_sig().
op_sig(Params, Result, Effects) when is_list(Params) ->
    #op_sig{
        name = undefined,
        params = Params,
        result = Result,
        effects = Effects,
        scope_id = undefined
    }.

%% @doc Create a complete operation signature with all fields.
-spec op_sig(atom(), [type_ref()], type_ref(), effect_row()) -> op_sig().
op_sig(Name, Params, Result, Effects) when is_atom(Name), is_list(Params) ->
    #op_sig{
        name = Name,
        params = Params,
        result = Result,
        effects = Effects,
        scope_id = undefined
    }.

%%%---------------------------------------------------------------------
%%% Row Variable Management
%%%---------------------------------------------------------------------

%% @doc Create a fresh row variable with default naming.
-spec fresh_row_var() -> row_var().
fresh_row_var() ->
    Id = {row_var, erlang:unique_integer([positive, monotonic])},
    row_var(Id).

%% @doc Create a fresh row variable with a specific name.
-spec fresh_row_var(atom()) -> row_var().
fresh_row_var(Name) when is_atom(Name) ->
    Id = {row_var, erlang:unique_integer([positive, monotonic])},
    #{
        kind => row_var,
        id => Id,
        name => Name
    }.

%% @doc Create a row variable with a specific ID.
-spec row_var(row_var_id()) -> row_var().
row_var(Id) ->
    #{
        kind => row_var,
        id => Id,
        name => undefined
    }.

%% @doc Get the ID of a row variable.
-spec row_var_id(row_var()) -> row_var_id().
row_var_id(#{id := Id}) ->
    Id.

%% @doc Check if a term is a row variable.
-spec is_row_var(term()) -> boolean().
is_row_var(#{kind := row_var}) -> true;
is_row_var(_) -> false.

%%%---------------------------------------------------------------------
%%% Row Variable Scope Tracking
%%%---------------------------------------------------------------------

%% @doc Create a new root scope for row variables.
-spec new_scope() -> scope().
new_scope() ->
    #{
        id => 0,
        parent => undefined,
        vars => #{},
        depth => 0
    }.

%% @doc Enter a new nested scope.
-spec enter_scope(scope()) -> scope().
enter_scope(Scope) ->
    ParentId = maps:get(id, Scope),
    Depth = maps:get(depth, Scope),
    #{
        id => erlang:unique_integer([positive, monotonic]),
        parent => ParentId,
        vars => #{},
        depth => Depth + 1
    }.

%% @doc Exit current scope and return parent scope.
-spec exit_scope(scope()) -> scope() | undefined.
exit_scope(#{parent := undefined}) ->
    undefined;
exit_scope(#{parent := ParentId}) ->
    %% In a real implementation, we'd look up the parent scope
    %% For now, return a marker that indicates we exited
    {exited, ParentId}.

%% @doc Check if a row variable is in scope.
-spec in_scope(row_var(), scope()) -> boolean().
in_scope(RowVar, Scope) when is_map(RowVar), is_map(Scope) ->
    Vars = maps:get(vars, Scope, #{}),
    Id = row_var_id(RowVar),
    maps:is_key(Id, Vars).

%% @doc Add a row variable to the current scope.
-spec scope_add_var(scope(), row_var()) -> scope().
scope_add_var(Scope, RowVar) when is_map(Scope), is_map(RowVar) ->
    Vars = maps:get(vars, Scope, #{}),
    Id = row_var_id(RowVar),
    Scope#{vars => Vars#{Id => RowVar}}.

%% @doc Get all row variables in scope.
-spec scope_get_vars(scope()) -> [row_var()].
scope_get_vars(Scope) when is_map(Scope) ->
    Vars = maps:get(vars, Scope, #{}),
    maps:values(Vars).

%% @doc Check if scope contains a specific row variable.
-spec scope_contains_var(scope(), row_var()) -> boolean().
scope_contains_var(Scope, RowVar) ->
    lists:member(RowVar, scope_get_vars(Scope)).

%%%---------------------------------------------------------------------
%%% Signature Validation
%%%---------------------------------------------------------------------

%% @doc Validate an operation signature.
-spec is_valid_sig(op_sig()) -> boolean().
is_valid_sig(#op_sig{params = Params, result = Result, effects = Effects}) ->
    is_valid_params(Params) andalso
    is_valid_result(Result) andalso
    is_valid_effect_row(Effects);
is_valid_sig(_) ->
    false.

%% @doc Validate an operation name.
-spec is_valid_op_name(term()) -> boolean().
is_valid_op_name(Name) when is_atom(Name) ->
    case atom_to_list(Name) of
        [] -> false;
        [C | _] when C >= $a, C =< $z -> true;  % Lowercase start
        _ -> false
    end;
is_valid_op_name(_) ->
    false.

%% @doc validate parameter types list.
-spec is_valid_params(term()) -> boolean().
is_valid_params(Params) when is_list(Params) ->
    lists:all(fun is_valid_type/1, Params);
is_valid_params(_) ->
    false.

%% @doc Validate a result type.
-spec is_valid_result(term()) -> boolean().
is_valid_result(Result) ->
    is_valid_type(Result).

%% @doc Validate an effect row.
-spec is_valid_effect_row(term()) -> boolean().
is_valid_effect_row(#{kind := effect_row, elements := Es, row_var := Rv}) when is_list(Es) ->
    lists:all(fun is_atom/1, Es) andalso
    (Rv =:= undefined orelse is_row_var(Rv));
is_valid_effect_row(_) ->
    false.

%%%---------------------------------------------------------------------
%%% Polymorphism Restrictions
%%%---------------------------------------------------------------------

%% @doc Check for row variable escape from signature.
%% A row variable escapes if it appears in the result type or
%% in a position that would make it visible outside the signature scope.
-spec check_row_var_escape(op_sig(), scope()) -> {ok, [row_var()]} | {error, row_var()}.
check_row_var_escape(#op_sig{effects = Effects}, Scope) ->
    RowVar = maps:get(row_var, Effects),
    check_var_escape(RowVar, Scope, []).

%% @private
check_var_escape(undefined, _Scope, Acc) ->
    {ok, lists:reverse(Acc)};
check_var_escape(RowVar, Scope, Acc) ->
    case in_scope(RowVar, Scope) of
        true -> {ok, lists:reverse([RowVar | Acc])};
        false -> {error, RowVar}
    end.

%% @doc Get signature polymorphism restrictions.
-spec signature_restrictions(op_sig()) -> sig_constraints().
signature_restrictions(#op_sig{effects = Effects}) ->
    Elements = maps:get(elements, Effects, []),
    RowVar = maps:get(row_var, Effects),
    #{
        required => Elements,
        forbidden => [],
        row_vars => case RowVar of
            undefined -> [];
            _ -> [RowVar]
        end
    }.

%% @doc Get required effects from a signature.
-spec required_effects(op_sig()) -> [atom()].
required_effects(#op_sig{effects = Effects}) ->
    maps:get(elements, Effects, []).

%% @doc Get forbidden effects from a signature.
-spec forbidden_effects(op_sig()) -> [atom()].
forbidden_effects(_Sig) ->
    %% For now, forbidden effects are not tracked in the signature
    %% They would be specified through absence constraints
    [].

%% @doc Check if an effect set satisfies signature constraints.
-spec satisfies_constraints([atom()], op_sig()) -> boolean().
satisfies_constraints(EffectSet, #op_sig{effects = Effects}) ->
    Required = maps:get(elements, Effects, []),
    %% All required effects must be present
    lists:all(fun(E) -> lists:member(E, EffectSet) end, Required).

%%%---------------------------------------------------------------------
%%% Signature Operations
%%%---------------------------------------------------------------------

%% @doc Substitute a row variable in a signature.
-spec substitute_row_var(op_sig(), row_var(), effect_row()) -> op_sig().
substitute_row_var(#op_sig{effects = Effects} = Sig, RowVar, Replacement) ->
    case maps:get(row_var, Effects) of
        RowVar ->
            Sig#op_sig{effects = Replacement};
        _ ->
            Sig
    end.

%% @doc Generalize a signature by introducing row variables.
-spec generalize_sig(op_sig()) -> op_sig().
generalize_sig(#op_sig{effects = Effects} = Sig) ->
    case maps:get(row_var, Effects) of
        undefined ->
            %% Add a fresh row variable
            FreshVar = fresh_row_var(),
            Sig#op_sig{effects = Effects#{row_var => FreshVar}};
        _ ->
            %% Already has row variable
            Sig
    end.

%% @doc Instantiate a signature by replacing row variables.
-spec instantiate_sig(op_sig(), effect_row()) -> op_sig().
instantiate_sig(#op_sig{effects = Effects} = Sig, Instantiation) ->
    RowVar = maps:get(row_var, Effects),
    case RowVar of
        undefined -> Sig;
        _ -> Sig#op_sig{effects = Instantiation}
    end.

%% @doc Check if two signatures are equal.
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

%% @doc Format an operation signature for display.
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

%% @doc Format a row variable for display.
-spec format_row_var(row_var()) -> binary().
format_row_var(#{name := undefined}) ->
    <<"ρ">>;
format_row_var(#{name := Name}) when is_atom(Name) ->
    list_to_binary(atom_to_list(Name));
format_row_var(_) ->
    <<"ρ">>.

%%%---------------------------------------------------------------------
%%% Accessor Functions
%%%---------------------------------------------------------------------

%% @doc Get the name from an operation signature.
-spec sig_name(op_sig()) -> atom() | undefined.
sig_name(#op_sig{name = Name}) -> Name.

%% @doc Get the parameters from an operation signature.
-spec sig_params(op_sig()) -> [type_ref()].
sig_params(#op_sig{params = Params}) -> Params.

%% @doc Get the result type from an operation signature.
-spec sig_result(op_sig()) -> type_ref().
sig_result(#op_sig{result = Result}) -> Result.

%% @doc Get the effect row from an operation signature.
-spec sig_effects(op_sig()) -> effect_row().
sig_effects(#op_sig{effects = Effects}) -> Effects.

%% @doc Get the scope ID from an operation signature.
-spec sig_scope_id(op_sig()) -> non_neg_integer() | undefined.
sig_scope_id(#op_sig{scope_id = ScopeId}) -> ScopeId.

%%%---------------------------------------------------------------------
%%% Internal Helpers
%%%---------------------------------------------------------------------

%% @doc Create an empty effect row.
-spec empty_effect_row() -> effect_row().
empty_effect_row() ->
    #{
        kind => effect_row,
        elements => [],
        row_var => undefined
    }.

%% @doc Check if a term is a valid type.
-spec is_valid_type(term()) -> boolean().
is_valid_type(Type) when is_map(Type); is_atom(Type); is_tuple(Type) ->
    true;
is_valid_type(_) ->
    false.

%% @doc Check if two types are equal.
-spec types_equal(type_ref(), type_ref()) -> boolean().
types_equal(T1, T2) when is_map(T1), is_map(T2) ->
    maps:size(T1) =:= maps:size(T2) andalso
    lists:all(fun({K, V}) ->
        case maps:find(K, T2) of
            {ok, V2} -> types_equal(V, V2);
            error -> false
        end
    end, maps:to_list(T1));
types_equal(T1, T2) when is_tuple(T1), is_tuple(T2) ->
    tuple_size(T1) =:= tuple_size(T2) andalso
    lists:all(fun({I1, I2}) -> types_equal(I1, I2) end,
        lists:zip(tuple_to_list(T1), tuple_to_list(T2)));
types_equal(A, B) when is_atom(A); is_number(A); is_binary(A) ->
    A =:= B;
types_equal([H1 | T1], [H2 | T2]) ->
    types_equal(H1, H2) andalso types_equal(T1, T2);
types_equal([], []) ->
    true;
types_equal(_, _) ->
    false.

%% @doc Check if two effect rows are equal.
-spec effect_rows_equal(effect_row(), effect_row()) -> boolean().
effect_rows_equal(#{elements := E1, row_var := RV1}, #{elements := E2, row_var := RV2}) ->
    lists:usort(E1) =:= lists:usort(E2) andalso row_vars_equal(RV1, RV2).

%% @private
row_vars_equal(undefined, undefined) -> true;
row_vars_equal(RV1, RV2) when is_map(RV1), is_map(RV2) ->
    maps:get(id, RV1) =:= maps:get(id, RV2);
row_vars_equal(_, _) -> false.

%% @doc Format a type for display.
-spec format_type(type_ref()) -> binary().
format_type(Type) when is_atom(Type) ->
    list_to_binary(atom_to_list(Type));
format_type(Type) when is_map(Type) ->
    case maps:get(kind, Type, undefined) of
        undefined -> <<"{}">>;
        Kind -> <<(list_to_binary(atom_to_list(Kind)))/binary, "{}">>
    end;
format_type(Type) when is_tuple(Type) ->
    Elements = tuple_to_list(Type),
    Formatted = [format_type(E) || E <- Elements],
    list_to_binary(["(" | [lists:join(<<", ">>, Formatted), ")"]]);
format_type(_) ->
    <<"?">>.

%% @doc Format a list of types.
-spec format_type_list([type_ref()]) -> binary().
format_type_list([]) ->
    <<"()">>;
format_type_list(Types) ->
    Formatted = [format_type(T) || T <- Types],
    list_to_binary(["(" | [lists:join(<<", ">>, Formatted), ")"]]).

%% @doc Format an effect row.
-spec format_effect_row(effect_row()) -> binary().
format_effect_row(#{elements := Es, row_var := RV}) ->
    ElementsPart = case Es of
        [] -> <<"">>;
        _ -> lists:foldl(fun(E, Acc) ->
                case Acc of
                    <<>> -> list_to_binary(atom_to_list(E));
                    _ -> <<Acc/binary, ", ", (list_to_binary(atom_to_list(E)))/binary>>
                end
            end, <<>>, Es)
    end,
    RowVarPart = case RV of
        undefined -> <<"">>;
        _ -> format_row_var(RV)
    end,
    case {ElementsPart, RowVarPart} of
        {<<>>, <<>>} -> <<"{}">>;
        {<<>>, _} -> <<"{", RowVarPart/binary, "}">>;
        {_, <<>>} -> <<"{", ElementsPart/binary, "}">>;
        {_, _} -> <<"{", ElementsPart/binary, " | ", RowVarPart/binary, "}">>
    end.
