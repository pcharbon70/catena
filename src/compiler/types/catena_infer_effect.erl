%%%-------------------------------------------------------------------
%%% @doc Effect Inference and Checking
%%%
%%% This module handles effect tracking during type inference, including:
%%% - Effect inference from expressions
%%% - Effect normalization and canonicalization
%%% - Effect checking for pure contexts
%%% - Effect subsumption and comparison
%%%
%%% For the PoC, effects are monomorphic (no effect variables).
%%% Effect polymorphism would require effect variables and constraints,
%%% which is deferred to future work.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_infer_effect).

-export([
    pure/0,
    from_list/1,
    normalize/1,
    union/2,
    is_pure/1,
    subsumes/2,
    check_pure/2,
    check_guard_purity/1,
    infer_guard_effects/1,
    infer_expr_effects/1,
    compatible/2
]).

-export_type([effect_set/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type effect_set() :: catena_types:effect_set().

%%%===================================================================
%%% API Functions - Effect Construction
%%%===================================================================

%% @doc Create a pure (empty) effect set
-spec pure() -> effect_set().
pure() ->
    {effect_set, []}.

%% @doc Create an effect set from a list of effect atoms
%% Normalizes the list (sorts and removes duplicates)
-spec from_list([atom()]) -> effect_set().
from_list(Effects) when is_list(Effects) ->
    normalize({effect_set, Effects}).

%% @doc Normalize an effect set
%% Removes duplicates and sorts effect atoms for canonical representation
-spec normalize(effect_set()) -> effect_set().
normalize({effect_set, Effects}) ->
    Normalized = lists:usort(Effects),
    {effect_set, Normalized}.

%% @doc Union of two effect sets
%% Combines effects from both sets and normalizes
-spec union(effect_set(), effect_set()) -> effect_set().
union({effect_set, E1}, {effect_set, E2}) ->
    Combined = E1 ++ E2,
    normalize({effect_set, Combined}).

%%%===================================================================
%%% API Functions - Effect Checking
%%%===================================================================

%% @doc Check if an effect set is pure (empty)
-spec is_pure(effect_set()) -> boolean().
is_pure({effect_set, []}) ->
    true;
is_pure({effect_set, [_|_]}) ->
    false.

%% @doc Check if one effect set subsumes another
%% E1 subsumes E2 if E2 ⊆ E1 (E2 is a subset of E1)
%%
%% This is used for subtyping: if a function is declared to have effects E1,
%% it can be used where a function with effects E2 is expected if E1 subsumes E2.
-spec subsumes(effect_set(), effect_set()) -> boolean().
subsumes({effect_set, E1}, {effect_set, E2}) ->
    % E1 subsumes E2 if all effects in E2 are in E1
    E1Set = sets:from_list(E1),
    E2Set = sets:from_list(E2),
    sets:is_subset(E2Set, E1Set).

%% @doc Check if two effect sets are compatible for unification
%% For monomorphic effects (PoC), they must be exactly equal
%% For polymorphic effects (future), would use subsumption
-spec compatible(effect_set(), effect_set()) -> boolean().
compatible(E1, E2) ->
    % Normalize both and compare
    {effect_set, N1} = normalize(E1),
    {effect_set, N2} = normalize(E2),
    N1 =:= N2.

%% @doc Check that an expression has pure effects
%% Returns ok if effects are pure, error with effect mismatch otherwise
-spec check_pure(catena_types:type(), effect_set()) ->
    ok | {error, catena_type_error:type_error()}.
check_pure(_Type, {effect_set, []}) ->
    ok;
check_pure(_Type, {effect_set, Effects}) ->
    {error, catena_type_error:effect_mismatch(pure(), {effect_set, Effects})}.

%% @doc Check that a guard expression is pure (no effects)
%% Guards in pattern matching must not have side effects.
%% Returns ok if pure, {error, Reason} if effectful.
-spec check_guard_purity(catena_ast:expr()) -> ok | {error, term()}.
check_guard_purity(Guard) ->
    Effects = infer_guard_effects(Guard),
    case is_pure(Effects) of
        true ->
            ok;
        false ->
            {error, {impure_guard, Guard, Effects}}
    end.

%% @doc Infer effects from a guard expression recursively
%% This walks the guard expression tree to detect any effectful operations
-spec infer_guard_effects(catena_ast:expr()) -> effect_set().
infer_guard_effects({literal, _, _, _}) ->
    pure();
infer_guard_effects({var, _, _}) ->
    pure();
infer_guard_effects({binary_op, _, Left, Right, _}) ->
    union(infer_guard_effects(Left), infer_guard_effects(Right));
infer_guard_effects({unary_op, _, Expr, _}) ->
    infer_guard_effects(Expr);
infer_guard_effects({app, Fun, Arg, _}) ->
    union(infer_guard_effects(Fun), infer_guard_effects(Arg));
infer_guard_effects({tuple, Elements, _}) when is_list(Elements) ->
    lists:foldl(fun(E, Acc) -> union(Acc, infer_guard_effects(E)) end, pure(), Elements);
infer_guard_effects({list, Elements, _}) when is_list(Elements) ->
    lists:foldl(fun(E, Acc) -> union(Acc, infer_guard_effects(E)) end, pure(), Elements);
infer_guard_effects({record, Fields, _}) when is_list(Fields) ->
    lists:foldl(fun({_, E}, Acc) -> union(Acc, infer_guard_effects(E)) end, pure(), Fields);
infer_guard_effects({field_access, Expr, _, _}) ->
    infer_guard_effects(Expr);
%% Effectful operations - reject in guards
infer_guard_effects({perform, Effect, _, _}) ->
    from_list([Effect]);
infer_guard_effects({do_expr, _, _}) ->
    %% Do expressions in guards are not allowed (likely effectful)
    from_list([unknown_effect]);
%% Default case - assume pure for other expressions
infer_guard_effects(_Other) ->
    pure().

%%%===================================================================
%%% API Functions - Effect Inference
%%%===================================================================

%% @doc Infer effects from an expression AST node
%% For PoC, most expressions are pure. This will be extended when
%% we add actual effectful operations like:
%% - perform operations (state, IO, exceptions)
%% - FFI calls
%% - async operations
-spec infer_expr_effects(catena_ast:expr()) -> effect_set().
infer_expr_effects({lit, _}) ->
    % Literals are pure
    pure();

infer_expr_effects({var, _}) ->
    % Variables are pure (lookup has no effects)
    pure();

infer_expr_effects({lam, _, _}) ->
    % Lambda construction is pure (application may have effects from function)
    pure();

infer_expr_effects({app, _Fun, _Arg}) ->
    % Application: effects come from the function type, not the operation itself
    % The type inference will extract effects from the function's type
    pure();

infer_expr_effects({'let', _, _Expr, _Body}) ->
    % Let binding is pure (effects come from subexpressions)
    pure();

infer_expr_effects({'letrec', _, _Expr, _Body}) ->
    % Let-rec binding is pure
    pure();

infer_expr_effects({'if', _Cond, _Then, _Else}) ->
    % If-then-else is pure (effects come from branches)
    pure();

infer_expr_effects({tuple, _Elements}) ->
    % Tuple construction is pure
    pure();

infer_expr_effects({record, _Fields}) ->
    % Record construction is pure
    pure();

infer_expr_effects({field, _Expr, _FieldName}) ->
    % Field access is pure
    pure();

infer_expr_effects({variant, _Constructor, _Args}) ->
    % Variant construction is pure
    pure();

infer_expr_effects({ann, _Expr, _Type}) ->
    % Annotation doesn't add effects
    pure().

% When we add effectful operations, we'll have patterns like:
% infer_expr_effects({perform, io, _}) -> from_list([io]);
% infer_expr_effects({perform, state, _}) -> from_list([state]);
% infer_expr_effects({perform, exn, _}) -> from_list([exn]);

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% Note: Most helper functions are already implemented as exports above.
%% Additional internal helpers can be added here as needed for more
%% sophisticated effect inference.
