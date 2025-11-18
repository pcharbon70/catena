%%%-------------------------------------------------------------------
%%% @doc Unification Algorithm for Type Inference
%%%
%%% This module implements Robinson's unification algorithm with occurs check,
%%% which is the core of Algorithm W. It solves type equations by finding
%%% most general unifiers (MGU).
%%%
%%% The unification algorithm handles:
%%% - Type variable unification with occurs check
%%% - Type constructor unification
%%% - Function type unification with effect matching
%%% - Record type unification with row polymorphism
%%% - Tuple type unification
%%% - Type application unification
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_infer_unify).

-export([
    unify/3,
    unify_many/3,
    unify_effects/2,
    unify_types/2
]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Unify two types, returning a substitution that makes them equal
%% Returns either a substitution or an error
%%
%% **Pattern 1 Error:** Returns {error, Error, State} for inference errors
%% since this function threads inference state through the unification process.
-spec unify(catena_types:type(), catena_types:type(), catena_infer_state:infer_state()) ->
    {ok, catena_type_subst:subst(), catena_infer_state:infer_state()} |
    {error, catena_type_error:type_error(), catena_infer_state:infer_state()}.
unify(T1, T2, State) ->
    % Apply current substitution to both types first
    Subst = catena_infer_state:get_subst(State),
    T1_sub = catena_type_subst:apply(Subst, T1),
    T2_sub = catena_type_subst:apply(Subst, T2),

    % Perform unification
    case unify_types(T1_sub, T2_sub) of
        {ok, NewSubst} ->
            % Compose with existing substitution
            State1 = catena_infer_state:compose_subst(NewSubst, State),
            {ok, NewSubst, State1};
        {error, Error} ->
            State1 = catena_infer_state:add_error(Error, State),
            {error, Error, State1}
    end.

%% @doc Unify multiple pairs of types
%% Returns accumulated substitution or first error encountered
-spec unify_many([{catena_types:type(), catena_types:type()}], catena_type_subst:subst(),
                 catena_infer_state:infer_state()) ->
    {ok, catena_type_subst:subst(), catena_infer_state:infer_state()} |
    {error, catena_type_error:type_error(), catena_infer_state:infer_state()}.
unify_many([], FinalSubst, State) ->
    {ok, FinalSubst, State};
unify_many([{T1, T2} | Rest], AccSubst, State) ->
    % Apply accumulated substitution to types
    T1_sub = catena_type_subst:apply(AccSubst, T1),
    T2_sub = catena_type_subst:apply(AccSubst, T2),

    % Unify this pair
    case unify_types(T1_sub, T2_sub) of
        {ok, NewSubst} ->
            % Compose substitutions
            ComposedSubst = catena_type_subst:compose(NewSubst, AccSubst),
            State1 = catena_infer_state:compose_subst(NewSubst, State),
            unify_many(Rest, ComposedSubst, State1);
        {error, Error} ->
            State1 = catena_infer_state:add_error(Error, State),
            {error, Error, State1}
    end.

%% @doc Unify two effect sets
%% In the PoC, effects are monomorphic (no effect variables),
%% so we just check for equality
%%
%% **Pattern 2 Error:** Returns {error, Error} for validation errors
%% since this function doesn't thread inference state.
-spec unify_effects(catena_types:effect_set(), catena_types:effect_set()) ->
    ok | {error, catena_type_error:type_error()}.
unify_effects(Effects1, Effects2) ->
    case catena_types:effects_equal(Effects1, Effects2) of
        true ->
            ok;
        false ->
            {error, catena_type_error:effect_mismatch(Effects1, Effects2)}
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Core unification algorithm
%% Implements Robinson's unification with occurs check
-spec unify_types(catena_types:type(), catena_types:type()) ->
    {ok, catena_type_subst:subst()} | {error, catena_type_error:type_error()}.

% Rule: Identity
% T ≡ T
unify_types(T, T) ->
    {ok, catena_type_subst:empty()};

% Rule: Variable-Type (left)
% α ≡ T  where α ∉ FV(T)
unify_types({tvar, VarId}, Type) ->
    case catena_type_subst:occurs_check(VarId, Type) of
        true ->
            {error, catena_type_error:occurs_check(VarId, Type)};
        false ->
            {ok, catena_type_subst:singleton(VarId, Type)}
    end;

% Rule: Variable-Type (right)
% T ≡ α  where α ∉ FV(T)
unify_types(Type, {tvar, VarId}) ->
    case catena_type_subst:occurs_check(VarId, Type) of
        true ->
            {error, catena_type_error:occurs_check(VarId, Type)};
        false ->
            {ok, catena_type_subst:singleton(VarId, Type)}
    end;

% Rule: Type Constructor
% c ≡ c  (same constructor)
unify_types({tcon, Con}, {tcon, Con}) ->
    {ok, catena_type_subst:empty()};

% Rule: Type Constructor Mismatch
% c1 ≡ c2  where c1 ≠ c2
unify_types({tcon, _} = T1, {tcon, _} = T2) ->
    {error, catena_type_error:unification_error(T1, T2)};

% Rule: Function Type
% (T1 -> T2) ≡ (T3 -> T4)  requires  T1 ≡ T3 ∧ T2 ≡ T4 ∧ E1 ≡ E2
unify_types({tfun, From1, To1, Effects1}, {tfun, From2, To2, Effects2}) ->
    case unify_types(From1, From2) of
        {ok, Subst1} ->
            % Apply substitution to remaining types
            To1_sub = catena_type_subst:apply(Subst1, To1),
            To2_sub = catena_type_subst:apply(Subst1, To2),

            case unify_types(To1_sub, To2_sub) of
                {ok, Subst2} ->
                    % Check effects match
                    case unify_effects(Effects1, Effects2) of
                        ok ->
                            % Compose substitutions
                            {ok, catena_type_subst:compose(Subst2, Subst1)};
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;

% Rule: Type Application
% C T1 ... Tn ≡ C S1 ... Sn  requires  Ti ≡ Si for all i
unify_types({tapp, Con1, Args1}, {tapp, Con2, Args2}) ->
    case unify_types(Con1, Con2) of
        {ok, Subst1} ->
            % Apply substitution to args
            Args1_sub = lists:map(fun(A) -> catena_type_subst:apply(Subst1, A) end, Args1),
            Args2_sub = lists:map(fun(A) -> catena_type_subst:apply(Subst1, A) end, Args2),

            % Check argument counts match
            Len1 = length(Args1_sub),
            Len2 = length(Args2_sub),

            if
                Len1 =/= Len2 ->
                    {error, catena_type_error:arity_mismatch(Con1, Len1, Len2)};
                true ->
                    % Unify arguments pairwise
                    unify_args(Args1_sub, Args2_sub, Subst1)
            end;
        {error, _} = Error ->
            Error
    end;

% Rule: Tuple Type
% (T1, ..., Tn) ≡ (S1, ..., Sn)  requires  Ti ≡ Si for all i
unify_types({ttuple, Elements1}, {ttuple, Elements2}) ->
    Len1 = length(Elements1),
    Len2 = length(Elements2),

    if
        Len1 =/= Len2 ->
            {error, catena_type_error:unification_error({ttuple, Elements1}, {ttuple, Elements2})};
        true ->
            unify_args(Elements1, Elements2, catena_type_subst:empty())
    end;

% Rule: Record Type (closed)
% {l1: T1, ..., ln: Tn | closed} ≡ {l1: S1, ..., ln: Sn | closed}
% requires Ti ≡ Si for all i and same field labels
unify_types({trecord, Fields1, closed}, {trecord, Fields2, closed}) ->
    % Sort fields by label for comparison
    Sorted1 = lists:keysort(1, Fields1),
    Sorted2 = lists:keysort(1, Fields2),

    % Extract labels
    Labels1 = [Label || {Label, _} <- Sorted1],
    Labels2 = [Label || {Label, _} <- Sorted2],

    case Labels1 =:= Labels2 of
        false ->
            {error, catena_type_error:unification_error({trecord, Fields1, closed}, {trecord, Fields2, closed})};
        true ->
            % Unify field types
            Types1 = [Type || {_, Type} <- Sorted1],
            Types2 = [Type || {_, Type} <- Sorted2],
            unify_args(Types1, Types2, catena_type_subst:empty())
    end;

% Rule: Record Type (row variable, same)
% {fields | α} ≡ {fields | α}
unify_types({trecord, Fields1, RowVar}, {trecord, Fields2, RowVar})
  when is_integer(RowVar) ->
    % Same row variable - just unify common fields
    unify_record_fields(Fields1, Fields2);

% Rule: Record Type (different row variables)
% {fields1 | α} ≡ {fields2 | β}  where α ≠ β
% This requires row unification - simplified for PoC
unify_types({trecord, Fields1, Row1}, {trecord, Fields2, Row2})
  when is_integer(Row1), is_integer(Row2), Row1 =/= Row2 ->
    % For PoC: require fields match exactly, unify row variables
    case unify_record_fields(Fields1, Fields2) of
        {ok, Subst1} ->
            % Unify row variables (treat as type variables)
            case unify_types({tvar, Row1}, {tvar, Row2}) of
                {ok, Subst2} ->
                    % Compose field substitution with row variable substitution
                    ComposedSubst = catena_type_subst:compose(Subst1, Subst2),
                    {ok, ComposedSubst};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;

% Rule: Record Type (row variable vs closed)
% {fields | α} ≡ {fields | closed}
% Instantiate α to closed if fields match
unify_types({trecord, Fields1, RowVar}, {trecord, Fields2, closed})
  when is_integer(RowVar) ->
    case unify_record_fields(Fields1, Fields2) of
        {ok, Subst1} ->
            % Row variable unifies with closed - bind row variable to empty record
            RowSubst = catena_type_subst:singleton(RowVar, {trecord, [], closed}),
            ComposedSubst = catena_type_subst:compose(RowSubst, Subst1),
            {ok, ComposedSubst};
        {error, _} = Error ->
            Error
    end;

unify_types({trecord, Fields1, closed}, {trecord, Fields2, RowVar})
  when is_integer(RowVar) ->
    unify_types({trecord, Fields2, RowVar}, {trecord, Fields1, closed});

% Rule: Variant Type
% [C1 T1 | ... | Cn Tn] ≡ [C1 S1 | ... | Cn Sn]
% requires Ti ≡ Si for all i and same constructors
unify_types({tvariant, Constructors1}, {tvariant, Constructors2}) ->
    % Sort constructors by name
    Sorted1 = lists:keysort(1, Constructors1),
    Sorted2 = lists:keysort(1, Constructors2),

    % Extract constructor names
    Names1 = [Name || {Name, _} <- Sorted1],
    Names2 = [Name || {Name, _} <- Sorted2],

    case Names1 =:= Names2 of
        false ->
            {error, catena_type_error:unification_error({tvariant, Constructors1}, {tvariant, Constructors2})};
        true ->
            % Unify constructor argument types
            unify_variant_constructors(Sorted1, Sorted2, catena_type_subst:empty())
    end;

% Mismatch: Different type structures
unify_types(T1, T2) ->
    {error, catena_type_error:unification_error(T1, T2)}.

%% @doc Unify lists of arguments pairwise
-spec unify_args([catena_types:type()], [catena_types:type()], catena_type_subst:subst()) ->
    {ok, catena_type_subst:subst()} | {error, catena_type_error:type_error()}.
unify_args([], [], AccSubst) ->
    {ok, AccSubst};
unify_args([Arg1 | Rest1], [Arg2 | Rest2], AccSubst) ->
    % Apply accumulated substitution
    Arg1_sub = catena_type_subst:apply(AccSubst, Arg1),
    Arg2_sub = catena_type_subst:apply(AccSubst, Arg2),

    case unify_types(Arg1_sub, Arg2_sub) of
        {ok, NewSubst} ->
            % Compose and continue
            ComposedSubst = catena_type_subst:compose(NewSubst, AccSubst),
            unify_args(Rest1, Rest2, ComposedSubst);
        {error, _} = Error ->
            Error
    end.

%% @doc Unify record fields (requires exact match of labels and types)
-spec unify_record_fields([{atom(), catena_types:type()}],
                         [{atom(), catena_types:type()}]) ->
    {ok, catena_type_subst:subst()} | {error, catena_type_error:type_error()}.
unify_record_fields(Fields1, Fields2) ->
    % Sort by label
    Sorted1 = lists:keysort(1, Fields1),
    Sorted2 = lists:keysort(1, Fields2),

    % Check labels match
    Labels1 = [Label || {Label, _} <- Sorted1],
    Labels2 = [Label || {Label, _} <- Sorted2],

    case Labels1 =:= Labels2 of
        false ->
            {error, catena_type_error:unification_error({trecord, Fields1, closed}, {trecord, Fields2, closed})};
        true ->
            % Unify types
            Types1 = [Type || {_, Type} <- Sorted1],
            Types2 = [Type || {_, Type} <- Sorted2],
            unify_args(Types1, Types2, catena_type_subst:empty())
    end.

%% @doc Unify variant constructors (already sorted by name)
-spec unify_variant_constructors([{atom(), [catena_types:type()]}],
                                [{atom(), [catena_types:type()]}],
                                catena_type_subst:subst()) ->
    {ok, catena_type_subst:subst()} | {error, catena_type_error:type_error()}.
unify_variant_constructors([], [], AccSubst) ->
    {ok, AccSubst};
unify_variant_constructors([{Name, Args1} | Rest1], [{Name, Args2} | Rest2], AccSubst) ->
    Len1 = length(Args1),
    Len2 = length(Args2),

    if
        Len1 =/= Len2 ->
            {error, catena_type_error:arity_mismatch({constructor, Name}, Len1, Len2)};
        true ->
            % Unify constructor arguments
            case unify_args(Args1, Args2, AccSubst) of
                {ok, NewSubst} ->
                    unify_variant_constructors(Rest1, Rest2, NewSubst);
                {error, _} = Error ->
                    Error
            end
    end.
