%%%
%%% @doc Monadic Combinators for Type Inference
%%%
%%% Provides monadic-style combinators to eliminate deep case nesting
%%% in inference functions and make the code more readable and maintainable.
%%%
%%% The inference monad represents computations that produce either:
%%% - {Result, State} on success
%%% - {error, Error, State} on failure
%%%
%%% @end
%%%
-module(catena_infer_monad).

-export([
    return/2,
    bind/3,
    map/2,
    lift/2,
    sequence/1,
    foldM/3,
    combine/2,
    with_fresh_var/1,
    with_fresh_vars/2
]).

%%====================================================================
%% Type Definitions
%%====================================================================

%% Type for inference monad computations
-type infer_m(A) :: fun((catena_infer_state:infer_state()) ->
    {A, catena_infer_state:infer_state()} |
    {error, catena_type_error:type_error(), catena_infer_state:infer_state()}).

-export_type([infer_m/1]).

%%====================================================================
%% Core Monad Operations
%%====================================================================

%% @doc Return a successful computation with a value
%% Equivalent to: fun(State) -> {Value, State} end
-spec return(A, catena_infer_state:infer_state()) ->
    {A, catena_infer_state:infer_state()}.
return(Value, State) ->
    {Value, State}.

%% @doc Monadic bind operation
%% Chains computations where the second function depends on the first's result
%% Equivalent to Haskell's >>= operator
-spec bind(infer_m(A), fun((A) -> infer_m(B)), catena_infer_state:infer_state()) ->
    {B, catena_infer_state:infer_state()} |
    {error, catena_type_error:type_error(), catena_infer_state:infer_state()}.
bind(CompA, FunctionB, State) ->
    case CompA(State) of
        {ResultA, State1} ->
            (FunctionB(ResultA))(State1);
        {error, Error, State1} ->
            {error, Error, State1}
    end.

%% @doc Map a pure function over a monadic computation
-spec map(fun((A) -> B), infer_m(A)) -> infer_m(B).
map(Fn, Comp) ->
    fun(State) ->
        case Comp(State) of
            {Result, State1} ->
                {Fn(Result), State1};
            {error, Error, State1} ->
                {error, Error, State1}
        end
    end.

%% @doc Lift a state-modifying computation into the monad
%% Takes a function {Value, State} -> {Value2, State2}
-spec lift(fun((A, catena_infer_state:infer_state()) ->
    {B, catena_infer_state:infer_state()}), infer_m(A)) -> infer_m(B).
lift(StateFn, Comp) ->
    fun(State) ->
        case Comp(State) of
            {Result, State1} ->
                StateFn(Result, State1);
            {error, Error, State1} ->
                {error, Error, State1}
        end
    end.

%%====================================================================
%% Utility Combinators
%%====================================================================

%% @doc Execute a sequence of computations, collecting results
-spec sequence([infer_m(A)]) -> infer_m([A]).
sequence(Comps) ->
    fun(State) ->
        sequence_acc(Comps, [], State)
    end.

sequence_acc([], Acc, State) ->
    {lists:reverse(Acc), State};
sequence_acc([Comp | Rest], Acc, State) ->
    case Comp(State) of
        {Result, State1} ->
            sequence_acc(Rest, [Result | Acc], State1);
        {error, Error, State1} ->
            {error, Error, State1}
    end.

%% @doc Monadic fold (similar to lists:foldl but with monadic computation)
-spec foldM(fun((A, B, catena_infer_state:infer_state()) ->
    {B, catena_infer_state:infer_state()} |
    {error, catena_type_error:type_error(), catena_infer_state:infer_state()}),
    [A], B) -> infer_m(B).
foldM(Fn, List, Initial) ->
    fun(State) ->
        foldM_acc(Fn, List, Initial, State)
    end.

foldM_acc(_Fn, [], Acc, State) ->
    {Acc, State};
foldM_acc(Fn, [Item | Rest], Acc, State) ->
    case Fn(Item, Acc, State) of
        {NewAcc, State1} ->
            foldM_acc(Fn, Rest, NewAcc, State1);
        {error, Error, State1} ->
            {error, Error, State1}
    end.

%% @doc Combine two inference monads, returning both results as a tuple
-spec combine(infer_m(A), infer_m(B)) -> infer_m({A, B}).
combine(CompA, CompB) ->
    fun(State) ->
        case CompA(State) of
            {ResultA, State1} ->
                case CompB(State1) of
                    {ResultB, State2} ->
                        {{ResultA, ResultB}, State2};
                    {error, Error, State2} ->
                        {error, Error, State2}
                end;
            {error, Error, State1} ->
                {error, Error, State1}
        end
    end.

%%====================================================================
%% Fresh Variable Utilities
%%====================================================================

%% @doc Run a computation with a fresh type variable
%% Provides the fresh type variable as the first argument to the computation
-spec with_fresh_var(fun((catena_types:type()) -> infer_m(A))) -> infer_m(A).
with_fresh_var(Comp) ->
    fun(State) ->
        case catena_infer_state:fresh_var(State) of
            {FreshVar, State1} ->
                (Comp(FreshVar))(State1)
        end
    end.

%% @doc Run a computation with multiple fresh type variables
%% Useful for binary operations, function types, etc.
-spec with_fresh_vars(pos_integer(), fun(([catena_types:type()]) -> infer_m(A))) -> infer_m(A).
with_fresh_vars(Count, Comp) ->
    fun(State) ->
        generate_fresh_vars(Count, [], State, Comp)
    end.

generate_fresh_vars(0, Vars, State, Comp) ->
    (Comp(lists:reverse(Vars)))(State);
generate_fresh_vars(Count, Vars, State, Comp) ->
    case catena_infer_state:fresh_var(State) of
        {FreshVar, State1} ->
            generate_fresh_vars(Count - 1, [FreshVar | Vars], State1, Comp)
    end.

%%====================================================================
%% Convenience Functions for Common Patterns
%%====================================================================

%% @doc Infer a list of expressions using sequence
infer_exprs(Exprs, Env) ->
    sequence([fun(S) -> catena_infer_expr:infer(Expr, Env, S) end || Expr <- Exprs]).

%% @doc Infer patterns and merge bindings using foldM
infer_patterns_merge(Patterns, Env) ->
    foldM(
        fun(Pattern, AccBindings, State) ->
            case catena_infer_pattern:infer(Pattern, Env, State) of
                {Type, Bindings, State1} ->
                    Merged = catena_infer_pattern:merge_bindings(AccBindings, Bindings),
                    {{Type, Merged}, State1};
                {error, Error, State1} ->
                    {error, Error, State1}
            end
        end,
        Patterns,
        catena_type_env:empty()
    ).