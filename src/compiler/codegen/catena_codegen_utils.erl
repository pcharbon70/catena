%%%-------------------------------------------------------------------
%%% @doc Code Generation Utilities
%%%
%%% Shared utilities for Core Erlang code generation including:
%%% - Fresh variable name generation
%%% - Core Erlang literal construction
%%% - Location tracking
%%% @end
%%%-------------------------------------------------------------------
-module(catena_codegen_utils).

-export([
    %% Variable generation
    fresh_var/1,
    fresh_vars/2,
    reset_var_counter/1,
    get_var_counter/1,

    %% State management
    new_state/0,
    with_scope/2,

    %% Core Erlang builders
    c_atom/1,
    c_int/1,
    c_float/1,
    c_string/1,
    c_nil/0,
    c_cons/2,
    c_tuple/1,
    c_var/1,
    c_fname/2,

    %% BIF calls
    c_bif_call/3,
    c_erlang_call/3,

    %% Annotations
    set_ann/2,
    get_ann/1
]).

-export_type([codegen_state/0]).

%%====================================================================
%% Types
%%====================================================================

-record(codegen_state, {
    var_counter = 0 :: non_neg_integer(),
    scope = [] :: [atom()],          % Variable scope stack
    module_name :: atom() | undefined
}).

-opaque codegen_state() :: #codegen_state{}.

%%====================================================================
%% State Management
%%====================================================================

%% @doc Create a new codegen state
-spec new_state() -> codegen_state().
new_state() ->
    #codegen_state{}.

%% @doc Execute function with new scope, then restore
-spec with_scope(fun((codegen_state()) -> {Result, codegen_state()}), codegen_state()) ->
    {Result, codegen_state()} when Result :: term().
with_scope(Fun, State) ->
    OldScope = State#codegen_state.scope,
    {Result, NewState} = Fun(State),
    {Result, NewState#codegen_state{scope = OldScope}}.

%%====================================================================
%% Variable Generation
%%====================================================================

%% @doc Generate a fresh variable name
%%
%% Generates names like _@c0, _@c1, etc.
%% These follow Core Erlang naming conventions for generated variables.
-spec fresh_var(codegen_state()) -> {cerl:cerl(), codegen_state()}.
fresh_var(#codegen_state{var_counter = N} = State) ->
    VarName = list_to_atom("_@c" ++ integer_to_list(N)),
    Var = cerl:c_var(VarName),
    {Var, State#codegen_state{var_counter = N + 1}}.

%% @doc Generate multiple fresh variables
-spec fresh_vars(non_neg_integer(), codegen_state()) -> {[cerl:cerl()], codegen_state()}.
fresh_vars(0, State) ->
    {[], State};
fresh_vars(N, State) when N > 0 ->
    {Var, State1} = fresh_var(State),
    {Vars, State2} = fresh_vars(N - 1, State1),
    {[Var | Vars], State2}.

%% @doc Reset the variable counter (for testing)
-spec reset_var_counter(codegen_state()) -> codegen_state().
reset_var_counter(State) ->
    State#codegen_state{var_counter = 0}.

%% @doc Get the current variable counter
-spec get_var_counter(codegen_state()) -> non_neg_integer().
get_var_counter(#codegen_state{var_counter = N}) ->
    N.

%%====================================================================
%% Core Erlang Builders
%%====================================================================

%% @doc Create a Core Erlang atom
-spec c_atom(atom()) -> cerl:cerl().
c_atom(Atom) when is_atom(Atom) ->
    cerl:c_atom(Atom).

%% @doc Create a Core Erlang integer
-spec c_int(integer()) -> cerl:cerl().
c_int(Int) when is_integer(Int) ->
    cerl:c_int(Int).

%% @doc Create a Core Erlang float
-spec c_float(float()) -> cerl:cerl().
c_float(Float) when is_float(Float) ->
    cerl:c_float(Float).

%% @doc Create a Core Erlang string (as a list of integers)
-spec c_string(binary() | string()) -> cerl:cerl().
c_string(Bin) when is_binary(Bin) ->
    c_string(binary_to_list(Bin));
c_string(Str) when is_list(Str) ->
    cerl:c_string(Str).

%% @doc Create a Core Erlang nil (empty list)
-spec c_nil() -> cerl:cerl().
c_nil() ->
    cerl:c_nil().

%% @doc Create a Core Erlang cons cell
-spec c_cons(cerl:cerl(), cerl:cerl()) -> cerl:cerl().
c_cons(Head, Tail) ->
    cerl:c_cons(Head, Tail).

%% @doc Create a Core Erlang tuple
-spec c_tuple([cerl:cerl()]) -> cerl:cerl().
c_tuple(Elements) ->
    cerl:c_tuple(Elements).

%% @doc Create a Core Erlang variable
-spec c_var(atom()) -> cerl:cerl().
c_var(Name) when is_atom(Name) ->
    cerl:c_var(Name).

%% @doc Create a Core Erlang function name
-spec c_fname(atom(), non_neg_integer()) -> cerl:cerl().
c_fname(Name, Arity) ->
    cerl:c_fname(Name, Arity).

%%====================================================================
%% BIF Calls
%%====================================================================

%% @doc Create a call to an Erlang BIF
%%
%% Translates operations like +, -, *, / to erlang:Op calls
-spec c_bif_call(atom(), [cerl:cerl()], cerl:cerl()) -> cerl:cerl().
c_bif_call(Op, Args, _Ann) ->
    c_erlang_call(Op, Args, _Ann).

%% @doc Create a call to an erlang module function
-spec c_erlang_call(atom(), [cerl:cerl()], cerl:cerl()) -> cerl:cerl().
c_erlang_call(Func, Args, _Ann) ->
    Module = cerl:c_atom(erlang),
    FuncAtom = cerl:c_atom(Func),
    cerl:c_call(Module, FuncAtom, Args).

%%====================================================================
%% Annotations
%%====================================================================

%% @doc Set annotation on a Core Erlang node
%%
%% Annotations preserve source location for debugging
-spec set_ann(cerl:cerl(), term()) -> cerl:cerl().
set_ann(Node, Ann) ->
    cerl:set_ann(Node, [Ann]).

%% @doc Get annotation from a Core Erlang node
-spec get_ann(cerl:cerl()) -> [term()].
get_ann(Node) ->
    cerl:get_ann(Node).
