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
    new_state/1,
    with_scope/2,
    with_function_scope/4,
    with_bindings/3,
    with_runtime_context/3,
    is_bound/2,
    runtime_context/1,
    has_runtime_context/1,
    is_effectful_transform/2,
    effect_entry_name/1,
    resolution_enabled/1,
    resolve_transform/4,
    resolve_constructor/4,
    resolve_value/3,
    resolve_trait_method/4,
    resolve_trait_value/3,
    callable_inventory/1,
    import_resolution/1,

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
    scope = [] :: [atom()],
    module_name :: atom() | undefined,
    current_transform :: atom() | undefined,
    callables :: catena_call_resolution:inventory() | undefined,
    imports :: catena_import_resolution:resolution() | undefined,
    traits :: catena_trait_dictionary:inventory() | undefined,
    runtime_context :: cerl:cerl() | undefined,
    effectful_transforms = #{} :: #{atom() => non_neg_integer()}
}).

-opaque codegen_state() :: #codegen_state{}.

%%====================================================================
%% State Management
%%====================================================================

%% @doc Create a new codegen state
-spec new_state() -> codegen_state().
new_state() ->
    #codegen_state{}.

%% @doc Create a state with the validated module resolution context.
-spec new_state(map()) -> codegen_state().
new_state(Context) when is_map(Context) ->
    #codegen_state{
        module_name = maps:get(module_name, Context, undefined),
        callables = maps:get(callables, Context, undefined),
        imports = maps:get(import_resolution, Context, undefined),
        traits = maps:get(trait_inventory, Context, undefined),
        effectful_transforms = maps:get(
            effectful_transforms,
            Context,
            #{}
        )
    }.

%% @doc Execute function with new scope, then restore
-spec with_scope(fun((codegen_state()) -> {Result, codegen_state()}), codegen_state()) ->
    {Result, codegen_state()} when Result :: term().
with_scope(Fun, State) ->
    OldScope = State#codegen_state.scope,
    {Result, NewState} = Fun(State),
    {Result, NewState#codegen_state{scope = OldScope}}.

%% @doc Compile a function body with its transform and parameters in scope.
-spec with_function_scope(
    atom(),
    [atom()],
    fun((codegen_state()) -> {Result, codegen_state()}),
    codegen_state()
) -> {Result, codegen_state()} when Result :: term().
with_function_scope(Name, Bindings, Fun, State) ->
    OldScope = State#codegen_state.scope,
    OldTransform = State#codegen_state.current_transform,
    FunctionState = State#codegen_state{
        scope = lists:usort(Bindings ++ OldScope),
        current_transform = Name
    },
    {Result, NewState} = Fun(FunctionState),
    {Result, NewState#codegen_state{
        scope = OldScope,
        current_transform = OldTransform
    }}.

%% @doc Compile an expression with additional runtime values in lexical scope.
-spec with_bindings(
    [atom()],
    fun((codegen_state()) -> {Result, codegen_state()}),
    codegen_state()
) -> {Result, codegen_state()} when Result :: term().
with_bindings(Bindings, Fun, State) ->
    OldScope = State#codegen_state.scope,
    ScopedState = State#codegen_state{
        scope = lists:usort(Bindings ++ OldScope)
    },
    {Result, NewState} = Fun(ScopedState),
    {Result, NewState#codegen_state{scope = OldScope}}.

%% @doc Compile inside one explicit effect-runtime context.
-spec with_runtime_context(
    cerl:cerl(),
    fun((codegen_state()) -> {Result, codegen_state()}),
    codegen_state()
) -> {Result, codegen_state()} when Result :: term().
with_runtime_context(ContextVar, Fun, State) ->
    OldContext = State#codegen_state.runtime_context,
    {Result, NewState} = Fun(
        State#codegen_state{runtime_context = ContextVar}
    ),
    {Result, NewState#codegen_state{runtime_context = OldContext}}.

%% @doc Return whether a source variable is bound as a runtime value.
-spec is_bound(atom(), codegen_state()) -> boolean().
is_bound(Name, #codegen_state{scope = Scope}) ->
    lists:member(Name, Scope).

%% @doc Return the explicit runtime context currently in lexical scope.
-spec runtime_context(codegen_state()) -> cerl:cerl() | undefined.
runtime_context(#codegen_state{runtime_context = Context}) ->
    Context.

-spec has_runtime_context(codegen_state()) -> boolean().
has_runtime_context(State) ->
    runtime_context(State) =/= undefined.

%% @doc Return whether a local transform consumes a threaded context.
-spec is_effectful_transform(atom(), codegen_state()) -> boolean().
is_effectful_transform(
    Name,
    #codegen_state{effectful_transforms = EffectfulTransforms}
) ->
    maps:is_key(Name, EffectfulTransforms).

%% @doc Collision-resistant internal entry point for an effectful transform.
-spec effect_entry_name(atom()) -> atom().
effect_entry_name(Name) ->
    list_to_atom("$catena_effect_entry$" ++ atom_to_list(Name)).

%% @doc Return whether local callable resolution is enabled for this state.
-spec resolution_enabled(codegen_state()) -> boolean().
resolution_enabled(#codegen_state{callables = undefined}) ->
    false;
resolution_enabled(#codegen_state{callables = Inventory}) ->
    catena_call_resolution:is_inventory(Inventory).

%% @doc Resolve a direct local transform with source-oriented context.
-spec resolve_transform(atom(), non_neg_integer(), term(), codegen_state()) ->
    {ok, catena_call_resolution:callable()} |
    {error, catena_backend_error:diagnostic()}.
resolve_transform(
    Name,
    Arity,
    SourceTerm,
    #codegen_state{
        module_name = Module,
        current_transform = Transform,
        callables = Inventory,
        imports = Imports
    }
) ->
    Context = catena_backend_error:context(
        call_resolution,
        call,
        SourceTerm,
        #{
            module => Module,
            transform => Transform
        }
    ),
    case catena_call_resolution:lookup(Name, Inventory) of
        [] when Imports =/= undefined ->
            catena_import_resolution:resolve_transform(
                Name,
                Arity,
                Imports,
                Context
            );
        _ ->
            catena_call_resolution:resolve_transform(
                Name,
                Arity,
                Inventory,
                Context
            )
    end.

%% @doc Resolve a constructor application with source-oriented context.
-spec resolve_constructor(atom(), non_neg_integer(), term(), codegen_state()) ->
    {ok, catena_call_resolution:callable()} |
    {error, catena_backend_error:diagnostic()}.
resolve_constructor(
    Name,
    Arity,
    SourceTerm,
    #codegen_state{
        module_name = Module,
        current_transform = Transform,
        callables = Inventory,
        imports = Imports
    }
) ->
    Context = catena_backend_error:context(
        constructor_resolution,
        constructor,
        SourceTerm,
        #{
            module => Module,
            transform => Transform
        }
    ),
    case catena_call_resolution:lookup(Name, Inventory) of
        [] when Imports =/= undefined ->
            catena_import_resolution:resolve_constructor(
                Name,
                Arity,
                Imports,
                Context
            );
        _ ->
            catena_call_resolution:resolve_constructor(
                Name,
                Arity,
                Inventory,
                Context
            )
    end.

%% @doc Resolve a top-level callable used as a first-class value.
-spec resolve_value(atom(), term(), codegen_state()) ->
    {ok, catena_call_resolution:callable()} |
    {error, catena_backend_error:diagnostic()}.
resolve_value(
    Name,
    SourceTerm,
    #codegen_state{
        module_name = Module,
        current_transform = Transform,
        callables = Inventory,
        imports = Imports
    }
) ->
    Context = catena_backend_error:context(
        callable_value_resolution,
        callable_value,
        SourceTerm,
        #{
            module => Module,
            transform => Transform
        }
    ),
    case catena_call_resolution:lookup(Name, Inventory) of
        [] when Imports =/= undefined ->
            catena_import_resolution:resolve_value(
                Name,
                Imports,
                Context
            );
        _ ->
            catena_call_resolution:resolve_value(Name, Inventory, Context)
    end.

%% @doc Return the callable inventory carried by the state.
-spec callable_inventory(codegen_state()) ->
    catena_call_resolution:inventory() | undefined.
callable_inventory(#codegen_state{callables = Inventory}) ->
    Inventory.

%% @doc Return the executable imported-symbol inventory.
-spec import_resolution(codegen_state()) ->
    catena_import_resolution:resolution() | undefined.
import_resolution(#codegen_state{imports = Imports}) ->
    Imports.

%% @doc Resolve a trait method to its visible runtime dictionaries.
-spec resolve_trait_method(
    atom(),
    non_neg_integer(),
    term(),
    codegen_state()
) -> {ok, [map()]} | {error, term()}.
resolve_trait_method(
    Name,
    Arity,
    SourceTerm,
    #codegen_state{
        module_name = Module,
        current_transform = Transform,
        traits = Inventory
    }
) ->
    Context = catena_backend_error:context(
        trait_dispatch,
        trait_method,
        SourceTerm,
        #{module => Module, transform => Transform}
    ),
    case Inventory of
        undefined ->
            {error, catena_backend_error:unresolved_call(
                Name,
                Arity,
                Context#{callable_kind => trait_method}
            )};
        _ ->
            catena_trait_dictionary:resolve_method(
                Name,
                Arity,
                Inventory,
                Context
            )
    end.

%% @doc Resolve a trait method referenced as a first-class function.
-spec resolve_trait_value(atom(), term(), codegen_state()) ->
    {ok, non_neg_integer(), [map()]} | {error, term()}.
resolve_trait_value(
    Name,
    SourceTerm,
    #codegen_state{
        module_name = Module,
        current_transform = Transform,
        traits = Inventory
    }
) ->
    Context = catena_backend_error:context(
        trait_dispatch,
        trait_method_value,
        SourceTerm,
        #{module => Module, transform => Transform}
    ),
    case Inventory of
        undefined ->
            {error, catena_backend_error:unresolved_call(
                Name,
                0,
                Context#{callable_kind => trait_method}
            )};
        _ ->
            catena_trait_dictionary:resolve_method_value(
                Name,
                Inventory,
                Context
            )
    end.

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
