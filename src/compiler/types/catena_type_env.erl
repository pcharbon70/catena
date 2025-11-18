%%%
%%% @doc Type Environment for Algorithm W
%%%
%%% Implements type environments (Γ) that map variable names to type schemes.
%%% Used in Algorithm W to track the types of let-bound variables and
%%% function parameters during type inference.
%%%
%%% @end
%%%
-module(catena_type_env).

-export([
    empty/0,
    singleton/2,
    lookup/2,
    extend/3,
    remove/2,
    merge/2,
    ftv_env/1,
    from_list/1
]).

%%====================================================================
%% Type Definitions
%%====================================================================

%% Type environment: mapping from variable names to type schemes
-type env() :: #{atom() => catena_type_scheme:scheme()}.

-export_type([env/0]).

%%====================================================================
%% Environment Construction
%%====================================================================

%% @doc Create an empty type environment.
%%
%% Empty environments represent scopes with no variable bindings,
%% such as the top-level scope before any definitions.
%%
%% @returns An empty environment map
%%
%% @see singleton/2
%% @see from_list/1
%% @see extend/3
%%
%% @example
%% ```
%% Env = catena_type_env:empty().
%% %% → #{}
%% '''
-spec empty() -> env().
empty() ->
    #{}.

%% @doc Create a type environment with a single variable binding.
%%
%% Convenience function for creating an environment with exactly one
%% variable-to-scheme mapping. Useful for simple scopes or as a base
%% for building larger environments.
%%
%% @param VarName The variable name (atom)
%% @param Scheme The type scheme for the variable
%% @returns An environment with one binding
%%
%% @see empty/0
%% @see from_list/1
%% @see extend/3
%%
%% @example
%% ```
%% %% Environment with x : Int
%% IntScheme = catena_type_scheme:mono(catena_types:tcon(integer)),
%% Env = catena_type_env:singleton(x, IntScheme).
%% %% → #{x => {mono, {tcon, integer}}}
%% '''
-spec singleton(atom(), catena_type_scheme:scheme()) -> env().
singleton(VarName, Scheme) when is_atom(VarName) ->
    #{VarName => Scheme}.

%% @doc Create a type environment from a list of bindings.
%%
%% Converts a list of `{VarName, Scheme}' pairs into an environment.
%% Useful for constructing environments from multiple bindings at once,
%% such as function parameters or let-bound variables.
%%
%% If there are duplicate variable names, later bindings override
%% earlier ones (standard map behavior).
%%
%% @param Bindings List of `{atom(), scheme()}' pairs
%% @returns An environment containing all the bindings
%%
%% @see empty/0
%% @see singleton/2
%%
%% @example
%% ```
%% %% Environment with multiple bindings
%% IntScheme = catena_type_scheme:mono(catena_types:tcon(integer)),
%% StrScheme = catena_type_scheme:mono(catena_types:tcon(string)),
%% Env = catena_type_env:from_list([
%%     {x, IntScheme},
%%     {y, StrScheme}
%% ]).
%% %% → #{x => {mono, {tcon, integer}}, y => {mono, {tcon, string}}}
%% '''
-spec from_list([{atom(), catena_type_scheme:scheme()}]) -> env().
from_list(Bindings) when is_list(Bindings) ->
    maps:from_list(Bindings).

%%====================================================================
%% Environment Operations
%%====================================================================

%% @doc Look up a variable's type scheme in the environment.
%%
%% Searches the environment for a variable binding. Returns `{ok, Scheme}'
%% if the variable is found, or `none' if it's not in scope.
%%
%% Essential for type inference when encountering variable references.
%% The returned scheme can be instantiated to get a concrete type for
%% the variable's use site.
%%
%% @param Env The type environment to search
%% @param VarName The variable name to look up
%% @returns `{ok, Scheme}' if found, `none' if not in scope
%%
%% @see extend/3
%% @see catena_type_scheme:instantiate/2
%%
%% @example
%% ```
%% %% Successful lookup
%% IntScheme = catena_type_scheme:mono(catena_types:tcon(integer)),
%% Env = catena_type_env:singleton(x, IntScheme),
%% {ok, Scheme} = catena_type_env:lookup(Env, x).
%% %% → {ok, {mono, {tcon, integer}}}
%%
%% %% Variable not in scope
%% none = catena_type_env:lookup(Env, y).
%% '''
-spec lookup(env(), atom()) -> {ok, catena_type_scheme:scheme()} | none.
lookup(Env, VarName) ->
    case maps:find(VarName, Env) of
        {ok, Scheme} -> {ok, Scheme};
        error -> none
    end.

%% @doc Extend the environment with a new variable binding.
%%
%% Adds or updates a variable-to-scheme mapping in the environment.
%% If the variable already exists, the new binding shadows the old one
%% (lexical scoping).
%%
%% Used when entering new scopes (let bindings, lambda parameters, etc.)
%% to track the types of newly bound variables.
%%
%% @param Env The environment to extend
%% @param VarName The variable name to bind
%% @param Scheme The type scheme for the variable
%% @returns A new environment with the added/updated binding
%%
%% @see lookup/2
%% @see remove/2
%%
%% @example
%% ```
%% %% Add binding to empty environment
%% IntScheme = catena_type_scheme:mono(catena_types:tcon(integer)),
%% Env0 = catena_type_env:empty(),
%% Env1 = catena_type_env:extend(Env0, x, IntScheme).
%% %% → #{x => {mono, {tcon, integer}}}
%%
%% %% Shadowing: new binding overrides old
%% StrScheme = catena_type_scheme:mono(catena_types:tcon(string)),
%% Env2 = catena_type_env:extend(Env1, x, StrScheme).
%% %% → #{x => {mono, {tcon, string}}}  (x now has String type)
%% '''
-spec extend(env(), atom(), catena_type_scheme:scheme()) -> env().
extend(Env, VarName, Scheme) ->
    maps:put(VarName, Scheme, Env).

%% @doc Remove a variable binding from the environment.
%%
%% Deletes a variable from the environment. If the variable doesn't exist,
%% returns the environment unchanged.
%%
%% Used when exiting scopes to restore the outer environment, though in
%% pure functional style it's often easier to just use the previous
%% environment instead of explicitly removing bindings.
%%
%% @param Env The environment to modify
%% @param VarName The variable name to remove
%% @returns A new environment without the binding
%%
%% @see extend/3
%%
%% @example
%% ```
%% %% Remove existing binding
%% IntScheme = catena_type_scheme:mono(catena_types:tcon(integer)),
%% Env0 = catena_type_env:singleton(x, IntScheme),
%% Env1 = catena_type_env:remove(Env0, x).
%% %% → #{}
%%
%% %% Removing non-existent variable is safe
%% Env2 = catena_type_env:remove(Env1, y).
%% %% → #{}
%% '''
-spec remove(env(), atom()) -> env().
remove(Env, VarName) ->
    maps:remove(VarName, Env).

%% @doc Merge two environments.
%%
%% Combines two environments, with bindings from Env2 taking precedence
%% over bindings from Env1 in case of conflicts. Useful for combining
%% bindings from multiple patterns.
%%
%% @param Env1 The first environment
%% @param Env2 The second environment (takes precedence)
%% @returns Merged environment
-spec merge(env(), env()) -> env().
merge(Env1, Env2) ->
    maps:merge(Env1, Env2).

%%====================================================================
%% Free Type Variables
%%====================================================================

%% @doc Compute the free type variables in an environment.
%%
%% Returns the union of all free type variables from all type schemes
%% in the environment. These are the variables that are still "unknown"
%% and can be constrained by unification.
%%
%% Essential for generalization - when generalizing a type at a let binding,
%% we quantify over variables that are local to the binding (not free in
%% the environment).
%%
%% @param Env The environment to analyze
%% @returns Set of all free type variable IDs across all bindings
%%
%% @see catena_type_scheme:ftv_scheme/1
%% @see catena_type_scheme:generalize/2
%%
%% @example
%% ```
%% %% Environment with one binding: x : α₁
%% State0 = catena_type_state:new(),
%% {Alpha, _State1} = catena_types:fresh_var(State0),
%% Scheme1 = catena_type_scheme:mono(Alpha),
%% Env1 = catena_type_env:singleton(x, Scheme1),
%% Vars1 = catena_type_env:ftv_env(Env1).
%% %% → set containing 1
%%
%% %% Environment with multiple bindings: x : α₁, y : α₁ -> α₂
%% {Beta, _State2} = catena_types:fresh_var(State0),
%% FuncType = catena_types:tfun(Alpha, Beta, catena_types:empty_effects()),
%% Scheme2 = catena_type_scheme:mono(FuncType),
%% Env2 = catena_type_env:extend(Env1, y, Scheme2),
%% Vars2 = catena_type_env:ftv_env(Env2).
%% %% → set containing 1 and 2 (union of vars from both bindings)
%%
%% %% Environment with quantified scheme: x : ∀α. α -> α
%% PolyScheme = catena_type_scheme:poly([1], FuncType),
%% Env3 = catena_type_env:singleton(z, PolyScheme),
%% Vars3 = catena_type_env:ftv_env(Env3).
%% %% → set containing only 2 (α₁ is quantified, α₂ is free)
%% '''
-spec ftv_env(env()) -> sets:set(catena_types:type_var_id()).
ftv_env(Env) ->
    maps:fold(
        fun(_VarName, Scheme, Acc) ->
            SchemeFtv = catena_type_scheme:ftv_scheme(Scheme),
            sets:union(Acc, SchemeFtv)
        end,
        sets:new(),
        Env
    ).
