%%%-------------------------------------------------------------------
%%% @doc Catena Algebraic Laws Library (Phase 8.5)
%%%
%%% This module provides a comprehensive library of algebraic laws
%%% for common effect types. These laws define the equational theory
%%% of algebraic effects and enable:
%%%
%%% - Verification of handler correctness
%%% - Optimization of effectful code
%%% - Equational reasoning about effectful programs
%%%
%%% == Law Categories ==
%%%
%%% The library is organized by effect type:
%%%
%%% - State Laws: get/put interaction laws
%%% - Reader Laws: ask/local interaction laws
%%% - Writer Laws: tell/tell fusion laws
%%% - Error Laws: throw/catch propagation laws
%%% - Choice Laws: alternative selection laws
%%% - Async Laws: spawn/await parallelism laws
%%%
%%% == Law Structure ==
%%%
%%% Each law is represented as an equation with:
%%% - A descriptive name for documentation
%%% - Left-hand side pattern to match
%%% - Right-hand side pattern for rewriting
%%% - Optional guard conditions
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_algebraic_laws).

%% Law set API
-export([
    new_law_set/1,
    get_law_set/1,
    list_law_sets/0,
    combine_law_sets/2
]).

%% State effect laws
-export([
    state_laws/0,
    state_get_put/0,
    state_put_get/0,
    state_put_put/0,
    state_get_id/0
]).

%% Reader effect laws
-export([
    reader_laws/0,
    reader_ask_local/0,
    reader_local_local/0,
    reader_local_id/0
]).

%% Writer effect laws
-export([
    writer_laws/0,
    writer_tell_tell/0,
    writer_tell_unit/0,
    writer_censor/0
]).

%% Error effect laws
-export([
    error_laws/0,
    error_throw_catch/0,
    error_catch_identity/0,
    error_throw_map/0
]).

%% Choice effect laws
-export([
    choice_laws/0,
    choice_assoc/0,
    choice_left_id/0,
    choice_right_id/0,
    choice_comm/0
]).

%% Async effect laws
-export([
    async_laws/0,
    async_spawn_await/0,
    async_map/0,
    async_assoc/0
]).

%% Monadic laws (applicable to all handlers)
-export([
    monadic_laws/0,
    monad_left_id/0,
    monad_right_id/0,
    monad_assoc/0
]).

%%====================================================================
%% Types
%%====================================================================

-type law_name() :: atom().
-type law_set_name() :: atom().
-type equation() :: catena_equations:equation().
-type equation_set() :: catena_equation_spec:equation_set().

-type law_entry() :: #{
    name => law_name(),
    equation => equation(),
    description => string()
}.

-type law_set() :: #{
    name => law_set_name(),
    equations => #{law_name() => equation()},
    metadata => map()
}.

-export_type([
    law_name/0,
    law_set/0,
    law_entry/0
]).

%%====================================================================
%% Law Set API
%%====================================================================

%% @doc Create a new named law set.
-spec new_law_set(law_set_name()) -> law_set().
new_law_set(Name) ->
    #{
        name => Name,
        equations => #{},
        metadata => #{}
    }.

%% @doc Get a predefined law set by name.
-spec get_law_set(law_set_name()) -> equation_set().
get_law_set(state) -> state_laws();
get_law_set(reader) -> reader_laws();
get_law_set(writer) -> writer_laws();
get_law_set(error) -> error_laws();
get_law_set(choice) -> choice_laws();
get_law_set(async) -> async_laws();
get_law_set(monadic) -> monadic_laws();
get_law_set(all) -> all_laws();
get_law_set(_) -> catena_equation_spec:new_set(empty).

%% @doc List all available law set names.
-spec list_law_sets() -> [law_set_name()].
list_law_sets() ->
    [state, reader, writer, error, choice, async, monadic, all].

%% @doc Combine two law sets.
-spec combine_law_sets(equation_set(), equation_set()) -> equation_set().
combine_law_sets(Set1, Set2) ->
    Names1 = catena_equation_spec:list_equations(Set1),
    Names2 = catena_equation_spec:list_equations(Set2),
    Combined = lists:map(fun(Name) ->
        {ok, Eq} = catena_equation_spec:get_equation(Set1, Name),
        {Name, Eq}
    end, Names1) ++ lists:map(fun(Name) ->
        {ok, Eq} = catena_equation_spec:get_equation(Set2, Name),
        {Name, Eq}
    end, Names2),
    catena_equation_spec:add_equations(
        catena_equation_spec:new_set(combined),
        Combined
    ).

%%====================================================================
%% State Effect Laws
%%====================================================================

%% @doc Get all State effect laws.
%%
%% State operations: get(), put(s), modify(f)
%%
%% Laws:
%% - get() >>= put ≡ put (get followed by put equals just put)
%% - put(s) >>= get ≡ put(s) (put then get returns the same s)
%% - put(s1) >>= put(s2) ≡ put(s2) (second put overwrites first)
%% - get() >>= return ≡ get (get with no modification)
-spec state_laws() -> equation_set().
state_laws() ->
    EqSet = catena_equation_spec:new_set(state),
    catena_equation_spec:add_equations(EqSet, [
        {state_get_put, state_get_put()},
        {state_put_get, state_put_get()},
        {state_put_put, state_put_put()},
        {state_get_id, state_get_id()}
    ]).

%% @doc Law: get() >>= put(x) ≡ put(x)
%%
%% Getting the state then immediately setting it is redundant.
-spec state_get_put() -> equation().
state_get_put() ->
    catena_equations:new(
        catena_equations:seq([
            catena_equations:op(get, 0, catena_equations:lit(undefined)),
            catena_equations:bind(s,
                catena_equations:op(put, 1, catena_equations:var(s))
            )
        ]),
        catena_equations:op(put, 1, catena_equations:var(s))
    ).

%% @doc Law: put(s) >> get ≡ put(s)
%%
%% Setting state then getting it returns the set value.
-spec state_put_get() -> equation().
state_put_get() ->
    catena_equations:new(
        catena_equations:seq([
            catena_equations:op(put, 1, catena_equations:var(s)),
            catena_equations:op(get, 0, catena_equations:lit(undefined))
        ]),
        catena_equations:seq([
            catena_equations:op(put, 1, catena_equations:var(s)),
            catena_equations:var(s)
        ])
    ).

%% @doc Law: put(s1) >> put(s2) ≡ put(s2)
%%
%% Sequential puts: only the last one matters.
-spec state_put_put() -> equation().
state_put_put() ->
    catena_equations:new(
        catena_equations:seq([
            catena_equations:op(put, 1, catena_equations:var(s1)),
            catena_equations:op(put, 1, catena_equations:var(s2))
        ]),
        catena_equations:op(put, 1, catena_equations:var(s2))
    ).

%% @doc Law: get() >>= return ≡ get
%%
%% Get with no modification is just get.
-spec state_get_id() -> equation().
state_get_id() ->
    catena_equations:new(
        catena_equations:seq([
            catena_equations:op(get, 0, catena_equations:lit(undefined)),
            catena_equations:bind(s, catena_equations:var(s))
        ]),
        catena_equations:op(get, 0, catena_equations:lit(undefined))
    ).

%%====================================================================
%% Reader Effect Laws
%%====================================================================

%% @doc Get all Reader effect laws.
%%
%% Reader operations: ask(), local(f)
%%
%% Laws:
%% - ask() >>= local ≡ local (local overrides environment)
%% - local(f1) >> local(f2) ≡ local(f2 ∘ f1)
%% - local(id) ≡ ask (local with identity is ask)
-spec reader_laws() -> equation_set().
reader_laws() ->
    EqSet = catena_equation_spec:new_set(reader),
    catena_equation_spec:add_equations(EqSet, [
        {reader_ask_local, reader_ask_local()},
        {reader_local_local, reader_local_local()},
        {reader_local_id, reader_local_id()}
    ]).

%% @doc Law: ask() >>= local(f) ≡ local(f)
%%
%% Reading then locally modifying is just local modification.
-spec reader_ask_local() -> equation().
reader_ask_local() ->
    catena_equations:new(
        catena_equations:seq([
            catena_equations:op(ask, 0, catena_equations:lit(undefined)),
            catena_equations:bind(env,
                catena_equations:op(local, 1, catena_equations:var(f))
            )
        ]),
        catena_equations:op(local, 1, catena_equations:var(f))
    ).

%% @doc Law: local(f1) >> local(f2) ≡ local(f2 ∘ f1)
%%
%% Local modifications compose.
-spec reader_local_local() -> equation().
reader_local_local() ->
    catena_equations:new(
        catena_equations:seq([
            catena_equations:op(local, 1, catena_equations:var(f1)),
            catena_equations:op(local, 1, catena_equations:var(f2))
        ]),
        catena_equations:op(local, 1, catena_equations:op(compose, 2,
            catena_equations:seq([catena_equations:var(f2), catena_equations:var(f1)])
        ))
    ).

%% @doc Law: local(id) ≡ id
%%
%% Local with identity function is transparent.
-spec reader_local_id() -> equation().
reader_local_id() ->
    catena_equations:new(
        catena_equations:seq([
            catena_equations:op(local, 1, catena_equations:var(id)),
            catena_equations:var(action)
        ]),
        catena_equations:var(action)
    ).

%%====================================================================
%% Writer Effect Laws
%%====================================================================

%% @doc Get all Writer effect laws.
%%
%% Writer operations: tell(w), censor(f)
%%
%% Laws:
%% - tell(w1) >> tell(w2) ≡ tell(w1 <> w2)
%% - tell(mempty) ≡ return ()
%% - censor preserves monoid structure
-spec writer_laws() -> equation_set().
writer_laws() ->
    EqSet = catena_equation_spec:new_set(writer),
    catena_equation_spec:add_equations(EqSet, [
        {writer_tell_tell, writer_tell_tell()},
        {writer_tell_unit, writer_tell_unit()},
        {writer_censor, writer_censor()}
    ]).

%% @doc Law: tell(w1) >> tell(w2) ≡ tell(w1 <> w2)
%%
%% Sequential tells accumulate.
-spec writer_tell_tell() -> equation().
writer_tell_tell() ->
    catena_equations:new(
        catena_equations:seq([
            catena_equations:op(tell, 1, catena_equations:var(w1)),
            catena_equations:op(tell, 1, catena_equations:var(w2))
        ]),
        catena_equations:op(tell, 1, catena_equations:op(append, 2,
            catena_equations:seq([catena_equations:var(w1), catena_equations:var(w2)])
        ))
    ).

%% @doc Law: tell(mempty) ≡ return ()
%%
%% Telling empty value is a no-op.
-spec writer_tell_unit() -> equation().
writer_tell_unit() ->
    catena_equations:new(
        catena_equations:op(tell, 1, catena_equations:var(mempty)),
        catena_equations:seq([catena_equations:lit(unit)])
    ).

%% @doc Law: censor(f) preserves monoid structure
%%
%% Censor is a homomorphism on the writer monoid.
-spec writer_censor() -> equation().
writer_censor() ->
    catena_equations:new(
        catena_equations:seq([
            catena_equations:op(censor, 1, catena_equations:var(f)),
            catena_equations:var(action)
        ]),
        catena_equations:var(action)
    ).

%%====================================================================
%% Error Effect Laws
%%====================================================================

%% @doc Get all Error effect laws.
%%
%% Error operations: throw(e), catch/handle
%%
%% Laws:
%% - throw(e) >>= f ≡ throw(e) (error propagates)
%% - catch(throw(e)) ≡ e (catch extracts error)
%% - map doesn't affect thrown errors
-spec error_laws() -> equation_set().
error_laws() ->
    EqSet = catena_equation_spec:new_set(error),
    catena_equation_spec:add_equations(EqSet, [
        {error_throw_catch, error_throw_catch()},
        {error_catch_identity, error_catch_identity()},
        {error_throw_map, error_throw_map()}
    ]).

%% @doc Law: throw(e) >>= f ≡ throw(e)
%%
%% Errors propagate through binds.
-spec error_throw_catch() -> equation().
error_throw_catch() ->
    catena_equations:new(
        catena_equations:seq([
            catena_equations:op(throw, 1, catena_equations:var(e)),
            catena_equations:bind(x, catena_equations:var(f))
        ]),
        catena_equations:op(throw, 1, catena_equations:var(e))
    ).

%% @doc Law: catch(throw(e)) ≡ return(e)
%%
%% Catch extracts the error value.
-spec error_catch_identity() -> equation().
error_catch_identity() ->
    catena_equations:new(
        catena_equations:op('catch', 1,
            catena_equations:op(throw, 1, catena_equations:var(e))
        ),
        catena_equations:var(e)
    ).

%% @doc Law: map(f, throw(e)) ≡ throw(e)
%%
%% Mapping over error doesn't change it.
-spec error_throw_map() -> equation().
error_throw_map() ->
    catena_equations:new(
        catena_equations:op(map, 2,
            catena_equations:seq([
                catena_equations:var(f),
                catena_equations:op(throw, 1, catena_equations:var(e))
            ])
        ),
        catena_equations:op(throw, 1, catena_equations:var(e))
    ).

%%====================================================================
%% Choice Effect Laws
%%====================================================================

%% @doc Get all Choice (Alternative) effect laws.
%%
%% Choice operations: <|> (alternative)
%%
%% Laws:
%%% - (x <|> y) <|> z ≡ x <|> (y <|> z) (associative)
%%% - empty <|> x ≡ x (left identity)
%%% - x <|> empty ≡ x (right identity)
%%% - x <|> y ≡ y <|> x (commutative, for some)
-spec choice_laws() -> equation_set().
choice_laws() ->
    EqSet = catena_equation_spec:new_set(choice),
    catena_equation_spec:add_equations(EqSet, [
        {choice_assoc, choice_assoc()},
        {choice_left_id, choice_left_id()},
        {choice_right_id, choice_right_id()},
        {choice_comm, choice_comm()}
    ]).

%% @doc Law: (x <|> y) <|> z ≡ x <|> (y <|> z)
%%
%%% Alternatives associate.
-spec choice_assoc() -> equation().
choice_assoc() ->
    catena_equations:new(
        catena_equations:op(alt, 2,
            catena_equations:seq([
                catena_equations:op(alt, 2,
                    catena_equations:seq([catena_equations:var(x), catena_equations:var(y)])
                ),
                catena_equations:var(z)
            ])
        ),
        catena_equations:op(alt, 2,
            catena_equations:seq([
                catena_equations:var(x),
                catena_equations:op(alt, 2,
                    catena_equations:seq([catena_equations:var(y), catena_equations:var(z)])
                )
            ])
        )
    ).

%% @doc Law: empty <|> x ≡ x
%%
%% Empty is left identity.
-spec choice_left_id() -> equation().
choice_left_id() ->
    catena_equations:new(
        catena_equations:op(alt, 2,
            catena_equations:seq([catena_equations:var(empty), catena_equations:var(x)])
        ),
        catena_equations:var(x)
    ).

%% @doc Law: x <|> empty ≡ x
%%
%% Empty is right identity.
-spec choice_right_id() -> equation().
choice_right_id() ->
    catena_equations:new(
        catena_equations:op(alt, 2,
            catena_equations:seq([catena_equations:var(x), catena_equations:var(empty)])
        ),
        catena_equations:var(x)
    ).

%% @doc Law: x <|> y ≡ y <|> x (for commutative alternatives)
%%
%% Some alternatives are commutative.
-spec choice_comm() -> equation().
choice_comm() ->
    catena_equations:new(
        catena_equations:op(alt, 2,
            catena_equations:seq([catena_equations:var(x), catena_equations:var(y)])
        ),
        catena_equations:op(alt, 2,
            catena_equations:seq([catena_equations:var(y), catena_equations:var(x)])
        )
    ).

%%====================================================================
%% Async Effect Laws
%%====================================================================

%% @doc Get all Async effect laws.
%%
%% Async operations: spawn(f), await(p)
%%
%% Laws:
%%% - spawn(f) >>= await ≡ spawn(f) (spawn returns process)
%%% - map distributes over spawn
%%% - spawn composition associates
-spec async_laws() -> equation_set().
async_laws() ->
    EqSet = catena_equation_spec:new_set(async),
    catena_equation_spec:add_equations(EqSet, [
        {async_spawn_await, async_spawn_await()},
        {async_map, async_map()},
        {async_assoc, async_assoc()}
    ]).

%% @doc Law: spawn(f) >>= await ≡ spawn(f)
%%
%% Spawn then await is the basic async pattern.
-spec async_spawn_await() -> equation().
async_spawn_await() ->
    catena_equations:new(
        catena_equations:seq([
            catena_equations:op(spawn, 1, catena_equations:var(f)),
            catena_equations:bind(p,
                catena_equations:op(await, 1, catena_equations:var(p))
            )
        ]),
        catena_equations:seq([
            catena_equations:op(spawn, 1, catena_equations:var(f)),
            catena_equations:bind(p,
                catena_equations:op(await, 1, catena_equations:var(p))
            )
        ])
    ).

%% @doc Law: map(f, spawn(g)) ≡ spawn(f ∘ g)
%%
%% Map distributes over spawn.
-spec async_map() -> equation().
async_map() ->
    catena_equations:new(
        catena_equations:op(map, 2,
            catena_equations:seq([
                catena_equations:var(f),
                catena_equations:op(spawn, 1, catena_equations:var(g))
            ])
        ),
        catena_equations:op(spawn, 1, catena_equations:op(compose, 2,
            catena_equations:seq([catena_equations:var(f), catena_equations:var(g)])
        ))
    ).

%% @doc Law: spawn(spawn(f)) ≡ spawn(f) (flattening)
%%
%% Nested spawn can be flattened.
-spec async_assoc() -> equation().
async_assoc() ->
    catena_equations:new(
        catena_equations:op(spawn, 1,
            catena_equations:op(spawn, 1, catena_equations:var(f))
        ),
        catena_equations:op(spawn, 1, catena_equations:var(f))
    ).

%%====================================================================
%% Monadic Laws (Universal)
%%====================================================================

%% @doc Get all monadic laws.
%%
%%% These laws apply to any handler implementing a monad:
%%% - left identity: return x >>= f ≡ f x
%%% - right identity: m >>= return ≡ m
%%% - associativity: (m >>= f) >>= g ≡ m >>= (x -> f x >>= g)
-spec monadic_laws() -> equation_set().
monadic_laws() ->
    EqSet = catena_equation_spec:new_set(monadic),
    catena_equation_spec:add_equations(EqSet, [
        {monad_left_id, monad_left_id()},
        {monad_right_id, monad_right_id()},
        {monad_assoc, monad_assoc()}
    ]).

%% @doc Law: return x >>= f ≡ f x
%%
%% Left identity: wrapping then binding is same as applying.
-spec monad_left_id() -> equation().
monad_left_id() ->
    catena_equations:new(
        catena_equations:seq([
            catena_equations:op(return, 1, catena_equations:var(x)),
            catena_equations:bind(v, catena_equations:var(f))
        ]),
        catena_equations:op(apply, 2,
            catena_equations:seq([catena_equations:var(f), catena_equations:var(x)])
        )
    ).

%% @doc Law: m >>= return ≡ m
%%
%% Right identity: binding return is identity.
-spec monad_right_id() -> equation().
monad_right_id() ->
    catena_equations:new(
        catena_equations:seq([
            catena_equations:var(m),
            catena_equations:bind(x,
                catena_equations:op(return, 1, catena_equations:var(x))
            )
        ]),
        catena_equations:var(m)
    ).

%% @doc Law: (m >>= f) >>= g ≡ m >>= (x -> f x >>= g)
%%
%% Associativity: binding chains can be regrouped.
-spec monad_assoc() -> equation().
monad_assoc() ->
    catena_equations:new(
        catena_equations:seq([
            catena_equations:seq([
                catena_equations:var(m),
                catena_equations:bind(x, catena_equations:var(f))
            ]),
            catena_equations:bind(y, catena_equations:var(g))
        ]),
        catena_equations:seq([
            catena_equations:var(m),
            catena_equations:bind(x,
                catena_equations:seq([
                    catena_equations:op(apply, 2,
                        catena_equations:seq([catena_equations:var(f), catena_equations:var(x)])
                    ),
                    catena_equations:bind(y, catena_equations:var(g))
                ])
            )
        ])
    ).

%%====================================================================
%% Private Functions
%%====================================================================

%% @private Get all laws combined.
-spec all_laws() -> equation_set().
all_laws() ->
    Sets = [state_laws(), reader_laws(), writer_laws(),
            error_laws(), choice_laws(), async_laws(), monadic_laws()],
    lists:foldl(fun combine_law_sets/2, catena_equation_spec:new_set(all), Sets).
