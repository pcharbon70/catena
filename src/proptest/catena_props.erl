%% @doc Type-directed property combinators for Phase 7.4.
%%
%% Catena does not yet expose general type reflection, so this module
%% implements the planned workaround: explicit property combinators plus
%% lightweight metadata recognition helpers.
-module(catena_props).

-export([
    recognize_patterns/1,
    roundtrip/4,
    roundtrip/5,
    idempotent/3,
    commutative/3,
    associative/3
]).

-type pattern() :: roundtrip | idempotent | commutative | associative.
-type roundtrip_options() :: #{
    eq => fun((term(), term()) -> boolean()),
    unwrap_result => boolean()
}.

-spec recognize_patterns(map() | [map()]) -> [pattern()].
recognize_patterns(Spec) when is_map(Spec) ->
    dedupe_patterns(
        roundtrip_patterns(Spec) ++
        idempotent_patterns(Spec) ++
        commutative_patterns(Spec) ++
        associative_patterns(Spec)
    );
recognize_patterns(Specs) when is_list(Specs) ->
    dedupe_patterns(lists:flatmap(fun recognize_patterns/1, Specs)).

-spec roundtrip(binary() | string(), fun((term()) -> term()), fun((term()) -> term()), catena_gen:generator(term())) ->
    catena_property:property().
roundtrip(Name, EncodeFun, DecodeFun, Generator) ->
    roundtrip(Name, EncodeFun, DecodeFun, Generator, #{}).

-spec roundtrip(
    binary() | string(),
    fun((term()) -> term()),
    fun((term()) -> term()),
    catena_gen:generator(term()),
    roundtrip_options()
) -> catena_property:property().
roundtrip(Name, EncodeFun, DecodeFun, Generator, Options)
        when is_function(EncodeFun, 1), is_function(DecodeFun, 1), is_map(Options) ->
    Eq = maps:get(eq, Options, fun(A, B) -> A =:= B end),
    Unwrap = maps:get(unwrap_result, Options, true),
    catena_property:new(
        Name,
        Generator,
        fun(Value) ->
            Encoded = EncodeFun(Value),
            Decoded = maybe_unwrap_result(DecodeFun(Encoded), Unwrap),
            case Decoded of
                {error, _Reason} ->
                    false;
                DecodedValue ->
                    Eq(Value, DecodedValue)
            end
        end
    ).

-spec idempotent(binary() | string(), fun((term()) -> term()), catena_gen:generator(term())) ->
    catena_property:property().
idempotent(Name, Fun, Generator) when is_function(Fun, 1) ->
    catena_property:new(
        Name,
        Generator,
        fun(Value) ->
            Fun(Fun(Value)) =:= Fun(Value)
        end
    ).

-spec commutative(binary() | string(), fun((term(), term()) -> term()), catena_gen:generator(term())) ->
    catena_property:property().
commutative(Name, Fun, Generator) when is_function(Fun, 2) ->
    catena_property:new(
        Name,
        catena_stdgen:gen_tuple2(Generator, Generator),
        fun({A, B}) ->
            Fun(A, B) =:= Fun(B, A)
        end
    ).

-spec associative(binary() | string(), fun((term(), term()) -> term()), catena_gen:generator(term())) ->
    catena_property:property().
associative(Name, Fun, Generator) when is_function(Fun, 2) ->
    catena_property:new(
        Name,
        catena_stdgen:gen_tuple3(Generator, Generator, Generator),
        fun({A, B, C}) ->
            Fun(Fun(A, B), C) =:= Fun(A, Fun(B, C))
        end
    ).

roundtrip_patterns(#{inverse := _}) ->
    [roundtrip];
roundtrip_patterns(_) ->
    [].

idempotent_patterns(#{arity := 1, input_type := Type, output_type := Type}) ->
    [idempotent];
idempotent_patterns(_) ->
    [].

commutative_patterns(#{arity := 2, commutative := true}) ->
    [commutative];
commutative_patterns(_) ->
    [].

associative_patterns(#{arity := 2, associative := true}) ->
    [associative];
associative_patterns(_) ->
    [].

maybe_unwrap_result({ok, Value}, true) ->
    Value;
maybe_unwrap_result({error, _Reason} = Error, true) ->
    Error;
maybe_unwrap_result(Value, _Unwrap) ->
    Value.

dedupe_patterns(Patterns) ->
    ordsets:from_list(Patterns).
