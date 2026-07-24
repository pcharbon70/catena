%%%-------------------------------------------------------------------
%%% @doc Runtime selection and invocation of validated Catena dictionaries.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_trait_runtime).

-export([invoke/3, select_dictionary/2, matches/2]).

-spec invoke([map()], atom(), [term()]) -> term().
invoke(Candidates, Method, Arguments) ->
    Dictionary = select_dictionary(Candidates, Arguments),
    Module = maps:get(runtime_module, Dictionary),
    Function = maps:get(dictionary_function, Dictionary),
    Trait = maps:get(trait, Dictionary),
    Head = maps:get(head, Dictionary),
    Methods = erlang:apply(Module, Function, [Trait, Head]),
    MethodFunction = maps:get(Method, Methods),
    erlang:apply(MethodFunction, Arguments).

-spec select_dictionary([map()], [term()]) -> map().
select_dictionary([Only], _Arguments) ->
    Only;
select_dictionary(Candidates, Arguments) ->
    Matches = [
        Candidate
        || Candidate <- Candidates,
           lists:any(
               fun(Argument) ->
                   matches(maps:get(match, Candidate), Argument)
               end,
               Arguments
           )
    ],
    case Matches of
        [Dictionary] ->
            Dictionary;
        [] ->
            error({catena_trait_instance_not_found, Candidates, Arguments});
        _ ->
            error({catena_trait_instance_ambiguous, Matches, Arguments})
    end.

-spec matches(term(), term()) -> boolean().
matches({builtin, integer}, Value) -> is_integer(Value);
matches({builtin, float}, Value) -> is_float(Value);
matches({builtin, boolean}, Value) -> is_boolean(Value);
matches({builtin, string}, Value) -> is_binary(Value) orelse is_list(Value);
matches({builtin, list}, Value) -> is_list(Value);
matches({builtin, function}, Value) -> is_function(Value);
matches({constructors, Constructors}, Value)
  when is_tuple(Value), tuple_size(Value) >= 1 ->
    lists:member(element(1, Value), Constructors);
matches({opaque, _}, _Value) ->
    false;
matches(_, _) ->
    false.
