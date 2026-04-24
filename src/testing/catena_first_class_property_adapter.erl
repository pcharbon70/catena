%% @doc Adapter from first-class stdlib `Test.prop` / `Gen` runtime values
%% into `src/proptest` property and generator values.
-module(catena_first_class_property_adapter).

-include("../proptest/catena_property.hrl").

-export([
    from_property_value/1,
    from_property_spec/1,
    from_generator_value/1
]).

-type adapter_error() ::
    missing_property_body |
    {unsupported_property_spec, term()} |
    {unsupported_generator_value, term()} |
    {unsupported_predicate_result, term()} |
    {invalid_property_seed, term()}.

-export_type([adapter_error/0]).

%%====================================================================
%% Public API
%%====================================================================

-spec from_property_value(map()) ->
    {ok, catena_property:property()} | {error, adapter_error()}.
from_property_value(PropertyValue) when is_map(PropertyValue) ->
    Name = maps:get(name, PropertyValue, <<"anonymous property">>),
    case maps:get(body, PropertyValue, undefined) of
        undefined ->
            {error, missing_property_body};
        BodyFun ->
            case from_property_spec(apply_runtime_fun(BodyFun, [unit_value()])) of
                {ok, {Generator, Predicate}} ->
                    Prop0 = catena_property:new(Name, Generator, Predicate),
                    apply_property_config(maps:get(config, PropertyValue, #{}), Prop0);
                {error, _} = Error ->
                    Error
            end
    end.

-spec from_property_spec(term()) ->
    {ok, {catena_gen:generator(term()), fun((term()) -> boolean() | discard)}} |
    {error, adapter_error()}.
from_property_spec({for_all, GeneratorValue, PredicateFun}) ->
    case from_generator_value(GeneratorValue) of
        {ok, Generator} ->
            {ok, {Generator, wrap_predicate(PredicateFun)}};
        {error, _} = Error ->
            Error
    end;
from_property_spec(Other) ->
    {error, {unsupported_property_spec, Other}}.

-spec from_generator_value(term()) ->
    {ok, catena_gen:generator(term())} | {error, adapter_error()}.
from_generator_value(gen_bool) ->
    {ok, catena_gen:gen_bool()};
from_generator_value(gen_int) ->
    {ok, catena_gen:gen_int()};
from_generator_value(gen_natural) ->
    {ok, catena_gen:gen_nat()};
from_generator_value({gen_int_range, Min, Max}) when is_integer(Min), is_integer(Max), Min =< Max ->
    {ok, catena_gen:gen_int_range(Min, Max)};
from_generator_value({gen_constant_bool, Value}) when is_boolean(Value) ->
    {ok, catena_gen:constant(Value)};
from_generator_value({gen_constant_int, Value}) when is_integer(Value) ->
    {ok, catena_gen:constant(Value)};
from_generator_value({gen_constant_text, Value}) when is_list(Value); is_binary(Value) ->
    {ok, catena_gen:constant(Value)};
from_generator_value({gen_element_bools, Values}) when is_list(Values) ->
    {ok, catena_gen:element(Values)};
from_generator_value({gen_element_ints, Values}) when is_list(Values) ->
    {ok, catena_gen:element(Values)};
from_generator_value({gen_element_texts, Values}) when is_list(Values) ->
    {ok, catena_gen:element(Values)};
from_generator_value({gen_one_of, Generators}) when is_list(Generators) ->
    case convert_generators(Generators) of
        {ok, Converted} ->
            {ok, catena_gen:gen_one_of(Converted)};
        {error, _} = Error ->
            Error
    end;
from_generator_value({gen_map, GeneratorValue, MapperFun}) ->
    case from_generator_value(GeneratorValue) of
        {ok, Generator} ->
            {ok, catena_gen:gen_map(wrap_mapper(MapperFun), Generator)};
        {error, _} = Error ->
            Error
    end;
from_generator_value({gen_flat_map, GeneratorValue, BinderFun}) ->
    case from_generator_value(GeneratorValue) of
        {ok, Generator} ->
            {ok, catena_gen:gen_bind(Generator, wrap_binder(BinderFun))};
        {error, _} = Error ->
            Error
    end;
from_generator_value({gen_filter, GeneratorValue, PredicateFun}) ->
    case from_generator_value(GeneratorValue) of
        {ok, Generator} ->
            {ok, catena_gen:gen_filter(wrap_filter(PredicateFun), Generator)};
        {error, _} = Error ->
            Error
    end;
from_generator_value(Other) ->
    {error, {unsupported_generator_value, Other}}.

%%====================================================================
%% Internal Helpers
%%====================================================================

apply_property_config(ConfigValue, Prop0) when is_map(ConfigValue) ->
    case apply_num_tests(ConfigValue, Prop0) of
        {ok, Prop1} ->
            case apply_seed(ConfigValue, Prop1) of
                {ok, Prop2} ->
                    {ok, apply_labels(ConfigValue, Prop2)};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;
apply_property_config(_Other, Prop0) ->
    {ok, Prop0}.

apply_num_tests(ConfigValue, Prop0) ->
    case maps:get(iterations, ConfigValue, undefined) of
        Iterations when is_integer(Iterations), Iterations > 0 ->
            {ok, catena_property:with_config([{num_tests, Iterations}], Prop0)};
        undefined ->
            {ok, Prop0};
        _Other ->
            {ok, Prop0}
    end.

apply_seed(ConfigValue, Prop0) ->
    case maps:get(seed, ConfigValue, undefined) of
        none ->
            {ok, Prop0};
        {some, SeedInt} when is_integer(SeedInt) ->
            {ok, catena_property:with_config(
                [{seed, catena_gen:seed_from_int(SeedInt)}],
                Prop0
            )};
        undefined ->
            {ok, Prop0};
        Other ->
            {error, {invalid_property_seed, Other}}
    end.

apply_labels(ConfigValue, Prop0) ->
    Labels = maps:get(labels, ConfigValue, []),
    lists:foldl(
        fun(Label, Acc) ->
            catena_property:with_label(normalize_label(Label), Acc)
        end,
        Prop0,
        Labels
    ).

normalize_label(Label) when is_binary(Label) ->
    Label;
normalize_label(Label) when is_list(Label) ->
    unicode:characters_to_binary(Label);
normalize_label(Label) ->
    unicode:characters_to_binary(io_lib:format("~p", [Label])).

convert_generators(GeneratorValues) ->
    lists:foldr(
        fun(GeneratorValue, {ok, Acc}) ->
                case from_generator_value(GeneratorValue) of
                    {ok, Generator} -> {ok, [Generator | Acc]};
                    {error, _} = Error -> Error
                end;
           (_GeneratorValue, {error, _} = Error) ->
                Error
        end,
        {ok, []},
        GeneratorValues
    ).

wrap_predicate(PredicateFun) ->
    fun(Value) ->
        normalize_predicate_result(apply_runtime_fun(PredicateFun, [Value]))
    end.

wrap_mapper(MapperFun) ->
    fun(Value) ->
        apply_runtime_fun(MapperFun, [Value])
    end.

wrap_binder(BinderFun) ->
    fun(Value) ->
        case from_generator_value(apply_runtime_fun(BinderFun, [Value])) of
            {ok, Generator} ->
                Generator;
            {error, Reason} ->
                erlang:error({invalid_generator_result, Reason})
        end
    end.

wrap_filter(PredicateFun) ->
    fun(Value) ->
        case normalize_predicate_result(apply_runtime_fun(PredicateFun, [Value])) of
            true -> true;
            false -> false;
            discard -> false;
            Other -> erlang:error({unsupported_predicate_result, Other})
        end
    end.

normalize_predicate_result(true) ->
    true;
normalize_predicate_result(false) ->
    false;
normalize_predicate_result(discard) ->
    discard;
normalize_predicate_result(Other) ->
    erlang:error({unsupported_predicate_result, Other}).

apply_runtime_fun({Fun, _Arity, _Type}, Args) when is_function(Fun) ->
    apply_runtime_fun(Fun, Args);
apply_runtime_fun(Fun, []) when is_function(Fun) ->
    case erlang:fun_info(Fun, arity) of
        {arity, 0} -> Fun();
        _ -> Fun
    end;
apply_runtime_fun(Fun, [Arg | Rest]) when is_function(Fun) ->
    case erlang:fun_info(Fun, arity) of
        {arity, 0} ->
            apply_runtime_fun(Fun(), [Arg | Rest]);
        {arity, 1} ->
            apply_runtime_result(Fun(Arg), Rest);
        {arity, N} ->
            Args = [Arg | Rest],
            case length(Args) >= N of
                true ->
                    {CallArgs, Remaining} = lists:split(N, Args),
                    apply_runtime_result(erlang:apply(Fun, CallArgs), Remaining);
                false ->
                    erlang:error({not_enough_arguments, Args})
            end
    end;
apply_runtime_fun(Other, _Args) ->
    erlang:error({not_a_function, Other}).

apply_runtime_result(Result, []) ->
    Result;
apply_runtime_result(Result, RemainingArgs) ->
    apply_runtime_fun(Result, RemainingArgs).

unit_value() ->
    {unit}.
