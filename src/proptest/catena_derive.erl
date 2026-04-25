%% @doc Generator derivation helpers for Phase 7.1.
%%
%% Catena does not yet have full type reflection or derive macros, so this
%% module implements the planned workaround: explicit type descriptors and a
%% registry for custom generators.
-module(catena_derive).

-export([
    derive_generator/1,
    derive_generator/2,
    derive_generator_for/1,
    record_gen/1,
    record_gen/2,
    variant_gen/1,
    variant_gen/2,
    recursive_gen/3,
    register/2,
    registered/1,
    clear_registry/0
]).

-define(REGISTRY_TABLE, catena_derive_registry).
-define(DEFAULT_TEXT_RANGE, {0, 16}).
-define(DEFAULT_LIST_RANGE, {0, 8}).
-define(DEFAULT_MAP_RANGE, {0, 8}).
-define(DEFAULT_RECURSIVE_DEPTH, 4).

%%====================================================================
%% API
%%====================================================================

-spec derive_generator(map()) -> catena_gen:generator(term()).
derive_generator(Spec) ->
    derive_generator(Spec, #{}).

-spec derive_generator(map(), map()) -> catena_gen:generator(term()).
derive_generator(Spec, Options) when is_map(Spec), is_map(Options) ->
    Depth = maps:get(max_depth, Options, ?DEFAULT_RECURSIVE_DEPTH),
    derive(Spec, Options#{max_depth => Depth}, Depth).

-spec derive_generator_for(atom()) -> catena_gen:generator(term()).
derive_generator_for(Name) when is_atom(Name) ->
    case registered(Name) of
        {ok, Generator} -> Generator;
        {error, _} = Error -> erlang:error(Error)
    end.

-spec record_gen([{atom(), catena_gen:generator(term())}]) -> catena_gen:generator(map()).
record_gen(Fields) ->
    record_gen(Fields, #{}).

-spec record_gen([{atom(), catena_gen:generator(term())}], map()) -> catena_gen:generator(term()).
record_gen(Fields, Options) when is_list(Fields), is_map(Options) ->
    Construct = maps:get(construct, Options, fun(Map) -> Map end),
    FieldNames = [Name || {Name, _Generator} <- Fields],
    catena_gen:gen_map(
        fun(Values) ->
            FieldMap = maps:from_list(lists:zip(FieldNames, Values)),
            Construct(FieldMap)
        end,
        tupled_generators([Generator || {_Name, Generator} <- Fields])
    ).

-spec variant_gen([map()]) -> catena_gen:generator(term()).
variant_gen(Constructors) ->
    variant_gen(Constructors, #{}).

-spec variant_gen([map()], map()) -> catena_gen:generator(term()).
variant_gen(Constructors, Options) when is_list(Constructors), is_map(Options) ->
    Weighted = lists:map(
        fun(Constructor) ->
            Weight = maps:get(weight, Constructor, 1),
            {Weight, constructor_generator(Constructor, Options)}
        end,
        Constructors
    ),
    catena_gen:gen_frequency(Weighted).

-spec recursive_gen(catena_gen:generator(term()), catena_gen:generator(term()), pos_integer()) ->
    catena_gen:generator(term()).
recursive_gen(BaseGen, StepGen, MaxDepth) when is_integer(MaxDepth), MaxDepth > 0 ->
    catena_gen:sized(fun(Size) ->
        EffectiveDepth = min(MaxDepth, max(Size, 1)),
        recursive_gen_with_depth(BaseGen, StepGen, EffectiveDepth)
    end).

-spec register(atom(), catena_gen:generator(term())) -> ok.
register(Name, Generator) when is_atom(Name) ->
    ensure_registry(),
    ets:insert(?REGISTRY_TABLE, {Name, Generator}),
    ok.

-spec registered(atom()) -> {ok, catena_gen:generator(term())} | {error, term()}.
registered(Name) when is_atom(Name) ->
    ensure_registry(),
    case ets:lookup(?REGISTRY_TABLE, Name) of
        [{Name, Generator}] -> {ok, Generator};
        [] -> {error, {unknown_generator, Name}}
    end.

-spec clear_registry() -> ok.
clear_registry() ->
    ensure_registry(),
    ets:delete_all_objects(?REGISTRY_TABLE),
    ok.

%%====================================================================
%% Internal derivation
%%====================================================================

derive(#{kind := record, fields := Fields} = Spec, Options, Depth) ->
    FieldGenerators = [
        {maps:get(name, Field), derive_type(maps:get(type, Field), Options, Depth - 1, field_options(Field, Options))}
        || Field <- Fields
    ],
    record_gen(FieldGenerators, maybe_construct(Spec));
derive(#{kind := variant, constructors := Constructors}, Options, Depth) ->
    DerivedConstructors = [
        Constructor#{
            args => [
                derive_type(ArgType, Options, Depth - 1, Options)
                || ArgType <- maps:get(args, Constructor, [])
            ]
        }
        || Constructor <- Constructors
    ],
    variant_gen(DerivedConstructors, Options);
derive(#{kind := recursive, base := BaseConstructors, recursive := RecursiveConstructors}, Options, Depth) ->
    MaxDepth = maps:get(max_depth, Options, ?DEFAULT_RECURSIVE_DEPTH),
    derive_recursive(
        #{kind => recursive, base => BaseConstructors, recursive => RecursiveConstructors},
        Options,
        max(MaxDepth, Depth)
    );
derive(#{kind := alias, type := Type}, Options, Depth) ->
    derive_type(Type, Options, Depth, Options);
derive(Spec, _Options, _Depth) ->
    erlang:error({invalid_derivation_spec, Spec}).

derive_type(_Type, _Options, Depth, _LocalOptions) when Depth < 0 ->
    catena_gen:constant(undefined);
derive_type(bool, _Options, _Depth, _LocalOptions) ->
    catena_gen:gen_bool();
derive_type(integer, _Options, _Depth, _LocalOptions) ->
    catena_gen:gen_int();
derive_type(pos_int, _Options, _Depth, _LocalOptions) ->
    catena_gen:gen_pos_int();
derive_type(nat, _Options, _Depth, _LocalOptions) ->
    catena_gen:gen_nat();
derive_type(atom, _Options, _Depth, LocalOptions) ->
    Atoms = maps:get(values, LocalOptions, [alpha, beta, gamma, delta, epsilon]),
    catena_gen:elements(Atoms);
derive_type(binary, _Options, _Depth, LocalOptions) ->
    Range = range_from(LocalOptions, ?DEFAULT_TEXT_RANGE),
    catena_stdgen:gen_binary(catena_range:range_constant(Range));
derive_type(string, _Options, _Depth, LocalOptions) ->
    Range = range_from(LocalOptions, ?DEFAULT_TEXT_RANGE),
    catena_stdgen:gen_string(catena_range:range_constant(Range));
derive_type({constant, Value}, _Options, _Depth, _LocalOptions) ->
    catena_gen:constant(Value);
derive_type({one_of, Values}, _Options, _Depth, _LocalOptions) ->
    catena_gen:elements(Values);
derive_type({registered, Name}, _Options, _Depth, _LocalOptions) ->
    derive_generator_for(Name);
derive_type({list, Type}, Options, Depth, LocalOptions) ->
    Range = range_from(LocalOptions, ?DEFAULT_LIST_RANGE),
    catena_stdgen:gen_list_of(catena_range:range_constant(Range),
        derive_type(Type, Options, Depth - 1, LocalOptions));
derive_type({'maybe', Type}, Options, Depth, LocalOptions) ->
    catena_stdgen:gen_maybe(derive_type(Type, Options, Depth - 1, LocalOptions));
derive_type({result, OkType, ErrorType}, Options, Depth, LocalOptions) ->
    catena_stdgen:gen_result(
        derive_type(OkType, Options, Depth - 1, LocalOptions),
        derive_type(ErrorType, Options, Depth - 1, LocalOptions)
    );
derive_type({tuple, Types}, Options, Depth, LocalOptions) ->
    tupled_generators([derive_type(Type, Options, Depth - 1, LocalOptions) || Type <- Types]);
derive_type({map, KeyType, ValueType}, Options, Depth, LocalOptions) ->
    Range = range_from(LocalOptions, ?DEFAULT_MAP_RANGE),
    catena_stdgen:gen_map_of(
        catena_range:range_constant(Range),
        derive_type(KeyType, Options, Depth - 1, LocalOptions),
        derive_type(ValueType, Options, Depth - 1, LocalOptions)
    );
derive_type({recursive_ref, Name}, _Options, Depth, _LocalOptions) ->
    case Depth =< 0 of
        true -> catena_gen:constant({recursive_ref, Name, max_depth});
        false -> derive_generator_for(Name)
    end;
derive_type(self, _Options, Depth, _LocalOptions) when Depth =< 0 ->
    catena_gen:constant(max_depth);
derive_type(#{kind := _} = NestedSpec, Options, Depth, _LocalOptions) ->
    derive(NestedSpec, Options, Depth);
derive_type(Type, _Options, _Depth, _LocalOptions) ->
    erlang:error({unsupported_type_descriptor, Type}).

field_options(Field, Options) ->
    FieldOptions = maps:get(options, Field, #{}),
    maps:merge(Options, FieldOptions).

maybe_construct(Spec) ->
    case maps:get(construct, Spec, undefined) of
        undefined -> #{};
        Construct -> #{construct => Construct}
    end.

constructor_generator(Constructor, _Options) ->
    Tag = maps:get(tag, Constructor),
    Args = maps:get(args, Constructor, []),
    GeneratorArgs = case Args of
        [] -> catena_gen:constant([]);
        _ -> tupled_generators(Args)
    end,
    catena_gen:gen_map(
        fun(Values) ->
            build_constructor(Tag, Values, maps:get(construct, Constructor, undefined))
        end,
        GeneratorArgs
    ).

build_constructor(Tag, [], undefined) ->
    Tag;
build_constructor(Tag, Values, undefined) when is_list(Values) ->
    list_to_tuple([Tag | Values]);
build_constructor(_Tag, Values, Construct) when is_function(Construct, 1) ->
    Construct(Values).

tupled_generators([]) ->
    catena_gen:constant([]);
tupled_generators([Generator]) ->
    catena_gen:gen_map(fun(Value) -> [Value] end, Generator);
tupled_generators([Generator | Rest]) ->
    catena_gen:gen_map2(
        fun(Value, Values) -> [Value | Values] end,
        Generator,
        tupled_generators(Rest)
    ).

recursive_gen_with_depth(BaseGen, _StepGen, 0) ->
    BaseGen;
recursive_gen_with_depth(BaseGen, StepGen, Depth) when Depth > 0 ->
    catena_gen:gen_frequency([
        {Depth + 1, BaseGen},
        {1, StepGen}
    ]).

derive_recursive(Spec, Options, Depth) when Depth =< 0 ->
    derive(#{kind => variant, constructors => maps:get(base, Spec)}, Options, 0);
derive_recursive(#{base := BaseConstructors, recursive := RecursiveConstructors} = Spec, Options, Depth) ->
    BaseGen = derive_recursive_variant(BaseConstructors, Spec, Options, Depth - 1),
    StepGen = derive_recursive_variant(RecursiveConstructors, Spec, Options, Depth - 1),
    recursive_gen_with_depth(BaseGen, StepGen, Depth).

derive_recursive_variant(Constructors, Spec, Options, Depth) ->
    DerivedConstructors = [
        Constructor#{
            args => [
                derive_recursive_arg(ArgType, Spec, Options, Depth)
                || ArgType <- maps:get(args, Constructor, [])
            ]
        }
        || Constructor <- Constructors
    ],
    variant_gen(DerivedConstructors, Options).

derive_recursive_arg(self, Spec, Options, Depth) ->
    derive_recursive(Spec, Options, Depth);
derive_recursive_arg({recursive_ref, _Name}, Spec, Options, Depth) ->
    derive_recursive(Spec, Options, Depth);
derive_recursive_arg(Type, _Spec, Options, Depth) ->
    derive_type(Type, Options, Depth, Options).

range_from(Options, Default) ->
    maps:get(range, Options, Default).

ensure_registry() ->
    case ets:info(?REGISTRY_TABLE) of
        undefined -> ets:new(?REGISTRY_TABLE, [set, public, named_table]);
        _ -> ok
    end.
