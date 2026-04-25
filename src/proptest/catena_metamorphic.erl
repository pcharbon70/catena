%% @doc Metamorphic testing helpers for Phase 7.3.
%%
%% Metamorphic properties generate a source input, derive a transformed input,
%% execute the system under test on both values, and check a declared output
%% relation. Failure counterexamples retain the original input, transformed
%% input, and both outputs so existing reporting surfaces stay informative.
-module(catena_metamorphic).

-export([
    metamorphic/3,
    relation/3,
    with_weight/2,
    identity/1,
    permutation_invariant/0,
    permutation_invariant/1,
    scaling/2,
    composition/2,
    format_failure/1
]).

-type relation_name() :: binary() | string() | atom().
-type relation_spec() :: #{
    name := binary(),
    transform := fun((term()) -> term()),
    relation := fun(),
    weight => pos_integer()
}.
-type metamorphic_spec() :: #{
    subject := fun((term()) -> term()),
    relations := [relation_spec()]
}.

-spec metamorphic(binary() | string(), catena_gen:generator(term()), metamorphic_spec()) ->
    catena_property:property().
metamorphic(Name, Generator, #{subject := Subject, relations := Relations} = _Spec)
        when is_function(Subject, 1), is_list(Relations) ->
    NormalizedRelations = normalize_relations(Relations),
    RelationLookup = maps:from_list(
        [{maps:get(name, Relation), Relation} || Relation <- NormalizedRelations]
    ),
    CaseGenerator = metamorphic_case_generator(Generator, Subject, NormalizedRelations),
    catena_property:new(
        Name,
        CaseGenerator,
        fun(Case) ->
            Relation = maps:get(maps:get(relation, Case), RelationLookup),
            evaluate_relation(Relation, Case)
        end
    ).

-spec relation(relation_name(), fun((term()) -> term()), fun()) -> relation_spec().
relation(Name, TransformFun, RelationFun)
        when is_function(TransformFun, 1), is_function(RelationFun) ->
    #{
        name => normalize_name(Name),
        transform => TransformFun,
        relation => RelationFun,
        weight => 1
    }.

-spec with_weight(relation_spec(), pos_integer()) -> relation_spec().
with_weight(Relation, Weight) when is_map(Relation), is_integer(Weight), Weight > 0 ->
    Relation#{weight => Weight}.

-spec identity(fun((term()) -> term())) -> relation_spec().
identity(TransformFun) ->
    relation(
        identity,
        TransformFun,
        fun(Output, TransformedOutput) ->
            Output =:= TransformedOutput
        end
    ).

-spec permutation_invariant() -> relation_spec().
permutation_invariant() ->
    permutation_invariant(fun(List) -> lists:reverse(List) end).

-spec permutation_invariant(fun((term()) -> term())) -> relation_spec().
permutation_invariant(TransformFun) ->
    relation(
        permutation_invariance,
        TransformFun,
        fun(Output, TransformedOutput) ->
            Output =:= TransformedOutput
        end
    ).

-spec scaling(fun((term()) -> term()), fun((term()) -> term())) -> relation_spec().
scaling(InputScaleFun, OutputScaleFun)
        when is_function(InputScaleFun, 1), is_function(OutputScaleFun, 1) ->
    relation(
        scaling,
        InputScaleFun,
        fun(Output, TransformedOutput) ->
            OutputScaleFun(Output) =:= TransformedOutput
        end
    ).

-spec composition(fun((term()) -> term()), fun((term()) -> term())) -> relation_spec().
composition(InputTransformFun, OutputTransformFun)
        when is_function(InputTransformFun, 1), is_function(OutputTransformFun, 1) ->
    relation(
        composition,
        InputTransformFun,
        fun(Output, TransformedOutput) ->
            OutputTransformFun(Output) =:= TransformedOutput
        end
    ).

-spec format_failure(map()) -> binary().
format_failure(Case) when is_map(Case) ->
    Relation = maps:get(relation, Case, <<"unknown">>),
    iolist_to_binary([
        "Relation: ", Relation, "\n",
        "Input: ", io_lib:format("~p", [maps:get(input, Case, undefined)]), "\n",
        "Transformed Input: ", io_lib:format("~p", [maps:get(transformed_input, Case, undefined)]), "\n",
        "Output: ", io_lib:format("~p", [maps:get(output, Case, undefined)]), "\n",
        "Transformed Output: ", io_lib:format("~p", [maps:get(transformed_output, Case, undefined)])
    ]).

metamorphic_case_generator(Generator, Subject, Relations) ->
    catena_gen:gen_bind(
        Generator,
        fun(Input) ->
            catena_gen:gen_frequency([
                {maps:get(weight, Relation, 1), build_case_generator(Input, Subject, Relation)}
                || Relation <- Relations
            ])
        end
    ).

build_case_generator(Input, Subject, Relation) ->
    catena_gen:constant(build_case(Input, Subject, Relation)).

build_case(Input, Subject, Relation) ->
    Transform = maps:get(transform, Relation),
    TransformedInput = Transform(Input),
    Output = Subject(Input),
    TransformedOutput = Subject(TransformedInput),
    #{
        relation => maps:get(name, Relation),
        input => Input,
        transformed_input => TransformedInput,
        output => Output,
        transformed_output => TransformedOutput
    }.

evaluate_relation(Relation, Case) ->
    RelationFun = maps:get(relation, Relation),
    Input = maps:get(input, Case),
    TransformedInput = maps:get(transformed_input, Case),
    Output = maps:get(output, Case),
    TransformedOutput = maps:get(transformed_output, Case),
    apply_relation(RelationFun, Input, TransformedInput, Output, TransformedOutput).

apply_relation(RelationFun, _Input, _TransformedInput, Output, TransformedOutput) ->
    {arity, Arity} = erlang:fun_info(RelationFun, arity),
    case Arity of
        2 ->
            RelationFun(Output, TransformedOutput);
        3 ->
            RelationFun(_Input, Output, TransformedOutput);
        4 ->
            RelationFun(_Input, _TransformedInput, Output, TransformedOutput);
        _ ->
            erlang:error({unsupported_relation_arity, Arity})
    end.

normalize_relations(Relations) ->
    [normalize_relation(Relation) || Relation <- Relations].

normalize_relation(#{name := _, transform := _, relation := _} = Relation) ->
    Relation#{
        name => normalize_name(maps:get(name, Relation)),
        weight => maps:get(weight, Relation, 1)
    };
normalize_relation(Relation) ->
    erlang:error({invalid_relation, Relation}).

normalize_name(Name) when is_binary(Name) ->
    Name;
normalize_name(Name) when is_atom(Name) ->
    atom_to_binary(Name);
normalize_name(Name) when is_list(Name) ->
    unicode:characters_to_binary(Name).
