%%%-------------------------------------------------------------------
%%% @doc Phase 6.1 selective-CPS Core Erlang lowering tests.
%%%-------------------------------------------------------------------
-module(catena_control_codegen_tests).

-include_lib("eunit/include/eunit.hrl").

validated_ir_emits_source_and_private_entry_arities_test() ->
    {ok, Unit} = catena_compile:compile_string_to_unit(mixed_source()),
    {ok, Core} = catena_control_codegen:generate(Unit),
    ?assertEqual(
        [
            {'$catena_cps$run', 3},
            {'$catena_direct$identity', 2},
            {identity, 1},
            {run, 1}
        ],
        lists:sort(export_identities(Core))
    ),
    Definitions = definition_identities(Core),
    ?assert(lists:member({identity, 1, 1}, Definitions)),
    ?assert(lists:member({'$catena_direct$identity', 2, 2}, Definitions)),
    ?assert(lists:member({run, 1, 1}, Definitions)),
    ?assert(lists:member({'$catena_cps$run', 3, 3}, Definitions)),
    ?assertEqual(
        1,
        module_attribute(catena_control_abi_version, Core)
    ).

public_contract_hides_beam_linkage_entries_test() ->
    with_artifact(mixed_source(), fun(Artifact) ->
        Beam = maps:get(beam, Artifact),
        {ok, {_, [{exports, Exports}]}} =
            beam_lib:chunks(Beam, [exports]),
        ?assertEqual(
            [
                {'$catena_cps$run', 3},
                {'$catena_direct$identity', 2},
                {identity, 1},
                {run, 1}
            ],
            lists:sort(Exports)
        ),
        Interface = maps:get(interface, Artifact),
        ?assertEqual(
            [{identity, 1}, {run, 1}],
            lists:sort([
                {maps:get(name, Entry), maps:get(arity, Entry)}
                || Entry <- maps:get(exports, Interface)
            ])
        )
    end).

explicit_resume_executes_captured_remainder_test() ->
    with_loaded(explicit_resume_source(), fun(Module) ->
        ?assertEqual(41, Module:run(ignored))
    end).

handler_result_flows_through_transformed_delimiter_test() ->
    with_loaded(transformed_result_source(), fun(Module) ->
        %% The resumed continuation doubles 21 before its delimiter result is
        %% returned to the handler, whose remaining expression adds one.
        ?assertEqual(43, Module:run())
    end).

public_wrapper_establishes_one_initial_runtime_context_test() ->
    {ok, Core} = catena_compile:compile_string_to_core(mixed_source()),
    RunWrapper = definition(run, 1, Core),
    ?assertEqual(1, count_runtime_calls(
        catena_effect_runtime,
        empty_context,
        RunWrapper
    )),
    ?assertEqual(1, count_runtime_calls(
        catena_effect_runtime,
        empty_context,
        definition(identity, 1, Core)
    )),
    ?assertEqual(0, count_runtime_calls(
        catena_effect_runtime,
        empty_context,
        definition('$catena_cps$run', 3, Core)
    )).

private_name_and_control_abi_are_stable_test() ->
    ?assertEqual(1, catena_control_codegen:control_abi_version()),
    ?assertEqual(
        '$catena_direct$map',
        catena_control_codegen:private_name(direct, map)
    ),
    ?assertEqual(
        '$catena_cps$map',
        catena_control_codegen:private_name(cps, map)
    ).

mixed_source() ->
    "module PhaseSixCoreBoundary\n"
    "export transform identity\n"
    "export transform run\n"
    "effect Choice\n"
    "operation choose : Int\n"
    "end\n"
    "transform identity value = value\n"
    "transform run ignored = handle "
        "(let chosen = perform Choice.choose() in identity chosen) then {\n"
    "  Choice { choose() with k -> resume(k, 41) }\n"
    "}\n".

explicit_resume_source() ->
    "module PhaseSixExplicitResume\n"
    "export transform run\n"
    "effect Choice\n"
    "operation choose : Int\n"
    "end\n"
    "transform identity value = value\n"
    "transform run ignored = handle "
        "(let chosen = perform Choice.choose() in identity chosen) then {\n"
    "  Choice { choose() with k -> resume(k, 41) }\n"
    "}\n".

transformed_result_source() ->
    "module PhaseSixTransformedResult\n"
    "export transform run\n"
    "effect Choice\n"
    "operation choose : Int\n"
    "end\n"
    "transform run = handle "
        "(let chosen = perform Choice.choose() in chosen * 2) then {\n"
    "  Choice { choose() with k -> "
        "let result = resume(k, 21) in result + 1 }\n"
    "}\n".

with_artifact(Source, Assertion) ->
    {ok, Artifact} = catena_compile:compile_string_to_beam(Source),
    Assertion(Artifact).

with_loaded(Source, Assertion) ->
    with_artifact(Source, fun(Artifact) ->
        Module = maps:get(runtime_module, Artifact),
        unload(Module),
        try
            {module, Module} = code:load_binary(
                Module,
                "phase-six-core-codegen-memory",
                maps:get(beam, Artifact)
            ),
            Assertion(Module)
        after
            unload(Module)
        end
    end).

unload(Module) ->
    code:purge(Module),
    code:delete(Module).

export_identities(Core) ->
    [
        {cerl:fname_id(Export), cerl:fname_arity(Export)}
        || Export <- cerl:module_exports(Core)
    ].

definition_identities(Core) ->
    [
        {
            cerl:fname_id(Name),
            cerl:fname_arity(Name),
            cerl:fun_arity(Function)
        }
        || {Name, Function} <- cerl:module_defs(Core)
    ].

definition(Name, Arity, Core) ->
    {_, Function} = lists:keyfind(
        {Name, Arity},
        1,
        [
            {{cerl:fname_id(FName), cerl:fname_arity(FName)}, Definition}
            || {FName, Definition} <- cerl:module_defs(Core)
        ]
    ),
    Function.

module_attribute(Name, Core) ->
    {_, Value} = lists:keyfind(
        Name,
        1,
        [
            {cerl:concrete(Key), cerl:concrete(AttributeValue)}
            || {Key, AttributeValue} <- cerl:module_attrs(Core)
        ]
    ),
    Value.

count_runtime_calls(Module, Function, Tree) ->
    cerl_trees:fold(
        fun(Node, Count) ->
            case cerl:type(Node) of
                call ->
                    case {
                        cerl:is_literal(cerl:call_module(Node)),
                        cerl:is_literal(cerl:call_name(Node))
                    } of
                        {true, true} ->
                            case {
                                cerl:concrete(cerl:call_module(Node)),
                                cerl:concrete(cerl:call_name(Node))
                            } of
                                {Module, Function} -> Count + 1;
                                _ -> Count
                            end;
                        _ -> Count
                    end;
                _ -> Count
            end
        end,
        0,
        Tree
    ).
