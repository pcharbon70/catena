-module(catena_control_ir_tests).

-include_lib("eunit/include/eunit.hrl").

node_contract_requires_complete_metadata_test() ->
    ?assertMatch(
        {error, {invalid_control_ir_node, return, _}},
        catena_control_ir:node(return, #{}, #{})
    ),
    Metadata = #{
        value_type => {tcon, int},
        effect_row => {teffectrow, [], closed},
        control_mode => direct,
        delimiter => none,
        continuation_arity => 0,
        runtime_disposition => direct,
        origin => {location, 1, 1}
    },
    ?assertMatch(
        {ok, #{
            '$catena_control_node' := 1,
            op := return,
            metadata := Metadata
        }},
        catena_control_ir:node(return, Metadata, #{value => 1})
    ).

direct_transform_retains_direct_entry_shape_test() ->
    {ok, Unit} = catena_compile:compile_string_to_unit(
        "module DirectIR\n"
        "transform identity x = x\n"
    ),
    IR = catena_compilation_unit:control_ir(Unit),
    ?assert(catena_control_ir:is_ir(IR)),
    {ok, Transform} = catena_control_ir:lookup(identity, IR),
    ?assertEqual(direct, maps:get(control_mode, Transform)),
    ?assertMatch(
        #{
            public := {identity, 1},
            private := {direct, identity, 2},
            source_arity := 1,
            context_arity := 1,
            continuation_arity := 0,
            control_mode := direct
        },
        maps:get(entry, Transform)
    ).

explicit_handler_builds_delimited_cps_nodes_test() ->
    Source =
        "module ExplicitControlIR\n"
        "effect Choice\n"
        "operation choose : Int\n"
        "end\n"
        "transform run = handle "
        "(let selected = perform Choice.choose() in selected + 1) then {\n"
        "  Choice { choose() with k -> resume(k, 4) }\n"
        "}\n",
    {ok, Unit} = catena_compile:compile_string_to_unit(Source),
    IR = catena_compilation_unit:control_ir(Unit),
    Ops = operations(IR),
    lists:foreach(
        fun(Operation) ->
            ?assert(lists:member(Operation, Ops))
        end,
        [
            delimiter,
            install_handler,
            bind,
            perform,
            make_resumption,
            resume
        ]
    ),
    {ok, Transform} = catena_control_ir:lookup(run, IR),
    ?assertEqual(resumable, maps:get(control_mode, Transform)),
    ?assertEqual(
        {cps, run, 2},
        maps:get(private, maps:get(entry, Transform))
    ),
    Delimiters = [
        maps:get(identity, maps:get(fields, Node))
        || Node <- catena_control_ir:nodes(IR),
           maps:get(op, Node) =:= delimiter
    ],
    ?assertEqual([{delimiter, run, 1}], Delimiters),
    lists:foreach(
        fun(Node) ->
            Metadata = maps:get(metadata, Node),
            ?assert(maps:is_key(value_type, Metadata)),
            ?assert(maps:is_key(effect_row, Metadata)),
            ?assert(maps:is_key(origin, Metadata)),
            ?assert(maps:is_key(runtime_disposition, Metadata))
        end,
        catena_control_ir:nodes(IR)
    ).

handler_return_without_resume_becomes_abort_test() ->
    Source =
        "module AbortControlIR\n"
        "effect Choice\n"
        "operation choose : Int\n"
        "end\n"
        "transform run = handle perform Choice.choose() then {\n"
        "  Choice { choose() with k -> 0 }\n"
        "}\n",
    {ok, Unit} = catena_compile:compile_string_to_unit(Source),
    IR = catena_compilation_unit:control_ir(Unit),
    ?assert(lists:member(abort, operations(IR))),
    ?assertNot(lists:member(resume, operations(IR))).

patterns_guards_and_fallthrough_are_preserved_test() ->
    Source =
        "module PatternControlIR\n"
        "transform classify value = match value of\n"
        "  | x when x > 0 -> x\n"
        "  | _ -> 0\n"
        "end\n",
    {ok, Unit} = catena_compile:compile_string_to_unit(Source),
    IR = catena_compilation_unit:control_ir(Unit),
    [Match] = [
        Node
        || Node <- catena_control_ir:nodes(IR),
           maps:get(op, Node) =:= match
    ],
    Fields = maps:get(fields, Match),
    [First, Second] = maps:get(clauses, Fields),
    ?assertMatch({pat_var, x, _}, maps:get(pattern, First)),
    ?assert(maps:get(guards, First) =/= []),
    ?assertEqual(next_clause, maps:get(failure, First)),
    ?assertEqual(next_clause, maps:get(failure, Second)),
    ?assertEqual(match_failure, maps:get(fallthrough, Fields)).

operations(IR) ->
    [maps:get(op, Node) || Node <- catena_control_ir:nodes(IR)].
