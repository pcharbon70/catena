%%%-------------------------------------------------------------------
%%% @doc Phase 4 source-to-validated-control-IR integration contract.
%%%
%%% Phase 4 makes control flow explicit and validates its ABI, but does not
%%% yet provide the Phase 5 runtime or Phase 6 Core lowering for explicit
%%% resumptions. The production backend must therefore remain fail-closed.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_delimited_resumption_phase4_tests).

-include_lib("eunit/include/eunit.hrl").

mixed_source_classifies_and_lowers_deterministically_test() ->
    Source = mixed_source(),
    {ok, First} = catena_compile:compile_string_to_unit(Source),
    {ok, Second} = catena_compile:compile_string_to_unit(Source),
    Modes = catena_compilation_unit:control_modes(First),
    ?assertEqual({ok, direct}, catena_control_mode:mode(identity, Modes)),
    ?assertEqual({ok, direct}, catena_control_mode:mode(provider, Modes)),
    ?assertEqual({ok, resumable}, catena_control_mode:mode(run, Modes)),
    ?assertEqual({ok, resumable}, catena_control_mode:mode(caller, Modes)),
    {ok, Caller} = catena_control_mode:lookup(caller, Modes),
    ?assertEqual({calls_resumable, run}, maps:get(reason, Caller)),
    IR = catena_compilation_unit:control_ir(First),
    ?assertEqual(IR, catena_compilation_unit:control_ir(Second)),
    Operations = operations(IR),
    lists:foreach(
        fun(Operation) -> ?assert(lists:member(Operation, Operations)) end,
        [
            delimiter,
            install_handler,
            perform,
            make_resumption,
            resume,
            bridge
        ]
    ),
    ?assert(catena_control_validate:is_report(
        catena_compilation_unit:control_validation(First)
    )).

recursive_dynamic_and_trait_calls_have_stable_modes_test() ->
    {ok, Recursive} =
        catena_compile:compile_string_to_unit(recursive_source()),
    RecursiveModes = catena_compilation_unit:control_modes(Recursive),
    ?assertEqual(
        {ok, resumable},
        catena_control_mode:mode(loop, RecursiveModes)
    ),
    ?assert(lists:member(
        cps_call,
        operations(catena_compilation_unit:control_ir(Recursive))
    )),
    {ok, Dynamic} =
        catena_compile:compile_string_to_unit(dynamic_source()),
    DynamicModes = catena_compilation_unit:control_modes(Dynamic),
    lists:foreach(
        fun(Name) ->
            ?assertEqual(
                {ok, resumable},
                catena_control_mode:mode(Name, DynamicModes)
            )
        end,
        [invoke, compare]
    ),
    {ok, Invoke} = catena_control_mode:lookup(invoke, DynamicModes),
    ?assert(lists:member(higher_order_call, maps:get(reasons, Invoke))),
    {ok, Compare} = catena_control_mode:lookup(compare, DynamicModes),
    ?assert(lists:member(trait_dispatch, maps:get(reasons, Compare))),
    Closures = [
        maps:get(closure, maps:get(fields, Node))
        || Node <- catena_control_ir:nodes(
            catena_compilation_unit:control_ir(Dynamic)
        ),
           lists:member(maps:get(op, Node), [direct_call, cps_call, bridge])
    ],
    ?assert(lists:any(
        fun(Closure) ->
            maps:get(kind, Closure) =:= higher_order andalso
                maps:get(control_mode, Closure) =:= resumable
        end,
        Closures
    )),
    ?assert(lists:any(
        fun(Closure) ->
            maps:get(kind, Closure) =:= trait_dictionary andalso
                maps:get(control_mode, Closure) =:= resumable
        end,
        Closures
    )).

source_resumption_rows_drive_conservative_open_mode_test() ->
    Source =
        "module PhaseFourOpenRow\n"
        "transform keep : Resumption k Int Int e -> "
        "Resumption k Int Int e\n",
    {ok, Unit} = catena_compile:compile_string_to_unit(Source),
    {module, _, _, _, [Signature], _} =
        catena_compilation_unit:normalized_ast(Unit),
    {transform_decl, keep, DeclaredType, [], Location} = Signature,
    Executable = {
        transform_decl,
        keep,
        DeclaredType,
        [
            {transform_clause,
                [{pat_var, value, Location}],
                undefined,
                {var, value, Location},
                Location}
        ],
        Location
    },
    [Typed] = catena_compilation_unit:typed_declarations(Unit),
    {ok, Callables} = catena_call_resolution:build(
        'PhaseFourOpenRow',
        [],
        [Executable]
    ),
    {ok, Modes} = catena_control_mode:analyze(
        'PhaseFourOpenRow',
        [Executable],
        [Typed],
        Callables,
        #{}
    ),
    ?assertEqual({ok, resumable}, catena_control_mode:mode(keep, Modes)),
    {ok, Entry} = catena_control_mode:lookup(keep, Modes),
    ?assertEqual(open_effect_row, maps:get(reason, Entry)).

imported_modes_are_published_and_consumed_test() ->
    Sources = #{
        'PhaseFourProvider' =>
            "module PhaseFourProvider\n"
            "export transform increment\n"
            "transform increment : Int -> Int\n"
            "transform increment value = value + 1\n",
        'PhaseFourConsumer' =>
            "module PhaseFourConsumer\n"
            "export transform run\n"
            "import PhaseFourProvider\n"
            "transform run value = increment value\n"
    },
    {ok, Result} = catena_module_compile:compile_source_set(Sources, #{}),
    Artifacts = maps:get(artifacts, Result),
    ProviderUnit = maps:get(
        unit,
        maps:get('PhaseFourProvider', Artifacts)
    ),
    ConsumerUnit = maps:get(
        unit,
        maps:get('PhaseFourConsumer', Artifacts)
    ),
    {ok, Published} = catena_module_interface:find_export(
        transform,
        increment,
        catena_compilation_unit:interface(ProviderUnit)
    ),
    ?assertEqual(direct, maps:get(control_mode, Published)),
    {ok, Run} = catena_control_mode:lookup(
        run,
        catena_compilation_unit:control_modes(ConsumerUnit)
    ),
    [ImportedEdge] = [
        Edge
        || Edge <- maps:get(edges, Run),
           maps:get(kind, Edge) =:= imported
    ],
    ?assertEqual(direct, maps:get(capability, ImportedEdge)),
    ?assertEqual(direct, maps:get(mode, Run)),
    [Call] = [
        Node
        || Node <- nodes_for(
            run,
            catena_compilation_unit:control_ir(ConsumerUnit)
        ),
           maps:get(op, Node) =:= direct_call
    ],
    ?assertMatch(
        #{kind := imported, control_mode := direct},
        maps:get(closure, maps:get(fields, Call))
    ).

automatic_resume_and_explicit_abort_are_distinct_test() ->
    Source =
        "module PhaseFourCaseModes\n"
        "effect Choice\n"
        "operation choose : Int\n"
        "end\n"
        "transform automatic = handle perform Choice.choose() then {\n"
        "  Choice { choose() -> 41 }\n"
        "}\n"
        "transform abandoned = handle perform Choice.choose() then {\n"
        "  Choice { choose() with k -> 0 }\n"
        "}\n",
    {ok, Unit} = catena_compile:compile_string_to_unit(Source),
    IR = catena_compilation_unit:control_ir(Unit),
    Automatic = [maps:get(op, Node) || Node <- nodes_for(automatic, IR)],
    Abandoned = [maps:get(op, Node) || Node <- nodes_for(abandoned, IR)],
    ?assert(lists:member(resume, Automatic)),
    ?assertNot(lists:member(abort, Automatic)),
    ?assert(lists:member(abort, Abandoned)),
    ?assertNot(lists:member(resume, Abandoned)).

first_class_resumption_uses_its_authority_delimiter_test() ->
    Source =
        "module PhaseFourFirstClass\n"
        "effect State\n"
        "operation put : Int -> Int\n"
        "end\n"
        "transform identity value = value\n"
        "transform advance k value = resume(k, value)\n"
        "transform run ignored = handle perform State.put(1) then {\n"
        "  State { put(value) with k -> "
        "let result = advance (identity k) value in result }\n"
        "}\n",
    {ok, Unit} = catena_compile:compile_string_to_unit(Source),
    [Resume] = [
        Node
        || Node <- nodes_for(
            advance,
            catena_compilation_unit:control_ir(Unit)
        ),
           maps:get(op, Node) =:= resume
    ],
    Fields = maps:get(fields, Resume),
    ?assertEqual(
        from_resumption_authority,
        maps:get(delimiter, Fields)
    ),
    ?assertMatch(
        #{type := {tresumption, {tkvar, resumption_kind, _}, _, _, _}},
        maps:get(authority, Fields)
    ).

malformed_graphs_fail_with_source_oriented_diagnostics_test() ->
    {ok, Unit} = catena_compile:compile_string_to_unit(mixed_source()),
    IR = catena_compilation_unit:control_ir(Unit),
    Cases = [
        {
            mutate_first(IR, resume, fun(Node) ->
                Metadata = maps:get(metadata, Node),
                Node#{metadata =>
                    Metadata#{continuation_arity => 0}}
            end),
            invalid_control_ir,
            continuation_arity_mismatch
        },
        {
            mutate_first(IR, resume, fun(Node) ->
                Fields = maps:get(fields, Node),
                Node#{fields => Fields#{delimiter => missing}}
            end),
            invalid_control_ir,
            dangling_delimiter
        },
        {
            mutate_first(IR, bridge, fun(Node) ->
                Fields = maps:get(fields, Node),
                Closure = maps:get(closure, Fields),
                Node#{fields => Fields#{closure =>
                    Closure#{runtime_arity => 99}}}
            end),
            resumption_abi_mismatch,
            invalid_control_closure
        },
        {
            mutate_first(IR, direct_expr, fun(Node) ->
                Fields = maps:get(fields, Node),
                Node#{fields => Fields#{
                    source => {resume_expr, forged, value, loc()}
                }}
            end),
            invalid_control_ir,
            leaked_control_ast
        }
    ],
    lists:foreach(
        fun({InvalidIR, Category, ReasonTag}) ->
            {error, {backend_error, Category, Details}} =
                validate(InvalidIR, Unit),
            ?assertMatch({location, _, _}, maps:get(location, Details)),
            ?assertEqual(
                ReasonTag,
                element(1, maps:get(reason, Details))
            )
        end,
        Cases
    ).

direct_backend_remains_executable_and_explicit_control_fails_closed_test() ->
    Direct =
        "module PhaseFourDirectExecution\n"
        "export transform run\n"
        "transform increment value = value + 1\n"
        "transform run value = increment value\n",
    with_loaded_module(Direct, fun() ->
        ?assertEqual(42, 'PhaseFourDirectExecution':run(41))
    end),
    Explicit = explicit_source(),
    ?assertMatch(
        {error, {missing_resumption_lowering, #{
            stage := backend_compatibility,
            mode := explicit_control,
            location := {location, _, _}
        }}},
        catena_compile:compile_string_to_core(Explicit)
    ),
    {ok, Unit} = catena_compile:compile_string_to_unit(Explicit),
    ?assert(catena_control_validate:is_report(
        catena_compilation_unit:control_validation(Unit)
    )).

mixed_source() ->
    "module PhaseFourMixed\n"
    "effect Choice\n"
    "operation choose : Int\n"
    "end\n"
    "effect Console\n"
    "operation read : Int\n"
    "end\n"
    "transform identity value = value\n"
    "transform provider ignored = perform Console.read()\n"
    "transform run ignored = handle "
    "(let chosen = perform Choice.choose() in identity chosen) then {\n"
    "  Choice { choose() with k -> resume(k, 41) }\n"
    "}\n"
    "transform caller value = run value\n".

recursive_source() ->
    "module PhaseFourRecursive\n"
    "effect Choice\n"
    "operation choose : Int\n"
    "end\n"
    "transform loop : Int -> Int\n"
    "transform loop value = handle perform Choice.choose() then {\n"
    "  Choice { choose() with k -> resume(k, loop value) }\n"
    "}\n".

dynamic_source() ->
    "module PhaseFourDynamic\n"
    "type Flag = On | Off\n"
    "trait Comparable a where\n"
    "  equals : a -> a -> Bool\n"
    "end\n"
    "instance Comparable Flag where\n"
    "  transform equals left right = true\n"
    "end\n"
    "transform invoke f value = f value\n"
    "transform compare left right = equals left right\n".

explicit_source() ->
    "module PhaseFourExplicitBoundary\n"
    "effect Choice\n"
    "operation choose : Int\n"
    "end\n"
    "transform run = handle perform Choice.choose() then {\n"
    "  Choice { choose() with k -> resume(k, 1) }\n"
    "}\n".

operations(IR) ->
    [maps:get(op, Node) || Node <- catena_control_ir:nodes(IR)].

nodes_for(Name, IR) ->
    {ok, Transform} = catena_control_ir:lookup(Name, IR),
    lists:append([
        collect_nodes(maps:get(body, Clause))
        || Clause <- maps:get(clauses, Transform)
    ]).

collect_nodes(Node) when is_map(Node) ->
    Current = case catena_control_ir:is_node(Node) of
        true -> [Node];
        false -> []
    end,
    Current ++ lists:append([
        collect_nodes(Value)
        || Value <- maps:values(Node)
    ]);
collect_nodes(Terms) when is_list(Terms) ->
    lists:append([collect_nodes(Term) || Term <- Terms]);
collect_nodes(Term) when is_tuple(Term) ->
    collect_nodes(tuple_to_list(Term));
collect_nodes(_Term) ->
    [].

mutate_first(IR, Operation, Fun) ->
    {Transforms, true} = rewrite_first(
        maps:get(transforms, IR),
        Operation,
        Fun,
        false
    ),
    IR#{transforms => Transforms}.

rewrite_first(Term, Operation, Fun, false) when is_map(Term) ->
    case catena_control_ir:is_node(Term) andalso
        maps:get(op, Term) =:= Operation
    of
        true ->
            {Fun(Term), true};
        false ->
            rewrite_map(Term, Operation, Fun)
    end;
rewrite_first(Terms, Operation, Fun, false) when is_list(Terms) ->
    rewrite_list(Terms, Operation, Fun, false);
rewrite_first(Term, Operation, Fun, false) when is_tuple(Term) ->
    {Values, Changed} = rewrite_list(
        tuple_to_list(Term),
        Operation,
        Fun,
        false
    ),
    {list_to_tuple(Values), Changed};
rewrite_first(Term, _Operation, _Fun, Changed) ->
    {Term, Changed}.

rewrite_map(Map, Operation, Fun) ->
    {Pairs, Changed} = rewrite_list(
        maps:to_list(Map),
        Operation,
        Fun,
        false
    ),
    {maps:from_list(Pairs), Changed}.

rewrite_list([], _Operation, _Fun, Changed) ->
    {[], Changed};
rewrite_list([Value | Rest], Operation, Fun, Changed0) ->
    {NewValue, Changed1} = rewrite_first(
        Value,
        Operation,
        Fun,
        Changed0
    ),
    {NewRest, Changed2} = rewrite_list(
        Rest,
        Operation,
        Fun,
        Changed1
    ),
    {[NewValue | NewRest], Changed2}.

validate(IR, Unit) ->
    catena_control_validate:validate_ir(
        IR,
        catena_compilation_unit:control_modes(Unit),
        #{
            module => catena_compilation_unit:module_name(Unit),
            source_identity => catena_compilation_unit:source_identity(Unit)
        }
    ).

with_loaded_module(Source, Assertion) ->
    {ok, Artifact} = catena_compile:compile_string_to_beam(Source),
    Module = maps:get(runtime_module, Artifact),
    unload(Module),
    try
        {module, Module} = code:load_binary(
            Module,
            "delimited-resumption-phase4-memory",
            maps:get(beam, Artifact)
        ),
        Assertion()
    after
        unload(Module)
    end.

unload(Module) ->
    _ = code:purge(Module),
    _ = code:delete(Module),
    ok.

loc() ->
    {location, 1, 1}.
