%%%-------------------------------------------------------------------
%%% @doc Phase 7 Section 7.1 mode surface and static-semantics contract.
%%%-------------------------------------------------------------------
-module(catena_delimited_resumption_phase7_static_tests).

-include_lib("eunit/include/eunit.hrl").

mode_keywords_boundaries_and_roundtrip_test() ->
    {ok, Tokens} = catena_lexer:tokenize(
        "shallow multi_shot shallower multi_shots"
    ),
    ?assertEqual(
        [
            {shallow, 1},
            {multi_shot, 1},
            {lower_ident, 1, "shallower"},
            {lower_ident, 1, "multi_shots"}
        ],
        Tokens
    ),
    Handle = parsed_handle(
        "handle multi_shot shallow 1 then { "
        "Choice { choose() with k -> 1 } }"
    ),
    {handle_expr, Mode, _, _, _} = Handle,
    ?assertEqual(
        #{depth => shallow, kind => multi_shot},
        catena_resumption_mode:interface_view(Mode)
    ),
    Printed = catena_ast_pp:pp_expr(Handle),
    ?assertEqual(
        "handle shallow multi_shot 1 then { Choice { choose() with k -> 1 } }",
        Printed
    ),
    ?assertMatch(
        {handle_expr,
            #{depth := shallow, kind := multi_shot},
            _, _, _},
        parsed_handle(Printed)
    ).

normalization_preserves_modes_and_auto_resume_test() ->
    Parsed = parsed_handle(
        "handle shallow perform Choice.choose() then { "
        "Choice { choose() -> 7 } }"
    ),
    {ok, Normalized} = catena_resumption_normalize:normalize_expr(Parsed),
    ?assertMatch(
        {handle_expr,
            #{depth := shallow, kind := one_shot},
            _,
            [{handler_clause, 'Choice', [
                {operation_case, choose, [],
                    {resumption_binder, _,
                        {synthetic, value_handler_auto_resume, _}},
                    {resume_expr, _, _,
                        {synthetic, value_handler_auto_resume, _}},
                    _}
            ], _}],
            _},
        Normalized
    ).

multi_shot_kind_allows_repeated_static_resume_test() ->
    {_Type, Evidence} = typed_run(multi_shot_source()),
    [Binder | _] = [
        Entry
        || Entry <- Evidence,
           maps:get(kind, Entry) =:= resumption_binder
    ],
    ?assertMatch(
        {tresumption, {tcon, 'MultiShot'}, _, _,
            {teffectrow, [], closed}},
        maps:get(type, Binder)
    ),
    ?assertEqual(multi_shot, maps:get(mode, Binder)),
    ?assertEqual(deep, maps:get(handler_depth, Binder)).

shallow_typing_retains_selected_effect_test() ->
    {Type, Evidence} = typed_run(shallow_source()),
    [Binder | _] = [
        Entry
        || Entry <- Evidence,
           maps:get(kind, Entry) =:= resumption_binder
    ],
    ?assertMatch(
        {tresumption, {tcon, 'OneShot'}, _, _,
            {teffectrow, ['Choice'], closed}},
        maps:get(type, Binder)
    ),
    ?assertEqual(shallow, maps:get(handler_depth, Binder)),
    ?assertMatch({tfun, _, _, {effect_set, ['Choice']}}, Type).

multi_shot_rejects_residual_and_open_effect_rows_test() ->
    {error, {type_error, run, Errors}} = catena_compile:compile_string(
        inadmissible_multi_shot_source()
    ),
    ?assertMatch(
        #{
            requested_mode := multi_shot,
            inadmissible_effects := ['Log'],
            reason := external_or_stateful_effects_not_duplicable
        },
        error_context(inadmissible_multi_shot_effects, Errors)
    ),
    OpenType = catena_types:tresumption(
        catena_types:multi_shot(),
        catena_types:tcon(int),
        catena_types:tcon(int),
        catena_types:teffectrow([], 77)
    ),
    ?assertMatch(
        {error, {inadmissible_multi_shot_effects,
            #{reason := open_effect_row}}},
        catena_resumption_flow:validate_supported_mode(OpenType, #{})
    ).

control_modes_and_interfaces_publish_handler_modes_test() ->
    Source = multi_shot_source(),
    {ok, Parsed} = catena_parse:parse(Source),
    {ok, {module, Module, _Exports, Imports, Declarations, _}} =
        catena_resumption_normalize:normalize(Parsed),
    {ok, {typed_module, Module, TypedDeclarations, _}} =
        catena_compile:compile_string(Source),
    {ok, Callables} = catena_call_resolution:build(
        Module,
        Imports,
        Declarations
    ),
    {ok, Modes} = catena_control_mode:analyze(
        Module,
        Declarations,
        TypedDeclarations,
        Callables,
        #{}
    ),
    ?assertEqual(
        [#{depth => deep, kind => multi_shot}],
        catena_control_mode:handler_modes(run, Modes)
    ),
    DefaultSource0 = lists:flatten(string:replace(
        Source, "handle multi_shot", "handle", all
    )),
    DefaultSource = lists:flatten(string:replace(
        DefaultSource0,
        "let first = resume(k, 1) in resume(k, first)",
        "resume(k, 1)",
        all
    )),
    {ok, Unit} = catena_compile:compile_string_to_unit(DefaultSource),
    {ok, PublishedInterface} = catena_module_interface:with_control_modes(
        catena_compilation_unit:interface(Unit),
        Modes
    ),
    {ok, Run} = catena_module_interface:find_export(
        transform,
        run,
        PublishedInterface
    ),
    ?assertEqual(
        [#{depth => deep, kind => multi_shot}],
        maps:get(handler_modes, Run)
    ).

parsed_handle(Expression) ->
    Source = "transform run = " ++ Expression,
    {ok, {module, _, _, _, [
        {transform_decl, run, _, [
            {transform_clause, _, _, Handle, _}
        ], _}
    ], _}} = catena_parse:parse(Source),
    Handle.

typed_run(Source) ->
    {ok, {typed_module, _Module, Declarations, _}} =
        catena_compile:compile_string(Source),
    {
        typed_transform,
        run,
        Type,
        _Clauses,
        #{resumptions := Evidence},
        _Location
    } = lists:keyfind(run, 2, Declarations),
    {Type, Evidence}.

multi_shot_source() ->
    "module PhaseSevenStaticMulti\n"
    "export transform run\n"
    "effect Choice\n"
    "operation choose : Int\n"
    "end\n"
    "transform run ignored = handle multi_shot "
    "perform Choice.choose() then {\n"
    "  Choice { choose() with k -> "
    "let first = resume(k, 1) in resume(k, first) }\n"
    "}\n".

shallow_source() ->
    "module PhaseSevenStaticShallow\n"
    "effect Choice\n"
    "operation choose : Int\n"
    "end\n"
    "transform run ignored = handle shallow "
    "perform Choice.choose() then {\n"
    "  Choice { choose() with k -> resume(k, 1) }\n"
    "}\n".

inadmissible_multi_shot_source() ->
    "module PhaseSevenStaticRejected\n"
    "effect Choice\n"
    "operation choose : Int\n"
    "end\n"
    "effect Log\n"
    "operation write : Int -> Int\n"
    "end\n"
    "transform run ignored = handle multi_shot "
    "(let logged = perform Log.write(0) in "
    "perform Choice.choose()) then {\n"
    "  Choice { choose() with k -> resume(k, 1) }\n"
    "}\n".

error_context(Reason, Errors) ->
    Matches = [Context || {Candidate, Context} <- Errors, Candidate =:= Reason],
    case Matches of
        [Context | _] -> Context;
        [] -> undefined
    end.
