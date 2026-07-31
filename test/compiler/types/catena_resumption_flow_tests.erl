%%%-------------------------------------------------------------------
%%% Phase 3.3 tests for first-class flow and conservative mode checks.
%%%-------------------------------------------------------------------
-module(catena_resumption_flow_tests).

-include_lib("eunit/include/eunit.hrl").

resumption_flows_through_module_local_higher_order_calls_test() ->
    Source =
        "module FirstClassResumeFlow\n"
        "effect State\n"
        "operation put : Int -> Int\n"
        "end\n"
        "transform identity value = value\n"
        "transform advance k value = resume(k, value)\n"
        "transform run ignored = handle perform State.put(1) then {\n"
        "  State { put(value) with k -> "
        "let result = advance (identity k) value in result }\n"
        "}\n",
    {ok, {typed_module, _Name, Declarations, _Env}} =
        catena_compile:compile_string(Source),
    Advance = typed_transform(advance, Declarations),
    ?assertMatch(
        {
            typed_transform,
            advance,
            {
                tfun,
                {tresumption, {tkvar, resumption_kind, _}, _, _, _},
                {tfun, _, _, _},
                _
            },
            _,
            #{resumptions := [#{kind := resume}]},
            _
        },
        Advance
    ),
    Run = typed_transform(run, Declarations),
    ?assertMatch(
        {
            typed_transform,
            run,
            _,
            _,
            #{resumptions := [
                #{
                    kind := resumption_binder,
                    type := {
                        tresumption,
                        {tcon, 'OneShot'},
                        {tcon, int},
                        {tcon, int},
                        {teffectrow, [], closed}
                    }
                }
            ]},
            _
        },
        Run
    ).

source_signature_preserves_all_resumption_roles_test() ->
    Source =
        "transform advance : "
        "Resumption OneShot Int Int {} -> Int -> Int\n",
    {ok, {typed_module, _Name, Declarations, _Env}} =
        catena_compile:compile_string(Source),
    ?assertMatch(
        {
            typed_transform,
            advance,
            {
                tfun,
                {
                    tresumption,
                    {tcon, 'OneShot'},
                    {tcon, int},
                    {tcon, int},
                    {teffectrow, [], closed}
                },
                {tfun, {tcon, int}, {tcon, int}, _},
                _
            },
            [],
            _
        },
        typed_transform(advance, Declarations)
    ).

ordinary_containers_and_patterns_preserve_resumption_type_test() ->
    ResumptionType = one_shot_type(),
    Env = resumption_env(ResumptionType),
    Expressions = [
        {
            tuple,
            [{var, k}, {var, k}]
        },
        {
            list,
            [{var, k}, {var, k}],
            location(1)
        },
        {
            record,
            [{authority, {var, k}}]
        },
        {
            variant,
            'Deferred',
            [{var, k}]
        }
    ],
    ExpectedTypes = [
        {ttuple, [ResumptionType, ResumptionType]},
        {tapp, {tcon, list}, [ResumptionType]},
        {trecord, [{authority, ResumptionType}], closed},
        {tvariant, [{'Deferred', [ResumptionType]}]}
    ],
    lists:foreach(
        fun({Expr, Expected}) ->
            ?assertEqual(
                {ok, Expected},
                catena_infer:infer_expr(Expr, Env)
            )
        end,
        lists:zip(Expressions, ExpectedTypes)
    ),
    PatternFlow = {
        match,
        {tuple, [{var, k}, {lit, {int, 1}}]},
        [
            {
                {ptuple, [{pvar, retained}, {pwild}]},
                {var, retained}
            }
        ]
    },
    ?assertEqual(
        {ok, ResumptionType},
        catena_infer:infer_expr(PatternFlow, Env)
    ).

obvious_direct_duplicate_resume_is_rejected_test() ->
    Source = duplicate_source(
        "let first = resume(k, value) in resume(k, value)"
    ),
    Errors = transform_type_errors(Source),
    Context = error_context(obvious_one_shot_reuse, Errors),
    ?assertMatch(
        #{
            binder := k,
            mode := one_shot,
            resume_count := 2,
            resume_sites := [_, _],
            first_resume := _,
            duplicate_resume := _
        },
        Context
    ).

obvious_alias_duplicate_resume_is_rejected_test() ->
    Source = duplicate_source(
        "let alias = k in "
        "let first = resume(alias, value) in resume(k, value)"
    ),
    Errors = transform_type_errors(Source),
    ?assertMatch(
        #{resume_count := 2},
        error_context(obvious_one_shot_reuse, Errors)
    ).

exclusive_branch_resumes_are_not_reported_as_duplicates_test() ->
    Type = one_shot_type(),
    Resume = fun(Line) ->
        {
            resume_expr,
            {var, k, location(Line)},
            {lit, {int, 1}},
            location(Line)
        }
    end,
    Body = {
        'if',
        {lit, {bool, true}},
        Resume(2),
        Resume(3)
    },
    ?assertEqual(
        ok,
        catena_resumption_flow:validate_one_shot_case(
            k,
            Type,
            Body,
            #{binder => k}
        )
    ).

nested_reentrant_shape_is_rejected_test() ->
    Type = one_shot_type(),
    Body = {
        resume_expr,
        {var, k, location(2)},
        {
            resume_expr,
            {var, k, location(3)},
            {lit, {int, 1}},
            location(3)
        },
        location(2)
    },
    ?assertMatch(
        {error, {obvious_one_shot_reuse, #{resume_count := 2}}},
        catena_resumption_flow:validate_one_shot_case(
            k,
            Type,
            Body,
            #{binder => k}
        )
    ).

multi_shot_resume_rejects_inadmissible_residual_effects_test() ->
    Type = catena_types:tresumption(
        catena_types:multi_shot(),
        catena_types:tcon(int),
        catena_types:tcon(bool),
        catena_types:teffectrow(['State'])
    ),
    Env = resumption_env(Type),
    Expr = {
        resume_expr,
        {var, k, location(1)},
        {lit, {int, 1}},
        location(1)
    },
    {error, Errors} = catena_infer:infer_expr(Expr, Env),
    ?assertMatch(
        #{
            requested_mode := multi_shot,
            reason := external_or_stateful_effects_not_duplicable,
            residual_effects := {teffectrow, ['State'], closed}
        },
        error_context(inadmissible_multi_shot_effects, Errors)
    ).

opaque_resumption_vocabulary_cannot_be_forged_test_() ->
    Cases = [
        {
            "reserved type",
            "type Resumption = Fake\n",
            reserved_type_name
        },
        {
            "reserved constructor",
            "type Fake = Resumption Int\n",
            reserved_constructor_name
        },
        {
            "opaque value",
            "transform forge value = Resumption value\n",
            opaque_value_construction
        }
    ],
    [
        {Label, fun() ->
            ?assertMatch(
                {error, {invalid_resumption_representation, #{
                    reason := Reason,
                    name := 'Resumption',
                    location := _
                }}},
                catena_compile:compile_string(Source)
            )
        end}
        || {Label, Source, Reason} <- Cases
    ].

opaque_resumption_pattern_is_rejected_test() ->
    Location = location(4),
    Declarations = [
        {
            transform_decl,
            inspect,
            undefined,
            [
                {
                    transform_clause,
                    [{pat_constructor, 'Resumption', [], Location}],
                    undefined,
                    {literal, 1, integer, Location},
                    Location
                }
            ],
            Location
        }
    ],
    ?assertMatch(
        {error, {invalid_resumption_representation, #{
            reason := opaque_pattern_match,
            name := 'Resumption',
            location := Location
        }}},
        catena_resumption_flow:validate_declarations(Declarations)
    ).

typed_transform(Name, Declarations) ->
    lists:keyfind(Name, 2, Declarations).

one_shot_type() ->
    catena_types:tresumption(
        catena_types:one_shot(),
        catena_types:tcon(int),
        catena_types:tcon(int),
        catena_types:teffectrow([])
    ).

resumption_env(Type) ->
    catena_type_env:singleton(
        k,
        catena_type_scheme:mono(Type)
    ).

duplicate_source(Body) ->
    "module DuplicateResume\n"
    "effect State\n"
    "operation put : Int -> Int\n"
    "end\n"
    "transform run ignored = handle perform State.put(1) then {\n"
    "  State { put(value) with k -> " ++ Body ++ " }\n"
    "}\n".

transform_type_errors(Source) ->
    {error, {type_error, run, Errors}} =
        catena_compile:compile_string(Source),
    Errors.

error_context(Tag, Errors) ->
    {Tag, Context} = lists:keyfind(Tag, 1, Errors),
    Context.

location(Line) ->
    {location, Line, 0}.
