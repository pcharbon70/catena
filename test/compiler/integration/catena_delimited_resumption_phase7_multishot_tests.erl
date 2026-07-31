%%%-------------------------------------------------------------------
%%% @doc Section 7.3 multi-shot branch authority and resource policy.
%%%-------------------------------------------------------------------
-module(catena_delimited_resumption_phase7_multishot_tests).

-include_lib("eunit/include/eunit.hrl").

catena_delimited_resumption_phase7_multishot_test_() ->
    {foreach,
        fun setup/0,
        fun cleanup/1,
        [
            fun repeated_invocations_receive_isolated_branch_identities/0,
            fun failed_branch_does_not_poison_later_branch/0,
            fun nested_one_shot_authorities_are_branch_local/0,
            fun every_branch_preserves_process_affinity/0,
            fun invocation_and_retention_budgets_fail_deterministically/0,
            fun reduction_timeout_and_depth_budgets_are_enforced/0,
            fun external_capabilities_and_provider_state_are_rejected/0,
            fun discard_releases_multi_shot_authority/0,
            fun source_to_loaded_beam_executes_two_distinct_branches/0
        ]}.

setup() ->
    catena_resumption_runtime:reset_for_test().

cleanup(_State) ->
    catena_resumption_runtime:reset_for_test(),
    unload('PhaseSevenMultiShotRuntime').

repeated_invocations_receive_isolated_branch_identities() ->
    Owner = self(),
    {ok, Handle} = capture_multi(
        fun(Value, Context) ->
            {Value, self(), maps:get(runtime_branch, Context)}
        end,
        #{}
    ),
    ?assertMatch(
        {ok, {first, Owner, #{id := 1, depth := 1}}},
        catena_resumption_runtime:resume(Handle, first)
    ),
    ?assertMatch(
        {ok, {second, Owner, #{id := 2, depth := 1}}},
        catena_resumption_runtime:resume(Handle, second)
    ),
    {ok, Stats} = catena_resumption_runtime:branch_stats(Handle),
    ?assertEqual(fresh, maps:get(state, Stats)),
    ?assertEqual(2, maps:get(invocation_count, Stats)),
    ?assertEqual(2, maps:get(completed_branches, Stats)),
    ?assertEqual(0, maps:get(failed_branches, Stats)),
    ?assertMatch(#{id := 2, status := normal}, maps:get(last_branch, Stats)).

failed_branch_does_not_poison_later_branch() ->
    {ok, Handle} = capture_multi(
        fun
            (fail, _Context) -> erlang:error(branch_failed);
            (Value, _Context) -> {recovered, Value}
        end,
        #{}
    ),
    ?assertMatch(
        {error, #{
            category := handler_failure,
            details := #{reason := branch_failed}
        }},
        catena_resumption_runtime:resume(Handle, fail)
    ),
    ?assertEqual(
        {ok, {recovered, later}},
        catena_resumption_runtime:resume(Handle, later)
    ),
    {ok, Stats} = catena_resumption_runtime:branch_stats(Handle),
    ?assertEqual(2, maps:get(completed_branches, Stats)),
    ?assertEqual(1, maps:get(failed_branches, Stats)).

nested_one_shot_authorities_are_branch_local() ->
    {ok, Outer} = capture_multi(
        fun(_Value, BranchContext) ->
            capture_one_in_context(BranchContext)
        end,
        #{}
    ),
    {ok, {ok, FirstInner}} =
        catena_resumption_runtime:resume(Outer, first_branch),
    {ok, {ok, SecondInner}} =
        catena_resumption_runtime:resume(Outer, second_branch),
    ?assertEqual(
        {ok, first_value},
        catena_resumption_runtime:resume(FirstInner, first_value)
    ),
    ?assertMatch(
        {error, #{category := resumption_already_consumed}},
        catena_resumption_runtime:resume(FirstInner, reused)
    ),
    ?assertEqual(
        {ok, second_value},
        catena_resumption_runtime:resume(SecondInner, second_value)
    ).

every_branch_preserves_process_affinity() ->
    Owner = self(),
    {ok, Handle} = capture_multi(
        fun(Value, _Context) -> {self(), Value} end,
        #{}
    ),
    ?assertEqual(
        {ok, {Owner, local_one}},
        catena_resumption_runtime:resume(Handle, local_one)
    ),
    Parent = self(),
    spawn(fun() ->
        Parent ! {foreign_branch,
            catena_resumption_runtime:resume(Handle, foreign)}
    end),
    receive
        {foreign_branch, Result} ->
            ?assertMatch(
                {error, #{category := wrong_resumption_owner}},
                Result
            )
    after 1000 ->
        ?assert(false)
    end,
    ?assertEqual(
        {ok, {Owner, local_two}},
        catena_resumption_runtime:resume(Handle, local_two)
    ).

invocation_and_retention_budgets_fail_deterministically() ->
    {ok, Handle} = capture_multi(
        fun(Value, _Context) -> Value end,
        #{max_invocations => 2}
    ),
    ?assertEqual({ok, one}, catena_resumption_runtime:resume(Handle, one)),
    ?assertEqual({ok, two}, catena_resumption_runtime:resume(Handle, two)),
    ?assertMatch(
        {error, #{
            category := resumption_budget_exceeded,
            details := #{resource := invocations, limit := 2}
        }},
        catena_resumption_runtime:resume(Handle, three)
    ),
    ?assertMatch(
        {error, #{
            category := resumption_budget_exceeded,
            details := #{resource := retained_words, limit := 1}
        }},
        capture_multi(fun(Value, _Context) -> Value end, #{
            max_retained_words => 1
        })
    ).

reduction_timeout_and_depth_budgets_are_enforced() ->
    {ok, ReductionHandle} = capture_multi(
        fun(Value, _Context) -> burn_reductions(Value) end,
        #{max_reductions => 10}
    ),
    ?assertMatch(
        {error, #{
            category := resumption_budget_exceeded,
            details := #{resource := reductions, limit := 10}
        }},
        catena_resumption_runtime:resume(ReductionHandle, 1000)
    ),
    {ok, TimeoutHandle} = capture_multi(
        fun(Value, _Context) -> timer:sleep(3), Value end,
        #{timeout => 1}
    ),
    ?assertMatch(
        {error, #{
            category := resumption_budget_exceeded,
            details := #{resource := timeout, limit := 1}
        }},
        catena_resumption_runtime:resume(TimeoutHandle, delayed)
    ),
    {ok, Outer} = capture_multi(
        fun(_Value, BranchContext) ->
            capture_multi_in_context(
                fun(Value, _InnerContext) -> Value end,
                BranchContext,
                #{max_branch_depth => 1}
            )
        end,
        #{max_branch_depth => 1}
    ),
    {ok, {ok, Inner}} = catena_resumption_runtime:resume(Outer, create),
    ?assertMatch(
        {error, #{
            category := resumption_budget_exceeded,
            details := #{resource := branch_depth, limit := 1}
        }},
        catena_resumption_runtime:resume(Inner, nested)
    ).

external_capabilities_and_provider_state_are_rejected() ->
    Owner = self(),
    ?assertMatch(
        {error, #{
            category := inadmissible_multishot_context,
            details := #{reason := lexical_capability}
        }},
        capture_multi(
            fun(Value, _Context) -> {Owner, Value} end,
            #{}
        )
    ),
    ProviderContext = catena_effect_runtime:with_value_provider(
        catena_effect_runtime:empty_context(),
        {'LocalState', [{read, fun() -> state end}]},
        fun(Context) -> Context end
    ),
    ?assertMatch(
        {error, #{
            category := inadmissible_multishot_context,
            details := #{reason := local_provider_state}
        }},
        capture_multi_in_context(
            fun(Value, _Context) -> Value end,
            ProviderContext,
            #{}
        )
    ).

discard_releases_multi_shot_authority() ->
    {ok, Handle} = capture_multi(
        fun(Value, _Context) -> Value end,
        #{}
    ),
    ?assertEqual(ok, catena_resumption_runtime:discard(Handle)),
    ?assertEqual({ok, consumed}, catena_resumption_runtime:status(Handle)),
    ?assertEqual(
        {ok, released},
        catena_resumption_runtime:lease_status(Handle)
    ),
    ?assertMatch(
        {error, #{category := resumption_already_consumed}},
        catena_resumption_runtime:resume(Handle, unavailable)
    ).

source_to_loaded_beam_executes_two_distinct_branches() ->
    {ok, Artifact} = catena_compile:compile_string_to_beam(source()),
    Contract = maps:get(runtime_contract, Artifact),
    ?assertEqual(
        [#{depth => deep, kind => multi_shot}],
        maps:get(handler_modes, Contract)
    ),
    ?assert(lists:member(
        multi_shot_resumptions,
        maps:get(required_handler_frame_features, Contract)
    )),
    {ok, Artifact} = catena_beam_artifact:validate(Artifact),
    {module, 'PhaseSevenMultiShotRuntime'} =
        catena_beam_artifact:load(Artifact),
    ?assertEqual(82, 'PhaseSevenMultiShotRuntime':run(ignored)).

capture_multi(Continuation, Budget) ->
    capture_multi_in_context(Continuation, #{}, Budget).

capture_multi_in_context(Continuation, Context, Budget) ->
    catena_resumption_runtime:capture(Continuation, #{
        context => Context,
        parent_context => Context,
        delimiter => make_ref(),
        depth => deep,
        kind => multi_shot,
        origin => {phase7_multishot, ?FUNCTION_NAME},
        metadata => #{fixture => compiler_generated},
        type_identity => dynamic,
        budget => Budget
    }).

capture_one_in_context(Context) ->
    catena_resumption_runtime:capture(
        fun(Value, _RestoredContext) -> Value end,
        #{
            context => Context,
            parent_context => Context,
            delimiter => make_ref(),
            depth => deep,
            kind => one_shot,
            origin => {phase7_nested_one_shot, ?FUNCTION_NAME},
            metadata => #{fixture => compiler_generated},
            type_identity => dynamic
        }
    ).

burn_reductions(0) ->
    done;
burn_reductions(Count) ->
    burn_reductions(Count - 1).

source() ->
    "module PhaseSevenMultiShotRuntime\n"
    "export transform run\n"
    "effect Choice\n"
    "operation choose : Int\n"
    "end\n"
    "transform run ignored = handle multi_shot "
        "(let selected = perform Choice.choose() in selected * 2) then {\n"
    "  Choice { choose() with k -> "
        "let first = resume(k, 20) in resume(k, first + 1) }\n"
    "}\n".

unload(Module) ->
    code:purge(Module),
    code:delete(Module).
