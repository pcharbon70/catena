-module(catena_effect_context_resumption_tests).
-include_lib("eunit/include/eunit.hrl").

catena_effect_context_resumption_test_() ->
    {foreach,
        fun setup/0,
        fun cleanup/1,
        [
            fun context_entries_distinguish_runtime_kinds/0,
            fun explicit_resume_returns_delimiter_result/0,
            fun value_case_auto_resumes_once/0,
            fun nested_handler_shadowing_is_innermost_first/0,
            fun missing_inner_operation_falls_back_to_parent/0,
            fun operation_arity_is_validated/0,
            fun local_value_provider_continues_on_owner/0,
            fun process_provider_never_runs_continuation/0,
            fun builtin_provider_preserves_owner_identity/0,
            fun retained_resumption_restores_deep_context/0
        ]}.

setup() ->
    catena_resumption_runtime:reset_for_test().

cleanup(_State) ->
    catena_resumption_runtime:reset_for_test().

context_entries_distinguish_runtime_kinds() ->
    Root = catena_effect_runtime:empty_context(),
    ProcessKind = catena_effect_runtime:with_handlers(
        Root,
        [{'Remote', [{get, fun() -> remote end}]}],
        fun(Ctx) -> current_entry_kind(Ctx) end
    ),
    ValueKind = catena_effect_runtime:with_value_provider(
        Root,
        {'LocalValue', [{get, fun() -> local end}]},
        fun(Ctx) -> current_entry_kind(Ctx) end
    ),
    ResumableKind = catena_effect_runtime:with_resumable_handler(
        Root,
        handler(
            'LocalControl',
            [catena_effect_runtime:value_case(
                get,
                0,
                fun([], _HandlerCtx) -> local end
            )]
        ),
        fun(Ctx) -> current_entry_kind(Ctx) end
    ),
    ?assertEqual(
        {process_provider, local_value_provider, local_resumable},
        {ProcessKind, ValueKind, ResumableKind}
    ).

explicit_resume_returns_delimiter_result() ->
    Owner = self(),
    Case = catena_effect_runtime:control_case(
        choose,
        0,
        fun([], Resumption, _HandlerCtx) ->
            {handler, self(),
                catena_effect_runtime:resume(Resumption, 41)}
        end
    ),
    Result = catena_effect_runtime:with_resumable_handler(
        catena_effect_runtime:empty_context(),
        handler('Choice', [Case]),
        fun(Ctx) ->
            catena_effect_runtime:perform_cps(
                Ctx,
                'Choice',
                choose,
                [],
                fun(Value, RestoredCtx) ->
                    {
                        continuation,
                        self(),
                        Value + 1,
                        current_entry_kind(RestoredCtx)
                    }
                end
            )
        end
    ),
    ?assertEqual(
        {handler, Owner, {continuation, Owner, 42, local_resumable}},
        Result
    ).

value_case_auto_resumes_once() ->
    Case = catena_effect_runtime:value_case(
        read,
        1,
        fun([Path], _HandlerCtx) -> {contents, Path} end
    ),
    Result = catena_effect_runtime:with_resumable_handler(
        catena_effect_runtime:empty_context(),
        handler('FileIO', [Case]),
        fun(Ctx) ->
            catena_effect_runtime:perform_cps(
                Ctx,
                'FileIO',
                read,
                [<<"notes">>],
                fun(Value, _RestoredCtx) -> {continued, Value} end
            )
        end
    ),
    ?assertEqual(
        {continued, {contents, <<"notes">>}},
        Result
    ).

nested_handler_shadowing_is_innermost_first() ->
    Outer = catena_effect_runtime:value_case(
        get,
        0,
        fun([], _Ctx) -> outer end
    ),
    Inner = catena_effect_runtime:value_case(
        get,
        0,
        fun([], _Ctx) -> inner end
    ),
    Result = catena_effect_runtime:with_resumable_handler(
        catena_effect_runtime:empty_context(),
        handler('State', [Outer]),
        fun(OuterCtx) ->
            catena_effect_runtime:with_resumable_handler(
                OuterCtx,
                handler('State', [Inner]),
                fun(InnerCtx) ->
                    catena_effect_runtime:perform_cps(
                        InnerCtx,
                        'State',
                        get,
                        [],
                        fun(Value, _Ctx) -> Value end
                    )
                end
            )
        end
    ),
    ?assertEqual(inner, Result).

missing_inner_operation_falls_back_to_parent() ->
    Outer = catena_effect_runtime:value_case(
        outer,
        0,
        fun([], _Ctx) -> outer_value end
    ),
    Inner = catena_effect_runtime:value_case(
        inner,
        0,
        fun([], _Ctx) -> inner_value end
    ),
    Result = catena_effect_runtime:with_resumable_handler(
        catena_effect_runtime:empty_context(),
        handler('Nested', [Outer]),
        fun(OuterCtx) ->
            catena_effect_runtime:with_resumable_handler(
                OuterCtx,
                handler('Nested', [Inner]),
                fun(InnerCtx) ->
                    catena_effect_runtime:perform_cps(
                        InnerCtx,
                        'Nested',
                        outer,
                        [],
                        fun(Value, RestoredCtx) ->
                            {Value, current_entry_kind(RestoredCtx)}
                        end
                    )
                end
            )
        end
    ),
    ?assertEqual({outer_value, local_resumable}, Result).

operation_arity_is_validated() ->
    Case = catena_effect_runtime:value_case(
        put,
        1,
        fun([Value], _Ctx) -> Value end
    ),
    ?assertError(
        {
            effect_runtime_error,
            {operation_arity_mismatch, 'State', put, 0}
        },
        catena_effect_runtime:with_resumable_handler(
            catena_effect_runtime:empty_context(),
            handler('State', [Case]),
            fun(Ctx) ->
                catena_effect_runtime:perform_cps(
                    Ctx,
                    'State',
                    put,
                    [],
                    fun(Value, _RestoredCtx) -> Value end
                )
            end
        )
    ).

local_value_provider_continues_on_owner() ->
    Owner = self(),
    Result = catena_effect_runtime:with_value_provider(
        catena_effect_runtime:empty_context(),
        {'Local', [{owner, fun() -> self() end}]},
        fun(Ctx) ->
            catena_effect_runtime:perform_cps(
                Ctx,
                'Local',
                owner,
                [],
                fun(ProviderOwner, _RestoredCtx) ->
                    {ProviderOwner, self()}
                end
            )
        end
    ),
    ?assertEqual({Owner, Owner}, Result).

process_provider_never_runs_continuation() ->
    Owner = self(),
    Result = catena_effect_runtime:with_handlers(
        catena_effect_runtime:empty_context(),
        [{'Remote', [{provider_owner, fun() -> self() end}]}],
        fun(Ctx) ->
            catena_effect_runtime:perform_cps(
                Ctx,
                'Remote',
                provider_owner,
                [],
                fun(ProviderOwner, _RestoredCtx) ->
                    {ProviderOwner, self()}
                end
            )
        end
    ),
    {ProviderOwner, ContinuationOwner} = Result,
    ?assert(ProviderOwner =/= Owner),
    ?assertEqual(Owner, ContinuationOwner).

builtin_provider_preserves_owner_identity() ->
    Owner = self(),
    Result = catena_effect_runtime:perform_cps(
        catena_effect_runtime:empty_context(),
        'Process',
        self,
        [],
        fun(BuiltinOwner, _Ctx) -> {BuiltinOwner, self()} end
    ),
    ?assertEqual({Owner, Owner}, Result).

retained_resumption_restores_deep_context() ->
    Case = catena_effect_runtime:control_case(
        step,
        1,
        fun
            ([retain], Resumption, _HandlerCtx) ->
                Resumption;
            ([Value], Resumption, _HandlerCtx) ->
                catena_effect_runtime:resume(Resumption, Value + 1)
        end
    ),
    Retained = catena_effect_runtime:with_resumable_handler(
        catena_effect_runtime:empty_context(),
        handler('Deep', [Case]),
        fun(Ctx) ->
            catena_effect_runtime:perform_cps(
                Ctx,
                'Deep',
                step,
                [retain],
                fun(First, RestoredCtx) ->
                    catena_effect_runtime:perform_cps(
                        RestoredCtx,
                        'Deep',
                        step,
                        [First],
                        fun(Second, _Ctx) -> {First, Second, self()} end
                    )
                end
            )
        end
    ),
    ?assert(catena_resumption_runtime:is_resumption(Retained)),
    ?assertEqual(
        {10, 11, self()},
        catena_effect_runtime:resume(Retained, 10)
    ).

handler(Effect, Cases) ->
    #{
        effect => Effect,
        cases => Cases,
        origin => {test_handler, Effect}
    }.

current_entry_kind(Ctx) ->
    [Entry] = maps:get(entries, Ctx),
    maps:get(kind, Entry).
