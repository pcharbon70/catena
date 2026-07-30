-module(catena_control_abi_tests).

-include_lib("eunit/include/eunit.hrl").

entry_shapes_preserve_public_arity_test() ->
    Direct = catena_control_abi:entry_shape(run, 2, direct),
    Resumable = catena_control_abi:entry_shape(run, 2, resumable),
    ?assertEqual({run, 2}, maps:get(public, Direct)),
    ?assertEqual({run, 2}, maps:get(public, Resumable)),
    ?assertEqual({direct, run, 3}, maps:get(private, Direct)),
    ?assertEqual({cps, run, 4}, maps:get(private, Resumable)),
    ?assertEqual(ok, catena_control_abi:validate_entry(Direct)),
    ?assertEqual(ok, catena_control_abi:validate_entry(Resumable)).

closure_shapes_carry_mode_capability_test() ->
    Direct = catena_control_abi:closure_shape(
        local,
        {run, 1},
        1,
        direct,
        loc()
    ),
    CPS = catena_control_abi:closure_shape(
        trait_dictionary,
        {map, 2},
        2,
        resumable,
        loc()
    ),
    ?assertEqual(2, maps:get(runtime_arity, Direct)),
    ?assertEqual(4, maps:get(runtime_arity, CPS)),
    ?assertEqual(0, maps:get(continuation_arity, Direct)),
    ?assertEqual(1, maps:get(continuation_arity, CPS)),
    ?assertEqual(ok, catena_control_abi:validate_closure(Direct)),
    ?assertEqual(ok, catena_control_abi:validate_closure(CPS)),
    ?assertMatch(
        {error, {invalid_control_closure, _}},
        catena_control_abi:validate_closure(
            Direct#{runtime_arity => 99}
        )
    ).

bridges_are_explicit_and_proof_gated_test() ->
    ?assertEqual(
        {ok, none},
        catena_control_abi:bridge(
            direct,
            direct,
            missing,
            run,
            loc()
        )
    ),
    ?assertMatch(
        {ok, #{kind := direct_to_cps, proof := direct_callee}},
        catena_control_abi:bridge(
            resumable,
            direct,
            missing,
            run,
            loc()
        )
    ),
    ?assertMatch(
        {ok, #{
            kind := resumable_to_direct,
            proof := non_suspending
        }},
        catena_control_abi:bridge(
            direct,
            resumable,
            non_suspending,
            run,
            loc()
        )
    ),
    ?assertMatch(
        {error, {resumption_abi_mismatch, _}},
        catena_control_abi:bridge(
            direct,
            resumable,
            missing,
            run,
            loc()
        )
    ).

loc() ->
    {location, 1, 1}.
