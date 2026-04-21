%%%-------------------------------------------------------------------
%%% @doc Tests for the Phase 14.4 effect validation surface.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_effect_validation_tests).

-include_lib("eunit/include/eunit.hrl").

validate_all_test() ->
    Report = catena_effect_validation:validate(),
    ?assertEqual(all, maps:get(section, Report)),
    ?assertEqual(3, length(maps:get(reports, Report))),
    ?assert(maps:get(passed, Report)).

theoretical_validation_test() ->
    Report = catena_effect_validation:theoretical_validation(),
    ?assertEqual(theoretical, maps:get(section, Report)),
    ?assert(maps:get(passed, Report)),
    ?assert(find_check(law_sets_well_formed, Report)),
    ?assert(find_check(handler_models_sound, Report)).

property_validation_test() ->
    Report = catena_effect_validation:property_validation(),
    ?assertEqual(properties, maps:get(section, Report)),
    ?assert(maps:get(passed, Report)),
    ?assert(find_check(row_union_commutative, Report)),
    ?assert(find_check(handler_echo_roundtrip, Report)).

conformance_validation_test() ->
    Report = catena_effect_validation:conformance_validation(),
    ?assertEqual(conformance, maps:get(section, Report)),
    ?assert(maps:get(passed, Report)),
    ?assert(find_check(runtime_contract, Report)),
    ?assert(find_check(codegen_contract, Report)).

law_set_validation_details_test() ->
    {Passed, Details} = catena_effect_validation:validate_law_sets([state, reader, writer]),
    ?assert(Passed),
    ?assertEqual(3, length(Details)).

find_check(Name, Report) ->
    lists:any(
        fun(Check) ->
            maps:get(name, Check) =:= Name andalso maps:get(passed, Check)
        end,
        maps:get(checks, Report)
    ).
