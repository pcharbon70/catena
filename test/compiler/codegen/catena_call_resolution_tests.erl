-module(catena_call_resolution_tests).

-include_lib("eunit/include/eunit.hrl").

predeclares_all_transforms_independent_of_order_test() ->
    EarlierLocation = location(3, 1),
    LaterLocation = location(5, 1),
    Declarations = [
        transform(first, 1, EarlierLocation),
        transform(later, 2, LaterLocation)
    ],
    {ok, Inventory} = catena_call_resolution:build(
        local_calls,
        [{export_transform, first}],
        Declarations
    ),
    [First] = catena_call_resolution:lookup(first, Inventory),
    [Later] = catena_call_resolution:lookup(later, Inventory),
    ?assertEqual(1, maps:get(arity, First)),
    ?assertEqual(public, maps:get(visibility, First)),
    ?assertEqual(EarlierLocation, maps:get(location, First)),
    ?assertEqual(2, maps:get(arity, Later)),
    ?assertEqual(private, maps:get(visibility, Later)),
    ?assertEqual(LaterLocation, maps:get(location, Later)).

predeclares_constructor_ownership_and_arity_test() ->
    Declaration =
        {type_decl,
            'Result',
            [a],
            [
                {constructor, 'Ok', [{type_var, a}], location(2, 15)},
                {constructor, 'Pair',
                    [{type_var, a}, {type_var, a}], location(2, 22)}
            ],
            [],
            location(2, 1)},
    {ok, Inventory} = catena_call_resolution:build(
        constructors,
        [{export_type, 'Result'}],
        [Declaration]
    ),
    [Ok] = catena_call_resolution:lookup('Ok', Inventory),
    [Pair] = catena_call_resolution:lookup('Pair', Inventory),
    ?assertEqual('Result', maps:get(owner, Ok)),
    ?assertEqual(1, maps:get(arity, Ok)),
    ?assertEqual(2, maps:get(arity, Pair)),
    ?assertEqual(public, maps:get(visibility, Pair)).

duplicate_callable_names_are_rejected_test() ->
    Result = catena_call_resolution:build(
        duplicate_calls,
        [],
        [
            transform(run, 1, location(2, 1)),
            transform(run, 2, location(4, 1))
        ]
    ),
    ?assertMatch(
        {error, {backend_error, ambiguous_call, #{}}},
        Result
    ),
    {error, Diagnostic} = Result,
    Details = catena_backend_error:details(Diagnostic),
    ?assertEqual(duplicate_or_overloaded_callable,
        maps:get(reason, Details)),
    ?assertEqual(2, length(maps:get(candidates, Details))).

call_arity_mismatch_has_call_and_declaration_locations_test() ->
    DeclarationLocation = location(2, 1),
    CallLocation = location(6, 9),
    {ok, Inventory} = catena_call_resolution:build(
        arity_calls,
        [],
        [transform(combine, 2, DeclarationLocation)]
    ),
    Context = catena_backend_error:context(
        call_resolution,
        call,
        {app, {var, combine, CallLocation}, [], CallLocation},
        #{module => arity_calls, location => CallLocation}
    ),
    Result = catena_call_resolution:resolve_transform(
        combine,
        1,
        Inventory,
        Context
    ),
    ?assertMatch(
        {error, {backend_error, arity_mismatch, #{}}},
        Result
    ),
    {error, Diagnostic} = Result,
    Details = catena_backend_error:details(Diagnostic),
    ?assertEqual(2, maps:get(expected_arity, Details)),
    ?assertEqual(1, maps:get(actual_arity, Details)),
    ?assertEqual(CallLocation, maps:get(location, Details)),
    ?assertEqual(DeclarationLocation,
        maps:get(declaration_location, Details)).

unknown_callable_is_rejected_test() ->
    {ok, Inventory} = catena_call_resolution:build(
        unresolved_calls,
        [],
        [transform(known, 0, location(2, 1))]
    ),
    Context = #{
        stage => call_resolution,
        module => unresolved_calls,
        location => location(4, 3)
    },
    ?assertMatch(
        {error, {backend_error, unresolved_call, #{}}},
        catena_call_resolution:resolve_transform(
            missing,
            1,
            Inventory,
            Context
        )
    ).

transform(Name, Arity, Location) ->
    Patterns = [
        {pat_var, list_to_atom("arg" ++ integer_to_list(Index)), Location}
        || Index <- lists:seq(1, Arity)
    ],
    {transform_decl,
        Name,
        {test_type, Arity},
        [
            {transform_clause,
                Patterns,
                undefined,
                {literal, integer, Arity, Location},
                Location}
        ],
        Location}.

location(Line, Column) ->
    {location, Line, Column}.
