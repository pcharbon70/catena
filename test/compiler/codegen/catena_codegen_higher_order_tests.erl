-module(catena_codegen_higher_order_tests).

-include_lib("eunit/include/eunit.hrl").

lambda_parameters_and_top_level_values_execute_test() ->
    Location = location(2, 1),
    ApplyTwiceBody =
        app(
            {var, function, Location},
            [
                app(
                    {var, function, Location},
                    [{var, value, Location}],
                    Location
                )
            ],
            Location
        ),
    IncrementBody =
        {binary_op, '+',
            {var, value, Location},
            {literal, integer, 1, Location},
            Location},
    RunBody =
        app(
            apply_twice,
            [
                {var, increment, Location},
                {literal, integer, 40, Location}
            ],
            Location
        ),
    Module =
        {module,
            higher_order_values,
            [{run, 0}],
            [],
            [
                transform(apply_twice, [function, value], ApplyTwiceBody,
                    Location),
                transform(increment, [value], IncrementBody, location(4, 1)),
                transform(run, [], RunBody, location(6, 1))
            ],
            location(1, 1)},
    {ok, CoreModule} = catena_codegen_module:generate_module(Module),
    with_loaded_module(CoreModule, fun() ->
        ?assertEqual(42, higher_order_values:run())
    end).

returned_and_let_bound_functions_execute_test() ->
    Location = location(2, 1),
    IncrementBody =
        {binary_op, '+',
            {var, value, Location},
            {literal, integer, 1, Location},
            Location},
    ReturnBody = {var, increment, Location},
    LetLambda =
        {lambda,
            [{pat_var, value, Location}],
            {binary_op, '*',
                {var, value, Location},
                {literal, integer, 2, Location},
                Location},
            Location},
    RunReturned =
        app(
            app(return_increment, [], Location),
            [{literal, integer, 9, Location}],
            Location
        ),
    RunLet =
        {let_expr,
            [
                {
                    {pat_var, local_function, Location},
                    LetLambda
                }
            ],
            app(
                local_function,
                [{literal, integer, 6, Location}],
                Location
            ),
            Location},
    Module =
        {module,
            returned_functions,
            [{run_returned, 0}, {run_let, 0}],
            [],
            [
                transform(increment, [value], IncrementBody, Location),
                transform(return_increment, [], ReturnBody, location(4, 1)),
                transform(run_returned, [], RunReturned, location(6, 1)),
                transform(run_let, [], RunLet, location(8, 1))
            ],
            location(1, 1)},
    {ok, CoreModule} = catena_codegen_module:generate_module(Module),
    with_loaded_module(CoreModule, fun() ->
        ?assertEqual(10, returned_functions:run_returned()),
        ?assertEqual(12, returned_functions:run_let())
    end).

runtime_parameter_shadows_top_level_transform_test() ->
    Location = location(2, 1),
    ApplyBody =
        app(
            {var, callback, Location},
            [{literal, integer, 5, Location}],
            Location
        ),
    TopLevelCallback =
        {literal, integer, 999, Location},
    RunBody =
        app(
            apply_callback,
            [
                {lambda,
                    [{pat_var, value, Location}],
                    {binary_op, '+',
                        {var, value, Location},
                        {literal, integer, 2, Location},
                        Location},
                    Location}
            ],
            Location
        ),
    Module =
        {module,
            shadowed_callable,
            [{run, 0}],
            [],
            [
                transform(callback, [], TopLevelCallback, Location),
                transform(apply_callback, [callback], ApplyBody,
                    location(4, 1)),
                transform(run, [], RunBody, location(6, 1))
            ],
            location(1, 1)},
    {ok, CoreModule} = catena_codegen_module:generate_module(Module),
    with_loaded_module(CoreModule, fun() ->
        ?assertEqual(7, shadowed_callable:run())
    end).

constructor_values_preserve_tag_and_all_fields_test() ->
    Location = location(2, 1),
    TypeDeclaration =
        {type_decl,
            'Value',
            [],
            [
                {constructor, 'Empty', [], location(2, 14)},
                {constructor, 'Single', [{type_con, 'Int'}],
                    location(2, 22)},
                {constructor, 'Pair',
                    [{type_con, 'Int'}, {type_con, 'Int'}],
                    location(2, 31)}
            ],
            [],
            Location},
    RunBody =
        {tuple_expr,
            [
                {constructor, 'Empty', [], Location},
                {constructor, 'Single',
                    [{literal, integer, 7, Location}], Location},
                {constructor, 'Pair',
                    [
                        {literal, integer, 3, Location},
                        {literal, integer, 4, Location}
                    ],
                    Location}
            ],
            Location},
    Module =
        {module,
            constructor_values,
            [{run, 0}],
            [],
            [
                TypeDeclaration,
                transform(run, [], RunBody, location(4, 1))
            ],
            location(1, 1)},
    {ok, CoreModule} = catena_codegen_module:generate_module(Module),
    with_loaded_module(CoreModule, fun() ->
        ?assertEqual(
            {{'Empty'}, {'Single', 7}, {'Pair', 3, 4}},
            constructor_values:run()
        )
    end).

constructor_under_and_over_application_are_rejected_test_() ->
    [
        {"under application",
            ?_test(assert_constructor_arity_error(
                {constructor, 'Pair',
                    [{literal, integer, 1, location(5, 5)}],
                    location(5, 5)},
                2,
                1
            ))},
        {"over application",
            ?_test(assert_constructor_arity_error(
                {constructor, 'Pair',
                    [
                        {literal, integer, 1, location(5, 5)},
                        {literal, integer, 2, location(5, 5)},
                        {literal, integer, 3, location(5, 5)}
                    ],
                    location(5, 5)},
                2,
                3
            ))}
    ].

assert_constructor_arity_error(Constructor, Expected, Actual) ->
    Location = location(2, 1),
    TypeDeclaration =
        {type_decl,
            'PairValue',
            [],
            [
                {constructor, 'Pair',
                    [{type_con, 'Int'}, {type_con, 'Int'}],
                    Location}
            ],
            [],
            Location},
    Module =
        {module,
            invalid_constructor_arity,
            [{run, 0}],
            [],
            [
                TypeDeclaration,
                transform(run, [], Constructor, location(4, 1))
            ],
            location(1, 1)},
    Result = catena_codegen_module:generate_module(Module),
    ?assertMatch(
        {error, {backend_error, arity_mismatch, #{}}},
        Result
    ),
    {error, Diagnostic} = Result,
    Details = catena_backend_error:details(Diagnostic),
    ?assertEqual(constructor, maps:get(callable_kind, Details)),
    ?assertEqual(Expected, maps:get(expected_arity, Details)),
    ?assertEqual(Actual, maps:get(actual_arity, Details)).

transform(Name, Parameters, Body, Location) ->
    {transform,
        Name,
        [{pat_var, Parameter, Location} || Parameter <- Parameters],
        Body,
        Location}.

app(Name, Arguments, Location) when is_atom(Name) ->
    app({var, Name, Location}, Arguments, Location);
app(Function, Arguments, Location) ->
    {app, Function, Arguments, Location}.

with_loaded_module(CoreModule, Assertion) ->
    Module = cerl:atom_val(cerl:module_name(CoreModule)),
    {ok, Module, Binary, _Warnings} = compile_core(CoreModule),
    unload(Module),
    try
        {module, Module} = code:load_binary(
            Module,
            atom_to_list(Module) ++ ".core",
            Binary
        ),
        Assertion()
    after
        unload(Module)
    end.

compile_core(CoreModule) ->
    compile:forms(
        CoreModule,
        [from_core, binary, return_errors, return_warnings]
    ).

unload(Module) ->
    code:purge(Module),
    code:delete(Module).

location(Line, Column) ->
    {location, Line, Column}.
