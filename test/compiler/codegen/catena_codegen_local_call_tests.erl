-module(catena_codegen_local_call_tests).

-include_lib("eunit/include/eunit.hrl").

direct_and_forward_calls_compile_and_execute_test() ->
    Location = location(2, 1),
    Module =
        {module,
            forward_calls,
            [{run, 1}],
            [],
            [
                transform(
                    run,
                    [x],
                    app(later, [{var, x, Location}], Location),
                    Location
                ),
                transform(
                    later,
                    [x],
                    {binary_op, '+',
                        {var, x, Location},
                        {literal, integer, 1, Location},
                        Location},
                    location(4, 1)
                )
            ],
            location(1, 1)},
    {ok, CoreModule} = catena_codegen_module:generate_module(Module),
    [{_RunName, RunDefinition} | _] = cerl:module_defs(CoreModule),
    RunBody = cerl:fun_body(RunDefinition),
    ?assertEqual(apply, cerl:type(RunBody)),
    ?assert(cerl:is_c_fname(cerl:apply_op(RunBody))),
    ?assertEqual(later, cerl:fname_id(cerl:apply_op(RunBody))),
    with_loaded_module(CoreModule, fun() ->
        ?assertEqual(42, forward_calls:run(41))
    end).

self_recursion_compiles_and_executes_test() ->
    Location = location(2, 1),
    N = {var, n, Location},
    Body =
        {if_expr,
            {binary_op, '=:=', N, {literal, integer, 0, Location}, Location},
            {literal, integer, 1, Location},
            {binary_op, '*',
                N,
                app(
                    factorial,
                    [
                        {binary_op, '-',
                            N,
                            {literal, integer, 1, Location},
                            Location}
                    ],
                    Location
                ),
                Location},
            Location},
    Module =
        {module,
            self_recursive,
            [{factorial, 1}],
            [],
            [transform(factorial, [n], Body, Location)],
            location(1, 1)},
    {ok, CoreModule} = catena_codegen_module:generate_module(Module),
    with_loaded_module(CoreModule, fun() ->
        ?assertEqual(120, self_recursive:factorial(5))
    end).

mutual_recursion_compiles_and_executes_test() ->
    Location = location(2, 1),
    N = {var, n, Location},
    Decrement =
        {binary_op, '-', N, {literal, integer, 1, Location}, Location},
    IsZero =
        {binary_op, '=:=', N, {literal, integer, 0, Location}, Location},
    EvenBody =
        {if_expr,
            IsZero,
            {literal, bool, true, Location},
            app(odd_number, [Decrement], Location),
            Location},
    OddBody =
        {if_expr,
            IsZero,
            {literal, bool, false, Location},
            app(even_number, [Decrement], Location),
            Location},
    Module =
        {module,
            mutual_recursive,
            [{even_number, 1}, {odd_number, 1}],
            [],
            [
                transform(even_number, [n], EvenBody, Location),
                transform(odd_number, [n], OddBody, location(4, 1))
            ],
            location(1, 1)},
    {ok, CoreModule} = catena_codegen_module:generate_module(Module),
    with_loaded_module(CoreModule, fun() ->
        ?assertEqual(true, mutual_recursive:even_number(10)),
        ?assertEqual(true, mutual_recursive:odd_number(9)),
        ?assertEqual(false, mutual_recursive:even_number(7))
    end).

local_call_arity_mismatch_fails_before_core_success_test() ->
    CallLocation = location(3, 9),
    DeclarationLocation = location(5, 1),
    Module =
        {module,
            invalid_arity,
            [{run, 0}],
            [],
            [
                transform(
                    run,
                    [],
                    app(combine, [{literal, integer, 1, CallLocation}],
                        CallLocation),
                    location(2, 1)
                ),
                transform(
                    combine,
                    [left, right],
                    {var, left, DeclarationLocation},
                    DeclarationLocation
                )
            ],
            location(1, 1)},
    Result = catena_codegen_module:generate_module(Module),
    ?assertMatch(
        {error, {backend_error, arity_mismatch, #{}}},
        Result
    ),
    {error, Diagnostic} = Result,
    Details = catena_backend_error:details(Diagnostic),
    ?assertEqual(CallLocation, maps:get(location, Details)),
    ?assertEqual(DeclarationLocation,
        maps:get(declaration_location, Details)).

unresolved_local_call_fails_before_core_success_test() ->
    Location = location(3, 5),
    Module =
        {module,
            unresolved_local,
            [{run, 0}],
            [],
            [
                transform(
                    run,
                    [],
                    app(missing, [], Location),
                    location(2, 1)
                )
            ],
            location(1, 1)},
    ?assertMatch(
        {error, {backend_error, unresolved_call, #{}}},
        catena_codegen_module:generate_module(Module)
    ).

transform(Name, Parameters, Body, Location) ->
    {transform,
        Name,
        [{pat_var, Parameter, Location} || Parameter <- Parameters],
        Body,
        Location}.

app(Name, Arguments, Location) ->
    {app, {var, Name, Location}, Arguments, Location}.

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
