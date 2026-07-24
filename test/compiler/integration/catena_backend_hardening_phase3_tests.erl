-module(catena_backend_hardening_phase3_tests).

-include_lib("eunit/include/eunit.hrl").

direct_and_forward_source_calls_execute_test() ->
    DirectSource =
        "module PhaseThreeDirect\n"
        "export transform run\n"
        "transform increment value = value + 1\n"
        "transform run value = increment value\n",
    with_executable_module(
        DirectSource,
        'PhaseThreeDirect',
        fun() ->
            ?assertEqual(42, 'PhaseThreeDirect':run(41))
        end
    ),
    ForwardSource =
        "module PhaseThreeForward\n"
        "export transform run\n"
        "type Choice = First | Second\n"
        "transform run : Choice -> Choice\n"
        "transform run value = later value\n"
        "type ForwardBoundary = ForwardBoundary\n"
        "transform later : Choice -> Choice\n"
        "transform later value = value\n",
    with_executable_module(
        ForwardSource,
        'PhaseThreeForward',
        fun() ->
            ?assertEqual({'First'}, 'PhaseThreeForward':run({'First'})),
            ?assertEqual({'Second'}, 'PhaseThreeForward':run({'Second'}))
        end
    ).

self_and_mutual_recursion_execute_test() ->
    SelfSource =
        "module PhaseThreeSelf\n"
        "export transform copy\n"
        "type Nat = Zero | Succ Nat\n"
        "transform copy : Nat -> Nat\n"
        "transform copy Zero = Zero\n"
        "transform copy Succ(rest) = Succ (copy rest)\n",
    with_executable_module(
        SelfSource,
        'PhaseThreeSelf',
        fun() ->
            Value = {'Succ', {'Succ', {'Zero'}}},
            ?assertEqual(Value, 'PhaseThreeSelf':copy(Value))
        end
    ),
    MutualSource =
        "module PhaseThreeMutual\n"
        "export transform even_number\n"
        "export transform odd_number\n"
        "type Nat = Zero | Succ Nat\n"
        "type Truth = Yes | No\n"
        "transform even_number : Nat -> Truth\n"
        "transform even_number Zero = Yes\n"
        "transform even_number Succ(rest) = odd_number rest\n"
        "type MutualBoundary = MutualBoundary\n"
        "transform odd_number : Nat -> Truth\n"
        "transform odd_number Zero = No\n"
        "transform odd_number Succ(rest) = even_number rest\n",
    with_executable_module(
        MutualSource,
        'PhaseThreeMutual',
        fun() ->
            Zero = {'Zero'},
            One = {'Succ', Zero},
            Two = {'Succ', One},
            ?assertEqual({'Yes'}, 'PhaseThreeMutual':even_number(Zero)),
            ?assertEqual({'No'}, 'PhaseThreeMutual':even_number(One)),
            ?assertEqual({'Yes'}, 'PhaseThreeMutual':even_number(Two)),
            ?assertEqual({'Yes'}, 'PhaseThreeMutual':odd_number(One))
        end
    ).

named_transform_values_and_closure_parameters_execute_test() ->
    Source =
        "module PhaseThreeHigherOrder\n"
        "export transform run\n"
        "type Bit = Off | On\n"
        "transform invert : Bit -> Bit\n"
        "transform invert Off = On\n"
        "transform invert On = Off\n"
        "type HigherBoundaryA = HigherBoundaryA\n"
        "transform apply_twice : (Bit -> Bit) -> Bit -> Bit\n"
        "transform apply_twice function value = "
            "function (function value)\n"
        "type HigherBoundaryB = HigherBoundaryB\n"
        "transform run : Bit\n"
        "transform run = apply_twice invert On\n",
    with_executable_module(
        Source,
        'PhaseThreeHigherOrder',
        fun() ->
            ?assertEqual({'On'}, 'PhaseThreeHigherOrder':run())
        end
    ).

lambda_let_and_returned_functions_execute_test() ->
    LambdaSource =
        "module PhaseThreeLambda\n"
        "export transform run\n"
        "type Bit = Off | On\n"
        "transform run : Bit\n"
        "transform run = "
            "let local = fn value -> value in local On\n",
    with_executable_module(
        LambdaSource,
        'PhaseThreeLambda',
        fun() ->
            ?assertEqual({'On'}, 'PhaseThreeLambda':run())
        end
    ),
    ReturnedSource =
        "module PhaseThreeReturned\n"
        "export transform run\n"
        "type Bit = Off | On\n"
        "transform invert : Bit -> Bit\n"
        "transform invert Off = On\n"
        "transform invert On = Off\n"
        "type ReturnBoundaryA = ReturnBoundaryA\n"
        "transform choose : Bit -> (Bit -> Bit)\n"
        "transform choose ignored = invert\n"
        "type ReturnBoundaryB = ReturnBoundaryB\n"
        "transform run : Bit\n"
        "transform run = "
            "let selected = choose Off in selected On\n",
    with_executable_module(
        ReturnedSource,
        'PhaseThreeReturned',
        fun() ->
            ?assertEqual({'Off'}, 'PhaseThreeReturned':run())
        end
    ).

constructor_arities_and_tagged_values_execute_test() ->
    Source =
        "module PhaseThreeConstructors\n"
        "export transform run\n"
        "type Bit = Off | On\n"
        "type Box = Box Bit\n"
        "type Pair = Pair Bit Bit\n"
        "transform run = (Off, Box On, Pair Off On)\n",
    with_executable_module(
        Source,
        'PhaseThreeConstructors',
        fun() ->
            ?assertEqual(
                {
                    {'Off'},
                    {'Box', {'On'}},
                    {'Pair', {'Off'}, {'On'}}
                },
                'PhaseThreeConstructors':run()
            )
        end
    ).

invalid_source_targets_fail_before_artifact_success_test() ->
    UnderApplied =
        "module PhaseThreeUnderApplied\n"
        "export transform run\n"
        "type Bit = Off | On\n"
        "type Pair = Pair Bit Bit\n"
        "transform run = Pair Off\n",
    UnderResult = catena_compile:compile_string_to_core(UnderApplied),
    ?assertMatch(
        {error, {backend_error, arity_mismatch, #{}}},
        UnderResult
    ),
    {error, UnderDiagnostic} = UnderResult,
    UnderDetails = catena_backend_error:details(UnderDiagnostic),
    ?assertEqual(constructor, maps:get(callable_kind, UnderDetails)),
    ?assertEqual(2, maps:get(expected_arity, UnderDetails)),
    ?assertEqual(1, maps:get(actual_arity, UnderDetails)),
    ?assertMatch({location, 5, _}, maps:get(location, UnderDetails)),
    ?assertMatch(
        {location, 4, _},
        maps:get(declaration_location, UnderDetails)
    ),
    OverApplied =
        "module PhaseThreeOverApplied\n"
        "export transform run\n"
        "type Bit = Off | On\n"
        "type Pair = Pair Bit Bit\n"
        "transform run = Pair Off On Off\n",
    ?assertMatch(
        {error, {type_error, run, _}},
        catena_compile:compile_string_to_core(OverApplied)
    ),
    Unresolved =
        "module PhaseThreeUnresolved\n"
        "export transform run\n"
        "transform run value = missing value\n",
    ?assertMatch(
        {error, {type_error, run, _}},
        catena_compile:compile_string_to_core(Unresolved)
    ).

with_executable_module(Source, Module, Assertion) ->
    {ok, CoreModule} = catena_compile:compile_string_to_core(Source),
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
