-module(catena_control_mode_tests).

-include_lib("eunit/include/eunit.hrl").

pure_and_provider_only_are_direct_test() ->
    Source =
        "module ControlDirect\n"
        "effect Console\n"
        "operation read : Int\n"
        "end\n"
        "transform identity x = x\n"
        "transform provider ignored = perform Console.read()\n",
    {ok, Unit} = catena_compile:compile_string_to_unit(Source),
    Modes = catena_compilation_unit:control_modes(Unit),
    ?assertEqual({ok, direct}, catena_control_mode:mode(identity, Modes)),
    ?assertEqual({ok, direct}, catena_control_mode:mode(provider, Modes)),
    {ok, Provider} = catena_control_mode:lookup(provider, Modes),
    ?assertEqual(provider_only, maps:get(reason, Provider)).

explicit_and_automatic_handlers_are_resumable_test() ->
    Explicit =
        "module ControlExplicit\n"
        "effect Choice\n"
        "operation choose : Int\n"
        "end\n"
        "transform run = handle perform Choice.choose() then {\n"
        "  Choice { choose() with k -> resume(k, 1) }\n"
        "}\n",
    Automatic =
        "module ControlAutomatic\n"
        "effect Choice\n"
        "operation choose : Int\n"
        "end\n"
        "transform run = handle perform Choice.choose() then {\n"
        "  Choice { choose() -> 1 }\n"
        "}\n",
    lists:foreach(
        fun(Source) ->
            {ok, Unit} =
                catena_compile:compile_string_to_unit(Source),
            Modes = catena_compilation_unit:control_modes(Unit),
            ?assertEqual(
                {ok, resumable},
                catena_control_mode:mode(run, Modes)
            ),
            ?assert(
                lists:any(
                    fun(Region) ->
                        maps:get(reason, Region) =:=
                            handler_delimiter
                    end,
                    catena_control_mode:regions(run, Modes)
                )
            )
        end,
        [Explicit, Automatic]
    ).

resumability_reaches_recursive_fixed_point_test() ->
    Loc = {location, 1, 1},
    Handle = {
        handle_expr,
        {perform_expr, 'Choice', choose, [], Loc},
        [
            {handler_clause, 'Choice', [
                {operation_case, choose, [],
                    {resumption_binder, k, Loc},
                    {resume_expr, var(k, Loc), var(n, Loc), Loc},
                    Loc}
            ], Loc}
        ],
        Loc
    },
    Declarations = [
        transform(left, [n], app(var(right, Loc), [var(n, Loc)], Loc),
            Loc),
        transform(right, [n],
            {let_expr, [{pat_var, value, Loc}, Handle],
                app(var(left, Loc), [var(value, Loc)], Loc), Loc},
            Loc)
    ],
    Typed = [
        typed_transform(left, 1, Loc),
        typed_transform(right, 1, Loc)
    ],
    {ok, Callables} = catena_call_resolution:build(
        'ControlRecursive',
        [],
        Declarations
    ),
    {ok, Modes} = catena_control_mode:analyze(
        'ControlRecursive',
        Declarations,
        Typed,
        Callables,
        #{}
    ),
    ?assertEqual({ok, resumable}, catena_control_mode:mode(left, Modes)),
    ?assertEqual({ok, resumable}, catena_control_mode:mode(right, Modes)),
    {ok, Left} = catena_control_mode:lookup(left, Modes),
    ?assertEqual({calls_resumable, right}, maps:get(reason, Left)).

higher_order_and_trait_edges_are_conservative_test() ->
    Loc = {location, 1, 1},
    Declarations = [
        transform(invoke, [f, x], app(var(f, Loc), [var(x, Loc)], Loc), Loc),
        transform(mapped, [f, xs],
            app(var(map, Loc), [var(f, Loc), var(xs, Loc)], Loc), Loc)
    ],
    Typed = [
        typed_transform(invoke, 2, Loc),
        typed_transform(mapped, 2, Loc)
    ],
    {ok, Callables} = catena_call_resolution:build(
        'ControlDynamic',
        [],
        Declarations
    ),
    {ok, Modes} = catena_control_mode:analyze(
        'ControlDynamic',
        Declarations,
        Typed,
        Callables,
        #{}
    ),
    ?assertEqual({ok, resumable}, catena_control_mode:mode(invoke, Modes)),
    ?assertEqual({ok, resumable}, catena_control_mode:mode(mapped, Modes)),
    {ok, Invoke} = catena_control_mode:lookup(invoke, Modes),
    ?assert(lists:member(higher_order_call, maps:get(reasons, Invoke))),
    {ok, Mapped} = catena_control_mode:lookup(mapped, Modes),
    ?assert(lists:member(trait_dispatch, maps:get(reasons, Mapped))).

open_effect_rows_are_conservative_test() ->
    Loc = {location, 1, 1},
    Declarations = [
        transform(apply_open, [x], var(x, Loc), Loc)
    ],
    OpenType = catena_types:tfun(
        catena_types:tcon(int),
        catena_types:tcon(int),
        {teffectrow, [], {rowvar, 7}}
    ),
    Typed = [
        {typed_transform, apply_open, OpenType,
            clauses(hd(Declarations)), Loc}
    ],
    {ok, Callables} = catena_call_resolution:build(
        'ControlOpen',
        [],
        Declarations
    ),
    {ok, Modes} = catena_control_mode:analyze(
        'ControlOpen',
        Declarations,
        Typed,
        Callables,
        #{}
    ),
    ?assertEqual(
        {ok, resumable},
        catena_control_mode:mode(apply_open, Modes)
    ),
    {ok, Entry} = catena_control_mode:lookup(apply_open, Modes),
    ?assertEqual(open_effect_row, maps:get(reason, Entry)).

transform(Name, Parameters, Body, Location) ->
    {transform_decl, Name, undefined, [
        {transform_clause,
            [{pat_var, Parameter, Location} || Parameter <- Parameters],
            undefined,
            Body,
            Location}
    ], Location}.

typed_transform(Name, Arity, Location) ->
    Type = lists:foldr(
        fun(_, Acc) ->
            catena_types:tfun(
                catena_types:tcon(int),
                Acc,
                catena_types:empty_effects()
            )
        end,
        catena_types:tcon(int),
        lists:seq(1, Arity)
    ),
    Declaration = transform(Name, lists:seq(1, Arity), var(x, Location),
        Location),
    {typed_transform, Name, Type, clauses(Declaration), Location}.

clauses({transform_decl, _Name, _Type, Clauses, _Location}) ->
    Clauses.

var(Name, Location) ->
    {var, Name, Location}.

app(Function, Arguments, Location) ->
    {app, Function, Arguments, Location}.
