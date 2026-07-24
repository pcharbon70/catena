%%%-------------------------------------------------------------------
%%% @doc Deterministic dependency planning for validated Catena modules.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_module_linkage).

-export([
    plan/1,
    dependency_graph/1,
    artifact_dependencies/2
]).

-spec plan(#{atom() => term()}) -> {ok, map()} | {error, term()}.
plan(Modules) when is_map(Modules) ->
    Names = maps:keys(Modules),
    case catena_module_identity:validate_unique(Names) of
        ok ->
            case dependency_graph(Modules) of
                {ok, Graph, ImportLocations} ->
                    case deterministic_order(Graph) of
                        {ok, Order} ->
                            {ok, #{
                                modules => Modules,
                                graph => Graph,
                                order => Order,
                                import_locations => ImportLocations
                            }};
                        {error, {dependency_cycle, Cycle}} ->
                            {error, #{
                                reason => dependency_cycle,
                                cycle => Cycle,
                                locations => [
                                    maps:get(
                                        {From, To},
                                        ImportLocations,
                                        undefined
                                    )
                                    || {From, To} <- cycle_edges(Cycle)
                                ]
                            }}
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;
plan(Modules) ->
    {error, {invalid_module_set, Modules}}.

-spec dependency_graph(#{atom() => term()}) ->
    {ok, #{atom() => [atom()]}, map()} | {error, term()}.
dependency_graph(Modules) ->
    maps:fold(
        fun
            (_Name, _AST, {error, _} = Error) ->
                Error;
            (Name, AST, {ok, Graph, Locations}) ->
                case module_imports(AST) of
                    {ok, Imports} ->
                        build_module_edges(
                            Name,
                            Imports,
                            Modules,
                            Graph,
                            Locations
                        );
                    {error, _} = Error ->
                        Error
                end
        end,
        {ok, #{}, #{}},
        Modules
    ).

-spec artifact_dependencies([term()], [map()]) -> [map()].
artifact_dependencies(Imports, RuntimeDependencies) ->
    ModuleDependencies = [
        #{
            kind => catena_module,
            source_module => Module,
            runtime_module => Module
        }
        || {import, Module, _, _, _, _} <- Imports
    ],
    lists:usort(ModuleDependencies ++ RuntimeDependencies).

module_imports({module, _Name, _Exports, Imports, _Decls, _Loc})
  when is_list(Imports) ->
    {ok, Imports};
module_imports(Other) ->
    {error, {invalid_module_ast, Other}}.

build_module_edges(Name, Imports, Modules, Graph, Locations) ->
    lists:foldl(
        fun
            (_Import, {error, _} = Error) ->
                Error;
            ({import, Dependency, _, _, _, Location}, {ok, G, L}) ->
                case maps:is_key(Dependency, Modules) of
                    true ->
                        Existing = maps:get(Name, G, []),
                        {ok,
                            G#{Name => lists:usort([Dependency | Existing])},
                            L#{{Name, Dependency} => Location}};
                    false ->
                        {error, #{
                            reason => missing_dependency,
                            module => Name,
                            dependency => Dependency,
                            location => Location
                        }}
                end;
            (_Other, Acc) ->
                Acc
        end,
        {ok, Graph#{Name => maps:get(Name, Graph, [])}, Locations},
        Imports
    ).

deterministic_order(Graph) ->
    deterministic_order(lists:sort(maps:keys(Graph)), Graph, #{}, []).

deterministic_order([], _Graph, _Marks, Order) ->
    {ok, Order};
deterministic_order([Module | Rest], Graph, Marks, Order) ->
    case visit(Module, Graph, Marks, Order, []) of
        {ok, NewMarks, NewOrder} ->
            deterministic_order(Rest, Graph, NewMarks, NewOrder);
        {error, _} = Error ->
            Error
    end.

visit(Module, Graph, Marks, Order, Stack) ->
    case maps:get(Module, Marks, unvisited) of
        visited ->
            {ok, Marks, Order};
        visiting ->
            {error, {dependency_cycle, close_cycle(Module, Stack)}};
        unvisited ->
            Marks1 = Marks#{Module => visiting},
            case visit_dependencies(
                lists:sort(maps:get(Module, Graph, [])),
                Graph,
                Marks1,
                Order,
                [Module | Stack]
            ) of
                {ok, Marks2, Order2} ->
                    {ok, Marks2#{Module => visited}, Order2 ++ [Module]};
                {error, _} = Error ->
                    Error
            end
    end.

visit_dependencies([], _Graph, Marks, Order, _Stack) ->
    {ok, Marks, Order};
visit_dependencies([Dependency | Rest], Graph, Marks, Order, Stack) ->
    case visit(Dependency, Graph, Marks, Order, Stack) of
        {ok, Marks1, Order1} ->
            visit_dependencies(Rest, Graph, Marks1, Order1, Stack);
        {error, _} = Error ->
            Error
    end.

close_cycle(Module, Stack) ->
    Path = take_until(Module, Stack, []),
    Path ++ [Module].

take_until(Module, [Module | _], Acc) ->
    [Module | Acc];
take_until(Module, [Head | Rest], Acc) ->
    take_until(Module, Rest, [Head | Acc]).

cycle_edges([_]) ->
    [];
cycle_edges([From, To | Rest]) ->
    [{From, To} | cycle_edges([To | Rest])].
