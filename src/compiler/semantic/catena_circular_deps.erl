%%%-------------------------------------------------------------------
%%% @doc Catena Circular Dependency Detection Module
%%%
%%% This module detects circular dependencies in the module import graph.
%%% It builds a dependency graph from module imports and uses depth-first
%%% search to detect cycles, providing clear error messages with the
%%% full cycle path.
%%%
%%% == Dependency Graph Structure ==
%%%
%%% The dependency graph is represented as a map where keys are module
%%% names and values are lists of modules that the key module depends on:
%%%
%%% ```
%%% #{
%%%   module_a => [module_b, module_c],
%%%   module_b => [module_c],
%%%   module_c => []
%%% }
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_circular_deps).

%% API
-export([
    detect_cycles/1,
    build_dependency_graph/1,
    format_cycle_error/1,
    get_module_dependencies/2,
    topological_sort/1
]).

%% Types
-type module_name() :: atom().
-type dependency_graph() :: #{module_name() => [module_name()]}.
-type cycle_path() :: [module_name()].
-type cycle_error() :: {cycle, cycle_path()}.

%%%=============================================================================
%%% Dependency Graph Construction
%%%=============================================================================

%% @doc Build a dependency graph from a list of module symbol tables.
%% Each module's dependencies are extracted from its import list.
%% @param SymbolTables Map of module_name -> module_info
%% @return Dependency graph where keys are modules and values are lists of dependencies
-spec build_dependency_graph(#{module_name() => map()}) -> dependency_graph().
build_dependency_graph(SymbolTables) ->
    maps:map(fun(_ModuleName, ModuleInfo) ->
        get_module_dependencies(ModuleInfo, SymbolTables)
    end, SymbolTables).

%% @doc Extract the list of modules that this module depends on from its imports.
%% @param ModuleInfo The module information map
%% @param SymbolTables All symbol tables (for validation)
%% @return List of module names that this module imports
-spec get_module_dependencies(map(), #{module_name() => map()}) -> [module_name()].
get_module_dependencies(ModuleInfo, SymbolTables) ->
    Imports = maps:get(imports, ModuleInfo, []),
    lists:filtermap(fun(Import) ->
        case Import of
            {import, ModuleName, _Items, _Qualified, _Alias, _Location} ->
                %% Only include dependencies that exist in symbol tables
                case maps:is_key(ModuleName, SymbolTables) of
                    true -> {true, ModuleName};
                    false -> false
                end;
            _ ->
                false
        end
    end, Imports).

%%%=============================================================================
%%% Cycle Detection
%%%=============================================================================

%% @doc Detect cycles in the dependency graph.
%% Returns a list of all cycles found, or an empty list if no cycles exist.
%% @param Graph The dependency graph
%% @return List of cycle paths (each path is a list of module names)
-spec detect_cycles(dependency_graph()) -> [cycle_path()].
detect_cycles(Graph) ->
    Modules = maps:keys(Graph),
    detect_cycles(Modules, Graph, #{}, []).

%% @doc Detect cycles using depth-first search with coloring.
%% @param Modules List of modules to visit
%% @param Graph The dependency graph
%% @param Visited Map of module -> visit status (unvisited, visiting, visited)
%% @param Cycles Accumulator for found cycles
-spec detect_cycles([module_name()], dependency_graph(), #{module_name() => unvisited | visiting | visited}, [cycle_path()]) -> [cycle_path()].
detect_cycles([], _Graph, _Visited, Cycles) ->
    lists:reverse(Cycles);
detect_cycles([Module | Rest], Graph, Visited, Cycles) ->
    case maps:get(Module, Visited, unvisited) of
        visited ->
            %% Already fully processed this module
            detect_cycles(Rest, Graph, Visited, Cycles);
        visiting ->
            %% Found a cycle - extract the cycle path
            %% This shouldn't happen here as we handle cycles in dfs_visit
            detect_cycles(Rest, Graph, Visited, Cycles);
        unvisited ->
            %% Start DFS from this module
            case dfs_visit(Module, Graph, Visited#{Module => visiting}, []) of
                {ok, NewVisited} ->
                    detect_cycles(Rest, Graph, NewVisited, Cycles);
                {cycle, CyclePath, NewVisited} ->
                    detect_cycles(Rest, Graph, NewVisited, [CyclePath | Cycles])
            end
    end.

%% @doc Depth-first search visit, detecting back edges (cycles).
%% @param Module Current module being visited
%% @param Graph The dependency graph
%% @param Visited Visit status map
%% @param Path Current path from root
-spec dfs_visit(module_name(), dependency_graph(), #{module_name() => unvisited | visiting | visited}, [module_name()]) ->
    {ok, #{module_name() => unvisited | visiting | visited}} |
    {cycle, cycle_path(), #{module_name() => unvisited | visiting | visited}}.
dfs_visit(Module, Graph, Visited, Path) ->
    Dependencies = maps:get(Module, Graph, []),
    dfs_visit_dependencies(Dependencies, Graph, Visited, [Module | Path]).

%% @doc Visit all dependencies of a module.
-spec dfs_visit_dependencies([module_name()], dependency_graph(), #{module_name() => unvisited | visiting | visited}, [module_name()]) ->
    {ok, #{module_name() => unvisited | visiting | visited}} |
    {cycle, cycle_path(), #{module_name() => unvisited | visiting | visited}}.
dfs_visit_dependencies([], _Graph, Visited, Path) ->
    %% Mark the module at the top of the path as visited
    [Module | _] = Path,
    {ok, Visited#{Module => visited}};
dfs_visit_dependencies([Dep | Rest], Graph, Visited, Path) ->
    case maps:get(Dep, Visited, unvisited) of
        visited ->
            %% Dependency already fully processed - no cycle
            dfs_visit_dependencies(Rest, Graph, Visited, Path);
        visiting ->
            %% Found a cycle - Dep is currently being visited
            %% Extract the cycle path from the current path
            CyclePath = extract_cycle_path(Dep, Path),
            {cycle, CyclePath, Visited};
        unvisited ->
            %% Visit the dependency
            case dfs_visit(Dep, Graph, Visited#{Dep => visiting}, Path) of
                {ok, NewVisited} ->
                    dfs_visit_dependencies(Rest, Graph, NewVisited, Path);
                {cycle, _CyclePath, _NewVisited} = CycleError ->
                    CycleError
            end
    end.

%% @doc Extract the cycle path from the current DFS path.
%% @param StartModule The module where the cycle was detected
%% @param Path The current DFS path
%% @return The cycle path as a list of modules
-spec extract_cycle_path(module_name(), [module_name()]) -> cycle_path().
extract_cycle_path(StartModule, Path) ->
    %% Find the start module in the path and extract from there
    extract_cycle_path(StartModule, Path, []).

extract_cycle_path(_StartModule, [], Acc) ->
    lists:reverse(Acc);
extract_cycle_path(StartModule, [Module | Rest], Acc) ->
    case Module of
        StartModule ->
            %% Found the start - the rest of the path is the cycle
            lists:reverse([Module | Acc]);
        _ ->
            extract_cycle_path(StartModule, Rest, [Module | Acc])
    end.

%%%=============================================================================
%%% Topological Sort
%%%=============================================================================

%% @doc Perform topological sort on the dependency graph.
%% Returns an ordered list of modules such that all dependencies appear
%% before the modules that depend on them.
%% @param Graph The dependency graph
%% @return {ok, OrderedModules} | {error, {cycle, CyclePath}}
-spec topological_sort(dependency_graph()) -> {ok, [module_name()]} | {error, cycle_error()}.
topological_sort(Graph) ->
    case detect_cycles(Graph) of
        [] ->
            %% No cycles - perform Kahn's algorithm
            {ok, kahn_sort(Graph)};
        [Cycle | _] ->
            %% Return the first cycle found
            {error, {cycle, Cycle}}
    end.

%% @doc Kahn's algorithm for topological sorting.
%% @param Graph The dependency graph (where A => [B, C] means A depends on B and C)
%% @return Ordered list of modules (dependencies before dependents)
-spec kahn_sort(dependency_graph()) -> [module_name()].
kahn_sort(Graph) ->
    %% Reverse the graph: create edges from dependencies to dependents
    %% If A depends on B, we want an edge B -> A for Kahn's algorithm
    ReversedGraph = reverse_graph(Graph),

    %% Calculate in-degrees (number of incoming edges in the reversed graph)
    AllModules = maps:keys(Graph),
    InDegrees = lists:foldl(fun(Module, Acc) ->
        Deps = maps:get(Module, ReversedGraph, []),
        lists:foldl(fun(Dep, AccIn) ->
            maps:update_with(Dep, fun(V) -> V + 1 end, 1, AccIn)
        end, Acc, Deps)
    end, maps:from_keys(AllModules, 0), AllModules),

    %% Find all modules with in-degree 0
    ZeroInDegree = [M || M <- AllModules, maps:get(M, InDegrees) =:= 0],

    %% Process the queue
    kahn_process(ZeroInDegree, ReversedGraph, InDegrees, []).

kahn_process([], _Graph, _InDegrees, Result) ->
    lists:reverse(Result);
kahn_process([Module | Rest], Graph, InDegrees, Result) ->
    %% Remove this module from the graph and update in-degrees
    Deps = maps:get(Module, Graph, []),
    NewInDegrees = lists:foldl(fun(Dep, Acc) ->
        Acc#{Dep => maps:get(Dep, Acc) - 1}
    end, InDegrees, Deps),

    %% Find newly zero-in-degree modules
    NewZeros = [D || D <- Deps, maps:get(D, NewInDegrees) =:= 0],

    %% Continue with the rest plus new zeros
    kahn_process(Rest ++ NewZeros, Graph, NewInDegrees, [Module | Result]).

%% @doc Reverse a dependency graph.
%% If the original graph has A => [B, C] (A depends on B and C),
%% the reversed graph has B => [A], C => [A] (B and C are depended on by A).
-spec reverse_graph(dependency_graph()) -> dependency_graph().
reverse_graph(Graph) ->
    maps:fold(fun(Module, Deps, Acc) ->
        lists:foldl(fun(Dep, AccIn) ->
            maps:update_with(Dep, fun(V) -> [Module | V] end, [Module], AccIn)
        end, Acc, Deps)
    end, maps:from_keys(maps:keys(Graph), []), Graph).

%%%=============================================================================
%%% Error Formatting
%%%=============================================================================

%% @doc Format a cycle error as a human-readable string.
%% @param CycleError The cycle error tuple
%% @return Formatted error message
-spec format_cycle_error(cycle_error()) -> iolist().
format_cycle_error({cycle, CyclePath}) ->
    CycleString = format_cycle_path(CyclePath),
    ["Circular dependency detected: ", CycleString, "\n",
     "Modules in cycle:\n",
     format_cycle_modules(CyclePath)].

%% @doc Format a cycle path as "A -> B -> C -> A".
-spec format_cycle_path(cycle_path()) -> iolist().
format_cycle_path([]) ->
    [];
format_cycle_path([Module]) ->
    atom_to_list(Module);
format_cycle_path(Path) ->
    %% Format as "A -> B -> C -> A"
    StringList = [atom_to_list(M) || M <- Path],
    lists:join(" -> ", StringList).

%% @doc Format modules in a cycle with indentation.
-spec format_cycle_modules(cycle_path()) -> iolist().
format_cycle_modules([]) ->
    [];
format_cycle_modules(Path) ->
    UniqueModules = lists:usort(Path),
    [io_lib:format("  - ~s~n", [M]) || M <- UniqueModules].
