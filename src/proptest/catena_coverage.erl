%% @doc Coverage-guided generation helpers for Phase 7.2.
%%
%% The primary API is callback-driven so it can be used without requiring
%% every property target to be cover-instrumented. When cover data is
%% available, the module can also read line-level execution counts.
-module(catena_coverage).

-export([
    new/1,
    new/2,
    record_case/3,
    uncovered_branches/1,
    prioritized_inputs/1,
    guided_inputs/3,
    guided_generator/3,
    coverage_report/1,
    module_lines/1
]).

-type branch() :: term().
-type coverage_options() :: #{
    modules => [module()],
    exploration_weight => pos_integer(),
    exploitation_weight => pos_integer()
}.
-type coverage_session() :: map().

-define(DEFAULT_EXPLORATION_WEIGHT, 3).
-define(DEFAULT_EXPLOITATION_WEIGHT, 1).
-define(DEFAULT_GUIDED_COUNT, 8).
-define(UNREACHABLE_DISTANCE_THRESHOLD, 8).

-spec new([branch()]) -> coverage_session().
new(Universe) ->
    new(Universe, #{}).

-spec new([branch()], coverage_options()) -> coverage_session().
new(Universe, Options) when is_list(Universe), is_map(Options) ->
    AllowedModules = normalize_modules(maps:get(modules, Options, all)),
    FilteredUniverse = filter_branches(Universe, AllowedModules),
    #{
        universe => ordsets:from_list(FilteredUniverse),
        covered => ordsets:new(),
        branch_hits => #{},
        cases => [],
        modules => AllowedModules,
        exploration_weight => maps:get(exploration_weight, Options, ?DEFAULT_EXPLORATION_WEIGHT),
        exploitation_weight => maps:get(exploitation_weight, Options, ?DEFAULT_EXPLOITATION_WEIGHT)
    }.

-spec record_case(coverage_session(), term(), [branch()]) -> coverage_session().
record_case(Session, Input, Branches) when is_map(Session), is_list(Branches) ->
    AllowedModules = maps:get(modules, Session),
    UniqueBranches = ordsets:from_list(filter_branches(Branches, AllowedModules)),
    Covered0 = maps:get(covered, Session),
    HitCounts0 = maps:get(branch_hits, Session),
    Cases0 = maps:get(cases, Session),
    HitCounts = lists:foldl(
        fun(Branch, Acc) ->
            Acc#{Branch => maps:get(Branch, Acc, 0) + 1}
        end,
        HitCounts0,
        UniqueBranches
    ),
    Session#{
        covered => ordsets:union(Covered0, UniqueBranches),
        branch_hits => HitCounts,
        cases => [#{input => Input, branches => UniqueBranches} | Cases0]
    }.

-spec uncovered_branches(coverage_session()) -> [branch()].
uncovered_branches(Session) ->
    ordsets:subtract(maps:get(universe, Session), maps:get(covered, Session)).

-spec prioritized_inputs(coverage_session()) -> [term()].
prioritized_inputs(Session) ->
    HitCounts = maps:get(branch_hits, Session),
    Uncovered = uncovered_branches(Session),
    Cases = maps:get(cases, Session),
    Ranked = lists:sort(
        fun({ScoreA, InputA}, {ScoreB, InputB}) ->
            case ScoreA =:= ScoreB of
                true -> erlang:phash2(InputA) =< erlang:phash2(InputB);
                false -> ScoreA > ScoreB
            end
        end,
        [{case_score(Case, HitCounts, Uncovered), maps:get(input, Case)} || Case <- Cases]
    ),
    dedupe_preserving_order([Input || {_Score, Input} <- Ranked]).

-spec guided_inputs(coverage_session(), fun((term()) -> term()), pos_integer()) -> [term()].
guided_inputs(Session, MutateFun, Count) when is_function(MutateFun, 1), is_integer(Count), Count > 0 ->
    Inputs = prioritized_inputs(Session),
    Candidates = lists:sublist(Inputs, Count),
    [MutateFun(Input) || Input <- Candidates];
guided_inputs(_Session, _MutateFun, Count) ->
    erlang:error({badarg, {guided_count, Count}}).

-spec guided_generator(catena_gen:generator(term()), coverage_session(), fun((term()) -> term())) ->
    catena_gen:generator(term()).
guided_generator(BaseGenerator, Session, MutateFun) ->
    Candidates = guided_inputs(Session, MutateFun, ?DEFAULT_GUIDED_COUNT),
    case Candidates of
        [] ->
            BaseGenerator;
        _ ->
            ExplorationWeight = maps:get(exploration_weight, Session, ?DEFAULT_EXPLORATION_WEIGHT),
            ExploitationWeight = maps:get(exploitation_weight, Session, ?DEFAULT_EXPLOITATION_WEIGHT),
            catena_gen:gen_frequency([
                {ExplorationWeight, catena_gen:elements(Candidates)},
                {ExploitationWeight, BaseGenerator}
            ])
    end.

-spec coverage_report(coverage_session()) -> map().
coverage_report(Session) ->
    Universe = maps:get(universe, Session),
    Covered = maps:get(covered, Session),
    Total = length(Universe),
    CoveredCount = length(Covered),
    Percent = case Total of
        0 -> 100.0;
        _ -> (CoveredCount * 100.0) / Total
    end,
    Uncovered = uncovered_branches(Session),
    #{
        total_branches => Total,
        covered_branches => CoveredCount,
        uncovered_branches => Uncovered,
        suspected_unreachable_branches => suspected_unreachable(Uncovered, Covered),
        coverage_percent => Percent,
        branch_hits => maps:get(branch_hits, Session),
        prioritized_inputs => prioritized_inputs(Session)
    }.

-spec module_lines(module()) -> {ok, [{term(), non_neg_integer()}]} | {error, term()}.
module_lines(Module) when is_atom(Module) ->
    ensure_cover_started(),
    read_cover_lines(Module).

read_cover_lines(Module) ->
    try cover:analyse(Module, coverage, line) of
        {ok, Lines} -> {ok, normalize_cover_lines(Lines)};
        {error, _} = Error -> Error;
        Error -> {error, Error}
    catch
        _:Reason -> {error, Reason}
    end.

normalize_cover_lines(Lines) when is_list(Lines) ->
    lists:map(
        fun({{_Module, Line}, Hits}) -> {Line, Hits};
           (Other) -> {Other, 0}
        end,
        Lines
    ).

case_score(Case, HitCounts, Uncovered) ->
    Branches = maps:get(branches, Case),
    NoveltyScore = lists:sum([
        1.0 / max(maps:get(Branch, HitCounts, 1), 1)
        || Branch <- Branches
    ]),
    ProximityScore = lists:sum([
        proximity_score(Branch, Uncovered)
        || Branch <- Branches
    ]),
    NoveltyScore + ProximityScore.

proximity_score(_Branch, []) ->
    0.0;
proximity_score(Branch, Uncovered) ->
    case [branch_distance(Branch, Candidate) || Candidate <- Uncovered, comparable_branches(Branch, Candidate)] of
        [] ->
            0.0;
        Distances ->
            1.0 / (1.0 + lists:min(Distances))
    end.

suspected_unreachable(Uncovered, Covered) ->
    [Branch ||
        Branch <- Uncovered,
        comparable_with_any(Branch, Covered),
        min_distance_to_covered(Branch, Covered) > ?UNREACHABLE_DISTANCE_THRESHOLD].

comparable_with_any(Branch, Covered) ->
    lists:any(fun(Candidate) -> comparable_branches(Branch, Candidate) end, Covered).

min_distance_to_covered(Branch, Covered) ->
    Distances = [branch_distance(Branch, Candidate) || Candidate <- Covered, comparable_branches(Branch, Candidate)],
    case Distances of
        [] -> infinity;
        _ -> lists:min(Distances)
    end.

comparable_branches({Module, LineA}, {Module, LineB}) when is_integer(LineA), is_integer(LineB) ->
    true;
comparable_branches(_, _) ->
    false.

branch_distance({Module, LineA}, {Module, LineB}) ->
    abs(LineA - LineB).

ensure_cover_started() ->
    case cover:start() of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok;
        {error, _} -> ok
    end.

normalize_modules(all) ->
    all;
normalize_modules(Modules) when is_list(Modules) ->
    ordsets:from_list([Module || Module <- Modules, is_atom(Module)]);
normalize_modules(Other) ->
    erlang:error({badarg, {modules, Other}}).

filter_branches(Branches, all) ->
    Branches;
filter_branches(Branches, AllowedModules) ->
    [Branch || Branch <- Branches, branch_allowed(Branch, AllowedModules)].

branch_allowed({Module, _}, AllowedModules) when is_atom(Module) ->
    ordsets:is_element(Module, AllowedModules);
branch_allowed(_Branch, _AllowedModules) ->
    true.

dedupe_preserving_order(Values) ->
    dedupe_preserving_order(Values, sets:new(), []).

dedupe_preserving_order([], _Seen, Acc) ->
    lists:reverse(Acc);
dedupe_preserving_order([Value | Rest], Seen, Acc) ->
    case sets:is_element(Value, Seen) of
        true ->
            dedupe_preserving_order(Rest, Seen, Acc);
        false ->
            dedupe_preserving_order(Rest, sets:add_element(Value, Seen), [Value | Acc])
    end.
