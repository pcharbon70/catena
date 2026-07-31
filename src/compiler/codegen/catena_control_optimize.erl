%%%-------------------------------------------------------------------
%%% @doc Validated, semantics-preserving selective-CPS simplifications.
%%%
%%% Only transformations already proven equivalent by the backend are
%%% admitted here: `return` is a codegen no-op, and a direct-callee bridge
%%% carries explicit `direct_callee` proof. Unknown/open capabilities remain
%%% untouched and therefore fail closed exactly as they did before this pass.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_control_optimize).

-export([optimize/1, optimize/2]).

-type report() :: #{
    enabled := boolean(),
    return_wrappers_eliminated := non_neg_integer(),
    direct_bridges_collapsed := non_neg_integer(),
    nodes_before := non_neg_integer(),
    nodes_after := non_neg_integer()
}.

-export_type([report/0]).

-spec optimize(catena_control_ir:ir()) ->
    {ok, catena_control_ir:ir(), report()} | {error, term()}.
optimize(IR) ->
    optimize(IR, #{}).

-spec optimize(catena_control_ir:ir(), map()) ->
    {ok, catena_control_ir:ir(), report()} | {error, term()}.
optimize(IR, Options) when is_map(Options) ->
    case catena_control_ir:is_ir(IR) of
        true ->
            Enabled = maps:get(enabled, Options, true),
            Before = length(catena_control_ir:nodes(IR)),
            case Enabled of
                false ->
                    {ok, IR, empty_report(false, Before, Before)};
                true ->
                    Stats0 = #{
                        return_wrappers_eliminated => 0,
                        direct_bridges_collapsed => 0
                    },
                    {Transforms, Stats1} = lists:mapfoldl(
                        fun optimize_transform/2,
                        Stats0,
                        catena_control_ir:transforms(IR)
                    ),
                    case catena_control_ir:new(
                        catena_control_ir:module_name(IR),
                        Transforms,
                        maps:get(origin, IR)
                    ) of
                        {ok, Optimized} ->
                            After = length(catena_control_ir:nodes(Optimized)),
                            {ok, Optimized, Stats1#{
                                enabled => true,
                                nodes_before => Before,
                                nodes_after => After
                            }};
                        {error, _} = Error ->
                            Error
                    end
            end;
        false ->
            {error, {invalid_control_optimization_input, IR}}
    end;
optimize(_IR, Options) ->
    {error, {invalid_control_optimization_options, Options}}.

empty_report(Enabled, Before, After) ->
    #{
        enabled => Enabled,
        return_wrappers_eliminated => 0,
        direct_bridges_collapsed => 0,
        nodes_before => Before,
        nodes_after => After
    }.

optimize_transform(Transform, Stats0) ->
    {Clauses, Stats1} = lists:mapfoldl(
        fun optimize_clause/2,
        Stats0,
        maps:get(clauses, Transform)
    ),
    {Transform#{clauses := Clauses}, Stats1}.

optimize_clause(Clause, Stats0) ->
    {Body, Stats1} = optimize_term(maps:get(body, Clause), Stats0),
    {Clause#{body := Body}, Stats1}.

optimize_term(Term, Stats0) when is_map(Term) ->
    case catena_control_ir:is_node(Term) of
        true ->
            optimize_node(Term, Stats0);
        false ->
            map_values(Term, Stats0)
    end;
optimize_term(Terms, Stats0) when is_list(Terms) ->
    lists:mapfoldl(fun optimize_term/2, Stats0, Terms);
optimize_term(Term, Stats0) when is_tuple(Term) ->
    {Values, Stats1} = lists:mapfoldl(
        fun optimize_term/2,
        Stats0,
        tuple_to_list(Term)
    ),
    {list_to_tuple(Values), Stats1};
optimize_term(Term, Stats) ->
    {Term, Stats}.

optimize_node(Node, Stats0) ->
    {Fields, Stats1} = map_values(maps:get(fields, Node), Stats0),
    Node1 = Node#{fields := Fields},
    case Node1 of
        #{op := return, fields := #{value := Value}} ->
            {Value, increment(return_wrappers_eliminated, Stats1)};
        #{
            op := bridge,
            fields := #{
                bridge := direct_to_cps,
                proof := direct_callee,
                closure := #{control_mode := direct}
            }
        } ->
            Metadata0 = maps:get(metadata, Node1),
            Metadata = Metadata0#{runtime_disposition := direct},
            DirectFields = maps:without(
                [bridge, proof, bridge_evidence],
                Fields
            ),
            {
                Node1#{
                    op := direct_call,
                    metadata := Metadata,
                    fields := DirectFields
                },
                increment(direct_bridges_collapsed, Stats1)
            };
        _ ->
            {Node1, Stats1}
    end.

map_values(Map, Stats0) ->
    {Pairs, Stats1} = lists:mapfoldl(
        fun({Key, Value}, Stats) ->
            {Optimized, NextStats} = optimize_term(Value, Stats),
            {{Key, Optimized}, NextStats}
        end,
        Stats0,
        maps:to_list(Map)
    ),
    {maps:from_list(Pairs), Stats1}.

increment(Key, Stats) ->
    Stats#{Key := maps:get(Key, Stats) + 1}.
