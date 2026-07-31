%%%-------------------------------------------------------------------
%%% @doc Fail-closed validation for selective-CPS control graphs.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_control_validate).

-export([validate/1, validate_ir/3, is_report/1]).

-define(REPORT_VERSION, 1).

-type report() :: #{
    '$catena_control_validation' := pos_integer(),
    module := atom(),
    transforms := non_neg_integer(),
    nodes := non_neg_integer(),
    delimiters := non_neg_integer(),
    continuations := non_neg_integer(),
    bridges := non_neg_integer(),
    status := passed,
    optimization => map()
}.

-export_type([report/0]).

-spec validate(catena_compilation_unit:t()) ->
    {ok, report()} | {error, catena_backend_error:diagnostic()}.
validate(Unit) ->
    case catena_compilation_unit:is_compilation_unit(Unit) of
        true ->
            validate_ir(
                catena_compilation_unit:control_ir(Unit),
                catena_compilation_unit:control_modes(Unit),
                #{
                    module =>
                        catena_compilation_unit:module_name(Unit),
                    source_identity =>
                        catena_compilation_unit:source_identity(Unit)
                }
            );
        false ->
            control_error(
                unchecked_compilation_unit,
                #{stage => control_ir_validation}
            )
    end.

-spec validate_ir(
    catena_control_ir:ir(),
    catena_control_mode:inventory(),
    map()
) -> {ok, report()} | {error, catena_backend_error:diagnostic()}.
validate_ir(IR, Modes, Context) ->
    case {
        catena_control_ir:is_ir(IR),
        catena_control_mode:is_inventory(Modes)
    } of
        {true, true} ->
            Transforms = catena_control_ir:transforms(IR),
            Nodes = catena_control_ir:nodes(IR),
            Delimiters = delimiter_identities(Nodes),
            DelimiterModes = delimiter_modes(Nodes),
            Continuations = continuation_identities(Transforms, Nodes),
            Checks = [
                fun() -> validate_transforms(Transforms, Modes, Context) end,
                fun() -> validate_nodes(Nodes, Context) end,
                fun() ->
                    validate_unique_identities(
                        Delimiters,
                        Continuations,
                        Context
                    )
                end,
                fun() ->
                    validate_ownership(
                        Nodes,
                        Delimiters,
                        Continuations,
                        Context
                    )
                end,
                fun() ->
                    validate_handler_modes(
                        Nodes,
                        DelimiterModes,
                        Context
                    )
                end,
                fun() -> validate_call_shapes(Nodes, Context) end,
                fun() -> validate_bridges(Nodes, Context) end,
                fun() -> validate_backend_readiness(Nodes, Context) end
            ],
            case run_checks(Checks) of
                ok ->
                    {ok, #{
                        '$catena_control_validation' =>
                            ?REPORT_VERSION,
                        module => catena_control_ir:module_name(IR),
                        transforms => length(Transforms),
                        nodes => length(Nodes),
                        delimiters => length(Delimiters),
                        continuations => length(Continuations),
                        bridges => length([
                            Node
                            || Node <- Nodes,
                               maps:get(op, Node) =:= bridge
                        ]),
                        status => passed
                    }};
                {error, _} = Error ->
                    Error
            end;
        _ ->
            control_error(malformed_control_graph, Context)
    end.

-spec is_report(term()) -> boolean().
is_report(#{
    '$catena_control_validation' := ?REPORT_VERSION,
    module := Module,
    transforms := Transforms,
    nodes := Nodes,
    status := passed
}) ->
    is_atom(Module) andalso
        is_integer(Transforms) andalso
        Transforms >= 0 andalso
        is_integer(Nodes) andalso
        Nodes >= 0;
is_report(_) ->
    false.

validate_transforms([], _Modes, _Context) ->
    ok;
validate_transforms([Transform | Rest], Modes, Context) ->
    Name = maps:get(name, Transform),
    Mode = maps:get(control_mode, Transform),
    ModeResult = case catena_control_mode:mode(Name, Modes) of
        {ok, Mode} -> ok;
        {ok, OtherMode} ->
            control_error(
                {control_mode_mismatch, Name, Mode, OtherMode},
                Context#{
                    transform => Name,
                    location => maps:get(origin, Transform)
                }
            );
        none ->
            control_error(
                {missing_control_mode, Name},
                Context#{
                    transform => Name,
                    location => maps:get(origin, Transform)
                }
            )
    end,
    case ModeResult of
        ok ->
            case catena_control_abi:validate_entry(
                maps:get(entry, Transform)
            ) of
                ok ->
                    case validate_final_continuation(
                        Transform,
                        Context
                    ) of
                        ok ->
                            validate_transforms(
                                Rest,
                                Modes,
                                Context
                            );
                        {error, _} = Error ->
                            Error
                    end;
                {error, Reason} ->
                    abi_error(
                        Reason,
                        Context#{
                            transform => Name,
                            location => maps:get(origin, Transform)
                        }
                    )
            end;
        {error, _} = Error ->
            Error
    end.

validate_final_continuation(Transform, Context) ->
    Name = maps:get(name, Transform),
    case maps:get(final_continuation, Transform, #{}) of
        #{
            identity := {continuation, Name, 0},
            arity := 1,
            origin := Origin
        } when Origin =/= undefined ->
            ok;
        Invalid ->
            control_error(
                {invalid_final_continuation, Invalid},
                Context#{
                    transform => Name,
                    location => maps:get(origin, Transform)
                }
            )
    end.

validate_nodes([], _Context) ->
    ok;
validate_nodes([Node | Rest], Context) ->
    Metadata = maps:get(metadata, Node),
    Origin = maps:get(origin, Metadata, undefined),
    case Origin of
        undefined ->
            control_error(
                {missing_control_origin, maps:get(op, Node)},
                node_context(Node, Context)
            );
        _ ->
            case expected_continuation_arity(Node) of
                any ->
                    validate_node_ast_leakage(Node, Rest, Context);
                Expected ->
                    Actual = maps:get(
                        continuation_arity,
                        Metadata
                    ),
                    case Actual =:= Expected of
                        true ->
                            validate_node_ast_leakage(
                                Node,
                                Rest,
                                Context
                            );
                        false ->
                            control_error(
                                {
                                    continuation_arity_mismatch,
                                    maps:get(op, Node),
                                    Expected,
                                    Actual
                                },
                                node_context(Node, Context)
                            )
                    end
            end
    end.

validate_node_ast_leakage(Node, Rest, Context) ->
    case contains_control_ast(maps:get(fields, Node)) of
        true ->
            control_error(
                {leaked_control_ast, maps:get(op, Node)},
                node_context(Node, Context)
            );
        false ->
            validate_nodes(Rest, Context)
    end.

validate_ownership([], _Delimiters, _Continuations, _Context) ->
    ok;
validate_ownership(
    [Node | Rest],
    Delimiters,
    Continuations,
    Context
) ->
    case validate_node_ownership(Node, Delimiters, Continuations) of
        ok ->
            validate_ownership(
                Rest,
                Delimiters,
                Continuations,
                Context
            );
        {error, Reason} ->
            control_error(Reason, node_context(Node, Context))
    end.

validate_node_ownership(
    #{op := delimiter, fields := Fields},
    _Delimiters,
    Continuations
) ->
    require_continuation(
        maps:get(continuation, Fields, undefined),
        Continuations
    );
validate_node_ownership(
    #{op := install_handler, fields := Fields},
    Delimiters,
    _Continuations
) ->
    require_delimiter(
        maps:get(delimiter, Fields, undefined),
        Delimiters
    );
validate_node_ownership(
    #{op := make_resumption, fields := Fields},
    Delimiters,
    Continuations
) ->
    case {
        maps:get(kind, Fields, undefined),
        maps:get(depth, Fields, undefined)
    } of
        {Kind, Depth}
                when
                    (Kind =:= one_shot orelse Kind =:= multi_shot),
                    (Depth =:= deep orelse Depth =:= shallow)
                ->
            case require_delimiter(
                maps:get(delimiter, Fields, undefined),
                Delimiters
            ) of
                ok ->
                    require_continuation(
                        maps:get(
                            continuation,
                            Fields,
                            undefined
                        ),
                        Continuations
                    );
                {error, _} = Error ->
                    Error
            end;
        {Kind, Depth} ->
            {error, {invalid_resumption_mode, Depth, Kind}}
    end;
validate_node_ownership(
    #{op := resume, fields := Fields},
    Delimiters,
    _Continuations
) ->
    Authority = maps:get(authority, Fields, undefined),
    case validate_resume_authority(Authority) of
        ok ->
            case maps:get(delimiter, Fields, undefined) of
                from_resumption_authority ->
                    ok;
                Delimiter ->
                    require_delimiter(Delimiter, Delimiters)
            end;
        {error, _} = Error ->
            Error
    end;
validate_node_ownership(
    #{op := abort, fields := Fields},
    Delimiters,
    _Continuations
) ->
    case require_delimiter(
        maps:get(delimiter, Fields, undefined),
        Delimiters
    ) of
        ok -> ok;
        {error, _} ->
            {error, {invalid_abort_target,
                maps:get(delimiter, Fields, undefined)}}
    end;
validate_node_ownership(
    #{op := perform, fields := #{suspension := true} = Fields},
    Delimiters,
    _Continuations
) ->
    Metadata = maps:get(metadata, Fields, #{}),
    Delimiter = case maps:find(delimiter, Fields) of
        {ok, Value} -> Value;
        error -> maps:get(delimiter, Metadata, undefined)
    end,
    case maps:get(resumption, Fields, undefined) of
        Resumption when is_map(Resumption) ->
            require_delimiter(
                maps:get(
                    delimiter,
                    maps:get(fields, Resumption, #{}),
                    Delimiter
                ),
                Delimiters
            );
        _ ->
            {error, missing_resumption_construction}
    end;
validate_node_ownership(
    _Node,
    _Delimiters,
    _Continuations
) ->
    ok.

validate_handler_modes([], _DelimiterModes, _Context) ->
    ok;
validate_handler_modes([Node | Rest], DelimiterModes, Context) ->
    case validate_node_handler_mode(Node, DelimiterModes) of
        ok ->
            validate_handler_modes(Rest, DelimiterModes, Context);
        {error, Reason} ->
            control_error(Reason, node_context(Node, Context))
    end.

validate_node_handler_mode(
    #{op := delimiter, fields := Fields},
    _DelimiterModes
) ->
    validate_mode_fields(delimiter, Fields);
validate_node_handler_mode(
    #{op := Operation, fields := Fields},
    DelimiterModes
) when Operation =:= install_handler; Operation =:= make_resumption ->
    case validate_mode_fields(Operation, Fields) of
        ok ->
            Delimiter = maps:get(delimiter, Fields, undefined),
            Actual = mode_from_fields(Fields),
            case maps:find(Delimiter, DelimiterModes) of
                {ok, Actual} ->
                    ok;
                {ok, Expected} ->
                    {error, {handler_mode_mismatch,
                        Operation, Delimiter, Expected, Actual}};
                error ->
                    {error, {missing_delimiter_mode,
                        Operation, Delimiter}}
            end;
        {error, _} = Error ->
            Error
    end;
validate_node_handler_mode(_Node, _DelimiterModes) ->
    ok.

validate_mode_fields(Operation, Fields) ->
    case {
        maps:get(depth, Fields, undefined),
        maps:get(kind, Fields, undefined)
    } of
        {Depth, Kind}
                when
                    (Depth =:= deep orelse Depth =:= shallow),
                    (Kind =:= one_shot orelse Kind =:= multi_shot)
                ->
            ok;
        {Depth, Kind} ->
            {error, {invalid_handler_mode, Operation, Depth, Kind}}
    end.

mode_from_fields(Fields) ->
    #{
        depth => maps:get(depth, Fields, undefined),
        kind => maps:get(kind, Fields, undefined)
    }.

validate_call_shapes([], _Context) ->
    ok;
validate_call_shapes(
    [#{op := Operation, fields := Fields} = Node | Rest],
    Context
) when
    Operation =:= direct_call;
    Operation =:= cps_call;
    Operation =:= bridge
->
    Closure = maps:get(closure, Fields, #{}),
    ExpectedMode = case Operation of
        direct_call -> direct;
        cps_call -> resumable;
        bridge -> maps:get(control_mode, Closure, undefined)
    end,
    case catena_control_abi:validate_closure(Closure) of
        ok ->
            case maps:get(control_mode, Closure) =:= ExpectedMode of
                true ->
                    validate_call_shapes(Rest, Context);
                false ->
                    abi_error(
                        {
                            call_closure_mode_mismatch,
                            Operation,
                            ExpectedMode,
                            maps:get(control_mode, Closure)
                        },
                        node_context(Node, Context)
                    )
            end;
        {error, Reason} ->
            abi_error(Reason, node_context(Node, Context))
    end;
validate_call_shapes([_ | Rest], Context) ->
    validate_call_shapes(Rest, Context).

validate_bridges([], _Context) ->
    ok;
validate_bridges([#{op := bridge} = Node | Rest], Context) ->
    Metadata = maps:get(metadata, Node),
    Fields = maps:get(fields, Node),
    Caller = maps:get(control_mode, Metadata),
    Closure = maps:get(closure, Fields, #{}),
    Callee = maps:get(control_mode, Closure, undefined),
    Proof = maps:get(proof, Fields, missing),
    Identity = maps:get(target, Fields, undefined),
    Origin = maps:get(origin, Metadata),
    case catena_control_abi:bridge(
        Caller,
        Callee,
        Proof,
        Identity,
        Origin
    ) of
        {ok, _} ->
            validate_bridges(Rest, Context);
        {error, Reason} ->
            abi_error(Reason, node_context(Node, Context))
    end;
validate_bridges([_ | Rest], Context) ->
    validate_bridges(Rest, Context).

validate_backend_readiness([], _Context) ->
    ok;
validate_backend_readiness([Node | Rest], Context) ->
    Disposition = maps:get(
        runtime_disposition,
        maps:get(metadata, Node)
    ),
    case lists:member(
        Disposition,
        [
            direct,
            provider_runtime,
            requires_resumption_runtime,
            direct_to_cps_bridge
        ]
    ) of
        true ->
            validate_backend_readiness(Rest, Context);
        false ->
            control_error(
                {
                    missing_control_disposition,
                    maps:get(op, Node),
                    Disposition
                },
                node_context(Node, Context)
            )
    end.

expected_continuation_arity(#{op := Op}) when
    Op =:= delimiter;
    Op =:= install_handler;
    Op =:= make_resumption;
    Op =:= resume
->
    1;
expected_continuation_arity(#{
    op := perform,
    fields := #{suspension := true}
}) ->
    1;
expected_continuation_arity(#{op := direct_call}) ->
    0;
expected_continuation_arity(#{op := cps_call}) ->
    1;
expected_continuation_arity(#{op := direct_expr}) ->
    0;
expected_continuation_arity(#{op := abort}) ->
    0;
expected_continuation_arity(#{op := perform}) ->
    0;
expected_continuation_arity(_Node) ->
    any.

delimiter_identities(Nodes) ->
    [
        maps:get(identity, maps:get(fields, Node))
        || Node <- Nodes,
           maps:get(op, Node) =:= delimiter
    ].

delimiter_modes(Nodes) ->
    maps:from_list([
        {
            maps:get(identity, maps:get(fields, Node)),
            mode_from_fields(maps:get(fields, Node))
        }
        || Node <- Nodes,
           maps:get(op, Node) =:= delimiter
    ]).

continuation_identities(Transforms, Nodes) ->
    [
        maps:get(
            identity,
            maps:get(final_continuation, Transform)
        )
        || Transform <- Transforms
    ] ++
        [
            maps:get(continuation, maps:get(fields, Node))
            || Node <- Nodes,
               lists:member(maps:get(op, Node), [delimiter, bind])
        ].

validate_unique_identities(Delimiters, Continuations, Context) ->
    case first_duplicate(Delimiters) of
        none ->
            case first_duplicate(Continuations) of
                none ->
                    ok;
                {duplicate, Continuation} ->
                    control_error(
                        {duplicate_continuation, Continuation},
                        Context#{stage => control_ir_validation}
                    )
            end;
        {duplicate, Delimiter} ->
            control_error(
                {duplicate_delimiter, Delimiter},
                Context#{stage => control_ir_validation}
            )
    end.

first_duplicate(Identities) ->
    first_duplicate(Identities, #{}).

first_duplicate([], _Seen) ->
    none;
first_duplicate([Identity | Rest], Seen) ->
    case maps:is_key(Identity, Seen) of
        true -> {duplicate, Identity};
        false -> first_duplicate(Rest, Seen#{Identity => true})
    end.

require_delimiter(Delimiter, Delimiters) ->
    case lists:member(Delimiter, Delimiters) of
        true -> ok;
        false -> {error, {dangling_delimiter, Delimiter}}
    end.

require_continuation(Continuation, Continuations) ->
    case lists:member(Continuation, Continuations) of
        true -> ok;
        false -> {error, {dangling_continuation, Continuation}}
    end.

validate_resume_authority(#{
    type := {tresumption, {tcon, 'OneShot'}, _, _, _}
}) ->
    ok;
validate_resume_authority(#{
    type := {tresumption, {tcon, 'MultiShot'}, _, _, _}
}) ->
    ok;
validate_resume_authority(#{
    type := {tresumption, {tkvar, resumption_kind, _}, _, _, _}
}) ->
    ok;
validate_resume_authority(Authority) ->
    {error, {resume_without_authority, Authority}}.

contains_control_ast(Term) when is_tuple(Term), tuple_size(Term) > 0 ->
    case element(1, Term) of
        resume_expr -> true;
        handle_expr -> true;
        _ -> contains_control_ast(tuple_to_list(Term))
    end;
contains_control_ast(Terms) when is_list(Terms) ->
    lists:any(fun contains_control_ast/1, Terms);
contains_control_ast(Term) when is_map(Term) ->
    case catena_control_ir:is_node(Term) of
        true -> false;
        false ->
            lists:any(
                fun contains_control_ast/1,
                maps:values(Term)
            )
    end;
contains_control_ast(_) ->
    false.

node_context(Node, Context) ->
    Metadata = maps:get(metadata, Node),
    Context#{
        stage => control_ir_validation,
        construct => maps:get(op, Node),
        transform => maps:get(transform, Metadata, undefined),
        location => maps:get(origin, Metadata, undefined),
        source_term => Node
    }.

control_error(Reason, Context) ->
    {error, catena_backend_error:invalid_control_ir(Reason, Context)}.

abi_error(Reason, Context) ->
    {error, catena_backend_error:resumption_abi_mismatch(
        Reason,
        Context
    )}.

run_checks([]) ->
    ok;
run_checks([Check | Rest]) ->
    case Check() of
        ok -> run_checks(Rest);
        {error, _} = Error -> Error
    end.
