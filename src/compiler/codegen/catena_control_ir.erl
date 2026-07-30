%%%-------------------------------------------------------------------
%%% @doc Canonical selective-CPS control IR.
%%%
%%% Every node carries the metadata required by later validation and Core
%%% lowering. The IR is compiler-owned and deliberately independent of Core
%%% Erlang syntax and the production runtime representation.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_control_ir).

-compile({no_auto_import, [nodes/1]}).

-export([
    new/3,
    node/3,
    is_ir/1,
    is_node/1,
    module_name/1,
    transforms/1,
    lookup/2,
    nodes/1,
    walk/2
]).

-define(IR_VERSION, 1).
-define(NODE_VERSION, 1).

-type operation() ::
    return |
    direct_expr |
    bind |
    match |
    direct_call |
    cps_call |
    delimiter |
    install_handler |
    perform |
    make_resumption |
    resume |
    abort |
    bridge |
    closure.
-type metadata() :: #{
    value_type := term(),
    effect_row := term(),
    control_mode := direct | resumable,
    delimiter := term(),
    continuation_arity := non_neg_integer(),
    runtime_disposition := term(),
    origin := term()
}.
-opaque control_node() :: #{
    '$catena_control_node' := pos_integer(),
    op := operation(),
    metadata := metadata(),
    fields := map()
}.
-opaque ir() :: #{
    '$catena_control_ir' := pos_integer(),
    module := atom(),
    transforms := [map()],
    by_name := #{atom() => map()},
    origin := term()
}.

-export_type([operation/0, metadata/0, control_node/0, ir/0]).

-spec new(atom(), [map()], term()) -> {ok, ir()} | {error, term()}.
new(Module, Transforms, Origin)
        when is_atom(Module), is_list(Transforms) ->
    ByName = maps:from_list([
        {maps:get(name, Transform), Transform}
        || Transform <- Transforms
    ]),
    Candidate = #{
        '$catena_control_ir' => ?IR_VERSION,
        module => Module,
        transforms => Transforms,
        by_name => ByName,
        origin => Origin
    },
    case is_ir(Candidate) of
        true -> {ok, Candidate};
        false -> {error, {invalid_control_ir, malformed_module}}
    end.

-spec node(operation(), metadata(), map()) ->
    {ok, control_node()} | {error, term()}.
node(Operation, Metadata, Fields)
        when is_atom(Operation), is_map(Metadata), is_map(Fields) ->
    Candidate = #{
        '$catena_control_node' => ?NODE_VERSION,
        op => Operation,
        metadata => Metadata,
        fields => Fields
    },
    case is_node(Candidate) of
        true -> {ok, Candidate};
        false ->
            {error, {invalid_control_ir_node, Operation, Metadata}}
    end.

-spec is_ir(term()) -> boolean().
is_ir(#{
    '$catena_control_ir' := ?IR_VERSION,
    module := Module,
    transforms := Transforms,
    by_name := ByName
}) ->
    is_atom(Module) andalso
        is_list(Transforms) andalso
        is_map(ByName) andalso
        lists:all(fun valid_transform/1, Transforms);
is_ir(_) ->
    false.

-spec is_node(term()) -> boolean().
is_node(#{
    '$catena_control_node' := ?NODE_VERSION,
    op := Operation,
    metadata := Metadata,
    fields := Fields
}) ->
    lists:member(Operation, operations()) andalso
        valid_metadata(Metadata) andalso
        is_map(Fields);
is_node(_) ->
    false.

-spec module_name(ir()) -> atom().
module_name(IR) ->
    maps:get(module, IR).

-spec transforms(ir()) -> [map()].
transforms(IR) ->
    maps:get(transforms, IR).

-spec lookup(atom(), ir()) -> {ok, map()} | none.
lookup(Name, IR) ->
    case maps:find(Name, maps:get(by_name, IR)) of
        {ok, Transform} -> {ok, Transform};
        error -> none
    end.

-spec nodes(ir()) -> [control_node()].
nodes(IR) ->
    lists:append([
        lists:append([
            collect_nodes(maps:get(body, Clause))
            || Clause <- maps:get(clauses, Transform)
        ])
        || Transform <- transforms(IR)
    ]).

-spec walk(fun((control_node(), term()) -> term()), {ir(), term()}) -> term().
walk(Fun, {IR, Acc}) when is_function(Fun, 2) ->
    lists:foldl(Fun, Acc, nodes(IR)).

collect_nodes(Node) when is_map(Node) ->
    Current = case is_node(Node) of
        true -> [Node];
        false -> []
    end,
    Current ++ lists:append([
        collect_nodes(Value)
        || Value <- maps:values(Node)
    ]);
collect_nodes(Terms) when is_list(Terms) ->
    lists:append([collect_nodes(Term) || Term <- Terms]);
collect_nodes(Term) when is_tuple(Term) ->
    collect_nodes(tuple_to_list(Term));
collect_nodes(_Other) ->
    [].

valid_transform(#{
    name := Name,
    arity := Arity,
    control_mode := Mode,
    entry := Entry,
    clauses := Clauses,
    type := _Type,
    effect_row := _Effects,
    origin := _Origin
}) ->
    is_atom(Name) andalso
        is_integer(Arity) andalso
        Arity >= 0 andalso
        lists:member(Mode, [direct, resumable]) andalso
        is_map(Entry) andalso
        is_list(Clauses) andalso
        lists:all(fun valid_clause/1, Clauses);
valid_transform(_) ->
    false.

valid_clause(#{
    patterns := Patterns,
    guards := _Guards,
    body := Body,
    origin := _Origin
}) ->
    is_list(Patterns) andalso is_node(Body);
valid_clause(_) ->
    false.

valid_metadata(#{
    value_type := _ValueType,
    effect_row := _EffectRow,
    control_mode := Mode,
    delimiter := _Delimiter,
    continuation_arity := Arity,
    runtime_disposition := _Disposition,
    origin := _Origin
}) ->
    lists:member(Mode, [direct, resumable]) andalso
        is_integer(Arity) andalso
        Arity >= 0;
valid_metadata(_) ->
    false.

operations() ->
    [
        return,
        direct_expr,
        bind,
        match,
        direct_call,
        cps_call,
        delimiter,
        install_handler,
        perform,
        make_resumption,
        resume,
        abort,
        bridge,
        closure
    ].
