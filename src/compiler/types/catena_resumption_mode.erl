%%%-------------------------------------------------------------------
%%% @doc Canonical source and static semantics for handler depth and
%%% resumption invocation kind.
%%%
%%% Deep one-shot remains the implicit language default. Explicit mode maps
%%% are opaque compiler metadata carried from parsing through interfaces and
%%% lowering. Multi-shot is admitted only when the residual row is closed and
%%% empty; Catena does not claim to duplicate external or stateful authority.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_resumption_mode).

-export([
    default/1,
    normalize/2,
    is_mode/1,
    depth/1,
    kind/1,
    interface_view/1,
    resumption_kind_type/1,
    resumption_effect_row/3,
    validate_multi_shot/2,
    format_error/1
]).

-define(MODE_VERSION, 1).

-type depth() :: deep | shallow.
-type kind() :: one_shot | multi_shot.
-type mode() :: #{
    '$catena_handler_mode' := pos_integer(),
    depth := depth(),
    kind := kind(),
    explicit := boolean(),
    origin := term()
}.

-export_type([depth/0, kind/0, mode/0]).

-spec default(term()) -> mode().
default(Origin) ->
    #{
        '$catena_handler_mode' => ?MODE_VERSION,
        depth => deep,
        kind => one_shot,
        explicit => false,
        origin => Origin
    }.

-spec normalize(term(), term()) -> {ok, mode()} | {error, term()}.
normalize(#{
    '$catena_handler_mode' := ?MODE_VERSION,
    depth := _Depth,
    kind := _Kind
} = Mode, FallbackOrigin) ->
    Candidate = Mode#{
        explicit => maps:get(explicit, Mode, true),
        origin => maps:get(origin, Mode, FallbackOrigin)
    },
    case is_mode(Candidate) of
        true -> {ok, Candidate};
        false -> {error, {invalid_handler_mode, Candidate}}
    end;
normalize(undefined, Origin) ->
    {ok, default(Origin)};
normalize(Mode, _Origin) ->
    {error, {invalid_handler_mode, Mode}}.

-spec is_mode(term()) -> boolean().
is_mode(#{
    '$catena_handler_mode' := ?MODE_VERSION,
    depth := Depth,
    kind := Kind,
    explicit := Explicit,
    origin := _Origin
}) ->
    lists:member(Depth, [deep, shallow]) andalso
        lists:member(Kind, [one_shot, multi_shot]) andalso
        is_boolean(Explicit);
is_mode(_) ->
    false.

-spec depth(mode()) -> depth().
depth(Mode) -> maps:get(depth, Mode).

-spec kind(mode()) -> kind().
kind(Mode) -> maps:get(kind, Mode).

-spec interface_view(mode()) -> map().
interface_view(Mode) ->
    #{depth => depth(Mode), kind => kind(Mode)}.

-spec resumption_kind_type(mode()) -> catena_types:type().
resumption_kind_type(Mode) ->
    case kind(Mode) of
        one_shot -> catena_types:one_shot();
        multi_shot -> catena_types:multi_shot()
    end.

%% @doc Shallow resume cannot assume that the selected handler remains
%% installed, so every selected effect is retained in the residual row.
-spec resumption_effect_row(mode(), [atom()], catena_types:type()) ->
    catena_types:type().
resumption_effect_row(Mode, HandledEffects, {teffectrow, Effects, Tail}) ->
    case depth(Mode) of
        deep -> {teffectrow, Effects, Tail};
        shallow -> {teffectrow, lists:usort(Effects ++ HandledEffects), Tail}
    end.

-spec validate_multi_shot(mode(), catena_types:type()) ->
    ok | {error, term()}.
validate_multi_shot(Mode, _ResidualRow) when map_get(kind, Mode) =:= one_shot ->
    ok;
validate_multi_shot(Mode, {teffectrow, [], closed})
        when map_get(kind, Mode) =:= multi_shot ->
    ok;
validate_multi_shot(Mode, {teffectrow, Effects, closed} = Row)
        when map_get(kind, Mode) =:= multi_shot ->
    {error, {inadmissible_multi_shot_effects, #{
        mode => interface_view(Mode),
        requested_mode => multi_shot,
        residual_effects => Row,
        inadmissible_effects => Effects,
        reason => external_or_stateful_effects_not_duplicable,
        location => maps:get(origin, Mode)
    }}};
validate_multi_shot(Mode, {teffectrow, Effects, Tail} = Row)
        when map_get(kind, Mode) =:= multi_shot ->
    {error, {inadmissible_multi_shot_effects, #{
        mode => interface_view(Mode),
        requested_mode => multi_shot,
        residual_effects => Row,
        known_effects => Effects,
        open_tail => Tail,
        reason => open_effect_row,
        location => maps:get(origin, Mode)
    }}};
validate_multi_shot(Mode, Row) ->
    {error, {invalid_multi_shot_effect_row, #{
        mode => interface_view(Mode),
        residual_effects => Row,
        location => maps:get(origin, Mode)
    }}}.

-spec format_error(term()) -> string().
format_error({invalid_handler_mode, Mode}) ->
    lists:flatten(io_lib:format("Invalid handler mode: ~p", [Mode]));
format_error({inadmissible_multi_shot_effects, Details}) ->
    lists:flatten(io_lib:format(
        "Multi-shot resumption has inadmissible residual effects: ~p",
        [Details]
    ));
format_error(Other) ->
    lists:flatten(io_lib:format("Resumption mode error: ~p", [Other])).
