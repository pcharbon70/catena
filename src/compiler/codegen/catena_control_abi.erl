%%%-------------------------------------------------------------------
%%% @doc Stable direct/selective-CPS calling conventions and bridges.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_control_abi).

-export([
    entry_shape/3,
    final_continuation/3,
    closure_shape/5,
    bridge/5,
    validate_entry/1,
    validate_closure/1
]).

-spec entry_shape(atom(), non_neg_integer(), direct | resumable) -> map().
entry_shape(Name, Arity, direct) ->
    #{
        public => {Name, Arity},
        private => {direct, Name, Arity + 1},
        source_arity => Arity,
        context_arity => 1,
        continuation_arity => 0,
        control_mode => direct
    };
entry_shape(Name, Arity, resumable) ->
    #{
        public => {Name, Arity},
        private => {cps, Name, Arity + 2},
        source_arity => Arity,
        context_arity => 1,
        continuation_arity => 1,
        control_mode => resumable
    }.

-spec final_continuation(atom(), term(), term()) -> map().
final_continuation(Name, ResultType, Origin) ->
    #{
        identity => {continuation, Name, 0},
        arity => 1,
        result_type => ResultType,
        disposition => public_boundary,
        origin => Origin
    }.

-spec closure_shape(
    local | imported | trait_dictionary | higher_order,
    term(),
    non_neg_integer(),
    direct | resumable,
    term()
) -> map().
closure_shape(Kind, Identity, SourceArity, Mode, Origin) ->
    ExtraArity = case Mode of
        direct -> 1;
        resumable -> 2
    end,
    #{
        kind => Kind,
        identity => Identity,
        source_arity => SourceArity,
        runtime_arity => SourceArity + ExtraArity,
        control_mode => Mode,
        context_arity => 1,
        continuation_arity => case Mode of
            direct -> 0;
            resumable -> 1
        end,
        origin => Origin
    }.

-spec bridge(
    direct | resumable,
    direct | resumable,
    term(),
    term(),
    term()
) -> {ok, none | map()} | {error, term()}.
bridge(Mode, Mode, _Proof, _Identity, _Origin) ->
    {ok, none};
bridge(resumable, direct, _Proof, Identity, Origin) ->
    {ok, #{
        kind => direct_to_cps,
        caller_mode => resumable,
        callee_mode => direct,
        proof => direct_callee,
        identity => Identity,
        origin => Origin
    }};
bridge(direct, resumable, non_suspending, Identity, Origin) ->
    {ok, #{
        kind => resumable_to_direct,
        caller_mode => direct,
        callee_mode => resumable,
        proof => non_suspending,
        identity => Identity,
        origin => Origin
    }};
bridge(direct, resumable, Proof, Identity, Origin) ->
    {error, {resumption_abi_mismatch, #{
        caller_mode => direct,
        callee_mode => resumable,
        proof => Proof,
        identity => Identity,
        origin => Origin
    }}}.

-spec validate_entry(term()) -> ok | {error, term()}.
validate_entry(#{
    public := {Name, SourceArity},
    private := {direct, Name, PrivateArity},
    source_arity := SourceArity,
    context_arity := 1,
    continuation_arity := 0,
    control_mode := direct
}) when PrivateArity =:= SourceArity + 1 ->
    ok;
validate_entry(#{
    public := {Name, SourceArity},
    private := {cps, Name, PrivateArity},
    source_arity := SourceArity,
    context_arity := 1,
    continuation_arity := 1,
    control_mode := resumable
}) when PrivateArity =:= SourceArity + 2 ->
    ok;
validate_entry(Entry) ->
    {error, {invalid_control_entry, Entry}}.

-spec validate_closure(term()) -> ok | {error, term()}.
validate_closure(#{
    kind := Kind,
    identity := _Identity,
    source_arity := SourceArity,
    runtime_arity := RuntimeArity,
    control_mode := direct,
    context_arity := 1,
    continuation_arity := 0,
    origin := Origin
}) when
    (Kind =:= local orelse Kind =:= imported orelse
        Kind =:= trait_dictionary orelse Kind =:= higher_order),
    is_integer(SourceArity),
    SourceArity >= 0,
    RuntimeArity =:= SourceArity + 1,
    Origin =/= undefined
->
    ok;
validate_closure(#{
    kind := Kind,
    identity := _Identity,
    source_arity := SourceArity,
    runtime_arity := RuntimeArity,
    control_mode := resumable,
    context_arity := 1,
    continuation_arity := 1,
    origin := Origin
}) when
    (Kind =:= local orelse Kind =:= imported orelse
        Kind =:= trait_dictionary orelse Kind =:= higher_order),
    is_integer(SourceArity),
    SourceArity >= 0,
    RuntimeArity =:= SourceArity + 2,
    Origin =/= undefined
->
    ok;
validate_closure(Closure) ->
    {error, {invalid_control_closure, Closure}}.
