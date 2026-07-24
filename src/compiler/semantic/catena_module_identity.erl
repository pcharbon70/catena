%%%-------------------------------------------------------------------
%%% @doc Canonical Catena source and runtime module identities.
%%%
%%% Catena module names are already atoms by the time the parser hands them
%%% to semantic analysis.  The BEAM identity is deliberately the same atom:
%%% dotted atoms are valid BEAM module names and remain distinct from simple
%%% names such as `Effect_IO`.  No additional atoms are manufactured while
%%% resolving or publishing an identity.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_module_identity).

-export([
    normalize/1,
    runtime_module/1,
    components/1,
    validate_unique/1
]).

-type identity() :: #{
    source_module := atom(),
    runtime_module := atom(),
    components := [binary()],
    canonical := binary()
}.

-export_type([identity/0]).

-spec normalize(atom()) -> {ok, identity()} | {error, term()}.
normalize(Name) when is_atom(Name) ->
    Canonical = atom_to_binary(Name, utf8),
    Parts = binary:split(Canonical, <<".">>, [global]),
    case Parts =/= [] andalso
         lists:all(fun valid_component/1, Parts) of
        true ->
            {ok, #{
                source_module => Name,
                runtime_module => Name,
                components => Parts,
                canonical => Canonical
            }};
        false ->
            {error, {invalid_module_identity, Name}}
    end;
normalize(Name) ->
    {error, {invalid_module_identity, Name}}.

-spec runtime_module(atom()) -> {ok, atom()} | {error, term()}.
runtime_module(Name) ->
    case normalize(Name) of
        {ok, #{runtime_module := RuntimeModule}} ->
            {ok, RuntimeModule};
        {error, _} = Error ->
            Error
    end.

-spec components(atom()) -> {ok, [binary()]} | {error, term()}.
components(Name) ->
    case normalize(Name) of
        {ok, #{components := Parts}} ->
            {ok, Parts};
        {error, _} = Error ->
            Error
    end.

-spec validate_unique([atom()]) -> ok | {error, term()}.
validate_unique(Names) when is_list(Names) ->
    validate_unique(Names, #{}, #{});
validate_unique(Names) ->
    {error, {invalid_module_identity_set, Names}}.

validate_unique([], _Sources, _Runtimes) ->
    ok;
validate_unique([Name | Rest], Sources, Runtimes) ->
    case normalize(Name) of
        {ok, #{
            source_module := Source,
            runtime_module := Runtime
        }} ->
            case {
                maps:is_key(Source, Sources),
                maps:get(Runtime, Runtimes, undefined)
            } of
                {true, _} ->
                    {error, {duplicate_source_module, Source}};
                {false, undefined} ->
                    validate_unique(
                        Rest,
                        Sources#{Source => true},
                        Runtimes#{Runtime => Source}
                    );
                {false, OtherSource} ->
                    {error, {
                        runtime_module_identity_collision,
                        Runtime,
                        OtherSource,
                        Source
                    }}
            end;
        {error, _} = Error ->
            Error
    end.

valid_component(<<>>) ->
    false;
valid_component(Component) ->
    binary:match(Component, <<0>>) =:= nomatch.
