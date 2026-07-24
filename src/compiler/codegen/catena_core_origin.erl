%%%-------------------------------------------------------------------
%%% @doc Source-origin metadata for generated Core Erlang.
%%%
%%% Core annotations carry only OTP-supported line/file entries plus one
%%% Catena-owned origin entry.  The public diagnostic contract consumes the
%%% normalized maps produced here rather than exposing raw Core records.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_core_origin).

-export([
    user/4,
    user/5,
    synthetic/4,
    synthetic/5,
    inventory/2,
    annotation/1,
    nearest/2
]).

-type origin() :: #{
    origin := user | synthetic,
    construct := atom(),
    module => atom() | undefined,
    transform => atom() | undefined,
    generated_identity => term(),
    location => term(),
    file => string()
}.

-export_type([origin/0]).

-spec user(cerl:cerl(), atom(), term(),
    catena_codegen_utils:codegen_state()) -> cerl:cerl().
user(Node, Construct, SourceTerm, State) ->
    user(Node, Construct, SourceTerm, State, #{}).

-spec user(cerl:cerl(), atom(), term(),
    catena_codegen_utils:codegen_state(), map()) -> cerl:cerl().
user(Node, Construct, SourceTerm, State, Extra) ->
    annotate(
        Node,
        origin(
            user,
            Construct,
            catena_backend_error:source_location(SourceTerm),
            State,
            Extra
        )
    ).

-spec synthetic(cerl:cerl(), atom(), term(),
    catena_codegen_utils:codegen_state()) -> cerl:cerl().
synthetic(Node, Construct, SourceTerm, State) ->
    synthetic(Node, Construct, SourceTerm, State, #{}).

-spec synthetic(cerl:cerl(), atom(), term(),
    catena_codegen_utils:codegen_state(), map()) -> cerl:cerl().
synthetic(Node, Construct, SourceTerm, State, Extra) ->
    annotate(
        Node,
        origin(
            synthetic,
            Construct,
            catena_backend_error:source_location(SourceTerm),
            State,
            Extra
        )
    ).

%% @doc Build the source/generated identity index stored with an artifact.
-spec inventory(catena_compilation_unit:t(), cerl:cerl()) -> map().
inventory(Unit, CoreModule) ->
    Symbols = catena_compilation_unit:symbols(Unit),
    Module = catena_compilation_unit:module_name(Unit),
    Generated = [
        generated_origin(
            cerl:var_name(NameNode),
            Module,
            Symbols,
            Unit
        )
        || {NameNode, _Definition} <- cerl:module_defs(CoreModule)
    ],
    #{
        module => Module,
        source_identity => catena_compilation_unit:source_identity(Unit),
        source_locations => catena_compilation_unit:locations(Unit),
        generated => Generated
    }.

%% @doc Return the Catena origin annotation from a Core node.
-spec annotation(cerl:cerl()) -> {ok, origin()} | error.
annotation(Node) ->
    case [
        Origin
        || {catena_origin, Origin} <- cerl:get_ann(Node),
           is_map(Origin)
    ] of
        [Origin | _] ->
            {ok, Origin};
        [] ->
            error
    end.

%% @doc Select the nearest known source origin for one OTP diagnostic.
-spec nearest(term(), map()) -> map().
nearest(OtpDetail, Context) ->
    Inventory = maps:get(origins, Context, #{}),
    Generated = maps:get(generated, Inventory, []),
    Identity = find_generated_identity(OtpDetail, Generated),
    case [
        Origin
        || Origin <- Generated,
           maps:get(generated_identity, Origin, undefined) =:= Identity
    ] of
        [Origin | _] ->
            Origin;
        [] ->
            nearest_line_origin(
                diagnostic_line(OtpDetail),
                Generated,
                default_origin(Context, Inventory)
            )
    end.

annotate(Node, Origin) ->
    Existing = [
        Annotation
        || Annotation <- cerl:get_ann(Node),
           not is_catena_origin(Annotation)
    ],
    cerl:set_ann(Node, otp_annotations(Origin) ++ Existing ++ [
        {catena_origin, Origin}
    ]).

origin(OriginKind, Construct, Location, State, Extra) ->
    StateContext = catena_codegen_utils:origin_context(State),
    maps:merge(
        #{
            origin => OriginKind,
            construct => Construct,
            module => maps:get(module, StateContext, undefined),
            transform => maps:get(transform, StateContext, undefined),
            generated_identity => undefined,
            location => Location,
            file => maps:get(file, StateContext, "nofile")
        },
        Extra
    ).

otp_annotations(Origin) ->
    Line = location_line(maps:get(location, Origin, undefined)),
    File = maps:get(file, Origin, "nofile"),
    line_annotation(Line) ++ file_annotation(File).

line_annotation(undefined) -> [];
line_annotation(Line) -> [Line].

file_annotation(undefined) -> [];
file_annotation(File) -> [{file, File}].

is_catena_origin({catena_origin, _}) -> true;
is_catena_origin(_) -> false.

generated_origin({Name, Arity} = Identity, Module, Symbols, Unit) ->
    case source_transform(Name, Arity, Symbols) of
        {ok, Symbol} ->
            #{
                origin => user,
                construct => transform,
                module => Module,
                transform => Name,
                generated_identity => Identity,
                location => maps:get(location, Symbol, undefined)
            };
        error ->
            synthetic_generated_origin(
                Identity,
                Module,
                Symbols,
                Unit
            )
    end.

source_transform(Name, Arity, Symbols) ->
    case [
        Symbol
        || Symbol <- Symbols,
           maps:get(kind, Symbol, undefined) =:= transform,
           maps:get(name, Symbol, undefined) =:= Name,
           maps:get(arity, Symbol, undefined) =:= Arity
    ] of
        [Symbol | _] -> {ok, Symbol};
        [] -> error
    end.

synthetic_generated_origin(
    {'$catena_dictionary', 2} = Identity,
    Module,
    _Symbols,
    Unit
) ->
    #{
        origin => synthetic,
        construct => trait_dictionary,
        module => Module,
        transform => undefined,
        generated_identity => Identity,
        location => module_location(Unit)
    };
synthetic_generated_origin({Name, Arity} = Identity, Module, Symbols, Unit) ->
    Prefix = "$catena_effect_entry$",
    NameString = atom_to_list(Name),
    case lists:prefix(Prefix, NameString) of
        true ->
            SourceName = list_to_atom(lists:nthtail(length(Prefix), NameString)),
            SourceArity = erlang:max(0, Arity - 1),
            Location = case source_transform(
                SourceName,
                SourceArity,
                Symbols
            ) of
                {ok, Symbol} -> maps:get(location, Symbol, undefined);
                error -> module_location(Unit)
            end,
            #{
                origin => synthetic,
                construct => effect_runtime_entry,
                module => Module,
                transform => SourceName,
                generated_identity => Identity,
                location => Location
            };
        false ->
            #{
                origin => synthetic,
                construct => generated_function,
                module => Module,
                transform => undefined,
                generated_identity => Identity,
                location => module_location(Unit)
            }
    end.

module_location(Unit) ->
    maps:get(module, catena_compilation_unit:locations(Unit), undefined).

find_generated_identity(Term, Generated) ->
    Identities = [
        maps:get(generated_identity, Origin)
        || Origin <- Generated
    ],
    find_identity(Term, Identities).

find_identity(Term, Identities) ->
    case lists:member(Term, Identities) of
        true ->
            Term;
        false when is_tuple(Term) ->
            find_identity(tuple_to_list(Term), Identities);
        false when is_list(Term) ->
            find_identity_list(Term, Identities);
        false when is_map(Term) ->
            find_identity(maps:to_list(Term), Identities);
        false ->
            undefined
    end.

find_identity_list([], _Identities) ->
    undefined;
find_identity_list([Term | Rest], Identities) ->
    case find_identity(Term, Identities) of
        undefined -> find_identity_list(Rest, Identities);
        Identity -> Identity
    end.

diagnostic_line({_File, Diagnostics}) when is_list(Diagnostics) ->
    diagnostic_line(Diagnostics);
diagnostic_line([{Line, _Module, _Reason} | _]) ->
    location_line(Line);
diagnostic_line([Diagnostic | Rest]) ->
    case diagnostic_line(Diagnostic) of
        undefined -> diagnostic_line(Rest);
        Line -> Line
    end;
diagnostic_line({Line, _Module, _Reason}) ->
    location_line(Line);
diagnostic_line(_) ->
    undefined.

nearest_line_origin(undefined, _Generated, Default) ->
    Default;
nearest_line_origin(Line, Generated, Default) ->
    case [
        Origin
        || Origin <- Generated,
           location_line(maps:get(location, Origin, undefined)) =:= Line
    ] of
        [Origin | _] -> Origin;
        [] -> Default#{location => {location, Line, 1}}
    end.

default_origin(Context, Inventory) ->
    #{
        origin => user,
        construct => module,
        module => maps:get(module, Context, maps:get(module, Inventory, undefined)),
        transform => maps:get(transform, Context, undefined),
        generated_identity => maps:get(
            generated_identity,
            Context,
            undefined
        ),
        location => maps:get(location, Context, module_inventory_location(Inventory))
    }.

module_inventory_location(Inventory) ->
    maps:get(
        module,
        maps:get(source_locations, Inventory, #{}),
        undefined
    ).

location_line({location, Line, _Column}) when is_integer(Line) ->
    Line;
location_line({Line, _Column}) when is_integer(Line) ->
    Line;
location_line(Line) when is_integer(Line) ->
    Line;
location_line(_) ->
    undefined.
