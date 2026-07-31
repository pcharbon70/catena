#!/usr/bin/env escript
%%! -noshell

main(Args) ->
    {Artifact, Root} = arguments(Args),
    Ebin = filename:join([Root, "_build", "default", "lib", "catena", "ebin"]),
    true = code:add_patha(Ebin),
    case code:ensure_loaded(catena_dialyzer_inventory) of
        {module, catena_dialyzer_inventory} ->
            report(Artifact, Root);
        {error, Reason} ->
            io:format(
                standard_error,
                "Unable to load catena_dialyzer_inventory from ~s: ~p~n"
                "Run `rebar3 compile` first.~n",
                [Ebin, Reason]
            ),
            halt(1)
    end.

arguments([ArtifactValue, RootValue]) ->
    {filename:absname(ArtifactValue), filename:absname(RootValue)};
arguments(_) ->
    io:format(
        standard_error,
        "Usage: escript scripts/dialyzer_inventory.escript <warning-artifact> <repo-root>~n",
        []
    ),
    halt(2).

report(Artifact, Root) ->
    case catena_dialyzer_inventory:read(Artifact, Root) of
        {ok, Warnings} ->
            Report = catena_dialyzer_inventory:summarize(Warnings),
            io:format("~s", [catena_dialyzer_inventory:format_report(Report)]);
        {error, Reason} ->
            io:format(standard_error, "Unable to inventory ~s: ~p~n", [Artifact, Reason]),
            halt(1)
    end.
