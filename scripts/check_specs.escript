#!/usr/bin/env escript
%%! -noshell

main(Args) ->
    Root = case Args of
        [] ->
            filename:absname(filename:join(filename:dirname(escript:script_name()), ".."));
        [RootValue] ->
            filename:absname(RootValue);
        _ ->
            usage()
    end,
    Ebin = filename:join([Root, "_build", "default", "lib", "catena", "ebin"]),
    true = code:add_patha(Ebin),
    case code:ensure_loaded(catena_specs_governance) of
        {module, catena_specs_governance} ->
            run(Root);
        {error, Reason} ->
            io:format(
                standard_error,
                "Unable to load catena_specs_governance from ~s: ~p~n"
                "Run `rebar3 compile` first or use `make check-specs`.~n",
                [Ebin, Reason]
            ),
            halt(1)
    end.

run(Root) ->
    case catena_specs_governance:validate(Root) of
        {ok, Report} ->
            io:format("~s~n", [catena_specs_governance:format_report(Report)]);
        {error, Errors} ->
            io:format(
                standard_error,
                "~s",
                [catena_specs_governance:format_errors(Errors)]
            ),
            halt(1)
    end.

usage() ->
    io:format(standard_error, "Usage: escript scripts/check_specs.escript [repo-root]~n", []),
    halt(2).
