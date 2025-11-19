%%%-------------------------------------------------------------------
%%% @doc Enhanced type error formatting with highlighting
%%%
%%% This module provides rich formatting for type errors, showing
%%% expected vs actual types with visual highlighting of differences.
%%% It produces human-readable error messages with clear explanations
%%% of type mismatches and suggestions for fixes.
%%%
%%% ## Example Output
%%%
%%% ### Basic Type Mismatch (normal mode):
%%% ```
%%% Type mismatch in function application 'add':
%%%   Expected: Integer -> Integer -> Integer
%%%   Got:      String -> Integer -> Integer
%%%
%%% The parameter types don't match:
%%%   - Parameter 1: Expected Integer but got String
%%% ```
%%%
%%% ### Terse Mode (for IDEs):
%%% ```
%%% Expected Integer -> Integer, got String -> Integer
%%% ```
%%%
%%% ### Verbose Mode (with suggestions):
%%% ```
%%% Type mismatch in function application 'add':
%%%   Expected: Integer -> Integer -> Integer
%%%   Got:      String -> Integer -> Integer
%%%
%%% The parameter types don't match:
%%%   - Parameter 1: Expected Integer but got String
%%%
%%% Fix suggestions:
%%%   - Use String.to_integer to convert the string
%%%   - Check if you meant to use a numeric literal
%%%
%%% Examples:
%%%   Convert string to integer:
%%%     value = String.to_integer("42")
%%%     -- or handle parse errors:
%%%     case String.to_integer(str) of
%%%       Some(n) -> use_number(n)
%%%       None -> handle_error()
%%%     end
%%% ```
%%%
%%% ## Verbosity Levels
%%%
%%% The formatter supports three verbosity levels:
%%% - `terse`: Minimal output for space-constrained contexts
%%% - `normal`: Standard developer-facing output (default)
%%% - `verbose`: Full output with examples and suggestions
%%%
%%% Pass the verbosity level in the options map:
%%% ```erlang
%%% Opts = #{verbosity => verbose},
%%% format_type_mismatch(Expected, Actual, Context, Opts)
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_type_error_formatter).

%% API exports
-export([
    format_type_mismatch/3,
    format_type_mismatch/4,
    format_missing_instance/2,
    format_arity_mismatch/4,
    format_occurs_check/3,
    highlight_difference/2,
    format_error/1,
    format_error/2,
    format_error/3
]).

%% Internal exports for testing
-export([
    find_type_differences/2,
    format_type_with_highlight/2
]).

%% No test macros needed in production code

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Format a type error with optional context
-spec format_error(catena_type_error:type_error()) -> iolist().
format_error(Error) ->
    format_error(Error, #{}).

-spec format_error(catena_type_error:type_error(), map()) -> iolist().
format_error(Error, Context) ->
    format_error(Error, Context, #{}).

-spec format_error(catena_type_error:type_error(), map(), map()) -> iolist().
format_error({unification_error, Expected, Actual}, Context, Opts) ->
    format_type_mismatch(Expected, Actual, Context, Opts);
format_error({unsatisfied_constraint, Trait, Types, no_instance}, _Context, _Opts) ->
    format_missing_instance(Trait, Types);
format_error({arity_mismatch, Expected, Actual, FunName}, Context, _Opts) ->
    format_arity_mismatch(Expected, Actual, FunName, Context);
format_error({occurs_check, Var, Type}, Context, _Opts) ->
    format_occurs_check(Var, Type, Context);
format_error({unbound_variable, Var}, _Context, _Opts) ->
    io_lib:format("Unbound variable: ~s", [Var]);
format_error({ambiguous_type, Var}, _Context, _Opts) ->
    io_lib:format("Cannot resolve ambiguous type variable: ~s",
                  [catena_type_pp:pp_type(Var)]);
format_error(Other, _Context, _Opts) ->
    %% Fallback to basic formatting
    io_lib:format("Type error: ~p", [Other]).

%% @doc Format a type mismatch with highlighting
-spec format_type_mismatch(catena_types:ty(), catena_types:ty(), map()) -> iolist().
format_type_mismatch(Expected, Actual, Context) ->
    format_type_mismatch(Expected, Actual, Context, #{}).

-spec format_type_mismatch(catena_types:ty(), catena_types:ty(), map(), map()) -> iolist().
format_type_mismatch(Expected, Actual, Context, Opts) ->
    %% Extract verbosity level from options (defaults to normal)
    Verbosity = maps:get(verbosity, Opts, normal),

    case Verbosity of
        terse ->
            %% Terse mode: Minimal output for IDE tooltips or quick summaries
            ExpectedSimple = catena_type_pp:pp_type(Expected),
            ActualSimple = catena_type_pp:pp_type(Actual),
            io_lib:format("Expected ~s, got ~s", [ExpectedSimple, ActualSimple]);

        normal ->
            %% Normal mode: Standard developer-facing output with highlighting
            Diffs = find_type_differences(Expected, Actual),
            ExpectedFormatted = format_type_with_highlight(Expected,
                                                           [{role, expected} | Diffs]),
            ActualFormatted = format_type_with_highlight(Actual,
                                                         [{role, actual} | Diffs]),

            Header = case maps:get(expr, Context, undefined) of
                undefined -> "Type mismatch";
                Expr -> io_lib:format("Type mismatch in ~s", [format_expr_context(Expr)])
            end,

            Explanation = explain_type_difference(Expected, Actual),

            [
                Header, ":\n",
                "  Expected: ", ExpectedFormatted, "\n",
                "  Got:      ", ActualFormatted, "\n",
                case Explanation of
                    [] -> "";
                    Exp -> ["\n", Exp, "\n"]
                end
            ];

        verbose ->
            %% Verbose mode: Full output with examples and suggestions
            Diffs = find_type_differences(Expected, Actual),
            ExpectedFormatted = format_type_with_highlight(Expected,
                                                           [{role, expected} | Diffs]),
            ActualFormatted = format_type_with_highlight(Actual,
                                                         [{role, actual} | Diffs]),

            Header = case maps:get(expr, Context, undefined) of
                undefined -> "Type mismatch";
                Expr -> io_lib:format("Type mismatch in ~s", [format_expr_context(Expr)])
            end,

            Explanation = explain_type_difference(Expected, Actual),
            Suggestions = generate_suggestions(Expected, Actual),
            Examples = generate_examples(Expected, Actual),

            [
                Header, ":\n",
                "  Expected: ", ExpectedFormatted, "\n",
                "  Got:      ", ActualFormatted, "\n",
                case Explanation of
                    [] -> "";
                    Exp -> ["\n", Exp, "\n"]
                end,
                "\nFix suggestions:\n", Suggestions,
                "\nExamples:\n", Examples
            ]
    end.

%% @doc Format a missing trait instance error
-spec format_missing_instance(atom(), [catena_types:ty()]) -> iolist().
format_missing_instance(Trait, Types) ->
    TypesStr = string:join(
        [catena_type_pp:pp_type(T) || T <- Types],
        ", "
    ),
    [
        io_lib:format("No instance of '~s' for type(s): ~s\n", [Trait, TypesStr]),
        "\n",
        "Possible solutions:\n",
        "  1. Add an instance declaration:\n",
        io_lib:format("     instance ~s ~s where\n", [Trait, TypesStr]),
        "       -- implement required methods\n",
        "  2. Derive the trait if possible:\n",
        io_lib:format("     shape MyType = ... derives [~s]\n", [Trait]),
        "  3. Check if the type is correct for this operation\n"
    ].

%% @doc Format an arity mismatch error
-spec format_arity_mismatch(integer(), integer(), atom() | string(), map()) -> iolist().
format_arity_mismatch(Expected, Actual, FunName, _Context) ->
    Plurality = fun(1) -> "argument"; (_) -> "arguments" end,
    [
        io_lib:format("Function '~s' expects ~B ~s but got ~B\n",
                     [FunName, Expected, Plurality(Expected), Actual]),
        "\n",
        case Actual > Expected of
            true ->
                io_lib:format("Remove ~B extra ~s\n",
                            [Actual - Expected, Plurality(Actual - Expected)]);
            false ->
                io_lib:format("Add ~B more ~s\n",
                            [Expected - Actual, Plurality(Expected - Actual)])
        end
    ].

%% @doc Format an occurs check (infinite type) error
-spec format_occurs_check(catena_types:ty(), catena_types:ty(), map()) -> iolist().
format_occurs_check(Var, Type, _Context) ->
    [
        "Cannot construct infinite type:\n",
        io_lib:format("  ~s = ~s\n",
                     [catena_type_pp:pp_type(Var),
                      catena_type_pp:pp_type(Type)]),
        "\n",
        "This would create a type that references itself infinitely.\n",
        "\n",
        "Possible solutions:\n",
        "  1. Check if you're missing a base case in a recursive type\n",
        "  2. Consider wrapping the recursive reference in a container type\n",
        "  3. Ensure type variables are used correctly\n"
    ].

%% @doc Highlight differences between two types
-spec highlight_difference(catena_types:ty(), catena_types:ty()) ->
    {iolist(), iolist()}.
highlight_difference(Type1, Type2) ->
    Diffs = find_type_differences(Type1, Type2),
    Formatted1 = format_type_with_highlight(Type1, [{role, expected} | Diffs]),
    Formatted2 = format_type_with_highlight(Type2, [{role, actual} | Diffs]),
    {Formatted1, Formatted2}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% Maximum depth for type difference analysis to prevent stack overflow
%% on deeply nested or recursive types
-define(MAX_DIFF_DEPTH, 10).

%% @doc Find structural differences between two types
-spec find_type_differences(catena_types:ty(), catena_types:ty()) ->
    [{path, [atom() | integer()]} | {mismatch, atom()}].
find_type_differences(Type1, Type2) ->
    find_differences_impl(Type1, Type2, [], 0).

find_differences_impl(_Type1, _Type2, Path, Depth) when Depth > ?MAX_DIFF_DEPTH ->
    [{path, Path}, {mismatch, too_deep}];
find_differences_impl({tvar, V1}, {tvar, V2}, Path, _Depth) when V1 =/= V2 ->
    [{path, Path}, {mismatch, var}];
find_differences_impl({tcon, C1}, {tcon, C2}, Path, _Depth) when C1 =/= C2 ->
    [{path, Path}, {mismatch, constructor}];
find_differences_impl({tapp, T1, A1}, {tapp, T2, A2}, Path, Depth) ->
    NewDepth = Depth + 1,
    find_differences_impl(T1, T2, [app_fun | Path], NewDepth) ++
    find_differences_impl(A1, A2, [app_arg | Path], NewDepth);
find_differences_impl({tfun, P1, R1, E1}, {tfun, P2, R2, E2}, Path, Depth) ->
    NewDepth = Depth + 1,
    ParamDiffs = find_differences_impl(P1, P2, [param | Path], NewDepth),
    ReturnDiffs = find_differences_impl(R1, R2, [return | Path], NewDepth),
    EffectDiffs = case {E1, E2} of
        {E, E} -> [];
        _ -> [{path, [effects | Path]}, {mismatch, effects}]
    end,
    ParamDiffs ++ ReturnDiffs ++ EffectDiffs;
find_differences_impl({trecord, Fields1, _}, {trecord, Fields2, _}, Path, Depth) ->
    find_record_differences(Fields1, Fields2, Path, Depth + 1);
find_differences_impl({tvariant, Ctors1}, {tvariant, Ctors2}, Path, Depth) ->
    find_variant_differences(Ctors1, Ctors2, Path, Depth + 1);
find_differences_impl({ttuple, Elems1}, {ttuple, Elems2}, Path, Depth) ->
    find_tuple_differences(Elems1, Elems2, Path, Depth + 1);
find_differences_impl(_, _, Path, _Depth) when Path =/= [] ->
    [{path, Path}, {mismatch, structure}];
find_differences_impl(_, _, [], _Depth) ->
    [{mismatch, root}].

%% @doc Find differences in record fields
find_record_differences(Fields1, Fields2, Path, Depth) ->
    case Depth > ?MAX_DIFF_DEPTH of
        true ->
            [{path, Path}, {mismatch, too_deep}];
        false ->
            F1Map = maps:from_list(Fields1),
            F2Map = maps:from_list(Fields2),
            Keys = sets:to_list(sets:union(
                sets:from_list(maps:keys(F1Map)),
                sets:from_list(maps:keys(F2Map))
            )),
            lists:flatmap(fun(Key) ->
                case {maps:find(Key, F1Map), maps:find(Key, F2Map)} of
                    {{ok, T1}, {ok, T2}} ->
                        find_differences_impl(T1, T2, [{field, Key} | Path], Depth);
                    {{ok, _}, error} ->
                        [{path, [{field, Key} | Path]}, {mismatch, missing_field}];
                    {error, {ok, _}} ->
                        [{path, [{field, Key} | Path]}, {mismatch, extra_field}];
                    _ -> []
                end
            end, Keys)
    end.

%% @doc Find differences in variant constructors
find_variant_differences(Ctors1, Ctors2, Path, Depth) ->
    case Depth > ?MAX_DIFF_DEPTH of
        true ->
            [{path, Path}, {mismatch, too_deep}];
        false ->
            C1Map = maps:from_list(Ctors1),
            C2Map = maps:from_list(Ctors2),
            Keys = sets:to_list(sets:union(
                sets:from_list(maps:keys(C1Map)),
                sets:from_list(maps:keys(C2Map))
            )),
            lists:flatmap(fun(Key) ->
                case {maps:find(Key, C1Map), maps:find(Key, C2Map)} of
                    {{ok, T1}, {ok, T2}} ->
                        find_differences_impl(T1, T2, [{constructor, Key} | Path], Depth);
                    {{ok, _}, error} ->
                        [{path, [{constructor, Key} | Path]}, {mismatch, missing_ctor}];
                    {error, {ok, _}} ->
                        [{path, [{constructor, Key} | Path]}, {mismatch, extra_ctor}];
                    _ -> []
                end
            end, Keys)
    end.

%% @doc Find differences in tuple elements
find_tuple_differences(Elems1, Elems2, Path, Depth) ->
    case Depth > ?MAX_DIFF_DEPTH of
        true ->
            [{path, Path}, {mismatch, too_deep}];
        false ->
            case length(Elems1) =:= length(Elems2) of
                true ->
                    lists:append([
                        find_differences_impl(E1, E2, [{elem, I} | Path], Depth)
                        || {I, E1, E2} <- lists:zip3(
                            lists:seq(1, length(Elems1)),
                            Elems1, Elems2
                        )
                    ]);
                false ->
                    [{path, Path}, {mismatch, tuple_arity}]
            end
    end.

%% @doc Format a type with highlighting for differences
-spec format_type_with_highlight(catena_types:ty(), list()) -> iolist().
format_type_with_highlight(Type, Highlights) ->
    %% Use ANSI colors if terminal supports it
    UseColor = application:get_env(catena, use_color, true),
    format_type_impl(Type, Highlights, [], UseColor).

format_type_impl({tvar, Var}, Highlights, Path, UseColor) ->
    Str = io_lib:format("~s", [Var]),
    maybe_highlight(Str, Path, Highlights, UseColor);

format_type_impl({tcon, Con}, Highlights, Path, UseColor) ->
    Str = atom_to_list(Con),
    maybe_highlight(Str, Path, Highlights, UseColor);

format_type_impl({tapp, Type, Arg}, Highlights, Path, UseColor) ->
    [
        format_type_impl(Type, Highlights, [app_fun | Path], UseColor),
        "<",
        format_type_impl(Arg, Highlights, [app_arg | Path], UseColor),
        ">"
    ];

format_type_impl({tfun, Param, Return, Effects}, Highlights, Path, UseColor) ->
    EffectStr = case Effects of
        {effect_set, []} -> "";
        {effect_set, EffList} ->
            [" / {", string:join([atom_to_list(E) || E <- EffList], ", "), "}"];
        _ -> ""
    end,
    [
        format_type_impl(Param, Highlights, [param | Path], UseColor),
        " -> ",
        format_type_impl(Return, Highlights, [return | Path], UseColor),
        maybe_highlight(EffectStr, [effects | Path], Highlights, UseColor)
    ];

format_type_impl({trecord, Fields, _Openness}, Highlights, Path, UseColor) ->
    FieldStrs = [
        [atom_to_list(Name), ": ",
         format_type_impl(Type, Highlights, [{field, Name} | Path], UseColor)]
        || {Name, Type} <- Fields
    ],
    ["{", string:join(FieldStrs, ", "), "}"];

format_type_impl({tvariant, Ctors}, Highlights, Path, UseColor) ->
    CtorStrs = [
        [atom_to_list(Name),
         case Type of
            {ttuple, []} -> "";
            T -> ["(", format_type_impl(T, Highlights,
                                       [{constructor, Name} | Path], UseColor), ")"]
         end]
        || {Name, Type} <- Ctors
    ],
    string:join(CtorStrs, " | ");

format_type_impl({ttuple, Elems}, Highlights, Path, UseColor) ->
    ElemStrs = [
        format_type_impl(E, Highlights, [{elem, I} | Path], UseColor)
        || {I, E} <- lists:zip(lists:seq(1, length(Elems)), Elems)
    ],
    ["(", string:join(ElemStrs, ", "), ")"];

format_type_impl(Type, _Highlights, _Path, _UseColor) ->
    %% Fallback to basic pretty printing
    catena_type_pp:pp_type(Type).

%% @doc Maybe highlight text based on path and highlights
maybe_highlight(Text, Path, Highlights, UseColor) ->
    case should_highlight(Path, Highlights) of
        {true, Style} when UseColor ->
            apply_ansi_style(Text, Style);
        _ ->
            Text
    end.

%% @doc Check if path should be highlighted
should_highlight(Path, Highlights) ->
    case lists:keyfind(path, 1, Highlights) of
        {path, HighPath} when Path =:= HighPath ->
            Role = proplists:get_value(role, Highlights, neutral),
            Mismatch = proplists:get_value(mismatch, Highlights, none),
            {true, style_for_mismatch(Mismatch, Role)};
        _ ->
            false
    end.

%% @doc Get ANSI style for mismatch type
style_for_mismatch(constructor, expected) -> bold_red;
style_for_mismatch(constructor, actual) -> bold_yellow;
style_for_mismatch(missing_field, _) -> red_underline;
style_for_mismatch(extra_field, _) -> yellow_underline;
style_for_mismatch(effects, _) -> cyan;
style_for_mismatch(_, _) -> bold.

%% @doc Apply ANSI style to text
apply_ansi_style(Text, bold) ->
    ["\033[1m", Text, "\033[0m"];
apply_ansi_style(Text, bold_red) ->
    ["\033[1;31m", Text, "\033[0m"];
apply_ansi_style(Text, bold_yellow) ->
    ["\033[1;33m", Text, "\033[0m"];
apply_ansi_style(Text, red_underline) ->
    ["\033[4;31m", Text, "\033[0m"];
apply_ansi_style(Text, yellow_underline) ->
    ["\033[4;33m", Text, "\033[0m"];
apply_ansi_style(Text, cyan) ->
    ["\033[36m", Text, "\033[0m"];
apply_ansi_style(Text, _) ->
    Text.

%% @doc Explain type differences in plain English
explain_type_difference({tcon, C1}, {tcon, C2}) ->
    io_lib:format("These types are incompatible:\n"
                 "  - Expected type '~s'\n"
                 "  - Got type '~s'\n", [C1, C2]);

explain_type_difference({tfun, _, _, _}, Type) when element(1, Type) =/= tfun ->
    "Cannot use a non-function value where a function is expected";

explain_type_difference(Type, {tfun, _, _, _}) when element(1, Type) =/= tfun ->
    "Cannot use a function where a value is expected";

explain_type_difference({tapp, {tcon, list}, _}, {tapp, {tcon, maybe_type}, _}) ->
    "Lists and Maybe values are different container types:\n"
    "  - List can contain zero or more elements\n"
    "  - Maybe contains zero or one element\n"
    "Consider using 'map' if you need to transform the container contents";

explain_type_difference({trecord, F1, _}, {trecord, F2, _}) ->
    Missing = [Name || {Name, _} <- F1, not lists:keymember(Name, 1, F2)],
    Extra = [Name || {Name, _} <- F2, not lists:keymember(Name, 1, F1)],
    case {Missing, Extra} of
        {[], []} -> "Record types have incompatible field types";
        {[], E} -> io_lib:format("Record has unexpected fields: ~p", [E]);
        {M, []} -> io_lib:format("Record is missing required fields: ~p", [M]);
        {M, E} -> io_lib:format("Record has missing fields ~p and unexpected fields ~p",
                               [M, E])
    end;

explain_type_difference(_, _) ->
    [].

%% @doc Format expression context for error messages
format_expr_context({app, _, {var, _, Name}, _}) ->
    io_lib:format("function application '~s'", [Name]);
format_expr_context({binop, _, Op, _, _}) ->
    io_lib:format("binary operation '~s'", [Op]);
format_expr_context({'let', _, _, _, _}) ->
    "let binding";
format_expr_context({match, _, _, _}) ->
    "match expression";
format_expr_context(_) ->
    "expression".

%% @doc Generate fix suggestions based on type mismatch patterns
-spec generate_suggestions(catena_types:ty(), catena_types:ty()) -> iolist().
generate_suggestions({tfun, _, _, _}, Type) when element(1, Type) =/= tfun ->
    [
        "  - Add parentheses if you meant to call the function\n",
        "  - Check if you're passing a function where a value is expected\n"
    ];
generate_suggestions({tcon, integer}, {tcon, string}) ->
    [
        "  - Use String.to_integer to convert the string\n",
        "  - Check if you meant to use a numeric literal\n"
    ];
generate_suggestions({tcon, string}, {tcon, integer}) ->
    [
        "  - Use Integer.to_string to convert the number\n",
        "  - Add quotes around the value to make it a string\n"
    ];
generate_suggestions({tapp, {tcon, list}, _}, {tapp, {tcon, maybe_type}, _}) ->
    [
        "  - Use List.head to get the first element as a Maybe\n",
        "  - Use pattern matching to handle the list\n"
    ];
generate_suggestions(_, _) ->
    [
        "  - Check the type signatures of the functions involved\n",
        "  - Add explicit type annotations to clarify intent\n",
        "  - Verify the expected types match your usage\n"
    ].

%% @doc Generate examples for common type fixes
-spec generate_examples(catena_types:ty(), catena_types:ty()) -> iolist().
generate_examples({tcon, integer}, {tcon, string}) ->
    [
        "  Convert string to integer:\n",
        "    value = String.to_integer(\"42\")\n",
        "    -- or handle parse errors:\n",
        "    case String.to_integer(str) of\n",
        "      Some(n) -> use_number(n)\n",
        "      None -> handle_error()\n",
        "    end\n"
    ];
generate_examples({tapp, {tcon, list}, _}, _) ->
    [
        "  Working with lists:\n",
        "    List.map(fn x -> x + 1, my_list)\n",
        "    List.filter(fn x -> x > 0, my_list)\n",
        "    List.head(my_list)  -- returns Maybe\n"
    ];
generate_examples(_, _) ->
    "  Consult the documentation for specific type conversion functions.\n".

