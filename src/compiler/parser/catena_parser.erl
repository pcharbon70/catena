%% This file is generated from catena_parser.yrl. Do not edit directly.
-file("src/compiler/parser/catena_parser.yrl", 0).
-module(catena_parser).
-file("src/compiler/parser/catena_parser.erl", 4).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("src/compiler/parser/catena_parser.yrl", 1213).

%% @doc Extract atom from token
%% Delegates to catena_compiler_utils to avoid code duplication
extract_atom(Token) -> catena_compiler_utils:extract_atom(Token).

%% @doc Extract value from token
%% Delegates to catena_compiler_utils to avoid code duplication
extract_value(Token) -> catena_compiler_utils:extract_value(Token).

%% @doc Extract location from token or AST node
%% Delegates to catena_compiler_utils to avoid code duplication
%% Supports both legacy {line, N} format and enhanced {location, ...} format
extract_location(Node) -> catena_compiler_utils:extract_location(Node).

%% @doc Extract transform name from transform signature
extract_transform_name({transform_sig, Name, _Type, _Loc}) -> Name.

%% @doc Extract transform type from transform signature
extract_transform_type({transform_sig, _Name, Type, _Loc}) -> Type.

%% @doc Create an error declaration node for error recovery
%% Returns a special AST node that marks a parsing error
make_error_declaration(Location, Message, _ErrorInfo) ->
    {error_decl,
        Message,
        Location}.

%% @doc Extract trait constraint from type expression
%% Delegates to catena_compiler_utils for centralized implementation
%% This helper is shared between parser and type checker
extract_trait_constraint(TypeExpr) ->
    catena_compiler_utils:extract_trait_constraint(TypeExpr).

%% @doc Extract import declarations from a list of declarations
%% Returns list of {import, ModuleName, Loc} tuples
extract_imports(Decls) ->
    [Import || {import, _, _, _, _, _} = Import <- Decls].

%% @doc Filter out import declarations from a list of declarations
%% Returns all non-import declarations
filter_imports(Decls) ->
    [D || D <- Decls, element(1, D) =/= import].



-file("/Users/Pascal/.asdf/installs/erlang/28.3.1/lib/parsetools-2.7/include/yeccpre.hrl", 0).
%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1996-2025. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

-type yecc_ret() :: {'error', _} | {'ok', _}.

-ifdef (YECC_PARSE_DOC).
-doc ?YECC_PARSE_DOC.
-endif.
-spec parse(Tokens :: list()) -> yecc_ret().
parse(Tokens) ->
    yeccpars0(Tokens, {no_func, no_location}, 0, [], []).

-ifdef (YECC_PARSE_AND_SCAN_DOC).
-doc ?YECC_PARSE_AND_SCAN_DOC.
-endif.
-spec parse_and_scan({function() | {atom(), atom()}, [_]}
                     | {atom(), atom(), [_]}) -> yecc_ret().
parse_and_scan({F, A}) ->
    yeccpars0([], {{F, A}, no_location}, 0, [], []);
parse_and_scan({M, F, A}) ->
    Arity = length(A),
    yeccpars0([], {{fun M:F/Arity, A}, no_location}, 0, [], []).

-ifdef (YECC_FORMAT_ERROR_DOC).
-doc ?YECC_FORMAT_ERROR_DOC.
-endif.
-spec format_error(any()) -> [char() | list()].
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true ->
            Message;
        _ ->
            io_lib:write(Message)
    end.

%% To be used in grammar files to throw an error message to the parser
%% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function, return_error/2}).
-spec return_error(erl_anno:location(), any()) -> no_return().
return_error(Location, Message) ->
    throw({error, {Location, ?MODULE, Message}}).

-define(CODE_VERSION, "1.4").

yeccpars0(Tokens, Tzr, State, States, Vstack) ->
    try yeccpars1(Tokens, Tzr, State, States, Vstack)
    catch 
        error: Error: Stacktrace ->
            try yecc_error_type(Error, Stacktrace) of
                Desc ->
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                 Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        %% Probably thrown from return_error/2:
        throw: {error, {_Location, ?MODULE, _M}} = Error ->
            Error
    end.

yecc_error_type(function_clause, [{?MODULE,F,ArityOrArgs,_} | _]) ->
    case atom_to_list(F) of
        "yeccgoto_" ++ SymbolL ->
            {ok,[{atom,_,Symbol}],_} = erl_scan:string(SymbolL),
            State = case ArityOrArgs of
                        [S,_,_,_,_,_,_] -> S;
                        _ -> state_is_unknown
                    end,
            {Symbol, State, missing_in_goto_table}
    end.

yeccpars1([Token | Tokens], Tzr, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, Tzr);
yeccpars1([], {{F, A},_Location}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, EndLocation} ->
            yeccpars1(Tokens, {{F, A}, EndLocation}, State, States, Vstack);
        {eof, EndLocation} ->
            yeccpars1([], {no_func, EndLocation}, State, States, Vstack);
        {error, Descriptor, _EndLocation} ->
            {error, Descriptor}
    end;
yeccpars1([], {no_func, no_location}, State, States, Vstack) ->
    Line = 999999,
    yeccpars2(State, '$end', States, Vstack, yecc_end(Line), [],
              {no_func, Line});
yeccpars1([], {no_func, EndLocation}, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, yecc_end(EndLocation), [],
              {no_func, EndLocation}).

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Token0, [Token | Tokens], Tzr) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Token0 | Vstack], Token, Tokens, Tzr);
yeccpars1(State1, State, States, Vstack, Token0, [], {{_F,_A}, _Location}=Tzr) ->
    yeccpars1([], Tzr, State, [State1 | States], [Token0 | Vstack]);
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, no_location}) ->
    Location = yecctoken_end_location(Token0),
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Location), [], {no_func, Location});
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, Location}) ->
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Location), [], {no_func, Location}).

%% For internal use only.
yecc_end(Location) ->
    {'$end', Location}.

yecctoken_end_location(Token) ->
    try erl_anno:end_location(element(2, Token)) of
        undefined -> yecctoken_location(Token);
        Loc -> Loc
    catch _:_ -> yecctoken_location(Token)
    end.

-compile({nowarn_unused_function, yeccerror/1}).
yeccerror(Token) ->
    Text = yecctoken_to_string(Token),
    Location = yecctoken_location(Token),
    {error, {Location, ?MODULE, ["syntax error before: ", Text]}}.

-compile({nowarn_unused_function, yecctoken_to_string/1}).
yecctoken_to_string(Token) ->
    try erl_scan:text(Token) of
        undefined -> yecctoken2string(Token);
        Txt -> Txt
    catch _:_ -> yecctoken2string(Token)
    end.

yecctoken_location(Token) ->
    try erl_scan:location(Token)
    catch _:_ -> element(2, Token)
    end.

-compile({nowarn_unused_function, yecctoken2string/1}).
yecctoken2string(Token) ->
    try
        yecctoken2string1(Token)
    catch
        _:_ ->
            io_lib:format("~tp", [Token])
    end.

-compile({nowarn_unused_function, yecctoken2string1/1}).
yecctoken2string1({atom, _, A}) -> io_lib:write_atom(A);
yecctoken2string1({integer,_,N}) -> io_lib:write(N);
yecctoken2string1({float,_,F}) -> io_lib:write(F);
yecctoken2string1({char,_,C}) -> io_lib:write_char(C);
yecctoken2string1({var,_,V}) -> io_lib:format("~s", [V]);
yecctoken2string1({string,_,S}) -> io_lib:write_string(S);
yecctoken2string1({reserved_symbol, _, A}) -> io_lib:write(A);
yecctoken2string1({_Cat, _, Val}) -> io_lib:format("~tp", [Val]);
yecctoken2string1({dot, _}) -> "'.'";
yecctoken2string1({'$end', _}) -> [];
yecctoken2string1({Other, _}) when is_atom(Other) ->
    io_lib:write_atom(Other);
yecctoken2string1(Other) ->
    io_lib:format("~tp", [Other]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("src/compiler/parser/catena_parser.erl", 242).

-dialyzer({nowarn_function, yeccpars2/7}).
-compile({nowarn_unused_function,  yeccpars2/7}).
yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_37(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_49(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_52(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(57=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_59(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(60=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(61=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_61(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(62=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_62(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(63=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(64=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(65=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_65(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(66=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_66(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(67=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(68=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_68(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(69=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(70=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(71=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(72=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(74=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(75=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_76(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(77=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(78=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(80=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(82=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(84=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(85=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_85(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(86=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(87=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_87(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_99(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_100(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_102(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(103=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(104=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(105=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_105(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(106=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(107=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(108=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(109=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(110=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(111=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(112=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(113=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(114=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(115=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_115(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(116=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_116(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(117=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_117(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(118=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(119=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(120=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(121=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_121(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(122=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(123=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(124=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_124(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(125=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(126=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(127=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_127(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(128=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(129=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_129(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(130=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_130(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(131=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_131(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(132=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(133=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_133(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(134=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_134(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(135=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(136=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_136(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(137=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(138=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(139=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_139(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(140=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_140(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(141=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(142=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(143=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(144=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_144(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(145=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(146=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_146(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(147=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(148=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_148(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(149=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(150=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(151=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(152=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_152(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(153=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_153(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(154=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_154(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(155=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_155(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(156=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_156(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(157=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_157(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(158=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_158(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(159=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_159(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(160=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_160(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(161=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_161(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(162=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(163=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_163(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(164=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(165=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_165(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(166=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(167=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(168=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_168(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(169=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_169(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(170=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(171=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_171(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(172=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_172(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(173=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_173(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(174=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(175=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_175(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(176=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_176(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(177=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_177(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(178=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_178(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(179=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_179(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(180=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_180(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(181=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_181(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(182=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(183=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(184=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(185=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(186=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(187=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(188=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(189=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(190=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(191=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(192=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(193=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(194=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(195=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(196=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(197=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(198=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(199=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(200=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(201=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(202=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(203=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(204=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(205=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_205(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(206=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_206(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(207=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_207(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(208=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_208(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(209=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_209(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(210=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_210(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(211=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_211(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(212=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_212(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(213=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_213(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(214=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_214(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(215=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_215(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(216=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_216(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(217=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_217(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(218=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_218(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(219=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_219(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(220=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_220(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(221=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_221(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(222=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_222(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(223=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_223(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(224=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_224(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(225=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_225(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(226=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_226(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(227=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_227(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(228=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_228(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(229=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_229(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(230=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_230(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(231=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_231(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(232=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(233=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_233(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(234=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_234(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(235=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(236=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(237=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_237(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(238=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(239=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_239(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(240=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_240(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(241=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(242=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_242(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(243=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(244=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_244(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(245=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_245(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(246=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_246(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(247=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_247(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(248=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_248(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(249=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_249(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(250=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(251=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_251(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(252=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_252(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(253=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_253(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(254=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_254(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(255=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_255(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(256=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(257=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_257(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(258=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(259=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_259(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(260=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_260(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(261=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_261(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(262=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_262(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(263=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_263(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(264=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_264(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(265=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_265(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(266=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_266(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(267=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(268=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_268(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(269=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_269(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(270=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_270(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(271=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_271(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(272=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_272(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(273=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_273(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(274=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_274(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(275=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_275(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(276=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_276(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(277=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_277(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(278=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_278(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(279=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_279(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(280=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_280(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(281=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_281(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(282=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(283=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_283(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(284=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_284(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(285=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_285(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(286=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_286(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(287=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_287(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(288=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_288(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(289=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(290=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_290(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(291=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_291(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(292=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_292(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(293=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_293(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(294=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_294(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(295=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_295(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(296=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_296(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(297=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(298=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_298(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(299=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_299(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(300=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_300(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(301=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_301(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(302=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_302(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(303=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_303(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(304=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_304(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(305=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(306=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_306(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(307=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_307(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(308=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(309=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_309(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(310=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_299(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(311=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_311(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(312=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_312(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(313=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_313(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(314=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_314(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(315=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_315(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(316=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(317=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_317(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(318=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(319=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_319(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(320=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_320(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(321=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(322=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_322(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(323=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_323(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(324=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_324(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(325=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_325(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(326=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_326(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(327=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(328=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_328(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(329=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_329(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(330=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(331=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_331(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(332=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_332(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(333=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_333(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(334=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_334(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(335=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_335(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(336=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_336(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(337=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_337(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(338=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_338(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(339=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_339(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(340=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(341=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_341(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(342=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_342(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(343=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_343(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(344=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_344(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(345=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_345(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(346=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_346(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(347=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(348=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_348(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(349=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_349(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(350=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_350(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(351=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_351(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(352=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_352(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(353=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_353(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(354=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_354(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(355=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_355(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(356=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_356(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(357=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_357(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(358=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_358(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(359=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_353(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(360=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_360(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(361=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(362=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_362(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(363=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_363(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(364=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_364(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(365=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_365(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(366=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_366(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(367=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_367(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(368=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_368(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(369=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_369(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(370=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_370(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(371=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_371(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(372=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_372(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(373=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_373(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(374=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_374(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(375=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_375(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(376=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_376(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(377=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_377(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(378=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_378(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(379=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(380=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_380(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(381=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_381(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(382=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_382(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(383=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_383(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(384=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_384(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(385=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_385(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(386=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_386(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(387=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_387(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(388=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_388(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(389=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_389(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(390=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_390(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(391=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_391(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(392=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_392(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(393=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(394=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_394(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(395=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_395(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(396=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_396(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(397=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_397(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(398=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_398(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(399=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_399(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(400=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_400(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(401=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_398(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(402=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_402(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(403=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_403(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(404=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_404(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(405=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_398(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(406=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_406(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(407=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_407(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(408=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_408(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(409=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_409(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(410=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_410(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(411=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_411(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(412=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_412(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(413=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_413(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(414=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_398(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(415=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_415(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(416=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_416(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(417=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_417(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(418=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_398(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(419=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_419(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(420=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_420(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(421=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_421(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(422=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_422(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(423=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_423(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(424=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_424(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(425=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_425(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(426=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_426(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(427=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_427(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(428=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_428(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(429=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_429(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(430=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_430(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(431=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_431(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(432=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(433=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_433(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(434=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_434(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(435=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_435(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(436=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_436(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(437=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_437(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(438=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_438(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(439=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_439(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(440=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_440(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(441=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_441(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(442=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_442(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(443=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_443(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(444=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_444(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(445=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_445(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(446=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_446(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(447=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_447(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(448=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_448(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(449=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_449(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(450=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_450(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(451=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_451(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(452=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_452(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(453=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_453(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(454=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_454(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(455=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_455(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(456=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(457=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(458=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_458(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(459=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(460=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_460(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(461=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_461(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(462=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_462(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.4",{missing_state_in_action_table, Other}}).

yeccpars2_0(S, 'module', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_1/7}).
-compile({nowarn_unused_function,  yeccpars2_1/7}).
yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_1_(Stack),
 yeccgoto_declaration(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_2/7}).
-compile({nowarn_unused_function,  yeccpars2_2/7}).
yeccpars2_2(S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 453, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_2_(Stack),
 yeccgoto_transform_decl(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_3/7}).
-compile({nowarn_unused_function,  yeccpars2_3/7}).
yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_3_(Stack),
 yeccgoto_declaration(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_4/7}).
-compile({nowarn_unused_function,  yeccpars2_4/7}).
yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_4_(Stack),
 yeccgoto_declaration(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_5/7}).
-compile({nowarn_unused_function,  yeccpars2_5/7}).
yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_5_(Stack),
 yeccgoto_declaration(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_6/7}).
-compile({nowarn_unused_function,  yeccpars2_6/7}).
yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_6_(Stack),
 yeccgoto_declaration(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_7/7}).
-compile({nowarn_unused_function,  yeccpars2_7/7}).
yeccpars2_7(S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, 'import', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, 'instance', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, 'property', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, 'test', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, 'trait', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, 'type', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_8/7}).
-compile({nowarn_unused_function,  yeccpars2_8/7}).
yeccpars2_8(S, 'export', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 440, Ss, Stack, T, Ts, Tzr);
yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_8_(Stack),
 yeccgoto_module_header(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_9/7}).
-compile({nowarn_unused_function,  yeccpars2_9/7}).
yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_9_(Stack),
 yeccgoto_declaration(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_10/7}).
-compile({nowarn_unused_function,  yeccpars2_10/7}).
yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_(Stack),
 yeccgoto_declaration(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_11/7}).
-compile({nowarn_unused_function,  yeccpars2_11/7}).
yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_11_(Stack),
 yeccgoto_declaration(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_12/7}).
-compile({nowarn_unused_function,  yeccpars2_12/7}).
yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_12_(Stack),
 yeccgoto_catena_module(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_13/7}).
-compile({nowarn_unused_function,  yeccpars2_13/7}).
yeccpars2_13(S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, 'import', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, 'instance', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, 'property', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, 'test', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, 'trait', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, 'type', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_13_(Stack),
 yeccgoto_declarations(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_14/7}).
-compile({nowarn_unused_function,  yeccpars2_14/7}).
yeccpars2_14(_S, '$end', _Ss, Stack, _T, _Ts, _Tzr) ->
 {ok, hd(Stack)};
yeccpars2_14(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_15/7}).
-compile({nowarn_unused_function,  yeccpars2_15/7}).
yeccpars2_15(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 426, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 427, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_16/7}).
-compile({nowarn_unused_function,  yeccpars2_16/7}).
yeccpars2_16(S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 421, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, 'instance', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 422, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, 'trait', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 423, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 424, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, 'type', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 425, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_17/7}).
-compile({nowarn_unused_function,  yeccpars2_17/7}).
yeccpars2_17(S, 'qualified', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 395, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 396, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_18(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 368, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 369, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_18(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_18/7}).
-compile({nowarn_unused_function,  yeccpars2_18/7}).
yeccpars2_cont_18(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_18(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_18(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_19/7}).
-compile({nowarn_unused_function,  yeccpars2_19/7}).
yeccpars2_19(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 363, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_20/7}).
-compile({nowarn_unused_function,  yeccpars2_20/7}).
yeccpars2_20(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 349, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 350, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_21/7}).
-compile({nowarn_unused_function,  yeccpars2_21/7}).
yeccpars2_21(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 345, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 346, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_22/7}).
-compile({nowarn_unused_function,  yeccpars2_22/7}).
yeccpars2_22(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 323, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 324, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_23/7}).
-compile({nowarn_unused_function,  yeccpars2_23/7}).
yeccpars2_23(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_24/7}).
-compile({nowarn_unused_function,  yeccpars2_24/7}).
yeccpars2_24(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_25/7}).
-compile({nowarn_unused_function,  yeccpars2_25/7}).
yeccpars2_25(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 90, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_25_(Stack),
 yeccgoto_type_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_26/7}).
-compile({nowarn_unused_function,  yeccpars2_26/7}).
yeccpars2_26(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_26_(Stack),
 yeccpars2_28(28, Cat, [26 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_27/7}).
-compile({nowarn_unused_function,  yeccpars2_27/7}).
yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_27_(Stack),
 yeccgoto_type_params(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_28/7}).
-compile({nowarn_unused_function,  yeccpars2_28/7}).
yeccpars2_28(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_29/7}).
-compile({nowarn_unused_function,  yeccpars2_29/7}).
yeccpars2_29(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_29_(Stack),
 yeccgoto_type_params_nonempty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_30/7}).
-compile({nowarn_unused_function,  yeccpars2_30/7}).
yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_30_(Stack),
 yeccgoto_type_params_nonempty(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_31/7}).
-compile({nowarn_unused_function,  yeccpars2_31/7}).
yeccpars2_31(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_31(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_32/7}).
-compile({nowarn_unused_function,  yeccpars2_32/7}).
yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_32_(Stack),
 yeccgoto_type_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_33/7}).
-compile({nowarn_unused_function,  yeccpars2_33/7}).
yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_33_(Stack),
 yeccgoto_type_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_34/7}).
-compile({nowarn_unused_function,  yeccpars2_34/7}).
yeccpars2_34(S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 88, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_34_(Stack),
 yeccgoto_constructors(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_35/7}).
-compile({nowarn_unused_function,  yeccpars2_35/7}).
yeccpars2_35(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_35(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_35(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_35(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_35_(Stack),
 yeccgoto_constructor(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_36/7}).
-compile({nowarn_unused_function,  yeccpars2_36/7}).
yeccpars2_36(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_36_(Stack),
 yeccgoto_constructor_fields(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_37/7}).
-compile({nowarn_unused_function,  yeccpars2_37/7}).
yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_37_(Stack),
 yeccgoto_constructor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_38/7}).
-compile({nowarn_unused_function,  yeccpars2_38/7}).
yeccpars2_38(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_39/7}).
-compile({nowarn_unused_function,  yeccpars2_39/7}).
yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_39_(Stack),
 yeccgoto_type_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_40(S, 'forall', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_40(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_40(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_18(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_41/7}).
-compile({nowarn_unused_function,  yeccpars2_41/7}).
yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_41_(Stack),
 yeccgoto_type_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_42/7}).
-compile({nowarn_unused_function,  yeccpars2_42/7}).
yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_42_(Stack),
 yeccgoto_type_expr_app(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_43/7}).
-compile({nowarn_unused_function,  yeccpars2_43/7}).
yeccpars2_43(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_43_(Stack),
 yeccgoto_type_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_44/7}).
-compile({nowarn_unused_function,  yeccpars2_44/7}).
yeccpars2_44(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, 'constrain', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_45/7}).
-compile({nowarn_unused_function,  yeccpars2_45/7}).
yeccpars2_45(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_45_(Stack),
 yeccpars2_52(52, Cat, [45 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_46/7}).
-compile({nowarn_unused_function,  yeccpars2_46/7}).
yeccpars2_46(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_46_(Stack),
 yeccgoto_type_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_47/7}).
-compile({nowarn_unused_function,  yeccpars2_47/7}).
yeccpars2_47(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_47_(Stack),
 yeccgoto_type_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_48/7}).
-compile({nowarn_unused_function,  yeccpars2_48/7}).
yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_48_(Stack),
 yeccgoto_type_expr_app(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_49/7}).
-compile({nowarn_unused_function,  yeccpars2_49/7}).
yeccpars2_49(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_49(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_49(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_49(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_49_(Stack),
 yeccgoto_type_expr_primary_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_50/7}).
-compile({nowarn_unused_function,  yeccpars2_50/7}).
yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_50_(Stack),
 yeccgoto_type_expr_primary_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_51/7}).
-compile({nowarn_unused_function,  yeccpars2_51/7}).
yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_51_(Stack),
 yeccgoto_type_expr_app(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_52/7}).
-compile({nowarn_unused_function,  yeccpars2_52/7}).
yeccpars2_52(S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_52(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_53: see yeccpars2_40

-dialyzer({nowarn_function, yeccpars2_54/7}).
-compile({nowarn_unused_function,  yeccpars2_54/7}).
yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_54_(Stack),
 yeccgoto_type_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_55: see yeccpars2_40

yeccpars2_56(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_18(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_57/7}).
-compile({nowarn_unused_function,  yeccpars2_57/7}).
yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_57_(Stack),
 yeccgoto_trait_constraint(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_58/7}).
-compile({nowarn_unused_function,  yeccpars2_58/7}).
yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_58_(Stack),
 yeccgoto_type_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_59/7}).
-compile({nowarn_unused_function,  yeccpars2_59/7}).
yeccpars2_59(S, 'ampersand', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_59_(Stack),
 yeccgoto_type_constraints(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_60: see yeccpars2_56

-dialyzer({nowarn_function, yeccpars2_61/7}).
-compile({nowarn_unused_function,  yeccpars2_61/7}).
yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_61_(Stack),
 yeccgoto_type_constraints(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_62/7}).
-compile({nowarn_unused_function,  yeccpars2_62/7}).
yeccpars2_62(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_62_(Stack),
 yeccgoto_type_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_63: see yeccpars2_40

-dialyzer({nowarn_function, yeccpars2_64/7}).
-compile({nowarn_unused_function,  yeccpars2_64/7}).
yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_64_(Stack),
 yeccgoto_type_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_65/7}).
-compile({nowarn_unused_function,  yeccpars2_65/7}).
yeccpars2_65(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_66/7}).
-compile({nowarn_unused_function,  yeccpars2_66/7}).
yeccpars2_66(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(S, 'constrain', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_66_(Stack),
 yeccgoto_type_expr_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_67: see yeccpars2_40

-dialyzer({nowarn_function, yeccpars2_68/7}).
-compile({nowarn_unused_function,  yeccpars2_68/7}).
yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_68_(Stack),
 yeccgoto_type_expr_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_69/7}).
-compile({nowarn_unused_function,  yeccpars2_69/7}).
yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_69_(Stack),
 yeccgoto_type_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_70/7}).
-compile({nowarn_unused_function,  yeccpars2_70/7}).
yeccpars2_70(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_71/7}).
-compile({nowarn_unused_function,  yeccpars2_71/7}).
yeccpars2_71(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_72/7}).
-compile({nowarn_unused_function,  yeccpars2_72/7}).
yeccpars2_72(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_73/7}).
-compile({nowarn_unused_function,  yeccpars2_73/7}).
yeccpars2_73(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_73_(Stack),
 yeccgoto_type_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_74/7}).
-compile({nowarn_unused_function,  yeccpars2_74/7}).
yeccpars2_74(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_74(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_74_(Stack),
 yeccgoto_effect_list_nonempty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_75/7}).
-compile({nowarn_unused_function,  yeccpars2_75/7}).
yeccpars2_75(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_76/7}).
-compile({nowarn_unused_function,  yeccpars2_76/7}).
yeccpars2_76(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_76_(Stack),
 yeccgoto_effect_list_nonempty(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_77/7}).
-compile({nowarn_unused_function,  yeccpars2_77/7}).
yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_77_(Stack),
 yeccgoto_type_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_78/7}).
-compile({nowarn_unused_function,  yeccpars2_78/7}).
yeccpars2_78(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_79/7}).
-compile({nowarn_unused_function,  yeccpars2_79/7}).
yeccpars2_79(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_79_(Stack),
 yeccgoto_type_record_fields(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_80/7}).
-compile({nowarn_unused_function,  yeccpars2_80/7}).
yeccpars2_80(S, 'colon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_81/7}).
-compile({nowarn_unused_function,  yeccpars2_81/7}).
yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_81_(Stack),
 yeccgoto_type_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_82: see yeccpars2_40

-dialyzer({nowarn_function, yeccpars2_83/7}).
-compile({nowarn_unused_function,  yeccpars2_83/7}).
yeccpars2_83(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(S, 'constrain', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_83_(Stack),
 yeccgoto_type_record_field(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_84/7}).
-compile({nowarn_unused_function,  yeccpars2_84/7}).
yeccpars2_84(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_84(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_85/7}).
-compile({nowarn_unused_function,  yeccpars2_85/7}).
yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_85_(Stack),
 yeccgoto_type_record_fields(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_86/7}).
-compile({nowarn_unused_function,  yeccpars2_86/7}).
yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_86_(Stack),
 yeccgoto_type_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_87/7}).
-compile({nowarn_unused_function,  yeccpars2_87/7}).
yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_87_(Stack),
 yeccgoto_constructor_fields(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_88: see yeccpars2_31

-dialyzer({nowarn_function, yeccpars2_89/7}).
-compile({nowarn_unused_function,  yeccpars2_89/7}).
yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_89_(Stack),
 yeccgoto_constructors(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_90: see yeccpars2_31

-dialyzer({nowarn_function, yeccpars2_91/7}).
-compile({nowarn_unused_function,  yeccpars2_91/7}).
yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_91_(Stack),
 yeccgoto_type_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_92/7}).
-compile({nowarn_unused_function,  yeccpars2_92/7}).
yeccpars2_92(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 321, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_92_(Stack),
 yeccgoto_transform_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_93/7}).
-compile({nowarn_unused_function,  yeccpars2_93/7}).
yeccpars2_93(S, 'colon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_93_(Stack),
 yeccpars2_95(95, Cat, [93 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_94/7}).
-compile({nowarn_unused_function,  yeccpars2_94/7}).
yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_(Stack),
 yeccgoto_pattern_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_95/7}).
-compile({nowarn_unused_function,  yeccpars2_95/7}).
yeccpars2_95(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 149, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 150, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 151, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_96/7}).
-compile({nowarn_unused_function,  yeccpars2_96/7}).
yeccpars2_96(S, 'as', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_96_(Stack),
 yeccgoto_pattern_list_nonempty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_97: see yeccpars2_40

-dialyzer({nowarn_function, yeccpars2_98/7}).
-compile({nowarn_unused_function,  yeccpars2_98/7}).
yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_(Stack),
 yeccgoto_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_99/7}).
-compile({nowarn_unused_function,  yeccpars2_99/7}).
yeccpars2_99(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_(Stack),
 yeccgoto_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_100/7}).
-compile({nowarn_unused_function,  yeccpars2_100/7}).
yeccpars2_100(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_100(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_100(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_101(S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_102/7}).
-compile({nowarn_unused_function,  yeccpars2_102/7}).
yeccpars2_102(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_(Stack),
 yeccgoto_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_103/7}).
-compile({nowarn_unused_function,  yeccpars2_103/7}).
yeccpars2_103(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_104/7}).
-compile({nowarn_unused_function,  yeccpars2_104/7}).
yeccpars2_104(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_104_(Stack),
 yeccgoto_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_105/7}).
-compile({nowarn_unused_function,  yeccpars2_105/7}).
yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_105_(Stack),
 yeccgoto_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_106/7}).
-compile({nowarn_unused_function,  yeccpars2_106/7}).
yeccpars2_106(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 108, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 109, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 111, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_(Stack),
 yeccgoto_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_107/7}).
-compile({nowarn_unused_function,  yeccpars2_107/7}).
yeccpars2_107(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 108, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 109, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 111, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 128, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_107_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_108/7}).
-compile({nowarn_unused_function,  yeccpars2_108/7}).
yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_108_(Stack),
 yeccgoto_pattern_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_109/7}).
-compile({nowarn_unused_function,  yeccpars2_109/7}).
yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_109_(Stack),
 yeccgoto_pattern_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_110(S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_111/7}).
-compile({nowarn_unused_function,  yeccpars2_111/7}).
yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_111_(Stack),
 yeccgoto_pattern_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_112/7}).
-compile({nowarn_unused_function,  yeccpars2_112/7}).
yeccpars2_112(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_112_(Stack),
 yeccpars2_115(115, Cat, [112 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_113/7}).
-compile({nowarn_unused_function,  yeccpars2_113/7}).
yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_113_(Stack),
 yeccgoto_pattern_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_114/7}).
-compile({nowarn_unused_function,  yeccpars2_114/7}).
yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_114_(Stack),
 yeccgoto_pattern_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_115/7}).
-compile({nowarn_unused_function,  yeccpars2_115/7}).
yeccpars2_115(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_116(S, 'as', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_117/7}).
-compile({nowarn_unused_function,  yeccpars2_117/7}).
yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_117_(Stack),
 yeccgoto_pattern_list_nonempty(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_118/7}).
-compile({nowarn_unused_function,  yeccpars2_118/7}).
yeccpars2_118(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 122, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_119: see yeccpars2_103

-dialyzer({nowarn_function, yeccpars2_120/7}).
-compile({nowarn_unused_function,  yeccpars2_120/7}).
yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_120_(Stack),
 yeccgoto_pattern_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_121/7}).
-compile({nowarn_unused_function,  yeccpars2_121/7}).
yeccpars2_121(S, 'as', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_121_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_122/7}).
-compile({nowarn_unused_function,  yeccpars2_122/7}).
yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_122_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_123/7}).
-compile({nowarn_unused_function,  yeccpars2_123/7}).
yeccpars2_123(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_123_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_124/7}).
-compile({nowarn_unused_function,  yeccpars2_124/7}).
yeccpars2_124(S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_124(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_125/7}).
-compile({nowarn_unused_function,  yeccpars2_125/7}).
yeccpars2_125(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_125_(Stack),
 yeccgoto_pattern_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_126/7}).
-compile({nowarn_unused_function,  yeccpars2_126/7}).
yeccpars2_126(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_126_(Stack),
 yeccgoto_pattern_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_127/7}).
-compile({nowarn_unused_function,  yeccpars2_127/7}).
yeccpars2_127(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_127_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_128: see yeccpars2_103

-dialyzer({nowarn_function, yeccpars2_129/7}).
-compile({nowarn_unused_function,  yeccpars2_129/7}).
yeccpars2_129(S, 'as', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_130/7}).
-compile({nowarn_unused_function,  yeccpars2_130/7}).
yeccpars2_130(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_131/7}).
-compile({nowarn_unused_function,  yeccpars2_131/7}).
yeccpars2_131(S, 'as', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_132: see yeccpars2_103

-dialyzer({nowarn_function, yeccpars2_133/7}).
-compile({nowarn_unused_function,  yeccpars2_133/7}).
yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_133_(Stack),
 yeccgoto_tuple_pattern_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_134/7}).
-compile({nowarn_unused_function,  yeccpars2_134/7}).
yeccpars2_134(S, 'as', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_134_(Stack),
 yeccgoto_tuple_pattern_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_135/7}).
-compile({nowarn_unused_function,  yeccpars2_135/7}).
yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_135_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_136/7}).
-compile({nowarn_unused_function,  yeccpars2_136/7}).
yeccpars2_136(S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_137/7}).
-compile({nowarn_unused_function,  yeccpars2_137/7}).
yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_137_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_138/7}).
-compile({nowarn_unused_function,  yeccpars2_138/7}).
yeccpars2_138(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_138_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_139/7}).
-compile({nowarn_unused_function,  yeccpars2_139/7}).
yeccpars2_139(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 147, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_140/7}).
-compile({nowarn_unused_function,  yeccpars2_140/7}).
yeccpars2_140(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 145, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_140_(Stack),
 yeccgoto_record_pattern_fields(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_141/7}).
-compile({nowarn_unused_function,  yeccpars2_141/7}).
yeccpars2_141(S, 'colon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 143, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_142/7}).
-compile({nowarn_unused_function,  yeccpars2_142/7}).
yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_142_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_143: see yeccpars2_103

-dialyzer({nowarn_function, yeccpars2_144/7}).
-compile({nowarn_unused_function,  yeccpars2_144/7}).
yeccpars2_144(S, 'as', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_144_(Stack),
 yeccgoto_record_pattern_field(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_145/7}).
-compile({nowarn_unused_function,  yeccpars2_145/7}).
yeccpars2_145(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_146/7}).
-compile({nowarn_unused_function,  yeccpars2_146/7}).
yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_146_(Stack),
 yeccgoto_record_pattern_fields(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_147/7}).
-compile({nowarn_unused_function,  yeccpars2_147/7}).
yeccpars2_147(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_147_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_148/7}).
-compile({nowarn_unused_function,  yeccpars2_148/7}).
yeccpars2_148(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(S, 'constrain', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_148_(Stack),
 yeccgoto_transform_signature(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_149(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 168, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 169, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_149(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_149/7}).
-compile({nowarn_unused_function,  yeccpars2_149/7}).
yeccpars2_cont_149(S, 'do', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 161, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_149(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 162, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_149(S, 'fn', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 163, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_149(S, 'handle', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_149(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 165, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_149(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 166, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_149(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 167, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_149(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 170, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_149(S, 'match', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 171, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_149(S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 172, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_149(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 173, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_149(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_149(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_150/7}).
-compile({nowarn_unused_function,  yeccpars2_150/7}).
yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_150_(Stack),
 yeccgoto_transform_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_151: see yeccpars2_149

-dialyzer({nowarn_function, yeccpars2_152/7}).
-compile({nowarn_unused_function,  yeccpars2_152/7}).
yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_152_(Stack),
 yeccgoto_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_153/7}).
-compile({nowarn_unused_function,  yeccpars2_153/7}).
yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_153_(Stack),
 yeccgoto_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_154/7}).
-compile({nowarn_unused_function,  yeccpars2_154/7}).
yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_154_(Stack),
 yeccgoto_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_155/7}).
-compile({nowarn_unused_function,  yeccpars2_155/7}).
yeccpars2_155(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 318, Ss, Stack, T, Ts, Tzr);
yeccpars2_155(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_156/7}).
-compile({nowarn_unused_function,  yeccpars2_156/7}).
yeccpars2_156(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 316, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_156_(Stack),
 yeccgoto_guards(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_157/7}).
-compile({nowarn_unused_function,  yeccpars2_157/7}).
yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_157_(Stack),
 yeccgoto_expr_app(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_158/7}).
-compile({nowarn_unused_function,  yeccpars2_158/7}).
yeccpars2_158(S, 'do', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 161, Ss, Stack, T, Ts, Tzr);
yeccpars2_158(S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 314, Ss, Stack, T, Ts, Tzr);
yeccpars2_158(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 162, Ss, Stack, T, Ts, Tzr);
yeccpars2_158(S, 'fn', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 163, Ss, Stack, T, Ts, Tzr);
yeccpars2_158(S, 'handle', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_158(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 165, Ss, Stack, T, Ts, Tzr);
yeccpars2_158(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 166, Ss, Stack, T, Ts, Tzr);
yeccpars2_158(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 167, Ss, Stack, T, Ts, Tzr);
yeccpars2_158(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 168, Ss, Stack, T, Ts, Tzr);
yeccpars2_158(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 169, Ss, Stack, T, Ts, Tzr);
yeccpars2_158(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 170, Ss, Stack, T, Ts, Tzr);
yeccpars2_158(S, 'match', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 171, Ss, Stack, T, Ts, Tzr);
yeccpars2_158(S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 172, Ss, Stack, T, Ts, Tzr);
yeccpars2_158(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 173, Ss, Stack, T, Ts, Tzr);
yeccpars2_158(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_158_(Stack),
 yeccgoto_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_159/7}).
-compile({nowarn_unused_function,  yeccpars2_159/7}).
yeccpars2_159(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(S, 'ap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(S, 'fmap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(S, 'kleisli', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(S, 'mappend', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 198, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 201, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_159_(Stack),
 yeccgoto_guard(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_160/7}).
-compile({nowarn_unused_function,  yeccpars2_160/7}).
yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_160_(Stack),
 yeccgoto_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_161/7}).
-compile({nowarn_unused_function,  yeccpars2_161/7}).
yeccpars2_161(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 299, Ss, Stack, T, Ts, Tzr);
yeccpars2_161(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_162/7}).
-compile({nowarn_unused_function,  yeccpars2_162/7}).
yeccpars2_162(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_162_(Stack),
 yeccgoto_literal(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_163/7}).
-compile({nowarn_unused_function,  yeccpars2_163/7}).
yeccpars2_163(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 296, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_164: see yeccpars2_149

-dialyzer({nowarn_function, yeccpars2_165/7}).
-compile({nowarn_unused_function,  yeccpars2_165/7}).
yeccpars2_165(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_165_(Stack),
 yeccgoto_literal(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_166/7}).
-compile({nowarn_unused_function,  yeccpars2_166/7}).
yeccpars2_166(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 265, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 266, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_167(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 168, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 169, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 261, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_149(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_168/7}).
-compile({nowarn_unused_function,  yeccpars2_168/7}).
yeccpars2_168(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 255, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_169/7}).
-compile({nowarn_unused_function,  yeccpars2_169/7}).
yeccpars2_169(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_169_(Stack),
 yeccgoto_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_170: see yeccpars2_149

yeccpars2_171(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 168, Ss, Stack, T, Ts, Tzr);
yeccpars2_171(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 169, Ss, Stack, T, Ts, Tzr);
yeccpars2_171(S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 232, Ss, Stack, T, Ts, Tzr);
yeccpars2_171(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_149(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_172/7}).
-compile({nowarn_unused_function,  yeccpars2_172/7}).
yeccpars2_172(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 175, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_173/7}).
-compile({nowarn_unused_function,  yeccpars2_173/7}).
yeccpars2_173(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_173_(Stack),
 yeccgoto_literal(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_174/7}).
-compile({nowarn_unused_function,  yeccpars2_174/7}).
yeccpars2_174(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_174_(Stack),
 yeccgoto_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_175/7}).
-compile({nowarn_unused_function,  yeccpars2_175/7}).
yeccpars2_175(S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 176, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_176/7}).
-compile({nowarn_unused_function,  yeccpars2_176/7}).
yeccpars2_176(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 177, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_177/7}).
-compile({nowarn_unused_function,  yeccpars2_177/7}).
yeccpars2_177(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_178/7}).
-compile({nowarn_unused_function,  yeccpars2_178/7}).
yeccpars2_178(S, 'do', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 161, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 162, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, 'fn', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 163, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, 'handle', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 165, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 166, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 167, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 168, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 169, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 170, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, 'match', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 171, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 172, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 173, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_178_(Stack),
 yeccpars2_179(179, Cat, [178 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_179/7}).
-compile({nowarn_unused_function,  yeccpars2_179/7}).
yeccpars2_179(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 228, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_180/7}).
-compile({nowarn_unused_function,  yeccpars2_180/7}).
yeccpars2_180(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_180_(Stack),
 yeccgoto_expr_list_opt(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_181/7}).
-compile({nowarn_unused_function,  yeccpars2_181/7}).
yeccpars2_181(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'ap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'fmap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'kleisli', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'mappend', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 198, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 201, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_181_(Stack),
 yeccgoto_expr_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_182: see yeccpars2_149

%% yeccpars2_183: see yeccpars2_149

%% yeccpars2_184: see yeccpars2_149

%% yeccpars2_185: see yeccpars2_149

%% yeccpars2_186: see yeccpars2_149

%% yeccpars2_187: see yeccpars2_149

%% yeccpars2_188: see yeccpars2_149

%% yeccpars2_189: see yeccpars2_149

%% yeccpars2_190: see yeccpars2_149

%% yeccpars2_191: see yeccpars2_149

%% yeccpars2_192: see yeccpars2_149

%% yeccpars2_193: see yeccpars2_149

%% yeccpars2_194: see yeccpars2_149

%% yeccpars2_195: see yeccpars2_149

%% yeccpars2_196: see yeccpars2_149

%% yeccpars2_197: see yeccpars2_149

%% yeccpars2_198: see yeccpars2_149

%% yeccpars2_199: see yeccpars2_149

%% yeccpars2_200: see yeccpars2_149

%% yeccpars2_201: see yeccpars2_149

%% yeccpars2_202: see yeccpars2_149

%% yeccpars2_203: see yeccpars2_149

%% yeccpars2_204: see yeccpars2_149

-dialyzer({nowarn_function, yeccpars2_205/7}).
-compile({nowarn_unused_function,  yeccpars2_205/7}).
yeccpars2_205(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_206/7}).
-compile({nowarn_unused_function,  yeccpars2_206/7}).
yeccpars2_206(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_207/7}).
-compile({nowarn_unused_function,  yeccpars2_207/7}).
yeccpars2_207(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_207(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_207(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_207(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_207(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_207(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_207(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_207(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_207(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_207(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_207_$end'(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_and(Stack),
 yeccgoto_expr(hd(Nss), 'and', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'ap', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_ap(Stack),
 yeccgoto_expr(hd(Nss), 'ap', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_arrow(Stack),
 yeccgoto_expr(hd(Nss), 'arrow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_bind(Stack),
 yeccgoto_expr(hd(Nss), 'bind', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_comma(Stack),
 yeccgoto_expr(hd(Nss), 'comma', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_cons(Stack),
 yeccgoto_expr(hd(Nss), 'cons', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'do', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_do(Stack),
 yeccgoto_expr(hd(Nss), 'do', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_dot(Stack),
 yeccgoto_expr(hd(Nss), 'dot', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_effect(Stack),
 yeccgoto_expr(hd(Nss), 'effect', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_end(Stack),
 yeccgoto_expr(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_equals(Stack),
 yeccgoto_expr(hd(Nss), 'equals', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'error', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_error(Stack),
 yeccgoto_expr(hd(Nss), 'error', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'float', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_float(Stack),
 yeccgoto_expr(hd(Nss), 'float', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'fmap', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_fmap(Stack),
 yeccgoto_expr(hd(Nss), 'fmap', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'fn', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_fn(Stack),
 yeccgoto_expr(hd(Nss), 'fn', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'handle', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_handle(Stack),
 yeccgoto_expr(hd(Nss), 'handle', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'import', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_import(Stack),
 yeccgoto_expr(hd(Nss), 'import', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_in(Stack),
 yeccgoto_expr(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'instance', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_instance(Stack),
 yeccgoto_expr(hd(Nss), 'instance', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_integer(Stack),
 yeccgoto_expr(hd(Nss), 'integer', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'kleisli', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_kleisli(Stack),
 yeccgoto_expr(hd(Nss), 'kleisli', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_lbrace(Stack),
 yeccgoto_expr(hd(Nss), 'lbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_lbracket(Stack),
 yeccgoto_expr(hd(Nss), 'lbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'let', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_let(Stack),
 yeccgoto_expr(hd(Nss), 'let', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_lower_ident(Stack),
 yeccgoto_expr(hd(Nss), 'lower_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_lparen(Stack),
 yeccgoto_expr(hd(Nss), 'lparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'mappend', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_mappend(Stack),
 yeccgoto_expr(hd(Nss), 'mappend', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'match', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_match(Stack),
 yeccgoto_expr(hd(Nss), 'match', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_of(Stack),
 yeccgoto_expr(hd(Nss), 'of', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_or(Stack),
 yeccgoto_expr(hd(Nss), 'or', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_perform(Stack),
 yeccgoto_expr(hd(Nss), 'perform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_pipe(Stack),
 yeccgoto_expr(hd(Nss), 'pipe', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_pipe_right(Stack),
 yeccgoto_expr(hd(Nss), 'pipe_right', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'property', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_property(Stack),
 yeccgoto_expr(hd(Nss), 'property', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_rbrace(Stack),
 yeccgoto_expr(hd(Nss), 'rbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_rbracket(Stack),
 yeccgoto_expr(hd(Nss), 'rbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_rparen(Stack),
 yeccgoto_expr(hd(Nss), 'rparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'semicolon', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_semicolon(Stack),
 yeccgoto_expr(hd(Nss), 'semicolon', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'string', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_string(Stack),
 yeccgoto_expr(hd(Nss), 'string', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'test', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_test(Stack),
 yeccgoto_expr(hd(Nss), 'test', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_then(Stack),
 yeccgoto_expr(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'trait', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_trait(Stack),
 yeccgoto_expr(hd(Nss), 'trait', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_transform(Stack),
 yeccgoto_expr(hd(Nss), 'transform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'type', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_type(Stack),
 yeccgoto_expr(hd(Nss), 'type', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_upper_ident(Stack),
 yeccgoto_expr(hd(Nss), 'upper_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_207(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_208/7}).
-compile({nowarn_unused_function,  yeccpars2_208/7}).
yeccpars2_208(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_208_$end'(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_and(Stack),
 yeccgoto_expr(hd(Nss), 'and', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'ap', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_ap(Stack),
 yeccgoto_expr(hd(Nss), 'ap', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_arrow(Stack),
 yeccgoto_expr(hd(Nss), 'arrow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_bind(Stack),
 yeccgoto_expr(hd(Nss), 'bind', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_comma(Stack),
 yeccgoto_expr(hd(Nss), 'comma', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_cons(Stack),
 yeccgoto_expr(hd(Nss), 'cons', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'do', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_do(Stack),
 yeccgoto_expr(hd(Nss), 'do', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_dot(Stack),
 yeccgoto_expr(hd(Nss), 'dot', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_effect(Stack),
 yeccgoto_expr(hd(Nss), 'effect', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_end(Stack),
 yeccgoto_expr(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_equals(Stack),
 yeccgoto_expr(hd(Nss), 'equals', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'error', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_error(Stack),
 yeccgoto_expr(hd(Nss), 'error', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'float', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_float(Stack),
 yeccgoto_expr(hd(Nss), 'float', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'fmap', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_fmap(Stack),
 yeccgoto_expr(hd(Nss), 'fmap', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'fn', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_fn(Stack),
 yeccgoto_expr(hd(Nss), 'fn', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'handle', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_handle(Stack),
 yeccgoto_expr(hd(Nss), 'handle', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'import', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_import(Stack),
 yeccgoto_expr(hd(Nss), 'import', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_in(Stack),
 yeccgoto_expr(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'instance', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_instance(Stack),
 yeccgoto_expr(hd(Nss), 'instance', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_integer(Stack),
 yeccgoto_expr(hd(Nss), 'integer', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'kleisli', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_kleisli(Stack),
 yeccgoto_expr(hd(Nss), 'kleisli', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_lbrace(Stack),
 yeccgoto_expr(hd(Nss), 'lbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_lbracket(Stack),
 yeccgoto_expr(hd(Nss), 'lbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'let', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_let(Stack),
 yeccgoto_expr(hd(Nss), 'let', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_lower_ident(Stack),
 yeccgoto_expr(hd(Nss), 'lower_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_lparen(Stack),
 yeccgoto_expr(hd(Nss), 'lparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'mappend', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_mappend(Stack),
 yeccgoto_expr(hd(Nss), 'mappend', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'match', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_match(Stack),
 yeccgoto_expr(hd(Nss), 'match', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_of(Stack),
 yeccgoto_expr(hd(Nss), 'of', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_or(Stack),
 yeccgoto_expr(hd(Nss), 'or', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_perform(Stack),
 yeccgoto_expr(hd(Nss), 'perform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_pipe(Stack),
 yeccgoto_expr(hd(Nss), 'pipe', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_pipe_right(Stack),
 yeccgoto_expr(hd(Nss), 'pipe_right', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'property', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_property(Stack),
 yeccgoto_expr(hd(Nss), 'property', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_rbrace(Stack),
 yeccgoto_expr(hd(Nss), 'rbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_rbracket(Stack),
 yeccgoto_expr(hd(Nss), 'rbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_rparen(Stack),
 yeccgoto_expr(hd(Nss), 'rparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'semicolon', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_semicolon(Stack),
 yeccgoto_expr(hd(Nss), 'semicolon', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'string', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_string(Stack),
 yeccgoto_expr(hd(Nss), 'string', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'test', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_test(Stack),
 yeccgoto_expr(hd(Nss), 'test', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_then(Stack),
 yeccgoto_expr(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'trait', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_trait(Stack),
 yeccgoto_expr(hd(Nss), 'trait', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_transform(Stack),
 yeccgoto_expr(hd(Nss), 'transform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'type', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_type(Stack),
 yeccgoto_expr(hd(Nss), 'type', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_upper_ident(Stack),
 yeccgoto_expr(hd(Nss), 'upper_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_209/7}).
-compile({nowarn_unused_function,  yeccpars2_209/7}).
yeccpars2_209(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_209(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_209(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_209(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_209(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_209(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_210/7}).
-compile({nowarn_unused_function,  yeccpars2_210/7}).
yeccpars2_210(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_210(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_210(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_211/7}).
-compile({nowarn_unused_function,  yeccpars2_211/7}).
yeccpars2_211(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_211(S, 'ap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_211(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_211(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_211(S, 'fmap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_211(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_211(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_211(S, 'kleisli', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_211(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_211(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_211(S, 'mappend', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_211(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_211(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_211(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_211(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 198, Ss, Stack, T, Ts, Tzr);
yeccpars2_211(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_211(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_211(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 201, Ss, Stack, T, Ts, Tzr);
yeccpars2_211(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_211(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_211(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_211(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_212/7}).
-compile({nowarn_unused_function,  yeccpars2_212/7}).
yeccpars2_212(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 201, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_213/7}).
-compile({nowarn_unused_function,  yeccpars2_213/7}).
yeccpars2_213(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_213_$end'(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_and(Stack),
 yeccgoto_expr(hd(Nss), 'and', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'ap', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_ap(Stack),
 yeccgoto_expr(hd(Nss), 'ap', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_arrow(Stack),
 yeccgoto_expr(hd(Nss), 'arrow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_bind(Stack),
 yeccgoto_expr(hd(Nss), 'bind', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_comma(Stack),
 yeccgoto_expr(hd(Nss), 'comma', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_cons(Stack),
 yeccgoto_expr(hd(Nss), 'cons', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'do', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_do(Stack),
 yeccgoto_expr(hd(Nss), 'do', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_dot(Stack),
 yeccgoto_expr(hd(Nss), 'dot', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_effect(Stack),
 yeccgoto_expr(hd(Nss), 'effect', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_end(Stack),
 yeccgoto_expr(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_equals(Stack),
 yeccgoto_expr(hd(Nss), 'equals', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'error', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_error(Stack),
 yeccgoto_expr(hd(Nss), 'error', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'float', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_float(Stack),
 yeccgoto_expr(hd(Nss), 'float', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'fmap', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_fmap(Stack),
 yeccgoto_expr(hd(Nss), 'fmap', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'fn', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_fn(Stack),
 yeccgoto_expr(hd(Nss), 'fn', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'handle', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_handle(Stack),
 yeccgoto_expr(hd(Nss), 'handle', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'import', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_import(Stack),
 yeccgoto_expr(hd(Nss), 'import', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_in(Stack),
 yeccgoto_expr(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'instance', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_instance(Stack),
 yeccgoto_expr(hd(Nss), 'instance', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_integer(Stack),
 yeccgoto_expr(hd(Nss), 'integer', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'kleisli', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_kleisli(Stack),
 yeccgoto_expr(hd(Nss), 'kleisli', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_lbrace(Stack),
 yeccgoto_expr(hd(Nss), 'lbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_lbracket(Stack),
 yeccgoto_expr(hd(Nss), 'lbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'let', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_let(Stack),
 yeccgoto_expr(hd(Nss), 'let', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_lower_ident(Stack),
 yeccgoto_expr(hd(Nss), 'lower_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_lparen(Stack),
 yeccgoto_expr(hd(Nss), 'lparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'mappend', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_mappend(Stack),
 yeccgoto_expr(hd(Nss), 'mappend', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'match', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_match(Stack),
 yeccgoto_expr(hd(Nss), 'match', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_of(Stack),
 yeccgoto_expr(hd(Nss), 'of', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_or(Stack),
 yeccgoto_expr(hd(Nss), 'or', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_perform(Stack),
 yeccgoto_expr(hd(Nss), 'perform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_pipe(Stack),
 yeccgoto_expr(hd(Nss), 'pipe', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_pipe_right(Stack),
 yeccgoto_expr(hd(Nss), 'pipe_right', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'property', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_property(Stack),
 yeccgoto_expr(hd(Nss), 'property', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_rbrace(Stack),
 yeccgoto_expr(hd(Nss), 'rbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_rbracket(Stack),
 yeccgoto_expr(hd(Nss), 'rbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_rparen(Stack),
 yeccgoto_expr(hd(Nss), 'rparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'semicolon', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_semicolon(Stack),
 yeccgoto_expr(hd(Nss), 'semicolon', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'string', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_string(Stack),
 yeccgoto_expr(hd(Nss), 'string', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'test', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_test(Stack),
 yeccgoto_expr(hd(Nss), 'test', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_then(Stack),
 yeccgoto_expr(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'trait', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_trait(Stack),
 yeccgoto_expr(hd(Nss), 'trait', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_transform(Stack),
 yeccgoto_expr(hd(Nss), 'transform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'type', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_type(Stack),
 yeccgoto_expr(hd(Nss), 'type', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_upper_ident(Stack),
 yeccgoto_expr(hd(Nss), 'upper_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_213(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_214/7}).
-compile({nowarn_unused_function,  yeccpars2_214/7}).
yeccpars2_214(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_214(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_214(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_214_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_215/7}).
-compile({nowarn_unused_function,  yeccpars2_215/7}).
yeccpars2_215(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(S, 'mappend', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 201, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_215_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_216/7}).
-compile({nowarn_unused_function,  yeccpars2_216/7}).
yeccpars2_216(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_216(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_216(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_216(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_216(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_216(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_216_$end'(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_and(Stack),
 yeccgoto_expr(hd(Nss), 'and', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'ap', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_ap(Stack),
 yeccgoto_expr(hd(Nss), 'ap', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_arrow(Stack),
 yeccgoto_expr(hd(Nss), 'arrow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_bind(Stack),
 yeccgoto_expr(hd(Nss), 'bind', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_comma(Stack),
 yeccgoto_expr(hd(Nss), 'comma', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_cons(Stack),
 yeccgoto_expr(hd(Nss), 'cons', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'do', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_do(Stack),
 yeccgoto_expr(hd(Nss), 'do', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_dot(Stack),
 yeccgoto_expr(hd(Nss), 'dot', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_effect(Stack),
 yeccgoto_expr(hd(Nss), 'effect', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_end(Stack),
 yeccgoto_expr(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_eq(Stack),
 yeccgoto_expr(hd(Nss), 'eq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_equals(Stack),
 yeccgoto_expr(hd(Nss), 'equals', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'error', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_error(Stack),
 yeccgoto_expr(hd(Nss), 'error', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'float', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_float(Stack),
 yeccgoto_expr(hd(Nss), 'float', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'fmap', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_fmap(Stack),
 yeccgoto_expr(hd(Nss), 'fmap', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'fn', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_fn(Stack),
 yeccgoto_expr(hd(Nss), 'fn', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'handle', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_handle(Stack),
 yeccgoto_expr(hd(Nss), 'handle', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'import', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_import(Stack),
 yeccgoto_expr(hd(Nss), 'import', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_in(Stack),
 yeccgoto_expr(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'instance', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_instance(Stack),
 yeccgoto_expr(hd(Nss), 'instance', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_integer(Stack),
 yeccgoto_expr(hd(Nss), 'integer', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'kleisli', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_kleisli(Stack),
 yeccgoto_expr(hd(Nss), 'kleisli', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_lbrace(Stack),
 yeccgoto_expr(hd(Nss), 'lbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_lbracket(Stack),
 yeccgoto_expr(hd(Nss), 'lbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'let', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_let(Stack),
 yeccgoto_expr(hd(Nss), 'let', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_lower_ident(Stack),
 yeccgoto_expr(hd(Nss), 'lower_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_lparen(Stack),
 yeccgoto_expr(hd(Nss), 'lparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'mappend', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_mappend(Stack),
 yeccgoto_expr(hd(Nss), 'mappend', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'match', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_match(Stack),
 yeccgoto_expr(hd(Nss), 'match', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_neq(Stack),
 yeccgoto_expr(hd(Nss), 'neq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_of(Stack),
 yeccgoto_expr(hd(Nss), 'of', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_or(Stack),
 yeccgoto_expr(hd(Nss), 'or', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_perform(Stack),
 yeccgoto_expr(hd(Nss), 'perform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_pipe(Stack),
 yeccgoto_expr(hd(Nss), 'pipe', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_pipe_right(Stack),
 yeccgoto_expr(hd(Nss), 'pipe_right', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'property', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_property(Stack),
 yeccgoto_expr(hd(Nss), 'property', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_rbrace(Stack),
 yeccgoto_expr(hd(Nss), 'rbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_rbracket(Stack),
 yeccgoto_expr(hd(Nss), 'rbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_rparen(Stack),
 yeccgoto_expr(hd(Nss), 'rparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'semicolon', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_semicolon(Stack),
 yeccgoto_expr(hd(Nss), 'semicolon', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_setoid_eq(Stack),
 yeccgoto_expr(hd(Nss), 'setoid_eq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_setoid_neq(Stack),
 yeccgoto_expr(hd(Nss), 'setoid_neq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'string', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_string(Stack),
 yeccgoto_expr(hd(Nss), 'string', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'test', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_test(Stack),
 yeccgoto_expr(hd(Nss), 'test', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_then(Stack),
 yeccgoto_expr(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'trait', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_trait(Stack),
 yeccgoto_expr(hd(Nss), 'trait', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_transform(Stack),
 yeccgoto_expr(hd(Nss), 'transform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'type', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_type(Stack),
 yeccgoto_expr(hd(Nss), 'type', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_upper_ident(Stack),
 yeccgoto_expr(hd(Nss), 'upper_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_216(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_217/7}).
-compile({nowarn_unused_function,  yeccpars2_217/7}).
yeccpars2_217(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_217(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_217(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_217(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_217(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_217(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_217_$end'(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_and(Stack),
 yeccgoto_expr(hd(Nss), 'and', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'ap', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_ap(Stack),
 yeccgoto_expr(hd(Nss), 'ap', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_arrow(Stack),
 yeccgoto_expr(hd(Nss), 'arrow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_bind(Stack),
 yeccgoto_expr(hd(Nss), 'bind', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_comma(Stack),
 yeccgoto_expr(hd(Nss), 'comma', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_cons(Stack),
 yeccgoto_expr(hd(Nss), 'cons', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'do', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_do(Stack),
 yeccgoto_expr(hd(Nss), 'do', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_dot(Stack),
 yeccgoto_expr(hd(Nss), 'dot', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_effect(Stack),
 yeccgoto_expr(hd(Nss), 'effect', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_end(Stack),
 yeccgoto_expr(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_eq(Stack),
 yeccgoto_expr(hd(Nss), 'eq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_equals(Stack),
 yeccgoto_expr(hd(Nss), 'equals', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'error', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_error(Stack),
 yeccgoto_expr(hd(Nss), 'error', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'float', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_float(Stack),
 yeccgoto_expr(hd(Nss), 'float', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'fmap', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_fmap(Stack),
 yeccgoto_expr(hd(Nss), 'fmap', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'fn', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_fn(Stack),
 yeccgoto_expr(hd(Nss), 'fn', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'handle', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_handle(Stack),
 yeccgoto_expr(hd(Nss), 'handle', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'import', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_import(Stack),
 yeccgoto_expr(hd(Nss), 'import', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_in(Stack),
 yeccgoto_expr(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'instance', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_instance(Stack),
 yeccgoto_expr(hd(Nss), 'instance', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_integer(Stack),
 yeccgoto_expr(hd(Nss), 'integer', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'kleisli', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_kleisli(Stack),
 yeccgoto_expr(hd(Nss), 'kleisli', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_lbrace(Stack),
 yeccgoto_expr(hd(Nss), 'lbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_lbracket(Stack),
 yeccgoto_expr(hd(Nss), 'lbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'let', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_let(Stack),
 yeccgoto_expr(hd(Nss), 'let', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_lower_ident(Stack),
 yeccgoto_expr(hd(Nss), 'lower_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_lparen(Stack),
 yeccgoto_expr(hd(Nss), 'lparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'mappend', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_mappend(Stack),
 yeccgoto_expr(hd(Nss), 'mappend', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'match', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_match(Stack),
 yeccgoto_expr(hd(Nss), 'match', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_neq(Stack),
 yeccgoto_expr(hd(Nss), 'neq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_of(Stack),
 yeccgoto_expr(hd(Nss), 'of', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_or(Stack),
 yeccgoto_expr(hd(Nss), 'or', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_perform(Stack),
 yeccgoto_expr(hd(Nss), 'perform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_pipe(Stack),
 yeccgoto_expr(hd(Nss), 'pipe', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_pipe_right(Stack),
 yeccgoto_expr(hd(Nss), 'pipe_right', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'property', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_property(Stack),
 yeccgoto_expr(hd(Nss), 'property', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_rbrace(Stack),
 yeccgoto_expr(hd(Nss), 'rbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_rbracket(Stack),
 yeccgoto_expr(hd(Nss), 'rbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_rparen(Stack),
 yeccgoto_expr(hd(Nss), 'rparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'semicolon', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_semicolon(Stack),
 yeccgoto_expr(hd(Nss), 'semicolon', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_setoid_eq(Stack),
 yeccgoto_expr(hd(Nss), 'setoid_eq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_setoid_neq(Stack),
 yeccgoto_expr(hd(Nss), 'setoid_neq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'string', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_string(Stack),
 yeccgoto_expr(hd(Nss), 'string', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'test', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_test(Stack),
 yeccgoto_expr(hd(Nss), 'test', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_then(Stack),
 yeccgoto_expr(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'trait', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_trait(Stack),
 yeccgoto_expr(hd(Nss), 'trait', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_transform(Stack),
 yeccgoto_expr(hd(Nss), 'transform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'type', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_type(Stack),
 yeccgoto_expr(hd(Nss), 'type', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_upper_ident(Stack),
 yeccgoto_expr(hd(Nss), 'upper_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_217(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_218/7}).
-compile({nowarn_unused_function,  yeccpars2_218/7}).
yeccpars2_218(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(S, 'ap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(S, 'fmap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(S, 'kleisli', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(S, 'mappend', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 201, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_218_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_219/7}).
-compile({nowarn_unused_function,  yeccpars2_219/7}).
yeccpars2_219(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_219(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_219(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_219(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_219(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_219(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_219_$end'(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_and(Stack),
 yeccgoto_expr(hd(Nss), 'and', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'ap', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_ap(Stack),
 yeccgoto_expr(hd(Nss), 'ap', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_arrow(Stack),
 yeccgoto_expr(hd(Nss), 'arrow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_bind(Stack),
 yeccgoto_expr(hd(Nss), 'bind', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_comma(Stack),
 yeccgoto_expr(hd(Nss), 'comma', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_cons(Stack),
 yeccgoto_expr(hd(Nss), 'cons', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'do', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_do(Stack),
 yeccgoto_expr(hd(Nss), 'do', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_dot(Stack),
 yeccgoto_expr(hd(Nss), 'dot', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_effect(Stack),
 yeccgoto_expr(hd(Nss), 'effect', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_end(Stack),
 yeccgoto_expr(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_eq(Stack),
 yeccgoto_expr(hd(Nss), 'eq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_equals(Stack),
 yeccgoto_expr(hd(Nss), 'equals', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'error', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_error(Stack),
 yeccgoto_expr(hd(Nss), 'error', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'float', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_float(Stack),
 yeccgoto_expr(hd(Nss), 'float', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'fmap', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_fmap(Stack),
 yeccgoto_expr(hd(Nss), 'fmap', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'fn', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_fn(Stack),
 yeccgoto_expr(hd(Nss), 'fn', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'handle', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_handle(Stack),
 yeccgoto_expr(hd(Nss), 'handle', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'import', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_import(Stack),
 yeccgoto_expr(hd(Nss), 'import', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_in(Stack),
 yeccgoto_expr(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'instance', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_instance(Stack),
 yeccgoto_expr(hd(Nss), 'instance', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_integer(Stack),
 yeccgoto_expr(hd(Nss), 'integer', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'kleisli', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_kleisli(Stack),
 yeccgoto_expr(hd(Nss), 'kleisli', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_lbrace(Stack),
 yeccgoto_expr(hd(Nss), 'lbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_lbracket(Stack),
 yeccgoto_expr(hd(Nss), 'lbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'let', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_let(Stack),
 yeccgoto_expr(hd(Nss), 'let', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_lower_ident(Stack),
 yeccgoto_expr(hd(Nss), 'lower_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_lparen(Stack),
 yeccgoto_expr(hd(Nss), 'lparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'mappend', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_mappend(Stack),
 yeccgoto_expr(hd(Nss), 'mappend', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'match', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_match(Stack),
 yeccgoto_expr(hd(Nss), 'match', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_neq(Stack),
 yeccgoto_expr(hd(Nss), 'neq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_of(Stack),
 yeccgoto_expr(hd(Nss), 'of', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_or(Stack),
 yeccgoto_expr(hd(Nss), 'or', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_perform(Stack),
 yeccgoto_expr(hd(Nss), 'perform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_pipe(Stack),
 yeccgoto_expr(hd(Nss), 'pipe', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_pipe_right(Stack),
 yeccgoto_expr(hd(Nss), 'pipe_right', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'property', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_property(Stack),
 yeccgoto_expr(hd(Nss), 'property', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_rbrace(Stack),
 yeccgoto_expr(hd(Nss), 'rbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_rbracket(Stack),
 yeccgoto_expr(hd(Nss), 'rbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_rparen(Stack),
 yeccgoto_expr(hd(Nss), 'rparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'semicolon', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_semicolon(Stack),
 yeccgoto_expr(hd(Nss), 'semicolon', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_setoid_eq(Stack),
 yeccgoto_expr(hd(Nss), 'setoid_eq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_setoid_neq(Stack),
 yeccgoto_expr(hd(Nss), 'setoid_neq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'string', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_string(Stack),
 yeccgoto_expr(hd(Nss), 'string', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'test', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_test(Stack),
 yeccgoto_expr(hd(Nss), 'test', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_then(Stack),
 yeccgoto_expr(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'trait', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_trait(Stack),
 yeccgoto_expr(hd(Nss), 'trait', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_transform(Stack),
 yeccgoto_expr(hd(Nss), 'transform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'type', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_type(Stack),
 yeccgoto_expr(hd(Nss), 'type', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_upper_ident(Stack),
 yeccgoto_expr(hd(Nss), 'upper_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_219(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_220/7}).
-compile({nowarn_unused_function,  yeccpars2_220/7}).
yeccpars2_220(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_220_$end'(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_and(Stack),
 yeccgoto_expr(hd(Nss), 'and', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'ap', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_ap(Stack),
 yeccgoto_expr(hd(Nss), 'ap', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_arrow(Stack),
 yeccgoto_expr(hd(Nss), 'arrow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_bind(Stack),
 yeccgoto_expr(hd(Nss), 'bind', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_comma(Stack),
 yeccgoto_expr(hd(Nss), 'comma', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_cons(Stack),
 yeccgoto_expr(hd(Nss), 'cons', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'do', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_do(Stack),
 yeccgoto_expr(hd(Nss), 'do', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_dot(Stack),
 yeccgoto_expr(hd(Nss), 'dot', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_effect(Stack),
 yeccgoto_expr(hd(Nss), 'effect', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_end(Stack),
 yeccgoto_expr(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_eq(Stack),
 yeccgoto_expr(hd(Nss), 'eq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_equals(Stack),
 yeccgoto_expr(hd(Nss), 'equals', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'error', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_error(Stack),
 yeccgoto_expr(hd(Nss), 'error', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'float', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_float(Stack),
 yeccgoto_expr(hd(Nss), 'float', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'fmap', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_fmap(Stack),
 yeccgoto_expr(hd(Nss), 'fmap', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'fn', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_fn(Stack),
 yeccgoto_expr(hd(Nss), 'fn', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'handle', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_handle(Stack),
 yeccgoto_expr(hd(Nss), 'handle', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'import', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_import(Stack),
 yeccgoto_expr(hd(Nss), 'import', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_in(Stack),
 yeccgoto_expr(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'instance', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_instance(Stack),
 yeccgoto_expr(hd(Nss), 'instance', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_integer(Stack),
 yeccgoto_expr(hd(Nss), 'integer', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'kleisli', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_kleisli(Stack),
 yeccgoto_expr(hd(Nss), 'kleisli', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_lbrace(Stack),
 yeccgoto_expr(hd(Nss), 'lbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_lbracket(Stack),
 yeccgoto_expr(hd(Nss), 'lbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'let', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_let(Stack),
 yeccgoto_expr(hd(Nss), 'let', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_lower_ident(Stack),
 yeccgoto_expr(hd(Nss), 'lower_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_lparen(Stack),
 yeccgoto_expr(hd(Nss), 'lparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'mappend', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_mappend(Stack),
 yeccgoto_expr(hd(Nss), 'mappend', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'match', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_match(Stack),
 yeccgoto_expr(hd(Nss), 'match', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_neq(Stack),
 yeccgoto_expr(hd(Nss), 'neq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_of(Stack),
 yeccgoto_expr(hd(Nss), 'of', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_or(Stack),
 yeccgoto_expr(hd(Nss), 'or', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_perform(Stack),
 yeccgoto_expr(hd(Nss), 'perform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_pipe(Stack),
 yeccgoto_expr(hd(Nss), 'pipe', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_pipe_right(Stack),
 yeccgoto_expr(hd(Nss), 'pipe_right', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'property', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_property(Stack),
 yeccgoto_expr(hd(Nss), 'property', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_rbrace(Stack),
 yeccgoto_expr(hd(Nss), 'rbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_rbracket(Stack),
 yeccgoto_expr(hd(Nss), 'rbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_rparen(Stack),
 yeccgoto_expr(hd(Nss), 'rparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'semicolon', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_semicolon(Stack),
 yeccgoto_expr(hd(Nss), 'semicolon', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_setoid_eq(Stack),
 yeccgoto_expr(hd(Nss), 'setoid_eq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_setoid_neq(Stack),
 yeccgoto_expr(hd(Nss), 'setoid_neq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'string', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_string(Stack),
 yeccgoto_expr(hd(Nss), 'string', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'test', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_test(Stack),
 yeccgoto_expr(hd(Nss), 'test', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_then(Stack),
 yeccgoto_expr(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'trait', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_trait(Stack),
 yeccgoto_expr(hd(Nss), 'trait', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_transform(Stack),
 yeccgoto_expr(hd(Nss), 'transform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'type', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_type(Stack),
 yeccgoto_expr(hd(Nss), 'type', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_upper_ident(Stack),
 yeccgoto_expr(hd(Nss), 'upper_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_220(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_221/7}).
-compile({nowarn_unused_function,  yeccpars2_221/7}).
yeccpars2_221(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'ap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'mappend', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 201, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_221_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_222/7}).
-compile({nowarn_unused_function,  yeccpars2_222/7}).
yeccpars2_222(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_222(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_222(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_222(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_222(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_222(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_222(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_222(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_222(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_222(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_222_$end'(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_and(Stack),
 yeccgoto_expr(hd(Nss), 'and', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'ap', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_ap(Stack),
 yeccgoto_expr(hd(Nss), 'ap', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_arrow(Stack),
 yeccgoto_expr(hd(Nss), 'arrow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_bind(Stack),
 yeccgoto_expr(hd(Nss), 'bind', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_comma(Stack),
 yeccgoto_expr(hd(Nss), 'comma', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_cons(Stack),
 yeccgoto_expr(hd(Nss), 'cons', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'do', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_do(Stack),
 yeccgoto_expr(hd(Nss), 'do', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_dot(Stack),
 yeccgoto_expr(hd(Nss), 'dot', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_effect(Stack),
 yeccgoto_expr(hd(Nss), 'effect', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_end(Stack),
 yeccgoto_expr(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_equals(Stack),
 yeccgoto_expr(hd(Nss), 'equals', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'error', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_error(Stack),
 yeccgoto_expr(hd(Nss), 'error', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'float', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_float(Stack),
 yeccgoto_expr(hd(Nss), 'float', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'fmap', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_fmap(Stack),
 yeccgoto_expr(hd(Nss), 'fmap', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'fn', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_fn(Stack),
 yeccgoto_expr(hd(Nss), 'fn', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'handle', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_handle(Stack),
 yeccgoto_expr(hd(Nss), 'handle', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'import', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_import(Stack),
 yeccgoto_expr(hd(Nss), 'import', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_in(Stack),
 yeccgoto_expr(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'instance', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_instance(Stack),
 yeccgoto_expr(hd(Nss), 'instance', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_integer(Stack),
 yeccgoto_expr(hd(Nss), 'integer', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'kleisli', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_kleisli(Stack),
 yeccgoto_expr(hd(Nss), 'kleisli', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_lbrace(Stack),
 yeccgoto_expr(hd(Nss), 'lbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_lbracket(Stack),
 yeccgoto_expr(hd(Nss), 'lbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'let', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_let(Stack),
 yeccgoto_expr(hd(Nss), 'let', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_lower_ident(Stack),
 yeccgoto_expr(hd(Nss), 'lower_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_lparen(Stack),
 yeccgoto_expr(hd(Nss), 'lparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'mappend', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_mappend(Stack),
 yeccgoto_expr(hd(Nss), 'mappend', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'match', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_match(Stack),
 yeccgoto_expr(hd(Nss), 'match', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_of(Stack),
 yeccgoto_expr(hd(Nss), 'of', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_or(Stack),
 yeccgoto_expr(hd(Nss), 'or', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_perform(Stack),
 yeccgoto_expr(hd(Nss), 'perform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_pipe(Stack),
 yeccgoto_expr(hd(Nss), 'pipe', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_pipe_right(Stack),
 yeccgoto_expr(hd(Nss), 'pipe_right', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'property', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_property(Stack),
 yeccgoto_expr(hd(Nss), 'property', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_rbrace(Stack),
 yeccgoto_expr(hd(Nss), 'rbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_rbracket(Stack),
 yeccgoto_expr(hd(Nss), 'rbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_rparen(Stack),
 yeccgoto_expr(hd(Nss), 'rparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'semicolon', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_semicolon(Stack),
 yeccgoto_expr(hd(Nss), 'semicolon', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'string', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_string(Stack),
 yeccgoto_expr(hd(Nss), 'string', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'test', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_test(Stack),
 yeccgoto_expr(hd(Nss), 'test', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_then(Stack),
 yeccgoto_expr(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'trait', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_trait(Stack),
 yeccgoto_expr(hd(Nss), 'trait', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_transform(Stack),
 yeccgoto_expr(hd(Nss), 'transform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'type', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_type(Stack),
 yeccgoto_expr(hd(Nss), 'type', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_upper_ident(Stack),
 yeccgoto_expr(hd(Nss), 'upper_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_222(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_223/7}).
-compile({nowarn_unused_function,  yeccpars2_223/7}).
yeccpars2_223(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'ap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'fmap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'kleisli', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'mappend', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 198, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 201, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_223_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_224/7}).
-compile({nowarn_unused_function,  yeccpars2_224/7}).
yeccpars2_224(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_224_(Stack),
 yeccgoto_expr_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_225/7}).
-compile({nowarn_unused_function,  yeccpars2_225/7}).
yeccpars2_225(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, 'ap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, 'fmap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, 'mappend', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 201, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_225_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_226/7}).
-compile({nowarn_unused_function,  yeccpars2_226/7}).
yeccpars2_226(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(S, 'mappend', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 201, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_226_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_227/7}).
-compile({nowarn_unused_function,  yeccpars2_227/7}).
yeccpars2_227(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 201, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_227_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_228/7}).
-compile({nowarn_unused_function,  yeccpars2_228/7}).
yeccpars2_228(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_228_(Stack),
 yeccgoto_perform_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_229/7}).
-compile({nowarn_unused_function,  yeccpars2_229/7}).
yeccpars2_229(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 247, Ss, Stack, T, Ts, Tzr);
yeccpars2_229(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_230/7}).
-compile({nowarn_unused_function,  yeccpars2_230/7}).
yeccpars2_230(S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 232, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_230_(Stack),
 yeccgoto_match_clauses(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_231(S, 'of', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 243, Ss, Stack, T, Ts, Tzr);
yeccpars2_231(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_231(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_231/7}).
-compile({nowarn_unused_function,  yeccpars2_231/7}).
yeccpars2_cont_231(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_231(S, 'ap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_231(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_231(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_231(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_231(S, 'fmap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_231(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_231(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_231(S, 'kleisli', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_231(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_231(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_231(S, 'mappend', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_231(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_231(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_231(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_231(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 198, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_231(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_231(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_231(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 201, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_231(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_231(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_231(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_231(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_232: see yeccpars2_103

-dialyzer({nowarn_function, yeccpars2_233/7}).
-compile({nowarn_unused_function,  yeccpars2_233/7}).
yeccpars2_233(S, 'as', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_233(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_233(S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 241, Ss, Stack, T, Ts, Tzr);
yeccpars2_233(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_233_(Stack),
 yeccgoto_or_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_234/7}).
-compile({nowarn_unused_function,  yeccpars2_234/7}).
yeccpars2_234(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 235, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 236, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_235: see yeccpars2_149

%% yeccpars2_236: see yeccpars2_149

-dialyzer({nowarn_function, yeccpars2_237/7}).
-compile({nowarn_unused_function,  yeccpars2_237/7}).
yeccpars2_237(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 238, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_238: see yeccpars2_149

-dialyzer({nowarn_function, yeccpars2_239/7}).
-compile({nowarn_unused_function,  yeccpars2_239/7}).
yeccpars2_239(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'ap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'fmap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'kleisli', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'mappend', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 198, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 201, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_239_(Stack),
 yeccgoto_match_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_240/7}).
-compile({nowarn_unused_function,  yeccpars2_240/7}).
yeccpars2_240(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(S, 'ap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(S, 'fmap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(S, 'kleisli', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(S, 'mappend', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 198, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 201, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_240_(Stack),
 yeccgoto_match_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_241: see yeccpars2_103

-dialyzer({nowarn_function, yeccpars2_242/7}).
-compile({nowarn_unused_function,  yeccpars2_242/7}).
yeccpars2_242(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_242_(Stack),
 yeccgoto_or_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_243/7}).
-compile({nowarn_unused_function,  yeccpars2_243/7}).
yeccpars2_243(S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 232, Ss, Stack, T, Ts, Tzr);
yeccpars2_243(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_244/7}).
-compile({nowarn_unused_function,  yeccpars2_244/7}).
yeccpars2_244(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 245, Ss, Stack, T, Ts, Tzr);
yeccpars2_244(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_245/7}).
-compile({nowarn_unused_function,  yeccpars2_245/7}).
yeccpars2_245(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_245_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_246/7}).
-compile({nowarn_unused_function,  yeccpars2_246/7}).
yeccpars2_246(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_246_(Stack),
 yeccgoto_match_clauses(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_247/7}).
-compile({nowarn_unused_function,  yeccpars2_247/7}).
yeccpars2_247(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_247_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_248/7}).
-compile({nowarn_unused_function,  yeccpars2_248/7}).
yeccpars2_248(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 254, Ss, Stack, T, Ts, Tzr);
yeccpars2_248(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_249(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 250, Ss, Stack, T, Ts, Tzr);
yeccpars2_249(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 251, Ss, Stack, T, Ts, Tzr);
yeccpars2_249(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_231(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_250: see yeccpars2_149

-dialyzer({nowarn_function, yeccpars2_251/7}).
-compile({nowarn_unused_function,  yeccpars2_251/7}).
yeccpars2_251(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_251_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_252/7}).
-compile({nowarn_unused_function,  yeccpars2_252/7}).
yeccpars2_252(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_252_(Stack),
 yeccgoto_tuple_expr_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_253/7}).
-compile({nowarn_unused_function,  yeccpars2_253/7}).
yeccpars2_253(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'ap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 250, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'fmap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'kleisli', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'mappend', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 198, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 201, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_253_(Stack),
 yeccgoto_tuple_expr_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_254/7}).
-compile({nowarn_unused_function,  yeccpars2_254/7}).
yeccpars2_254(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_254_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_255/7}).
-compile({nowarn_unused_function,  yeccpars2_255/7}).
yeccpars2_255(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 256, Ss, Stack, T, Ts, Tzr);
yeccpars2_255(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_256: see yeccpars2_149

yeccpars2_257(S, 'in', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 258, Ss, Stack, T, Ts, Tzr);
yeccpars2_257(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_231(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_258: see yeccpars2_149

-dialyzer({nowarn_function, yeccpars2_259/7}).
-compile({nowarn_unused_function,  yeccpars2_259/7}).
yeccpars2_259(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_259(S, 'ap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_259(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_259(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_259(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_259(S, 'fmap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_259(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_259(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_259(S, 'kleisli', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_259(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_259(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_259(S, 'mappend', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_259(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_259(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_259(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_259(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 198, Ss, Stack, T, Ts, Tzr);
yeccpars2_259(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_259(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_259(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 201, Ss, Stack, T, Ts, Tzr);
yeccpars2_259(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_259(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_259(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_259(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_259_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_260/7}).
-compile({nowarn_unused_function,  yeccpars2_260/7}).
yeccpars2_260(S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 262, Ss, Stack, T, Ts, Tzr);
yeccpars2_260(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_261/7}).
-compile({nowarn_unused_function,  yeccpars2_261/7}).
yeccpars2_261(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_261_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_262/7}).
-compile({nowarn_unused_function,  yeccpars2_262/7}).
yeccpars2_262(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_262_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_263/7}).
-compile({nowarn_unused_function,  yeccpars2_263/7}).
yeccpars2_263(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 271, Ss, Stack, T, Ts, Tzr);
yeccpars2_263(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_264/7}).
-compile({nowarn_unused_function,  yeccpars2_264/7}).
yeccpars2_264(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 269, Ss, Stack, T, Ts, Tzr);
yeccpars2_264(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_264_(Stack),
 yeccgoto_record_fields(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_265/7}).
-compile({nowarn_unused_function,  yeccpars2_265/7}).
yeccpars2_265(S, 'colon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 267, Ss, Stack, T, Ts, Tzr);
yeccpars2_265(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_266/7}).
-compile({nowarn_unused_function,  yeccpars2_266/7}).
yeccpars2_266(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_266_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_267: see yeccpars2_149

-dialyzer({nowarn_function, yeccpars2_268/7}).
-compile({nowarn_unused_function,  yeccpars2_268/7}).
yeccpars2_268(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_268(S, 'ap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_268(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_268(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_268(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_268(S, 'fmap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_268(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_268(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_268(S, 'kleisli', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_268(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_268(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_268(S, 'mappend', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_268(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_268(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_268(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_268(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 198, Ss, Stack, T, Ts, Tzr);
yeccpars2_268(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_268(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_268(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 201, Ss, Stack, T, Ts, Tzr);
yeccpars2_268(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_268(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_268(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_268(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_268_(Stack),
 yeccgoto_record_field(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_269/7}).
-compile({nowarn_unused_function,  yeccpars2_269/7}).
yeccpars2_269(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 265, Ss, Stack, T, Ts, Tzr);
yeccpars2_269(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_270/7}).
-compile({nowarn_unused_function,  yeccpars2_270/7}).
yeccpars2_270(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_270_(Stack),
 yeccgoto_record_fields(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_271/7}).
-compile({nowarn_unused_function,  yeccpars2_271/7}).
yeccpars2_271(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_271_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_272(S, 'then', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 273, Ss, Stack, T, Ts, Tzr);
yeccpars2_272(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_231(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_273/7}).
-compile({nowarn_unused_function,  yeccpars2_273/7}).
yeccpars2_273(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 274, Ss, Stack, T, Ts, Tzr);
yeccpars2_273(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_274/7}).
-compile({nowarn_unused_function,  yeccpars2_274/7}).
yeccpars2_274(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 277, Ss, Stack, T, Ts, Tzr);
yeccpars2_274(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_275/7}).
-compile({nowarn_unused_function,  yeccpars2_275/7}).
yeccpars2_275(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 295, Ss, Stack, T, Ts, Tzr);
yeccpars2_275(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_276/7}).
-compile({nowarn_unused_function,  yeccpars2_276/7}).
yeccpars2_276(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 277, Ss, Stack, T, Ts, Tzr);
yeccpars2_276(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_276_(Stack),
 yeccgoto_handler_clauses(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_277/7}).
-compile({nowarn_unused_function,  yeccpars2_277/7}).
yeccpars2_277(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 278, Ss, Stack, T, Ts, Tzr);
yeccpars2_277(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_278/7}).
-compile({nowarn_unused_function,  yeccpars2_278/7}).
yeccpars2_278(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 281, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_279/7}).
-compile({nowarn_unused_function,  yeccpars2_279/7}).
yeccpars2_279(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 281, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 293, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_280/7}).
-compile({nowarn_unused_function,  yeccpars2_280/7}).
yeccpars2_280(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_280_(Stack),
 yeccgoto_operation_cases(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_281/7}).
-compile({nowarn_unused_function,  yeccpars2_281/7}).
yeccpars2_281(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 282, Ss, Stack, T, Ts, Tzr);
yeccpars2_281(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 283, Ss, Stack, T, Ts, Tzr);
yeccpars2_281(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_282: see yeccpars2_149

-dialyzer({nowarn_function, yeccpars2_283/7}).
-compile({nowarn_unused_function,  yeccpars2_283/7}).
yeccpars2_283(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_283_(Stack),
 yeccpars2_284(284, Cat, [283 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_284/7}).
-compile({nowarn_unused_function,  yeccpars2_284/7}).
yeccpars2_284(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 288, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_285/7}).
-compile({nowarn_unused_function,  yeccpars2_285/7}).
yeccpars2_285(S, 'as', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 286, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_285_(Stack),
 yeccgoto_pattern_list_comma(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_286/7}).
-compile({nowarn_unused_function,  yeccpars2_286/7}).
yeccpars2_286(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_286_(Stack),
 yeccpars2_287(_S, Cat, [286 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_287/7}).
-compile({nowarn_unused_function,  yeccpars2_287/7}).
yeccpars2_287(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_287_(Stack),
 yeccgoto_pattern_list_comma(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_288/7}).
-compile({nowarn_unused_function,  yeccpars2_288/7}).
yeccpars2_288(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 289, Ss, Stack, T, Ts, Tzr);
yeccpars2_288(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_289: see yeccpars2_149

-dialyzer({nowarn_function, yeccpars2_290/7}).
-compile({nowarn_unused_function,  yeccpars2_290/7}).
yeccpars2_290(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_290_(Stack),
 yeccgoto_operation_case(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_291/7}).
-compile({nowarn_unused_function,  yeccpars2_291/7}).
yeccpars2_291(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_291_(Stack),
 yeccgoto_operation_case(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_292/7}).
-compile({nowarn_unused_function,  yeccpars2_292/7}).
yeccpars2_292(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_292_(Stack),
 yeccgoto_operation_cases(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_293/7}).
-compile({nowarn_unused_function,  yeccpars2_293/7}).
yeccpars2_293(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_293_(Stack),
 yeccgoto_handler_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_294/7}).
-compile({nowarn_unused_function,  yeccpars2_294/7}).
yeccpars2_294(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_294_(Stack),
 yeccgoto_handler_clauses(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_295/7}).
-compile({nowarn_unused_function,  yeccpars2_295/7}).
yeccpars2_295(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_295_(Stack),
 yeccgoto_try_with_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_296/7}).
-compile({nowarn_unused_function,  yeccpars2_296/7}).
yeccpars2_296(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 297, Ss, Stack, T, Ts, Tzr);
yeccpars2_296(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_297: see yeccpars2_149

-dialyzer({nowarn_function, yeccpars2_298/7}).
-compile({nowarn_unused_function,  yeccpars2_298/7}).
yeccpars2_298(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, 'ap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, 'fmap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, 'kleisli', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, 'mappend', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 198, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 201, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_298_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_299(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 303, Ss, Stack, T, Ts, Tzr);
yeccpars2_299(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 304, Ss, Stack, T, Ts, Tzr);
yeccpars2_299(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_149(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_300/7}).
-compile({nowarn_unused_function,  yeccpars2_300/7}).
yeccpars2_300(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_300(S, 'ap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_300(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_300(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_300(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_300(S, 'fmap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_300(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_300(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_300(S, 'kleisli', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_300(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_300(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_300(S, 'mappend', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_300(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_300(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_300(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_300(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 198, Ss, Stack, T, Ts, Tzr);
yeccpars2_300(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_300(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_300(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 201, Ss, Stack, T, Ts, Tzr);
yeccpars2_300(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_300(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_300(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_300(_S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_300_rbrace(Stack),
 yeccgoto_do_statements(hd(Ss), 'rbrace', Ss, NewStack, T, Ts, Tzr);
yeccpars2_300(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_300_(Stack),
 yeccgoto_do_statement(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_301/7}).
-compile({nowarn_unused_function,  yeccpars2_301/7}).
yeccpars2_301(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 312, Ss, Stack, T, Ts, Tzr);
yeccpars2_301(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_302/7}).
-compile({nowarn_unused_function,  yeccpars2_302/7}).
yeccpars2_302(S, 'semicolon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 310, Ss, Stack, T, Ts, Tzr);
yeccpars2_302(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_303/7}).
-compile({nowarn_unused_function,  yeccpars2_303/7}).
yeccpars2_303(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 307, Ss, Stack, T, Ts, Tzr);
yeccpars2_303(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_304/7}).
-compile({nowarn_unused_function,  yeccpars2_304/7}).
yeccpars2_304(S, 'left_arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 305, Ss, Stack, T, Ts, Tzr);
yeccpars2_304(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_304_(Stack),
 yeccgoto_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_305: see yeccpars2_149

-dialyzer({nowarn_function, yeccpars2_306/7}).
-compile({nowarn_unused_function,  yeccpars2_306/7}).
yeccpars2_306(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_306(S, 'ap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_306(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_306(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_306(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_306(S, 'fmap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_306(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_306(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_306(S, 'kleisli', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_306(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_306(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_306(S, 'mappend', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_306(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_306(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_306(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_306(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 198, Ss, Stack, T, Ts, Tzr);
yeccpars2_306(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_306(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_306(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 201, Ss, Stack, T, Ts, Tzr);
yeccpars2_306(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_306(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_306(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_306(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_306_(Stack),
 yeccgoto_do_statement(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_307/7}).
-compile({nowarn_unused_function,  yeccpars2_307/7}).
yeccpars2_307(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 308, Ss, Stack, T, Ts, Tzr);
yeccpars2_307(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_308: see yeccpars2_149

-dialyzer({nowarn_function, yeccpars2_309/7}).
-compile({nowarn_unused_function,  yeccpars2_309/7}).
yeccpars2_309(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_309(S, 'ap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_309(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_309(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_309(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_309(S, 'fmap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_309(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_309(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_309(S, 'in', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 258, Ss, Stack, T, Ts, Tzr);
yeccpars2_309(S, 'kleisli', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_309(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_309(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_309(S, 'mappend', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_309(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_309(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_309(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_309(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 198, Ss, Stack, T, Ts, Tzr);
yeccpars2_309(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_309(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_309(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 201, Ss, Stack, T, Ts, Tzr);
yeccpars2_309(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_309(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_309(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_309(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_309_(Stack),
 yeccgoto_do_statement(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_310: see yeccpars2_299

-dialyzer({nowarn_function, yeccpars2_311/7}).
-compile({nowarn_unused_function,  yeccpars2_311/7}).
yeccpars2_311(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_311_(Stack),
 yeccgoto_do_statements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_312/7}).
-compile({nowarn_unused_function,  yeccpars2_312/7}).
yeccpars2_312(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_312_(Stack),
 yeccgoto_do_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_313/7}).
-compile({nowarn_unused_function,  yeccpars2_313/7}).
yeccpars2_313(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_313_(Stack),
 yeccgoto_expr_app(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_314/7}).
-compile({nowarn_unused_function,  yeccpars2_314/7}).
yeccpars2_314(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 315, Ss, Stack, T, Ts, Tzr);
yeccpars2_314(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_315/7}).
-compile({nowarn_unused_function,  yeccpars2_315/7}).
yeccpars2_315(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_315_(Stack),
 yeccgoto_expr_app(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_316: see yeccpars2_149

-dialyzer({nowarn_function, yeccpars2_317/7}).
-compile({nowarn_unused_function,  yeccpars2_317/7}).
yeccpars2_317(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_317_(Stack),
 yeccgoto_guards(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_318: see yeccpars2_149

-dialyzer({nowarn_function, yeccpars2_319/7}).
-compile({nowarn_unused_function,  yeccpars2_319/7}).
yeccpars2_319(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_319(S, 'ap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_319(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_319(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_319(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_319(S, 'fmap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_319(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_319(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_319(S, 'kleisli', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_319(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_319(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_319(S, 'mappend', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_319(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_319(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_319(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_319(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 198, Ss, Stack, T, Ts, Tzr);
yeccpars2_319(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_319(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_319(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 201, Ss, Stack, T, Ts, Tzr);
yeccpars2_319(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_319(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_319(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_319(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_319_(Stack),
 yeccgoto_transform_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_320/7}).
-compile({nowarn_unused_function,  yeccpars2_320/7}).
yeccpars2_320(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'ap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'fmap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'kleisli', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'mappend', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 198, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 201, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_320_(Stack),
 yeccgoto_transform_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_321: see yeccpars2_149

-dialyzer({nowarn_function, yeccpars2_322/7}).
-compile({nowarn_unused_function,  yeccpars2_322/7}).
yeccpars2_322(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_322(S, 'ap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_322(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_322(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_322(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_322(S, 'fmap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_322(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_322(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_322(S, 'kleisli', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_322(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_322(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_322(S, 'mappend', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_322(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_322(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_322(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_322(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 198, Ss, Stack, T, Ts, Tzr);
yeccpars2_322(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_322(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_322(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 201, Ss, Stack, T, Ts, Tzr);
yeccpars2_322(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_322(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_322(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_322(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_322_(Stack),
 yeccgoto_transform_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_323/7}).
-compile({nowarn_unused_function,  yeccpars2_323/7}).
yeccpars2_323(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_323_(Stack),
 yeccgoto_trait_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_324/7}).
-compile({nowarn_unused_function,  yeccpars2_324/7}).
yeccpars2_324(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_324(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_324_(Stack),
 yeccpars2_325(325, Cat, [324 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_325/7}).
-compile({nowarn_unused_function,  yeccpars2_325/7}).
yeccpars2_325(S, 'extend', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 327, Ss, Stack, T, Ts, Tzr);
yeccpars2_325(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_325_(Stack),
 yeccpars2_326(326, Cat, [325 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_326/7}).
-compile({nowarn_unused_function,  yeccpars2_326/7}).
yeccpars2_326(S, 'where', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 332, Ss, Stack, T, Ts, Tzr);
yeccpars2_326(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_327: see yeccpars2_56

-dialyzer({nowarn_function, yeccpars2_328/7}).
-compile({nowarn_unused_function,  yeccpars2_328/7}).
yeccpars2_328(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_328_(Stack),
 yeccgoto_maybe_trait_extends(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_329/7}).
-compile({nowarn_unused_function,  yeccpars2_329/7}).
yeccpars2_329(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 330, Ss, Stack, T, Ts, Tzr);
yeccpars2_329(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_329_(Stack),
 yeccgoto_trait_extends_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_330: see yeccpars2_56

-dialyzer({nowarn_function, yeccpars2_331/7}).
-compile({nowarn_unused_function,  yeccpars2_331/7}).
yeccpars2_331(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_331_(Stack),
 yeccgoto_trait_extends_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_332/7}).
-compile({nowarn_unused_function,  yeccpars2_332/7}).
yeccpars2_332(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 335, Ss, Stack, T, Ts, Tzr);
yeccpars2_332(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_333/7}).
-compile({nowarn_unused_function,  yeccpars2_333/7}).
yeccpars2_333(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 344, Ss, Stack, T, Ts, Tzr);
yeccpars2_333(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_334/7}).
-compile({nowarn_unused_function,  yeccpars2_334/7}).
yeccpars2_334(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 342, Ss, Stack, T, Ts, Tzr);
yeccpars2_334(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_334_(Stack),
 yeccgoto_trait_members(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_335(S, 'colon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 337, Ss, Stack, T, Ts, Tzr);
yeccpars2_335(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_336/7}).
-compile({nowarn_unused_function,  yeccpars2_336/7}).
yeccpars2_336(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 340, Ss, Stack, T, Ts, Tzr);
yeccpars2_336(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_337(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 339, Ss, Stack, T, Ts, Tzr);
yeccpars2_337(S, 'forall', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_337(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_337(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_337(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_18(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_338/7}).
-compile({nowarn_unused_function,  yeccpars2_338/7}).
yeccpars2_338(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_338(S, 'constrain', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_338(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_338_(Stack),
 yeccgoto_trait_member(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_339/7}).
-compile({nowarn_unused_function,  yeccpars2_339/7}).
yeccpars2_339(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_339_(Stack),
 yeccgoto_trait_member(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_340: see yeccpars2_149

-dialyzer({nowarn_function, yeccpars2_341/7}).
-compile({nowarn_unused_function,  yeccpars2_341/7}).
yeccpars2_341(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(S, 'ap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(S, 'fmap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(S, 'kleisli', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(S, 'mappend', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 198, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 201, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_341_(Stack),
 yeccgoto_trait_member(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_342/7}).
-compile({nowarn_unused_function,  yeccpars2_342/7}).
yeccpars2_342(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 335, Ss, Stack, T, Ts, Tzr);
yeccpars2_342(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_342_(Stack),
 yeccgoto_trait_members(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_343/7}).
-compile({nowarn_unused_function,  yeccpars2_343/7}).
yeccpars2_343(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_343_(Stack),
 yeccgoto_trait_members(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_344/7}).
-compile({nowarn_unused_function,  yeccpars2_344/7}).
yeccpars2_344(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_344_(Stack),
 yeccgoto_trait_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_345/7}).
-compile({nowarn_unused_function,  yeccpars2_345/7}).
yeccpars2_345(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_345_(Stack),
 yeccgoto_test_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_346/7}).
-compile({nowarn_unused_function,  yeccpars2_346/7}).
yeccpars2_346(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 347, Ss, Stack, T, Ts, Tzr);
yeccpars2_346(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_347: see yeccpars2_149

-dialyzer({nowarn_function, yeccpars2_348/7}).
-compile({nowarn_unused_function,  yeccpars2_348/7}).
yeccpars2_348(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_348(S, 'ap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_348(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_348(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_348(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_348(S, 'fmap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_348(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_348(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_348(S, 'kleisli', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_348(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_348(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_348(S, 'mappend', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_348(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_348(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_348(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_348(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 198, Ss, Stack, T, Ts, Tzr);
yeccpars2_348(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_348(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_348(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 201, Ss, Stack, T, Ts, Tzr);
yeccpars2_348(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_348(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_348(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_348(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_348_(Stack),
 yeccgoto_test_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_349/7}).
-compile({nowarn_unused_function,  yeccpars2_349/7}).
yeccpars2_349(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_349_(Stack),
 yeccgoto_property_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_350/7}).
-compile({nowarn_unused_function,  yeccpars2_350/7}).
yeccpars2_350(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 351, Ss, Stack, T, Ts, Tzr);
yeccpars2_350(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_351/7}).
-compile({nowarn_unused_function,  yeccpars2_351/7}).
yeccpars2_351(S, 'forall', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 353, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_352/7}).
-compile({nowarn_unused_function,  yeccpars2_352/7}).
yeccpars2_352(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_352_(Stack),
 yeccgoto_property_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_353/7}).
-compile({nowarn_unused_function,  yeccpars2_353/7}).
yeccpars2_353(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 356, Ss, Stack, T, Ts, Tzr);
yeccpars2_353(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_354/7}).
-compile({nowarn_unused_function,  yeccpars2_354/7}).
yeccpars2_354(S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 361, Ss, Stack, T, Ts, Tzr);
yeccpars2_354(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_355/7}).
-compile({nowarn_unused_function,  yeccpars2_355/7}).
yeccpars2_355(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 359, Ss, Stack, T, Ts, Tzr);
yeccpars2_355(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_355_(Stack),
 yeccgoto_property_bindings(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_356/7}).
-compile({nowarn_unused_function,  yeccpars2_356/7}).
yeccpars2_356(S, 'colon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 357, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_357/7}).
-compile({nowarn_unused_function,  yeccpars2_357/7}).
yeccpars2_357(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 358, Ss, Stack, T, Ts, Tzr);
yeccpars2_357(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_358/7}).
-compile({nowarn_unused_function,  yeccpars2_358/7}).
yeccpars2_358(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_358_(Stack),
 yeccgoto_property_binding(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_359: see yeccpars2_353

-dialyzer({nowarn_function, yeccpars2_360/7}).
-compile({nowarn_unused_function,  yeccpars2_360/7}).
yeccpars2_360(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_360_(Stack),
 yeccgoto_property_bindings(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_361: see yeccpars2_149

-dialyzer({nowarn_function, yeccpars2_362/7}).
-compile({nowarn_unused_function,  yeccpars2_362/7}).
yeccpars2_362(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_362(S, 'ap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_362(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_362(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_362(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_362(S, 'fmap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_362(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_362(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_362(S, 'kleisli', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_362(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_362(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_362(S, 'mappend', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_362(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_362(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_362(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_362(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 198, Ss, Stack, T, Ts, Tzr);
yeccpars2_362(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_362(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_362(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 201, Ss, Stack, T, Ts, Tzr);
yeccpars2_362(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_362(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_362(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_362(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_362_(Stack),
 yeccgoto_property_body(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_363/7}).
-compile({nowarn_unused_function,  yeccpars2_363/7}).
yeccpars2_363(S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 364, Ss, Stack, T, Ts, Tzr);
yeccpars2_363(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_363_(Stack),
 yeccgoto_module_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_364/7}).
-compile({nowarn_unused_function,  yeccpars2_364/7}).
yeccpars2_364(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 365, Ss, Stack, T, Ts, Tzr);
yeccpars2_364(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_365/7}).
-compile({nowarn_unused_function,  yeccpars2_365/7}).
yeccpars2_365(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_365_(Stack),
 yeccgoto_module_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_366/7}).
-compile({nowarn_unused_function,  yeccpars2_366/7}).
yeccpars2_366(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 393, Ss, Stack, T, Ts, Tzr);
yeccpars2_366(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_366_(Stack),
 yeccgoto_instance_constraints(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_367/7}).
-compile({nowarn_unused_function,  yeccpars2_367/7}).
yeccpars2_367(S, 'double_arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 385, Ss, Stack, T, Ts, Tzr);
yeccpars2_367(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_368/7}).
-compile({nowarn_unused_function,  yeccpars2_368/7}).
yeccpars2_368(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_368_(Stack),
 yeccgoto_instance_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_369/7}).
-compile({nowarn_unused_function,  yeccpars2_369/7}).
yeccpars2_369(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_369(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_369(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_369(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_369(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_369_(Stack),
 yeccgoto_type_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_370/7}).
-compile({nowarn_unused_function,  yeccpars2_370/7}).
yeccpars2_370(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_370(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_370(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_370(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_370(_S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_370_comma(Stack),
 yeccgoto_type_expr_primary_list(hd(Ss), 'comma', Ss, NewStack, T, Ts, Tzr);
yeccpars2_370(_S, 'double_arrow', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_370_double_arrow(Stack),
 yeccgoto_type_expr_primary_list(hd(Ss), 'double_arrow', Ss, NewStack, T, Ts, Tzr);
yeccpars2_370(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_370_(Stack),
 yeccgoto_instance_type_args(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_371/7}).
-compile({nowarn_unused_function,  yeccpars2_371/7}).
yeccpars2_371(S, 'where', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 372, Ss, Stack, T, Ts, Tzr);
yeccpars2_371(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_372/7}).
-compile({nowarn_unused_function,  yeccpars2_372/7}).
yeccpars2_372(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 375, Ss, Stack, T, Ts, Tzr);
yeccpars2_372(S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 376, Ss, Stack, T, Ts, Tzr);
yeccpars2_372(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_373/7}).
-compile({nowarn_unused_function,  yeccpars2_373/7}).
yeccpars2_373(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 383, Ss, Stack, T, Ts, Tzr);
yeccpars2_373(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_374/7}).
-compile({nowarn_unused_function,  yeccpars2_374/7}).
yeccpars2_374(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 381, Ss, Stack, T, Ts, Tzr);
yeccpars2_374(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_374_(Stack),
 yeccgoto_instance_methods(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_375/7}).
-compile({nowarn_unused_function,  yeccpars2_375/7}).
yeccpars2_375(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_375_(Stack),
 yeccgoto_instance_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_376/7}).
-compile({nowarn_unused_function,  yeccpars2_376/7}).
yeccpars2_376(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 377, Ss, Stack, T, Ts, Tzr);
yeccpars2_376(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_377/7}).
-compile({nowarn_unused_function,  yeccpars2_377/7}).
yeccpars2_377(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_377(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_377(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_377(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_377(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_377(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_377(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_377(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_377(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_377(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_377_(Stack),
 yeccpars2_378(378, Cat, [377 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_378/7}).
-compile({nowarn_unused_function,  yeccpars2_378/7}).
yeccpars2_378(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 379, Ss, Stack, T, Ts, Tzr);
yeccpars2_378(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_379: see yeccpars2_149

-dialyzer({nowarn_function, yeccpars2_380/7}).
-compile({nowarn_unused_function,  yeccpars2_380/7}).
yeccpars2_380(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_380(S, 'ap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_380(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_380(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_380(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_380(S, 'fmap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_380(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_380(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_380(S, 'kleisli', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_380(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_380(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_380(S, 'mappend', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_380(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_380(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_380(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_380(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 198, Ss, Stack, T, Ts, Tzr);
yeccpars2_380(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_380(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_380(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 201, Ss, Stack, T, Ts, Tzr);
yeccpars2_380(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_380(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_380(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_380(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_380_(Stack),
 yeccgoto_instance_method(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_381/7}).
-compile({nowarn_unused_function,  yeccpars2_381/7}).
yeccpars2_381(S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 376, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_381_(Stack),
 yeccgoto_instance_methods(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_382/7}).
-compile({nowarn_unused_function,  yeccpars2_382/7}).
yeccpars2_382(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_382_(Stack),
 yeccgoto_instance_methods(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_383/7}).
-compile({nowarn_unused_function,  yeccpars2_383/7}).
yeccpars2_383(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_383_(Stack),
 yeccgoto_instance_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_384/7}).
-compile({nowarn_unused_function,  yeccpars2_384/7}).
yeccpars2_384(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_384_(Stack),
 yeccgoto_instance_type_args(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_385/7}).
-compile({nowarn_unused_function,  yeccpars2_385/7}).
yeccpars2_385(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 386, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_386(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_386(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_386(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_18(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_387/7}).
-compile({nowarn_unused_function,  yeccpars2_387/7}).
yeccpars2_387(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_387(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_387(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_387(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_387(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_387_(Stack),
 yeccgoto_instance_type_args(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_388/7}).
-compile({nowarn_unused_function,  yeccpars2_388/7}).
yeccpars2_388(S, 'where', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 389, Ss, Stack, T, Ts, Tzr);
yeccpars2_388(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_389/7}).
-compile({nowarn_unused_function,  yeccpars2_389/7}).
yeccpars2_389(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 391, Ss, Stack, T, Ts, Tzr);
yeccpars2_389(S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 376, Ss, Stack, T, Ts, Tzr);
yeccpars2_389(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_390/7}).
-compile({nowarn_unused_function,  yeccpars2_390/7}).
yeccpars2_390(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 392, Ss, Stack, T, Ts, Tzr);
yeccpars2_390(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_391/7}).
-compile({nowarn_unused_function,  yeccpars2_391/7}).
yeccpars2_391(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_391_(Stack),
 yeccgoto_instance_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_392/7}).
-compile({nowarn_unused_function,  yeccpars2_392/7}).
yeccpars2_392(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_392_(Stack),
 yeccgoto_instance_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_393: see yeccpars2_56

-dialyzer({nowarn_function, yeccpars2_394/7}).
-compile({nowarn_unused_function,  yeccpars2_394/7}).
yeccpars2_394(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_394_(Stack),
 yeccgoto_instance_constraints(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_395/7}).
-compile({nowarn_unused_function,  yeccpars2_395/7}).
yeccpars2_395(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 408, Ss, Stack, T, Ts, Tzr);
yeccpars2_395(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_396/7}).
-compile({nowarn_unused_function,  yeccpars2_396/7}).
yeccpars2_396(S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 397, Ss, Stack, T, Ts, Tzr);
yeccpars2_396(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 398, Ss, Stack, T, Ts, Tzr);
yeccpars2_396(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_396_(Stack),
 yeccgoto_import_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_397/7}).
-compile({nowarn_unused_function,  yeccpars2_397/7}).
yeccpars2_397(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 404, Ss, Stack, T, Ts, Tzr);
yeccpars2_397(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_398/7}).
-compile({nowarn_unused_function,  yeccpars2_398/7}).
yeccpars2_398(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 400, Ss, Stack, T, Ts, Tzr);
yeccpars2_398(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_399/7}).
-compile({nowarn_unused_function,  yeccpars2_399/7}).
yeccpars2_399(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 403, Ss, Stack, T, Ts, Tzr);
yeccpars2_399(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_400/7}).
-compile({nowarn_unused_function,  yeccpars2_400/7}).
yeccpars2_400(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 401, Ss, Stack, T, Ts, Tzr);
yeccpars2_400(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_400_(Stack),
 yeccgoto_import_items(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_401: see yeccpars2_398

-dialyzer({nowarn_function, yeccpars2_402/7}).
-compile({nowarn_unused_function,  yeccpars2_402/7}).
yeccpars2_402(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_402_(Stack),
 yeccgoto_import_items(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_403/7}).
-compile({nowarn_unused_function,  yeccpars2_403/7}).
yeccpars2_403(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_403_(Stack),
 yeccgoto_import_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_404/7}).
-compile({nowarn_unused_function,  yeccpars2_404/7}).
yeccpars2_404(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 405, Ss, Stack, T, Ts, Tzr);
yeccpars2_404(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_404_(Stack),
 yeccgoto_import_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_405: see yeccpars2_398

-dialyzer({nowarn_function, yeccpars2_406/7}).
-compile({nowarn_unused_function,  yeccpars2_406/7}).
yeccpars2_406(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 407, Ss, Stack, T, Ts, Tzr);
yeccpars2_406(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_407/7}).
-compile({nowarn_unused_function,  yeccpars2_407/7}).
yeccpars2_407(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_407_(Stack),
 yeccgoto_import_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_408/7}).
-compile({nowarn_unused_function,  yeccpars2_408/7}).
yeccpars2_408(S, 'as', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 409, Ss, Stack, T, Ts, Tzr);
yeccpars2_408(S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 410, Ss, Stack, T, Ts, Tzr);
yeccpars2_408(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_409/7}).
-compile({nowarn_unused_function,  yeccpars2_409/7}).
yeccpars2_409(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 417, Ss, Stack, T, Ts, Tzr);
yeccpars2_409(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_410/7}).
-compile({nowarn_unused_function,  yeccpars2_410/7}).
yeccpars2_410(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 411, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_411/7}).
-compile({nowarn_unused_function,  yeccpars2_411/7}).
yeccpars2_411(S, 'as', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 412, Ss, Stack, T, Ts, Tzr);
yeccpars2_411(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_412/7}).
-compile({nowarn_unused_function,  yeccpars2_412/7}).
yeccpars2_412(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 413, Ss, Stack, T, Ts, Tzr);
yeccpars2_412(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_413/7}).
-compile({nowarn_unused_function,  yeccpars2_413/7}).
yeccpars2_413(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 414, Ss, Stack, T, Ts, Tzr);
yeccpars2_413(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_413_(Stack),
 yeccgoto_import_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_414: see yeccpars2_398

-dialyzer({nowarn_function, yeccpars2_415/7}).
-compile({nowarn_unused_function,  yeccpars2_415/7}).
yeccpars2_415(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 416, Ss, Stack, T, Ts, Tzr);
yeccpars2_415(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_416/7}).
-compile({nowarn_unused_function,  yeccpars2_416/7}).
yeccpars2_416(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_416_(Stack),
 yeccgoto_import_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_417/7}).
-compile({nowarn_unused_function,  yeccpars2_417/7}).
yeccpars2_417(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 418, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_417_(Stack),
 yeccgoto_import_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_418: see yeccpars2_398

-dialyzer({nowarn_function, yeccpars2_419/7}).
-compile({nowarn_unused_function,  yeccpars2_419/7}).
yeccpars2_419(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 420, Ss, Stack, T, Ts, Tzr);
yeccpars2_419(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_420/7}).
-compile({nowarn_unused_function,  yeccpars2_420/7}).
yeccpars2_420(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_420_(Stack),
 yeccgoto_import_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_421/7}).
-compile({nowarn_unused_function,  yeccpars2_421/7}).
yeccpars2_421(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_421_(Stack),
 yeccgoto_declaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_422/7}).
-compile({nowarn_unused_function,  yeccpars2_422/7}).
yeccpars2_422(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_422_(Stack),
 yeccgoto_declaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_423/7}).
-compile({nowarn_unused_function,  yeccpars2_423/7}).
yeccpars2_423(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_423_(Stack),
 yeccgoto_declaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_424/7}).
-compile({nowarn_unused_function,  yeccpars2_424/7}).
yeccpars2_424(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_424_(Stack),
 yeccgoto_declaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_425/7}).
-compile({nowarn_unused_function,  yeccpars2_425/7}).
yeccpars2_425(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_425_(Stack),
 yeccgoto_declaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_426/7}).
-compile({nowarn_unused_function,  yeccpars2_426/7}).
yeccpars2_426(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_426_(Stack),
 yeccgoto_effect_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_427/7}).
-compile({nowarn_unused_function,  yeccpars2_427/7}).
yeccpars2_427(S, 'operation', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 430, Ss, Stack, T, Ts, Tzr);
yeccpars2_427(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_427_(Stack),
 yeccpars2_428(428, Cat, [427 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_428/7}).
-compile({nowarn_unused_function,  yeccpars2_428/7}).
yeccpars2_428(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 435, Ss, Stack, T, Ts, Tzr);
yeccpars2_428(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_429/7}).
-compile({nowarn_unused_function,  yeccpars2_429/7}).
yeccpars2_429(S, 'operation', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 430, Ss, Stack, T, Ts, Tzr);
yeccpars2_429(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_429_(Stack),
 yeccpars2_434(_S, Cat, [429 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_430/7}).
-compile({nowarn_unused_function,  yeccpars2_430/7}).
yeccpars2_430(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 431, Ss, Stack, T, Ts, Tzr);
yeccpars2_430(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_431/7}).
-compile({nowarn_unused_function,  yeccpars2_431/7}).
yeccpars2_431(S, 'colon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 432, Ss, Stack, T, Ts, Tzr);
yeccpars2_431(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_431_(Stack),
 yeccgoto_effect_operation(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_432: see yeccpars2_40

-dialyzer({nowarn_function, yeccpars2_433/7}).
-compile({nowarn_unused_function,  yeccpars2_433/7}).
yeccpars2_433(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_433(S, 'constrain', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_433(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_433_(Stack),
 yeccgoto_effect_operation(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_434/7}).
-compile({nowarn_unused_function,  yeccpars2_434/7}).
yeccpars2_434(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_434_(Stack),
 yeccgoto_effect_operations(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_435/7}).
-compile({nowarn_unused_function,  yeccpars2_435/7}).
yeccpars2_435(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_435_(Stack),
 yeccgoto_effect_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_436/7}).
-compile({nowarn_unused_function,  yeccpars2_436/7}).
yeccpars2_436(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_436_(Stack),
 yeccgoto_declarations(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_437/7}).
-compile({nowarn_unused_function,  yeccpars2_437/7}).
yeccpars2_437(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_437_(Stack),
 yeccgoto_export_decl(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_438/7}).
-compile({nowarn_unused_function,  yeccpars2_438/7}).
yeccpars2_438(S, 'export', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 440, Ss, Stack, T, Ts, Tzr);
yeccpars2_438(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_438_(Stack),
 yeccgoto_export_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_439/7}).
-compile({nowarn_unused_function,  yeccpars2_439/7}).
yeccpars2_439(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_439_(Stack),
 yeccgoto_module_header(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_440/7}).
-compile({nowarn_unused_function,  yeccpars2_440/7}).
yeccpars2_440(S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 441, Ss, Stack, T, Ts, Tzr);
yeccpars2_440(S, 'trait', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 442, Ss, Stack, T, Ts, Tzr);
yeccpars2_440(S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 443, Ss, Stack, T, Ts, Tzr);
yeccpars2_440(S, 'type', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 444, Ss, Stack, T, Ts, Tzr);
yeccpars2_440(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_441/7}).
-compile({nowarn_unused_function,  yeccpars2_441/7}).
yeccpars2_441(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 448, Ss, Stack, T, Ts, Tzr);
yeccpars2_441(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_442/7}).
-compile({nowarn_unused_function,  yeccpars2_442/7}).
yeccpars2_442(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 447, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_443/7}).
-compile({nowarn_unused_function,  yeccpars2_443/7}).
yeccpars2_443(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 446, Ss, Stack, T, Ts, Tzr);
yeccpars2_443(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_444/7}).
-compile({nowarn_unused_function,  yeccpars2_444/7}).
yeccpars2_444(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 445, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_445/7}).
-compile({nowarn_unused_function,  yeccpars2_445/7}).
yeccpars2_445(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_445_(Stack),
 yeccgoto_export_item(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_446/7}).
-compile({nowarn_unused_function,  yeccpars2_446/7}).
yeccpars2_446(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_446_(Stack),
 yeccgoto_export_item(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_447/7}).
-compile({nowarn_unused_function,  yeccpars2_447/7}).
yeccpars2_447(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_447_(Stack),
 yeccgoto_export_item(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_448/7}).
-compile({nowarn_unused_function,  yeccpars2_448/7}).
yeccpars2_448(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_448_(Stack),
 yeccgoto_export_item(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_449/7}).
-compile({nowarn_unused_function,  yeccpars2_449/7}).
yeccpars2_449(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_449_(Stack),
 yeccgoto_export_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_450/7}).
-compile({nowarn_unused_function,  yeccpars2_450/7}).
yeccpars2_450(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_450_(Stack),
 yeccgoto_catena_module(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_451/7}).
-compile({nowarn_unused_function,  yeccpars2_451/7}).
yeccpars2_451(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_451_(Stack),
 yeccgoto_transform_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_452/7}).
-compile({nowarn_unused_function,  yeccpars2_452/7}).
yeccpars2_452(S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 453, Ss, Stack, T, Ts, Tzr);
yeccpars2_452(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_452_(Stack),
 yeccgoto_transform_clauses(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_453/7}).
-compile({nowarn_unused_function,  yeccpars2_453/7}).
yeccpars2_453(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 454, Ss, Stack, T, Ts, Tzr);
yeccpars2_453(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_454/7}).
-compile({nowarn_unused_function,  yeccpars2_454/7}).
yeccpars2_454(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_454(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_454(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_454(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_454(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_454(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_454(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_454(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_454(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_454(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_454_(Stack),
 yeccpars2_455(455, Cat, [454 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_455/7}).
-compile({nowarn_unused_function,  yeccpars2_455/7}).
yeccpars2_455(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 456, Ss, Stack, T, Ts, Tzr);
yeccpars2_455(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 457, Ss, Stack, T, Ts, Tzr);
yeccpars2_455(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_456: see yeccpars2_149

%% yeccpars2_457: see yeccpars2_149

-dialyzer({nowarn_function, yeccpars2_458/7}).
-compile({nowarn_unused_function,  yeccpars2_458/7}).
yeccpars2_458(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 459, Ss, Stack, T, Ts, Tzr);
yeccpars2_458(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_459: see yeccpars2_149

-dialyzer({nowarn_function, yeccpars2_460/7}).
-compile({nowarn_unused_function,  yeccpars2_460/7}).
yeccpars2_460(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_460(S, 'ap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_460(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_460(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_460(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_460(S, 'fmap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_460(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_460(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_460(S, 'kleisli', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_460(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_460(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_460(S, 'mappend', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_460(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_460(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_460(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_460(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 198, Ss, Stack, T, Ts, Tzr);
yeccpars2_460(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_460(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_460(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 201, Ss, Stack, T, Ts, Tzr);
yeccpars2_460(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_460(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_460(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_460(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_460_(Stack),
 yeccgoto_transform_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_461/7}).
-compile({nowarn_unused_function,  yeccpars2_461/7}).
yeccpars2_461(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_461(S, 'ap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_461(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_461(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_461(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_461(S, 'fmap', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_461(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_461(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_461(S, 'kleisli', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_461(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_461(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_461(S, 'mappend', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_461(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_461(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_461(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_461(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 198, Ss, Stack, T, Ts, Tzr);
yeccpars2_461(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_461(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_461(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 201, Ss, Stack, T, Ts, Tzr);
yeccpars2_461(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_461(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_461(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_461(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_461_(Stack),
 yeccgoto_transform_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_462/7}).
-compile({nowarn_unused_function,  yeccpars2_462/7}).
yeccpars2_462(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_462_(Stack),
 yeccgoto_transform_clauses(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_catena_module/7}).
-compile({nowarn_unused_function,  yeccgoto_catena_module/7}).
yeccgoto_catena_module(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(14, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_constructor/7}).
-compile({nowarn_unused_function,  yeccgoto_constructor/7}).
yeccgoto_constructor(31, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_constructor(88, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_constructor(90, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_constructor_fields/7}).
-compile({nowarn_unused_function,  yeccgoto_constructor_fields/7}).
yeccgoto_constructor_fields(35=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_constructor_fields(36=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_constructors/7}).
-compile({nowarn_unused_function,  yeccgoto_constructors/7}).
yeccgoto_constructors(31=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_constructors(88=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_constructors(90=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_declaration/7}).
-compile({nowarn_unused_function,  yeccgoto_declaration/7}).
yeccgoto_declaration(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(13, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_declaration(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(13, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_declaration(13, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(13, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_declarations/7}).
-compile({nowarn_unused_function,  yeccgoto_declarations/7}).
yeccgoto_declarations(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_declarations(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_450(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_declarations(13=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_436(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_do_expr/7}).
-compile({nowarn_unused_function,  yeccgoto_do_expr/7}).
yeccgoto_do_expr(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(151=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(167=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(170=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(171=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(178=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(182=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(185=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(187=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(188=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(189=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(191=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(192=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(193=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(194=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(197=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(198=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(199=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(200=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(201=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(202=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(203=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(204=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(235=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(238=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(250=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(256=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(267=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(282=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(289=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(297=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(299=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(305=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(308=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(310=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(316=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(318=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(321=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(340=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(347=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(361=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(379=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(456=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(457=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_do_statement/7}).
-compile({nowarn_unused_function,  yeccgoto_do_statement/7}).
yeccgoto_do_statement(299, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_302(302, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_statement(310, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_302(302, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_do_statements/7}).
-compile({nowarn_unused_function,  yeccgoto_do_statements/7}).
yeccgoto_do_statements(299, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_301(301, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_statements(310=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_311(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_effect_decl/7}).
-compile({nowarn_unused_function,  yeccgoto_effect_decl/7}).
yeccgoto_effect_decl(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_effect_decl(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_effect_decl(13=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_effect_list_nonempty/7}).
-compile({nowarn_unused_function,  yeccgoto_effect_list_nonempty/7}).
yeccgoto_effect_list_nonempty(71, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(72, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_effect_list_nonempty(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_effect_operation/7}).
-compile({nowarn_unused_function,  yeccgoto_effect_operation/7}).
yeccgoto_effect_operation(427, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_429(429, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_effect_operation(429, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_429(429, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_effect_operations/7}).
-compile({nowarn_unused_function,  yeccgoto_effect_operations/7}).
yeccgoto_effect_operations(427, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_428(428, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_effect_operations(429=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_434(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_export_decl/7}).
-compile({nowarn_unused_function,  yeccgoto_export_decl/7}).
yeccgoto_export_decl(8=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_439(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_export_item/7}).
-compile({nowarn_unused_function,  yeccgoto_export_item/7}).
yeccgoto_export_item(8, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_438(438, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_export_item(438, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_438(438, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_export_list/7}).
-compile({nowarn_unused_function,  yeccgoto_export_list/7}).
yeccgoto_export_list(8=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_437(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_export_list(438=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_449(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_expr/7}).
-compile({nowarn_unused_function,  yeccgoto_expr/7}).
yeccgoto_expr(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_320(320, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(151, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_159(159, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(164, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_272(272, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(167, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_181(181, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(170, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_249(249, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(171, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_231(231, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(178, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_181(181, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(182, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_227(227, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(183, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_226(226, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(184, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_225(225, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(185, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_181(181, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(186, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_223(223, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(187, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(222, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(188, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(221, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(189, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_220(220, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_219(219, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(191, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_218(218, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(192, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_217(217, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(193, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_216(216, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(194, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_215(215, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(195, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(214, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(196, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_213(213, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(197, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_212(212, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(198, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_211(211, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(199, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_210(210, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(200, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_209(209, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(201, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_208(208, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(202, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_207(207, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(203=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_206(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(204=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_205(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(235, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_240(240, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(236, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_159(159, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(238, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_239(239, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(250, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_253(253, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(256, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_257(257, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(258, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_259(259, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(267, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_268(268, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(297, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_298(298, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(299, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_300(300, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(305, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_306(306, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(308, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_309(309, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(310, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_300(300, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(316, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_159(159, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(318, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_319(319, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(321, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_322(322, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(340, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_341(341, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(347, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_348(348, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(361, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_362(362, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(379, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_380(380, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(456, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_461(461, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(457, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_159(159, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(459, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_460(460, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_expr_app/7}).
-compile({nowarn_unused_function,  yeccgoto_expr_app/7}).
yeccgoto_expr_app(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(151, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(164, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(167, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(170, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(171, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(178, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(182, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(183, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(184, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(185, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(186, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(187, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(188, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(189, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(191, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(192, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(193, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(194, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(195, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(196, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(197, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(198, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(199, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(200, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(201, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(202, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(203, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(204, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(235, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(236, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(238, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(250, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(256, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(258, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(267, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(297, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(299, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(305, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(308, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(310, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(316, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(318, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(321, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(340, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(347, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(361, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(379, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(456, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(457, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(459, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_expr_list/7}).
-compile({nowarn_unused_function,  yeccgoto_expr_list/7}).
yeccgoto_expr_list(167, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_260(260, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_list(178=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_180(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_list(185=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_224(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_expr_list_opt/7}).
-compile({nowarn_unused_function,  yeccgoto_expr_list_opt/7}).
yeccgoto_expr_list_opt(178, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_179(179, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_expr_primary/7}).
-compile({nowarn_unused_function,  yeccgoto_expr_primary/7}).
yeccgoto_expr_primary(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(151=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_313(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(167=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(170=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(171=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(178=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(182=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(185=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(187=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(188=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(189=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(191=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(192=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(193=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(194=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(197=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(198=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(199=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(200=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(201=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(202=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(203=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(204=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(235=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(238=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(250=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(256=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(267=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(282=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_291(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(289=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_290(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(297=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(299=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(305=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(308=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(310=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(316=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(318=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(321=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(340=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(347=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(361=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(379=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(456=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(457=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_guard/7}).
-compile({nowarn_unused_function,  yeccgoto_guard/7}).
yeccgoto_guard(151, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_guard(236, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_guard(316, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_guard(457, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_guards/7}).
-compile({nowarn_unused_function,  yeccgoto_guards/7}).
yeccgoto_guards(151, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(155, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_guards(236, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_237(237, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_guards(316=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_317(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_guards(457, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_458(458, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_handler_clause/7}).
-compile({nowarn_unused_function,  yeccgoto_handler_clause/7}).
yeccgoto_handler_clause(274, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_276(276, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_handler_clause(276, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_276(276, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_handler_clauses/7}).
-compile({nowarn_unused_function,  yeccgoto_handler_clauses/7}).
yeccgoto_handler_clauses(274, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_275(275, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_handler_clauses(276=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_294(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_import_decl/7}).
-compile({nowarn_unused_function,  yeccgoto_import_decl/7}).
yeccgoto_import_decl(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_import_decl(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_import_decl(13=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_import_items/7}).
-compile({nowarn_unused_function,  yeccgoto_import_items/7}).
yeccgoto_import_items(398, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_399(399, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_import_items(401=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_402(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_import_items(405, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_406(406, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_import_items(414, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_415(415, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_import_items(418, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_419(419, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_instance_constraints/7}).
-compile({nowarn_unused_function,  yeccgoto_instance_constraints/7}).
yeccgoto_instance_constraints(18, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_367(367, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_instance_constraints(393=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_394(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_instance_decl/7}).
-compile({nowarn_unused_function,  yeccgoto_instance_decl/7}).
yeccgoto_instance_decl(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_instance_decl(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_instance_decl(13=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_instance_method/7}).
-compile({nowarn_unused_function,  yeccgoto_instance_method/7}).
yeccgoto_instance_method(372, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_374(374, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_instance_method(381, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_374(374, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_instance_method(389, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_374(374, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_instance_methods/7}).
-compile({nowarn_unused_function,  yeccgoto_instance_methods/7}).
yeccgoto_instance_methods(372, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_373(373, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_instance_methods(381=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_382(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_instance_methods(389, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_390(390, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_instance_type_args/7}).
-compile({nowarn_unused_function,  yeccgoto_instance_type_args/7}).
yeccgoto_instance_type_args(369, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_371(371, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_instance_type_args(370=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_384(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_instance_type_args(386, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_388(388, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_instance_type_args(387=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_384(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_literal/7}).
-compile({nowarn_unused_function,  yeccgoto_literal/7}).
yeccgoto_literal(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(151=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(167=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(170=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(171=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(178=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(182=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(185=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(187=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(188=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(189=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(191=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(192=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(193=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(194=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(197=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(198=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(199=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(200=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(201=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(202=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(203=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(204=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(235=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(238=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(250=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(256=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(267=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(282=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(289=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(297=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(299=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(305=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(308=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(310=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(316=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(318=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(321=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(340=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(347=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(361=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(379=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(456=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(457=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_match_clause/7}).
-compile({nowarn_unused_function,  yeccgoto_match_clause/7}).
yeccgoto_match_clause(171, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_230(230, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_clause(230, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_230(230, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_clause(243, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_230(230, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_match_clauses/7}).
-compile({nowarn_unused_function,  yeccgoto_match_clauses/7}).
yeccgoto_match_clauses(171, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_229(229, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_clauses(230=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_246(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_clauses(243, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(244, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_maybe_trait_extends/7}).
-compile({nowarn_unused_function,  yeccgoto_maybe_trait_extends/7}).
yeccgoto_maybe_trait_extends(325, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_326(326, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_module_decl/7}).
-compile({nowarn_unused_function,  yeccgoto_module_decl/7}).
yeccgoto_module_decl(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_module_header/7}).
-compile({nowarn_unused_function,  yeccgoto_module_header/7}).
yeccgoto_module_header(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_operation_case/7}).
-compile({nowarn_unused_function,  yeccgoto_operation_case/7}).
yeccgoto_operation_case(278=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_280(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operation_case(279=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_292(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_operation_cases/7}).
-compile({nowarn_unused_function,  yeccgoto_operation_cases/7}).
yeccgoto_operation_cases(278, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_279(279, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_or_pattern/7}).
-compile({nowarn_unused_function,  yeccgoto_or_pattern/7}).
yeccgoto_or_pattern(232, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_234(234, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_pattern(241=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_242(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pattern/7}).
-compile({nowarn_unused_function,  yeccgoto_pattern/7}).
yeccgoto_pattern(93, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(96, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(103, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(131, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(112, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(116, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(116, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(119, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(121, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(129, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(132, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(143, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(144, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(232, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_233(233, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(241, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_233(233, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(283, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_285(285, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(286, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_285(285, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(335, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(377, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(454, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pattern_list/7}).
-compile({nowarn_unused_function,  yeccgoto_pattern_list/7}).
yeccgoto_pattern_list(93, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(95, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(136, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(124, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list(112, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list(377, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_378(378, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list(454, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_455(455, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pattern_list_comma/7}).
-compile({nowarn_unused_function,  yeccgoto_pattern_list_comma/7}).
yeccgoto_pattern_list_comma(283, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_284(284, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list_comma(286=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_287(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pattern_list_nonempty/7}).
-compile({nowarn_unused_function,  yeccgoto_pattern_list_nonempty/7}).
yeccgoto_pattern_list_nonempty(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list_nonempty(96=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list_nonempty(101=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list_nonempty(110=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list_nonempty(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list_nonempty(116=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list_nonempty(335, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_336(336, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list_nonempty(377=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list_nonempty(454=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pattern_primary/7}).
-compile({nowarn_unused_function,  yeccgoto_pattern_primary/7}).
yeccgoto_pattern_primary(106, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(107, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_primary(107=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_127(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_perform_expr/7}).
-compile({nowarn_unused_function,  yeccgoto_perform_expr/7}).
yeccgoto_perform_expr(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(151=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(167=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(170=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(171=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(178=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(182=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(185=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(187=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(188=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(189=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(191=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(192=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(193=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(194=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(197=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(198=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(199=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(200=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(201=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(202=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(203=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(204=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(235=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(238=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(250=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(256=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(267=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(282=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(289=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(297=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(299=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(305=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(308=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(310=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(316=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(318=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(321=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(340=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(347=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(361=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(379=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(456=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(457=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_property_binding/7}).
-compile({nowarn_unused_function,  yeccgoto_property_binding/7}).
yeccgoto_property_binding(353, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_355(355, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_property_binding(359, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_355(355, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_property_bindings/7}).
-compile({nowarn_unused_function,  yeccgoto_property_bindings/7}).
yeccgoto_property_bindings(353, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_354(354, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_property_bindings(359=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_360(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_property_body/7}).
-compile({nowarn_unused_function,  yeccgoto_property_body/7}).
yeccgoto_property_body(351=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_352(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_property_decl/7}).
-compile({nowarn_unused_function,  yeccgoto_property_decl/7}).
yeccgoto_property_decl(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_property_decl(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_property_decl(13=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_record_field/7}).
-compile({nowarn_unused_function,  yeccgoto_record_field/7}).
yeccgoto_record_field(166, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_264(264, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_field(269, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_264(264, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_record_fields/7}).
-compile({nowarn_unused_function,  yeccgoto_record_fields/7}).
yeccgoto_record_fields(166, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_263(263, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_fields(269=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_270(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_record_pattern_field/7}).
-compile({nowarn_unused_function,  yeccgoto_record_pattern_field/7}).
yeccgoto_record_pattern_field(100, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(140, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_pattern_field(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(140, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_record_pattern_fields/7}).
-compile({nowarn_unused_function,  yeccgoto_record_pattern_fields/7}).
yeccgoto_record_pattern_fields(100, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(139, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_pattern_fields(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_test_decl/7}).
-compile({nowarn_unused_function,  yeccgoto_test_decl/7}).
yeccgoto_test_decl(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_test_decl(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_test_decl(13=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_trait_constraint/7}).
-compile({nowarn_unused_function,  yeccgoto_trait_constraint/7}).
yeccgoto_trait_constraint(18, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_366(366, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_trait_constraint(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_trait_constraint(60, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_trait_constraint(327, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_329(329, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_trait_constraint(330, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_329(329, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_trait_constraint(393, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_366(366, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_trait_decl/7}).
-compile({nowarn_unused_function,  yeccgoto_trait_decl/7}).
yeccgoto_trait_decl(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_trait_decl(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_trait_decl(13=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_trait_extends_list/7}).
-compile({nowarn_unused_function,  yeccgoto_trait_extends_list/7}).
yeccgoto_trait_extends_list(327=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_328(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_trait_extends_list(330=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_331(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_trait_member/7}).
-compile({nowarn_unused_function,  yeccgoto_trait_member/7}).
yeccgoto_trait_member(332, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_334(334, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_trait_member(342, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_334(334, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_trait_members/7}).
-compile({nowarn_unused_function,  yeccgoto_trait_members/7}).
yeccgoto_trait_members(332, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_333(333, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_trait_members(342=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_343(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_transform_clause/7}).
-compile({nowarn_unused_function,  yeccgoto_transform_clause/7}).
yeccgoto_transform_clause(2, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_452(452, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transform_clause(452, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_452(452, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_transform_clauses/7}).
-compile({nowarn_unused_function,  yeccgoto_transform_clauses/7}).
yeccgoto_transform_clauses(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_451(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transform_clauses(452=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_462(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_transform_decl/7}).
-compile({nowarn_unused_function,  yeccgoto_transform_decl/7}).
yeccgoto_transform_decl(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transform_decl(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transform_decl(13=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_transform_signature/7}).
-compile({nowarn_unused_function,  yeccgoto_transform_signature/7}).
yeccgoto_transform_signature(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transform_signature(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transform_signature(13, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_try_with_expr/7}).
-compile({nowarn_unused_function,  yeccgoto_try_with_expr/7}).
yeccgoto_try_with_expr(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(151=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(167=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(170=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(171=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(178=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(182=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(185=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(187=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(188=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(189=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(191=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(192=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(193=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(194=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(197=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(198=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(199=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(200=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(201=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(202=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(203=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(204=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(235=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(238=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(250=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(256=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(267=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(282=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(289=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(297=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(299=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(305=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(308=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(310=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(316=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(318=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(321=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(340=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(347=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(361=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(379=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(456=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(457=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_tuple_expr_list/7}).
-compile({nowarn_unused_function,  yeccgoto_tuple_expr_list/7}).
yeccgoto_tuple_expr_list(170, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_248(248, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple_expr_list(250=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_252(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_tuple_pattern_list/7}).
-compile({nowarn_unused_function,  yeccgoto_tuple_pattern_list/7}).
yeccgoto_tuple_pattern_list(103, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(130, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple_pattern_list(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_constraints/7}).
-compile({nowarn_unused_function,  yeccgoto_type_constraints/7}).
yeccgoto_type_constraints(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_constraints(60=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_decl/7}).
-compile({nowarn_unused_function,  yeccgoto_type_decl/7}).
yeccgoto_type_decl(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_decl(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_decl(13=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_expr/7}).
-compile({nowarn_unused_function,  yeccgoto_type_expr/7}).
yeccgoto_type_expr(40, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr(55, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(62, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr(63, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr(67, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr(82, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(83, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr(97, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_148(148, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr(337, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_338(338, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr(432, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_433(433, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_expr_app/7}).
-compile({nowarn_unused_function,  yeccgoto_type_expr_app/7}).
yeccgoto_type_expr_app(18=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(40, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(43, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(43, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(55, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(43, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(60=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(63, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(43, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(67, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(43, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(82, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(43, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(97, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(43, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(327=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(330=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(337, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(43, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(393=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(432, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(43, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_expr_list/7}).
-compile({nowarn_unused_function,  yeccgoto_type_expr_list/7}).
yeccgoto_type_expr_list(63, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(65, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_list(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_expr_primary/7}).
-compile({nowarn_unused_function,  yeccgoto_type_expr_primary/7}).
yeccgoto_type_expr_primary(18=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(35, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(36, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(40=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(46, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(47, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(49, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(60=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(63=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(82=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(97=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(327=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(330=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(337=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(369, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_370(370, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(370, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_370(370, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(386, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_387(387, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(387, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_387(387, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(393=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(432=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_expr_primary_list/7}).
-compile({nowarn_unused_function,  yeccgoto_type_expr_primary_list/7}).
yeccgoto_type_expr_primary_list(46=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary_list(47=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary_list(49=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary_list(369=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary_list(370=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_params/7}).
-compile({nowarn_unused_function,  yeccgoto_type_params/7}).
yeccgoto_type_params(26, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_params(45, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(52, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_params(324, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_325(325, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_params_nonempty/7}).
-compile({nowarn_unused_function,  yeccgoto_type_params_nonempty/7}).
yeccgoto_type_params_nonempty(26=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_params_nonempty(29=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_params_nonempty(45=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_params_nonempty(324=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_record_field/7}).
-compile({nowarn_unused_function,  yeccgoto_type_record_field/7}).
yeccgoto_type_record_field(38, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_record_field(84, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_record_fields/7}).
-compile({nowarn_unused_function,  yeccgoto_type_record_fields/7}).
yeccgoto_type_record_fields(38, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_record_fields(84=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_1_/1}).
-dialyzer({nowarn_function, yeccpars2_1_/1}).
-compile({nowarn_unused_function,  yeccpars2_1_/1}).
-file("src/compiler/parser/catena_parser.yrl", 261).
yeccpars2_1_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                           ___1
  end | __Stack].

-compile({inline,yeccpars2_2_/1}).
-dialyzer({nowarn_function, yeccpars2_2_/1}).
-compile({nowarn_unused_function,  yeccpars2_2_/1}).
-file("src/compiler/parser/catena_parser.yrl", 598).
yeccpars2_2_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                       
    {transform_decl,
        extract_transform_name(___1),
        extract_transform_type(___1),
        [],
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_3_/1}).
-dialyzer({nowarn_function, yeccpars2_3_/1}).
-compile({nowarn_unused_function,  yeccpars2_3_/1}).
-file("src/compiler/parser/catena_parser.yrl", 262).
yeccpars2_3_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                ___1
  end | __Stack].

-compile({inline,yeccpars2_4_/1}).
-dialyzer({nowarn_function, yeccpars2_4_/1}).
-compile({nowarn_unused_function,  yeccpars2_4_/1}).
-file("src/compiler/parser/catena_parser.yrl", 264).
yeccpars2_4_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                            ___1
  end | __Stack].

-compile({inline,yeccpars2_5_/1}).
-dialyzer({nowarn_function, yeccpars2_5_/1}).
-compile({nowarn_unused_function,  yeccpars2_5_/1}).
-file("src/compiler/parser/catena_parser.yrl", 266).
yeccpars2_5_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                           ___1
  end | __Stack].

-compile({inline,yeccpars2_6_/1}).
-dialyzer({nowarn_function, yeccpars2_6_/1}).
-compile({nowarn_unused_function,  yeccpars2_6_/1}).
-file("src/compiler/parser/catena_parser.yrl", 267).
yeccpars2_6_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               ___1
  end | __Stack].

-compile({inline,yeccpars2_8_/1}).
-dialyzer({nowarn_function, yeccpars2_8_/1}).
-compile({nowarn_unused_function,  yeccpars2_8_/1}).
-file("src/compiler/parser/catena_parser.yrl", 221).
yeccpars2_8_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                              
    {module_header, element(2, ___1), element(3, ___1), element(4, ___1)}
  end | __Stack].

-compile({inline,yeccpars2_9_/1}).
-dialyzer({nowarn_function, yeccpars2_9_/1}).
-compile({nowarn_unused_function,  yeccpars2_9_/1}).
-file("src/compiler/parser/catena_parser.yrl", 265).
yeccpars2_9_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               ___1
  end | __Stack].

-compile({inline,yeccpars2_10_/1}).
-dialyzer({nowarn_function, yeccpars2_10_/1}).
-compile({nowarn_unused_function,  yeccpars2_10_/1}).
-file("src/compiler/parser/catena_parser.yrl", 268).
yeccpars2_10_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             ___1
  end | __Stack].

-compile({inline,yeccpars2_11_/1}).
-dialyzer({nowarn_function, yeccpars2_11_/1}).
-compile({nowarn_unused_function,  yeccpars2_11_/1}).
-file("src/compiler/parser/catena_parser.yrl", 263).
yeccpars2_11_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             ___1
  end | __Stack].

-compile({inline,yeccpars2_12_/1}).
-dialyzer({nowarn_function, yeccpars2_12_/1}).
-compile({nowarn_unused_function,  yeccpars2_12_/1}).
-file("src/compiler/parser/catena_parser.yrl", 217).
yeccpars2_12_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               
    {module, undefined, [], extract_imports(___1), filter_imports(___1), {line, 1}}
  end | __Stack].

-compile({inline,yeccpars2_13_/1}).
-dialyzer({nowarn_function, yeccpars2_13_/1}).
-compile({nowarn_unused_function,  yeccpars2_13_/1}).
-file("src/compiler/parser/catena_parser.yrl", 256).
yeccpars2_13_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_25_/1}).
-dialyzer({nowarn_function, yeccpars2_25_/1}).
-compile({nowarn_unused_function,  yeccpars2_25_/1}).
-file("src/compiler/parser/catena_parser.yrl", 347).
yeccpars2_25_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                         
    make_error_declaration(extract_location(___1), "Incomplete type declaration", ___2)
  end | __Stack].

-compile({inline,yeccpars2_26_/1}).
-dialyzer({nowarn_function, yeccpars2_26_/1}).
-compile({nowarn_unused_function,  yeccpars2_26_/1}).
-file("src/compiler/parser/catena_parser.yrl", 350).
yeccpars2_26_(__Stack0) ->
 [begin
                         
    []
  end | __Stack0].

-compile({inline,yeccpars2_27_/1}).
-dialyzer({nowarn_function, yeccpars2_27_/1}).
-compile({nowarn_unused_function,  yeccpars2_27_/1}).
-file("src/compiler/parser/catena_parser.yrl", 352).
yeccpars2_27_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                     
    ___1
  end | __Stack].

-compile({inline,yeccpars2_29_/1}).
-dialyzer({nowarn_function, yeccpars2_29_/1}).
-compile({nowarn_unused_function,  yeccpars2_29_/1}).
-file("src/compiler/parser/catena_parser.yrl", 355).
yeccpars2_29_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                     
    [extract_atom(___1)]
  end | __Stack].

-compile({inline,yeccpars2_30_/1}).
-dialyzer({nowarn_function, yeccpars2_30_/1}).
-compile({nowarn_unused_function,  yeccpars2_30_/1}).
-file("src/compiler/parser/catena_parser.yrl", 357).
yeccpars2_30_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                          
    [extract_atom(___1) | ___2]
  end | __Stack].

-compile({inline,yeccpars2_32_/1}).
-dialyzer({nowarn_function, yeccpars2_32_/1}).
-compile({nowarn_unused_function,  yeccpars2_32_/1}).
-file("src/compiler/parser/catena_parser.yrl", 345).
yeccpars2_32_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                 
    make_error_declaration(extract_location(___1), "Missing '=' or constructors in type declaration", ___4)
  end | __Stack].

-compile({inline,yeccpars2_33_/1}).
-dialyzer({nowarn_function, yeccpars2_33_/1}).
-compile({nowarn_unused_function,  yeccpars2_33_/1}).
-file("src/compiler/parser/catena_parser.yrl", 334).
yeccpars2_33_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                               
    {type_decl,
        extract_atom(___2),
        ___3,
        ___5,
        [],
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_34_/1}).
-dialyzer({nowarn_function, yeccpars2_34_/1}).
-compile({nowarn_unused_function,  yeccpars2_34_/1}).
-file("src/compiler/parser/catena_parser.yrl", 360).
yeccpars2_34_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_35_/1}).
-dialyzer({nowarn_function, yeccpars2_35_/1}).
-compile({nowarn_unused_function,  yeccpars2_35_/1}).
-file("src/compiler/parser/catena_parser.yrl", 365).
yeccpars2_35_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                            
    {constructor,
        extract_atom(___1),
        [],
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_36_/1}).
-dialyzer({nowarn_function, yeccpars2_36_/1}).
-compile({nowarn_unused_function,  yeccpars2_36_/1}).
-file("src/compiler/parser/catena_parser.yrl", 377).
yeccpars2_36_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                         
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_37_/1}).
-dialyzer({nowarn_function, yeccpars2_37_/1}).
-compile({nowarn_unused_function,  yeccpars2_37_/1}).
-file("src/compiler/parser/catena_parser.yrl", 371).
yeccpars2_37_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                               
    {constructor,
        extract_atom(___1),
        ___2,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_39_/1}).
-dialyzer({nowarn_function, yeccpars2_39_/1}).
-compile({nowarn_unused_function,  yeccpars2_39_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1152).
yeccpars2_39_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                  
    {type_var, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_41_/1}).
-dialyzer({nowarn_function, yeccpars2_41_/1}).
-compile({nowarn_unused_function,  yeccpars2_41_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1155).
yeccpars2_41_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                  
    {type_con, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_42_/1}).
-dialyzer({nowarn_function, yeccpars2_42_/1}).
-compile({nowarn_unused_function,  yeccpars2_42_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1141).
yeccpars2_42_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                    
    ___1
  end | __Stack].

-compile({inline,yeccpars2_43_/1}).
-dialyzer({nowarn_function, yeccpars2_43_/1}).
-compile({nowarn_unused_function,  yeccpars2_43_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1115).
yeccpars2_43_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             ___1
  end | __Stack].

-compile({inline,yeccpars2_45_/1}).
-dialyzer({nowarn_function, yeccpars2_45_/1}).
-compile({nowarn_unused_function,  yeccpars2_45_/1}).
-file("src/compiler/parser/catena_parser.yrl", 350).
yeccpars2_45_(__Stack0) ->
 [begin
                         
    []
  end | __Stack0].

-compile({inline,yeccpars2_46_/1}).
-dialyzer({nowarn_function, yeccpars2_46_/1}).
-compile({nowarn_unused_function,  yeccpars2_46_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1152).
yeccpars2_46_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                  
    {type_var, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_47_/1}).
-dialyzer({nowarn_function, yeccpars2_47_/1}).
-compile({nowarn_unused_function,  yeccpars2_47_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1155).
yeccpars2_47_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                  
    {type_con, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_48_/1}).
-dialyzer({nowarn_function, yeccpars2_48_/1}).
-compile({nowarn_unused_function,  yeccpars2_48_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1128).
yeccpars2_48_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                     
    {type_app,
        {type_con, extract_atom(___1), extract_location(___1)},
        ___2,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_49_/1}).
-dialyzer({nowarn_function, yeccpars2_49_/1}).
-compile({nowarn_unused_function,  yeccpars2_49_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1146).
yeccpars2_49_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                             
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_50_/1}).
-dialyzer({nowarn_function, yeccpars2_50_/1}).
-compile({nowarn_unused_function,  yeccpars2_50_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1148).
yeccpars2_50_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                                    
    [___1 | ___2]
  end | __Stack].

-compile({inline,yeccpars2_51_/1}).
-dialyzer({nowarn_function, yeccpars2_51_/1}).
-compile({nowarn_unused_function,  yeccpars2_51_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1135).
yeccpars2_51_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                     
    {type_app,
        {type_var, extract_atom(___1), extract_location(___1)},
        ___2,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_54_/1}).
-dialyzer({nowarn_function, yeccpars2_54_/1}).
-compile({nowarn_unused_function,  yeccpars2_54_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1086).
yeccpars2_54_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                               
    {type_forall, ___2, ___4, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_57_/1}).
-dialyzer({nowarn_function, yeccpars2_57_/1}).
-compile({nowarn_unused_function,  yeccpars2_57_/1}).
-file("src/compiler/parser/catena_parser.yrl", 443).
yeccpars2_57_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                   
    extract_trait_constraint(___1)
  end | __Stack].

-compile({inline,yeccpars2_58_/1}).
-dialyzer({nowarn_function, yeccpars2_58_/1}).
-compile({nowarn_unused_function,  yeccpars2_58_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1112).
yeccpars2_58_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    {constrained_type, ___3, ___1, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_59_/1}).
-dialyzer({nowarn_function, yeccpars2_59_/1}).
-compile({nowarn_unused_function,  yeccpars2_59_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1118).
yeccpars2_59_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                      
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_61_/1}).
-dialyzer({nowarn_function, yeccpars2_61_/1}).
-compile({nowarn_unused_function,  yeccpars2_61_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1120).
yeccpars2_61_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                 
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_62_/1}).
-dialyzer({nowarn_function, yeccpars2_62_/1}).
-compile({nowarn_unused_function,  yeccpars2_62_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1083).
yeccpars2_62_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                        
    {type_fun, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_64_/1}).
-dialyzer({nowarn_function, yeccpars2_64_/1}).
-compile({nowarn_unused_function,  yeccpars2_64_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1159).
yeccpars2_64_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                              
    ___2
  end | __Stack].

-compile({inline,yeccpars2_66_/1}).
-dialyzer({nowarn_function, yeccpars2_66_/1}).
-compile({nowarn_unused_function,  yeccpars2_66_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1174).
yeccpars2_66_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_68_/1}).
-dialyzer({nowarn_function, yeccpars2_68_/1}).
-compile({nowarn_unused_function,  yeccpars2_68_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1176).
yeccpars2_68_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                  
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_69_/1}).
-dialyzer({nowarn_function, yeccpars2_69_/1}).
-compile({nowarn_unused_function,  yeccpars2_69_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1164).
yeccpars2_69_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                   
    {type_tuple, [___2 | ___4], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_73_/1}).
-dialyzer({nowarn_function, yeccpars2_73_/1}).
-compile({nowarn_unused_function,  yeccpars2_73_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1105).
yeccpars2_73_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                
    {type_effect, ___1, [], extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_74_/1}).
-dialyzer({nowarn_function, yeccpars2_74_/1}).
-compile({nowarn_unused_function,  yeccpars2_74_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1200).
yeccpars2_74_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                     
    [extract_atom(___1)]
  end | __Stack].

-compile({inline,yeccpars2_76_/1}).
-dialyzer({nowarn_function, yeccpars2_76_/1}).
-compile({nowarn_unused_function,  yeccpars2_76_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1202).
yeccpars2_76_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                
    [extract_atom(___1) | ___3]
  end | __Stack].

-compile({inline,yeccpars2_77_/1}).
-dialyzer({nowarn_function, yeccpars2_77_/1}).
-compile({nowarn_unused_function,  yeccpars2_77_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1108).
yeccpars2_77_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                     
    {type_effect, ___1, ___4, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_79_/1}).
-dialyzer({nowarn_function, yeccpars2_79_/1}).
-compile({nowarn_unused_function,  yeccpars2_79_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1186).
yeccpars2_79_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                         
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_81_/1}).
-dialyzer({nowarn_function, yeccpars2_81_/1}).
-compile({nowarn_unused_function,  yeccpars2_81_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1167).
yeccpars2_81_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                    
    {type_record, [], undefined, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_83_/1}).
-dialyzer({nowarn_function, yeccpars2_83_/1}).
-compile({nowarn_unused_function,  yeccpars2_83_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1191).
yeccpars2_83_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                  
    {extract_atom(___1), ___3}
  end | __Stack].

-compile({inline,yeccpars2_85_/1}).
-dialyzer({nowarn_function, yeccpars2_85_/1}).
-compile({nowarn_unused_function,  yeccpars2_85_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1188).
yeccpars2_85_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                  
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_86_/1}).
-dialyzer({nowarn_function, yeccpars2_86_/1}).
-compile({nowarn_unused_function,  yeccpars2_86_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1170).
yeccpars2_86_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                       
    {type_record, ___2, undefined, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_87_/1}).
-dialyzer({nowarn_function, yeccpars2_87_/1}).
-compile({nowarn_unused_function,  yeccpars2_87_/1}).
-file("src/compiler/parser/catena_parser.yrl", 379).
yeccpars2_87_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                            
    [___1 | ___2]
  end | __Stack].

-compile({inline,yeccpars2_89_/1}).
-dialyzer({nowarn_function, yeccpars2_89_/1}).
-compile({nowarn_unused_function,  yeccpars2_89_/1}).
-file("src/compiler/parser/catena_parser.yrl", 362).
yeccpars2_89_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                               
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_91_/1}).
-dialyzer({nowarn_function, yeccpars2_91_/1}).
-compile({nowarn_unused_function,  yeccpars2_91_/1}).
-file("src/compiler/parser/catena_parser.yrl", 343).
yeccpars2_91_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    make_error_declaration(extract_location(___1), "Invalid type name", ___2)
  end | __Stack].

-compile({inline,yeccpars2_92_/1}).
-dialyzer({nowarn_function, yeccpars2_92_/1}).
-compile({nowarn_unused_function,  yeccpars2_92_/1}).
-file("src/compiler/parser/catena_parser.yrl", 625).
yeccpars2_92_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                   
    make_error_declaration(extract_location(___1), "Incomplete transform declaration", ___2)
  end | __Stack].

-compile({inline,yeccpars2_93_/1}).
-dialyzer({nowarn_function, yeccpars2_93_/1}).
-compile({nowarn_unused_function,  yeccpars2_93_/1}).
-file("src/compiler/parser/catena_parser.yrl", 692).
yeccpars2_93_(__Stack0) ->
 [begin
                          
    []
  end | __Stack0].

-compile({inline,yeccpars2_94_/1}).
-dialyzer({nowarn_function, yeccpars2_94_/1}).
-compile({nowarn_unused_function,  yeccpars2_94_/1}).
-file("src/compiler/parser/catena_parser.yrl", 694).
yeccpars2_94_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                       
    ___1
  end | __Stack].

-compile({inline,yeccpars2_96_/1}).
-dialyzer({nowarn_function, yeccpars2_96_/1}).
-compile({nowarn_unused_function,  yeccpars2_96_/1}).
-file("src/compiler/parser/catena_parser.yrl", 697).
yeccpars2_96_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                  
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_98_/1}).
-dialyzer({nowarn_function, yeccpars2_98_/1}).
-compile({nowarn_unused_function,  yeccpars2_98_/1}).
-file("src/compiler/parser/catena_parser.yrl", 757).
yeccpars2_98_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                  
    {pat_literal, extract_value(___1), float, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_99_/1}).
-dialyzer({nowarn_function, yeccpars2_99_/1}).
-compile({nowarn_unused_function,  yeccpars2_99_/1}).
-file("src/compiler/parser/catena_parser.yrl", 754).
yeccpars2_99_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                    
    {pat_literal, extract_value(___1), integer, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_102_/1}).
-dialyzer({nowarn_function, yeccpars2_102_/1}).
-compile({nowarn_unused_function,  yeccpars2_102_/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_102_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                        
    {pat_var, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_104_/1}).
-dialyzer({nowarn_function, yeccpars2_104_/1}).
-compile({nowarn_unused_function,  yeccpars2_104_/1}).
-file("src/compiler/parser/catena_parser.yrl", 760).
yeccpars2_104_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                   
    {pat_literal, extract_value(___1), string, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_105_/1}).
-dialyzer({nowarn_function, yeccpars2_105_/1}).
-compile({nowarn_unused_function,  yeccpars2_105_/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_105_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                       
    {pat_wildcard, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_106_/1}).
-dialyzer({nowarn_function, yeccpars2_106_/1}).
-compile({nowarn_unused_function,  yeccpars2_106_/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_106_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                        
    {pat_constructor, extract_atom(___1), [], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_107_/1}).
-dialyzer({nowarn_function, yeccpars2_107_/1}).
-compile({nowarn_unused_function,  yeccpars2_107_/1}).
-file("src/compiler/parser/catena_parser.yrl", 723).
yeccpars2_107_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                        
    {pat_constructor, extract_atom(___1), [___2], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_108_/1}).
-dialyzer({nowarn_function, yeccpars2_108_/1}).
-compile({nowarn_unused_function,  yeccpars2_108_/1}).
-file("src/compiler/parser/catena_parser.yrl", 740).
yeccpars2_108_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                          
    {pat_literal, extract_value(___1), float, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_109_/1}).
-dialyzer({nowarn_function, yeccpars2_109_/1}).
-compile({nowarn_unused_function,  yeccpars2_109_/1}).
-file("src/compiler/parser/catena_parser.yrl", 737).
yeccpars2_109_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                            
    {pat_literal, extract_value(___1), integer, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_111_/1}).
-dialyzer({nowarn_function, yeccpars2_111_/1}).
-compile({nowarn_unused_function,  yeccpars2_111_/1}).
-file("src/compiler/parser/catena_parser.yrl", 731).
yeccpars2_111_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                
    {pat_var, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_112_/1}).
-dialyzer({nowarn_function, yeccpars2_112_/1}).
-compile({nowarn_unused_function,  yeccpars2_112_/1}).
-file("src/compiler/parser/catena_parser.yrl", 692).
yeccpars2_112_(__Stack0) ->
 [begin
                          
    []
  end | __Stack0].

-compile({inline,yeccpars2_113_/1}).
-dialyzer({nowarn_function, yeccpars2_113_/1}).
-compile({nowarn_unused_function,  yeccpars2_113_/1}).
-file("src/compiler/parser/catena_parser.yrl", 743).
yeccpars2_113_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                           
    {pat_literal, extract_value(___1), string, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_114_/1}).
-dialyzer({nowarn_function, yeccpars2_114_/1}).
-compile({nowarn_unused_function,  yeccpars2_114_/1}).
-file("src/compiler/parser/catena_parser.yrl", 734).
yeccpars2_114_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               
    {pat_wildcard, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_117_/1}).
-dialyzer({nowarn_function, yeccpars2_117_/1}).
-compile({nowarn_unused_function,  yeccpars2_117_/1}).
-file("src/compiler/parser/catena_parser.yrl", 699).
yeccpars2_117_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                        
    [___1 | ___2]
  end | __Stack].

-compile({inline,yeccpars2_120_/1}).
-dialyzer({nowarn_function, yeccpars2_120_/1}).
-compile({nowarn_unused_function,  yeccpars2_120_/1}).
-file("src/compiler/parser/catena_parser.yrl", 746).
yeccpars2_120_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                           ___2
  end | __Stack].

-compile({inline,yeccpars2_121_/1}).
-dialyzer({nowarn_function, yeccpars2_121_/1}).
-compile({nowarn_unused_function,  yeccpars2_121_/1}).
-file("src/compiler/parser/catena_parser.yrl", 779).
yeccpars2_121_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                 
    {pat_cons, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_122_/1}).
-dialyzer({nowarn_function, yeccpars2_122_/1}).
-compile({nowarn_unused_function,  yeccpars2_122_/1}).
-file("src/compiler/parser/catena_parser.yrl", 784).
yeccpars2_122_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                   
    {pat_as, extract_atom(___3), ___1, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_123_/1}).
-dialyzer({nowarn_function, yeccpars2_123_/1}).
-compile({nowarn_unused_function,  yeccpars2_123_/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_123_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    {pat_constructor, extract_atom(___1), ___3, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_125_/1}).
-dialyzer({nowarn_function, yeccpars2_125_/1}).
-compile({nowarn_unused_function,  yeccpars2_125_/1}).
-file("src/compiler/parser/catena_parser.yrl", 748).
yeccpars2_125_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                      
    {pat_list, [], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_126_/1}).
-dialyzer({nowarn_function, yeccpars2_126_/1}).
-compile({nowarn_unused_function,  yeccpars2_126_/1}).
-file("src/compiler/parser/catena_parser.yrl", 751).
yeccpars2_126_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    {pat_list, ___2, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_127_/1}).
-dialyzer({nowarn_function, yeccpars2_127_/1}).
-compile({nowarn_unused_function,  yeccpars2_127_/1}).
-file("src/compiler/parser/catena_parser.yrl", 727).
yeccpars2_127_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                        
    {pat_constructor, extract_atom(___1), [___2, ___3], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_133_/1}).
-dialyzer({nowarn_function, yeccpars2_133_/1}).
-compile({nowarn_unused_function,  yeccpars2_133_/1}).
-file("src/compiler/parser/catena_parser.yrl", 790).
yeccpars2_133_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                        
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_134_/1}).
-dialyzer({nowarn_function, yeccpars2_134_/1}).
-compile({nowarn_unused_function,  yeccpars2_134_/1}).
-file("src/compiler/parser/catena_parser.yrl", 788).
yeccpars2_134_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    [___1, ___3]
  end | __Stack].

-compile({inline,yeccpars2_135_/1}).
-dialyzer({nowarn_function, yeccpars2_135_/1}).
-compile({nowarn_unused_function,  yeccpars2_135_/1}).
-file("src/compiler/parser/catena_parser.yrl", 769).
yeccpars2_135_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    {pat_tuple, ___2, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_137_/1}).
-dialyzer({nowarn_function, yeccpars2_137_/1}).
-compile({nowarn_unused_function,  yeccpars2_137_/1}).
-file("src/compiler/parser/catena_parser.yrl", 763).
yeccpars2_137_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {pat_list, [], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_138_/1}).
-dialyzer({nowarn_function, yeccpars2_138_/1}).
-compile({nowarn_unused_function,  yeccpars2_138_/1}).
-file("src/compiler/parser/catena_parser.yrl", 766).
yeccpars2_138_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                           
    {pat_list, ___2, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_140_/1}).
-dialyzer({nowarn_function, yeccpars2_140_/1}).
-compile({nowarn_unused_function,  yeccpars2_140_/1}).
-file("src/compiler/parser/catena_parser.yrl", 794).
yeccpars2_140_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                               
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_142_/1}).
-dialyzer({nowarn_function, yeccpars2_142_/1}).
-compile({nowarn_unused_function,  yeccpars2_142_/1}).
-file("src/compiler/parser/catena_parser.yrl", 772).
yeccpars2_142_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                          
    {pat_record, [], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_144_/1}).
-dialyzer({nowarn_function, yeccpars2_144_/1}).
-compile({nowarn_unused_function,  yeccpars2_144_/1}).
-file("src/compiler/parser/catena_parser.yrl", 799).
yeccpars2_144_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    {extract_atom(___1), ___3}
  end | __Stack].

-compile({inline,yeccpars2_146_/1}).
-dialyzer({nowarn_function, yeccpars2_146_/1}).
-compile({nowarn_unused_function,  yeccpars2_146_/1}).
-file("src/compiler/parser/catena_parser.yrl", 796).
yeccpars2_146_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                           
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_147_/1}).
-dialyzer({nowarn_function, yeccpars2_147_/1}).
-compile({nowarn_unused_function,  yeccpars2_147_/1}).
-file("src/compiler/parser/catena_parser.yrl", 775).
yeccpars2_147_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                
    {pat_record, ___2, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_148_/1}).
-dialyzer({nowarn_function, yeccpars2_148_/1}).
-compile({nowarn_unused_function,  yeccpars2_148_/1}).
-file("src/compiler/parser/catena_parser.yrl", 630).
yeccpars2_148_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                              
    {transform_sig, extract_atom(___2), ___4, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_150_/1}).
-dialyzer({nowarn_function, yeccpars2_150_/1}).
-compile({nowarn_unused_function,  yeccpars2_150_/1}).
-file("src/compiler/parser/catena_parser.yrl", 623).
yeccpars2_150_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                            
    make_error_declaration(extract_location(___1), "Missing '=' or expression in transform declaration", ___4)
  end | __Stack].

-compile({inline,yeccpars2_152_/1}).
-dialyzer({nowarn_function, yeccpars2_152_/1}).
-compile({nowarn_unused_function,  yeccpars2_152_/1}).
-file("src/compiler/parser/catena_parser.yrl", 935).
yeccpars2_152_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                ___1
  end | __Stack].

-compile({inline,yeccpars2_153_/1}).
-dialyzer({nowarn_function, yeccpars2_153_/1}).
-compile({nowarn_unused_function,  yeccpars2_153_/1}).
-file("src/compiler/parser/catena_parser.yrl", 933).
yeccpars2_153_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               ___1
  end | __Stack].

-compile({inline,yeccpars2_154_/1}).
-dialyzer({nowarn_function, yeccpars2_154_/1}).
-compile({nowarn_unused_function,  yeccpars2_154_/1}).
-file("src/compiler/parser/catena_parser.yrl", 901).
yeccpars2_154_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                          ___1
  end | __Stack].

-compile({inline,yeccpars2_156_/1}).
-dialyzer({nowarn_function, yeccpars2_156_/1}).
-compile({nowarn_unused_function,  yeccpars2_156_/1}).
-file("src/compiler/parser/catena_parser.yrl", 806).
yeccpars2_156_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                 
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_157_/1}).
-dialyzer({nowarn_function, yeccpars2_157_/1}).
-compile({nowarn_unused_function,  yeccpars2_157_/1}).
-file("src/compiler/parser/catena_parser.yrl", 898).
yeccpars2_157_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                           ___1
  end | __Stack].

-compile({inline,yeccpars2_158_/1}).
-dialyzer({nowarn_function, yeccpars2_158_/1}).
-compile({nowarn_unused_function,  yeccpars2_158_/1}).
-file("src/compiler/parser/catena_parser.yrl", 889).
yeccpars2_158_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                   ___1
  end | __Stack].

-compile({inline,yeccpars2_159_/1}).
-dialyzer({nowarn_function, yeccpars2_159_/1}).
-compile({nowarn_unused_function,  yeccpars2_159_/1}).
-file("src/compiler/parser/catena_parser.yrl", 811).
yeccpars2_159_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                ___1
  end | __Stack].

-compile({inline,yeccpars2_160_/1}).
-dialyzer({nowarn_function, yeccpars2_160_/1}).
-compile({nowarn_unused_function,  yeccpars2_160_/1}).
-file("src/compiler/parser/catena_parser.yrl", 937).
yeccpars2_160_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                          ___1
  end | __Stack].

-compile({inline,yeccpars2_162_/1}).
-dialyzer({nowarn_function, yeccpars2_162_/1}).
-compile({nowarn_unused_function,  yeccpars2_162_/1}).
-file("src/compiler/parser/catena_parser.yrl", 988).
yeccpars2_162_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                  
    {literal, extract_value(___1), float, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_165_/1}).
-dialyzer({nowarn_function, yeccpars2_165_/1}).
-compile({nowarn_unused_function,  yeccpars2_165_/1}).
-file("src/compiler/parser/catena_parser.yrl", 985).
yeccpars2_165_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                    
    {literal, extract_value(___1), integer, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_169_/1}).
-dialyzer({nowarn_function, yeccpars2_169_/1}).
-compile({nowarn_unused_function,  yeccpars2_169_/1}).
-file("src/compiler/parser/catena_parser.yrl", 903).
yeccpars2_169_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             
    {var, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_173_/1}).
-dialyzer({nowarn_function, yeccpars2_173_/1}).
-compile({nowarn_unused_function,  yeccpars2_173_/1}).
-file("src/compiler/parser/catena_parser.yrl", 991).
yeccpars2_173_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                   
    {literal, extract_value(___1), string, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_174_/1}).
-dialyzer({nowarn_function, yeccpars2_174_/1}).
-compile({nowarn_unused_function,  yeccpars2_174_/1}).
-file("src/compiler/parser/catena_parser.yrl", 906).
yeccpars2_174_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             
    {var, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_178_/1}).
-dialyzer({nowarn_function, yeccpars2_178_/1}).
-compile({nowarn_unused_function,  yeccpars2_178_/1}).
-file("src/compiler/parser/catena_parser.yrl", 976).
yeccpars2_178_(__Stack0) ->
 [begin
                           
    []
  end | __Stack0].

-compile({inline,yeccpars2_180_/1}).
-dialyzer({nowarn_function, yeccpars2_180_/1}).
-compile({nowarn_unused_function,  yeccpars2_180_/1}).
-file("src/compiler/parser/catena_parser.yrl", 978).
yeccpars2_180_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                            
    ___1
  end | __Stack].

-compile({inline,yeccpars2_181_/1}).
-dialyzer({nowarn_function, yeccpars2_181_/1}).
-compile({nowarn_unused_function,  yeccpars2_181_/1}).
-file("src/compiler/parser/catena_parser.yrl", 970).
yeccpars2_181_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                   
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_205_/1}).
-dialyzer({nowarn_function, yeccpars2_205_/1}).
-compile({nowarn_unused_function,  yeccpars2_205_/1}).
-file("src/compiler/parser/catena_parser.yrl", 828).
yeccpars2_205_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                        
    {binary_op, star, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_/1}).
-dialyzer({nowarn_function, yeccpars2_206_/1}).
-compile({nowarn_unused_function,  yeccpars2_206_/1}).
-file("src/compiler/parser/catena_parser.yrl", 831).
yeccpars2_206_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                         
    {binary_op, slash, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,'yeccpars2_207_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_207_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_207_$end'/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
'yeccpars2_207_$end'(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_and/1}).
-dialyzer({nowarn_function, yeccpars2_207_and/1}).
-compile({nowarn_unused_function,  yeccpars2_207_and/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_and(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_ap/1}).
-dialyzer({nowarn_function, yeccpars2_207_ap/1}).
-compile({nowarn_unused_function,  yeccpars2_207_ap/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_ap(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_207_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_207_arrow/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_arrow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_bind/1}).
-dialyzer({nowarn_function, yeccpars2_207_bind/1}).
-compile({nowarn_unused_function,  yeccpars2_207_bind/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_bind(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_comma/1}).
-dialyzer({nowarn_function, yeccpars2_207_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_207_comma/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_comma(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_cons/1}).
-dialyzer({nowarn_function, yeccpars2_207_cons/1}).
-compile({nowarn_unused_function,  yeccpars2_207_cons/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_cons(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_do/1}).
-dialyzer({nowarn_function, yeccpars2_207_do/1}).
-compile({nowarn_unused_function,  yeccpars2_207_do/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_do(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_dot/1}).
-dialyzer({nowarn_function, yeccpars2_207_dot/1}).
-compile({nowarn_unused_function,  yeccpars2_207_dot/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_dot(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_effect/1}).
-dialyzer({nowarn_function, yeccpars2_207_effect/1}).
-compile({nowarn_unused_function,  yeccpars2_207_effect/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_effect(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_end/1}).
-dialyzer({nowarn_function, yeccpars2_207_end/1}).
-compile({nowarn_unused_function,  yeccpars2_207_end/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_end(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_equals/1}).
-dialyzer({nowarn_function, yeccpars2_207_equals/1}).
-compile({nowarn_unused_function,  yeccpars2_207_equals/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_equals(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_error/1}).
-dialyzer({nowarn_function, yeccpars2_207_error/1}).
-compile({nowarn_unused_function,  yeccpars2_207_error/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_error(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_float/1}).
-dialyzer({nowarn_function, yeccpars2_207_float/1}).
-compile({nowarn_unused_function,  yeccpars2_207_float/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_float(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_fmap/1}).
-dialyzer({nowarn_function, yeccpars2_207_fmap/1}).
-compile({nowarn_unused_function,  yeccpars2_207_fmap/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_fmap(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_fn/1}).
-dialyzer({nowarn_function, yeccpars2_207_fn/1}).
-compile({nowarn_unused_function,  yeccpars2_207_fn/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_fn(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_handle/1}).
-dialyzer({nowarn_function, yeccpars2_207_handle/1}).
-compile({nowarn_unused_function,  yeccpars2_207_handle/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_handle(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_import/1}).
-dialyzer({nowarn_function, yeccpars2_207_import/1}).
-compile({nowarn_unused_function,  yeccpars2_207_import/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_import(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_in/1}).
-dialyzer({nowarn_function, yeccpars2_207_in/1}).
-compile({nowarn_unused_function,  yeccpars2_207_in/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_in(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_instance/1}).
-dialyzer({nowarn_function, yeccpars2_207_instance/1}).
-compile({nowarn_unused_function,  yeccpars2_207_instance/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_instance(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_integer/1}).
-dialyzer({nowarn_function, yeccpars2_207_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_207_integer/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_integer(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_kleisli/1}).
-dialyzer({nowarn_function, yeccpars2_207_kleisli/1}).
-compile({nowarn_unused_function,  yeccpars2_207_kleisli/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_kleisli(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_lbrace/1}).
-dialyzer({nowarn_function, yeccpars2_207_lbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_207_lbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_lbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_lbracket/1}).
-dialyzer({nowarn_function, yeccpars2_207_lbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_207_lbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_lbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_let/1}).
-dialyzer({nowarn_function, yeccpars2_207_let/1}).
-compile({nowarn_unused_function,  yeccpars2_207_let/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_let(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_lower_ident/1}).
-dialyzer({nowarn_function, yeccpars2_207_lower_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_207_lower_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_lower_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_lparen/1}).
-dialyzer({nowarn_function, yeccpars2_207_lparen/1}).
-compile({nowarn_unused_function,  yeccpars2_207_lparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_lparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_mappend/1}).
-dialyzer({nowarn_function, yeccpars2_207_mappend/1}).
-compile({nowarn_unused_function,  yeccpars2_207_mappend/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_mappend(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_match/1}).
-dialyzer({nowarn_function, yeccpars2_207_match/1}).
-compile({nowarn_unused_function,  yeccpars2_207_match/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_match(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_of/1}).
-dialyzer({nowarn_function, yeccpars2_207_of/1}).
-compile({nowarn_unused_function,  yeccpars2_207_of/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_of(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_or/1}).
-dialyzer({nowarn_function, yeccpars2_207_or/1}).
-compile({nowarn_unused_function,  yeccpars2_207_or/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_or(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_perform/1}).
-dialyzer({nowarn_function, yeccpars2_207_perform/1}).
-compile({nowarn_unused_function,  yeccpars2_207_perform/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_perform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_pipe/1}).
-dialyzer({nowarn_function, yeccpars2_207_pipe/1}).
-compile({nowarn_unused_function,  yeccpars2_207_pipe/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_pipe(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_pipe_right/1}).
-dialyzer({nowarn_function, yeccpars2_207_pipe_right/1}).
-compile({nowarn_unused_function,  yeccpars2_207_pipe_right/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_pipe_right(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_property/1}).
-dialyzer({nowarn_function, yeccpars2_207_property/1}).
-compile({nowarn_unused_function,  yeccpars2_207_property/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_property(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_207_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_207_rbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_rbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_rbracket/1}).
-dialyzer({nowarn_function, yeccpars2_207_rbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_207_rbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_rbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_rparen/1}).
-dialyzer({nowarn_function, yeccpars2_207_rparen/1}).
-compile({nowarn_unused_function,  yeccpars2_207_rparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_rparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_semicolon/1}).
-dialyzer({nowarn_function, yeccpars2_207_semicolon/1}).
-compile({nowarn_unused_function,  yeccpars2_207_semicolon/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_semicolon(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_string/1}).
-dialyzer({nowarn_function, yeccpars2_207_string/1}).
-compile({nowarn_unused_function,  yeccpars2_207_string/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_string(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_test/1}).
-dialyzer({nowarn_function, yeccpars2_207_test/1}).
-compile({nowarn_unused_function,  yeccpars2_207_test/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_test(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_then/1}).
-dialyzer({nowarn_function, yeccpars2_207_then/1}).
-compile({nowarn_unused_function,  yeccpars2_207_then/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_then(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_trait/1}).
-dialyzer({nowarn_function, yeccpars2_207_trait/1}).
-compile({nowarn_unused_function,  yeccpars2_207_trait/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_trait(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_transform/1}).
-dialyzer({nowarn_function, yeccpars2_207_transform/1}).
-compile({nowarn_unused_function,  yeccpars2_207_transform/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_transform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_type/1}).
-dialyzer({nowarn_function, yeccpars2_207_type/1}).
-compile({nowarn_unused_function,  yeccpars2_207_type/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_type(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_upper_ident/1}).
-dialyzer({nowarn_function, yeccpars2_207_upper_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_207_upper_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 843).
yeccpars2_207_upper_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,'yeccpars2_208_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_208_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_208_$end'/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
'yeccpars2_208_$end'(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_and/1}).
-dialyzer({nowarn_function, yeccpars2_208_and/1}).
-compile({nowarn_unused_function,  yeccpars2_208_and/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_and(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_ap/1}).
-dialyzer({nowarn_function, yeccpars2_208_ap/1}).
-compile({nowarn_unused_function,  yeccpars2_208_ap/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_ap(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_208_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_208_arrow/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_arrow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_bind/1}).
-dialyzer({nowarn_function, yeccpars2_208_bind/1}).
-compile({nowarn_unused_function,  yeccpars2_208_bind/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_bind(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_comma/1}).
-dialyzer({nowarn_function, yeccpars2_208_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_208_comma/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_comma(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_cons/1}).
-dialyzer({nowarn_function, yeccpars2_208_cons/1}).
-compile({nowarn_unused_function,  yeccpars2_208_cons/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_cons(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_do/1}).
-dialyzer({nowarn_function, yeccpars2_208_do/1}).
-compile({nowarn_unused_function,  yeccpars2_208_do/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_do(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_dot/1}).
-dialyzer({nowarn_function, yeccpars2_208_dot/1}).
-compile({nowarn_unused_function,  yeccpars2_208_dot/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_dot(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_effect/1}).
-dialyzer({nowarn_function, yeccpars2_208_effect/1}).
-compile({nowarn_unused_function,  yeccpars2_208_effect/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_effect(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_end/1}).
-dialyzer({nowarn_function, yeccpars2_208_end/1}).
-compile({nowarn_unused_function,  yeccpars2_208_end/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_end(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_equals/1}).
-dialyzer({nowarn_function, yeccpars2_208_equals/1}).
-compile({nowarn_unused_function,  yeccpars2_208_equals/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_equals(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_error/1}).
-dialyzer({nowarn_function, yeccpars2_208_error/1}).
-compile({nowarn_unused_function,  yeccpars2_208_error/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_error(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_float/1}).
-dialyzer({nowarn_function, yeccpars2_208_float/1}).
-compile({nowarn_unused_function,  yeccpars2_208_float/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_float(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_fmap/1}).
-dialyzer({nowarn_function, yeccpars2_208_fmap/1}).
-compile({nowarn_unused_function,  yeccpars2_208_fmap/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_fmap(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_fn/1}).
-dialyzer({nowarn_function, yeccpars2_208_fn/1}).
-compile({nowarn_unused_function,  yeccpars2_208_fn/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_fn(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_handle/1}).
-dialyzer({nowarn_function, yeccpars2_208_handle/1}).
-compile({nowarn_unused_function,  yeccpars2_208_handle/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_handle(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_import/1}).
-dialyzer({nowarn_function, yeccpars2_208_import/1}).
-compile({nowarn_unused_function,  yeccpars2_208_import/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_import(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_in/1}).
-dialyzer({nowarn_function, yeccpars2_208_in/1}).
-compile({nowarn_unused_function,  yeccpars2_208_in/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_in(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_instance/1}).
-dialyzer({nowarn_function, yeccpars2_208_instance/1}).
-compile({nowarn_unused_function,  yeccpars2_208_instance/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_instance(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_integer/1}).
-dialyzer({nowarn_function, yeccpars2_208_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_208_integer/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_integer(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_kleisli/1}).
-dialyzer({nowarn_function, yeccpars2_208_kleisli/1}).
-compile({nowarn_unused_function,  yeccpars2_208_kleisli/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_kleisli(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_lbrace/1}).
-dialyzer({nowarn_function, yeccpars2_208_lbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_208_lbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_lbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_lbracket/1}).
-dialyzer({nowarn_function, yeccpars2_208_lbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_208_lbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_lbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_let/1}).
-dialyzer({nowarn_function, yeccpars2_208_let/1}).
-compile({nowarn_unused_function,  yeccpars2_208_let/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_let(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_lower_ident/1}).
-dialyzer({nowarn_function, yeccpars2_208_lower_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_208_lower_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_lower_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_lparen/1}).
-dialyzer({nowarn_function, yeccpars2_208_lparen/1}).
-compile({nowarn_unused_function,  yeccpars2_208_lparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_lparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_mappend/1}).
-dialyzer({nowarn_function, yeccpars2_208_mappend/1}).
-compile({nowarn_unused_function,  yeccpars2_208_mappend/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_mappend(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_match/1}).
-dialyzer({nowarn_function, yeccpars2_208_match/1}).
-compile({nowarn_unused_function,  yeccpars2_208_match/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_match(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_of/1}).
-dialyzer({nowarn_function, yeccpars2_208_of/1}).
-compile({nowarn_unused_function,  yeccpars2_208_of/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_of(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_or/1}).
-dialyzer({nowarn_function, yeccpars2_208_or/1}).
-compile({nowarn_unused_function,  yeccpars2_208_or/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_or(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_perform/1}).
-dialyzer({nowarn_function, yeccpars2_208_perform/1}).
-compile({nowarn_unused_function,  yeccpars2_208_perform/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_perform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_pipe/1}).
-dialyzer({nowarn_function, yeccpars2_208_pipe/1}).
-compile({nowarn_unused_function,  yeccpars2_208_pipe/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_pipe(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_pipe_right/1}).
-dialyzer({nowarn_function, yeccpars2_208_pipe_right/1}).
-compile({nowarn_unused_function,  yeccpars2_208_pipe_right/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_pipe_right(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_property/1}).
-dialyzer({nowarn_function, yeccpars2_208_property/1}).
-compile({nowarn_unused_function,  yeccpars2_208_property/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_property(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_208_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_208_rbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_rbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_rbracket/1}).
-dialyzer({nowarn_function, yeccpars2_208_rbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_208_rbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_rbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_rparen/1}).
-dialyzer({nowarn_function, yeccpars2_208_rparen/1}).
-compile({nowarn_unused_function,  yeccpars2_208_rparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_rparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_semicolon/1}).
-dialyzer({nowarn_function, yeccpars2_208_semicolon/1}).
-compile({nowarn_unused_function,  yeccpars2_208_semicolon/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_semicolon(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_string/1}).
-dialyzer({nowarn_function, yeccpars2_208_string/1}).
-compile({nowarn_unused_function,  yeccpars2_208_string/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_string(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_test/1}).
-dialyzer({nowarn_function, yeccpars2_208_test/1}).
-compile({nowarn_unused_function,  yeccpars2_208_test/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_test(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_then/1}).
-dialyzer({nowarn_function, yeccpars2_208_then/1}).
-compile({nowarn_unused_function,  yeccpars2_208_then/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_then(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_trait/1}).
-dialyzer({nowarn_function, yeccpars2_208_trait/1}).
-compile({nowarn_unused_function,  yeccpars2_208_trait/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_trait(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_transform/1}).
-dialyzer({nowarn_function, yeccpars2_208_transform/1}).
-compile({nowarn_unused_function,  yeccpars2_208_transform/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_transform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_type/1}).
-dialyzer({nowarn_function, yeccpars2_208_type/1}).
-compile({nowarn_unused_function,  yeccpars2_208_type/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_type(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_upper_ident/1}).
-dialyzer({nowarn_function, yeccpars2_208_upper_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_208_upper_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_208_upper_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_/1}).
-dialyzer({nowarn_function, yeccpars2_209_/1}).
-compile({nowarn_unused_function,  yeccpars2_209_/1}).
-file("src/compiler/parser/catena_parser.yrl", 866).
yeccpars2_209_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, plus_plus, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_/1}).
-dialyzer({nowarn_function, yeccpars2_210_/1}).
-compile({nowarn_unused_function,  yeccpars2_210_/1}).
-file("src/compiler/parser/catena_parser.yrl", 822).
yeccpars2_210_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                        
    {binary_op, plus, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_/1}).
-dialyzer({nowarn_function, yeccpars2_211_/1}).
-compile({nowarn_unused_function,  yeccpars2_211_/1}).
-file("src/compiler/parser/catena_parser.yrl", 819).
yeccpars2_211_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, pipe_right, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_/1}).
-dialyzer({nowarn_function, yeccpars2_212_/1}).
-compile({nowarn_unused_function,  yeccpars2_212_/1}).
-file("src/compiler/parser/catena_parser.yrl", 862).
yeccpars2_212_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                        
    {binary_op, 'or', ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,'yeccpars2_213_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_213_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_213_$end'/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
'yeccpars2_213_$end'(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_and/1}).
-dialyzer({nowarn_function, yeccpars2_213_and/1}).
-compile({nowarn_unused_function,  yeccpars2_213_and/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_and(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_ap/1}).
-dialyzer({nowarn_function, yeccpars2_213_ap/1}).
-compile({nowarn_unused_function,  yeccpars2_213_ap/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_ap(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_213_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_213_arrow/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_arrow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_bind/1}).
-dialyzer({nowarn_function, yeccpars2_213_bind/1}).
-compile({nowarn_unused_function,  yeccpars2_213_bind/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_bind(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_comma/1}).
-dialyzer({nowarn_function, yeccpars2_213_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_213_comma/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_comma(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_cons/1}).
-dialyzer({nowarn_function, yeccpars2_213_cons/1}).
-compile({nowarn_unused_function,  yeccpars2_213_cons/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_cons(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_do/1}).
-dialyzer({nowarn_function, yeccpars2_213_do/1}).
-compile({nowarn_unused_function,  yeccpars2_213_do/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_do(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_dot/1}).
-dialyzer({nowarn_function, yeccpars2_213_dot/1}).
-compile({nowarn_unused_function,  yeccpars2_213_dot/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_dot(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_effect/1}).
-dialyzer({nowarn_function, yeccpars2_213_effect/1}).
-compile({nowarn_unused_function,  yeccpars2_213_effect/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_effect(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_end/1}).
-dialyzer({nowarn_function, yeccpars2_213_end/1}).
-compile({nowarn_unused_function,  yeccpars2_213_end/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_end(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_equals/1}).
-dialyzer({nowarn_function, yeccpars2_213_equals/1}).
-compile({nowarn_unused_function,  yeccpars2_213_equals/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_equals(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_error/1}).
-dialyzer({nowarn_function, yeccpars2_213_error/1}).
-compile({nowarn_unused_function,  yeccpars2_213_error/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_error(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_float/1}).
-dialyzer({nowarn_function, yeccpars2_213_float/1}).
-compile({nowarn_unused_function,  yeccpars2_213_float/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_float(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_fmap/1}).
-dialyzer({nowarn_function, yeccpars2_213_fmap/1}).
-compile({nowarn_unused_function,  yeccpars2_213_fmap/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_fmap(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_fn/1}).
-dialyzer({nowarn_function, yeccpars2_213_fn/1}).
-compile({nowarn_unused_function,  yeccpars2_213_fn/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_fn(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_handle/1}).
-dialyzer({nowarn_function, yeccpars2_213_handle/1}).
-compile({nowarn_unused_function,  yeccpars2_213_handle/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_handle(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_import/1}).
-dialyzer({nowarn_function, yeccpars2_213_import/1}).
-compile({nowarn_unused_function,  yeccpars2_213_import/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_import(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_in/1}).
-dialyzer({nowarn_function, yeccpars2_213_in/1}).
-compile({nowarn_unused_function,  yeccpars2_213_in/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_in(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_instance/1}).
-dialyzer({nowarn_function, yeccpars2_213_instance/1}).
-compile({nowarn_unused_function,  yeccpars2_213_instance/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_instance(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_integer/1}).
-dialyzer({nowarn_function, yeccpars2_213_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_213_integer/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_integer(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_kleisli/1}).
-dialyzer({nowarn_function, yeccpars2_213_kleisli/1}).
-compile({nowarn_unused_function,  yeccpars2_213_kleisli/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_kleisli(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_lbrace/1}).
-dialyzer({nowarn_function, yeccpars2_213_lbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_213_lbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_lbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_lbracket/1}).
-dialyzer({nowarn_function, yeccpars2_213_lbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_213_lbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_lbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_let/1}).
-dialyzer({nowarn_function, yeccpars2_213_let/1}).
-compile({nowarn_unused_function,  yeccpars2_213_let/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_let(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_lower_ident/1}).
-dialyzer({nowarn_function, yeccpars2_213_lower_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_213_lower_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_lower_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_lparen/1}).
-dialyzer({nowarn_function, yeccpars2_213_lparen/1}).
-compile({nowarn_unused_function,  yeccpars2_213_lparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_lparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_mappend/1}).
-dialyzer({nowarn_function, yeccpars2_213_mappend/1}).
-compile({nowarn_unused_function,  yeccpars2_213_mappend/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_mappend(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_match/1}).
-dialyzer({nowarn_function, yeccpars2_213_match/1}).
-compile({nowarn_unused_function,  yeccpars2_213_match/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_match(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_of/1}).
-dialyzer({nowarn_function, yeccpars2_213_of/1}).
-compile({nowarn_unused_function,  yeccpars2_213_of/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_of(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_or/1}).
-dialyzer({nowarn_function, yeccpars2_213_or/1}).
-compile({nowarn_unused_function,  yeccpars2_213_or/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_or(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_perform/1}).
-dialyzer({nowarn_function, yeccpars2_213_perform/1}).
-compile({nowarn_unused_function,  yeccpars2_213_perform/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_perform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_pipe/1}).
-dialyzer({nowarn_function, yeccpars2_213_pipe/1}).
-compile({nowarn_unused_function,  yeccpars2_213_pipe/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_pipe(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_pipe_right/1}).
-dialyzer({nowarn_function, yeccpars2_213_pipe_right/1}).
-compile({nowarn_unused_function,  yeccpars2_213_pipe_right/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_pipe_right(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_property/1}).
-dialyzer({nowarn_function, yeccpars2_213_property/1}).
-compile({nowarn_unused_function,  yeccpars2_213_property/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_property(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_213_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_213_rbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_rbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_rbracket/1}).
-dialyzer({nowarn_function, yeccpars2_213_rbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_213_rbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_rbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_rparen/1}).
-dialyzer({nowarn_function, yeccpars2_213_rparen/1}).
-compile({nowarn_unused_function,  yeccpars2_213_rparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_rparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_semicolon/1}).
-dialyzer({nowarn_function, yeccpars2_213_semicolon/1}).
-compile({nowarn_unused_function,  yeccpars2_213_semicolon/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_semicolon(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_string/1}).
-dialyzer({nowarn_function, yeccpars2_213_string/1}).
-compile({nowarn_unused_function,  yeccpars2_213_string/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_string(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_test/1}).
-dialyzer({nowarn_function, yeccpars2_213_test/1}).
-compile({nowarn_unused_function,  yeccpars2_213_test/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_test(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_then/1}).
-dialyzer({nowarn_function, yeccpars2_213_then/1}).
-compile({nowarn_unused_function,  yeccpars2_213_then/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_then(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_trait/1}).
-dialyzer({nowarn_function, yeccpars2_213_trait/1}).
-compile({nowarn_unused_function,  yeccpars2_213_trait/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_trait(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_transform/1}).
-dialyzer({nowarn_function, yeccpars2_213_transform/1}).
-compile({nowarn_unused_function,  yeccpars2_213_transform/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_transform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_type/1}).
-dialyzer({nowarn_function, yeccpars2_213_type/1}).
-compile({nowarn_unused_function,  yeccpars2_213_type/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_type(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_upper_ident/1}).
-dialyzer({nowarn_function, yeccpars2_213_upper_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_213_upper_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 837).
yeccpars2_213_upper_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_214_/1}).
-dialyzer({nowarn_function, yeccpars2_214_/1}).
-compile({nowarn_unused_function,  yeccpars2_214_/1}).
-file("src/compiler/parser/catena_parser.yrl", 825).
yeccpars2_214_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                         
    {binary_op, minus, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_215_/1}).
-dialyzer({nowarn_function, yeccpars2_215_/1}).
-compile({nowarn_unused_function,  yeccpars2_215_/1}).
-file("src/compiler/parser/catena_parser.yrl", 883).
yeccpars2_215_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                           
    {binary_op, mappend, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,'yeccpars2_216_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_216_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_216_$end'/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
'yeccpars2_216_$end'(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_and/1}).
-dialyzer({nowarn_function, yeccpars2_216_and/1}).
-compile({nowarn_unused_function,  yeccpars2_216_and/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_and(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_ap/1}).
-dialyzer({nowarn_function, yeccpars2_216_ap/1}).
-compile({nowarn_unused_function,  yeccpars2_216_ap/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_ap(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_216_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_216_arrow/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_arrow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_bind/1}).
-dialyzer({nowarn_function, yeccpars2_216_bind/1}).
-compile({nowarn_unused_function,  yeccpars2_216_bind/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_bind(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_comma/1}).
-dialyzer({nowarn_function, yeccpars2_216_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_216_comma/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_comma(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_cons/1}).
-dialyzer({nowarn_function, yeccpars2_216_cons/1}).
-compile({nowarn_unused_function,  yeccpars2_216_cons/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_cons(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_do/1}).
-dialyzer({nowarn_function, yeccpars2_216_do/1}).
-compile({nowarn_unused_function,  yeccpars2_216_do/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_do(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_dot/1}).
-dialyzer({nowarn_function, yeccpars2_216_dot/1}).
-compile({nowarn_unused_function,  yeccpars2_216_dot/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_dot(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_effect/1}).
-dialyzer({nowarn_function, yeccpars2_216_effect/1}).
-compile({nowarn_unused_function,  yeccpars2_216_effect/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_effect(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_end/1}).
-dialyzer({nowarn_function, yeccpars2_216_end/1}).
-compile({nowarn_unused_function,  yeccpars2_216_end/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_end(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_eq/1}).
-dialyzer({nowarn_function, yeccpars2_216_eq/1}).
-compile({nowarn_unused_function,  yeccpars2_216_eq/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_eq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_equals/1}).
-dialyzer({nowarn_function, yeccpars2_216_equals/1}).
-compile({nowarn_unused_function,  yeccpars2_216_equals/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_equals(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_error/1}).
-dialyzer({nowarn_function, yeccpars2_216_error/1}).
-compile({nowarn_unused_function,  yeccpars2_216_error/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_error(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_float/1}).
-dialyzer({nowarn_function, yeccpars2_216_float/1}).
-compile({nowarn_unused_function,  yeccpars2_216_float/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_float(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_fmap/1}).
-dialyzer({nowarn_function, yeccpars2_216_fmap/1}).
-compile({nowarn_unused_function,  yeccpars2_216_fmap/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_fmap(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_fn/1}).
-dialyzer({nowarn_function, yeccpars2_216_fn/1}).
-compile({nowarn_unused_function,  yeccpars2_216_fn/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_fn(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_handle/1}).
-dialyzer({nowarn_function, yeccpars2_216_handle/1}).
-compile({nowarn_unused_function,  yeccpars2_216_handle/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_handle(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_import/1}).
-dialyzer({nowarn_function, yeccpars2_216_import/1}).
-compile({nowarn_unused_function,  yeccpars2_216_import/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_import(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_in/1}).
-dialyzer({nowarn_function, yeccpars2_216_in/1}).
-compile({nowarn_unused_function,  yeccpars2_216_in/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_in(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_instance/1}).
-dialyzer({nowarn_function, yeccpars2_216_instance/1}).
-compile({nowarn_unused_function,  yeccpars2_216_instance/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_instance(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_integer/1}).
-dialyzer({nowarn_function, yeccpars2_216_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_216_integer/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_integer(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_kleisli/1}).
-dialyzer({nowarn_function, yeccpars2_216_kleisli/1}).
-compile({nowarn_unused_function,  yeccpars2_216_kleisli/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_kleisli(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_lbrace/1}).
-dialyzer({nowarn_function, yeccpars2_216_lbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_216_lbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_lbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_lbracket/1}).
-dialyzer({nowarn_function, yeccpars2_216_lbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_216_lbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_lbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_let/1}).
-dialyzer({nowarn_function, yeccpars2_216_let/1}).
-compile({nowarn_unused_function,  yeccpars2_216_let/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_let(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_lower_ident/1}).
-dialyzer({nowarn_function, yeccpars2_216_lower_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_216_lower_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_lower_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_lparen/1}).
-dialyzer({nowarn_function, yeccpars2_216_lparen/1}).
-compile({nowarn_unused_function,  yeccpars2_216_lparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_lparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_mappend/1}).
-dialyzer({nowarn_function, yeccpars2_216_mappend/1}).
-compile({nowarn_unused_function,  yeccpars2_216_mappend/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_mappend(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_match/1}).
-dialyzer({nowarn_function, yeccpars2_216_match/1}).
-compile({nowarn_unused_function,  yeccpars2_216_match/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_match(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_neq/1}).
-dialyzer({nowarn_function, yeccpars2_216_neq/1}).
-compile({nowarn_unused_function,  yeccpars2_216_neq/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_neq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_of/1}).
-dialyzer({nowarn_function, yeccpars2_216_of/1}).
-compile({nowarn_unused_function,  yeccpars2_216_of/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_of(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_or/1}).
-dialyzer({nowarn_function, yeccpars2_216_or/1}).
-compile({nowarn_unused_function,  yeccpars2_216_or/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_or(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_perform/1}).
-dialyzer({nowarn_function, yeccpars2_216_perform/1}).
-compile({nowarn_unused_function,  yeccpars2_216_perform/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_perform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_pipe/1}).
-dialyzer({nowarn_function, yeccpars2_216_pipe/1}).
-compile({nowarn_unused_function,  yeccpars2_216_pipe/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_pipe(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_pipe_right/1}).
-dialyzer({nowarn_function, yeccpars2_216_pipe_right/1}).
-compile({nowarn_unused_function,  yeccpars2_216_pipe_right/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_pipe_right(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_property/1}).
-dialyzer({nowarn_function, yeccpars2_216_property/1}).
-compile({nowarn_unused_function,  yeccpars2_216_property/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_property(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_216_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_216_rbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_rbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_rbracket/1}).
-dialyzer({nowarn_function, yeccpars2_216_rbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_216_rbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_rbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_rparen/1}).
-dialyzer({nowarn_function, yeccpars2_216_rparen/1}).
-compile({nowarn_unused_function,  yeccpars2_216_rparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_rparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_semicolon/1}).
-dialyzer({nowarn_function, yeccpars2_216_semicolon/1}).
-compile({nowarn_unused_function,  yeccpars2_216_semicolon/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_semicolon(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_setoid_eq/1}).
-dialyzer({nowarn_function, yeccpars2_216_setoid_eq/1}).
-compile({nowarn_unused_function,  yeccpars2_216_setoid_eq/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_setoid_eq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_setoid_neq/1}).
-dialyzer({nowarn_function, yeccpars2_216_setoid_neq/1}).
-compile({nowarn_unused_function,  yeccpars2_216_setoid_neq/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_setoid_neq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_string/1}).
-dialyzer({nowarn_function, yeccpars2_216_string/1}).
-compile({nowarn_unused_function,  yeccpars2_216_string/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_string(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_test/1}).
-dialyzer({nowarn_function, yeccpars2_216_test/1}).
-compile({nowarn_unused_function,  yeccpars2_216_test/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_test(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_then/1}).
-dialyzer({nowarn_function, yeccpars2_216_then/1}).
-compile({nowarn_unused_function,  yeccpars2_216_then/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_then(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_trait/1}).
-dialyzer({nowarn_function, yeccpars2_216_trait/1}).
-compile({nowarn_unused_function,  yeccpars2_216_trait/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_trait(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_transform/1}).
-dialyzer({nowarn_function, yeccpars2_216_transform/1}).
-compile({nowarn_unused_function,  yeccpars2_216_transform/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_transform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_type/1}).
-dialyzer({nowarn_function, yeccpars2_216_type/1}).
-compile({nowarn_unused_function,  yeccpars2_216_type/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_type(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_upper_ident/1}).
-dialyzer({nowarn_function, yeccpars2_216_upper_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_216_upper_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_216_upper_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,'yeccpars2_217_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_217_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_217_$end'/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
'yeccpars2_217_$end'(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_and/1}).
-dialyzer({nowarn_function, yeccpars2_217_and/1}).
-compile({nowarn_unused_function,  yeccpars2_217_and/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_and(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_ap/1}).
-dialyzer({nowarn_function, yeccpars2_217_ap/1}).
-compile({nowarn_unused_function,  yeccpars2_217_ap/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_ap(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_217_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_217_arrow/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_arrow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_bind/1}).
-dialyzer({nowarn_function, yeccpars2_217_bind/1}).
-compile({nowarn_unused_function,  yeccpars2_217_bind/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_bind(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_comma/1}).
-dialyzer({nowarn_function, yeccpars2_217_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_217_comma/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_comma(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_cons/1}).
-dialyzer({nowarn_function, yeccpars2_217_cons/1}).
-compile({nowarn_unused_function,  yeccpars2_217_cons/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_cons(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_do/1}).
-dialyzer({nowarn_function, yeccpars2_217_do/1}).
-compile({nowarn_unused_function,  yeccpars2_217_do/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_do(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_dot/1}).
-dialyzer({nowarn_function, yeccpars2_217_dot/1}).
-compile({nowarn_unused_function,  yeccpars2_217_dot/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_dot(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_effect/1}).
-dialyzer({nowarn_function, yeccpars2_217_effect/1}).
-compile({nowarn_unused_function,  yeccpars2_217_effect/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_effect(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_end/1}).
-dialyzer({nowarn_function, yeccpars2_217_end/1}).
-compile({nowarn_unused_function,  yeccpars2_217_end/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_end(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_eq/1}).
-dialyzer({nowarn_function, yeccpars2_217_eq/1}).
-compile({nowarn_unused_function,  yeccpars2_217_eq/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_eq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_equals/1}).
-dialyzer({nowarn_function, yeccpars2_217_equals/1}).
-compile({nowarn_unused_function,  yeccpars2_217_equals/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_equals(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_error/1}).
-dialyzer({nowarn_function, yeccpars2_217_error/1}).
-compile({nowarn_unused_function,  yeccpars2_217_error/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_error(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_float/1}).
-dialyzer({nowarn_function, yeccpars2_217_float/1}).
-compile({nowarn_unused_function,  yeccpars2_217_float/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_float(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_fmap/1}).
-dialyzer({nowarn_function, yeccpars2_217_fmap/1}).
-compile({nowarn_unused_function,  yeccpars2_217_fmap/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_fmap(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_fn/1}).
-dialyzer({nowarn_function, yeccpars2_217_fn/1}).
-compile({nowarn_unused_function,  yeccpars2_217_fn/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_fn(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_handle/1}).
-dialyzer({nowarn_function, yeccpars2_217_handle/1}).
-compile({nowarn_unused_function,  yeccpars2_217_handle/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_handle(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_import/1}).
-dialyzer({nowarn_function, yeccpars2_217_import/1}).
-compile({nowarn_unused_function,  yeccpars2_217_import/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_import(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_in/1}).
-dialyzer({nowarn_function, yeccpars2_217_in/1}).
-compile({nowarn_unused_function,  yeccpars2_217_in/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_in(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_instance/1}).
-dialyzer({nowarn_function, yeccpars2_217_instance/1}).
-compile({nowarn_unused_function,  yeccpars2_217_instance/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_instance(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_integer/1}).
-dialyzer({nowarn_function, yeccpars2_217_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_217_integer/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_integer(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_kleisli/1}).
-dialyzer({nowarn_function, yeccpars2_217_kleisli/1}).
-compile({nowarn_unused_function,  yeccpars2_217_kleisli/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_kleisli(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_lbrace/1}).
-dialyzer({nowarn_function, yeccpars2_217_lbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_217_lbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_lbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_lbracket/1}).
-dialyzer({nowarn_function, yeccpars2_217_lbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_217_lbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_lbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_let/1}).
-dialyzer({nowarn_function, yeccpars2_217_let/1}).
-compile({nowarn_unused_function,  yeccpars2_217_let/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_let(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_lower_ident/1}).
-dialyzer({nowarn_function, yeccpars2_217_lower_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_217_lower_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_lower_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_lparen/1}).
-dialyzer({nowarn_function, yeccpars2_217_lparen/1}).
-compile({nowarn_unused_function,  yeccpars2_217_lparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_lparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_mappend/1}).
-dialyzer({nowarn_function, yeccpars2_217_mappend/1}).
-compile({nowarn_unused_function,  yeccpars2_217_mappend/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_mappend(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_match/1}).
-dialyzer({nowarn_function, yeccpars2_217_match/1}).
-compile({nowarn_unused_function,  yeccpars2_217_match/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_match(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_neq/1}).
-dialyzer({nowarn_function, yeccpars2_217_neq/1}).
-compile({nowarn_unused_function,  yeccpars2_217_neq/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_neq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_of/1}).
-dialyzer({nowarn_function, yeccpars2_217_of/1}).
-compile({nowarn_unused_function,  yeccpars2_217_of/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_of(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_or/1}).
-dialyzer({nowarn_function, yeccpars2_217_or/1}).
-compile({nowarn_unused_function,  yeccpars2_217_or/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_or(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_perform/1}).
-dialyzer({nowarn_function, yeccpars2_217_perform/1}).
-compile({nowarn_unused_function,  yeccpars2_217_perform/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_perform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_pipe/1}).
-dialyzer({nowarn_function, yeccpars2_217_pipe/1}).
-compile({nowarn_unused_function,  yeccpars2_217_pipe/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_pipe(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_pipe_right/1}).
-dialyzer({nowarn_function, yeccpars2_217_pipe_right/1}).
-compile({nowarn_unused_function,  yeccpars2_217_pipe_right/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_pipe_right(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_property/1}).
-dialyzer({nowarn_function, yeccpars2_217_property/1}).
-compile({nowarn_unused_function,  yeccpars2_217_property/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_property(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_217_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_217_rbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_rbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_rbracket/1}).
-dialyzer({nowarn_function, yeccpars2_217_rbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_217_rbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_rbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_rparen/1}).
-dialyzer({nowarn_function, yeccpars2_217_rparen/1}).
-compile({nowarn_unused_function,  yeccpars2_217_rparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_rparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_semicolon/1}).
-dialyzer({nowarn_function, yeccpars2_217_semicolon/1}).
-compile({nowarn_unused_function,  yeccpars2_217_semicolon/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_semicolon(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_setoid_eq/1}).
-dialyzer({nowarn_function, yeccpars2_217_setoid_eq/1}).
-compile({nowarn_unused_function,  yeccpars2_217_setoid_eq/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_setoid_eq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_setoid_neq/1}).
-dialyzer({nowarn_function, yeccpars2_217_setoid_neq/1}).
-compile({nowarn_unused_function,  yeccpars2_217_setoid_neq/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_setoid_neq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_string/1}).
-dialyzer({nowarn_function, yeccpars2_217_string/1}).
-compile({nowarn_unused_function,  yeccpars2_217_string/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_string(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_test/1}).
-dialyzer({nowarn_function, yeccpars2_217_test/1}).
-compile({nowarn_unused_function,  yeccpars2_217_test/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_test(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_then/1}).
-dialyzer({nowarn_function, yeccpars2_217_then/1}).
-compile({nowarn_unused_function,  yeccpars2_217_then/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_then(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_trait/1}).
-dialyzer({nowarn_function, yeccpars2_217_trait/1}).
-compile({nowarn_unused_function,  yeccpars2_217_trait/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_trait(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_transform/1}).
-dialyzer({nowarn_function, yeccpars2_217_transform/1}).
-compile({nowarn_unused_function,  yeccpars2_217_transform/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_transform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_type/1}).
-dialyzer({nowarn_function, yeccpars2_217_type/1}).
-compile({nowarn_unused_function,  yeccpars2_217_type/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_type(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_217_upper_ident/1}).
-dialyzer({nowarn_function, yeccpars2_217_upper_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_217_upper_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 846).
yeccpars2_217_upper_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_218_/1}).
-dialyzer({nowarn_function, yeccpars2_218_/1}).
-compile({nowarn_unused_function,  yeccpars2_218_/1}).
-file("src/compiler/parser/catena_parser.yrl", 886).
yeccpars2_218_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                           
    {binary_op, kleisli, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,'yeccpars2_219_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_219_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_219_$end'/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
'yeccpars2_219_$end'(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_and/1}).
-dialyzer({nowarn_function, yeccpars2_219_and/1}).
-compile({nowarn_unused_function,  yeccpars2_219_and/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_and(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_ap/1}).
-dialyzer({nowarn_function, yeccpars2_219_ap/1}).
-compile({nowarn_unused_function,  yeccpars2_219_ap/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_ap(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_219_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_219_arrow/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_arrow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_bind/1}).
-dialyzer({nowarn_function, yeccpars2_219_bind/1}).
-compile({nowarn_unused_function,  yeccpars2_219_bind/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_bind(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_comma/1}).
-dialyzer({nowarn_function, yeccpars2_219_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_219_comma/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_comma(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_cons/1}).
-dialyzer({nowarn_function, yeccpars2_219_cons/1}).
-compile({nowarn_unused_function,  yeccpars2_219_cons/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_cons(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_do/1}).
-dialyzer({nowarn_function, yeccpars2_219_do/1}).
-compile({nowarn_unused_function,  yeccpars2_219_do/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_do(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_dot/1}).
-dialyzer({nowarn_function, yeccpars2_219_dot/1}).
-compile({nowarn_unused_function,  yeccpars2_219_dot/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_dot(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_effect/1}).
-dialyzer({nowarn_function, yeccpars2_219_effect/1}).
-compile({nowarn_unused_function,  yeccpars2_219_effect/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_effect(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_end/1}).
-dialyzer({nowarn_function, yeccpars2_219_end/1}).
-compile({nowarn_unused_function,  yeccpars2_219_end/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_end(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_eq/1}).
-dialyzer({nowarn_function, yeccpars2_219_eq/1}).
-compile({nowarn_unused_function,  yeccpars2_219_eq/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_eq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_equals/1}).
-dialyzer({nowarn_function, yeccpars2_219_equals/1}).
-compile({nowarn_unused_function,  yeccpars2_219_equals/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_equals(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_error/1}).
-dialyzer({nowarn_function, yeccpars2_219_error/1}).
-compile({nowarn_unused_function,  yeccpars2_219_error/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_error(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_float/1}).
-dialyzer({nowarn_function, yeccpars2_219_float/1}).
-compile({nowarn_unused_function,  yeccpars2_219_float/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_float(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_fmap/1}).
-dialyzer({nowarn_function, yeccpars2_219_fmap/1}).
-compile({nowarn_unused_function,  yeccpars2_219_fmap/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_fmap(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_fn/1}).
-dialyzer({nowarn_function, yeccpars2_219_fn/1}).
-compile({nowarn_unused_function,  yeccpars2_219_fn/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_fn(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_handle/1}).
-dialyzer({nowarn_function, yeccpars2_219_handle/1}).
-compile({nowarn_unused_function,  yeccpars2_219_handle/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_handle(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_import/1}).
-dialyzer({nowarn_function, yeccpars2_219_import/1}).
-compile({nowarn_unused_function,  yeccpars2_219_import/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_import(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_in/1}).
-dialyzer({nowarn_function, yeccpars2_219_in/1}).
-compile({nowarn_unused_function,  yeccpars2_219_in/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_in(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_instance/1}).
-dialyzer({nowarn_function, yeccpars2_219_instance/1}).
-compile({nowarn_unused_function,  yeccpars2_219_instance/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_instance(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_integer/1}).
-dialyzer({nowarn_function, yeccpars2_219_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_219_integer/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_integer(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_kleisli/1}).
-dialyzer({nowarn_function, yeccpars2_219_kleisli/1}).
-compile({nowarn_unused_function,  yeccpars2_219_kleisli/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_kleisli(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_lbrace/1}).
-dialyzer({nowarn_function, yeccpars2_219_lbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_219_lbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_lbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_lbracket/1}).
-dialyzer({nowarn_function, yeccpars2_219_lbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_219_lbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_lbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_let/1}).
-dialyzer({nowarn_function, yeccpars2_219_let/1}).
-compile({nowarn_unused_function,  yeccpars2_219_let/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_let(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_lower_ident/1}).
-dialyzer({nowarn_function, yeccpars2_219_lower_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_219_lower_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_lower_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_lparen/1}).
-dialyzer({nowarn_function, yeccpars2_219_lparen/1}).
-compile({nowarn_unused_function,  yeccpars2_219_lparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_lparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_mappend/1}).
-dialyzer({nowarn_function, yeccpars2_219_mappend/1}).
-compile({nowarn_unused_function,  yeccpars2_219_mappend/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_mappend(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_match/1}).
-dialyzer({nowarn_function, yeccpars2_219_match/1}).
-compile({nowarn_unused_function,  yeccpars2_219_match/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_match(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_neq/1}).
-dialyzer({nowarn_function, yeccpars2_219_neq/1}).
-compile({nowarn_unused_function,  yeccpars2_219_neq/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_neq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_of/1}).
-dialyzer({nowarn_function, yeccpars2_219_of/1}).
-compile({nowarn_unused_function,  yeccpars2_219_of/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_of(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_or/1}).
-dialyzer({nowarn_function, yeccpars2_219_or/1}).
-compile({nowarn_unused_function,  yeccpars2_219_or/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_or(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_perform/1}).
-dialyzer({nowarn_function, yeccpars2_219_perform/1}).
-compile({nowarn_unused_function,  yeccpars2_219_perform/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_perform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_pipe/1}).
-dialyzer({nowarn_function, yeccpars2_219_pipe/1}).
-compile({nowarn_unused_function,  yeccpars2_219_pipe/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_pipe(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_pipe_right/1}).
-dialyzer({nowarn_function, yeccpars2_219_pipe_right/1}).
-compile({nowarn_unused_function,  yeccpars2_219_pipe_right/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_pipe_right(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_property/1}).
-dialyzer({nowarn_function, yeccpars2_219_property/1}).
-compile({nowarn_unused_function,  yeccpars2_219_property/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_property(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_219_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_219_rbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_rbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_rbracket/1}).
-dialyzer({nowarn_function, yeccpars2_219_rbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_219_rbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_rbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_rparen/1}).
-dialyzer({nowarn_function, yeccpars2_219_rparen/1}).
-compile({nowarn_unused_function,  yeccpars2_219_rparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_rparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_semicolon/1}).
-dialyzer({nowarn_function, yeccpars2_219_semicolon/1}).
-compile({nowarn_unused_function,  yeccpars2_219_semicolon/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_semicolon(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_setoid_eq/1}).
-dialyzer({nowarn_function, yeccpars2_219_setoid_eq/1}).
-compile({nowarn_unused_function,  yeccpars2_219_setoid_eq/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_setoid_eq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_setoid_neq/1}).
-dialyzer({nowarn_function, yeccpars2_219_setoid_neq/1}).
-compile({nowarn_unused_function,  yeccpars2_219_setoid_neq/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_setoid_neq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_string/1}).
-dialyzer({nowarn_function, yeccpars2_219_string/1}).
-compile({nowarn_unused_function,  yeccpars2_219_string/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_string(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_test/1}).
-dialyzer({nowarn_function, yeccpars2_219_test/1}).
-compile({nowarn_unused_function,  yeccpars2_219_test/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_test(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_then/1}).
-dialyzer({nowarn_function, yeccpars2_219_then/1}).
-compile({nowarn_unused_function,  yeccpars2_219_then/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_then(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_trait/1}).
-dialyzer({nowarn_function, yeccpars2_219_trait/1}).
-compile({nowarn_unused_function,  yeccpars2_219_trait/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_trait(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_transform/1}).
-dialyzer({nowarn_function, yeccpars2_219_transform/1}).
-compile({nowarn_unused_function,  yeccpars2_219_transform/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_transform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_type/1}).
-dialyzer({nowarn_function, yeccpars2_219_type/1}).
-compile({nowarn_unused_function,  yeccpars2_219_type/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_type(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_219_upper_ident/1}).
-dialyzer({nowarn_function, yeccpars2_219_upper_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_219_upper_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 855).
yeccpars2_219_upper_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,'yeccpars2_220_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_220_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_220_$end'/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
'yeccpars2_220_$end'(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_and/1}).
-dialyzer({nowarn_function, yeccpars2_220_and/1}).
-compile({nowarn_unused_function,  yeccpars2_220_and/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_and(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_ap/1}).
-dialyzer({nowarn_function, yeccpars2_220_ap/1}).
-compile({nowarn_unused_function,  yeccpars2_220_ap/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_ap(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_220_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_220_arrow/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_arrow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_bind/1}).
-dialyzer({nowarn_function, yeccpars2_220_bind/1}).
-compile({nowarn_unused_function,  yeccpars2_220_bind/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_bind(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_comma/1}).
-dialyzer({nowarn_function, yeccpars2_220_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_220_comma/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_comma(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_cons/1}).
-dialyzer({nowarn_function, yeccpars2_220_cons/1}).
-compile({nowarn_unused_function,  yeccpars2_220_cons/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_cons(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_do/1}).
-dialyzer({nowarn_function, yeccpars2_220_do/1}).
-compile({nowarn_unused_function,  yeccpars2_220_do/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_do(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_dot/1}).
-dialyzer({nowarn_function, yeccpars2_220_dot/1}).
-compile({nowarn_unused_function,  yeccpars2_220_dot/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_dot(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_effect/1}).
-dialyzer({nowarn_function, yeccpars2_220_effect/1}).
-compile({nowarn_unused_function,  yeccpars2_220_effect/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_effect(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_end/1}).
-dialyzer({nowarn_function, yeccpars2_220_end/1}).
-compile({nowarn_unused_function,  yeccpars2_220_end/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_end(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_eq/1}).
-dialyzer({nowarn_function, yeccpars2_220_eq/1}).
-compile({nowarn_unused_function,  yeccpars2_220_eq/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_eq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_equals/1}).
-dialyzer({nowarn_function, yeccpars2_220_equals/1}).
-compile({nowarn_unused_function,  yeccpars2_220_equals/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_equals(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_error/1}).
-dialyzer({nowarn_function, yeccpars2_220_error/1}).
-compile({nowarn_unused_function,  yeccpars2_220_error/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_error(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_float/1}).
-dialyzer({nowarn_function, yeccpars2_220_float/1}).
-compile({nowarn_unused_function,  yeccpars2_220_float/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_float(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_fmap/1}).
-dialyzer({nowarn_function, yeccpars2_220_fmap/1}).
-compile({nowarn_unused_function,  yeccpars2_220_fmap/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_fmap(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_fn/1}).
-dialyzer({nowarn_function, yeccpars2_220_fn/1}).
-compile({nowarn_unused_function,  yeccpars2_220_fn/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_fn(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_handle/1}).
-dialyzer({nowarn_function, yeccpars2_220_handle/1}).
-compile({nowarn_unused_function,  yeccpars2_220_handle/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_handle(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_import/1}).
-dialyzer({nowarn_function, yeccpars2_220_import/1}).
-compile({nowarn_unused_function,  yeccpars2_220_import/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_import(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_in/1}).
-dialyzer({nowarn_function, yeccpars2_220_in/1}).
-compile({nowarn_unused_function,  yeccpars2_220_in/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_in(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_instance/1}).
-dialyzer({nowarn_function, yeccpars2_220_instance/1}).
-compile({nowarn_unused_function,  yeccpars2_220_instance/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_instance(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_integer/1}).
-dialyzer({nowarn_function, yeccpars2_220_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_220_integer/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_integer(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_kleisli/1}).
-dialyzer({nowarn_function, yeccpars2_220_kleisli/1}).
-compile({nowarn_unused_function,  yeccpars2_220_kleisli/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_kleisli(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_lbrace/1}).
-dialyzer({nowarn_function, yeccpars2_220_lbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_220_lbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_lbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_lbracket/1}).
-dialyzer({nowarn_function, yeccpars2_220_lbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_220_lbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_lbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_let/1}).
-dialyzer({nowarn_function, yeccpars2_220_let/1}).
-compile({nowarn_unused_function,  yeccpars2_220_let/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_let(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_lower_ident/1}).
-dialyzer({nowarn_function, yeccpars2_220_lower_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_220_lower_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_lower_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_lparen/1}).
-dialyzer({nowarn_function, yeccpars2_220_lparen/1}).
-compile({nowarn_unused_function,  yeccpars2_220_lparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_lparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_mappend/1}).
-dialyzer({nowarn_function, yeccpars2_220_mappend/1}).
-compile({nowarn_unused_function,  yeccpars2_220_mappend/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_mappend(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_match/1}).
-dialyzer({nowarn_function, yeccpars2_220_match/1}).
-compile({nowarn_unused_function,  yeccpars2_220_match/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_match(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_neq/1}).
-dialyzer({nowarn_function, yeccpars2_220_neq/1}).
-compile({nowarn_unused_function,  yeccpars2_220_neq/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_neq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_of/1}).
-dialyzer({nowarn_function, yeccpars2_220_of/1}).
-compile({nowarn_unused_function,  yeccpars2_220_of/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_of(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_or/1}).
-dialyzer({nowarn_function, yeccpars2_220_or/1}).
-compile({nowarn_unused_function,  yeccpars2_220_or/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_or(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_perform/1}).
-dialyzer({nowarn_function, yeccpars2_220_perform/1}).
-compile({nowarn_unused_function,  yeccpars2_220_perform/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_perform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_pipe/1}).
-dialyzer({nowarn_function, yeccpars2_220_pipe/1}).
-compile({nowarn_unused_function,  yeccpars2_220_pipe/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_pipe(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_pipe_right/1}).
-dialyzer({nowarn_function, yeccpars2_220_pipe_right/1}).
-compile({nowarn_unused_function,  yeccpars2_220_pipe_right/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_pipe_right(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_property/1}).
-dialyzer({nowarn_function, yeccpars2_220_property/1}).
-compile({nowarn_unused_function,  yeccpars2_220_property/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_property(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_220_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_220_rbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_rbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_rbracket/1}).
-dialyzer({nowarn_function, yeccpars2_220_rbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_220_rbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_rbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_rparen/1}).
-dialyzer({nowarn_function, yeccpars2_220_rparen/1}).
-compile({nowarn_unused_function,  yeccpars2_220_rparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_rparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_semicolon/1}).
-dialyzer({nowarn_function, yeccpars2_220_semicolon/1}).
-compile({nowarn_unused_function,  yeccpars2_220_semicolon/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_semicolon(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_setoid_eq/1}).
-dialyzer({nowarn_function, yeccpars2_220_setoid_eq/1}).
-compile({nowarn_unused_function,  yeccpars2_220_setoid_eq/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_setoid_eq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_setoid_neq/1}).
-dialyzer({nowarn_function, yeccpars2_220_setoid_neq/1}).
-compile({nowarn_unused_function,  yeccpars2_220_setoid_neq/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_setoid_neq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_string/1}).
-dialyzer({nowarn_function, yeccpars2_220_string/1}).
-compile({nowarn_unused_function,  yeccpars2_220_string/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_string(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_test/1}).
-dialyzer({nowarn_function, yeccpars2_220_test/1}).
-compile({nowarn_unused_function,  yeccpars2_220_test/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_test(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_then/1}).
-dialyzer({nowarn_function, yeccpars2_220_then/1}).
-compile({nowarn_unused_function,  yeccpars2_220_then/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_then(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_trait/1}).
-dialyzer({nowarn_function, yeccpars2_220_trait/1}).
-compile({nowarn_unused_function,  yeccpars2_220_trait/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_trait(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_transform/1}).
-dialyzer({nowarn_function, yeccpars2_220_transform/1}).
-compile({nowarn_unused_function,  yeccpars2_220_transform/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_transform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_type/1}).
-dialyzer({nowarn_function, yeccpars2_220_type/1}).
-compile({nowarn_unused_function,  yeccpars2_220_type/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_type(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_220_upper_ident/1}).
-dialyzer({nowarn_function, yeccpars2_220_upper_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_220_upper_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 849).
yeccpars2_220_upper_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_221_/1}).
-dialyzer({nowarn_function, yeccpars2_221_/1}).
-compile({nowarn_unused_function,  yeccpars2_221_/1}).
-file("src/compiler/parser/catena_parser.yrl", 877).
yeccpars2_221_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                        
    {binary_op, fmap, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,'yeccpars2_222_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_222_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_222_$end'/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
'yeccpars2_222_$end'(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_and/1}).
-dialyzer({nowarn_function, yeccpars2_222_and/1}).
-compile({nowarn_unused_function,  yeccpars2_222_and/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_and(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_ap/1}).
-dialyzer({nowarn_function, yeccpars2_222_ap/1}).
-compile({nowarn_unused_function,  yeccpars2_222_ap/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_ap(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_222_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_222_arrow/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_arrow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_bind/1}).
-dialyzer({nowarn_function, yeccpars2_222_bind/1}).
-compile({nowarn_unused_function,  yeccpars2_222_bind/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_bind(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_comma/1}).
-dialyzer({nowarn_function, yeccpars2_222_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_222_comma/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_comma(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_cons/1}).
-dialyzer({nowarn_function, yeccpars2_222_cons/1}).
-compile({nowarn_unused_function,  yeccpars2_222_cons/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_cons(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_do/1}).
-dialyzer({nowarn_function, yeccpars2_222_do/1}).
-compile({nowarn_unused_function,  yeccpars2_222_do/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_do(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_dot/1}).
-dialyzer({nowarn_function, yeccpars2_222_dot/1}).
-compile({nowarn_unused_function,  yeccpars2_222_dot/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_dot(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_effect/1}).
-dialyzer({nowarn_function, yeccpars2_222_effect/1}).
-compile({nowarn_unused_function,  yeccpars2_222_effect/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_effect(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_end/1}).
-dialyzer({nowarn_function, yeccpars2_222_end/1}).
-compile({nowarn_unused_function,  yeccpars2_222_end/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_end(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_equals/1}).
-dialyzer({nowarn_function, yeccpars2_222_equals/1}).
-compile({nowarn_unused_function,  yeccpars2_222_equals/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_equals(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_error/1}).
-dialyzer({nowarn_function, yeccpars2_222_error/1}).
-compile({nowarn_unused_function,  yeccpars2_222_error/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_error(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_float/1}).
-dialyzer({nowarn_function, yeccpars2_222_float/1}).
-compile({nowarn_unused_function,  yeccpars2_222_float/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_float(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_fmap/1}).
-dialyzer({nowarn_function, yeccpars2_222_fmap/1}).
-compile({nowarn_unused_function,  yeccpars2_222_fmap/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_fmap(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_fn/1}).
-dialyzer({nowarn_function, yeccpars2_222_fn/1}).
-compile({nowarn_unused_function,  yeccpars2_222_fn/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_fn(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_handle/1}).
-dialyzer({nowarn_function, yeccpars2_222_handle/1}).
-compile({nowarn_unused_function,  yeccpars2_222_handle/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_handle(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_import/1}).
-dialyzer({nowarn_function, yeccpars2_222_import/1}).
-compile({nowarn_unused_function,  yeccpars2_222_import/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_import(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_in/1}).
-dialyzer({nowarn_function, yeccpars2_222_in/1}).
-compile({nowarn_unused_function,  yeccpars2_222_in/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_in(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_instance/1}).
-dialyzer({nowarn_function, yeccpars2_222_instance/1}).
-compile({nowarn_unused_function,  yeccpars2_222_instance/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_instance(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_integer/1}).
-dialyzer({nowarn_function, yeccpars2_222_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_222_integer/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_integer(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_kleisli/1}).
-dialyzer({nowarn_function, yeccpars2_222_kleisli/1}).
-compile({nowarn_unused_function,  yeccpars2_222_kleisli/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_kleisli(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_lbrace/1}).
-dialyzer({nowarn_function, yeccpars2_222_lbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_222_lbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_lbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_lbracket/1}).
-dialyzer({nowarn_function, yeccpars2_222_lbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_222_lbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_lbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_let/1}).
-dialyzer({nowarn_function, yeccpars2_222_let/1}).
-compile({nowarn_unused_function,  yeccpars2_222_let/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_let(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_lower_ident/1}).
-dialyzer({nowarn_function, yeccpars2_222_lower_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_222_lower_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_lower_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_lparen/1}).
-dialyzer({nowarn_function, yeccpars2_222_lparen/1}).
-compile({nowarn_unused_function,  yeccpars2_222_lparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_lparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_mappend/1}).
-dialyzer({nowarn_function, yeccpars2_222_mappend/1}).
-compile({nowarn_unused_function,  yeccpars2_222_mappend/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_mappend(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_match/1}).
-dialyzer({nowarn_function, yeccpars2_222_match/1}).
-compile({nowarn_unused_function,  yeccpars2_222_match/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_match(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_of/1}).
-dialyzer({nowarn_function, yeccpars2_222_of/1}).
-compile({nowarn_unused_function,  yeccpars2_222_of/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_of(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_or/1}).
-dialyzer({nowarn_function, yeccpars2_222_or/1}).
-compile({nowarn_unused_function,  yeccpars2_222_or/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_or(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_perform/1}).
-dialyzer({nowarn_function, yeccpars2_222_perform/1}).
-compile({nowarn_unused_function,  yeccpars2_222_perform/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_perform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_pipe/1}).
-dialyzer({nowarn_function, yeccpars2_222_pipe/1}).
-compile({nowarn_unused_function,  yeccpars2_222_pipe/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_pipe(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_pipe_right/1}).
-dialyzer({nowarn_function, yeccpars2_222_pipe_right/1}).
-compile({nowarn_unused_function,  yeccpars2_222_pipe_right/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_pipe_right(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_property/1}).
-dialyzer({nowarn_function, yeccpars2_222_property/1}).
-compile({nowarn_unused_function,  yeccpars2_222_property/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_property(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_222_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_222_rbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_rbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_rbracket/1}).
-dialyzer({nowarn_function, yeccpars2_222_rbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_222_rbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_rbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_rparen/1}).
-dialyzer({nowarn_function, yeccpars2_222_rparen/1}).
-compile({nowarn_unused_function,  yeccpars2_222_rparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_rparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_semicolon/1}).
-dialyzer({nowarn_function, yeccpars2_222_semicolon/1}).
-compile({nowarn_unused_function,  yeccpars2_222_semicolon/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_semicolon(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_string/1}).
-dialyzer({nowarn_function, yeccpars2_222_string/1}).
-compile({nowarn_unused_function,  yeccpars2_222_string/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_string(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_test/1}).
-dialyzer({nowarn_function, yeccpars2_222_test/1}).
-compile({nowarn_unused_function,  yeccpars2_222_test/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_test(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_then/1}).
-dialyzer({nowarn_function, yeccpars2_222_then/1}).
-compile({nowarn_unused_function,  yeccpars2_222_then/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_then(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_trait/1}).
-dialyzer({nowarn_function, yeccpars2_222_trait/1}).
-compile({nowarn_unused_function,  yeccpars2_222_trait/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_trait(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_transform/1}).
-dialyzer({nowarn_function, yeccpars2_222_transform/1}).
-compile({nowarn_unused_function,  yeccpars2_222_transform/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_transform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_type/1}).
-dialyzer({nowarn_function, yeccpars2_222_type/1}).
-compile({nowarn_unused_function,  yeccpars2_222_type/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_type(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_222_upper_ident/1}).
-dialyzer({nowarn_function, yeccpars2_222_upper_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_222_upper_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 834).
yeccpars2_222_upper_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_223_/1}).
-dialyzer({nowarn_function, yeccpars2_223_/1}).
-compile({nowarn_unused_function,  yeccpars2_223_/1}).
-file("src/compiler/parser/catena_parser.yrl", 870).
yeccpars2_223_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                        
    {cons_expr, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_224_/1}).
-dialyzer({nowarn_function, yeccpars2_224_/1}).
-compile({nowarn_unused_function,  yeccpars2_224_/1}).
-file("src/compiler/parser/catena_parser.yrl", 972).
yeccpars2_224_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                   
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_225_/1}).
-dialyzer({nowarn_function, yeccpars2_225_/1}).
-compile({nowarn_unused_function,  yeccpars2_225_/1}).
-file("src/compiler/parser/catena_parser.yrl", 874).
yeccpars2_225_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                        
    {binary_op, bind, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_226_/1}).
-dialyzer({nowarn_function, yeccpars2_226_/1}).
-compile({nowarn_unused_function,  yeccpars2_226_/1}).
-file("src/compiler/parser/catena_parser.yrl", 880).
yeccpars2_226_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, ap, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_227_/1}).
-dialyzer({nowarn_function, yeccpars2_227_/1}).
-compile({nowarn_unused_function,  yeccpars2_227_/1}).
-file("src/compiler/parser/catena_parser.yrl", 859).
yeccpars2_227_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                         
    {binary_op, 'and', ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_228_/1}).
-dialyzer({nowarn_function, yeccpars2_228_/1}).
-compile({nowarn_unused_function,  yeccpars2_228_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1031).
yeccpars2_228_(__Stack0) ->
 [___7,___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                                 
    {perform_expr,
        extract_atom(___2),
        extract_atom(___4),
        ___6,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_230_/1}).
-dialyzer({nowarn_function, yeccpars2_230_/1}).
-compile({nowarn_unused_function,  yeccpars2_230_/1}).
-file("src/compiler/parser/catena_parser.yrl", 656).
yeccpars2_230_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_233_/1}).
-dialyzer({nowarn_function, yeccpars2_233_/1}).
-compile({nowarn_unused_function,  yeccpars2_233_/1}).
-file("src/compiler/parser/catena_parser.yrl", 678).
yeccpars2_233_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                       
    ___1
  end | __Stack].

-compile({inline,yeccpars2_239_/1}).
-dialyzer({nowarn_function, yeccpars2_239_/1}).
-compile({nowarn_unused_function,  yeccpars2_239_/1}).
-file("src/compiler/parser/catena_parser.yrl", 668).
yeccpars2_239_(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                          
    {match_clause,
        ___2,
        ___4,
        ___6,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_240_/1}).
-dialyzer({nowarn_function, yeccpars2_240_/1}).
-compile({nowarn_unused_function,  yeccpars2_240_/1}).
-file("src/compiler/parser/catena_parser.yrl", 661).
yeccpars2_240_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                            
    {match_clause,
        ___2,
        undefined,
        ___4,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_242_/1}).
-dialyzer({nowarn_function, yeccpars2_242_/1}).
-compile({nowarn_unused_function,  yeccpars2_242_/1}).
-file("src/compiler/parser/catena_parser.yrl", 680).
yeccpars2_242_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                       
    case ___3 of
        {pat_or, Patterns, _Loc} ->
            {pat_or, [___1 | Patterns], extract_location(___2)};
        SinglePattern ->
            {pat_or, [___1, SinglePattern], extract_location(___2)}
    end
  end | __Stack].

-compile({inline,yeccpars2_245_/1}).
-dialyzer({nowarn_function, yeccpars2_245_/1}).
-compile({nowarn_unused_function,  yeccpars2_245_/1}).
-file("src/compiler/parser/catena_parser.yrl", 945).
yeccpars2_245_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                     
    {match_expr, ___2, ___4, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_246_/1}).
-dialyzer({nowarn_function, yeccpars2_246_/1}).
-compile({nowarn_unused_function,  yeccpars2_246_/1}).
-file("src/compiler/parser/catena_parser.yrl", 658).
yeccpars2_246_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    [___1 | ___2]
  end | __Stack].

-compile({inline,yeccpars2_247_/1}).
-dialyzer({nowarn_function, yeccpars2_247_/1}).
-compile({nowarn_unused_function,  yeccpars2_247_/1}).
-file("src/compiler/parser/catena_parser.yrl", 941).
yeccpars2_247_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                           
    {match_expr, undefined, ___2, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_251_/1}).
-dialyzer({nowarn_function, yeccpars2_251_/1}).
-compile({nowarn_unused_function,  yeccpars2_251_/1}).
-file("src/compiler/parser/catena_parser.yrl", 909).
yeccpars2_251_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                    
    ___2
  end | __Stack].

-compile({inline,yeccpars2_252_/1}).
-dialyzer({nowarn_function, yeccpars2_252_/1}).
-compile({nowarn_unused_function,  yeccpars2_252_/1}).
-file("src/compiler/parser/catena_parser.yrl", 918).
yeccpars2_252_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                               
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_253_/1}).
-dialyzer({nowarn_function, yeccpars2_253_/1}).
-compile({nowarn_unused_function,  yeccpars2_253_/1}).
-file("src/compiler/parser/catena_parser.yrl", 916).
yeccpars2_253_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                    
    [___1, ___3]
  end | __Stack].

-compile({inline,yeccpars2_254_/1}).
-dialyzer({nowarn_function, yeccpars2_254_/1}).
-compile({nowarn_unused_function,  yeccpars2_254_/1}).
-file("src/compiler/parser/catena_parser.yrl", 912).
yeccpars2_254_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                               
    {tuple_expr, ___2, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_259_/1}).
-dialyzer({nowarn_function, yeccpars2_259_/1}).
-compile({nowarn_unused_function,  yeccpars2_259_/1}).
-file("src/compiler/parser/catena_parser.yrl", 921).
yeccpars2_259_(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                         
    {let_expr,
        [{pat_var, extract_atom(___2), extract_location(___2)}, ___4],
        ___6,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_261_/1}).
-dialyzer({nowarn_function, yeccpars2_261_/1}).
-compile({nowarn_unused_function,  yeccpars2_261_/1}).
-file("src/compiler/parser/catena_parser.yrl", 948).
yeccpars2_261_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                   
    {list_expr, [], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_262_/1}).
-dialyzer({nowarn_function, yeccpars2_262_/1}).
-compile({nowarn_unused_function,  yeccpars2_262_/1}).
-file("src/compiler/parser/catena_parser.yrl", 951).
yeccpars2_262_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    {list_expr, ___2, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_264_/1}).
-dialyzer({nowarn_function, yeccpars2_264_/1}).
-compile({nowarn_unused_function,  yeccpars2_264_/1}).
-file("src/compiler/parser/catena_parser.yrl", 961).
yeccpars2_264_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_266_/1}).
-dialyzer({nowarn_function, yeccpars2_266_/1}).
-compile({nowarn_unused_function,  yeccpars2_266_/1}).
-file("src/compiler/parser/catena_parser.yrl", 954).
yeccpars2_266_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                               
    {record_expr, [], undefined, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_268_/1}).
-dialyzer({nowarn_function, yeccpars2_268_/1}).
-compile({nowarn_unused_function,  yeccpars2_268_/1}).
-file("src/compiler/parser/catena_parser.yrl", 966).
yeccpars2_268_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                        
    {extract_atom(___1), ___3}
  end | __Stack].

-compile({inline,yeccpars2_270_/1}).
-dialyzer({nowarn_function, yeccpars2_270_/1}).
-compile({nowarn_unused_function,  yeccpars2_270_/1}).
-file("src/compiler/parser/catena_parser.yrl", 963).
yeccpars2_270_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_271_/1}).
-dialyzer({nowarn_function, yeccpars2_271_/1}).
-compile({nowarn_unused_function,  yeccpars2_271_/1}).
-file("src/compiler/parser/catena_parser.yrl", 957).
yeccpars2_271_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    {record_expr, ___2, undefined, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_276_/1}).
-dialyzer({nowarn_function, yeccpars2_276_/1}).
-compile({nowarn_unused_function,  yeccpars2_276_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1047).
yeccpars2_276_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                   
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_280_/1}).
-dialyzer({nowarn_function, yeccpars2_280_/1}).
-compile({nowarn_unused_function,  yeccpars2_280_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1062).
yeccpars2_280_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                   
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_283_/1}).
-dialyzer({nowarn_function, yeccpars2_283_/1}).
-compile({nowarn_unused_function,  yeccpars2_283_/1}).
-file("src/compiler/parser/catena_parser.yrl", 703).
yeccpars2_283_(__Stack0) ->
 [begin
                                
    []
  end | __Stack0].

-compile({inline,yeccpars2_285_/1}).
-dialyzer({nowarn_function, yeccpars2_285_/1}).
-compile({nowarn_unused_function,  yeccpars2_285_/1}).
-file("src/compiler/parser/catena_parser.yrl", 705).
yeccpars2_285_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_286_/1}).
-dialyzer({nowarn_function, yeccpars2_286_/1}).
-compile({nowarn_unused_function,  yeccpars2_286_/1}).
-file("src/compiler/parser/catena_parser.yrl", 703).
yeccpars2_286_(__Stack0) ->
 [begin
                                
    []
  end | __Stack0].

-compile({inline,yeccpars2_287_/1}).
-dialyzer({nowarn_function, yeccpars2_287_/1}).
-compile({nowarn_unused_function,  yeccpars2_287_/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_287_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                        
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_290_/1}).
-dialyzer({nowarn_function, yeccpars2_290_/1}).
-compile({nowarn_unused_function,  yeccpars2_290_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1065).
yeccpars2_290_(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                                   
    {operation_case,
        extract_atom(___1),
        ___3,
        ___6,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_291_/1}).
-dialyzer({nowarn_function, yeccpars2_291_/1}).
-compile({nowarn_unused_function,  yeccpars2_291_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1072).
yeccpars2_291_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                  
    {operation_case,
        extract_atom(___1),
        [],
        ___3,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_292_/1}).
-dialyzer({nowarn_function, yeccpars2_292_/1}).
-compile({nowarn_unused_function,  yeccpars2_292_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1060).
yeccpars2_292_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    ___1 ++ [___2]
  end | __Stack].

-compile({inline,yeccpars2_293_/1}).
-dialyzer({nowarn_function, yeccpars2_293_/1}).
-compile({nowarn_unused_function,  yeccpars2_293_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1052).
yeccpars2_293_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             
    {handler_clause,
        extract_atom(___1),
        ___3,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_294_/1}).
-dialyzer({nowarn_function, yeccpars2_294_/1}).
-compile({nowarn_unused_function,  yeccpars2_294_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1049).
yeccpars2_294_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    [___1 | ___2]
  end | __Stack].

-compile({inline,yeccpars2_295_/1}).
-dialyzer({nowarn_function, yeccpars2_295_/1}).
-compile({nowarn_unused_function,  yeccpars2_295_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1040).
yeccpars2_295_(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                 
    {handle_expr,
        ___2,
        ___5,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_298_/1}).
-dialyzer({nowarn_function, yeccpars2_298_/1}).
-compile({nowarn_unused_function,  yeccpars2_298_/1}).
-file("src/compiler/parser/catena_parser.yrl", 928).
yeccpars2_298_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                           
    {lambda, [{pat_var, extract_atom(___2), extract_location(___2)}], ___4, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_300_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_300_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_300_rbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 1003).
yeccpars2_300_rbrace(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                       
    [{do_return, ___1, extract_location(___1)}]
  end | __Stack].

-compile({inline,yeccpars2_300_/1}).
-dialyzer({nowarn_function, yeccpars2_300_/1}).
-compile({nowarn_unused_function,  yeccpars2_300_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1022).
yeccpars2_300_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                      
    {do_action, ___1, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_304_/1}).
-dialyzer({nowarn_function, yeccpars2_304_/1}).
-compile({nowarn_unused_function,  yeccpars2_304_/1}).
-file("src/compiler/parser/catena_parser.yrl", 903).
yeccpars2_304_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             
    {var, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_306_/1}).
-dialyzer({nowarn_function, yeccpars2_306_/1}).
-compile({nowarn_unused_function,  yeccpars2_306_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1014).
yeccpars2_306_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    {do_bind, extract_atom(___1), ___3, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_309_/1}).
-dialyzer({nowarn_function, yeccpars2_309_/1}).
-compile({nowarn_unused_function,  yeccpars2_309_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1018).
yeccpars2_309_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                               
    {do_let, extract_atom(___2), ___4, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_311_/1}).
-dialyzer({nowarn_function, yeccpars2_311_/1}).
-compile({nowarn_unused_function,  yeccpars2_311_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1005).
yeccpars2_311_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                       
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_312_/1}).
-dialyzer({nowarn_function, yeccpars2_312_/1}).
-compile({nowarn_unused_function,  yeccpars2_312_/1}).
-file("src/compiler/parser/catena_parser.yrl", 999).
yeccpars2_312_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    {do_expr, ___3, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_313_/1}).
-dialyzer({nowarn_function, yeccpars2_313_/1}).
-compile({nowarn_unused_function,  yeccpars2_313_/1}).
-file("src/compiler/parser/catena_parser.yrl", 892).
yeccpars2_313_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                   
    {app, ___1, [___2], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_315_/1}).
-dialyzer({nowarn_function, yeccpars2_315_/1}).
-compile({nowarn_unused_function,  yeccpars2_315_/1}).
-file("src/compiler/parser/catena_parser.yrl", 895).
yeccpars2_315_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                      
    {record_access, ___1, extract_atom(___3), extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_317_/1}).
-dialyzer({nowarn_function, yeccpars2_317_/1}).
-compile({nowarn_unused_function,  yeccpars2_317_/1}).
-file("src/compiler/parser/catena_parser.yrl", 808).
yeccpars2_317_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_319_/1}).
-dialyzer({nowarn_function, yeccpars2_319_/1}).
-compile({nowarn_unused_function,  yeccpars2_319_/1}).
-file("src/compiler/parser/catena_parser.yrl", 613).
yeccpars2_319_(__Stack0) ->
 [___7,___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                                
    {transform_decl,
        extract_atom(___2),
        undefined,
        [{transform_clause, ___3, ___5, ___7, extract_location(___1)}],
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_320_/1}).
-dialyzer({nowarn_function, yeccpars2_320_/1}).
-compile({nowarn_unused_function,  yeccpars2_320_/1}).
-file("src/compiler/parser/catena_parser.yrl", 606).
yeccpars2_320_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                  
    {transform_decl,
        extract_atom(___2),
        undefined,
        [{transform_clause, ___3, undefined, ___5, extract_location(___1)}],
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_322_/1}).
-dialyzer({nowarn_function, yeccpars2_322_/1}).
-compile({nowarn_unused_function,  yeccpars2_322_/1}).
-file("src/compiler/parser/catena_parser.yrl", 621).
yeccpars2_322_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                               
    make_error_declaration(extract_location(___1), "Invalid transform name", ___2)
  end | __Stack].

-compile({inline,yeccpars2_323_/1}).
-dialyzer({nowarn_function, yeccpars2_323_/1}).
-compile({nowarn_unused_function,  yeccpars2_323_/1}).
-file("src/compiler/parser/catena_parser.yrl", 428).
yeccpars2_323_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                           
    make_error_declaration(extract_location(___1), "Incomplete trait declaration", ___2)
  end | __Stack].

-compile({inline,yeccpars2_324_/1}).
-dialyzer({nowarn_function, yeccpars2_324_/1}).
-compile({nowarn_unused_function,  yeccpars2_324_/1}).
-file("src/compiler/parser/catena_parser.yrl", 350).
yeccpars2_324_(__Stack0) ->
 [begin
                         
    []
  end | __Stack0].

-compile({inline,yeccpars2_325_/1}).
-dialyzer({nowarn_function, yeccpars2_325_/1}).
-compile({nowarn_unused_function,  yeccpars2_325_/1}).
-file("src/compiler/parser/catena_parser.yrl", 433).
yeccpars2_325_(__Stack0) ->
 [begin
                                  undefined
  end | __Stack0].

-compile({inline,yeccpars2_328_/1}).
-dialyzer({nowarn_function, yeccpars2_328_/1}).
-compile({nowarn_unused_function,  yeccpars2_328_/1}).
-file("src/compiler/parser/catena_parser.yrl", 432).
yeccpars2_328_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                   ___2
  end | __Stack].

-compile({inline,yeccpars2_329_/1}).
-dialyzer({nowarn_function, yeccpars2_329_/1}).
-compile({nowarn_unused_function,  yeccpars2_329_/1}).
-file("src/compiler/parser/catena_parser.yrl", 436).
yeccpars2_329_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                        
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_331_/1}).
-dialyzer({nowarn_function, yeccpars2_331_/1}).
-compile({nowarn_unused_function,  yeccpars2_331_/1}).
-file("src/compiler/parser/catena_parser.yrl", 438).
yeccpars2_331_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                 
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_334_/1}).
-dialyzer({nowarn_function, yeccpars2_334_/1}).
-compile({nowarn_unused_function,  yeccpars2_334_/1}).
-file("src/compiler/parser/catena_parser.yrl", 448).
yeccpars2_334_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_338_/1}).
-dialyzer({nowarn_function, yeccpars2_338_/1}).
-compile({nowarn_unused_function,  yeccpars2_338_/1}).
-file("src/compiler/parser/catena_parser.yrl", 458).
yeccpars2_338_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    {trait_sig, extract_atom(___1), ___3, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_339_/1}).
-dialyzer({nowarn_function, yeccpars2_339_/1}).
-compile({nowarn_unused_function,  yeccpars2_339_/1}).
-file("src/compiler/parser/catena_parser.yrl", 466).
yeccpars2_339_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                         
    make_error_declaration(extract_location(___2),
        "Invalid method signature. " ++
        "Common issues and solutions:\n" ++
        "  • Cannot use simple tuples as parameters: '(a, b) -> ...'\n" ++
        "  • Try: 'Pair a b -> ...' or '((a -> b), c) -> ...'\n" ++
        "  • See trait signatures documentation for examples", ___3)
  end | __Stack].

-compile({inline,yeccpars2_341_/1}).
-dialyzer({nowarn_function, yeccpars2_341_/1}).
-compile({nowarn_unused_function,  yeccpars2_341_/1}).
-file("src/compiler/parser/catena_parser.yrl", 462).
yeccpars2_341_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                               
    {trait_default, extract_atom(___1), ___2, ___4, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_342_/1}).
-dialyzer({nowarn_function, yeccpars2_342_/1}).
-compile({nowarn_unused_function,  yeccpars2_342_/1}).
-file("src/compiler/parser/catena_parser.yrl", 452).
yeccpars2_342_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                     
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_343_/1}).
-dialyzer({nowarn_function, yeccpars2_343_/1}).
-compile({nowarn_unused_function,  yeccpars2_343_/1}).
-file("src/compiler/parser/catena_parser.yrl", 450).
yeccpars2_343_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_344_/1}).
-dialyzer({nowarn_function, yeccpars2_344_/1}).
-compile({nowarn_unused_function,  yeccpars2_344_/1}).
-file("src/compiler/parser/catena_parser.yrl", 419).
yeccpars2_344_(__Stack0) ->
 [___7,___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                                           
    {trait_decl,
        extract_atom(___2),
        ___3,
        ___4,
        ___6,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_345_/1}).
-dialyzer({nowarn_function, yeccpars2_345_/1}).
-compile({nowarn_unused_function,  yeccpars2_345_/1}).
-file("src/compiler/parser/catena_parser.yrl", 554).
yeccpars2_345_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                         
    make_error_declaration(extract_location(___1), "Incomplete test declaration", ___2)
  end | __Stack].

-compile({inline,yeccpars2_348_/1}).
-dialyzer({nowarn_function, yeccpars2_348_/1}).
-compile({nowarn_unused_function,  yeccpars2_348_/1}).
-file("src/compiler/parser/catena_parser.yrl", 547).
yeccpars2_348_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                      
    {test_decl,
        extract_value(___2),
        ___4,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_349_/1}).
-dialyzer({nowarn_function, yeccpars2_349_/1}).
-compile({nowarn_unused_function,  yeccpars2_349_/1}).
-file("src/compiler/parser/catena_parser.yrl", 583).
yeccpars2_349_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                 
    make_error_declaration(extract_location(___1), "Incomplete property declaration", ___2)
  end | __Stack].

-compile({inline,yeccpars2_352_/1}).
-dialyzer({nowarn_function, yeccpars2_352_/1}).
-compile({nowarn_unused_function,  yeccpars2_352_/1}).
-file("src/compiler/parser/catena_parser.yrl", 562).
yeccpars2_352_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                       
    {property_decl,
        extract_value(___2),
        ___4,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_355_/1}).
-dialyzer({nowarn_function, yeccpars2_355_/1}).
-compile({nowarn_unused_function,  yeccpars2_355_/1}).
-file("src/compiler/parser/catena_parser.yrl", 573).
yeccpars2_355_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                       
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_358_/1}).
-dialyzer({nowarn_function, yeccpars2_358_/1}).
-compile({nowarn_unused_function,  yeccpars2_358_/1}).
-file("src/compiler/parser/catena_parser.yrl", 579).
yeccpars2_358_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    {extract_atom(___1), extract_atom(___3)}
  end | __Stack].

-compile({inline,yeccpars2_360_/1}).
-dialyzer({nowarn_function, yeccpars2_360_/1}).
-compile({nowarn_unused_function,  yeccpars2_360_/1}).
-file("src/compiler/parser/catena_parser.yrl", 575).
yeccpars2_360_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                               
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_362_/1}).
-dialyzer({nowarn_function, yeccpars2_362_/1}).
-compile({nowarn_unused_function,  yeccpars2_362_/1}).
-file("src/compiler/parser/catena_parser.yrl", 569).
yeccpars2_362_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                    
    {property_forall, ___2, ___4, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_363_/1}).
-dialyzer({nowarn_function, yeccpars2_363_/1}).
-compile({nowarn_unused_function,  yeccpars2_363_/1}).
-file("src/compiler/parser/catena_parser.yrl", 227).
yeccpars2_363_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                   
    {module_decl, extract_atom(___2), [], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_365_/1}).
-dialyzer({nowarn_function, yeccpars2_365_/1}).
-compile({nowarn_unused_function,  yeccpars2_365_/1}).
-file("src/compiler/parser/catena_parser.yrl", 231).
yeccpars2_365_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    {module_decl,
        list_to_atom(atom_to_list(extract_atom(___2)) ++ "." ++ atom_to_list(extract_atom(___4))),
        [],
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_366_/1}).
-dialyzer({nowarn_function, yeccpars2_366_/1}).
-compile({nowarn_unused_function,  yeccpars2_366_/1}).
-file("src/compiler/parser/catena_parser.yrl", 523).
yeccpars2_366_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                          
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_368_/1}).
-dialyzer({nowarn_function, yeccpars2_368_/1}).
-compile({nowarn_unused_function,  yeccpars2_368_/1}).
-file("src/compiler/parser/catena_parser.yrl", 515).
yeccpars2_368_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                 
    make_error_declaration(extract_location(___1), "Incomplete instance declaration", ___2)
  end | __Stack].

-compile({inline,yeccpars2_369_/1}).
-dialyzer({nowarn_function, yeccpars2_369_/1}).
-compile({nowarn_unused_function,  yeccpars2_369_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1155).
yeccpars2_369_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                  
    {type_con, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_370_comma/1}).
-dialyzer({nowarn_function, yeccpars2_370_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_370_comma/1}).
-file("src/compiler/parser/catena_parser.yrl", 1146).
yeccpars2_370_comma(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                             
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_370_double_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_370_double_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_370_double_arrow/1}).
-file("src/compiler/parser/catena_parser.yrl", 1146).
yeccpars2_370_double_arrow(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                             
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_370_/1}).
-dialyzer({nowarn_function, yeccpars2_370_/1}).
-compile({nowarn_unused_function,  yeccpars2_370_/1}).
-file("src/compiler/parser/catena_parser.yrl", 519).
yeccpars2_370_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                          [___1]
  end | __Stack].

-compile({inline,yeccpars2_374_/1}).
-dialyzer({nowarn_function, yeccpars2_374_/1}).
-compile({nowarn_unused_function,  yeccpars2_374_/1}).
-file("src/compiler/parser/catena_parser.yrl", 530).
yeccpars2_374_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                     
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_375_/1}).
-dialyzer({nowarn_function, yeccpars2_375_/1}).
-compile({nowarn_unused_function,  yeccpars2_375_/1}).
-file("src/compiler/parser/catena_parser.yrl", 497).
yeccpars2_375_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                      
    {instance_decl,
        extract_atom(___2),
        ___3,
        undefined,
        [],
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_377_/1}).
-dialyzer({nowarn_function, yeccpars2_377_/1}).
-compile({nowarn_unused_function,  yeccpars2_377_/1}).
-file("src/compiler/parser/catena_parser.yrl", 692).
yeccpars2_377_(__Stack0) ->
 [begin
                          
    []
  end | __Stack0].

-compile({inline,yeccpars2_380_/1}).
-dialyzer({nowarn_function, yeccpars2_380_/1}).
-compile({nowarn_unused_function,  yeccpars2_380_/1}).
-file("src/compiler/parser/catena_parser.yrl", 538).
yeccpars2_380_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                   
    {extract_atom(___2), {lambda, ___3, ___5, extract_location(___1)}}
  end | __Stack].

-compile({inline,yeccpars2_381_/1}).
-dialyzer({nowarn_function, yeccpars2_381_/1}).
-compile({nowarn_unused_function,  yeccpars2_381_/1}).
-file("src/compiler/parser/catena_parser.yrl", 534).
yeccpars2_381_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                           
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_382_/1}).
-dialyzer({nowarn_function, yeccpars2_382_/1}).
-compile({nowarn_unused_function,  yeccpars2_382_/1}).
-file("src/compiler/parser/catena_parser.yrl", 532).
yeccpars2_382_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                            
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_383_/1}).
-dialyzer({nowarn_function, yeccpars2_383_/1}).
-compile({nowarn_unused_function,  yeccpars2_383_/1}).
-file("src/compiler/parser/catena_parser.yrl", 488).
yeccpars2_383_(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                                       
    {instance_decl,
        extract_atom(___2),
        ___3,
        undefined,
        ___5,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_384_/1}).
-dialyzer({nowarn_function, yeccpars2_384_/1}).
-compile({nowarn_unused_function,  yeccpars2_384_/1}).
-file("src/compiler/parser/catena_parser.yrl", 520).
yeccpars2_384_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                             [___1 | ___2]
  end | __Stack].

-compile({inline,yeccpars2_387_/1}).
-dialyzer({nowarn_function, yeccpars2_387_/1}).
-compile({nowarn_unused_function,  yeccpars2_387_/1}).
-file("src/compiler/parser/catena_parser.yrl", 519).
yeccpars2_387_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                          [___1]
  end | __Stack].

-compile({inline,yeccpars2_391_/1}).
-dialyzer({nowarn_function, yeccpars2_391_/1}).
-compile({nowarn_unused_function,  yeccpars2_391_/1}).
-file("src/compiler/parser/catena_parser.yrl", 506).
yeccpars2_391_(__Stack0) ->
 [___7,___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                                                        
    {instance_decl,
        extract_atom(___4),
        ___5,
        ___2,
        [],
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_392_/1}).
-dialyzer({nowarn_function, yeccpars2_392_/1}).
-compile({nowarn_unused_function,  yeccpars2_392_/1}).
-file("src/compiler/parser/catena_parser.yrl", 479).
yeccpars2_392_(__Stack0) ->
 [___8,___7,___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                                                                         
    {instance_decl,
        extract_atom(___4),
        ___5,
        ___2,
        ___7,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_394_/1}).
-dialyzer({nowarn_function, yeccpars2_394_/1}).
-compile({nowarn_unused_function,  yeccpars2_394_/1}).
-file("src/compiler/parser/catena_parser.yrl", 525).
yeccpars2_394_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                     
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_396_/1}).
-dialyzer({nowarn_function, yeccpars2_396_/1}).
-compile({nowarn_unused_function,  yeccpars2_396_/1}).
-file("src/compiler/parser/catena_parser.yrl", 275).
yeccpars2_396_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                   
    {import, extract_atom(___2), all, false, undefined, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_400_/1}).
-dialyzer({nowarn_function, yeccpars2_400_/1}).
-compile({nowarn_unused_function,  yeccpars2_400_/1}).
-file("src/compiler/parser/catena_parser.yrl", 315).
yeccpars2_400_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                              [extract_atom(___1)]
  end | __Stack].

-compile({inline,yeccpars2_402_/1}).
-dialyzer({nowarn_function, yeccpars2_402_/1}).
-compile({nowarn_unused_function,  yeccpars2_402_/1}).
-file("src/compiler/parser/catena_parser.yrl", 316).
yeccpars2_402_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                 [extract_atom(___1) | ___3]
  end | __Stack].

-compile({inline,yeccpars2_403_/1}).
-dialyzer({nowarn_function, yeccpars2_403_/1}).
-compile({nowarn_unused_function,  yeccpars2_403_/1}).
-file("src/compiler/parser/catena_parser.yrl", 295).
yeccpars2_403_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                              
    {import, extract_atom(___2), ___4, false, undefined, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_404_/1}).
-dialyzer({nowarn_function, yeccpars2_404_/1}).
-compile({nowarn_unused_function,  yeccpars2_404_/1}).
-file("src/compiler/parser/catena_parser.yrl", 279).
yeccpars2_404_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    ModuleName = list_to_atom(atom_to_list(extract_atom(___2)) ++ "." ++
                               atom_to_list(extract_atom(___4))),
    {import, ModuleName, all, false, undefined, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_407_/1}).
-dialyzer({nowarn_function, yeccpars2_407_/1}).
-compile({nowarn_unused_function,  yeccpars2_407_/1}).
-file("src/compiler/parser/catena_parser.yrl", 299).
yeccpars2_407_(__Stack0) ->
 [___7,___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                              
    ModuleName = list_to_atom(atom_to_list(extract_atom(___2)) ++ "." ++
                               atom_to_list(extract_atom(___4))),
    {import, ModuleName, ___6, false, undefined, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_413_/1}).
-dialyzer({nowarn_function, yeccpars2_413_/1}).
-compile({nowarn_unused_function,  yeccpars2_413_/1}).
-file("src/compiler/parser/catena_parser.yrl", 289).
yeccpars2_413_(__Stack0) ->
 [___7,___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                            
    ModuleName = list_to_atom(atom_to_list(extract_atom(___3)) ++ "." ++
                               atom_to_list(extract_atom(___5))),
    {import, ModuleName, all, true, extract_atom(___7), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_416_/1}).
-dialyzer({nowarn_function, yeccpars2_416_/1}).
-compile({nowarn_unused_function,  yeccpars2_416_/1}).
-file("src/compiler/parser/catena_parser.yrl", 309).
yeccpars2_416_(__Stack0) ->
 [___10,___9,___8,___7,___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                                                       
    ModuleName = list_to_atom(atom_to_list(extract_atom(___3)) ++ "." ++
                               atom_to_list(extract_atom(___5))),
    {import, ModuleName, ___9, true, extract_atom(___7), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_417_/1}).
-dialyzer({nowarn_function, yeccpars2_417_/1}).
-compile({nowarn_unused_function,  yeccpars2_417_/1}).
-file("src/compiler/parser/catena_parser.yrl", 285).
yeccpars2_417_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                            
    {import, extract_atom(___3), all, true, extract_atom(___5), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_420_/1}).
-dialyzer({nowarn_function, yeccpars2_420_/1}).
-compile({nowarn_unused_function,  yeccpars2_420_/1}).
-file("src/compiler/parser/catena_parser.yrl", 305).
yeccpars2_420_(__Stack0) ->
 [___8,___7,___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                                       
    {import, extract_atom(___3), ___7, true, extract_atom(___5), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_421_/1}).
-dialyzer({nowarn_function, yeccpars2_421_/1}).
-compile({nowarn_unused_function,  yeccpars2_421_/1}).
-file("src/compiler/parser/catena_parser.yrl", 323).
yeccpars2_421_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                             
    make_error_declaration(extract_location(___2), "Malformed declaration before 'effect'", ___1)
  end | __Stack].

-compile({inline,yeccpars2_422_/1}).
-dialyzer({nowarn_function, yeccpars2_422_/1}).
-compile({nowarn_unused_function,  yeccpars2_422_/1}).
-file("src/compiler/parser/catena_parser.yrl", 327).
yeccpars2_422_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                               
    make_error_declaration(extract_location(___2), "Malformed declaration before 'instance'", ___1)
  end | __Stack].

-compile({inline,yeccpars2_423_/1}).
-dialyzer({nowarn_function, yeccpars2_423_/1}).
-compile({nowarn_unused_function,  yeccpars2_423_/1}).
-file("src/compiler/parser/catena_parser.yrl", 325).
yeccpars2_423_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                            
    make_error_declaration(extract_location(___2), "Malformed declaration before 'trait'", ___1)
  end | __Stack].

-compile({inline,yeccpars2_424_/1}).
-dialyzer({nowarn_function, yeccpars2_424_/1}).
-compile({nowarn_unused_function,  yeccpars2_424_/1}).
-file("src/compiler/parser/catena_parser.yrl", 321).
yeccpars2_424_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                
    make_error_declaration(extract_location(___2), "Malformed declaration before 'transform'", ___1)
  end | __Stack].

-compile({inline,yeccpars2_425_/1}).
-dialyzer({nowarn_function, yeccpars2_425_/1}).
-compile({nowarn_unused_function,  yeccpars2_425_/1}).
-file("src/compiler/parser/catena_parser.yrl", 319).
yeccpars2_425_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                           
    make_error_declaration(extract_location(___2), "Malformed declaration before 'type'", ___1)
  end | __Stack].

-compile({inline,yeccpars2_426_/1}).
-dialyzer({nowarn_function, yeccpars2_426_/1}).
-compile({nowarn_unused_function,  yeccpars2_426_/1}).
-file("src/compiler/parser/catena_parser.yrl", 393).
yeccpars2_426_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                             
    make_error_declaration(extract_location(___1), "Incomplete effect declaration", ___2)
  end | __Stack].

-compile({inline,yeccpars2_427_/1}).
-dialyzer({nowarn_function, yeccpars2_427_/1}).
-compile({nowarn_unused_function,  yeccpars2_427_/1}).
-file("src/compiler/parser/catena_parser.yrl", 396).
yeccpars2_427_(__Stack0) ->
 [begin
                               
    []
  end | __Stack0].

-compile({inline,yeccpars2_429_/1}).
-dialyzer({nowarn_function, yeccpars2_429_/1}).
-compile({nowarn_unused_function,  yeccpars2_429_/1}).
-file("src/compiler/parser/catena_parser.yrl", 396).
yeccpars2_429_(__Stack0) ->
 [begin
                               
    []
  end | __Stack0].

-compile({inline,yeccpars2_431_/1}).
-dialyzer({nowarn_function, yeccpars2_431_/1}).
-compile({nowarn_unused_function,  yeccpars2_431_/1}).
-file("src/compiler/parser/catena_parser.yrl", 401).
yeccpars2_431_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                           
    {effect_operation,
        extract_atom(___2),
        undefined,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_433_/1}).
-dialyzer({nowarn_function, yeccpars2_433_/1}).
-compile({nowarn_unused_function,  yeccpars2_433_/1}).
-file("src/compiler/parser/catena_parser.yrl", 407).
yeccpars2_433_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                           
    {effect_operation,
        extract_atom(___2),
        ___4,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_434_/1}).
-dialyzer({nowarn_function, yeccpars2_434_/1}).
-compile({nowarn_unused_function,  yeccpars2_434_/1}).
-file("src/compiler/parser/catena_parser.yrl", 398).
yeccpars2_434_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                         
    [___1 | ___2]
  end | __Stack].

-compile({inline,yeccpars2_435_/1}).
-dialyzer({nowarn_function, yeccpars2_435_/1}).
-compile({nowarn_unused_function,  yeccpars2_435_/1}).
-file("src/compiler/parser/catena_parser.yrl", 386).
yeccpars2_435_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                           
    {effect_decl,
        extract_atom(___2),
        ___3,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_436_/1}).
-dialyzer({nowarn_function, yeccpars2_436_/1}).
-compile({nowarn_unused_function,  yeccpars2_436_/1}).
-file("src/compiler/parser/catena_parser.yrl", 258).
yeccpars2_436_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                          
    [___1 | ___2]
  end | __Stack].

-compile({inline,yeccpars2_437_/1}).
-dialyzer({nowarn_function, yeccpars2_437_/1}).
-compile({nowarn_unused_function,  yeccpars2_437_/1}).
-file("src/compiler/parser/catena_parser.yrl", 238).
yeccpars2_437_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                            
    ___1
  end | __Stack].

-compile({inline,yeccpars2_438_/1}).
-dialyzer({nowarn_function, yeccpars2_438_/1}).
-compile({nowarn_unused_function,  yeccpars2_438_/1}).
-file("src/compiler/parser/catena_parser.yrl", 241).
yeccpars2_438_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                            
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_439_/1}).
-dialyzer({nowarn_function, yeccpars2_439_/1}).
-compile({nowarn_unused_function,  yeccpars2_439_/1}).
-file("src/compiler/parser/catena_parser.yrl", 223).
yeccpars2_439_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                          
    {module_header, element(2, ___1), ___2, element(4, ___1)}
  end | __Stack].

-compile({inline,yeccpars2_445_/1}).
-dialyzer({nowarn_function, yeccpars2_445_/1}).
-compile({nowarn_unused_function,  yeccpars2_445_/1}).
-file("src/compiler/parser/catena_parser.yrl", 249).
yeccpars2_445_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                        
    {export_type, extract_atom(___3)}
  end | __Stack].

-compile({inline,yeccpars2_446_/1}).
-dialyzer({nowarn_function, yeccpars2_446_/1}).
-compile({nowarn_unused_function,  yeccpars2_446_/1}).
-file("src/compiler/parser/catena_parser.yrl", 251).
yeccpars2_446_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    {export_transform, extract_atom(___3)}
  end | __Stack].

-compile({inline,yeccpars2_447_/1}).
-dialyzer({nowarn_function, yeccpars2_447_/1}).
-compile({nowarn_unused_function,  yeccpars2_447_/1}).
-file("src/compiler/parser/catena_parser.yrl", 247).
yeccpars2_447_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                         
    {export_trait, extract_atom(___3)}
  end | __Stack].

-compile({inline,yeccpars2_448_/1}).
-dialyzer({nowarn_function, yeccpars2_448_/1}).
-compile({nowarn_unused_function,  yeccpars2_448_/1}).
-file("src/compiler/parser/catena_parser.yrl", 253).
yeccpars2_448_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                          
    {export_effect, extract_atom(___3)}
  end | __Stack].

-compile({inline,yeccpars2_449_/1}).
-dialyzer({nowarn_function, yeccpars2_449_/1}).
-compile({nowarn_unused_function,  yeccpars2_449_/1}).
-file("src/compiler/parser/catena_parser.yrl", 243).
yeccpars2_449_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                        
    [___1 | ___2]
  end | __Stack].

-compile({inline,yeccpars2_450_/1}).
-dialyzer({nowarn_function, yeccpars2_450_/1}).
-compile({nowarn_unused_function,  yeccpars2_450_/1}).
-file("src/compiler/parser/catena_parser.yrl", 209).
yeccpars2_450_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    {module,
        element(2, ___1),   %% Module name
        element(3, ___1),   %% Exports
        extract_imports(___2),  %% Extract imports from declarations
        filter_imports(___2),   %% Filter out imports from declarations
        element(4, ___1)}
  end | __Stack].

-compile({inline,yeccpars2_451_/1}).
-dialyzer({nowarn_function, yeccpars2_451_/1}).
-compile({nowarn_unused_function,  yeccpars2_451_/1}).
-file("src/compiler/parser/catena_parser.yrl", 590).
yeccpars2_451_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                         
    {transform_decl,
        extract_transform_name(___1),
        extract_transform_type(___1),
        ___2,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_452_/1}).
-dialyzer({nowarn_function, yeccpars2_452_/1}).
-compile({nowarn_unused_function,  yeccpars2_452_/1}).
-file("src/compiler/parser/catena_parser.yrl", 633).
yeccpars2_452_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                       
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_454_/1}).
-dialyzer({nowarn_function, yeccpars2_454_/1}).
-compile({nowarn_unused_function,  yeccpars2_454_/1}).
-file("src/compiler/parser/catena_parser.yrl", 692).
yeccpars2_454_(__Stack0) ->
 [begin
                          
    []
  end | __Stack0].

-compile({inline,yeccpars2_460_/1}).
-dialyzer({nowarn_function, yeccpars2_460_/1}).
-compile({nowarn_unused_function,  yeccpars2_460_/1}).
-file("src/compiler/parser/catena_parser.yrl", 645).
yeccpars2_460_(__Stack0) ->
 [___7,___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                                  
    {transform_clause,
        ___3,
        ___5,
        ___7,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_461_/1}).
-dialyzer({nowarn_function, yeccpars2_461_/1}).
-compile({nowarn_unused_function,  yeccpars2_461_/1}).
-file("src/compiler/parser/catena_parser.yrl", 638).
yeccpars2_461_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                    
    {transform_clause,
        ___3,
        undefined,
        ___5,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_462_/1}).
-dialyzer({nowarn_function, yeccpars2_462_/1}).
-compile({nowarn_unused_function,  yeccpars2_462_/1}).
-file("src/compiler/parser/catena_parser.yrl", 635).
yeccpars2_462_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                         
    [___1 | ___2]
  end | __Stack].


-file("src/compiler/parser/catena_parser.yrl", 1258).
