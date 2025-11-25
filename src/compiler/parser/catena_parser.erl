%% This file is generated from catena_parser.yrl. Do not edit directly.
-file("src/compiler/parser/catena_parser.yrl", 0).
-module(catena_parser).
-file("src/compiler/parser/catena_parser.erl", 4).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("src/compiler/parser/catena_parser.yrl", 1137).

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



-file("/home/ducky/.asdf/installs/erlang/27.3/lib/parsetools-2.6/include/yeccpre.hrl", 0).
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2024. All Rights Reserved.
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



-file("src/compiler/parser/catena_parser.erl", 230).

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
yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
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
%% yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_49(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_52(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(57=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_59(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(60=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(61=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(62=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(63=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(64=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(65=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(66=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_66(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(67=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(68=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(69=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(70=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(71=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(72=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(74=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_74(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(75=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_76(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(77=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_77(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(78=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(80=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(82=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(84=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(85=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_85(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(86=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(87=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_87(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
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
%% yeccpars2(105=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_105(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(106=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(107=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr);
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
%% yeccpars2(113=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_113(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(114=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_114(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(115=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_115(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(116=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(117=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(118=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(119=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_119(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(120=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(121=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(122=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_122(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(123=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(124=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(125=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_125(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(126=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(127=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_127(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(128=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_128(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(129=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_129(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(130=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(131=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_131(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(132=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_132(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(133=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(134=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_134(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(135=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(136=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(137=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_137(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(138=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_138(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(139=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(140=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(141=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(142=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_142(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(143=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(144=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_144(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(145=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(146=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_146(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(147=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(148=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_148(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(149=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(150=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_150(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(151=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_151(S, Cat, Ss, Stack, T, Ts, Tzr);
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
yeccpars2(159=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_159(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(160=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(161=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_161(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(162=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(163=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_163(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(164=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_164(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(165=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_165(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(166=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(167=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(168=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(169=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_169(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(170=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_170(S, Cat, Ss, Stack, T, Ts, Tzr);
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
%% yeccpars2(177=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_177(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(178=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_178(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(179=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_179(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(180=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(181=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(182=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(183=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(184=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(185=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(186=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(187=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(188=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(189=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(190=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(191=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(192=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(193=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(194=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(195=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(196=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(197=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(198=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_198(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(199=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_199(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(200=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_200(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(201=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_201(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(202=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_202(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(203=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_203(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(204=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_204(S, Cat, Ss, Stack, T, Ts, Tzr);
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
yeccpars2(216=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_216(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(217=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_217(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(218=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_218(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(219=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_219(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(220=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(221=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_221(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(222=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_222(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(223=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(224=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(225=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_225(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(226=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(227=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_227(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(228=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_228(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(229=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(230=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_230(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(231=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_231(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(232=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_232(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(233=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_233(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(234=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_234(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(235=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_235(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(236=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_236(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(237=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_237(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(238=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(239=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_239(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(240=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_240(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(241=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_241(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(242=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_242(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(243=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(244=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(245=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_245(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(246=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(247=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_247(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(248=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_248(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(249=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_249(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(250=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_250(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(251=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_251(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(252=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_252(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(253=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_253(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(254=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_254(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(255=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(256=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_256(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(257=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_257(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(258=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_258(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(259=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_259(S, Cat, Ss, Stack, T, Ts, Tzr);
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
%% yeccpars2(267=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_267(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(268=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_268(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(269=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_269(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(270=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(271=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_271(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(272=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_272(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(273=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_273(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(274=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_274(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(275=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_275(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(276=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_276(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(277=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(278=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_278(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(279=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_279(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(280=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_280(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(281=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_281(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(282=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_282(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(283=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_283(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(284=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_284(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(285=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(286=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_286(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(287=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_287(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(288=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_288(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(289=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_289(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(290=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_290(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(291=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_291(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(292=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_292(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(293=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(294=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_294(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(295=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_295(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(296=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(297=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_297(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(298=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_287(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(299=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_299(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(300=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_300(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(301=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_301(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(302=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_302(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(303=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_303(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(304=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(305=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_305(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(306=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(307=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_307(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(308=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_308(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(309=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(310=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_310(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(311=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_311(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(312=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_312(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(313=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_313(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(314=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_314(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(315=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(316=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_316(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(317=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_317(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(318=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(319=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_319(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(320=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_320(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(321=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_321(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(322=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_322(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(323=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_323(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(324=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_324(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(325=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_325(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(326=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_326(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(327=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_327(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(328=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(329=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_329(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(330=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_330(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(331=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_331(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(332=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_332(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(333=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_333(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(334=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_334(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(335=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(336=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_336(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(337=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_337(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(338=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_338(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(339=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_339(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(340=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_340(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(341=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_341(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(342=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_342(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(343=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_343(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(344=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_344(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(345=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_345(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(346=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_346(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(347=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_341(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(348=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_348(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(349=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(350=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_350(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(351=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_351(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(352=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_352(S, Cat, Ss, Stack, T, Ts, Tzr);
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
%% yeccpars2(358=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_358(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(359=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_359(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(360=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_360(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(361=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_361(S, Cat, Ss, Stack, T, Ts, Tzr);
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
yeccpars2(367=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(368=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_368(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(369=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_369(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(370=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_370(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(371=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_371(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(372=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_372(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(373=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_373(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(374=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_374(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(375=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_375(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(376=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_376(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(377=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_377(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(378=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_378(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(379=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_379(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(380=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_380(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(381=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(382=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_382(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(383=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_383(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(384=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_384(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(385=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_385(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(386=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_386(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(387=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_387(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(388=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_388(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(389=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_389(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(390=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_390(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(391=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_391(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(392=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_392(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(393=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_393(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(394=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(395=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_395(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(396=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_396(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(397=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_397(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(398=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_398(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(399=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_399(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(400=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_400(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(401=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_401(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(402=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_402(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(403=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_403(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(404=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_404(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(405=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_405(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(406=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_406(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(407=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_407(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(408=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_408(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(409=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_409(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(410=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_410(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(411=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_411(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(412=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_412(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(413=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_413(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(414=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_414(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(415=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_415(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(416=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_416(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(417=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_417(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(418=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(419=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(420=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_420(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(421=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(422=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_422(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(423=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_423(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(424=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_424(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.4",{missing_state_in_action_table, Other}}).

yeccpars2_0(S, 'module', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
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
 yeccpars1(S, 415, Ss, Stack, T, Ts, Tzr);
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
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, 'instance', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, 'property', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, 'test', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, 'trait', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, 'type', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_8/7}).
-compile({nowarn_unused_function,  yeccpars2_8/7}).
yeccpars2_8(S, 'export', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 402, Ss, Stack, T, Ts, Tzr);
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
 yeccgoto_catena_module(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_12/7}).
-compile({nowarn_unused_function,  yeccpars2_12/7}).
yeccpars2_12(S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'instance', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'property', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'test', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'trait', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'type', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_12_(Stack),
 yeccgoto_declarations(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_13/7}).
-compile({nowarn_unused_function,  yeccpars2_13/7}).
yeccpars2_13(_S, '$end', _Ss, Stack, _T, _Ts, _Tzr) ->
 {ok, hd(Stack)};
yeccpars2_13(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_14/7}).
-compile({nowarn_unused_function,  yeccpars2_14/7}).
yeccpars2_14(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 388, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 389, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_15/7}).
-compile({nowarn_unused_function,  yeccpars2_15/7}).
yeccpars2_15(S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 383, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, 'instance', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 384, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, 'trait', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 385, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 386, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, 'type', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 387, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_16(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 356, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 357, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_16(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_16/7}).
-compile({nowarn_unused_function,  yeccpars2_16/7}).
yeccpars2_cont_16(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_16(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_16(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_17/7}).
-compile({nowarn_unused_function,  yeccpars2_17/7}).
yeccpars2_17(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 351, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_18/7}).
-compile({nowarn_unused_function,  yeccpars2_18/7}).
yeccpars2_18(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 337, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 338, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_19/7}).
-compile({nowarn_unused_function,  yeccpars2_19/7}).
yeccpars2_19(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 333, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 334, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_20/7}).
-compile({nowarn_unused_function,  yeccpars2_20/7}).
yeccpars2_20(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 311, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 312, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_21/7}).
-compile({nowarn_unused_function,  yeccpars2_21/7}).
yeccpars2_21(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 90, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 91, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_22/7}).
-compile({nowarn_unused_function,  yeccpars2_22/7}).
yeccpars2_22(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_23/7}).
-compile({nowarn_unused_function,  yeccpars2_23/7}).
yeccpars2_23(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 88, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_23_(Stack),
 yeccgoto_type_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_24/7}).
-compile({nowarn_unused_function,  yeccpars2_24/7}).
yeccpars2_24(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_24_(Stack),
 yeccpars2_26(26, Cat, [24 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_25/7}).
-compile({nowarn_unused_function,  yeccpars2_25/7}).
yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_25_(Stack),
 yeccgoto_type_params(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_26/7}).
-compile({nowarn_unused_function,  yeccpars2_26/7}).
yeccpars2_26(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_26(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_26(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_27/7}).
-compile({nowarn_unused_function,  yeccpars2_27/7}).
yeccpars2_27(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_27_(Stack),
 yeccgoto_type_params_nonempty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_28/7}).
-compile({nowarn_unused_function,  yeccpars2_28/7}).
yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_28_(Stack),
 yeccgoto_type_params_nonempty(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_29/7}).
-compile({nowarn_unused_function,  yeccpars2_29/7}).
yeccpars2_29(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_30/7}).
-compile({nowarn_unused_function,  yeccpars2_30/7}).
yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_30_(Stack),
 yeccgoto_type_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_31/7}).
-compile({nowarn_unused_function,  yeccpars2_31/7}).
yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_31_(Stack),
 yeccgoto_type_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_32/7}).
-compile({nowarn_unused_function,  yeccpars2_32/7}).
yeccpars2_32(S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_32_(Stack),
 yeccgoto_constructors(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_33/7}).
-compile({nowarn_unused_function,  yeccpars2_33/7}).
yeccpars2_33(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_33_(Stack),
 yeccgoto_constructor(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_34/7}).
-compile({nowarn_unused_function,  yeccpars2_34/7}).
yeccpars2_34(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_34_(Stack),
 yeccgoto_constructor_fields(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_35/7}).
-compile({nowarn_unused_function,  yeccpars2_35/7}).
yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_35_(Stack),
 yeccgoto_constructor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_36/7}).
-compile({nowarn_unused_function,  yeccpars2_36/7}).
yeccpars2_36(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_37/7}).
-compile({nowarn_unused_function,  yeccpars2_37/7}).
yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_37_(Stack),
 yeccgoto_type_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_38(S, 'forall', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_16(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_39/7}).
-compile({nowarn_unused_function,  yeccpars2_39/7}).
yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_39_(Stack),
 yeccgoto_type_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_40/7}).
-compile({nowarn_unused_function,  yeccpars2_40/7}).
yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_40_(Stack),
 yeccgoto_type_expr_app(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_41/7}).
-compile({nowarn_unused_function,  yeccpars2_41/7}).
yeccpars2_41(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_41_(Stack),
 yeccgoto_type_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_42/7}).
-compile({nowarn_unused_function,  yeccpars2_42/7}).
yeccpars2_42(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, 'constrain', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_43/7}).
-compile({nowarn_unused_function,  yeccpars2_43/7}).
yeccpars2_43(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_43_(Stack),
 yeccpars2_50(50, Cat, [43 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_44/7}).
-compile({nowarn_unused_function,  yeccpars2_44/7}).
yeccpars2_44(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_44_(Stack),
 yeccgoto_type_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_45/7}).
-compile({nowarn_unused_function,  yeccpars2_45/7}).
yeccpars2_45(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_45_(Stack),
 yeccgoto_type_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_46/7}).
-compile({nowarn_unused_function,  yeccpars2_46/7}).
yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_46_(Stack),
 yeccgoto_type_expr_app(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_47/7}).
-compile({nowarn_unused_function,  yeccpars2_47/7}).
yeccpars2_47(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_47_(Stack),
 yeccgoto_type_expr_primary_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_48/7}).
-compile({nowarn_unused_function,  yeccpars2_48/7}).
yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_48_(Stack),
 yeccgoto_type_expr_primary_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_49/7}).
-compile({nowarn_unused_function,  yeccpars2_49/7}).
yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_49_(Stack),
 yeccgoto_type_expr_app(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_50/7}).
-compile({nowarn_unused_function,  yeccpars2_50/7}).
yeccpars2_50(S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_51: see yeccpars2_38

-dialyzer({nowarn_function, yeccpars2_52/7}).
-compile({nowarn_unused_function,  yeccpars2_52/7}).
yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_52_(Stack),
 yeccgoto_type_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_53: see yeccpars2_38

yeccpars2_54(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_16(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_55/7}).
-compile({nowarn_unused_function,  yeccpars2_55/7}).
yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_55_(Stack),
 yeccgoto_trait_constraint(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_56/7}).
-compile({nowarn_unused_function,  yeccpars2_56/7}).
yeccpars2_56(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_56_(Stack),
 yeccgoto_type_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_57/7}).
-compile({nowarn_unused_function,  yeccpars2_57/7}).
yeccpars2_57(S, 'ampersand', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_57_(Stack),
 yeccgoto_type_constraints(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_58: see yeccpars2_54

-dialyzer({nowarn_function, yeccpars2_59/7}).
-compile({nowarn_unused_function,  yeccpars2_59/7}).
yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_59_(Stack),
 yeccgoto_type_constraints(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_60/7}).
-compile({nowarn_unused_function,  yeccpars2_60/7}).
yeccpars2_60(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_60_(Stack),
 yeccgoto_type_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_61: see yeccpars2_38

-dialyzer({nowarn_function, yeccpars2_62/7}).
-compile({nowarn_unused_function,  yeccpars2_62/7}).
yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_62_(Stack),
 yeccgoto_type_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_63/7}).
-compile({nowarn_unused_function,  yeccpars2_63/7}).
yeccpars2_63(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_64/7}).
-compile({nowarn_unused_function,  yeccpars2_64/7}).
yeccpars2_64(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, 'constrain', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_64_(Stack),
 yeccgoto_type_expr_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_65: see yeccpars2_38

-dialyzer({nowarn_function, yeccpars2_66/7}).
-compile({nowarn_unused_function,  yeccpars2_66/7}).
yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_66_(Stack),
 yeccgoto_type_expr_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_67/7}).
-compile({nowarn_unused_function,  yeccpars2_67/7}).
yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_67_(Stack),
 yeccgoto_type_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_68/7}).
-compile({nowarn_unused_function,  yeccpars2_68/7}).
yeccpars2_68(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_69/7}).
-compile({nowarn_unused_function,  yeccpars2_69/7}).
yeccpars2_69(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_70/7}).
-compile({nowarn_unused_function,  yeccpars2_70/7}).
yeccpars2_70(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_71/7}).
-compile({nowarn_unused_function,  yeccpars2_71/7}).
yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_71_(Stack),
 yeccgoto_type_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_72/7}).
-compile({nowarn_unused_function,  yeccpars2_72/7}).
yeccpars2_72(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_72_(Stack),
 yeccgoto_effect_list_nonempty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_73/7}).
-compile({nowarn_unused_function,  yeccpars2_73/7}).
yeccpars2_73(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_73(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_74/7}).
-compile({nowarn_unused_function,  yeccpars2_74/7}).
yeccpars2_74(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_74_(Stack),
 yeccgoto_effect_list_nonempty(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_75/7}).
-compile({nowarn_unused_function,  yeccpars2_75/7}).
yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_75_(Stack),
 yeccgoto_type_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_76/7}).
-compile({nowarn_unused_function,  yeccpars2_76/7}).
yeccpars2_76(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_77/7}).
-compile({nowarn_unused_function,  yeccpars2_77/7}).
yeccpars2_77(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_77_(Stack),
 yeccgoto_type_record_fields(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_78/7}).
-compile({nowarn_unused_function,  yeccpars2_78/7}).
yeccpars2_78(S, 'colon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_79/7}).
-compile({nowarn_unused_function,  yeccpars2_79/7}).
yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_79_(Stack),
 yeccgoto_type_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_80: see yeccpars2_38

-dialyzer({nowarn_function, yeccpars2_81/7}).
-compile({nowarn_unused_function,  yeccpars2_81/7}).
yeccpars2_81(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, 'constrain', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_81_(Stack),
 yeccgoto_type_record_field(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_82/7}).
-compile({nowarn_unused_function,  yeccpars2_82/7}).
yeccpars2_82(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_83/7}).
-compile({nowarn_unused_function,  yeccpars2_83/7}).
yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_83_(Stack),
 yeccgoto_type_record_fields(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_84/7}).
-compile({nowarn_unused_function,  yeccpars2_84/7}).
yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_84_(Stack),
 yeccgoto_type_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_85/7}).
-compile({nowarn_unused_function,  yeccpars2_85/7}).
yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_85_(Stack),
 yeccgoto_constructor_fields(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_86: see yeccpars2_29

-dialyzer({nowarn_function, yeccpars2_87/7}).
-compile({nowarn_unused_function,  yeccpars2_87/7}).
yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_87_(Stack),
 yeccgoto_constructors(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_88: see yeccpars2_29

-dialyzer({nowarn_function, yeccpars2_89/7}).
-compile({nowarn_unused_function,  yeccpars2_89/7}).
yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_89_(Stack),
 yeccgoto_type_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_90/7}).
-compile({nowarn_unused_function,  yeccpars2_90/7}).
yeccpars2_90(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 309, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_90_(Stack),
 yeccgoto_transform_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_91/7}).
-compile({nowarn_unused_function,  yeccpars2_91/7}).
yeccpars2_91(S, 'colon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_91_(Stack),
 yeccpars2_93(93, Cat, [91 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_92/7}).
-compile({nowarn_unused_function,  yeccpars2_92/7}).
yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_92_(Stack),
 yeccgoto_pattern_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_93/7}).
-compile({nowarn_unused_function,  yeccpars2_93/7}).
yeccpars2_93(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 147, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 148, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 149, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_94/7}).
-compile({nowarn_unused_function,  yeccpars2_94/7}).
yeccpars2_94(S, 'as', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_(Stack),
 yeccgoto_pattern_list_nonempty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_95: see yeccpars2_38

-dialyzer({nowarn_function, yeccpars2_96/7}).
-compile({nowarn_unused_function,  yeccpars2_96/7}).
yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_96_(Stack),
 yeccgoto_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_97/7}).
-compile({nowarn_unused_function,  yeccpars2_97/7}).
yeccpars2_97(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_97_(Stack),
 yeccgoto_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_98/7}).
-compile({nowarn_unused_function,  yeccpars2_98/7}).
yeccpars2_98(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_98(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_98(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_99(S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_100/7}).
-compile({nowarn_unused_function,  yeccpars2_100/7}).
yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_100_(Stack),
 yeccgoto_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_101/7}).
-compile({nowarn_unused_function,  yeccpars2_101/7}).
yeccpars2_101(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_102/7}).
-compile({nowarn_unused_function,  yeccpars2_102/7}).
yeccpars2_102(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_(Stack),
 yeccgoto_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_103/7}).
-compile({nowarn_unused_function,  yeccpars2_103/7}).
yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_(Stack),
 yeccgoto_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_104/7}).
-compile({nowarn_unused_function,  yeccpars2_104/7}).
yeccpars2_104(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 108, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 109, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 111, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_104_(Stack),
 yeccgoto_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_105/7}).
-compile({nowarn_unused_function,  yeccpars2_105/7}).
yeccpars2_105(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 108, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 109, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 111, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_105_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_106/7}).
-compile({nowarn_unused_function,  yeccpars2_106/7}).
yeccpars2_106(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_(Stack),
 yeccgoto_pattern_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_107/7}).
-compile({nowarn_unused_function,  yeccpars2_107/7}).
yeccpars2_107(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_(Stack),
 yeccgoto_pattern_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_108(S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_109/7}).
-compile({nowarn_unused_function,  yeccpars2_109/7}).
yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_109_(Stack),
 yeccgoto_pattern_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_110/7}).
-compile({nowarn_unused_function,  yeccpars2_110/7}).
yeccpars2_110(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_110_(Stack),
 yeccpars2_113(113, Cat, [110 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_111/7}).
-compile({nowarn_unused_function,  yeccpars2_111/7}).
yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_111_(Stack),
 yeccgoto_pattern_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_112/7}).
-compile({nowarn_unused_function,  yeccpars2_112/7}).
yeccpars2_112(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_112_(Stack),
 yeccgoto_pattern_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_113/7}).
-compile({nowarn_unused_function,  yeccpars2_113/7}).
yeccpars2_113(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_114(S, 'as', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_115/7}).
-compile({nowarn_unused_function,  yeccpars2_115/7}).
yeccpars2_115(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_115_(Stack),
 yeccgoto_pattern_list_nonempty(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_116/7}).
-compile({nowarn_unused_function,  yeccpars2_116/7}).
yeccpars2_116(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_117: see yeccpars2_101

-dialyzer({nowarn_function, yeccpars2_118/7}).
-compile({nowarn_unused_function,  yeccpars2_118/7}).
yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_118_(Stack),
 yeccgoto_pattern_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_119/7}).
-compile({nowarn_unused_function,  yeccpars2_119/7}).
yeccpars2_119(S, 'as', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_119(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_119_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_120/7}).
-compile({nowarn_unused_function,  yeccpars2_120/7}).
yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_120_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_121/7}).
-compile({nowarn_unused_function,  yeccpars2_121/7}).
yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_121_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_122/7}).
-compile({nowarn_unused_function,  yeccpars2_122/7}).
yeccpars2_122(S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_123/7}).
-compile({nowarn_unused_function,  yeccpars2_123/7}).
yeccpars2_123(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_123_(Stack),
 yeccgoto_pattern_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_124/7}).
-compile({nowarn_unused_function,  yeccpars2_124/7}).
yeccpars2_124(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_(Stack),
 yeccgoto_pattern_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_125/7}).
-compile({nowarn_unused_function,  yeccpars2_125/7}).
yeccpars2_125(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_125_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_126: see yeccpars2_101

-dialyzer({nowarn_function, yeccpars2_127/7}).
-compile({nowarn_unused_function,  yeccpars2_127/7}).
yeccpars2_127(S, 'as', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_128/7}).
-compile({nowarn_unused_function,  yeccpars2_128/7}).
yeccpars2_128(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_129/7}).
-compile({nowarn_unused_function,  yeccpars2_129/7}).
yeccpars2_129(S, 'as', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_130: see yeccpars2_101

-dialyzer({nowarn_function, yeccpars2_131/7}).
-compile({nowarn_unused_function,  yeccpars2_131/7}).
yeccpars2_131(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_(Stack),
 yeccgoto_tuple_pattern_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_132/7}).
-compile({nowarn_unused_function,  yeccpars2_132/7}).
yeccpars2_132(S, 'as', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_132_(Stack),
 yeccgoto_tuple_pattern_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_133/7}).
-compile({nowarn_unused_function,  yeccpars2_133/7}).
yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_133_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_134/7}).
-compile({nowarn_unused_function,  yeccpars2_134/7}).
yeccpars2_134(S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_135/7}).
-compile({nowarn_unused_function,  yeccpars2_135/7}).
yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_135_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_136/7}).
-compile({nowarn_unused_function,  yeccpars2_136/7}).
yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_136_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_137/7}).
-compile({nowarn_unused_function,  yeccpars2_137/7}).
yeccpars2_137(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 145, Ss, Stack, T, Ts, Tzr);
yeccpars2_137(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_138/7}).
-compile({nowarn_unused_function,  yeccpars2_138/7}).
yeccpars2_138(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 143, Ss, Stack, T, Ts, Tzr);
yeccpars2_138(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_138_(Stack),
 yeccgoto_record_pattern_fields(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_139/7}).
-compile({nowarn_unused_function,  yeccpars2_139/7}).
yeccpars2_139(S, 'colon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_140/7}).
-compile({nowarn_unused_function,  yeccpars2_140/7}).
yeccpars2_140(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_140_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_141: see yeccpars2_101

-dialyzer({nowarn_function, yeccpars2_142/7}).
-compile({nowarn_unused_function,  yeccpars2_142/7}).
yeccpars2_142(S, 'as', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_142_(Stack),
 yeccgoto_record_pattern_field(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_143/7}).
-compile({nowarn_unused_function,  yeccpars2_143/7}).
yeccpars2_143(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_144/7}).
-compile({nowarn_unused_function,  yeccpars2_144/7}).
yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_144_(Stack),
 yeccgoto_record_pattern_fields(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_145/7}).
-compile({nowarn_unused_function,  yeccpars2_145/7}).
yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_145_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_146/7}).
-compile({nowarn_unused_function,  yeccpars2_146/7}).
yeccpars2_146(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_146(S, 'constrain', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_146_(Stack),
 yeccgoto_transform_signature(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_147(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 166, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 167, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_147(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_147/7}).
-compile({nowarn_unused_function,  yeccpars2_147/7}).
yeccpars2_cont_147(S, 'do', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 159, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_147(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 160, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_147(S, 'fn', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 161, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_147(S, 'handle', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 162, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_147(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 163, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_147(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_147(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 165, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_147(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 168, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_147(S, 'match', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 169, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_147(S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 170, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_147(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 171, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_147(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 172, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_147(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_148/7}).
-compile({nowarn_unused_function,  yeccpars2_148/7}).
yeccpars2_148(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_148_(Stack),
 yeccgoto_transform_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_149: see yeccpars2_147

-dialyzer({nowarn_function, yeccpars2_150/7}).
-compile({nowarn_unused_function,  yeccpars2_150/7}).
yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_(Stack),
 yeccgoto_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_151/7}).
-compile({nowarn_unused_function,  yeccpars2_151/7}).
yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_151_(Stack),
 yeccgoto_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_152/7}).
-compile({nowarn_unused_function,  yeccpars2_152/7}).
yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_152_(Stack),
 yeccgoto_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_153/7}).
-compile({nowarn_unused_function,  yeccpars2_153/7}).
yeccpars2_153(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 306, Ss, Stack, T, Ts, Tzr);
yeccpars2_153(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_154/7}).
-compile({nowarn_unused_function,  yeccpars2_154/7}).
yeccpars2_154(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 304, Ss, Stack, T, Ts, Tzr);
yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_154_(Stack),
 yeccgoto_guards(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_155/7}).
-compile({nowarn_unused_function,  yeccpars2_155/7}).
yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_155_(Stack),
 yeccgoto_expr_app(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_156/7}).
-compile({nowarn_unused_function,  yeccpars2_156/7}).
yeccpars2_156(S, 'do', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 159, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 302, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 160, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, 'fn', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 161, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, 'handle', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 162, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 163, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 165, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 166, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 167, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 168, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, 'match', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 169, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 170, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 171, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 172, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_156_(Stack),
 yeccgoto_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_157/7}).
-compile({nowarn_unused_function,  yeccpars2_157/7}).
yeccpars2_157(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_157_(Stack),
 yeccgoto_guard(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_158/7}).
-compile({nowarn_unused_function,  yeccpars2_158/7}).
yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_158_(Stack),
 yeccgoto_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_159/7}).
-compile({nowarn_unused_function,  yeccpars2_159/7}).
yeccpars2_159(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 287, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_160/7}).
-compile({nowarn_unused_function,  yeccpars2_160/7}).
yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_160_(Stack),
 yeccgoto_literal(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_161/7}).
-compile({nowarn_unused_function,  yeccpars2_161/7}).
yeccpars2_161(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 284, Ss, Stack, T, Ts, Tzr);
yeccpars2_161(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_162: see yeccpars2_147

-dialyzer({nowarn_function, yeccpars2_163/7}).
-compile({nowarn_unused_function,  yeccpars2_163/7}).
yeccpars2_163(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_163_(Stack),
 yeccgoto_literal(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_164/7}).
-compile({nowarn_unused_function,  yeccpars2_164/7}).
yeccpars2_164(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 253, Ss, Stack, T, Ts, Tzr);
yeccpars2_164(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 254, Ss, Stack, T, Ts, Tzr);
yeccpars2_164(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_165(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 166, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 167, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 249, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_147(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_166/7}).
-compile({nowarn_unused_function,  yeccpars2_166/7}).
yeccpars2_166(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 243, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_167/7}).
-compile({nowarn_unused_function,  yeccpars2_167/7}).
yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_167_(Stack),
 yeccgoto_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_168: see yeccpars2_147

yeccpars2_169(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 166, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 167, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 220, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_147(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_170/7}).
-compile({nowarn_unused_function,  yeccpars2_170/7}).
yeccpars2_170(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 173, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_171/7}).
-compile({nowarn_unused_function,  yeccpars2_171/7}).
yeccpars2_171(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_171_(Stack),
 yeccgoto_literal(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_172/7}).
-compile({nowarn_unused_function,  yeccpars2_172/7}).
yeccpars2_172(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_172_(Stack),
 yeccgoto_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_173/7}).
-compile({nowarn_unused_function,  yeccpars2_173/7}).
yeccpars2_173(S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_174/7}).
-compile({nowarn_unused_function,  yeccpars2_174/7}).
yeccpars2_174(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 175, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_175/7}).
-compile({nowarn_unused_function,  yeccpars2_175/7}).
yeccpars2_175(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 176, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_176/7}).
-compile({nowarn_unused_function,  yeccpars2_176/7}).
yeccpars2_176(S, 'do', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 159, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 160, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'fn', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 161, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'handle', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 162, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 163, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 165, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 166, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 167, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 168, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'match', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 169, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 170, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 171, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 172, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_176_(Stack),
 yeccpars2_177(177, Cat, [176 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_177/7}).
-compile({nowarn_unused_function,  yeccpars2_177/7}).
yeccpars2_177(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 216, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_178/7}).
-compile({nowarn_unused_function,  yeccpars2_178/7}).
yeccpars2_178(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_178_(Stack),
 yeccgoto_expr_list_opt(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_179/7}).
-compile({nowarn_unused_function,  yeccpars2_179/7}).
yeccpars2_179(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_179_(Stack),
 yeccgoto_expr_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_180: see yeccpars2_147

%% yeccpars2_181: see yeccpars2_147

%% yeccpars2_182: see yeccpars2_147

%% yeccpars2_183: see yeccpars2_147

%% yeccpars2_184: see yeccpars2_147

%% yeccpars2_185: see yeccpars2_147

%% yeccpars2_186: see yeccpars2_147

%% yeccpars2_187: see yeccpars2_147

%% yeccpars2_188: see yeccpars2_147

%% yeccpars2_189: see yeccpars2_147

%% yeccpars2_190: see yeccpars2_147

%% yeccpars2_191: see yeccpars2_147

%% yeccpars2_192: see yeccpars2_147

%% yeccpars2_193: see yeccpars2_147

%% yeccpars2_194: see yeccpars2_147

%% yeccpars2_195: see yeccpars2_147

%% yeccpars2_196: see yeccpars2_147

%% yeccpars2_197: see yeccpars2_147

-dialyzer({nowarn_function, yeccpars2_198/7}).
-compile({nowarn_unused_function,  yeccpars2_198/7}).
yeccpars2_198(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_198_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_199/7}).
-compile({nowarn_unused_function,  yeccpars2_199/7}).
yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_199_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_200/7}).
-compile({nowarn_unused_function,  yeccpars2_200/7}).
yeccpars2_200(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_200_$end'(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_and(Stack),
 yeccgoto_expr(hd(Nss), 'and', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_arrow(Stack),
 yeccgoto_expr(hd(Nss), 'arrow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_comma(Stack),
 yeccgoto_expr(hd(Nss), 'comma', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_cons(Stack),
 yeccgoto_expr(hd(Nss), 'cons', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'do', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_do(Stack),
 yeccgoto_expr(hd(Nss), 'do', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_dot(Stack),
 yeccgoto_expr(hd(Nss), 'dot', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_effect(Stack),
 yeccgoto_expr(hd(Nss), 'effect', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_end(Stack),
 yeccgoto_expr(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_equals(Stack),
 yeccgoto_expr(hd(Nss), 'equals', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'error', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_error(Stack),
 yeccgoto_expr(hd(Nss), 'error', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'float', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_float(Stack),
 yeccgoto_expr(hd(Nss), 'float', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'fn', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_fn(Stack),
 yeccgoto_expr(hd(Nss), 'fn', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'handle', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_handle(Stack),
 yeccgoto_expr(hd(Nss), 'handle', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_in(Stack),
 yeccgoto_expr(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'instance', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_instance(Stack),
 yeccgoto_expr(hd(Nss), 'instance', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_integer(Stack),
 yeccgoto_expr(hd(Nss), 'integer', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_lbrace(Stack),
 yeccgoto_expr(hd(Nss), 'lbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_lbracket(Stack),
 yeccgoto_expr(hd(Nss), 'lbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'let', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_let(Stack),
 yeccgoto_expr(hd(Nss), 'let', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_lower_ident(Stack),
 yeccgoto_expr(hd(Nss), 'lower_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_lparen(Stack),
 yeccgoto_expr(hd(Nss), 'lparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'match', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_match(Stack),
 yeccgoto_expr(hd(Nss), 'match', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_of(Stack),
 yeccgoto_expr(hd(Nss), 'of', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_or(Stack),
 yeccgoto_expr(hd(Nss), 'or', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_perform(Stack),
 yeccgoto_expr(hd(Nss), 'perform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_pipe(Stack),
 yeccgoto_expr(hd(Nss), 'pipe', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_pipe_right(Stack),
 yeccgoto_expr(hd(Nss), 'pipe_right', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'property', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_property(Stack),
 yeccgoto_expr(hd(Nss), 'property', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_rbrace(Stack),
 yeccgoto_expr(hd(Nss), 'rbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_rbracket(Stack),
 yeccgoto_expr(hd(Nss), 'rbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_rparen(Stack),
 yeccgoto_expr(hd(Nss), 'rparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'semicolon', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_semicolon(Stack),
 yeccgoto_expr(hd(Nss), 'semicolon', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'string', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_string(Stack),
 yeccgoto_expr(hd(Nss), 'string', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'test', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_test(Stack),
 yeccgoto_expr(hd(Nss), 'test', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_then(Stack),
 yeccgoto_expr(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'trait', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_trait(Stack),
 yeccgoto_expr(hd(Nss), 'trait', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_transform(Stack),
 yeccgoto_expr(hd(Nss), 'transform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'type', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_type(Stack),
 yeccgoto_expr(hd(Nss), 'type', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_upper_ident(Stack),
 yeccgoto_expr(hd(Nss), 'upper_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_200(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_201/7}).
-compile({nowarn_unused_function,  yeccpars2_201/7}).
yeccpars2_201(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_201_$end'(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_and(Stack),
 yeccgoto_expr(hd(Nss), 'and', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_arrow(Stack),
 yeccgoto_expr(hd(Nss), 'arrow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_comma(Stack),
 yeccgoto_expr(hd(Nss), 'comma', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_cons(Stack),
 yeccgoto_expr(hd(Nss), 'cons', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'do', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_do(Stack),
 yeccgoto_expr(hd(Nss), 'do', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_dot(Stack),
 yeccgoto_expr(hd(Nss), 'dot', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_effect(Stack),
 yeccgoto_expr(hd(Nss), 'effect', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_end(Stack),
 yeccgoto_expr(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_equals(Stack),
 yeccgoto_expr(hd(Nss), 'equals', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'error', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_error(Stack),
 yeccgoto_expr(hd(Nss), 'error', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'float', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_float(Stack),
 yeccgoto_expr(hd(Nss), 'float', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'fn', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_fn(Stack),
 yeccgoto_expr(hd(Nss), 'fn', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'handle', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_handle(Stack),
 yeccgoto_expr(hd(Nss), 'handle', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_in(Stack),
 yeccgoto_expr(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'instance', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_instance(Stack),
 yeccgoto_expr(hd(Nss), 'instance', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_integer(Stack),
 yeccgoto_expr(hd(Nss), 'integer', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_lbrace(Stack),
 yeccgoto_expr(hd(Nss), 'lbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_lbracket(Stack),
 yeccgoto_expr(hd(Nss), 'lbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'let', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_let(Stack),
 yeccgoto_expr(hd(Nss), 'let', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_lower_ident(Stack),
 yeccgoto_expr(hd(Nss), 'lower_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_lparen(Stack),
 yeccgoto_expr(hd(Nss), 'lparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'match', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_match(Stack),
 yeccgoto_expr(hd(Nss), 'match', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_of(Stack),
 yeccgoto_expr(hd(Nss), 'of', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_or(Stack),
 yeccgoto_expr(hd(Nss), 'or', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_perform(Stack),
 yeccgoto_expr(hd(Nss), 'perform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_pipe(Stack),
 yeccgoto_expr(hd(Nss), 'pipe', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_pipe_right(Stack),
 yeccgoto_expr(hd(Nss), 'pipe_right', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'property', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_property(Stack),
 yeccgoto_expr(hd(Nss), 'property', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_rbrace(Stack),
 yeccgoto_expr(hd(Nss), 'rbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_rbracket(Stack),
 yeccgoto_expr(hd(Nss), 'rbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_rparen(Stack),
 yeccgoto_expr(hd(Nss), 'rparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'semicolon', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_semicolon(Stack),
 yeccgoto_expr(hd(Nss), 'semicolon', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'string', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_string(Stack),
 yeccgoto_expr(hd(Nss), 'string', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'test', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_test(Stack),
 yeccgoto_expr(hd(Nss), 'test', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_then(Stack),
 yeccgoto_expr(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'trait', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_trait(Stack),
 yeccgoto_expr(hd(Nss), 'trait', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_transform(Stack),
 yeccgoto_expr(hd(Nss), 'transform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'type', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_type(Stack),
 yeccgoto_expr(hd(Nss), 'type', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_upper_ident(Stack),
 yeccgoto_expr(hd(Nss), 'upper_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_201(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_202/7}).
-compile({nowarn_unused_function,  yeccpars2_202/7}).
yeccpars2_202(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_203/7}).
-compile({nowarn_unused_function,  yeccpars2_203/7}).
yeccpars2_203(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_204/7}).
-compile({nowarn_unused_function,  yeccpars2_204/7}).
yeccpars2_204(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_204(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_204(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_204(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_204(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_204(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_204(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_204(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_204(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_204(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_204(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_204(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_204(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_204(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_204(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_204(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_204(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_205/7}).
-compile({nowarn_unused_function,  yeccpars2_205/7}).
yeccpars2_205(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_206/7}).
-compile({nowarn_unused_function,  yeccpars2_206/7}).
yeccpars2_206(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_206_$end'(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_and(Stack),
 yeccgoto_expr(hd(Nss), 'and', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_arrow(Stack),
 yeccgoto_expr(hd(Nss), 'arrow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_comma(Stack),
 yeccgoto_expr(hd(Nss), 'comma', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_cons(Stack),
 yeccgoto_expr(hd(Nss), 'cons', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'do', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_do(Stack),
 yeccgoto_expr(hd(Nss), 'do', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_dot(Stack),
 yeccgoto_expr(hd(Nss), 'dot', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_effect(Stack),
 yeccgoto_expr(hd(Nss), 'effect', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_end(Stack),
 yeccgoto_expr(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_equals(Stack),
 yeccgoto_expr(hd(Nss), 'equals', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'error', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_error(Stack),
 yeccgoto_expr(hd(Nss), 'error', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'float', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_float(Stack),
 yeccgoto_expr(hd(Nss), 'float', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'fn', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_fn(Stack),
 yeccgoto_expr(hd(Nss), 'fn', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'handle', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_handle(Stack),
 yeccgoto_expr(hd(Nss), 'handle', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_in(Stack),
 yeccgoto_expr(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'instance', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_instance(Stack),
 yeccgoto_expr(hd(Nss), 'instance', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_integer(Stack),
 yeccgoto_expr(hd(Nss), 'integer', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_lbrace(Stack),
 yeccgoto_expr(hd(Nss), 'lbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_lbracket(Stack),
 yeccgoto_expr(hd(Nss), 'lbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'let', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_let(Stack),
 yeccgoto_expr(hd(Nss), 'let', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_lower_ident(Stack),
 yeccgoto_expr(hd(Nss), 'lower_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_lparen(Stack),
 yeccgoto_expr(hd(Nss), 'lparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'match', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_match(Stack),
 yeccgoto_expr(hd(Nss), 'match', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_of(Stack),
 yeccgoto_expr(hd(Nss), 'of', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_or(Stack),
 yeccgoto_expr(hd(Nss), 'or', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_perform(Stack),
 yeccgoto_expr(hd(Nss), 'perform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_pipe(Stack),
 yeccgoto_expr(hd(Nss), 'pipe', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_pipe_right(Stack),
 yeccgoto_expr(hd(Nss), 'pipe_right', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'property', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_property(Stack),
 yeccgoto_expr(hd(Nss), 'property', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_rbrace(Stack),
 yeccgoto_expr(hd(Nss), 'rbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_rbracket(Stack),
 yeccgoto_expr(hd(Nss), 'rbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_rparen(Stack),
 yeccgoto_expr(hd(Nss), 'rparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'semicolon', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_semicolon(Stack),
 yeccgoto_expr(hd(Nss), 'semicolon', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'string', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_string(Stack),
 yeccgoto_expr(hd(Nss), 'string', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'test', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_test(Stack),
 yeccgoto_expr(hd(Nss), 'test', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_then(Stack),
 yeccgoto_expr(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'trait', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_trait(Stack),
 yeccgoto_expr(hd(Nss), 'trait', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_transform(Stack),
 yeccgoto_expr(hd(Nss), 'transform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'type', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_type(Stack),
 yeccgoto_expr(hd(Nss), 'type', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_upper_ident(Stack),
 yeccgoto_expr(hd(Nss), 'upper_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_207/7}).
-compile({nowarn_unused_function,  yeccpars2_207/7}).
yeccpars2_207(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_207(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_207(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_208/7}).
-compile({nowarn_unused_function,  yeccpars2_208/7}).
yeccpars2_208(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_208_$end'(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_and(Stack),
 yeccgoto_expr(hd(Nss), 'and', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_arrow(Stack),
 yeccgoto_expr(hd(Nss), 'arrow', Nss, NewStack, T, Ts, Tzr);
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
yeccpars2_208(_S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_eq(Stack),
 yeccgoto_expr(hd(Nss), 'eq', Nss, NewStack, T, Ts, Tzr);
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
yeccpars2_208(_S, 'fn', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_fn(Stack),
 yeccgoto_expr(hd(Nss), 'fn', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'handle', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_handle(Stack),
 yeccgoto_expr(hd(Nss), 'handle', Nss, NewStack, T, Ts, Tzr);
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
yeccpars2_208(_S, 'match', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_match(Stack),
 yeccgoto_expr(hd(Nss), 'match', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_neq(Stack),
 yeccgoto_expr(hd(Nss), 'neq', Nss, NewStack, T, Ts, Tzr);
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
yeccpars2_208(_S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_setoid_eq(Stack),
 yeccgoto_expr(hd(Nss), 'setoid_eq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_setoid_neq(Stack),
 yeccgoto_expr(hd(Nss), 'setoid_neq', Nss, NewStack, T, Ts, Tzr);
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
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_209(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_209(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_209(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_209(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_209(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_209_$end'(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_and(Stack),
 yeccgoto_expr(hd(Nss), 'and', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_arrow(Stack),
 yeccgoto_expr(hd(Nss), 'arrow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_comma(Stack),
 yeccgoto_expr(hd(Nss), 'comma', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_cons(Stack),
 yeccgoto_expr(hd(Nss), 'cons', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'do', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_do(Stack),
 yeccgoto_expr(hd(Nss), 'do', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_dot(Stack),
 yeccgoto_expr(hd(Nss), 'dot', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_effect(Stack),
 yeccgoto_expr(hd(Nss), 'effect', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_end(Stack),
 yeccgoto_expr(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_eq(Stack),
 yeccgoto_expr(hd(Nss), 'eq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_equals(Stack),
 yeccgoto_expr(hd(Nss), 'equals', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'error', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_error(Stack),
 yeccgoto_expr(hd(Nss), 'error', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'float', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_float(Stack),
 yeccgoto_expr(hd(Nss), 'float', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'fn', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_fn(Stack),
 yeccgoto_expr(hd(Nss), 'fn', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'handle', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_handle(Stack),
 yeccgoto_expr(hd(Nss), 'handle', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_in(Stack),
 yeccgoto_expr(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'instance', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_instance(Stack),
 yeccgoto_expr(hd(Nss), 'instance', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_integer(Stack),
 yeccgoto_expr(hd(Nss), 'integer', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_lbrace(Stack),
 yeccgoto_expr(hd(Nss), 'lbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_lbracket(Stack),
 yeccgoto_expr(hd(Nss), 'lbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'let', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_let(Stack),
 yeccgoto_expr(hd(Nss), 'let', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_lower_ident(Stack),
 yeccgoto_expr(hd(Nss), 'lower_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_lparen(Stack),
 yeccgoto_expr(hd(Nss), 'lparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'match', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_match(Stack),
 yeccgoto_expr(hd(Nss), 'match', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_neq(Stack),
 yeccgoto_expr(hd(Nss), 'neq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_of(Stack),
 yeccgoto_expr(hd(Nss), 'of', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_or(Stack),
 yeccgoto_expr(hd(Nss), 'or', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_perform(Stack),
 yeccgoto_expr(hd(Nss), 'perform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_pipe(Stack),
 yeccgoto_expr(hd(Nss), 'pipe', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_pipe_right(Stack),
 yeccgoto_expr(hd(Nss), 'pipe_right', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'property', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_property(Stack),
 yeccgoto_expr(hd(Nss), 'property', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_rbrace(Stack),
 yeccgoto_expr(hd(Nss), 'rbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_rbracket(Stack),
 yeccgoto_expr(hd(Nss), 'rbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_rparen(Stack),
 yeccgoto_expr(hd(Nss), 'rparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'semicolon', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_semicolon(Stack),
 yeccgoto_expr(hd(Nss), 'semicolon', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_setoid_eq(Stack),
 yeccgoto_expr(hd(Nss), 'setoid_eq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_setoid_neq(Stack),
 yeccgoto_expr(hd(Nss), 'setoid_neq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'string', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_string(Stack),
 yeccgoto_expr(hd(Nss), 'string', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'test', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_test(Stack),
 yeccgoto_expr(hd(Nss), 'test', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_then(Stack),
 yeccgoto_expr(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'trait', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_trait(Stack),
 yeccgoto_expr(hd(Nss), 'trait', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_transform(Stack),
 yeccgoto_expr(hd(Nss), 'transform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'type', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_type(Stack),
 yeccgoto_expr(hd(Nss), 'type', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_upper_ident(Stack),
 yeccgoto_expr(hd(Nss), 'upper_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_209(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_210/7}).
-compile({nowarn_unused_function,  yeccpars2_210/7}).
yeccpars2_210(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_210(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_210(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_210(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_210(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_210(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_210_$end'(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_and(Stack),
 yeccgoto_expr(hd(Nss), 'and', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_arrow(Stack),
 yeccgoto_expr(hd(Nss), 'arrow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_comma(Stack),
 yeccgoto_expr(hd(Nss), 'comma', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_cons(Stack),
 yeccgoto_expr(hd(Nss), 'cons', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'do', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_do(Stack),
 yeccgoto_expr(hd(Nss), 'do', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_dot(Stack),
 yeccgoto_expr(hd(Nss), 'dot', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_effect(Stack),
 yeccgoto_expr(hd(Nss), 'effect', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_end(Stack),
 yeccgoto_expr(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_eq(Stack),
 yeccgoto_expr(hd(Nss), 'eq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_equals(Stack),
 yeccgoto_expr(hd(Nss), 'equals', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'error', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_error(Stack),
 yeccgoto_expr(hd(Nss), 'error', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'float', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_float(Stack),
 yeccgoto_expr(hd(Nss), 'float', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'fn', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_fn(Stack),
 yeccgoto_expr(hd(Nss), 'fn', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'handle', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_handle(Stack),
 yeccgoto_expr(hd(Nss), 'handle', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_in(Stack),
 yeccgoto_expr(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'instance', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_instance(Stack),
 yeccgoto_expr(hd(Nss), 'instance', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_integer(Stack),
 yeccgoto_expr(hd(Nss), 'integer', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_lbrace(Stack),
 yeccgoto_expr(hd(Nss), 'lbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_lbracket(Stack),
 yeccgoto_expr(hd(Nss), 'lbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'let', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_let(Stack),
 yeccgoto_expr(hd(Nss), 'let', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_lower_ident(Stack),
 yeccgoto_expr(hd(Nss), 'lower_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_lparen(Stack),
 yeccgoto_expr(hd(Nss), 'lparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'match', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_match(Stack),
 yeccgoto_expr(hd(Nss), 'match', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_neq(Stack),
 yeccgoto_expr(hd(Nss), 'neq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_of(Stack),
 yeccgoto_expr(hd(Nss), 'of', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_or(Stack),
 yeccgoto_expr(hd(Nss), 'or', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_perform(Stack),
 yeccgoto_expr(hd(Nss), 'perform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_pipe(Stack),
 yeccgoto_expr(hd(Nss), 'pipe', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_pipe_right(Stack),
 yeccgoto_expr(hd(Nss), 'pipe_right', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'property', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_property(Stack),
 yeccgoto_expr(hd(Nss), 'property', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_rbrace(Stack),
 yeccgoto_expr(hd(Nss), 'rbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_rbracket(Stack),
 yeccgoto_expr(hd(Nss), 'rbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_rparen(Stack),
 yeccgoto_expr(hd(Nss), 'rparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'semicolon', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_semicolon(Stack),
 yeccgoto_expr(hd(Nss), 'semicolon', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_setoid_eq(Stack),
 yeccgoto_expr(hd(Nss), 'setoid_eq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_setoid_neq(Stack),
 yeccgoto_expr(hd(Nss), 'setoid_neq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'string', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_string(Stack),
 yeccgoto_expr(hd(Nss), 'string', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'test', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_test(Stack),
 yeccgoto_expr(hd(Nss), 'test', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_then(Stack),
 yeccgoto_expr(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'trait', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_trait(Stack),
 yeccgoto_expr(hd(Nss), 'trait', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_transform(Stack),
 yeccgoto_expr(hd(Nss), 'transform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'type', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_type(Stack),
 yeccgoto_expr(hd(Nss), 'type', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_upper_ident(Stack),
 yeccgoto_expr(hd(Nss), 'upper_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_210(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_211/7}).
-compile({nowarn_unused_function,  yeccpars2_211/7}).
yeccpars2_211(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_211(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_211(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_211(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_211(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_211(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_211_$end'(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_and(Stack),
 yeccgoto_expr(hd(Nss), 'and', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_arrow(Stack),
 yeccgoto_expr(hd(Nss), 'arrow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_comma(Stack),
 yeccgoto_expr(hd(Nss), 'comma', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_cons(Stack),
 yeccgoto_expr(hd(Nss), 'cons', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'do', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_do(Stack),
 yeccgoto_expr(hd(Nss), 'do', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_dot(Stack),
 yeccgoto_expr(hd(Nss), 'dot', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_effect(Stack),
 yeccgoto_expr(hd(Nss), 'effect', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_end(Stack),
 yeccgoto_expr(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_eq(Stack),
 yeccgoto_expr(hd(Nss), 'eq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_equals(Stack),
 yeccgoto_expr(hd(Nss), 'equals', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'error', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_error(Stack),
 yeccgoto_expr(hd(Nss), 'error', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'float', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_float(Stack),
 yeccgoto_expr(hd(Nss), 'float', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'fn', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_fn(Stack),
 yeccgoto_expr(hd(Nss), 'fn', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'handle', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_handle(Stack),
 yeccgoto_expr(hd(Nss), 'handle', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_in(Stack),
 yeccgoto_expr(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'instance', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_instance(Stack),
 yeccgoto_expr(hd(Nss), 'instance', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_integer(Stack),
 yeccgoto_expr(hd(Nss), 'integer', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_lbrace(Stack),
 yeccgoto_expr(hd(Nss), 'lbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_lbracket(Stack),
 yeccgoto_expr(hd(Nss), 'lbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'let', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_let(Stack),
 yeccgoto_expr(hd(Nss), 'let', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_lower_ident(Stack),
 yeccgoto_expr(hd(Nss), 'lower_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_lparen(Stack),
 yeccgoto_expr(hd(Nss), 'lparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'match', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_match(Stack),
 yeccgoto_expr(hd(Nss), 'match', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_neq(Stack),
 yeccgoto_expr(hd(Nss), 'neq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_of(Stack),
 yeccgoto_expr(hd(Nss), 'of', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_or(Stack),
 yeccgoto_expr(hd(Nss), 'or', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_perform(Stack),
 yeccgoto_expr(hd(Nss), 'perform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_pipe(Stack),
 yeccgoto_expr(hd(Nss), 'pipe', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_pipe_right(Stack),
 yeccgoto_expr(hd(Nss), 'pipe_right', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'property', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_property(Stack),
 yeccgoto_expr(hd(Nss), 'property', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_rbrace(Stack),
 yeccgoto_expr(hd(Nss), 'rbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_rbracket(Stack),
 yeccgoto_expr(hd(Nss), 'rbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_rparen(Stack),
 yeccgoto_expr(hd(Nss), 'rparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'semicolon', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_semicolon(Stack),
 yeccgoto_expr(hd(Nss), 'semicolon', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_setoid_eq(Stack),
 yeccgoto_expr(hd(Nss), 'setoid_eq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_setoid_neq(Stack),
 yeccgoto_expr(hd(Nss), 'setoid_neq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'string', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_string(Stack),
 yeccgoto_expr(hd(Nss), 'string', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'test', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_test(Stack),
 yeccgoto_expr(hd(Nss), 'test', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_then(Stack),
 yeccgoto_expr(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'trait', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_trait(Stack),
 yeccgoto_expr(hd(Nss), 'trait', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_transform(Stack),
 yeccgoto_expr(hd(Nss), 'transform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'type', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_type(Stack),
 yeccgoto_expr(hd(Nss), 'type', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_upper_ident(Stack),
 yeccgoto_expr(hd(Nss), 'upper_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_211(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_212/7}).
-compile({nowarn_unused_function,  yeccpars2_212/7}).
yeccpars2_212(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_212_$end'(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_and(Stack),
 yeccgoto_expr(hd(Nss), 'and', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_arrow(Stack),
 yeccgoto_expr(hd(Nss), 'arrow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_comma(Stack),
 yeccgoto_expr(hd(Nss), 'comma', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_cons(Stack),
 yeccgoto_expr(hd(Nss), 'cons', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'do', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_do(Stack),
 yeccgoto_expr(hd(Nss), 'do', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_dot(Stack),
 yeccgoto_expr(hd(Nss), 'dot', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_effect(Stack),
 yeccgoto_expr(hd(Nss), 'effect', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_end(Stack),
 yeccgoto_expr(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_equals(Stack),
 yeccgoto_expr(hd(Nss), 'equals', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'error', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_error(Stack),
 yeccgoto_expr(hd(Nss), 'error', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'float', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_float(Stack),
 yeccgoto_expr(hd(Nss), 'float', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'fn', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_fn(Stack),
 yeccgoto_expr(hd(Nss), 'fn', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'handle', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_handle(Stack),
 yeccgoto_expr(hd(Nss), 'handle', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_in(Stack),
 yeccgoto_expr(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'instance', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_instance(Stack),
 yeccgoto_expr(hd(Nss), 'instance', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_integer(Stack),
 yeccgoto_expr(hd(Nss), 'integer', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_lbrace(Stack),
 yeccgoto_expr(hd(Nss), 'lbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_lbracket(Stack),
 yeccgoto_expr(hd(Nss), 'lbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'let', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_let(Stack),
 yeccgoto_expr(hd(Nss), 'let', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_lower_ident(Stack),
 yeccgoto_expr(hd(Nss), 'lower_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_lparen(Stack),
 yeccgoto_expr(hd(Nss), 'lparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'match', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_match(Stack),
 yeccgoto_expr(hd(Nss), 'match', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_of(Stack),
 yeccgoto_expr(hd(Nss), 'of', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_or(Stack),
 yeccgoto_expr(hd(Nss), 'or', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_perform(Stack),
 yeccgoto_expr(hd(Nss), 'perform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_pipe(Stack),
 yeccgoto_expr(hd(Nss), 'pipe', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_pipe_right(Stack),
 yeccgoto_expr(hd(Nss), 'pipe_right', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'property', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_property(Stack),
 yeccgoto_expr(hd(Nss), 'property', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_rbrace(Stack),
 yeccgoto_expr(hd(Nss), 'rbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_rbracket(Stack),
 yeccgoto_expr(hd(Nss), 'rbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_rparen(Stack),
 yeccgoto_expr(hd(Nss), 'rparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'semicolon', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_semicolon(Stack),
 yeccgoto_expr(hd(Nss), 'semicolon', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'string', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_string(Stack),
 yeccgoto_expr(hd(Nss), 'string', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'test', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_test(Stack),
 yeccgoto_expr(hd(Nss), 'test', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_then(Stack),
 yeccgoto_expr(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'trait', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_trait(Stack),
 yeccgoto_expr(hd(Nss), 'trait', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_transform(Stack),
 yeccgoto_expr(hd(Nss), 'transform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'type', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_type(Stack),
 yeccgoto_expr(hd(Nss), 'type', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_upper_ident(Stack),
 yeccgoto_expr(hd(Nss), 'upper_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_212(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_213/7}).
-compile({nowarn_unused_function,  yeccpars2_213/7}).
yeccpars2_213(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_214/7}).
-compile({nowarn_unused_function,  yeccpars2_214/7}).
yeccpars2_214(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_214_(Stack),
 yeccgoto_expr_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_215/7}).
-compile({nowarn_unused_function,  yeccpars2_215/7}).
yeccpars2_215(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_215_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_216/7}).
-compile({nowarn_unused_function,  yeccpars2_216/7}).
yeccpars2_216(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_216_(Stack),
 yeccgoto_perform_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_217/7}).
-compile({nowarn_unused_function,  yeccpars2_217/7}).
yeccpars2_217(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 235, Ss, Stack, T, Ts, Tzr);
yeccpars2_217(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_218/7}).
-compile({nowarn_unused_function,  yeccpars2_218/7}).
yeccpars2_218(S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 220, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_218_(Stack),
 yeccgoto_match_clauses(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_219(S, 'of', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 231, Ss, Stack, T, Ts, Tzr);
yeccpars2_219(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_219(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_219/7}).
-compile({nowarn_unused_function,  yeccpars2_219/7}).
yeccpars2_cont_219(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_219(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_219(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_219(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_219(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_219(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_219(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_219(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_219(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_219(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_219(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_219(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_219(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_219(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_219(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_219(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_219(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_219(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_220: see yeccpars2_101

-dialyzer({nowarn_function, yeccpars2_221/7}).
-compile({nowarn_unused_function,  yeccpars2_221/7}).
yeccpars2_221(S, 'as', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 229, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_221_(Stack),
 yeccgoto_or_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_222/7}).
-compile({nowarn_unused_function,  yeccpars2_222/7}).
yeccpars2_222(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 223, Ss, Stack, T, Ts, Tzr);
yeccpars2_222(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 224, Ss, Stack, T, Ts, Tzr);
yeccpars2_222(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_223: see yeccpars2_147

%% yeccpars2_224: see yeccpars2_147

-dialyzer({nowarn_function, yeccpars2_225/7}).
-compile({nowarn_unused_function,  yeccpars2_225/7}).
yeccpars2_225(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 226, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_226: see yeccpars2_147

-dialyzer({nowarn_function, yeccpars2_227/7}).
-compile({nowarn_unused_function,  yeccpars2_227/7}).
yeccpars2_227(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_227_(Stack),
 yeccgoto_match_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_228/7}).
-compile({nowarn_unused_function,  yeccpars2_228/7}).
yeccpars2_228(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_228(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_228(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_228(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_228(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_228(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_228(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_228(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_228(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_228(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_228(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_228(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_228(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_228(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_228(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_228(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_228(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_228(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_228_(Stack),
 yeccgoto_match_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_229: see yeccpars2_101

-dialyzer({nowarn_function, yeccpars2_230/7}).
-compile({nowarn_unused_function,  yeccpars2_230/7}).
yeccpars2_230(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_230_(Stack),
 yeccgoto_or_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_231/7}).
-compile({nowarn_unused_function,  yeccpars2_231/7}).
yeccpars2_231(S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 220, Ss, Stack, T, Ts, Tzr);
yeccpars2_231(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_232/7}).
-compile({nowarn_unused_function,  yeccpars2_232/7}).
yeccpars2_232(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 233, Ss, Stack, T, Ts, Tzr);
yeccpars2_232(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_233/7}).
-compile({nowarn_unused_function,  yeccpars2_233/7}).
yeccpars2_233(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_233_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_234/7}).
-compile({nowarn_unused_function,  yeccpars2_234/7}).
yeccpars2_234(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_234_(Stack),
 yeccgoto_match_clauses(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_235/7}).
-compile({nowarn_unused_function,  yeccpars2_235/7}).
yeccpars2_235(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_235_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_236/7}).
-compile({nowarn_unused_function,  yeccpars2_236/7}).
yeccpars2_236(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 242, Ss, Stack, T, Ts, Tzr);
yeccpars2_236(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_237(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 238, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 239, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_219(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_238: see yeccpars2_147

-dialyzer({nowarn_function, yeccpars2_239/7}).
-compile({nowarn_unused_function,  yeccpars2_239/7}).
yeccpars2_239(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_239_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_240/7}).
-compile({nowarn_unused_function,  yeccpars2_240/7}).
yeccpars2_240(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_240_(Stack),
 yeccgoto_tuple_expr_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_241/7}).
-compile({nowarn_unused_function,  yeccpars2_241/7}).
yeccpars2_241(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 238, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_241_(Stack),
 yeccgoto_tuple_expr_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_242/7}).
-compile({nowarn_unused_function,  yeccpars2_242/7}).
yeccpars2_242(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_242_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_243/7}).
-compile({nowarn_unused_function,  yeccpars2_243/7}).
yeccpars2_243(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 244, Ss, Stack, T, Ts, Tzr);
yeccpars2_243(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_244: see yeccpars2_147

yeccpars2_245(S, 'in', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 246, Ss, Stack, T, Ts, Tzr);
yeccpars2_245(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_219(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_246: see yeccpars2_147

-dialyzer({nowarn_function, yeccpars2_247/7}).
-compile({nowarn_unused_function,  yeccpars2_247/7}).
yeccpars2_247(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_247_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_248/7}).
-compile({nowarn_unused_function,  yeccpars2_248/7}).
yeccpars2_248(S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 250, Ss, Stack, T, Ts, Tzr);
yeccpars2_248(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_249/7}).
-compile({nowarn_unused_function,  yeccpars2_249/7}).
yeccpars2_249(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_249_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_250/7}).
-compile({nowarn_unused_function,  yeccpars2_250/7}).
yeccpars2_250(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_250_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_251/7}).
-compile({nowarn_unused_function,  yeccpars2_251/7}).
yeccpars2_251(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 259, Ss, Stack, T, Ts, Tzr);
yeccpars2_251(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_252/7}).
-compile({nowarn_unused_function,  yeccpars2_252/7}).
yeccpars2_252(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 257, Ss, Stack, T, Ts, Tzr);
yeccpars2_252(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_252_(Stack),
 yeccgoto_record_fields(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_253/7}).
-compile({nowarn_unused_function,  yeccpars2_253/7}).
yeccpars2_253(S, 'colon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 255, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_254/7}).
-compile({nowarn_unused_function,  yeccpars2_254/7}).
yeccpars2_254(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_254_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_255: see yeccpars2_147

-dialyzer({nowarn_function, yeccpars2_256/7}).
-compile({nowarn_unused_function,  yeccpars2_256/7}).
yeccpars2_256(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_256(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_256(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_256(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_256(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_256(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_256(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_256(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_256(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_256(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_256(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_256(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_256(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_256(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_256(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_256(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_256(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_256(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_256_(Stack),
 yeccgoto_record_field(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_257/7}).
-compile({nowarn_unused_function,  yeccpars2_257/7}).
yeccpars2_257(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 253, Ss, Stack, T, Ts, Tzr);
yeccpars2_257(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_258/7}).
-compile({nowarn_unused_function,  yeccpars2_258/7}).
yeccpars2_258(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_258_(Stack),
 yeccgoto_record_fields(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_259/7}).
-compile({nowarn_unused_function,  yeccpars2_259/7}).
yeccpars2_259(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_259_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_260(S, 'then', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 261, Ss, Stack, T, Ts, Tzr);
yeccpars2_260(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_219(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_261/7}).
-compile({nowarn_unused_function,  yeccpars2_261/7}).
yeccpars2_261(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 262, Ss, Stack, T, Ts, Tzr);
yeccpars2_261(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_262/7}).
-compile({nowarn_unused_function,  yeccpars2_262/7}).
yeccpars2_262(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 265, Ss, Stack, T, Ts, Tzr);
yeccpars2_262(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_263/7}).
-compile({nowarn_unused_function,  yeccpars2_263/7}).
yeccpars2_263(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 283, Ss, Stack, T, Ts, Tzr);
yeccpars2_263(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_264/7}).
-compile({nowarn_unused_function,  yeccpars2_264/7}).
yeccpars2_264(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 265, Ss, Stack, T, Ts, Tzr);
yeccpars2_264(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_264_(Stack),
 yeccgoto_handler_clauses(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_265/7}).
-compile({nowarn_unused_function,  yeccpars2_265/7}).
yeccpars2_265(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 266, Ss, Stack, T, Ts, Tzr);
yeccpars2_265(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_266/7}).
-compile({nowarn_unused_function,  yeccpars2_266/7}).
yeccpars2_266(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 269, Ss, Stack, T, Ts, Tzr);
yeccpars2_266(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_267/7}).
-compile({nowarn_unused_function,  yeccpars2_267/7}).
yeccpars2_267(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 269, Ss, Stack, T, Ts, Tzr);
yeccpars2_267(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 281, Ss, Stack, T, Ts, Tzr);
yeccpars2_267(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_268/7}).
-compile({nowarn_unused_function,  yeccpars2_268/7}).
yeccpars2_268(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_268_(Stack),
 yeccgoto_operation_cases(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_269/7}).
-compile({nowarn_unused_function,  yeccpars2_269/7}).
yeccpars2_269(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 270, Ss, Stack, T, Ts, Tzr);
yeccpars2_269(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 271, Ss, Stack, T, Ts, Tzr);
yeccpars2_269(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_270: see yeccpars2_147

-dialyzer({nowarn_function, yeccpars2_271/7}).
-compile({nowarn_unused_function,  yeccpars2_271/7}).
yeccpars2_271(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_271(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_271(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_271(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_271(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_271(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_271(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_271(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_271(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_271(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_271_(Stack),
 yeccpars2_272(272, Cat, [271 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_272/7}).
-compile({nowarn_unused_function,  yeccpars2_272/7}).
yeccpars2_272(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 276, Ss, Stack, T, Ts, Tzr);
yeccpars2_272(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_273/7}).
-compile({nowarn_unused_function,  yeccpars2_273/7}).
yeccpars2_273(S, 'as', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_273(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 274, Ss, Stack, T, Ts, Tzr);
yeccpars2_273(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_273(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_273_(Stack),
 yeccgoto_pattern_list_comma(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_274/7}).
-compile({nowarn_unused_function,  yeccpars2_274/7}).
yeccpars2_274(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_274(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_274(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_274(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_274(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_274(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_274(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_274(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_274(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_274(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_274_(Stack),
 yeccpars2_275(_S, Cat, [274 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_275/7}).
-compile({nowarn_unused_function,  yeccpars2_275/7}).
yeccpars2_275(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_275_(Stack),
 yeccgoto_pattern_list_comma(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_276/7}).
-compile({nowarn_unused_function,  yeccpars2_276/7}).
yeccpars2_276(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 277, Ss, Stack, T, Ts, Tzr);
yeccpars2_276(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_277: see yeccpars2_147

-dialyzer({nowarn_function, yeccpars2_278/7}).
-compile({nowarn_unused_function,  yeccpars2_278/7}).
yeccpars2_278(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_278_(Stack),
 yeccgoto_operation_case(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_279/7}).
-compile({nowarn_unused_function,  yeccpars2_279/7}).
yeccpars2_279(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_279_(Stack),
 yeccgoto_operation_case(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_280/7}).
-compile({nowarn_unused_function,  yeccpars2_280/7}).
yeccpars2_280(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_280_(Stack),
 yeccgoto_operation_cases(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_281/7}).
-compile({nowarn_unused_function,  yeccpars2_281/7}).
yeccpars2_281(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_281_(Stack),
 yeccgoto_handler_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_282/7}).
-compile({nowarn_unused_function,  yeccpars2_282/7}).
yeccpars2_282(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_282_(Stack),
 yeccgoto_handler_clauses(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_283/7}).
-compile({nowarn_unused_function,  yeccpars2_283/7}).
yeccpars2_283(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_283_(Stack),
 yeccgoto_try_with_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_284/7}).
-compile({nowarn_unused_function,  yeccpars2_284/7}).
yeccpars2_284(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 285, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_285: see yeccpars2_147

-dialyzer({nowarn_function, yeccpars2_286/7}).
-compile({nowarn_unused_function,  yeccpars2_286/7}).
yeccpars2_286(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_286_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_287(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 291, Ss, Stack, T, Ts, Tzr);
yeccpars2_287(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 292, Ss, Stack, T, Ts, Tzr);
yeccpars2_287(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_147(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_288/7}).
-compile({nowarn_unused_function,  yeccpars2_288/7}).
yeccpars2_288(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_288(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_288(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_288(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_288(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_288(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_288(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_288(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_288(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_288(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_288(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_288(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_288(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_288(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_288(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_288(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_288(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_288(_S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_288_rbrace(Stack),
 yeccgoto_do_statements(hd(Ss), 'rbrace', Ss, NewStack, T, Ts, Tzr);
yeccpars2_288(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_288_(Stack),
 yeccgoto_do_statement(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_289/7}).
-compile({nowarn_unused_function,  yeccpars2_289/7}).
yeccpars2_289(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 300, Ss, Stack, T, Ts, Tzr);
yeccpars2_289(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_290/7}).
-compile({nowarn_unused_function,  yeccpars2_290/7}).
yeccpars2_290(S, 'semicolon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 298, Ss, Stack, T, Ts, Tzr);
yeccpars2_290(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_291/7}).
-compile({nowarn_unused_function,  yeccpars2_291/7}).
yeccpars2_291(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 295, Ss, Stack, T, Ts, Tzr);
yeccpars2_291(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_292/7}).
-compile({nowarn_unused_function,  yeccpars2_292/7}).
yeccpars2_292(S, 'left_arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 293, Ss, Stack, T, Ts, Tzr);
yeccpars2_292(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_292_(Stack),
 yeccgoto_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_293: see yeccpars2_147

-dialyzer({nowarn_function, yeccpars2_294/7}).
-compile({nowarn_unused_function,  yeccpars2_294/7}).
yeccpars2_294(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_294_(Stack),
 yeccgoto_do_statement(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_295/7}).
-compile({nowarn_unused_function,  yeccpars2_295/7}).
yeccpars2_295(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 296, Ss, Stack, T, Ts, Tzr);
yeccpars2_295(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_296: see yeccpars2_147

-dialyzer({nowarn_function, yeccpars2_297/7}).
-compile({nowarn_unused_function,  yeccpars2_297/7}).
yeccpars2_297(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_297(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_297(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_297(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_297(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_297(S, 'in', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 246, Ss, Stack, T, Ts, Tzr);
yeccpars2_297(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_297(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_297(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_297(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_297(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_297(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_297(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_297(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_297(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_297(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_297(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_297(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_297(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_297_(Stack),
 yeccgoto_do_statement(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_298: see yeccpars2_287

-dialyzer({nowarn_function, yeccpars2_299/7}).
-compile({nowarn_unused_function,  yeccpars2_299/7}).
yeccpars2_299(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_299_(Stack),
 yeccgoto_do_statements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_300/7}).
-compile({nowarn_unused_function,  yeccpars2_300/7}).
yeccpars2_300(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_300_(Stack),
 yeccgoto_do_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_301/7}).
-compile({nowarn_unused_function,  yeccpars2_301/7}).
yeccpars2_301(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_301_(Stack),
 yeccgoto_expr_app(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_302/7}).
-compile({nowarn_unused_function,  yeccpars2_302/7}).
yeccpars2_302(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 303, Ss, Stack, T, Ts, Tzr);
yeccpars2_302(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_303/7}).
-compile({nowarn_unused_function,  yeccpars2_303/7}).
yeccpars2_303(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_303_(Stack),
 yeccgoto_expr_app(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_304: see yeccpars2_147

-dialyzer({nowarn_function, yeccpars2_305/7}).
-compile({nowarn_unused_function,  yeccpars2_305/7}).
yeccpars2_305(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_305_(Stack),
 yeccgoto_guards(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_306: see yeccpars2_147

-dialyzer({nowarn_function, yeccpars2_307/7}).
-compile({nowarn_unused_function,  yeccpars2_307/7}).
yeccpars2_307(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_307(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_307(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_307(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_307(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_307(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_307(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_307(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_307(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_307(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_307(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_307(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_307(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_307(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_307(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_307(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_307(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_307(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_307_(Stack),
 yeccgoto_transform_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_308/7}).
-compile({nowarn_unused_function,  yeccpars2_308/7}).
yeccpars2_308(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_308(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_308(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_308(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_308(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_308(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_308(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_308(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_308(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_308(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_308(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_308(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_308(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_308(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_308(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_308(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_308(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_308(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_308_(Stack),
 yeccgoto_transform_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_309: see yeccpars2_147

-dialyzer({nowarn_function, yeccpars2_310/7}).
-compile({nowarn_unused_function,  yeccpars2_310/7}).
yeccpars2_310(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_310(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_310(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_310(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_310(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_310(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_310(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_310(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_310(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_310(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_310(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_310(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_310(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_310(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_310(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_310(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_310(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_310(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_310_(Stack),
 yeccgoto_transform_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_311/7}).
-compile({nowarn_unused_function,  yeccpars2_311/7}).
yeccpars2_311(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_311_(Stack),
 yeccgoto_trait_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_312/7}).
-compile({nowarn_unused_function,  yeccpars2_312/7}).
yeccpars2_312(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_312(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_312_(Stack),
 yeccpars2_313(313, Cat, [312 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_313/7}).
-compile({nowarn_unused_function,  yeccpars2_313/7}).
yeccpars2_313(S, 'extend', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 315, Ss, Stack, T, Ts, Tzr);
yeccpars2_313(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_313_(Stack),
 yeccpars2_314(314, Cat, [313 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_314/7}).
-compile({nowarn_unused_function,  yeccpars2_314/7}).
yeccpars2_314(S, 'where', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 320, Ss, Stack, T, Ts, Tzr);
yeccpars2_314(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_315: see yeccpars2_54

-dialyzer({nowarn_function, yeccpars2_316/7}).
-compile({nowarn_unused_function,  yeccpars2_316/7}).
yeccpars2_316(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_316_(Stack),
 yeccgoto_maybe_trait_extends(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_317/7}).
-compile({nowarn_unused_function,  yeccpars2_317/7}).
yeccpars2_317(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 318, Ss, Stack, T, Ts, Tzr);
yeccpars2_317(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_317_(Stack),
 yeccgoto_trait_extends_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_318: see yeccpars2_54

-dialyzer({nowarn_function, yeccpars2_319/7}).
-compile({nowarn_unused_function,  yeccpars2_319/7}).
yeccpars2_319(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_319_(Stack),
 yeccgoto_trait_extends_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_320/7}).
-compile({nowarn_unused_function,  yeccpars2_320/7}).
yeccpars2_320(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 323, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_321/7}).
-compile({nowarn_unused_function,  yeccpars2_321/7}).
yeccpars2_321(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 332, Ss, Stack, T, Ts, Tzr);
yeccpars2_321(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_322/7}).
-compile({nowarn_unused_function,  yeccpars2_322/7}).
yeccpars2_322(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 330, Ss, Stack, T, Ts, Tzr);
yeccpars2_322(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_322_(Stack),
 yeccgoto_trait_members(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_323(S, 'colon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 325, Ss, Stack, T, Ts, Tzr);
yeccpars2_323(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_324/7}).
-compile({nowarn_unused_function,  yeccpars2_324/7}).
yeccpars2_324(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 328, Ss, Stack, T, Ts, Tzr);
yeccpars2_324(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_325(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 327, Ss, Stack, T, Ts, Tzr);
yeccpars2_325(S, 'forall', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_325(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_325(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_325(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_16(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_326/7}).
-compile({nowarn_unused_function,  yeccpars2_326/7}).
yeccpars2_326(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_326(S, 'constrain', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_326(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_326_(Stack),
 yeccgoto_trait_member(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_327/7}).
-compile({nowarn_unused_function,  yeccpars2_327/7}).
yeccpars2_327(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_327_(Stack),
 yeccgoto_trait_member(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_328: see yeccpars2_147

-dialyzer({nowarn_function, yeccpars2_329/7}).
-compile({nowarn_unused_function,  yeccpars2_329/7}).
yeccpars2_329(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_329(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_329(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_329(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_329(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_329(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_329(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_329(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_329(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_329(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_329(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_329(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_329(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_329(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_329(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_329(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_329(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_329(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_329_(Stack),
 yeccgoto_trait_member(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_330/7}).
-compile({nowarn_unused_function,  yeccpars2_330/7}).
yeccpars2_330(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 323, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_330_(Stack),
 yeccgoto_trait_members(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_331/7}).
-compile({nowarn_unused_function,  yeccpars2_331/7}).
yeccpars2_331(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_331_(Stack),
 yeccgoto_trait_members(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_332/7}).
-compile({nowarn_unused_function,  yeccpars2_332/7}).
yeccpars2_332(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_332_(Stack),
 yeccgoto_trait_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_333/7}).
-compile({nowarn_unused_function,  yeccpars2_333/7}).
yeccpars2_333(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_333_(Stack),
 yeccgoto_test_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_334/7}).
-compile({nowarn_unused_function,  yeccpars2_334/7}).
yeccpars2_334(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 335, Ss, Stack, T, Ts, Tzr);
yeccpars2_334(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_335: see yeccpars2_147

-dialyzer({nowarn_function, yeccpars2_336/7}).
-compile({nowarn_unused_function,  yeccpars2_336/7}).
yeccpars2_336(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_336(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_336(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_336(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_336(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_336(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_336(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_336(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_336(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_336(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_336(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_336(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_336(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_336(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_336(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_336(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_336(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_336(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_336_(Stack),
 yeccgoto_test_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_337/7}).
-compile({nowarn_unused_function,  yeccpars2_337/7}).
yeccpars2_337(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_337_(Stack),
 yeccgoto_property_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_338/7}).
-compile({nowarn_unused_function,  yeccpars2_338/7}).
yeccpars2_338(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 339, Ss, Stack, T, Ts, Tzr);
yeccpars2_338(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_339/7}).
-compile({nowarn_unused_function,  yeccpars2_339/7}).
yeccpars2_339(S, 'forall', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 341, Ss, Stack, T, Ts, Tzr);
yeccpars2_339(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_340/7}).
-compile({nowarn_unused_function,  yeccpars2_340/7}).
yeccpars2_340(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_340_(Stack),
 yeccgoto_property_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_341/7}).
-compile({nowarn_unused_function,  yeccpars2_341/7}).
yeccpars2_341(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 344, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_342/7}).
-compile({nowarn_unused_function,  yeccpars2_342/7}).
yeccpars2_342(S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 349, Ss, Stack, T, Ts, Tzr);
yeccpars2_342(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_343/7}).
-compile({nowarn_unused_function,  yeccpars2_343/7}).
yeccpars2_343(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 347, Ss, Stack, T, Ts, Tzr);
yeccpars2_343(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_343_(Stack),
 yeccgoto_property_bindings(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_344/7}).
-compile({nowarn_unused_function,  yeccpars2_344/7}).
yeccpars2_344(S, 'colon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 345, Ss, Stack, T, Ts, Tzr);
yeccpars2_344(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_345/7}).
-compile({nowarn_unused_function,  yeccpars2_345/7}).
yeccpars2_345(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 346, Ss, Stack, T, Ts, Tzr);
yeccpars2_345(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_346/7}).
-compile({nowarn_unused_function,  yeccpars2_346/7}).
yeccpars2_346(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_346_(Stack),
 yeccgoto_property_binding(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_347: see yeccpars2_341

-dialyzer({nowarn_function, yeccpars2_348/7}).
-compile({nowarn_unused_function,  yeccpars2_348/7}).
yeccpars2_348(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_348_(Stack),
 yeccgoto_property_bindings(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_349: see yeccpars2_147

-dialyzer({nowarn_function, yeccpars2_350/7}).
-compile({nowarn_unused_function,  yeccpars2_350/7}).
yeccpars2_350(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_350(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_350(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_350(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_350(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_350(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_350(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_350(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_350(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_350(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_350(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_350(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_350(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_350(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_350(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_350(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_350(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_350(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_350_(Stack),
 yeccgoto_property_body(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_351/7}).
-compile({nowarn_unused_function,  yeccpars2_351/7}).
yeccpars2_351(S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 352, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_351_(Stack),
 yeccgoto_module_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_352/7}).
-compile({nowarn_unused_function,  yeccpars2_352/7}).
yeccpars2_352(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 353, Ss, Stack, T, Ts, Tzr);
yeccpars2_352(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_353/7}).
-compile({nowarn_unused_function,  yeccpars2_353/7}).
yeccpars2_353(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_353_(Stack),
 yeccgoto_module_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_354/7}).
-compile({nowarn_unused_function,  yeccpars2_354/7}).
yeccpars2_354(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 381, Ss, Stack, T, Ts, Tzr);
yeccpars2_354(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_354_(Stack),
 yeccgoto_instance_constraints(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_355/7}).
-compile({nowarn_unused_function,  yeccpars2_355/7}).
yeccpars2_355(S, 'double_arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 373, Ss, Stack, T, Ts, Tzr);
yeccpars2_355(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_356/7}).
-compile({nowarn_unused_function,  yeccpars2_356/7}).
yeccpars2_356(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_356_(Stack),
 yeccgoto_instance_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_357/7}).
-compile({nowarn_unused_function,  yeccpars2_357/7}).
yeccpars2_357(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_357(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_357(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_357(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_357(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_357_(Stack),
 yeccgoto_type_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_358/7}).
-compile({nowarn_unused_function,  yeccpars2_358/7}).
yeccpars2_358(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_358(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_358(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_358(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_358(_S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_358_comma(Stack),
 yeccgoto_type_expr_primary_list(hd(Ss), 'comma', Ss, NewStack, T, Ts, Tzr);
yeccpars2_358(_S, 'double_arrow', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_358_double_arrow(Stack),
 yeccgoto_type_expr_primary_list(hd(Ss), 'double_arrow', Ss, NewStack, T, Ts, Tzr);
yeccpars2_358(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_358_(Stack),
 yeccgoto_instance_type_args(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_359/7}).
-compile({nowarn_unused_function,  yeccpars2_359/7}).
yeccpars2_359(S, 'where', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 360, Ss, Stack, T, Ts, Tzr);
yeccpars2_359(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_360/7}).
-compile({nowarn_unused_function,  yeccpars2_360/7}).
yeccpars2_360(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 363, Ss, Stack, T, Ts, Tzr);
yeccpars2_360(S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 364, Ss, Stack, T, Ts, Tzr);
yeccpars2_360(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_361/7}).
-compile({nowarn_unused_function,  yeccpars2_361/7}).
yeccpars2_361(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 371, Ss, Stack, T, Ts, Tzr);
yeccpars2_361(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_362/7}).
-compile({nowarn_unused_function,  yeccpars2_362/7}).
yeccpars2_362(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 369, Ss, Stack, T, Ts, Tzr);
yeccpars2_362(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_362_(Stack),
 yeccgoto_instance_methods(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_363/7}).
-compile({nowarn_unused_function,  yeccpars2_363/7}).
yeccpars2_363(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_363_(Stack),
 yeccgoto_instance_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_364/7}).
-compile({nowarn_unused_function,  yeccpars2_364/7}).
yeccpars2_364(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 365, Ss, Stack, T, Ts, Tzr);
yeccpars2_364(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_365/7}).
-compile({nowarn_unused_function,  yeccpars2_365/7}).
yeccpars2_365(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_365(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_365(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_365(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_365(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_365(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_365(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_365(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_365(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_365(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_365_(Stack),
 yeccpars2_366(366, Cat, [365 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_366/7}).
-compile({nowarn_unused_function,  yeccpars2_366/7}).
yeccpars2_366(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 367, Ss, Stack, T, Ts, Tzr);
yeccpars2_366(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_367: see yeccpars2_147

-dialyzer({nowarn_function, yeccpars2_368/7}).
-compile({nowarn_unused_function,  yeccpars2_368/7}).
yeccpars2_368(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_368(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_368(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_368(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_368(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_368(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_368(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_368(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_368(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_368(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_368(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_368(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_368(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_368(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_368(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_368(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_368(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_368(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_368_(Stack),
 yeccgoto_instance_method(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_369/7}).
-compile({nowarn_unused_function,  yeccpars2_369/7}).
yeccpars2_369(S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 364, Ss, Stack, T, Ts, Tzr);
yeccpars2_369(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_369_(Stack),
 yeccgoto_instance_methods(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_370/7}).
-compile({nowarn_unused_function,  yeccpars2_370/7}).
yeccpars2_370(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_370_(Stack),
 yeccgoto_instance_methods(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_371/7}).
-compile({nowarn_unused_function,  yeccpars2_371/7}).
yeccpars2_371(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_371_(Stack),
 yeccgoto_instance_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_372/7}).
-compile({nowarn_unused_function,  yeccpars2_372/7}).
yeccpars2_372(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_372_(Stack),
 yeccgoto_instance_type_args(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_373/7}).
-compile({nowarn_unused_function,  yeccpars2_373/7}).
yeccpars2_373(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 374, Ss, Stack, T, Ts, Tzr);
yeccpars2_373(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_374(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_374(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_374(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_16(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_375/7}).
-compile({nowarn_unused_function,  yeccpars2_375/7}).
yeccpars2_375(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_375(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_375(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_375(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_375(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_375_(Stack),
 yeccgoto_instance_type_args(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_376/7}).
-compile({nowarn_unused_function,  yeccpars2_376/7}).
yeccpars2_376(S, 'where', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 377, Ss, Stack, T, Ts, Tzr);
yeccpars2_376(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_377/7}).
-compile({nowarn_unused_function,  yeccpars2_377/7}).
yeccpars2_377(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 379, Ss, Stack, T, Ts, Tzr);
yeccpars2_377(S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 364, Ss, Stack, T, Ts, Tzr);
yeccpars2_377(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_378/7}).
-compile({nowarn_unused_function,  yeccpars2_378/7}).
yeccpars2_378(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 380, Ss, Stack, T, Ts, Tzr);
yeccpars2_378(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_379/7}).
-compile({nowarn_unused_function,  yeccpars2_379/7}).
yeccpars2_379(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_379_(Stack),
 yeccgoto_instance_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_380/7}).
-compile({nowarn_unused_function,  yeccpars2_380/7}).
yeccpars2_380(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_380_(Stack),
 yeccgoto_instance_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_381: see yeccpars2_54

-dialyzer({nowarn_function, yeccpars2_382/7}).
-compile({nowarn_unused_function,  yeccpars2_382/7}).
yeccpars2_382(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_382_(Stack),
 yeccgoto_instance_constraints(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_383/7}).
-compile({nowarn_unused_function,  yeccpars2_383/7}).
yeccpars2_383(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_383_(Stack),
 yeccgoto_declaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_384/7}).
-compile({nowarn_unused_function,  yeccpars2_384/7}).
yeccpars2_384(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_384_(Stack),
 yeccgoto_declaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_385/7}).
-compile({nowarn_unused_function,  yeccpars2_385/7}).
yeccpars2_385(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_385_(Stack),
 yeccgoto_declaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_386/7}).
-compile({nowarn_unused_function,  yeccpars2_386/7}).
yeccpars2_386(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_386_(Stack),
 yeccgoto_declaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_387/7}).
-compile({nowarn_unused_function,  yeccpars2_387/7}).
yeccpars2_387(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_387_(Stack),
 yeccgoto_declaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_388/7}).
-compile({nowarn_unused_function,  yeccpars2_388/7}).
yeccpars2_388(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_388_(Stack),
 yeccgoto_effect_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_389/7}).
-compile({nowarn_unused_function,  yeccpars2_389/7}).
yeccpars2_389(S, 'operation', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 392, Ss, Stack, T, Ts, Tzr);
yeccpars2_389(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_389_(Stack),
 yeccpars2_390(390, Cat, [389 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_390/7}).
-compile({nowarn_unused_function,  yeccpars2_390/7}).
yeccpars2_390(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 397, Ss, Stack, T, Ts, Tzr);
yeccpars2_390(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_391/7}).
-compile({nowarn_unused_function,  yeccpars2_391/7}).
yeccpars2_391(S, 'operation', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 392, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_391_(Stack),
 yeccpars2_396(_S, Cat, [391 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_392/7}).
-compile({nowarn_unused_function,  yeccpars2_392/7}).
yeccpars2_392(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 393, Ss, Stack, T, Ts, Tzr);
yeccpars2_392(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_393/7}).
-compile({nowarn_unused_function,  yeccpars2_393/7}).
yeccpars2_393(S, 'colon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 394, Ss, Stack, T, Ts, Tzr);
yeccpars2_393(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_393_(Stack),
 yeccgoto_effect_operation(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_394: see yeccpars2_38

-dialyzer({nowarn_function, yeccpars2_395/7}).
-compile({nowarn_unused_function,  yeccpars2_395/7}).
yeccpars2_395(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_395(S, 'constrain', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_395(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_395_(Stack),
 yeccgoto_effect_operation(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_396/7}).
-compile({nowarn_unused_function,  yeccpars2_396/7}).
yeccpars2_396(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_396_(Stack),
 yeccgoto_effect_operations(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_397/7}).
-compile({nowarn_unused_function,  yeccpars2_397/7}).
yeccpars2_397(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_397_(Stack),
 yeccgoto_effect_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_398/7}).
-compile({nowarn_unused_function,  yeccpars2_398/7}).
yeccpars2_398(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_398_(Stack),
 yeccgoto_declarations(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_399/7}).
-compile({nowarn_unused_function,  yeccpars2_399/7}).
yeccpars2_399(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_399_(Stack),
 yeccgoto_export_decl(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_400/7}).
-compile({nowarn_unused_function,  yeccpars2_400/7}).
yeccpars2_400(S, 'export', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 402, Ss, Stack, T, Ts, Tzr);
yeccpars2_400(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_400_(Stack),
 yeccgoto_export_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_401/7}).
-compile({nowarn_unused_function,  yeccpars2_401/7}).
yeccpars2_401(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_401_(Stack),
 yeccgoto_module_header(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_402/7}).
-compile({nowarn_unused_function,  yeccpars2_402/7}).
yeccpars2_402(S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 403, Ss, Stack, T, Ts, Tzr);
yeccpars2_402(S, 'trait', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 404, Ss, Stack, T, Ts, Tzr);
yeccpars2_402(S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 405, Ss, Stack, T, Ts, Tzr);
yeccpars2_402(S, 'type', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 406, Ss, Stack, T, Ts, Tzr);
yeccpars2_402(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_403/7}).
-compile({nowarn_unused_function,  yeccpars2_403/7}).
yeccpars2_403(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 410, Ss, Stack, T, Ts, Tzr);
yeccpars2_403(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_404/7}).
-compile({nowarn_unused_function,  yeccpars2_404/7}).
yeccpars2_404(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 409, Ss, Stack, T, Ts, Tzr);
yeccpars2_404(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_405/7}).
-compile({nowarn_unused_function,  yeccpars2_405/7}).
yeccpars2_405(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 408, Ss, Stack, T, Ts, Tzr);
yeccpars2_405(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_406/7}).
-compile({nowarn_unused_function,  yeccpars2_406/7}).
yeccpars2_406(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 407, Ss, Stack, T, Ts, Tzr);
yeccpars2_406(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_407/7}).
-compile({nowarn_unused_function,  yeccpars2_407/7}).
yeccpars2_407(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_407_(Stack),
 yeccgoto_export_item(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_408/7}).
-compile({nowarn_unused_function,  yeccpars2_408/7}).
yeccpars2_408(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_408_(Stack),
 yeccgoto_export_item(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_409/7}).
-compile({nowarn_unused_function,  yeccpars2_409/7}).
yeccpars2_409(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_409_(Stack),
 yeccgoto_export_item(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_410/7}).
-compile({nowarn_unused_function,  yeccpars2_410/7}).
yeccpars2_410(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_410_(Stack),
 yeccgoto_export_item(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_411/7}).
-compile({nowarn_unused_function,  yeccpars2_411/7}).
yeccpars2_411(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_411_(Stack),
 yeccgoto_export_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_412/7}).
-compile({nowarn_unused_function,  yeccpars2_412/7}).
yeccpars2_412(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_412_(Stack),
 yeccgoto_catena_module(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_413/7}).
-compile({nowarn_unused_function,  yeccpars2_413/7}).
yeccpars2_413(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_413_(Stack),
 yeccgoto_transform_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_414/7}).
-compile({nowarn_unused_function,  yeccpars2_414/7}).
yeccpars2_414(S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 415, Ss, Stack, T, Ts, Tzr);
yeccpars2_414(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_414_(Stack),
 yeccgoto_transform_clauses(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_415/7}).
-compile({nowarn_unused_function,  yeccpars2_415/7}).
yeccpars2_415(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 416, Ss, Stack, T, Ts, Tzr);
yeccpars2_415(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_416/7}).
-compile({nowarn_unused_function,  yeccpars2_416/7}).
yeccpars2_416(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_416(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_416(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_416(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_416(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_416(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_416(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_416(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_416(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_416(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_416_(Stack),
 yeccpars2_417(417, Cat, [416 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_417/7}).
-compile({nowarn_unused_function,  yeccpars2_417/7}).
yeccpars2_417(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 418, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 419, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_418: see yeccpars2_147

%% yeccpars2_419: see yeccpars2_147

-dialyzer({nowarn_function, yeccpars2_420/7}).
-compile({nowarn_unused_function,  yeccpars2_420/7}).
yeccpars2_420(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 421, Ss, Stack, T, Ts, Tzr);
yeccpars2_420(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_421: see yeccpars2_147

-dialyzer({nowarn_function, yeccpars2_422/7}).
-compile({nowarn_unused_function,  yeccpars2_422/7}).
yeccpars2_422(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_422(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_422(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_422(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_422(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_422(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_422(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_422(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_422(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_422(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_422(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_422(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_422(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_422(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_422(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_422(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_422(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_422(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_422_(Stack),
 yeccgoto_transform_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_423/7}).
-compile({nowarn_unused_function,  yeccpars2_423/7}).
yeccpars2_423(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_423(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_423(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_423(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_423(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_423(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_423(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_423(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_423(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_423(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_423(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_423(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_423(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_423(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_423(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_423(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_423(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_423(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_423_(Stack),
 yeccgoto_transform_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_424/7}).
-compile({nowarn_unused_function,  yeccpars2_424/7}).
yeccpars2_424(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_424_(Stack),
 yeccgoto_transform_clauses(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_catena_module/7}).
-compile({nowarn_unused_function,  yeccgoto_catena_module/7}).
yeccgoto_catena_module(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(13, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_constructor/7}).
-compile({nowarn_unused_function,  yeccgoto_constructor/7}).
yeccgoto_constructor(29, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_constructor(86, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_constructor(88, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_constructor_fields/7}).
-compile({nowarn_unused_function,  yeccgoto_constructor_fields/7}).
yeccgoto_constructor_fields(33=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_constructor_fields(34=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_constructors/7}).
-compile({nowarn_unused_function,  yeccgoto_constructors/7}).
yeccgoto_constructors(29=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_constructors(86=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_constructors(88=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_declaration/7}).
-compile({nowarn_unused_function,  yeccgoto_declaration/7}).
yeccgoto_declaration(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_declaration(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_declaration(12, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_declarations/7}).
-compile({nowarn_unused_function,  yeccgoto_declarations/7}).
yeccgoto_declarations(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_declarations(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_412(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_declarations(12=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_398(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_do_expr/7}).
-compile({nowarn_unused_function,  yeccgoto_do_expr/7}).
yeccgoto_do_expr(147=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(168=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(176=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(182=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(185=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(187=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(188=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(189=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(191=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(192=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(193=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(194=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(197=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(223=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(224=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(226=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(238=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(244=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(246=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(255=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(270=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(277=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(285=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(287=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(293=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(296=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(298=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(304=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(306=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(309=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(328=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(335=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(349=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(367=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(418=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(419=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(421=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_do_statement/7}).
-compile({nowarn_unused_function,  yeccgoto_do_statement/7}).
yeccgoto_do_statement(287, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_290(290, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_statement(298, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_290(290, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_do_statements/7}).
-compile({nowarn_unused_function,  yeccgoto_do_statements/7}).
yeccgoto_do_statements(287, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_289(289, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_statements(298=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_299(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_effect_decl/7}).
-compile({nowarn_unused_function,  yeccgoto_effect_decl/7}).
yeccgoto_effect_decl(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_effect_decl(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_effect_decl(12=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_effect_list_nonempty/7}).
-compile({nowarn_unused_function,  yeccgoto_effect_list_nonempty/7}).
yeccgoto_effect_list_nonempty(69, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(70, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_effect_list_nonempty(73=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_effect_operation/7}).
-compile({nowarn_unused_function,  yeccgoto_effect_operation/7}).
yeccgoto_effect_operation(389, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_391(391, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_effect_operation(391, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_391(391, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_effect_operations/7}).
-compile({nowarn_unused_function,  yeccgoto_effect_operations/7}).
yeccgoto_effect_operations(389, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_390(390, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_effect_operations(391=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_396(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_export_decl/7}).
-compile({nowarn_unused_function,  yeccgoto_export_decl/7}).
yeccgoto_export_decl(8=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_401(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_export_item/7}).
-compile({nowarn_unused_function,  yeccgoto_export_item/7}).
yeccgoto_export_item(8, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_400(400, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_export_item(400, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_400(400, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_export_list/7}).
-compile({nowarn_unused_function,  yeccgoto_export_list/7}).
yeccgoto_export_list(8=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_399(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_export_list(400=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_411(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_expr/7}).
-compile({nowarn_unused_function,  yeccgoto_expr/7}).
yeccgoto_expr(147, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_308(308, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(157, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_260(260, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(165, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_179(179, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(168, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_237(237, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(169, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_219(219, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(176, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_179(179, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(180, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_215(215, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(181, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_179(179, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(182, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_213(213, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(183, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_212(212, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(184, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_211(211, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(185, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_210(210, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(186, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_209(209, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(187, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_208(208, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(188, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_207(207, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(189, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_206(206, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_205(205, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(191, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_204(204, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(192, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_203(203, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(193, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_202(202, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(194, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_201(201, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(195, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_200(200, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(197=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_198(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(223, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_228(228, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(224, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(157, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(226, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_227(227, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(238, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_241(241, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(244, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_245(245, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(246, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_247(247, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(255, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_256(256, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(285, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_286(286, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(287, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_288(288, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(293, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_294(294, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(296, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_297(297, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(298, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_288(288, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(304, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(157, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(306, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_307(307, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(309, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_310(310, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(328, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_329(329, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(335, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_336(336, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(349, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_350(350, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(367, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_368(368, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(418, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_423(423, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(419, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(157, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(421, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_422(422, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_expr_app/7}).
-compile({nowarn_unused_function,  yeccgoto_expr_app/7}).
yeccgoto_expr_app(147, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(165, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(168, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(169, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(176, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(180, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(181, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(182, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(183, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(184, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(185, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(186, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(187, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(188, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(189, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(191, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(192, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(193, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(194, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(195, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(196, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(197, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(223, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(224, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(226, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(238, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(244, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(246, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(255, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(285, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(287, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(293, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(296, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(298, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(304, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(306, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(309, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(328, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(335, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(349, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(367, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(418, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(419, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(421, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_expr_list/7}).
-compile({nowarn_unused_function,  yeccgoto_expr_list/7}).
yeccgoto_expr_list(165, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_248(248, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_list(176=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_178(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_list(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_expr_list_opt/7}).
-compile({nowarn_unused_function,  yeccgoto_expr_list_opt/7}).
yeccgoto_expr_list_opt(176, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_177(177, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_expr_primary/7}).
-compile({nowarn_unused_function,  yeccgoto_expr_primary/7}).
yeccgoto_expr_primary(147=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_301(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(168=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(176=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(182=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(185=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(187=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(188=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(189=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(191=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(192=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(193=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(194=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(197=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(223=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(224=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(226=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(238=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(244=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(246=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(255=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(270=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_279(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(277=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_278(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(285=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(287=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(293=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(296=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(298=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(304=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(306=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(309=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(328=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(335=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(349=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(367=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(418=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(419=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(421=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_guard/7}).
-compile({nowarn_unused_function,  yeccgoto_guard/7}).
yeccgoto_guard(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(154, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_guard(224, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(154, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_guard(304, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(154, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_guard(419, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(154, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_guards/7}).
-compile({nowarn_unused_function,  yeccgoto_guards/7}).
yeccgoto_guards(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(153, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_guards(224, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_225(225, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_guards(304=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_305(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_guards(419, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_420(420, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_handler_clause/7}).
-compile({nowarn_unused_function,  yeccgoto_handler_clause/7}).
yeccgoto_handler_clause(262, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_264(264, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_handler_clause(264, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_264(264, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_handler_clauses/7}).
-compile({nowarn_unused_function,  yeccgoto_handler_clauses/7}).
yeccgoto_handler_clauses(262, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_263(263, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_handler_clauses(264=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_282(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_instance_constraints/7}).
-compile({nowarn_unused_function,  yeccgoto_instance_constraints/7}).
yeccgoto_instance_constraints(16, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_355(355, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_instance_constraints(381=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_382(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_instance_decl/7}).
-compile({nowarn_unused_function,  yeccgoto_instance_decl/7}).
yeccgoto_instance_decl(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_instance_decl(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_instance_decl(12=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_instance_method/7}).
-compile({nowarn_unused_function,  yeccgoto_instance_method/7}).
yeccgoto_instance_method(360, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_362(362, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_instance_method(369, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_362(362, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_instance_method(377, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_362(362, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_instance_methods/7}).
-compile({nowarn_unused_function,  yeccgoto_instance_methods/7}).
yeccgoto_instance_methods(360, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_361(361, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_instance_methods(369=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_370(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_instance_methods(377, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_378(378, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_instance_type_args/7}).
-compile({nowarn_unused_function,  yeccgoto_instance_type_args/7}).
yeccgoto_instance_type_args(357, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_359(359, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_instance_type_args(358=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_372(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_instance_type_args(374, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_376(376, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_instance_type_args(375=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_372(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_literal/7}).
-compile({nowarn_unused_function,  yeccgoto_literal/7}).
yeccgoto_literal(147=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(168=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(176=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(182=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(185=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(187=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(188=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(189=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(191=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(192=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(193=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(194=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(197=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(223=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(224=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(226=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(238=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(244=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(246=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(255=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(270=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(277=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(285=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(287=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(293=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(296=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(298=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(304=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(306=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(309=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(328=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(335=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(349=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(367=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(418=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(419=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(421=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_match_clause/7}).
-compile({nowarn_unused_function,  yeccgoto_match_clause/7}).
yeccgoto_match_clause(169, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_218(218, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_clause(218, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_218(218, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_clause(231, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_218(218, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_match_clauses/7}).
-compile({nowarn_unused_function,  yeccgoto_match_clauses/7}).
yeccgoto_match_clauses(169, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_217(217, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_clauses(218=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_234(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_clauses(231, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_232(232, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_maybe_trait_extends/7}).
-compile({nowarn_unused_function,  yeccgoto_maybe_trait_extends/7}).
yeccgoto_maybe_trait_extends(313, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_314(314, Cat, Ss, Stack, T, Ts, Tzr).

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
yeccgoto_operation_case(266=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_268(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operation_case(267=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_280(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_operation_cases/7}).
-compile({nowarn_unused_function,  yeccgoto_operation_cases/7}).
yeccgoto_operation_cases(266, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_267(267, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_or_pattern/7}).
-compile({nowarn_unused_function,  yeccgoto_or_pattern/7}).
yeccgoto_or_pattern(220, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(222, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_pattern(229=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_230(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pattern/7}).
-compile({nowarn_unused_function,  yeccgoto_pattern/7}).
yeccgoto_pattern(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(94, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(94, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(94, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(99, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(94, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(129, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(94, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(114, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(114, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(94, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(117, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(119, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(126, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_127(127, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(130, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(132, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(141, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(142, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(220, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(221, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(229, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(221, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(271, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_273(273, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(274, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_273(273, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(323, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(94, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(365, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(94, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(416, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(94, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pattern_list/7}).
-compile({nowarn_unused_function,  yeccgoto_pattern_list/7}).
yeccgoto_pattern_list(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(93, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list(99, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(122, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(113, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list(365, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_366(366, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list(416, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_417(417, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pattern_list_comma/7}).
-compile({nowarn_unused_function,  yeccgoto_pattern_list_comma/7}).
yeccgoto_pattern_list_comma(271, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_272(272, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list_comma(274=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_275(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pattern_list_nonempty/7}).
-compile({nowarn_unused_function,  yeccgoto_pattern_list_nonempty/7}).
yeccgoto_pattern_list_nonempty(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list_nonempty(94=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list_nonempty(99=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list_nonempty(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list_nonempty(110=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list_nonempty(114=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list_nonempty(323, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_324(324, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list_nonempty(365=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list_nonempty(416=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pattern_primary/7}).
-compile({nowarn_unused_function,  yeccgoto_pattern_primary/7}).
yeccgoto_pattern_primary(104, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_105(105, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_primary(105=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_perform_expr/7}).
-compile({nowarn_unused_function,  yeccgoto_perform_expr/7}).
yeccgoto_perform_expr(147=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(168=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(176=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(182=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(185=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(187=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(188=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(189=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(191=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(192=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(193=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(194=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(197=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(223=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(224=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(226=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(238=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(244=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(246=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(255=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(270=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(277=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(285=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(287=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(293=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(296=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(298=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(304=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(306=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(309=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(328=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(335=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(349=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(367=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(418=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(419=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(421=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_property_binding/7}).
-compile({nowarn_unused_function,  yeccgoto_property_binding/7}).
yeccgoto_property_binding(341, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_343(343, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_property_binding(347, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_343(343, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_property_bindings/7}).
-compile({nowarn_unused_function,  yeccgoto_property_bindings/7}).
yeccgoto_property_bindings(341, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_342(342, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_property_bindings(347=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_348(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_property_body/7}).
-compile({nowarn_unused_function,  yeccgoto_property_body/7}).
yeccgoto_property_body(339=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_340(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_property_decl/7}).
-compile({nowarn_unused_function,  yeccgoto_property_decl/7}).
yeccgoto_property_decl(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_property_decl(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_property_decl(12=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_record_field/7}).
-compile({nowarn_unused_function,  yeccgoto_record_field/7}).
yeccgoto_record_field(164, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_252(252, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_field(257, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_252(252, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_record_fields/7}).
-compile({nowarn_unused_function,  yeccgoto_record_fields/7}).
yeccgoto_record_fields(164, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_251(251, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_fields(257=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_record_pattern_field/7}).
-compile({nowarn_unused_function,  yeccgoto_record_pattern_field/7}).
yeccgoto_record_pattern_field(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(138, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_pattern_field(143, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(138, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_record_pattern_fields/7}).
-compile({nowarn_unused_function,  yeccgoto_record_pattern_fields/7}).
yeccgoto_record_pattern_fields(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(137, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_pattern_fields(143=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_test_decl/7}).
-compile({nowarn_unused_function,  yeccgoto_test_decl/7}).
yeccgoto_test_decl(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_test_decl(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_test_decl(12=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_trait_constraint/7}).
-compile({nowarn_unused_function,  yeccgoto_trait_constraint/7}).
yeccgoto_trait_constraint(16, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_354(354, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_trait_constraint(54, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_trait_constraint(58, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_trait_constraint(315, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_317(317, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_trait_constraint(318, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_317(317, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_trait_constraint(381, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_354(354, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_trait_decl/7}).
-compile({nowarn_unused_function,  yeccgoto_trait_decl/7}).
yeccgoto_trait_decl(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_trait_decl(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_trait_decl(12=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_trait_extends_list/7}).
-compile({nowarn_unused_function,  yeccgoto_trait_extends_list/7}).
yeccgoto_trait_extends_list(315=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_316(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_trait_extends_list(318=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_319(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_trait_member/7}).
-compile({nowarn_unused_function,  yeccgoto_trait_member/7}).
yeccgoto_trait_member(320, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_322(322, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_trait_member(330, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_322(322, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_trait_members/7}).
-compile({nowarn_unused_function,  yeccgoto_trait_members/7}).
yeccgoto_trait_members(320, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_321(321, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_trait_members(330=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_331(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_transform_clause/7}).
-compile({nowarn_unused_function,  yeccgoto_transform_clause/7}).
yeccgoto_transform_clause(2, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_414(414, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transform_clause(414, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_414(414, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_transform_clauses/7}).
-compile({nowarn_unused_function,  yeccgoto_transform_clauses/7}).
yeccgoto_transform_clauses(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_413(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transform_clauses(414=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_424(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_transform_decl/7}).
-compile({nowarn_unused_function,  yeccgoto_transform_decl/7}).
yeccgoto_transform_decl(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transform_decl(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transform_decl(12=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_transform_signature/7}).
-compile({nowarn_unused_function,  yeccgoto_transform_signature/7}).
yeccgoto_transform_signature(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transform_signature(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transform_signature(12, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_try_with_expr/7}).
-compile({nowarn_unused_function,  yeccgoto_try_with_expr/7}).
yeccgoto_try_with_expr(147=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(168=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(176=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(182=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(185=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(187=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(188=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(189=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(191=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(192=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(193=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(194=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(197=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(223=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(224=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(226=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(238=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(244=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(246=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(255=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(270=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(277=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(285=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(287=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(293=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(296=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(298=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(304=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(306=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(309=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(328=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(335=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(349=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(367=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(418=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(419=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(421=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_tuple_expr_list/7}).
-compile({nowarn_unused_function,  yeccgoto_tuple_expr_list/7}).
yeccgoto_tuple_expr_list(168, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_236(236, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple_expr_list(238=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_240(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_tuple_pattern_list/7}).
-compile({nowarn_unused_function,  yeccgoto_tuple_pattern_list/7}).
yeccgoto_tuple_pattern_list(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_128(128, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple_pattern_list(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_constraints/7}).
-compile({nowarn_unused_function,  yeccgoto_type_constraints/7}).
yeccgoto_type_constraints(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_constraints(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_decl/7}).
-compile({nowarn_unused_function,  yeccgoto_type_decl/7}).
yeccgoto_type_decl(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_decl(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_decl(12=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_expr/7}).
-compile({nowarn_unused_function,  yeccgoto_type_expr/7}).
yeccgoto_type_expr(38, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr(51=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr(61, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr(65, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr(80, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(81, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(146, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr(325, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_326(326, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr(394, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_395(395, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_expr_app/7}).
-compile({nowarn_unused_function,  yeccgoto_type_expr_app/7}).
yeccgoto_type_expr_app(16=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(38, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(51, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(61, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(65, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(80, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(315=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(318=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(325, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(381=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(394, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_expr_list/7}).
-compile({nowarn_unused_function,  yeccgoto_type_expr_list/7}).
yeccgoto_type_expr_list(61, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_list(65=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_expr_primary/7}).
-compile({nowarn_unused_function,  yeccgoto_type_expr_primary/7}).
yeccgoto_type_expr_primary(16=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(33, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(34, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(38=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(44, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(45, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(47, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(51=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(61=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(65=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(80=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(315=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(318=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(325=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(357, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_358(358, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(358, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_358(358, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(374, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_375(375, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(375, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_375(375, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(381=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(394=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_expr_primary_list/7}).
-compile({nowarn_unused_function,  yeccgoto_type_expr_primary_list/7}).
yeccgoto_type_expr_primary_list(44=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary_list(45=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary_list(47=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary_list(357=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary_list(358=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_params/7}).
-compile({nowarn_unused_function,  yeccgoto_type_params/7}).
yeccgoto_type_params(24, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(26, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_params(43, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(50, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_params(312, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_313(313, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_params_nonempty/7}).
-compile({nowarn_unused_function,  yeccgoto_type_params_nonempty/7}).
yeccgoto_type_params_nonempty(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_params_nonempty(27=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_params_nonempty(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_params_nonempty(312=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_record_field/7}).
-compile({nowarn_unused_function,  yeccgoto_type_record_field/7}).
yeccgoto_type_record_field(36, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(77, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_record_field(82, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(77, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_record_fields/7}).
-compile({nowarn_unused_function,  yeccgoto_type_record_fields/7}).
yeccgoto_type_record_fields(36, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_record_fields(82=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_1_/1}).
-dialyzer({nowarn_function, yeccpars2_1_/1}).
-compile({nowarn_unused_function,  yeccpars2_1_/1}).
-file("src/compiler/parser/catena_parser.yrl", 250).
yeccpars2_1_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                           ___1
  end | __Stack].

-compile({inline,yeccpars2_2_/1}).
-dialyzer({nowarn_function, yeccpars2_2_/1}).
-compile({nowarn_unused_function,  yeccpars2_2_/1}).
-file("src/compiler/parser/catena_parser.yrl", 538).
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
-file("src/compiler/parser/catena_parser.yrl", 251).
yeccpars2_3_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                ___1
  end | __Stack].

-compile({inline,yeccpars2_4_/1}).
-dialyzer({nowarn_function, yeccpars2_4_/1}).
-compile({nowarn_unused_function,  yeccpars2_4_/1}).
-file("src/compiler/parser/catena_parser.yrl", 253).
yeccpars2_4_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                            ___1
  end | __Stack].

-compile({inline,yeccpars2_5_/1}).
-dialyzer({nowarn_function, yeccpars2_5_/1}).
-compile({nowarn_unused_function,  yeccpars2_5_/1}).
-file("src/compiler/parser/catena_parser.yrl", 255).
yeccpars2_5_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                           ___1
  end | __Stack].

-compile({inline,yeccpars2_6_/1}).
-dialyzer({nowarn_function, yeccpars2_6_/1}).
-compile({nowarn_unused_function,  yeccpars2_6_/1}).
-file("src/compiler/parser/catena_parser.yrl", 256).
yeccpars2_6_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               ___1
  end | __Stack].

-compile({inline,yeccpars2_8_/1}).
-dialyzer({nowarn_function, yeccpars2_8_/1}).
-compile({nowarn_unused_function,  yeccpars2_8_/1}).
-file("src/compiler/parser/catena_parser.yrl", 210).
yeccpars2_8_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                              
    {module_header, element(2, ___1), element(3, ___1), element(4, ___1)}
  end | __Stack].

-compile({inline,yeccpars2_9_/1}).
-dialyzer({nowarn_function, yeccpars2_9_/1}).
-compile({nowarn_unused_function,  yeccpars2_9_/1}).
-file("src/compiler/parser/catena_parser.yrl", 254).
yeccpars2_9_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               ___1
  end | __Stack].

-compile({inline,yeccpars2_10_/1}).
-dialyzer({nowarn_function, yeccpars2_10_/1}).
-compile({nowarn_unused_function,  yeccpars2_10_/1}).
-file("src/compiler/parser/catena_parser.yrl", 252).
yeccpars2_10_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             ___1
  end | __Stack].

-compile({inline,yeccpars2_11_/1}).
-dialyzer({nowarn_function, yeccpars2_11_/1}).
-compile({nowarn_unused_function,  yeccpars2_11_/1}).
-file("src/compiler/parser/catena_parser.yrl", 206).
yeccpars2_11_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               
    {module, undefined, [], [], ___1, {line, 1}}
  end | __Stack].

-compile({inline,yeccpars2_12_/1}).
-dialyzer({nowarn_function, yeccpars2_12_/1}).
-compile({nowarn_unused_function,  yeccpars2_12_/1}).
-file("src/compiler/parser/catena_parser.yrl", 245).
yeccpars2_12_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_23_/1}).
-dialyzer({nowarn_function, yeccpars2_23_/1}).
-compile({nowarn_unused_function,  yeccpars2_23_/1}).
-file("src/compiler/parser/catena_parser.yrl", 287).
yeccpars2_23_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                         
    make_error_declaration(extract_location(___1), "Incomplete type declaration", ___2)
  end | __Stack].

-compile({inline,yeccpars2_24_/1}).
-dialyzer({nowarn_function, yeccpars2_24_/1}).
-compile({nowarn_unused_function,  yeccpars2_24_/1}).
-file("src/compiler/parser/catena_parser.yrl", 290).
yeccpars2_24_(__Stack0) ->
 [begin
                         
    []
  end | __Stack0].

-compile({inline,yeccpars2_25_/1}).
-dialyzer({nowarn_function, yeccpars2_25_/1}).
-compile({nowarn_unused_function,  yeccpars2_25_/1}).
-file("src/compiler/parser/catena_parser.yrl", 292).
yeccpars2_25_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                     
    ___1
  end | __Stack].

-compile({inline,yeccpars2_27_/1}).
-dialyzer({nowarn_function, yeccpars2_27_/1}).
-compile({nowarn_unused_function,  yeccpars2_27_/1}).
-file("src/compiler/parser/catena_parser.yrl", 295).
yeccpars2_27_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                     
    [extract_atom(___1)]
  end | __Stack].

-compile({inline,yeccpars2_28_/1}).
-dialyzer({nowarn_function, yeccpars2_28_/1}).
-compile({nowarn_unused_function,  yeccpars2_28_/1}).
-file("src/compiler/parser/catena_parser.yrl", 297).
yeccpars2_28_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                          
    [extract_atom(___1) | ___2]
  end | __Stack].

-compile({inline,yeccpars2_30_/1}).
-dialyzer({nowarn_function, yeccpars2_30_/1}).
-compile({nowarn_unused_function,  yeccpars2_30_/1}).
-file("src/compiler/parser/catena_parser.yrl", 285).
yeccpars2_30_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                 
    make_error_declaration(extract_location(___1), "Missing '=' or constructors in type declaration", ___4)
  end | __Stack].

-compile({inline,yeccpars2_31_/1}).
-dialyzer({nowarn_function, yeccpars2_31_/1}).
-compile({nowarn_unused_function,  yeccpars2_31_/1}).
-file("src/compiler/parser/catena_parser.yrl", 274).
yeccpars2_31_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                               
    {type_decl,
        extract_atom(___2),
        ___3,
        ___5,
        [],
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_32_/1}).
-dialyzer({nowarn_function, yeccpars2_32_/1}).
-compile({nowarn_unused_function,  yeccpars2_32_/1}).
-file("src/compiler/parser/catena_parser.yrl", 300).
yeccpars2_32_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_33_/1}).
-dialyzer({nowarn_function, yeccpars2_33_/1}).
-compile({nowarn_unused_function,  yeccpars2_33_/1}).
-file("src/compiler/parser/catena_parser.yrl", 305).
yeccpars2_33_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                            
    {constructor,
        extract_atom(___1),
        [],
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_34_/1}).
-dialyzer({nowarn_function, yeccpars2_34_/1}).
-compile({nowarn_unused_function,  yeccpars2_34_/1}).
-file("src/compiler/parser/catena_parser.yrl", 317).
yeccpars2_34_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                         
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_35_/1}).
-dialyzer({nowarn_function, yeccpars2_35_/1}).
-compile({nowarn_unused_function,  yeccpars2_35_/1}).
-file("src/compiler/parser/catena_parser.yrl", 311).
yeccpars2_35_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                               
    {constructor,
        extract_atom(___1),
        ___2,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_37_/1}).
-dialyzer({nowarn_function, yeccpars2_37_/1}).
-compile({nowarn_unused_function,  yeccpars2_37_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1076).
yeccpars2_37_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                  
    {type_var, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_39_/1}).
-dialyzer({nowarn_function, yeccpars2_39_/1}).
-compile({nowarn_unused_function,  yeccpars2_39_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1079).
yeccpars2_39_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                  
    {type_con, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_40_/1}).
-dialyzer({nowarn_function, yeccpars2_40_/1}).
-compile({nowarn_unused_function,  yeccpars2_40_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1065).
yeccpars2_40_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                    
    ___1
  end | __Stack].

-compile({inline,yeccpars2_41_/1}).
-dialyzer({nowarn_function, yeccpars2_41_/1}).
-compile({nowarn_unused_function,  yeccpars2_41_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1039).
yeccpars2_41_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             ___1
  end | __Stack].

-compile({inline,yeccpars2_43_/1}).
-dialyzer({nowarn_function, yeccpars2_43_/1}).
-compile({nowarn_unused_function,  yeccpars2_43_/1}).
-file("src/compiler/parser/catena_parser.yrl", 290).
yeccpars2_43_(__Stack0) ->
 [begin
                         
    []
  end | __Stack0].

-compile({inline,yeccpars2_44_/1}).
-dialyzer({nowarn_function, yeccpars2_44_/1}).
-compile({nowarn_unused_function,  yeccpars2_44_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1076).
yeccpars2_44_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                  
    {type_var, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_45_/1}).
-dialyzer({nowarn_function, yeccpars2_45_/1}).
-compile({nowarn_unused_function,  yeccpars2_45_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1079).
yeccpars2_45_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                  
    {type_con, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_46_/1}).
-dialyzer({nowarn_function, yeccpars2_46_/1}).
-compile({nowarn_unused_function,  yeccpars2_46_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1052).
yeccpars2_46_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                     
    {type_app,
        {type_con, extract_atom(___1), extract_location(___1)},
        ___2,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_47_/1}).
-dialyzer({nowarn_function, yeccpars2_47_/1}).
-compile({nowarn_unused_function,  yeccpars2_47_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1070).
yeccpars2_47_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                             
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_48_/1}).
-dialyzer({nowarn_function, yeccpars2_48_/1}).
-compile({nowarn_unused_function,  yeccpars2_48_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1072).
yeccpars2_48_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                                    
    [___1 | ___2]
  end | __Stack].

-compile({inline,yeccpars2_49_/1}).
-dialyzer({nowarn_function, yeccpars2_49_/1}).
-compile({nowarn_unused_function,  yeccpars2_49_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1059).
yeccpars2_49_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                     
    {type_app,
        {type_var, extract_atom(___1), extract_location(___1)},
        ___2,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_52_/1}).
-dialyzer({nowarn_function, yeccpars2_52_/1}).
-compile({nowarn_unused_function,  yeccpars2_52_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1010).
yeccpars2_52_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                               
    {type_forall, ___2, ___4, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_55_/1}).
-dialyzer({nowarn_function, yeccpars2_55_/1}).
-compile({nowarn_unused_function,  yeccpars2_55_/1}).
-file("src/compiler/parser/catena_parser.yrl", 383).
yeccpars2_55_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                   
    extract_trait_constraint(___1)
  end | __Stack].

-compile({inline,yeccpars2_56_/1}).
-dialyzer({nowarn_function, yeccpars2_56_/1}).
-compile({nowarn_unused_function,  yeccpars2_56_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1036).
yeccpars2_56_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    {constrained_type, ___3, ___1, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_57_/1}).
-dialyzer({nowarn_function, yeccpars2_57_/1}).
-compile({nowarn_unused_function,  yeccpars2_57_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1042).
yeccpars2_57_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                      
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_59_/1}).
-dialyzer({nowarn_function, yeccpars2_59_/1}).
-compile({nowarn_unused_function,  yeccpars2_59_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1044).
yeccpars2_59_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                 
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_60_/1}).
-dialyzer({nowarn_function, yeccpars2_60_/1}).
-compile({nowarn_unused_function,  yeccpars2_60_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1007).
yeccpars2_60_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                        
    {type_fun, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_62_/1}).
-dialyzer({nowarn_function, yeccpars2_62_/1}).
-compile({nowarn_unused_function,  yeccpars2_62_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1083).
yeccpars2_62_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                              
    ___2
  end | __Stack].

-compile({inline,yeccpars2_64_/1}).
-dialyzer({nowarn_function, yeccpars2_64_/1}).
-compile({nowarn_unused_function,  yeccpars2_64_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1098).
yeccpars2_64_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_66_/1}).
-dialyzer({nowarn_function, yeccpars2_66_/1}).
-compile({nowarn_unused_function,  yeccpars2_66_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1100).
yeccpars2_66_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                  
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_67_/1}).
-dialyzer({nowarn_function, yeccpars2_67_/1}).
-compile({nowarn_unused_function,  yeccpars2_67_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1088).
yeccpars2_67_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                   
    {type_tuple, [___2 | ___4], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_71_/1}).
-dialyzer({nowarn_function, yeccpars2_71_/1}).
-compile({nowarn_unused_function,  yeccpars2_71_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1029).
yeccpars2_71_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                
    {type_effect, ___1, [], extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_72_/1}).
-dialyzer({nowarn_function, yeccpars2_72_/1}).
-compile({nowarn_unused_function,  yeccpars2_72_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1124).
yeccpars2_72_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                     
    [extract_atom(___1)]
  end | __Stack].

-compile({inline,yeccpars2_74_/1}).
-dialyzer({nowarn_function, yeccpars2_74_/1}).
-compile({nowarn_unused_function,  yeccpars2_74_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1126).
yeccpars2_74_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                
    [extract_atom(___1) | ___3]
  end | __Stack].

-compile({inline,yeccpars2_75_/1}).
-dialyzer({nowarn_function, yeccpars2_75_/1}).
-compile({nowarn_unused_function,  yeccpars2_75_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1032).
yeccpars2_75_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                     
    {type_effect, ___1, ___4, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_77_/1}).
-dialyzer({nowarn_function, yeccpars2_77_/1}).
-compile({nowarn_unused_function,  yeccpars2_77_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1110).
yeccpars2_77_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                         
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_79_/1}).
-dialyzer({nowarn_function, yeccpars2_79_/1}).
-compile({nowarn_unused_function,  yeccpars2_79_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1091).
yeccpars2_79_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                    
    {type_record, [], undefined, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_81_/1}).
-dialyzer({nowarn_function, yeccpars2_81_/1}).
-compile({nowarn_unused_function,  yeccpars2_81_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1115).
yeccpars2_81_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                  
    {extract_atom(___1), ___3}
  end | __Stack].

-compile({inline,yeccpars2_83_/1}).
-dialyzer({nowarn_function, yeccpars2_83_/1}).
-compile({nowarn_unused_function,  yeccpars2_83_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1112).
yeccpars2_83_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                  
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_84_/1}).
-dialyzer({nowarn_function, yeccpars2_84_/1}).
-compile({nowarn_unused_function,  yeccpars2_84_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1094).
yeccpars2_84_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                       
    {type_record, ___2, undefined, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_85_/1}).
-dialyzer({nowarn_function, yeccpars2_85_/1}).
-compile({nowarn_unused_function,  yeccpars2_85_/1}).
-file("src/compiler/parser/catena_parser.yrl", 319).
yeccpars2_85_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                            
    [___1 | ___2]
  end | __Stack].

-compile({inline,yeccpars2_87_/1}).
-dialyzer({nowarn_function, yeccpars2_87_/1}).
-compile({nowarn_unused_function,  yeccpars2_87_/1}).
-file("src/compiler/parser/catena_parser.yrl", 302).
yeccpars2_87_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                               
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_89_/1}).
-dialyzer({nowarn_function, yeccpars2_89_/1}).
-compile({nowarn_unused_function,  yeccpars2_89_/1}).
-file("src/compiler/parser/catena_parser.yrl", 283).
yeccpars2_89_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    make_error_declaration(extract_location(___1), "Invalid type name", ___2)
  end | __Stack].

-compile({inline,yeccpars2_90_/1}).
-dialyzer({nowarn_function, yeccpars2_90_/1}).
-compile({nowarn_unused_function,  yeccpars2_90_/1}).
-file("src/compiler/parser/catena_parser.yrl", 565).
yeccpars2_90_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                   
    make_error_declaration(extract_location(___1), "Incomplete transform declaration", ___2)
  end | __Stack].

-compile({inline,yeccpars2_91_/1}).
-dialyzer({nowarn_function, yeccpars2_91_/1}).
-compile({nowarn_unused_function,  yeccpars2_91_/1}).
-file("src/compiler/parser/catena_parser.yrl", 632).
yeccpars2_91_(__Stack0) ->
 [begin
                          
    []
  end | __Stack0].

-compile({inline,yeccpars2_92_/1}).
-dialyzer({nowarn_function, yeccpars2_92_/1}).
-compile({nowarn_unused_function,  yeccpars2_92_/1}).
-file("src/compiler/parser/catena_parser.yrl", 634).
yeccpars2_92_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                       
    ___1
  end | __Stack].

-compile({inline,yeccpars2_94_/1}).
-dialyzer({nowarn_function, yeccpars2_94_/1}).
-compile({nowarn_unused_function,  yeccpars2_94_/1}).
-file("src/compiler/parser/catena_parser.yrl", 637).
yeccpars2_94_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                  
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_96_/1}).
-dialyzer({nowarn_function, yeccpars2_96_/1}).
-compile({nowarn_unused_function,  yeccpars2_96_/1}).
-file("src/compiler/parser/catena_parser.yrl", 697).
yeccpars2_96_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                  
    {pat_literal, extract_value(___1), float, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_97_/1}).
-dialyzer({nowarn_function, yeccpars2_97_/1}).
-compile({nowarn_unused_function,  yeccpars2_97_/1}).
-file("src/compiler/parser/catena_parser.yrl", 694).
yeccpars2_97_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                    
    {pat_literal, extract_value(___1), integer, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_100_/1}).
-dialyzer({nowarn_function, yeccpars2_100_/1}).
-compile({nowarn_unused_function,  yeccpars2_100_/1}).
-file("src/compiler/parser/catena_parser.yrl", 650).
yeccpars2_100_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                        
    {pat_var, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_102_/1}).
-dialyzer({nowarn_function, yeccpars2_102_/1}).
-compile({nowarn_unused_function,  yeccpars2_102_/1}).
-file("src/compiler/parser/catena_parser.yrl", 700).
yeccpars2_102_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                   
    {pat_literal, extract_value(___1), string, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_103_/1}).
-dialyzer({nowarn_function, yeccpars2_103_/1}).
-compile({nowarn_unused_function,  yeccpars2_103_/1}).
-file("src/compiler/parser/catena_parser.yrl", 653).
yeccpars2_103_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                       
    {pat_wildcard, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_104_/1}).
-dialyzer({nowarn_function, yeccpars2_104_/1}).
-compile({nowarn_unused_function,  yeccpars2_104_/1}).
-file("src/compiler/parser/catena_parser.yrl", 656).
yeccpars2_104_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                        
    {pat_constructor, extract_atom(___1), [], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_105_/1}).
-dialyzer({nowarn_function, yeccpars2_105_/1}).
-compile({nowarn_unused_function,  yeccpars2_105_/1}).
-file("src/compiler/parser/catena_parser.yrl", 663).
yeccpars2_105_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                        
    {pat_constructor, extract_atom(___1), [___2], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_106_/1}).
-dialyzer({nowarn_function, yeccpars2_106_/1}).
-compile({nowarn_unused_function,  yeccpars2_106_/1}).
-file("src/compiler/parser/catena_parser.yrl", 680).
yeccpars2_106_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                          
    {pat_literal, extract_value(___1), float, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_107_/1}).
-dialyzer({nowarn_function, yeccpars2_107_/1}).
-compile({nowarn_unused_function,  yeccpars2_107_/1}).
-file("src/compiler/parser/catena_parser.yrl", 677).
yeccpars2_107_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                            
    {pat_literal, extract_value(___1), integer, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_109_/1}).
-dialyzer({nowarn_function, yeccpars2_109_/1}).
-compile({nowarn_unused_function,  yeccpars2_109_/1}).
-file("src/compiler/parser/catena_parser.yrl", 671).
yeccpars2_109_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                
    {pat_var, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_110_/1}).
-dialyzer({nowarn_function, yeccpars2_110_/1}).
-compile({nowarn_unused_function,  yeccpars2_110_/1}).
-file("src/compiler/parser/catena_parser.yrl", 632).
yeccpars2_110_(__Stack0) ->
 [begin
                          
    []
  end | __Stack0].

-compile({inline,yeccpars2_111_/1}).
-dialyzer({nowarn_function, yeccpars2_111_/1}).
-compile({nowarn_unused_function,  yeccpars2_111_/1}).
-file("src/compiler/parser/catena_parser.yrl", 683).
yeccpars2_111_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                           
    {pat_literal, extract_value(___1), string, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_112_/1}).
-dialyzer({nowarn_function, yeccpars2_112_/1}).
-compile({nowarn_unused_function,  yeccpars2_112_/1}).
-file("src/compiler/parser/catena_parser.yrl", 674).
yeccpars2_112_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               
    {pat_wildcard, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_115_/1}).
-dialyzer({nowarn_function, yeccpars2_115_/1}).
-compile({nowarn_unused_function,  yeccpars2_115_/1}).
-file("src/compiler/parser/catena_parser.yrl", 639).
yeccpars2_115_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                        
    [___1 | ___2]
  end | __Stack].

-compile({inline,yeccpars2_118_/1}).
-dialyzer({nowarn_function, yeccpars2_118_/1}).
-compile({nowarn_unused_function,  yeccpars2_118_/1}).
-file("src/compiler/parser/catena_parser.yrl", 686).
yeccpars2_118_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                           ___2
  end | __Stack].

-compile({inline,yeccpars2_119_/1}).
-dialyzer({nowarn_function, yeccpars2_119_/1}).
-compile({nowarn_unused_function,  yeccpars2_119_/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_119_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                 
    {pat_cons, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_120_/1}).
-dialyzer({nowarn_function, yeccpars2_120_/1}).
-compile({nowarn_unused_function,  yeccpars2_120_/1}).
-file("src/compiler/parser/catena_parser.yrl", 724).
yeccpars2_120_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                   
    {pat_as, extract_atom(___3), ___1, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_121_/1}).
-dialyzer({nowarn_function, yeccpars2_121_/1}).
-compile({nowarn_unused_function,  yeccpars2_121_/1}).
-file("src/compiler/parser/catena_parser.yrl", 659).
yeccpars2_121_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    {pat_constructor, extract_atom(___1), ___3, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_123_/1}).
-dialyzer({nowarn_function, yeccpars2_123_/1}).
-compile({nowarn_unused_function,  yeccpars2_123_/1}).
-file("src/compiler/parser/catena_parser.yrl", 688).
yeccpars2_123_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                      
    {pat_list, [], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_124_/1}).
-dialyzer({nowarn_function, yeccpars2_124_/1}).
-compile({nowarn_unused_function,  yeccpars2_124_/1}).
-file("src/compiler/parser/catena_parser.yrl", 691).
yeccpars2_124_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    {pat_list, ___2, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_125_/1}).
-dialyzer({nowarn_function, yeccpars2_125_/1}).
-compile({nowarn_unused_function,  yeccpars2_125_/1}).
-file("src/compiler/parser/catena_parser.yrl", 667).
yeccpars2_125_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                        
    {pat_constructor, extract_atom(___1), [___2, ___3], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_131_/1}).
-dialyzer({nowarn_function, yeccpars2_131_/1}).
-compile({nowarn_unused_function,  yeccpars2_131_/1}).
-file("src/compiler/parser/catena_parser.yrl", 730).
yeccpars2_131_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                        
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_132_/1}).
-dialyzer({nowarn_function, yeccpars2_132_/1}).
-compile({nowarn_unused_function,  yeccpars2_132_/1}).
-file("src/compiler/parser/catena_parser.yrl", 728).
yeccpars2_132_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    [___1, ___3]
  end | __Stack].

-compile({inline,yeccpars2_133_/1}).
-dialyzer({nowarn_function, yeccpars2_133_/1}).
-compile({nowarn_unused_function,  yeccpars2_133_/1}).
-file("src/compiler/parser/catena_parser.yrl", 709).
yeccpars2_133_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    {pat_tuple, ___2, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_135_/1}).
-dialyzer({nowarn_function, yeccpars2_135_/1}).
-compile({nowarn_unused_function,  yeccpars2_135_/1}).
-file("src/compiler/parser/catena_parser.yrl", 703).
yeccpars2_135_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {pat_list, [], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_136_/1}).
-dialyzer({nowarn_function, yeccpars2_136_/1}).
-compile({nowarn_unused_function,  yeccpars2_136_/1}).
-file("src/compiler/parser/catena_parser.yrl", 706).
yeccpars2_136_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                           
    {pat_list, ___2, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_138_/1}).
-dialyzer({nowarn_function, yeccpars2_138_/1}).
-compile({nowarn_unused_function,  yeccpars2_138_/1}).
-file("src/compiler/parser/catena_parser.yrl", 734).
yeccpars2_138_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                               
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_140_/1}).
-dialyzer({nowarn_function, yeccpars2_140_/1}).
-compile({nowarn_unused_function,  yeccpars2_140_/1}).
-file("src/compiler/parser/catena_parser.yrl", 712).
yeccpars2_140_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                          
    {pat_record, [], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_142_/1}).
-dialyzer({nowarn_function, yeccpars2_142_/1}).
-compile({nowarn_unused_function,  yeccpars2_142_/1}).
-file("src/compiler/parser/catena_parser.yrl", 739).
yeccpars2_142_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    {extract_atom(___1), ___3}
  end | __Stack].

-compile({inline,yeccpars2_144_/1}).
-dialyzer({nowarn_function, yeccpars2_144_/1}).
-compile({nowarn_unused_function,  yeccpars2_144_/1}).
-file("src/compiler/parser/catena_parser.yrl", 736).
yeccpars2_144_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                           
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_145_/1}).
-dialyzer({nowarn_function, yeccpars2_145_/1}).
-compile({nowarn_unused_function,  yeccpars2_145_/1}).
-file("src/compiler/parser/catena_parser.yrl", 715).
yeccpars2_145_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                
    {pat_record, ___2, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_146_/1}).
-dialyzer({nowarn_function, yeccpars2_146_/1}).
-compile({nowarn_unused_function,  yeccpars2_146_/1}).
-file("src/compiler/parser/catena_parser.yrl", 570).
yeccpars2_146_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                              
    {transform_sig, extract_atom(___2), ___4, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_148_/1}).
-dialyzer({nowarn_function, yeccpars2_148_/1}).
-compile({nowarn_unused_function,  yeccpars2_148_/1}).
-file("src/compiler/parser/catena_parser.yrl", 563).
yeccpars2_148_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                            
    make_error_declaration(extract_location(___1), "Missing '=' or expression in transform declaration", ___4)
  end | __Stack].

-compile({inline,yeccpars2_150_/1}).
-dialyzer({nowarn_function, yeccpars2_150_/1}).
-compile({nowarn_unused_function,  yeccpars2_150_/1}).
-file("src/compiler/parser/catena_parser.yrl", 859).
yeccpars2_150_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                ___1
  end | __Stack].

-compile({inline,yeccpars2_151_/1}).
-dialyzer({nowarn_function, yeccpars2_151_/1}).
-compile({nowarn_unused_function,  yeccpars2_151_/1}).
-file("src/compiler/parser/catena_parser.yrl", 857).
yeccpars2_151_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               ___1
  end | __Stack].

-compile({inline,yeccpars2_152_/1}).
-dialyzer({nowarn_function, yeccpars2_152_/1}).
-compile({nowarn_unused_function,  yeccpars2_152_/1}).
-file("src/compiler/parser/catena_parser.yrl", 825).
yeccpars2_152_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                          ___1
  end | __Stack].

-compile({inline,yeccpars2_154_/1}).
-dialyzer({nowarn_function, yeccpars2_154_/1}).
-compile({nowarn_unused_function,  yeccpars2_154_/1}).
-file("src/compiler/parser/catena_parser.yrl", 746).
yeccpars2_154_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                 
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_155_/1}).
-dialyzer({nowarn_function, yeccpars2_155_/1}).
-compile({nowarn_unused_function,  yeccpars2_155_/1}).
-file("src/compiler/parser/catena_parser.yrl", 822).
yeccpars2_155_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                           ___1
  end | __Stack].

-compile({inline,yeccpars2_156_/1}).
-dialyzer({nowarn_function, yeccpars2_156_/1}).
-compile({nowarn_unused_function,  yeccpars2_156_/1}).
-file("src/compiler/parser/catena_parser.yrl", 813).
yeccpars2_156_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                   ___1
  end | __Stack].

-compile({inline,yeccpars2_157_/1}).
-dialyzer({nowarn_function, yeccpars2_157_/1}).
-compile({nowarn_unused_function,  yeccpars2_157_/1}).
-file("src/compiler/parser/catena_parser.yrl", 751).
yeccpars2_157_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                ___1
  end | __Stack].

-compile({inline,yeccpars2_158_/1}).
-dialyzer({nowarn_function, yeccpars2_158_/1}).
-compile({nowarn_unused_function,  yeccpars2_158_/1}).
-file("src/compiler/parser/catena_parser.yrl", 861).
yeccpars2_158_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                          ___1
  end | __Stack].

-compile({inline,yeccpars2_160_/1}).
-dialyzer({nowarn_function, yeccpars2_160_/1}).
-compile({nowarn_unused_function,  yeccpars2_160_/1}).
-file("src/compiler/parser/catena_parser.yrl", 912).
yeccpars2_160_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                  
    {literal, extract_value(___1), float, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_163_/1}).
-dialyzer({nowarn_function, yeccpars2_163_/1}).
-compile({nowarn_unused_function,  yeccpars2_163_/1}).
-file("src/compiler/parser/catena_parser.yrl", 909).
yeccpars2_163_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                    
    {literal, extract_value(___1), integer, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_167_/1}).
-dialyzer({nowarn_function, yeccpars2_167_/1}).
-compile({nowarn_unused_function,  yeccpars2_167_/1}).
-file("src/compiler/parser/catena_parser.yrl", 827).
yeccpars2_167_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             
    {var, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_171_/1}).
-dialyzer({nowarn_function, yeccpars2_171_/1}).
-compile({nowarn_unused_function,  yeccpars2_171_/1}).
-file("src/compiler/parser/catena_parser.yrl", 915).
yeccpars2_171_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                   
    {literal, extract_value(___1), string, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_172_/1}).
-dialyzer({nowarn_function, yeccpars2_172_/1}).
-compile({nowarn_unused_function,  yeccpars2_172_/1}).
-file("src/compiler/parser/catena_parser.yrl", 830).
yeccpars2_172_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             
    {var, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_176_/1}).
-dialyzer({nowarn_function, yeccpars2_176_/1}).
-compile({nowarn_unused_function,  yeccpars2_176_/1}).
-file("src/compiler/parser/catena_parser.yrl", 900).
yeccpars2_176_(__Stack0) ->
 [begin
                           
    []
  end | __Stack0].

-compile({inline,yeccpars2_178_/1}).
-dialyzer({nowarn_function, yeccpars2_178_/1}).
-compile({nowarn_unused_function,  yeccpars2_178_/1}).
-file("src/compiler/parser/catena_parser.yrl", 902).
yeccpars2_178_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                            
    ___1
  end | __Stack].

-compile({inline,yeccpars2_179_/1}).
-dialyzer({nowarn_function, yeccpars2_179_/1}).
-compile({nowarn_unused_function,  yeccpars2_179_/1}).
-file("src/compiler/parser/catena_parser.yrl", 894).
yeccpars2_179_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                   
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_198_/1}).
-dialyzer({nowarn_function, yeccpars2_198_/1}).
-compile({nowarn_unused_function,  yeccpars2_198_/1}).
-file("src/compiler/parser/catena_parser.yrl", 768).
yeccpars2_198_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                        
    {binary_op, star, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_199_/1}).
-dialyzer({nowarn_function, yeccpars2_199_/1}).
-compile({nowarn_unused_function,  yeccpars2_199_/1}).
-file("src/compiler/parser/catena_parser.yrl", 771).
yeccpars2_199_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                         
    {binary_op, slash, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,'yeccpars2_200_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_200_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_200_$end'/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
'yeccpars2_200_$end'(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_and/1}).
-dialyzer({nowarn_function, yeccpars2_200_and/1}).
-compile({nowarn_unused_function,  yeccpars2_200_and/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_and(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_200_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_200_arrow/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_arrow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_comma/1}).
-dialyzer({nowarn_function, yeccpars2_200_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_200_comma/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_comma(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_cons/1}).
-dialyzer({nowarn_function, yeccpars2_200_cons/1}).
-compile({nowarn_unused_function,  yeccpars2_200_cons/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_cons(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_do/1}).
-dialyzer({nowarn_function, yeccpars2_200_do/1}).
-compile({nowarn_unused_function,  yeccpars2_200_do/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_do(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_dot/1}).
-dialyzer({nowarn_function, yeccpars2_200_dot/1}).
-compile({nowarn_unused_function,  yeccpars2_200_dot/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_dot(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_effect/1}).
-dialyzer({nowarn_function, yeccpars2_200_effect/1}).
-compile({nowarn_unused_function,  yeccpars2_200_effect/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_effect(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_end/1}).
-dialyzer({nowarn_function, yeccpars2_200_end/1}).
-compile({nowarn_unused_function,  yeccpars2_200_end/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_end(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_equals/1}).
-dialyzer({nowarn_function, yeccpars2_200_equals/1}).
-compile({nowarn_unused_function,  yeccpars2_200_equals/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_equals(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_error/1}).
-dialyzer({nowarn_function, yeccpars2_200_error/1}).
-compile({nowarn_unused_function,  yeccpars2_200_error/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_error(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_float/1}).
-dialyzer({nowarn_function, yeccpars2_200_float/1}).
-compile({nowarn_unused_function,  yeccpars2_200_float/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_float(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_fn/1}).
-dialyzer({nowarn_function, yeccpars2_200_fn/1}).
-compile({nowarn_unused_function,  yeccpars2_200_fn/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_fn(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_handle/1}).
-dialyzer({nowarn_function, yeccpars2_200_handle/1}).
-compile({nowarn_unused_function,  yeccpars2_200_handle/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_handle(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_in/1}).
-dialyzer({nowarn_function, yeccpars2_200_in/1}).
-compile({nowarn_unused_function,  yeccpars2_200_in/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_in(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_instance/1}).
-dialyzer({nowarn_function, yeccpars2_200_instance/1}).
-compile({nowarn_unused_function,  yeccpars2_200_instance/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_instance(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_integer/1}).
-dialyzer({nowarn_function, yeccpars2_200_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_200_integer/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_integer(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_lbrace/1}).
-dialyzer({nowarn_function, yeccpars2_200_lbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_200_lbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_lbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_lbracket/1}).
-dialyzer({nowarn_function, yeccpars2_200_lbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_200_lbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_lbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_let/1}).
-dialyzer({nowarn_function, yeccpars2_200_let/1}).
-compile({nowarn_unused_function,  yeccpars2_200_let/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_let(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_lower_ident/1}).
-dialyzer({nowarn_function, yeccpars2_200_lower_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_200_lower_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_lower_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_lparen/1}).
-dialyzer({nowarn_function, yeccpars2_200_lparen/1}).
-compile({nowarn_unused_function,  yeccpars2_200_lparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_lparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_match/1}).
-dialyzer({nowarn_function, yeccpars2_200_match/1}).
-compile({nowarn_unused_function,  yeccpars2_200_match/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_match(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_of/1}).
-dialyzer({nowarn_function, yeccpars2_200_of/1}).
-compile({nowarn_unused_function,  yeccpars2_200_of/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_of(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_or/1}).
-dialyzer({nowarn_function, yeccpars2_200_or/1}).
-compile({nowarn_unused_function,  yeccpars2_200_or/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_or(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_perform/1}).
-dialyzer({nowarn_function, yeccpars2_200_perform/1}).
-compile({nowarn_unused_function,  yeccpars2_200_perform/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_perform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_pipe/1}).
-dialyzer({nowarn_function, yeccpars2_200_pipe/1}).
-compile({nowarn_unused_function,  yeccpars2_200_pipe/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_pipe(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_pipe_right/1}).
-dialyzer({nowarn_function, yeccpars2_200_pipe_right/1}).
-compile({nowarn_unused_function,  yeccpars2_200_pipe_right/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_pipe_right(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_property/1}).
-dialyzer({nowarn_function, yeccpars2_200_property/1}).
-compile({nowarn_unused_function,  yeccpars2_200_property/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_property(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_200_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_200_rbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_rbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_rbracket/1}).
-dialyzer({nowarn_function, yeccpars2_200_rbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_200_rbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_rbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_rparen/1}).
-dialyzer({nowarn_function, yeccpars2_200_rparen/1}).
-compile({nowarn_unused_function,  yeccpars2_200_rparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_rparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_semicolon/1}).
-dialyzer({nowarn_function, yeccpars2_200_semicolon/1}).
-compile({nowarn_unused_function,  yeccpars2_200_semicolon/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_semicolon(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_string/1}).
-dialyzer({nowarn_function, yeccpars2_200_string/1}).
-compile({nowarn_unused_function,  yeccpars2_200_string/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_string(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_test/1}).
-dialyzer({nowarn_function, yeccpars2_200_test/1}).
-compile({nowarn_unused_function,  yeccpars2_200_test/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_test(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_then/1}).
-dialyzer({nowarn_function, yeccpars2_200_then/1}).
-compile({nowarn_unused_function,  yeccpars2_200_then/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_then(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_trait/1}).
-dialyzer({nowarn_function, yeccpars2_200_trait/1}).
-compile({nowarn_unused_function,  yeccpars2_200_trait/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_trait(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_transform/1}).
-dialyzer({nowarn_function, yeccpars2_200_transform/1}).
-compile({nowarn_unused_function,  yeccpars2_200_transform/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_transform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_type/1}).
-dialyzer({nowarn_function, yeccpars2_200_type/1}).
-compile({nowarn_unused_function,  yeccpars2_200_type/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_type(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_upper_ident/1}).
-dialyzer({nowarn_function, yeccpars2_200_upper_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_200_upper_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 783).
yeccpars2_200_upper_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,'yeccpars2_201_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_201_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_201_$end'/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
'yeccpars2_201_$end'(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_and/1}).
-dialyzer({nowarn_function, yeccpars2_201_and/1}).
-compile({nowarn_unused_function,  yeccpars2_201_and/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_and(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_201_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_201_arrow/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_arrow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_comma/1}).
-dialyzer({nowarn_function, yeccpars2_201_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_201_comma/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_comma(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_cons/1}).
-dialyzer({nowarn_function, yeccpars2_201_cons/1}).
-compile({nowarn_unused_function,  yeccpars2_201_cons/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_cons(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_do/1}).
-dialyzer({nowarn_function, yeccpars2_201_do/1}).
-compile({nowarn_unused_function,  yeccpars2_201_do/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_do(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_dot/1}).
-dialyzer({nowarn_function, yeccpars2_201_dot/1}).
-compile({nowarn_unused_function,  yeccpars2_201_dot/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_dot(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_effect/1}).
-dialyzer({nowarn_function, yeccpars2_201_effect/1}).
-compile({nowarn_unused_function,  yeccpars2_201_effect/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_effect(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_end/1}).
-dialyzer({nowarn_function, yeccpars2_201_end/1}).
-compile({nowarn_unused_function,  yeccpars2_201_end/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_end(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_equals/1}).
-dialyzer({nowarn_function, yeccpars2_201_equals/1}).
-compile({nowarn_unused_function,  yeccpars2_201_equals/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_equals(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_error/1}).
-dialyzer({nowarn_function, yeccpars2_201_error/1}).
-compile({nowarn_unused_function,  yeccpars2_201_error/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_error(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_float/1}).
-dialyzer({nowarn_function, yeccpars2_201_float/1}).
-compile({nowarn_unused_function,  yeccpars2_201_float/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_float(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_fn/1}).
-dialyzer({nowarn_function, yeccpars2_201_fn/1}).
-compile({nowarn_unused_function,  yeccpars2_201_fn/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_fn(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_handle/1}).
-dialyzer({nowarn_function, yeccpars2_201_handle/1}).
-compile({nowarn_unused_function,  yeccpars2_201_handle/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_handle(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_in/1}).
-dialyzer({nowarn_function, yeccpars2_201_in/1}).
-compile({nowarn_unused_function,  yeccpars2_201_in/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_in(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_instance/1}).
-dialyzer({nowarn_function, yeccpars2_201_instance/1}).
-compile({nowarn_unused_function,  yeccpars2_201_instance/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_instance(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_integer/1}).
-dialyzer({nowarn_function, yeccpars2_201_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_201_integer/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_integer(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_lbrace/1}).
-dialyzer({nowarn_function, yeccpars2_201_lbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_201_lbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_lbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_lbracket/1}).
-dialyzer({nowarn_function, yeccpars2_201_lbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_201_lbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_lbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_let/1}).
-dialyzer({nowarn_function, yeccpars2_201_let/1}).
-compile({nowarn_unused_function,  yeccpars2_201_let/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_let(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_lower_ident/1}).
-dialyzer({nowarn_function, yeccpars2_201_lower_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_201_lower_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_lower_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_lparen/1}).
-dialyzer({nowarn_function, yeccpars2_201_lparen/1}).
-compile({nowarn_unused_function,  yeccpars2_201_lparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_lparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_match/1}).
-dialyzer({nowarn_function, yeccpars2_201_match/1}).
-compile({nowarn_unused_function,  yeccpars2_201_match/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_match(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_of/1}).
-dialyzer({nowarn_function, yeccpars2_201_of/1}).
-compile({nowarn_unused_function,  yeccpars2_201_of/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_of(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_or/1}).
-dialyzer({nowarn_function, yeccpars2_201_or/1}).
-compile({nowarn_unused_function,  yeccpars2_201_or/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_or(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_perform/1}).
-dialyzer({nowarn_function, yeccpars2_201_perform/1}).
-compile({nowarn_unused_function,  yeccpars2_201_perform/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_perform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_pipe/1}).
-dialyzer({nowarn_function, yeccpars2_201_pipe/1}).
-compile({nowarn_unused_function,  yeccpars2_201_pipe/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_pipe(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_pipe_right/1}).
-dialyzer({nowarn_function, yeccpars2_201_pipe_right/1}).
-compile({nowarn_unused_function,  yeccpars2_201_pipe_right/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_pipe_right(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_property/1}).
-dialyzer({nowarn_function, yeccpars2_201_property/1}).
-compile({nowarn_unused_function,  yeccpars2_201_property/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_property(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_201_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_201_rbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_rbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_rbracket/1}).
-dialyzer({nowarn_function, yeccpars2_201_rbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_201_rbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_rbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_rparen/1}).
-dialyzer({nowarn_function, yeccpars2_201_rparen/1}).
-compile({nowarn_unused_function,  yeccpars2_201_rparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_rparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_semicolon/1}).
-dialyzer({nowarn_function, yeccpars2_201_semicolon/1}).
-compile({nowarn_unused_function,  yeccpars2_201_semicolon/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_semicolon(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_string/1}).
-dialyzer({nowarn_function, yeccpars2_201_string/1}).
-compile({nowarn_unused_function,  yeccpars2_201_string/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_string(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_test/1}).
-dialyzer({nowarn_function, yeccpars2_201_test/1}).
-compile({nowarn_unused_function,  yeccpars2_201_test/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_test(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_then/1}).
-dialyzer({nowarn_function, yeccpars2_201_then/1}).
-compile({nowarn_unused_function,  yeccpars2_201_then/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_then(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_trait/1}).
-dialyzer({nowarn_function, yeccpars2_201_trait/1}).
-compile({nowarn_unused_function,  yeccpars2_201_trait/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_trait(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_transform/1}).
-dialyzer({nowarn_function, yeccpars2_201_transform/1}).
-compile({nowarn_unused_function,  yeccpars2_201_transform/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_transform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_type/1}).
-dialyzer({nowarn_function, yeccpars2_201_type/1}).
-compile({nowarn_unused_function,  yeccpars2_201_type/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_type(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_upper_ident/1}).
-dialyzer({nowarn_function, yeccpars2_201_upper_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_201_upper_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 780).
yeccpars2_201_upper_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_/1}).
-dialyzer({nowarn_function, yeccpars2_202_/1}).
-compile({nowarn_unused_function,  yeccpars2_202_/1}).
-file("src/compiler/parser/catena_parser.yrl", 806).
yeccpars2_202_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, plus_plus, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_/1}).
-dialyzer({nowarn_function, yeccpars2_203_/1}).
-compile({nowarn_unused_function,  yeccpars2_203_/1}).
-file("src/compiler/parser/catena_parser.yrl", 762).
yeccpars2_203_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                        
    {binary_op, plus, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_/1}).
-dialyzer({nowarn_function, yeccpars2_204_/1}).
-compile({nowarn_unused_function,  yeccpars2_204_/1}).
-file("src/compiler/parser/catena_parser.yrl", 759).
yeccpars2_204_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, pipe_right, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_/1}).
-dialyzer({nowarn_function, yeccpars2_205_/1}).
-compile({nowarn_unused_function,  yeccpars2_205_/1}).
-file("src/compiler/parser/catena_parser.yrl", 802).
yeccpars2_205_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                        
    {binary_op, 'or', ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,'yeccpars2_206_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_206_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_206_$end'/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
'yeccpars2_206_$end'(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_and/1}).
-dialyzer({nowarn_function, yeccpars2_206_and/1}).
-compile({nowarn_unused_function,  yeccpars2_206_and/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_and(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_206_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_206_arrow/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_arrow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_comma/1}).
-dialyzer({nowarn_function, yeccpars2_206_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_206_comma/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_comma(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_cons/1}).
-dialyzer({nowarn_function, yeccpars2_206_cons/1}).
-compile({nowarn_unused_function,  yeccpars2_206_cons/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_cons(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_do/1}).
-dialyzer({nowarn_function, yeccpars2_206_do/1}).
-compile({nowarn_unused_function,  yeccpars2_206_do/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_do(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_dot/1}).
-dialyzer({nowarn_function, yeccpars2_206_dot/1}).
-compile({nowarn_unused_function,  yeccpars2_206_dot/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_dot(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_effect/1}).
-dialyzer({nowarn_function, yeccpars2_206_effect/1}).
-compile({nowarn_unused_function,  yeccpars2_206_effect/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_effect(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_end/1}).
-dialyzer({nowarn_function, yeccpars2_206_end/1}).
-compile({nowarn_unused_function,  yeccpars2_206_end/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_end(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_equals/1}).
-dialyzer({nowarn_function, yeccpars2_206_equals/1}).
-compile({nowarn_unused_function,  yeccpars2_206_equals/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_equals(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_error/1}).
-dialyzer({nowarn_function, yeccpars2_206_error/1}).
-compile({nowarn_unused_function,  yeccpars2_206_error/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_error(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_float/1}).
-dialyzer({nowarn_function, yeccpars2_206_float/1}).
-compile({nowarn_unused_function,  yeccpars2_206_float/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_float(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_fn/1}).
-dialyzer({nowarn_function, yeccpars2_206_fn/1}).
-compile({nowarn_unused_function,  yeccpars2_206_fn/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_fn(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_handle/1}).
-dialyzer({nowarn_function, yeccpars2_206_handle/1}).
-compile({nowarn_unused_function,  yeccpars2_206_handle/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_handle(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_in/1}).
-dialyzer({nowarn_function, yeccpars2_206_in/1}).
-compile({nowarn_unused_function,  yeccpars2_206_in/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_in(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_instance/1}).
-dialyzer({nowarn_function, yeccpars2_206_instance/1}).
-compile({nowarn_unused_function,  yeccpars2_206_instance/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_instance(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_integer/1}).
-dialyzer({nowarn_function, yeccpars2_206_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_206_integer/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_integer(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_lbrace/1}).
-dialyzer({nowarn_function, yeccpars2_206_lbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_206_lbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_lbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_lbracket/1}).
-dialyzer({nowarn_function, yeccpars2_206_lbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_206_lbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_lbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_let/1}).
-dialyzer({nowarn_function, yeccpars2_206_let/1}).
-compile({nowarn_unused_function,  yeccpars2_206_let/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_let(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_lower_ident/1}).
-dialyzer({nowarn_function, yeccpars2_206_lower_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_206_lower_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_lower_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_lparen/1}).
-dialyzer({nowarn_function, yeccpars2_206_lparen/1}).
-compile({nowarn_unused_function,  yeccpars2_206_lparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_lparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_match/1}).
-dialyzer({nowarn_function, yeccpars2_206_match/1}).
-compile({nowarn_unused_function,  yeccpars2_206_match/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_match(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_of/1}).
-dialyzer({nowarn_function, yeccpars2_206_of/1}).
-compile({nowarn_unused_function,  yeccpars2_206_of/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_of(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_or/1}).
-dialyzer({nowarn_function, yeccpars2_206_or/1}).
-compile({nowarn_unused_function,  yeccpars2_206_or/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_or(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_perform/1}).
-dialyzer({nowarn_function, yeccpars2_206_perform/1}).
-compile({nowarn_unused_function,  yeccpars2_206_perform/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_perform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_pipe/1}).
-dialyzer({nowarn_function, yeccpars2_206_pipe/1}).
-compile({nowarn_unused_function,  yeccpars2_206_pipe/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_pipe(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_pipe_right/1}).
-dialyzer({nowarn_function, yeccpars2_206_pipe_right/1}).
-compile({nowarn_unused_function,  yeccpars2_206_pipe_right/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_pipe_right(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_property/1}).
-dialyzer({nowarn_function, yeccpars2_206_property/1}).
-compile({nowarn_unused_function,  yeccpars2_206_property/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_property(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_206_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_206_rbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_rbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_rbracket/1}).
-dialyzer({nowarn_function, yeccpars2_206_rbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_206_rbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_rbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_rparen/1}).
-dialyzer({nowarn_function, yeccpars2_206_rparen/1}).
-compile({nowarn_unused_function,  yeccpars2_206_rparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_rparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_semicolon/1}).
-dialyzer({nowarn_function, yeccpars2_206_semicolon/1}).
-compile({nowarn_unused_function,  yeccpars2_206_semicolon/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_semicolon(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_string/1}).
-dialyzer({nowarn_function, yeccpars2_206_string/1}).
-compile({nowarn_unused_function,  yeccpars2_206_string/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_string(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_test/1}).
-dialyzer({nowarn_function, yeccpars2_206_test/1}).
-compile({nowarn_unused_function,  yeccpars2_206_test/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_test(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_then/1}).
-dialyzer({nowarn_function, yeccpars2_206_then/1}).
-compile({nowarn_unused_function,  yeccpars2_206_then/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_then(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_trait/1}).
-dialyzer({nowarn_function, yeccpars2_206_trait/1}).
-compile({nowarn_unused_function,  yeccpars2_206_trait/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_trait(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_transform/1}).
-dialyzer({nowarn_function, yeccpars2_206_transform/1}).
-compile({nowarn_unused_function,  yeccpars2_206_transform/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_transform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_type/1}).
-dialyzer({nowarn_function, yeccpars2_206_type/1}).
-compile({nowarn_unused_function,  yeccpars2_206_type/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_type(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_upper_ident/1}).
-dialyzer({nowarn_function, yeccpars2_206_upper_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_206_upper_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 777).
yeccpars2_206_upper_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_/1}).
-dialyzer({nowarn_function, yeccpars2_207_/1}).
-compile({nowarn_unused_function,  yeccpars2_207_/1}).
-file("src/compiler/parser/catena_parser.yrl", 765).
yeccpars2_207_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                         
    {binary_op, minus, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,'yeccpars2_208_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_208_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_208_$end'/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
'yeccpars2_208_$end'(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_and/1}).
-dialyzer({nowarn_function, yeccpars2_208_and/1}).
-compile({nowarn_unused_function,  yeccpars2_208_and/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_and(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_208_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_208_arrow/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_arrow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_comma/1}).
-dialyzer({nowarn_function, yeccpars2_208_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_208_comma/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_comma(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_cons/1}).
-dialyzer({nowarn_function, yeccpars2_208_cons/1}).
-compile({nowarn_unused_function,  yeccpars2_208_cons/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_cons(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_do/1}).
-dialyzer({nowarn_function, yeccpars2_208_do/1}).
-compile({nowarn_unused_function,  yeccpars2_208_do/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_do(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_dot/1}).
-dialyzer({nowarn_function, yeccpars2_208_dot/1}).
-compile({nowarn_unused_function,  yeccpars2_208_dot/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_dot(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_effect/1}).
-dialyzer({nowarn_function, yeccpars2_208_effect/1}).
-compile({nowarn_unused_function,  yeccpars2_208_effect/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_effect(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_end/1}).
-dialyzer({nowarn_function, yeccpars2_208_end/1}).
-compile({nowarn_unused_function,  yeccpars2_208_end/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_end(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_eq/1}).
-dialyzer({nowarn_function, yeccpars2_208_eq/1}).
-compile({nowarn_unused_function,  yeccpars2_208_eq/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_eq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_equals/1}).
-dialyzer({nowarn_function, yeccpars2_208_equals/1}).
-compile({nowarn_unused_function,  yeccpars2_208_equals/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_equals(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_error/1}).
-dialyzer({nowarn_function, yeccpars2_208_error/1}).
-compile({nowarn_unused_function,  yeccpars2_208_error/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_error(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_float/1}).
-dialyzer({nowarn_function, yeccpars2_208_float/1}).
-compile({nowarn_unused_function,  yeccpars2_208_float/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_float(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_fn/1}).
-dialyzer({nowarn_function, yeccpars2_208_fn/1}).
-compile({nowarn_unused_function,  yeccpars2_208_fn/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_fn(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_handle/1}).
-dialyzer({nowarn_function, yeccpars2_208_handle/1}).
-compile({nowarn_unused_function,  yeccpars2_208_handle/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_handle(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_in/1}).
-dialyzer({nowarn_function, yeccpars2_208_in/1}).
-compile({nowarn_unused_function,  yeccpars2_208_in/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_in(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_instance/1}).
-dialyzer({nowarn_function, yeccpars2_208_instance/1}).
-compile({nowarn_unused_function,  yeccpars2_208_instance/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_instance(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_integer/1}).
-dialyzer({nowarn_function, yeccpars2_208_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_208_integer/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_integer(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_lbrace/1}).
-dialyzer({nowarn_function, yeccpars2_208_lbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_208_lbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_lbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_lbracket/1}).
-dialyzer({nowarn_function, yeccpars2_208_lbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_208_lbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_lbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_let/1}).
-dialyzer({nowarn_function, yeccpars2_208_let/1}).
-compile({nowarn_unused_function,  yeccpars2_208_let/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_let(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_lower_ident/1}).
-dialyzer({nowarn_function, yeccpars2_208_lower_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_208_lower_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_lower_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_lparen/1}).
-dialyzer({nowarn_function, yeccpars2_208_lparen/1}).
-compile({nowarn_unused_function,  yeccpars2_208_lparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_lparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_match/1}).
-dialyzer({nowarn_function, yeccpars2_208_match/1}).
-compile({nowarn_unused_function,  yeccpars2_208_match/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_match(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_neq/1}).
-dialyzer({nowarn_function, yeccpars2_208_neq/1}).
-compile({nowarn_unused_function,  yeccpars2_208_neq/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_neq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_of/1}).
-dialyzer({nowarn_function, yeccpars2_208_of/1}).
-compile({nowarn_unused_function,  yeccpars2_208_of/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_of(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_or/1}).
-dialyzer({nowarn_function, yeccpars2_208_or/1}).
-compile({nowarn_unused_function,  yeccpars2_208_or/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_or(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_perform/1}).
-dialyzer({nowarn_function, yeccpars2_208_perform/1}).
-compile({nowarn_unused_function,  yeccpars2_208_perform/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_perform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_pipe/1}).
-dialyzer({nowarn_function, yeccpars2_208_pipe/1}).
-compile({nowarn_unused_function,  yeccpars2_208_pipe/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_pipe(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_pipe_right/1}).
-dialyzer({nowarn_function, yeccpars2_208_pipe_right/1}).
-compile({nowarn_unused_function,  yeccpars2_208_pipe_right/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_pipe_right(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_property/1}).
-dialyzer({nowarn_function, yeccpars2_208_property/1}).
-compile({nowarn_unused_function,  yeccpars2_208_property/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_property(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_208_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_208_rbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_rbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_rbracket/1}).
-dialyzer({nowarn_function, yeccpars2_208_rbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_208_rbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_rbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_rparen/1}).
-dialyzer({nowarn_function, yeccpars2_208_rparen/1}).
-compile({nowarn_unused_function,  yeccpars2_208_rparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_rparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_semicolon/1}).
-dialyzer({nowarn_function, yeccpars2_208_semicolon/1}).
-compile({nowarn_unused_function,  yeccpars2_208_semicolon/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_semicolon(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_setoid_eq/1}).
-dialyzer({nowarn_function, yeccpars2_208_setoid_eq/1}).
-compile({nowarn_unused_function,  yeccpars2_208_setoid_eq/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_setoid_eq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_setoid_neq/1}).
-dialyzer({nowarn_function, yeccpars2_208_setoid_neq/1}).
-compile({nowarn_unused_function,  yeccpars2_208_setoid_neq/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_setoid_neq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_string/1}).
-dialyzer({nowarn_function, yeccpars2_208_string/1}).
-compile({nowarn_unused_function,  yeccpars2_208_string/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_string(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_test/1}).
-dialyzer({nowarn_function, yeccpars2_208_test/1}).
-compile({nowarn_unused_function,  yeccpars2_208_test/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_test(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_then/1}).
-dialyzer({nowarn_function, yeccpars2_208_then/1}).
-compile({nowarn_unused_function,  yeccpars2_208_then/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_then(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_trait/1}).
-dialyzer({nowarn_function, yeccpars2_208_trait/1}).
-compile({nowarn_unused_function,  yeccpars2_208_trait/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_trait(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_transform/1}).
-dialyzer({nowarn_function, yeccpars2_208_transform/1}).
-compile({nowarn_unused_function,  yeccpars2_208_transform/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_transform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_type/1}).
-dialyzer({nowarn_function, yeccpars2_208_type/1}).
-compile({nowarn_unused_function,  yeccpars2_208_type/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_type(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_upper_ident/1}).
-dialyzer({nowarn_function, yeccpars2_208_upper_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_208_upper_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 792).
yeccpars2_208_upper_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,'yeccpars2_209_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_209_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_209_$end'/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
'yeccpars2_209_$end'(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_and/1}).
-dialyzer({nowarn_function, yeccpars2_209_and/1}).
-compile({nowarn_unused_function,  yeccpars2_209_and/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_and(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_209_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_209_arrow/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_arrow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_comma/1}).
-dialyzer({nowarn_function, yeccpars2_209_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_209_comma/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_comma(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_cons/1}).
-dialyzer({nowarn_function, yeccpars2_209_cons/1}).
-compile({nowarn_unused_function,  yeccpars2_209_cons/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_cons(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_do/1}).
-dialyzer({nowarn_function, yeccpars2_209_do/1}).
-compile({nowarn_unused_function,  yeccpars2_209_do/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_do(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_dot/1}).
-dialyzer({nowarn_function, yeccpars2_209_dot/1}).
-compile({nowarn_unused_function,  yeccpars2_209_dot/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_dot(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_effect/1}).
-dialyzer({nowarn_function, yeccpars2_209_effect/1}).
-compile({nowarn_unused_function,  yeccpars2_209_effect/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_effect(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_end/1}).
-dialyzer({nowarn_function, yeccpars2_209_end/1}).
-compile({nowarn_unused_function,  yeccpars2_209_end/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_end(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_eq/1}).
-dialyzer({nowarn_function, yeccpars2_209_eq/1}).
-compile({nowarn_unused_function,  yeccpars2_209_eq/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_eq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_equals/1}).
-dialyzer({nowarn_function, yeccpars2_209_equals/1}).
-compile({nowarn_unused_function,  yeccpars2_209_equals/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_equals(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_error/1}).
-dialyzer({nowarn_function, yeccpars2_209_error/1}).
-compile({nowarn_unused_function,  yeccpars2_209_error/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_error(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_float/1}).
-dialyzer({nowarn_function, yeccpars2_209_float/1}).
-compile({nowarn_unused_function,  yeccpars2_209_float/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_float(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_fn/1}).
-dialyzer({nowarn_function, yeccpars2_209_fn/1}).
-compile({nowarn_unused_function,  yeccpars2_209_fn/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_fn(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_handle/1}).
-dialyzer({nowarn_function, yeccpars2_209_handle/1}).
-compile({nowarn_unused_function,  yeccpars2_209_handle/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_handle(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_in/1}).
-dialyzer({nowarn_function, yeccpars2_209_in/1}).
-compile({nowarn_unused_function,  yeccpars2_209_in/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_in(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_instance/1}).
-dialyzer({nowarn_function, yeccpars2_209_instance/1}).
-compile({nowarn_unused_function,  yeccpars2_209_instance/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_instance(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_integer/1}).
-dialyzer({nowarn_function, yeccpars2_209_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_209_integer/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_integer(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_lbrace/1}).
-dialyzer({nowarn_function, yeccpars2_209_lbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_209_lbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_lbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_lbracket/1}).
-dialyzer({nowarn_function, yeccpars2_209_lbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_209_lbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_lbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_let/1}).
-dialyzer({nowarn_function, yeccpars2_209_let/1}).
-compile({nowarn_unused_function,  yeccpars2_209_let/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_let(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_lower_ident/1}).
-dialyzer({nowarn_function, yeccpars2_209_lower_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_209_lower_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_lower_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_lparen/1}).
-dialyzer({nowarn_function, yeccpars2_209_lparen/1}).
-compile({nowarn_unused_function,  yeccpars2_209_lparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_lparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_match/1}).
-dialyzer({nowarn_function, yeccpars2_209_match/1}).
-compile({nowarn_unused_function,  yeccpars2_209_match/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_match(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_neq/1}).
-dialyzer({nowarn_function, yeccpars2_209_neq/1}).
-compile({nowarn_unused_function,  yeccpars2_209_neq/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_neq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_of/1}).
-dialyzer({nowarn_function, yeccpars2_209_of/1}).
-compile({nowarn_unused_function,  yeccpars2_209_of/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_of(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_or/1}).
-dialyzer({nowarn_function, yeccpars2_209_or/1}).
-compile({nowarn_unused_function,  yeccpars2_209_or/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_or(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_perform/1}).
-dialyzer({nowarn_function, yeccpars2_209_perform/1}).
-compile({nowarn_unused_function,  yeccpars2_209_perform/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_perform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_pipe/1}).
-dialyzer({nowarn_function, yeccpars2_209_pipe/1}).
-compile({nowarn_unused_function,  yeccpars2_209_pipe/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_pipe(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_pipe_right/1}).
-dialyzer({nowarn_function, yeccpars2_209_pipe_right/1}).
-compile({nowarn_unused_function,  yeccpars2_209_pipe_right/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_pipe_right(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_property/1}).
-dialyzer({nowarn_function, yeccpars2_209_property/1}).
-compile({nowarn_unused_function,  yeccpars2_209_property/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_property(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_209_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_209_rbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_rbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_rbracket/1}).
-dialyzer({nowarn_function, yeccpars2_209_rbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_209_rbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_rbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_rparen/1}).
-dialyzer({nowarn_function, yeccpars2_209_rparen/1}).
-compile({nowarn_unused_function,  yeccpars2_209_rparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_rparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_semicolon/1}).
-dialyzer({nowarn_function, yeccpars2_209_semicolon/1}).
-compile({nowarn_unused_function,  yeccpars2_209_semicolon/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_semicolon(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_setoid_eq/1}).
-dialyzer({nowarn_function, yeccpars2_209_setoid_eq/1}).
-compile({nowarn_unused_function,  yeccpars2_209_setoid_eq/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_setoid_eq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_setoid_neq/1}).
-dialyzer({nowarn_function, yeccpars2_209_setoid_neq/1}).
-compile({nowarn_unused_function,  yeccpars2_209_setoid_neq/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_setoid_neq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_string/1}).
-dialyzer({nowarn_function, yeccpars2_209_string/1}).
-compile({nowarn_unused_function,  yeccpars2_209_string/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_string(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_test/1}).
-dialyzer({nowarn_function, yeccpars2_209_test/1}).
-compile({nowarn_unused_function,  yeccpars2_209_test/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_test(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_then/1}).
-dialyzer({nowarn_function, yeccpars2_209_then/1}).
-compile({nowarn_unused_function,  yeccpars2_209_then/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_then(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_trait/1}).
-dialyzer({nowarn_function, yeccpars2_209_trait/1}).
-compile({nowarn_unused_function,  yeccpars2_209_trait/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_trait(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_transform/1}).
-dialyzer({nowarn_function, yeccpars2_209_transform/1}).
-compile({nowarn_unused_function,  yeccpars2_209_transform/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_transform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_type/1}).
-dialyzer({nowarn_function, yeccpars2_209_type/1}).
-compile({nowarn_unused_function,  yeccpars2_209_type/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_type(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_209_upper_ident/1}).
-dialyzer({nowarn_function, yeccpars2_209_upper_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_209_upper_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 786).
yeccpars2_209_upper_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,'yeccpars2_210_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_210_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_210_$end'/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
'yeccpars2_210_$end'(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_and/1}).
-dialyzer({nowarn_function, yeccpars2_210_and/1}).
-compile({nowarn_unused_function,  yeccpars2_210_and/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_and(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_210_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_210_arrow/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_arrow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_comma/1}).
-dialyzer({nowarn_function, yeccpars2_210_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_210_comma/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_comma(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_cons/1}).
-dialyzer({nowarn_function, yeccpars2_210_cons/1}).
-compile({nowarn_unused_function,  yeccpars2_210_cons/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_cons(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_do/1}).
-dialyzer({nowarn_function, yeccpars2_210_do/1}).
-compile({nowarn_unused_function,  yeccpars2_210_do/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_do(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_dot/1}).
-dialyzer({nowarn_function, yeccpars2_210_dot/1}).
-compile({nowarn_unused_function,  yeccpars2_210_dot/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_dot(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_effect/1}).
-dialyzer({nowarn_function, yeccpars2_210_effect/1}).
-compile({nowarn_unused_function,  yeccpars2_210_effect/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_effect(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_end/1}).
-dialyzer({nowarn_function, yeccpars2_210_end/1}).
-compile({nowarn_unused_function,  yeccpars2_210_end/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_end(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_eq/1}).
-dialyzer({nowarn_function, yeccpars2_210_eq/1}).
-compile({nowarn_unused_function,  yeccpars2_210_eq/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_eq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_equals/1}).
-dialyzer({nowarn_function, yeccpars2_210_equals/1}).
-compile({nowarn_unused_function,  yeccpars2_210_equals/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_equals(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_error/1}).
-dialyzer({nowarn_function, yeccpars2_210_error/1}).
-compile({nowarn_unused_function,  yeccpars2_210_error/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_error(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_float/1}).
-dialyzer({nowarn_function, yeccpars2_210_float/1}).
-compile({nowarn_unused_function,  yeccpars2_210_float/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_float(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_fn/1}).
-dialyzer({nowarn_function, yeccpars2_210_fn/1}).
-compile({nowarn_unused_function,  yeccpars2_210_fn/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_fn(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_handle/1}).
-dialyzer({nowarn_function, yeccpars2_210_handle/1}).
-compile({nowarn_unused_function,  yeccpars2_210_handle/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_handle(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_in/1}).
-dialyzer({nowarn_function, yeccpars2_210_in/1}).
-compile({nowarn_unused_function,  yeccpars2_210_in/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_in(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_instance/1}).
-dialyzer({nowarn_function, yeccpars2_210_instance/1}).
-compile({nowarn_unused_function,  yeccpars2_210_instance/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_instance(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_integer/1}).
-dialyzer({nowarn_function, yeccpars2_210_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_210_integer/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_integer(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_lbrace/1}).
-dialyzer({nowarn_function, yeccpars2_210_lbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_210_lbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_lbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_lbracket/1}).
-dialyzer({nowarn_function, yeccpars2_210_lbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_210_lbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_lbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_let/1}).
-dialyzer({nowarn_function, yeccpars2_210_let/1}).
-compile({nowarn_unused_function,  yeccpars2_210_let/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_let(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_lower_ident/1}).
-dialyzer({nowarn_function, yeccpars2_210_lower_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_210_lower_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_lower_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_lparen/1}).
-dialyzer({nowarn_function, yeccpars2_210_lparen/1}).
-compile({nowarn_unused_function,  yeccpars2_210_lparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_lparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_match/1}).
-dialyzer({nowarn_function, yeccpars2_210_match/1}).
-compile({nowarn_unused_function,  yeccpars2_210_match/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_match(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_neq/1}).
-dialyzer({nowarn_function, yeccpars2_210_neq/1}).
-compile({nowarn_unused_function,  yeccpars2_210_neq/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_neq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_of/1}).
-dialyzer({nowarn_function, yeccpars2_210_of/1}).
-compile({nowarn_unused_function,  yeccpars2_210_of/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_of(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_or/1}).
-dialyzer({nowarn_function, yeccpars2_210_or/1}).
-compile({nowarn_unused_function,  yeccpars2_210_or/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_or(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_perform/1}).
-dialyzer({nowarn_function, yeccpars2_210_perform/1}).
-compile({nowarn_unused_function,  yeccpars2_210_perform/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_perform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_pipe/1}).
-dialyzer({nowarn_function, yeccpars2_210_pipe/1}).
-compile({nowarn_unused_function,  yeccpars2_210_pipe/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_pipe(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_pipe_right/1}).
-dialyzer({nowarn_function, yeccpars2_210_pipe_right/1}).
-compile({nowarn_unused_function,  yeccpars2_210_pipe_right/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_pipe_right(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_property/1}).
-dialyzer({nowarn_function, yeccpars2_210_property/1}).
-compile({nowarn_unused_function,  yeccpars2_210_property/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_property(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_210_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_210_rbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_rbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_rbracket/1}).
-dialyzer({nowarn_function, yeccpars2_210_rbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_210_rbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_rbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_rparen/1}).
-dialyzer({nowarn_function, yeccpars2_210_rparen/1}).
-compile({nowarn_unused_function,  yeccpars2_210_rparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_rparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_semicolon/1}).
-dialyzer({nowarn_function, yeccpars2_210_semicolon/1}).
-compile({nowarn_unused_function,  yeccpars2_210_semicolon/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_semicolon(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_setoid_eq/1}).
-dialyzer({nowarn_function, yeccpars2_210_setoid_eq/1}).
-compile({nowarn_unused_function,  yeccpars2_210_setoid_eq/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_setoid_eq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_setoid_neq/1}).
-dialyzer({nowarn_function, yeccpars2_210_setoid_neq/1}).
-compile({nowarn_unused_function,  yeccpars2_210_setoid_neq/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_setoid_neq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_string/1}).
-dialyzer({nowarn_function, yeccpars2_210_string/1}).
-compile({nowarn_unused_function,  yeccpars2_210_string/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_string(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_test/1}).
-dialyzer({nowarn_function, yeccpars2_210_test/1}).
-compile({nowarn_unused_function,  yeccpars2_210_test/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_test(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_then/1}).
-dialyzer({nowarn_function, yeccpars2_210_then/1}).
-compile({nowarn_unused_function,  yeccpars2_210_then/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_then(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_trait/1}).
-dialyzer({nowarn_function, yeccpars2_210_trait/1}).
-compile({nowarn_unused_function,  yeccpars2_210_trait/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_trait(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_transform/1}).
-dialyzer({nowarn_function, yeccpars2_210_transform/1}).
-compile({nowarn_unused_function,  yeccpars2_210_transform/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_transform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_type/1}).
-dialyzer({nowarn_function, yeccpars2_210_type/1}).
-compile({nowarn_unused_function,  yeccpars2_210_type/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_type(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_upper_ident/1}).
-dialyzer({nowarn_function, yeccpars2_210_upper_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_210_upper_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_210_upper_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,'yeccpars2_211_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_211_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_211_$end'/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
'yeccpars2_211_$end'(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_and/1}).
-dialyzer({nowarn_function, yeccpars2_211_and/1}).
-compile({nowarn_unused_function,  yeccpars2_211_and/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_and(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_211_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_211_arrow/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_arrow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_comma/1}).
-dialyzer({nowarn_function, yeccpars2_211_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_211_comma/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_comma(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_cons/1}).
-dialyzer({nowarn_function, yeccpars2_211_cons/1}).
-compile({nowarn_unused_function,  yeccpars2_211_cons/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_cons(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_do/1}).
-dialyzer({nowarn_function, yeccpars2_211_do/1}).
-compile({nowarn_unused_function,  yeccpars2_211_do/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_do(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_dot/1}).
-dialyzer({nowarn_function, yeccpars2_211_dot/1}).
-compile({nowarn_unused_function,  yeccpars2_211_dot/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_dot(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_effect/1}).
-dialyzer({nowarn_function, yeccpars2_211_effect/1}).
-compile({nowarn_unused_function,  yeccpars2_211_effect/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_effect(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_end/1}).
-dialyzer({nowarn_function, yeccpars2_211_end/1}).
-compile({nowarn_unused_function,  yeccpars2_211_end/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_end(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_eq/1}).
-dialyzer({nowarn_function, yeccpars2_211_eq/1}).
-compile({nowarn_unused_function,  yeccpars2_211_eq/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_eq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_equals/1}).
-dialyzer({nowarn_function, yeccpars2_211_equals/1}).
-compile({nowarn_unused_function,  yeccpars2_211_equals/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_equals(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_error/1}).
-dialyzer({nowarn_function, yeccpars2_211_error/1}).
-compile({nowarn_unused_function,  yeccpars2_211_error/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_error(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_float/1}).
-dialyzer({nowarn_function, yeccpars2_211_float/1}).
-compile({nowarn_unused_function,  yeccpars2_211_float/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_float(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_fn/1}).
-dialyzer({nowarn_function, yeccpars2_211_fn/1}).
-compile({nowarn_unused_function,  yeccpars2_211_fn/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_fn(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_handle/1}).
-dialyzer({nowarn_function, yeccpars2_211_handle/1}).
-compile({nowarn_unused_function,  yeccpars2_211_handle/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_handle(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_in/1}).
-dialyzer({nowarn_function, yeccpars2_211_in/1}).
-compile({nowarn_unused_function,  yeccpars2_211_in/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_in(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_instance/1}).
-dialyzer({nowarn_function, yeccpars2_211_instance/1}).
-compile({nowarn_unused_function,  yeccpars2_211_instance/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_instance(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_integer/1}).
-dialyzer({nowarn_function, yeccpars2_211_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_211_integer/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_integer(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_lbrace/1}).
-dialyzer({nowarn_function, yeccpars2_211_lbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_211_lbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_lbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_lbracket/1}).
-dialyzer({nowarn_function, yeccpars2_211_lbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_211_lbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_lbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_let/1}).
-dialyzer({nowarn_function, yeccpars2_211_let/1}).
-compile({nowarn_unused_function,  yeccpars2_211_let/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_let(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_lower_ident/1}).
-dialyzer({nowarn_function, yeccpars2_211_lower_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_211_lower_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_lower_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_lparen/1}).
-dialyzer({nowarn_function, yeccpars2_211_lparen/1}).
-compile({nowarn_unused_function,  yeccpars2_211_lparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_lparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_match/1}).
-dialyzer({nowarn_function, yeccpars2_211_match/1}).
-compile({nowarn_unused_function,  yeccpars2_211_match/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_match(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_neq/1}).
-dialyzer({nowarn_function, yeccpars2_211_neq/1}).
-compile({nowarn_unused_function,  yeccpars2_211_neq/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_neq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_of/1}).
-dialyzer({nowarn_function, yeccpars2_211_of/1}).
-compile({nowarn_unused_function,  yeccpars2_211_of/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_of(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_or/1}).
-dialyzer({nowarn_function, yeccpars2_211_or/1}).
-compile({nowarn_unused_function,  yeccpars2_211_or/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_or(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_perform/1}).
-dialyzer({nowarn_function, yeccpars2_211_perform/1}).
-compile({nowarn_unused_function,  yeccpars2_211_perform/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_perform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_pipe/1}).
-dialyzer({nowarn_function, yeccpars2_211_pipe/1}).
-compile({nowarn_unused_function,  yeccpars2_211_pipe/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_pipe(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_pipe_right/1}).
-dialyzer({nowarn_function, yeccpars2_211_pipe_right/1}).
-compile({nowarn_unused_function,  yeccpars2_211_pipe_right/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_pipe_right(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_property/1}).
-dialyzer({nowarn_function, yeccpars2_211_property/1}).
-compile({nowarn_unused_function,  yeccpars2_211_property/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_property(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_211_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_211_rbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_rbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_rbracket/1}).
-dialyzer({nowarn_function, yeccpars2_211_rbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_211_rbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_rbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_rparen/1}).
-dialyzer({nowarn_function, yeccpars2_211_rparen/1}).
-compile({nowarn_unused_function,  yeccpars2_211_rparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_rparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_semicolon/1}).
-dialyzer({nowarn_function, yeccpars2_211_semicolon/1}).
-compile({nowarn_unused_function,  yeccpars2_211_semicolon/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_semicolon(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_setoid_eq/1}).
-dialyzer({nowarn_function, yeccpars2_211_setoid_eq/1}).
-compile({nowarn_unused_function,  yeccpars2_211_setoid_eq/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_setoid_eq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_setoid_neq/1}).
-dialyzer({nowarn_function, yeccpars2_211_setoid_neq/1}).
-compile({nowarn_unused_function,  yeccpars2_211_setoid_neq/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_setoid_neq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_string/1}).
-dialyzer({nowarn_function, yeccpars2_211_string/1}).
-compile({nowarn_unused_function,  yeccpars2_211_string/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_string(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_test/1}).
-dialyzer({nowarn_function, yeccpars2_211_test/1}).
-compile({nowarn_unused_function,  yeccpars2_211_test/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_test(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_then/1}).
-dialyzer({nowarn_function, yeccpars2_211_then/1}).
-compile({nowarn_unused_function,  yeccpars2_211_then/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_then(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_trait/1}).
-dialyzer({nowarn_function, yeccpars2_211_trait/1}).
-compile({nowarn_unused_function,  yeccpars2_211_trait/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_trait(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_transform/1}).
-dialyzer({nowarn_function, yeccpars2_211_transform/1}).
-compile({nowarn_unused_function,  yeccpars2_211_transform/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_transform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_type/1}).
-dialyzer({nowarn_function, yeccpars2_211_type/1}).
-compile({nowarn_unused_function,  yeccpars2_211_type/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_type(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_211_upper_ident/1}).
-dialyzer({nowarn_function, yeccpars2_211_upper_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_211_upper_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_211_upper_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,'yeccpars2_212_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_212_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_212_$end'/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
'yeccpars2_212_$end'(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_and/1}).
-dialyzer({nowarn_function, yeccpars2_212_and/1}).
-compile({nowarn_unused_function,  yeccpars2_212_and/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_and(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_212_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_212_arrow/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_arrow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_comma/1}).
-dialyzer({nowarn_function, yeccpars2_212_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_212_comma/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_comma(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_cons/1}).
-dialyzer({nowarn_function, yeccpars2_212_cons/1}).
-compile({nowarn_unused_function,  yeccpars2_212_cons/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_cons(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_do/1}).
-dialyzer({nowarn_function, yeccpars2_212_do/1}).
-compile({nowarn_unused_function,  yeccpars2_212_do/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_do(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_dot/1}).
-dialyzer({nowarn_function, yeccpars2_212_dot/1}).
-compile({nowarn_unused_function,  yeccpars2_212_dot/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_dot(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_effect/1}).
-dialyzer({nowarn_function, yeccpars2_212_effect/1}).
-compile({nowarn_unused_function,  yeccpars2_212_effect/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_effect(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_end/1}).
-dialyzer({nowarn_function, yeccpars2_212_end/1}).
-compile({nowarn_unused_function,  yeccpars2_212_end/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_end(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_equals/1}).
-dialyzer({nowarn_function, yeccpars2_212_equals/1}).
-compile({nowarn_unused_function,  yeccpars2_212_equals/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_equals(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_error/1}).
-dialyzer({nowarn_function, yeccpars2_212_error/1}).
-compile({nowarn_unused_function,  yeccpars2_212_error/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_error(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_float/1}).
-dialyzer({nowarn_function, yeccpars2_212_float/1}).
-compile({nowarn_unused_function,  yeccpars2_212_float/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_float(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_fn/1}).
-dialyzer({nowarn_function, yeccpars2_212_fn/1}).
-compile({nowarn_unused_function,  yeccpars2_212_fn/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_fn(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_handle/1}).
-dialyzer({nowarn_function, yeccpars2_212_handle/1}).
-compile({nowarn_unused_function,  yeccpars2_212_handle/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_handle(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_in/1}).
-dialyzer({nowarn_function, yeccpars2_212_in/1}).
-compile({nowarn_unused_function,  yeccpars2_212_in/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_in(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_instance/1}).
-dialyzer({nowarn_function, yeccpars2_212_instance/1}).
-compile({nowarn_unused_function,  yeccpars2_212_instance/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_instance(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_integer/1}).
-dialyzer({nowarn_function, yeccpars2_212_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_212_integer/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_integer(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_lbrace/1}).
-dialyzer({nowarn_function, yeccpars2_212_lbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_212_lbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_lbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_lbracket/1}).
-dialyzer({nowarn_function, yeccpars2_212_lbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_212_lbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_lbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_let/1}).
-dialyzer({nowarn_function, yeccpars2_212_let/1}).
-compile({nowarn_unused_function,  yeccpars2_212_let/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_let(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_lower_ident/1}).
-dialyzer({nowarn_function, yeccpars2_212_lower_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_212_lower_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_lower_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_lparen/1}).
-dialyzer({nowarn_function, yeccpars2_212_lparen/1}).
-compile({nowarn_unused_function,  yeccpars2_212_lparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_lparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_match/1}).
-dialyzer({nowarn_function, yeccpars2_212_match/1}).
-compile({nowarn_unused_function,  yeccpars2_212_match/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_match(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_of/1}).
-dialyzer({nowarn_function, yeccpars2_212_of/1}).
-compile({nowarn_unused_function,  yeccpars2_212_of/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_of(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_or/1}).
-dialyzer({nowarn_function, yeccpars2_212_or/1}).
-compile({nowarn_unused_function,  yeccpars2_212_or/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_or(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_perform/1}).
-dialyzer({nowarn_function, yeccpars2_212_perform/1}).
-compile({nowarn_unused_function,  yeccpars2_212_perform/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_perform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_pipe/1}).
-dialyzer({nowarn_function, yeccpars2_212_pipe/1}).
-compile({nowarn_unused_function,  yeccpars2_212_pipe/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_pipe(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_pipe_right/1}).
-dialyzer({nowarn_function, yeccpars2_212_pipe_right/1}).
-compile({nowarn_unused_function,  yeccpars2_212_pipe_right/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_pipe_right(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_property/1}).
-dialyzer({nowarn_function, yeccpars2_212_property/1}).
-compile({nowarn_unused_function,  yeccpars2_212_property/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_property(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_212_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_212_rbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_rbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_rbracket/1}).
-dialyzer({nowarn_function, yeccpars2_212_rbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_212_rbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_rbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_rparen/1}).
-dialyzer({nowarn_function, yeccpars2_212_rparen/1}).
-compile({nowarn_unused_function,  yeccpars2_212_rparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_rparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_semicolon/1}).
-dialyzer({nowarn_function, yeccpars2_212_semicolon/1}).
-compile({nowarn_unused_function,  yeccpars2_212_semicolon/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_semicolon(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_string/1}).
-dialyzer({nowarn_function, yeccpars2_212_string/1}).
-compile({nowarn_unused_function,  yeccpars2_212_string/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_string(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_test/1}).
-dialyzer({nowarn_function, yeccpars2_212_test/1}).
-compile({nowarn_unused_function,  yeccpars2_212_test/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_test(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_then/1}).
-dialyzer({nowarn_function, yeccpars2_212_then/1}).
-compile({nowarn_unused_function,  yeccpars2_212_then/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_then(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_trait/1}).
-dialyzer({nowarn_function, yeccpars2_212_trait/1}).
-compile({nowarn_unused_function,  yeccpars2_212_trait/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_trait(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_transform/1}).
-dialyzer({nowarn_function, yeccpars2_212_transform/1}).
-compile({nowarn_unused_function,  yeccpars2_212_transform/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_transform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_type/1}).
-dialyzer({nowarn_function, yeccpars2_212_type/1}).
-compile({nowarn_unused_function,  yeccpars2_212_type/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_type(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_212_upper_ident/1}).
-dialyzer({nowarn_function, yeccpars2_212_upper_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_212_upper_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 774).
yeccpars2_212_upper_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_213_/1}).
-dialyzer({nowarn_function, yeccpars2_213_/1}).
-compile({nowarn_unused_function,  yeccpars2_213_/1}).
-file("src/compiler/parser/catena_parser.yrl", 810).
yeccpars2_213_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                        
    {cons_expr, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_214_/1}).
-dialyzer({nowarn_function, yeccpars2_214_/1}).
-compile({nowarn_unused_function,  yeccpars2_214_/1}).
-file("src/compiler/parser/catena_parser.yrl", 896).
yeccpars2_214_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                   
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_215_/1}).
-dialyzer({nowarn_function, yeccpars2_215_/1}).
-compile({nowarn_unused_function,  yeccpars2_215_/1}).
-file("src/compiler/parser/catena_parser.yrl", 799).
yeccpars2_215_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                         
    {binary_op, 'and', ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_216_/1}).
-dialyzer({nowarn_function, yeccpars2_216_/1}).
-compile({nowarn_unused_function,  yeccpars2_216_/1}).
-file("src/compiler/parser/catena_parser.yrl", 955).
yeccpars2_216_(__Stack0) ->
 [___7,___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                                 
    {perform_expr,
        extract_atom(___2),
        extract_atom(___4),
        ___6,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_218_/1}).
-dialyzer({nowarn_function, yeccpars2_218_/1}).
-compile({nowarn_unused_function,  yeccpars2_218_/1}).
-file("src/compiler/parser/catena_parser.yrl", 596).
yeccpars2_218_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_221_/1}).
-dialyzer({nowarn_function, yeccpars2_221_/1}).
-compile({nowarn_unused_function,  yeccpars2_221_/1}).
-file("src/compiler/parser/catena_parser.yrl", 618).
yeccpars2_221_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                       
    ___1
  end | __Stack].

-compile({inline,yeccpars2_227_/1}).
-dialyzer({nowarn_function, yeccpars2_227_/1}).
-compile({nowarn_unused_function,  yeccpars2_227_/1}).
-file("src/compiler/parser/catena_parser.yrl", 608).
yeccpars2_227_(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                          
    {match_clause,
        ___2,
        ___4,
        ___6,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_228_/1}).
-dialyzer({nowarn_function, yeccpars2_228_/1}).
-compile({nowarn_unused_function,  yeccpars2_228_/1}).
-file("src/compiler/parser/catena_parser.yrl", 601).
yeccpars2_228_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                            
    {match_clause,
        ___2,
        undefined,
        ___4,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_230_/1}).
-dialyzer({nowarn_function, yeccpars2_230_/1}).
-compile({nowarn_unused_function,  yeccpars2_230_/1}).
-file("src/compiler/parser/catena_parser.yrl", 620).
yeccpars2_230_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                       
    case ___3 of
        {pat_or, Patterns, _Loc} ->
            {pat_or, [___1 | Patterns], extract_location(___2)};
        SinglePattern ->
            {pat_or, [___1, SinglePattern], extract_location(___2)}
    end
  end | __Stack].

-compile({inline,yeccpars2_233_/1}).
-dialyzer({nowarn_function, yeccpars2_233_/1}).
-compile({nowarn_unused_function,  yeccpars2_233_/1}).
-file("src/compiler/parser/catena_parser.yrl", 869).
yeccpars2_233_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                     
    {match_expr, ___2, ___4, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_234_/1}).
-dialyzer({nowarn_function, yeccpars2_234_/1}).
-compile({nowarn_unused_function,  yeccpars2_234_/1}).
-file("src/compiler/parser/catena_parser.yrl", 598).
yeccpars2_234_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    [___1 | ___2]
  end | __Stack].

-compile({inline,yeccpars2_235_/1}).
-dialyzer({nowarn_function, yeccpars2_235_/1}).
-compile({nowarn_unused_function,  yeccpars2_235_/1}).
-file("src/compiler/parser/catena_parser.yrl", 865).
yeccpars2_235_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                           
    {match_expr, undefined, ___2, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_239_/1}).
-dialyzer({nowarn_function, yeccpars2_239_/1}).
-compile({nowarn_unused_function,  yeccpars2_239_/1}).
-file("src/compiler/parser/catena_parser.yrl", 833).
yeccpars2_239_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                    
    ___2
  end | __Stack].

-compile({inline,yeccpars2_240_/1}).
-dialyzer({nowarn_function, yeccpars2_240_/1}).
-compile({nowarn_unused_function,  yeccpars2_240_/1}).
-file("src/compiler/parser/catena_parser.yrl", 842).
yeccpars2_240_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                               
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_241_/1}).
-dialyzer({nowarn_function, yeccpars2_241_/1}).
-compile({nowarn_unused_function,  yeccpars2_241_/1}).
-file("src/compiler/parser/catena_parser.yrl", 840).
yeccpars2_241_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                    
    [___1, ___3]
  end | __Stack].

-compile({inline,yeccpars2_242_/1}).
-dialyzer({nowarn_function, yeccpars2_242_/1}).
-compile({nowarn_unused_function,  yeccpars2_242_/1}).
-file("src/compiler/parser/catena_parser.yrl", 836).
yeccpars2_242_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                               
    {tuple_expr, ___2, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_247_/1}).
-dialyzer({nowarn_function, yeccpars2_247_/1}).
-compile({nowarn_unused_function,  yeccpars2_247_/1}).
-file("src/compiler/parser/catena_parser.yrl", 845).
yeccpars2_247_(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                         
    {let_expr,
        [{pat_var, extract_atom(___2), extract_location(___2)}, ___4],
        ___6,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_249_/1}).
-dialyzer({nowarn_function, yeccpars2_249_/1}).
-compile({nowarn_unused_function,  yeccpars2_249_/1}).
-file("src/compiler/parser/catena_parser.yrl", 872).
yeccpars2_249_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                   
    {list_expr, [], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_250_/1}).
-dialyzer({nowarn_function, yeccpars2_250_/1}).
-compile({nowarn_unused_function,  yeccpars2_250_/1}).
-file("src/compiler/parser/catena_parser.yrl", 875).
yeccpars2_250_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    {list_expr, ___2, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_252_/1}).
-dialyzer({nowarn_function, yeccpars2_252_/1}).
-compile({nowarn_unused_function,  yeccpars2_252_/1}).
-file("src/compiler/parser/catena_parser.yrl", 885).
yeccpars2_252_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_254_/1}).
-dialyzer({nowarn_function, yeccpars2_254_/1}).
-compile({nowarn_unused_function,  yeccpars2_254_/1}).
-file("src/compiler/parser/catena_parser.yrl", 878).
yeccpars2_254_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                               
    {record_expr, [], undefined, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_256_/1}).
-dialyzer({nowarn_function, yeccpars2_256_/1}).
-compile({nowarn_unused_function,  yeccpars2_256_/1}).
-file("src/compiler/parser/catena_parser.yrl", 890).
yeccpars2_256_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                        
    {extract_atom(___1), ___3}
  end | __Stack].

-compile({inline,yeccpars2_258_/1}).
-dialyzer({nowarn_function, yeccpars2_258_/1}).
-compile({nowarn_unused_function,  yeccpars2_258_/1}).
-file("src/compiler/parser/catena_parser.yrl", 887).
yeccpars2_258_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_259_/1}).
-dialyzer({nowarn_function, yeccpars2_259_/1}).
-compile({nowarn_unused_function,  yeccpars2_259_/1}).
-file("src/compiler/parser/catena_parser.yrl", 881).
yeccpars2_259_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    {record_expr, ___2, undefined, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_264_/1}).
-dialyzer({nowarn_function, yeccpars2_264_/1}).
-compile({nowarn_unused_function,  yeccpars2_264_/1}).
-file("src/compiler/parser/catena_parser.yrl", 971).
yeccpars2_264_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                   
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_268_/1}).
-dialyzer({nowarn_function, yeccpars2_268_/1}).
-compile({nowarn_unused_function,  yeccpars2_268_/1}).
-file("src/compiler/parser/catena_parser.yrl", 986).
yeccpars2_268_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                   
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_271_/1}).
-dialyzer({nowarn_function, yeccpars2_271_/1}).
-compile({nowarn_unused_function,  yeccpars2_271_/1}).
-file("src/compiler/parser/catena_parser.yrl", 643).
yeccpars2_271_(__Stack0) ->
 [begin
                                
    []
  end | __Stack0].

-compile({inline,yeccpars2_273_/1}).
-dialyzer({nowarn_function, yeccpars2_273_/1}).
-compile({nowarn_unused_function,  yeccpars2_273_/1}).
-file("src/compiler/parser/catena_parser.yrl", 645).
yeccpars2_273_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_274_/1}).
-dialyzer({nowarn_function, yeccpars2_274_/1}).
-compile({nowarn_unused_function,  yeccpars2_274_/1}).
-file("src/compiler/parser/catena_parser.yrl", 643).
yeccpars2_274_(__Stack0) ->
 [begin
                                
    []
  end | __Stack0].

-compile({inline,yeccpars2_275_/1}).
-dialyzer({nowarn_function, yeccpars2_275_/1}).
-compile({nowarn_unused_function,  yeccpars2_275_/1}).
-file("src/compiler/parser/catena_parser.yrl", 647).
yeccpars2_275_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                        
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_278_/1}).
-dialyzer({nowarn_function, yeccpars2_278_/1}).
-compile({nowarn_unused_function,  yeccpars2_278_/1}).
-file("src/compiler/parser/catena_parser.yrl", 989).
yeccpars2_278_(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                                   
    {operation_case,
        extract_atom(___1),
        ___3,
        ___6,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_279_/1}).
-dialyzer({nowarn_function, yeccpars2_279_/1}).
-compile({nowarn_unused_function,  yeccpars2_279_/1}).
-file("src/compiler/parser/catena_parser.yrl", 996).
yeccpars2_279_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                  
    {operation_case,
        extract_atom(___1),
        [],
        ___3,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_280_/1}).
-dialyzer({nowarn_function, yeccpars2_280_/1}).
-compile({nowarn_unused_function,  yeccpars2_280_/1}).
-file("src/compiler/parser/catena_parser.yrl", 984).
yeccpars2_280_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    ___1 ++ [___2]
  end | __Stack].

-compile({inline,yeccpars2_281_/1}).
-dialyzer({nowarn_function, yeccpars2_281_/1}).
-compile({nowarn_unused_function,  yeccpars2_281_/1}).
-file("src/compiler/parser/catena_parser.yrl", 976).
yeccpars2_281_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             
    {handler_clause,
        extract_atom(___1),
        ___3,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_282_/1}).
-dialyzer({nowarn_function, yeccpars2_282_/1}).
-compile({nowarn_unused_function,  yeccpars2_282_/1}).
-file("src/compiler/parser/catena_parser.yrl", 973).
yeccpars2_282_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    [___1 | ___2]
  end | __Stack].

-compile({inline,yeccpars2_283_/1}).
-dialyzer({nowarn_function, yeccpars2_283_/1}).
-compile({nowarn_unused_function,  yeccpars2_283_/1}).
-file("src/compiler/parser/catena_parser.yrl", 964).
yeccpars2_283_(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                 
    {handle_expr,
        ___2,
        ___5,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_286_/1}).
-dialyzer({nowarn_function, yeccpars2_286_/1}).
-compile({nowarn_unused_function,  yeccpars2_286_/1}).
-file("src/compiler/parser/catena_parser.yrl", 852).
yeccpars2_286_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                           
    {lambda, [{pat_var, extract_atom(___2), extract_location(___2)}], ___4, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_288_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_288_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_288_rbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 927).
yeccpars2_288_rbrace(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                       
    [{do_return, ___1, extract_location(___1)}]
  end | __Stack].

-compile({inline,yeccpars2_288_/1}).
-dialyzer({nowarn_function, yeccpars2_288_/1}).
-compile({nowarn_unused_function,  yeccpars2_288_/1}).
-file("src/compiler/parser/catena_parser.yrl", 946).
yeccpars2_288_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                      
    {do_action, ___1, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_292_/1}).
-dialyzer({nowarn_function, yeccpars2_292_/1}).
-compile({nowarn_unused_function,  yeccpars2_292_/1}).
-file("src/compiler/parser/catena_parser.yrl", 827).
yeccpars2_292_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             
    {var, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_294_/1}).
-dialyzer({nowarn_function, yeccpars2_294_/1}).
-compile({nowarn_unused_function,  yeccpars2_294_/1}).
-file("src/compiler/parser/catena_parser.yrl", 938).
yeccpars2_294_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    {do_bind, extract_atom(___1), ___3, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_297_/1}).
-dialyzer({nowarn_function, yeccpars2_297_/1}).
-compile({nowarn_unused_function,  yeccpars2_297_/1}).
-file("src/compiler/parser/catena_parser.yrl", 942).
yeccpars2_297_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                               
    {do_let, extract_atom(___2), ___4, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_299_/1}).
-dialyzer({nowarn_function, yeccpars2_299_/1}).
-compile({nowarn_unused_function,  yeccpars2_299_/1}).
-file("src/compiler/parser/catena_parser.yrl", 929).
yeccpars2_299_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                       
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_300_/1}).
-dialyzer({nowarn_function, yeccpars2_300_/1}).
-compile({nowarn_unused_function,  yeccpars2_300_/1}).
-file("src/compiler/parser/catena_parser.yrl", 923).
yeccpars2_300_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    {do_expr, ___3, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_301_/1}).
-dialyzer({nowarn_function, yeccpars2_301_/1}).
-compile({nowarn_unused_function,  yeccpars2_301_/1}).
-file("src/compiler/parser/catena_parser.yrl", 816).
yeccpars2_301_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                   
    {app, ___1, [___2], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_303_/1}).
-dialyzer({nowarn_function, yeccpars2_303_/1}).
-compile({nowarn_unused_function,  yeccpars2_303_/1}).
-file("src/compiler/parser/catena_parser.yrl", 819).
yeccpars2_303_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                      
    {record_access, ___1, extract_atom(___3), extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_305_/1}).
-dialyzer({nowarn_function, yeccpars2_305_/1}).
-compile({nowarn_unused_function,  yeccpars2_305_/1}).
-file("src/compiler/parser/catena_parser.yrl", 748).
yeccpars2_305_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_307_/1}).
-dialyzer({nowarn_function, yeccpars2_307_/1}).
-compile({nowarn_unused_function,  yeccpars2_307_/1}).
-file("src/compiler/parser/catena_parser.yrl", 553).
yeccpars2_307_(__Stack0) ->
 [___7,___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                                
    {transform_decl,
        extract_atom(___2),
        undefined,
        [{transform_clause, ___3, ___5, ___7, extract_location(___1)}],
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_308_/1}).
-dialyzer({nowarn_function, yeccpars2_308_/1}).
-compile({nowarn_unused_function,  yeccpars2_308_/1}).
-file("src/compiler/parser/catena_parser.yrl", 546).
yeccpars2_308_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                  
    {transform_decl,
        extract_atom(___2),
        undefined,
        [{transform_clause, ___3, undefined, ___5, extract_location(___1)}],
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_310_/1}).
-dialyzer({nowarn_function, yeccpars2_310_/1}).
-compile({nowarn_unused_function,  yeccpars2_310_/1}).
-file("src/compiler/parser/catena_parser.yrl", 561).
yeccpars2_310_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                               
    make_error_declaration(extract_location(___1), "Invalid transform name", ___2)
  end | __Stack].

-compile({inline,yeccpars2_311_/1}).
-dialyzer({nowarn_function, yeccpars2_311_/1}).
-compile({nowarn_unused_function,  yeccpars2_311_/1}).
-file("src/compiler/parser/catena_parser.yrl", 368).
yeccpars2_311_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                           
    make_error_declaration(extract_location(___1), "Incomplete trait declaration", ___2)
  end | __Stack].

-compile({inline,yeccpars2_312_/1}).
-dialyzer({nowarn_function, yeccpars2_312_/1}).
-compile({nowarn_unused_function,  yeccpars2_312_/1}).
-file("src/compiler/parser/catena_parser.yrl", 290).
yeccpars2_312_(__Stack0) ->
 [begin
                         
    []
  end | __Stack0].

-compile({inline,yeccpars2_313_/1}).
-dialyzer({nowarn_function, yeccpars2_313_/1}).
-compile({nowarn_unused_function,  yeccpars2_313_/1}).
-file("src/compiler/parser/catena_parser.yrl", 373).
yeccpars2_313_(__Stack0) ->
 [begin
                                  undefined
  end | __Stack0].

-compile({inline,yeccpars2_316_/1}).
-dialyzer({nowarn_function, yeccpars2_316_/1}).
-compile({nowarn_unused_function,  yeccpars2_316_/1}).
-file("src/compiler/parser/catena_parser.yrl", 372).
yeccpars2_316_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                   ___2
  end | __Stack].

-compile({inline,yeccpars2_317_/1}).
-dialyzer({nowarn_function, yeccpars2_317_/1}).
-compile({nowarn_unused_function,  yeccpars2_317_/1}).
-file("src/compiler/parser/catena_parser.yrl", 376).
yeccpars2_317_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                        
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_319_/1}).
-dialyzer({nowarn_function, yeccpars2_319_/1}).
-compile({nowarn_unused_function,  yeccpars2_319_/1}).
-file("src/compiler/parser/catena_parser.yrl", 378).
yeccpars2_319_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                 
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_322_/1}).
-dialyzer({nowarn_function, yeccpars2_322_/1}).
-compile({nowarn_unused_function,  yeccpars2_322_/1}).
-file("src/compiler/parser/catena_parser.yrl", 388).
yeccpars2_322_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_326_/1}).
-dialyzer({nowarn_function, yeccpars2_326_/1}).
-compile({nowarn_unused_function,  yeccpars2_326_/1}).
-file("src/compiler/parser/catena_parser.yrl", 398).
yeccpars2_326_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    {trait_sig, extract_atom(___1), ___3, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_327_/1}).
-dialyzer({nowarn_function, yeccpars2_327_/1}).
-compile({nowarn_unused_function,  yeccpars2_327_/1}).
-file("src/compiler/parser/catena_parser.yrl", 406).
yeccpars2_327_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                         
    make_error_declaration(extract_location(___2),
        "Invalid method signature. " ++
        "Common issues and solutions:\n" ++
        "  • Cannot use simple tuples as parameters: '(a, b) -> ...'\n" ++
        "  • Try: 'Pair a b -> ...' or '((a -> b), c) -> ...'\n" ++
        "  • See trait signatures documentation for examples", ___3)
  end | __Stack].

-compile({inline,yeccpars2_329_/1}).
-dialyzer({nowarn_function, yeccpars2_329_/1}).
-compile({nowarn_unused_function,  yeccpars2_329_/1}).
-file("src/compiler/parser/catena_parser.yrl", 402).
yeccpars2_329_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                               
    {trait_default, extract_atom(___1), ___2, ___4, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_330_/1}).
-dialyzer({nowarn_function, yeccpars2_330_/1}).
-compile({nowarn_unused_function,  yeccpars2_330_/1}).
-file("src/compiler/parser/catena_parser.yrl", 392).
yeccpars2_330_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                     
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_331_/1}).
-dialyzer({nowarn_function, yeccpars2_331_/1}).
-compile({nowarn_unused_function,  yeccpars2_331_/1}).
-file("src/compiler/parser/catena_parser.yrl", 390).
yeccpars2_331_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_332_/1}).
-dialyzer({nowarn_function, yeccpars2_332_/1}).
-compile({nowarn_unused_function,  yeccpars2_332_/1}).
-file("src/compiler/parser/catena_parser.yrl", 359).
yeccpars2_332_(__Stack0) ->
 [___7,___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                                           
    {trait_decl,
        extract_atom(___2),
        ___3,
        ___4,
        ___6,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_333_/1}).
-dialyzer({nowarn_function, yeccpars2_333_/1}).
-compile({nowarn_unused_function,  yeccpars2_333_/1}).
-file("src/compiler/parser/catena_parser.yrl", 494).
yeccpars2_333_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                         
    make_error_declaration(extract_location(___1), "Incomplete test declaration", ___2)
  end | __Stack].

-compile({inline,yeccpars2_336_/1}).
-dialyzer({nowarn_function, yeccpars2_336_/1}).
-compile({nowarn_unused_function,  yeccpars2_336_/1}).
-file("src/compiler/parser/catena_parser.yrl", 487).
yeccpars2_336_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                      
    {test_decl,
        extract_value(___2),
        ___4,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_337_/1}).
-dialyzer({nowarn_function, yeccpars2_337_/1}).
-compile({nowarn_unused_function,  yeccpars2_337_/1}).
-file("src/compiler/parser/catena_parser.yrl", 523).
yeccpars2_337_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                 
    make_error_declaration(extract_location(___1), "Incomplete property declaration", ___2)
  end | __Stack].

-compile({inline,yeccpars2_340_/1}).
-dialyzer({nowarn_function, yeccpars2_340_/1}).
-compile({nowarn_unused_function,  yeccpars2_340_/1}).
-file("src/compiler/parser/catena_parser.yrl", 502).
yeccpars2_340_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                       
    {property_decl,
        extract_value(___2),
        ___4,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_343_/1}).
-dialyzer({nowarn_function, yeccpars2_343_/1}).
-compile({nowarn_unused_function,  yeccpars2_343_/1}).
-file("src/compiler/parser/catena_parser.yrl", 513).
yeccpars2_343_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                       
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_346_/1}).
-dialyzer({nowarn_function, yeccpars2_346_/1}).
-compile({nowarn_unused_function,  yeccpars2_346_/1}).
-file("src/compiler/parser/catena_parser.yrl", 519).
yeccpars2_346_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    {extract_atom(___1), extract_atom(___3)}
  end | __Stack].

-compile({inline,yeccpars2_348_/1}).
-dialyzer({nowarn_function, yeccpars2_348_/1}).
-compile({nowarn_unused_function,  yeccpars2_348_/1}).
-file("src/compiler/parser/catena_parser.yrl", 515).
yeccpars2_348_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                               
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_350_/1}).
-dialyzer({nowarn_function, yeccpars2_350_/1}).
-compile({nowarn_unused_function,  yeccpars2_350_/1}).
-file("src/compiler/parser/catena_parser.yrl", 509).
yeccpars2_350_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                    
    {property_forall, ___2, ___4, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_351_/1}).
-dialyzer({nowarn_function, yeccpars2_351_/1}).
-compile({nowarn_unused_function,  yeccpars2_351_/1}).
-file("src/compiler/parser/catena_parser.yrl", 216).
yeccpars2_351_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                   
    {module_decl, extract_atom(___2), [], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_353_/1}).
-dialyzer({nowarn_function, yeccpars2_353_/1}).
-compile({nowarn_unused_function,  yeccpars2_353_/1}).
-file("src/compiler/parser/catena_parser.yrl", 220).
yeccpars2_353_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    {module_decl,
        list_to_atom(atom_to_list(extract_atom(___2)) ++ "." ++ atom_to_list(extract_atom(___4))),
        [],
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_354_/1}).
-dialyzer({nowarn_function, yeccpars2_354_/1}).
-compile({nowarn_unused_function,  yeccpars2_354_/1}).
-file("src/compiler/parser/catena_parser.yrl", 463).
yeccpars2_354_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                          
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_356_/1}).
-dialyzer({nowarn_function, yeccpars2_356_/1}).
-compile({nowarn_unused_function,  yeccpars2_356_/1}).
-file("src/compiler/parser/catena_parser.yrl", 455).
yeccpars2_356_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                 
    make_error_declaration(extract_location(___1), "Incomplete instance declaration", ___2)
  end | __Stack].

-compile({inline,yeccpars2_357_/1}).
-dialyzer({nowarn_function, yeccpars2_357_/1}).
-compile({nowarn_unused_function,  yeccpars2_357_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1079).
yeccpars2_357_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                  
    {type_con, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_358_comma/1}).
-dialyzer({nowarn_function, yeccpars2_358_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_358_comma/1}).
-file("src/compiler/parser/catena_parser.yrl", 1070).
yeccpars2_358_comma(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                             
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_358_double_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_358_double_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_358_double_arrow/1}).
-file("src/compiler/parser/catena_parser.yrl", 1070).
yeccpars2_358_double_arrow(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                             
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_358_/1}).
-dialyzer({nowarn_function, yeccpars2_358_/1}).
-compile({nowarn_unused_function,  yeccpars2_358_/1}).
-file("src/compiler/parser/catena_parser.yrl", 459).
yeccpars2_358_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                          [___1]
  end | __Stack].

-compile({inline,yeccpars2_362_/1}).
-dialyzer({nowarn_function, yeccpars2_362_/1}).
-compile({nowarn_unused_function,  yeccpars2_362_/1}).
-file("src/compiler/parser/catena_parser.yrl", 470).
yeccpars2_362_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                     
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_363_/1}).
-dialyzer({nowarn_function, yeccpars2_363_/1}).
-compile({nowarn_unused_function,  yeccpars2_363_/1}).
-file("src/compiler/parser/catena_parser.yrl", 437).
yeccpars2_363_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                      
    {instance_decl,
        extract_atom(___2),
        ___3,
        undefined,
        [],
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_365_/1}).
-dialyzer({nowarn_function, yeccpars2_365_/1}).
-compile({nowarn_unused_function,  yeccpars2_365_/1}).
-file("src/compiler/parser/catena_parser.yrl", 632).
yeccpars2_365_(__Stack0) ->
 [begin
                          
    []
  end | __Stack0].

-compile({inline,yeccpars2_368_/1}).
-dialyzer({nowarn_function, yeccpars2_368_/1}).
-compile({nowarn_unused_function,  yeccpars2_368_/1}).
-file("src/compiler/parser/catena_parser.yrl", 478).
yeccpars2_368_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                   
    {extract_atom(___2), {lambda, ___3, ___5, extract_location(___1)}}
  end | __Stack].

-compile({inline,yeccpars2_369_/1}).
-dialyzer({nowarn_function, yeccpars2_369_/1}).
-compile({nowarn_unused_function,  yeccpars2_369_/1}).
-file("src/compiler/parser/catena_parser.yrl", 474).
yeccpars2_369_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                           
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_370_/1}).
-dialyzer({nowarn_function, yeccpars2_370_/1}).
-compile({nowarn_unused_function,  yeccpars2_370_/1}).
-file("src/compiler/parser/catena_parser.yrl", 472).
yeccpars2_370_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                            
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_371_/1}).
-dialyzer({nowarn_function, yeccpars2_371_/1}).
-compile({nowarn_unused_function,  yeccpars2_371_/1}).
-file("src/compiler/parser/catena_parser.yrl", 428).
yeccpars2_371_(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                                       
    {instance_decl,
        extract_atom(___2),
        ___3,
        undefined,
        ___5,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_372_/1}).
-dialyzer({nowarn_function, yeccpars2_372_/1}).
-compile({nowarn_unused_function,  yeccpars2_372_/1}).
-file("src/compiler/parser/catena_parser.yrl", 460).
yeccpars2_372_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                             [___1 | ___2]
  end | __Stack].

-compile({inline,yeccpars2_375_/1}).
-dialyzer({nowarn_function, yeccpars2_375_/1}).
-compile({nowarn_unused_function,  yeccpars2_375_/1}).
-file("src/compiler/parser/catena_parser.yrl", 459).
yeccpars2_375_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                          [___1]
  end | __Stack].

-compile({inline,yeccpars2_379_/1}).
-dialyzer({nowarn_function, yeccpars2_379_/1}).
-compile({nowarn_unused_function,  yeccpars2_379_/1}).
-file("src/compiler/parser/catena_parser.yrl", 446).
yeccpars2_379_(__Stack0) ->
 [___7,___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                                                        
    {instance_decl,
        extract_atom(___4),
        ___5,
        ___2,
        [],
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_380_/1}).
-dialyzer({nowarn_function, yeccpars2_380_/1}).
-compile({nowarn_unused_function,  yeccpars2_380_/1}).
-file("src/compiler/parser/catena_parser.yrl", 419).
yeccpars2_380_(__Stack0) ->
 [___8,___7,___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                                                                         
    {instance_decl,
        extract_atom(___4),
        ___5,
        ___2,
        ___7,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_382_/1}).
-dialyzer({nowarn_function, yeccpars2_382_/1}).
-compile({nowarn_unused_function,  yeccpars2_382_/1}).
-file("src/compiler/parser/catena_parser.yrl", 465).
yeccpars2_382_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                     
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_383_/1}).
-dialyzer({nowarn_function, yeccpars2_383_/1}).
-compile({nowarn_unused_function,  yeccpars2_383_/1}).
-file("src/compiler/parser/catena_parser.yrl", 263).
yeccpars2_383_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                             
    make_error_declaration(extract_location(___2), "Malformed declaration before 'effect'", ___1)
  end | __Stack].

-compile({inline,yeccpars2_384_/1}).
-dialyzer({nowarn_function, yeccpars2_384_/1}).
-compile({nowarn_unused_function,  yeccpars2_384_/1}).
-file("src/compiler/parser/catena_parser.yrl", 267).
yeccpars2_384_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                               
    make_error_declaration(extract_location(___2), "Malformed declaration before 'instance'", ___1)
  end | __Stack].

-compile({inline,yeccpars2_385_/1}).
-dialyzer({nowarn_function, yeccpars2_385_/1}).
-compile({nowarn_unused_function,  yeccpars2_385_/1}).
-file("src/compiler/parser/catena_parser.yrl", 265).
yeccpars2_385_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                            
    make_error_declaration(extract_location(___2), "Malformed declaration before 'trait'", ___1)
  end | __Stack].

-compile({inline,yeccpars2_386_/1}).
-dialyzer({nowarn_function, yeccpars2_386_/1}).
-compile({nowarn_unused_function,  yeccpars2_386_/1}).
-file("src/compiler/parser/catena_parser.yrl", 261).
yeccpars2_386_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                
    make_error_declaration(extract_location(___2), "Malformed declaration before 'transform'", ___1)
  end | __Stack].

-compile({inline,yeccpars2_387_/1}).
-dialyzer({nowarn_function, yeccpars2_387_/1}).
-compile({nowarn_unused_function,  yeccpars2_387_/1}).
-file("src/compiler/parser/catena_parser.yrl", 259).
yeccpars2_387_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                           
    make_error_declaration(extract_location(___2), "Malformed declaration before 'type'", ___1)
  end | __Stack].

-compile({inline,yeccpars2_388_/1}).
-dialyzer({nowarn_function, yeccpars2_388_/1}).
-compile({nowarn_unused_function,  yeccpars2_388_/1}).
-file("src/compiler/parser/catena_parser.yrl", 333).
yeccpars2_388_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                             
    make_error_declaration(extract_location(___1), "Incomplete effect declaration", ___2)
  end | __Stack].

-compile({inline,yeccpars2_389_/1}).
-dialyzer({nowarn_function, yeccpars2_389_/1}).
-compile({nowarn_unused_function,  yeccpars2_389_/1}).
-file("src/compiler/parser/catena_parser.yrl", 336).
yeccpars2_389_(__Stack0) ->
 [begin
                               
    []
  end | __Stack0].

-compile({inline,yeccpars2_391_/1}).
-dialyzer({nowarn_function, yeccpars2_391_/1}).
-compile({nowarn_unused_function,  yeccpars2_391_/1}).
-file("src/compiler/parser/catena_parser.yrl", 336).
yeccpars2_391_(__Stack0) ->
 [begin
                               
    []
  end | __Stack0].

-compile({inline,yeccpars2_393_/1}).
-dialyzer({nowarn_function, yeccpars2_393_/1}).
-compile({nowarn_unused_function,  yeccpars2_393_/1}).
-file("src/compiler/parser/catena_parser.yrl", 341).
yeccpars2_393_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                           
    {effect_operation,
        extract_atom(___2),
        undefined,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_395_/1}).
-dialyzer({nowarn_function, yeccpars2_395_/1}).
-compile({nowarn_unused_function,  yeccpars2_395_/1}).
-file("src/compiler/parser/catena_parser.yrl", 347).
yeccpars2_395_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                           
    {effect_operation,
        extract_atom(___2),
        ___4,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_396_/1}).
-dialyzer({nowarn_function, yeccpars2_396_/1}).
-compile({nowarn_unused_function,  yeccpars2_396_/1}).
-file("src/compiler/parser/catena_parser.yrl", 338).
yeccpars2_396_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                         
    [___1 | ___2]
  end | __Stack].

-compile({inline,yeccpars2_397_/1}).
-dialyzer({nowarn_function, yeccpars2_397_/1}).
-compile({nowarn_unused_function,  yeccpars2_397_/1}).
-file("src/compiler/parser/catena_parser.yrl", 326).
yeccpars2_397_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                           
    {effect_decl,
        extract_atom(___2),
        ___3,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_398_/1}).
-dialyzer({nowarn_function, yeccpars2_398_/1}).
-compile({nowarn_unused_function,  yeccpars2_398_/1}).
-file("src/compiler/parser/catena_parser.yrl", 247).
yeccpars2_398_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                          
    [___1 | ___2]
  end | __Stack].

-compile({inline,yeccpars2_399_/1}).
-dialyzer({nowarn_function, yeccpars2_399_/1}).
-compile({nowarn_unused_function,  yeccpars2_399_/1}).
-file("src/compiler/parser/catena_parser.yrl", 227).
yeccpars2_399_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                            
    ___1
  end | __Stack].

-compile({inline,yeccpars2_400_/1}).
-dialyzer({nowarn_function, yeccpars2_400_/1}).
-compile({nowarn_unused_function,  yeccpars2_400_/1}).
-file("src/compiler/parser/catena_parser.yrl", 230).
yeccpars2_400_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                            
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_401_/1}).
-dialyzer({nowarn_function, yeccpars2_401_/1}).
-compile({nowarn_unused_function,  yeccpars2_401_/1}).
-file("src/compiler/parser/catena_parser.yrl", 212).
yeccpars2_401_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                          
    {module_header, element(2, ___1), ___2, element(4, ___1)}
  end | __Stack].

-compile({inline,yeccpars2_407_/1}).
-dialyzer({nowarn_function, yeccpars2_407_/1}).
-compile({nowarn_unused_function,  yeccpars2_407_/1}).
-file("src/compiler/parser/catena_parser.yrl", 238).
yeccpars2_407_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                        
    {export_type, extract_atom(___3)}
  end | __Stack].

-compile({inline,yeccpars2_408_/1}).
-dialyzer({nowarn_function, yeccpars2_408_/1}).
-compile({nowarn_unused_function,  yeccpars2_408_/1}).
-file("src/compiler/parser/catena_parser.yrl", 240).
yeccpars2_408_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    {export_transform, extract_atom(___3)}
  end | __Stack].

-compile({inline,yeccpars2_409_/1}).
-dialyzer({nowarn_function, yeccpars2_409_/1}).
-compile({nowarn_unused_function,  yeccpars2_409_/1}).
-file("src/compiler/parser/catena_parser.yrl", 236).
yeccpars2_409_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                         
    {export_trait, extract_atom(___3)}
  end | __Stack].

-compile({inline,yeccpars2_410_/1}).
-dialyzer({nowarn_function, yeccpars2_410_/1}).
-compile({nowarn_unused_function,  yeccpars2_410_/1}).
-file("src/compiler/parser/catena_parser.yrl", 242).
yeccpars2_410_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                          
    {export_effect, extract_atom(___3)}
  end | __Stack].

-compile({inline,yeccpars2_411_/1}).
-dialyzer({nowarn_function, yeccpars2_411_/1}).
-compile({nowarn_unused_function,  yeccpars2_411_/1}).
-file("src/compiler/parser/catena_parser.yrl", 232).
yeccpars2_411_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                        
    [___1 | ___2]
  end | __Stack].

-compile({inline,yeccpars2_412_/1}).
-dialyzer({nowarn_function, yeccpars2_412_/1}).
-compile({nowarn_unused_function,  yeccpars2_412_/1}).
-file("src/compiler/parser/catena_parser.yrl", 198).
yeccpars2_412_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    {module,
        element(2, ___1),   %% Module name
        element(3, ___1),   %% Exports
        [],                 %% Imports (future)
        ___2,               %% Declarations
        element(4, ___1)}
  end | __Stack].

-compile({inline,yeccpars2_413_/1}).
-dialyzer({nowarn_function, yeccpars2_413_/1}).
-compile({nowarn_unused_function,  yeccpars2_413_/1}).
-file("src/compiler/parser/catena_parser.yrl", 530).
yeccpars2_413_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                         
    {transform_decl,
        extract_transform_name(___1),
        extract_transform_type(___1),
        ___2,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_414_/1}).
-dialyzer({nowarn_function, yeccpars2_414_/1}).
-compile({nowarn_unused_function,  yeccpars2_414_/1}).
-file("src/compiler/parser/catena_parser.yrl", 573).
yeccpars2_414_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                       
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_416_/1}).
-dialyzer({nowarn_function, yeccpars2_416_/1}).
-compile({nowarn_unused_function,  yeccpars2_416_/1}).
-file("src/compiler/parser/catena_parser.yrl", 632).
yeccpars2_416_(__Stack0) ->
 [begin
                          
    []
  end | __Stack0].

-compile({inline,yeccpars2_422_/1}).
-dialyzer({nowarn_function, yeccpars2_422_/1}).
-compile({nowarn_unused_function,  yeccpars2_422_/1}).
-file("src/compiler/parser/catena_parser.yrl", 585).
yeccpars2_422_(__Stack0) ->
 [___7,___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                                  
    {transform_clause,
        ___3,
        ___5,
        ___7,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_423_/1}).
-dialyzer({nowarn_function, yeccpars2_423_/1}).
-compile({nowarn_unused_function,  yeccpars2_423_/1}).
-file("src/compiler/parser/catena_parser.yrl", 578).
yeccpars2_423_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                    
    {transform_clause,
        ___3,
        undefined,
        ___5,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_424_/1}).
-dialyzer({nowarn_function, yeccpars2_424_/1}).
-compile({nowarn_unused_function,  yeccpars2_424_/1}).
-file("src/compiler/parser/catena_parser.yrl", 575).
yeccpars2_424_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                         
    [___1 | ___2]
  end | __Stack].


-file("src/compiler/parser/catena_parser.yrl", 1172).
